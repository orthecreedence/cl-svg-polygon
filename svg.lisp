(dolist (p '(xmls split-sequence cl-ppcre))
  (ql:quickload p))

(defpackage :svg-load
  (:use :cl)
  (:export :parse-groups))
(in-package :svg-load)

(defmacro with-plist-string-reads (plist bindings &body body)
  "Helper macro to make convert-to-points much more readable. Basically wraps
  around reading values from a string in a plist and binding the result to a
  variable:
  
    (with-plist-string-reads my-plist ((x :x) (y :y))
      (+ x y))
  
  Expands to:

    (let ((x (read-from-string (getf my-plist :x)))
          (y (read-from-string (getf my-plist :y))))
      (+ x y))

  Much cleaner."
  `(let ,(loop for binding in bindings collect (list (car binding)
                                                     `(read-from-string (getf ,plist ,(cadr binding)))))
     ,@body))

(defun convert-to-points (obj &key (curve-resolution 10) ignore-errors)
  "Take an object loaded from and SVG file (most likely using parse-svg-nodes)
  and turn it into a set of points describing a polygon. Curves are
  approximated using :curve-resolution. The higher the resolution, the more
  accurate the curve will be. This works for paths with bezier curves as well
  as ellipses and circles.

  Some parts of the path spec aren't implemented, namely stopping a path in the
  middle and continuing it with Z/z (breaks the idea of building polygons), and
  path arcs with A/a (I don't have a use for them and I don't want to spend all
  night implementing something I won't use).
  
  The above cases in paths will throw 'unsupported-path-command command errors
  unless ignored using :ignore-errors t."
  (case (intern (string-upcase (getf obj :type)))
    (rect
      (with-plist-string-reads obj ((x :x) (y :y) (w :width) (h :height)) 
        (vector (list x y)
                (list (+ x w) y)
                (list (+ x w) (+ y h))
                (list x (+ y h)))))
    (polygon
      (let* ((pairs (split-sequence:split-sequence #\space (getf obj :points)))
             (points (loop for pair in pairs
                           if (find #\, pair) collect (split-sequence:split-sequence #\, pair))))
        (coerce points 'vector)))
    (path
      (coerce (get-points-from-path (getf obj :d) :curve-resolution curve-resolution :ignore-errors ignore-errors) 'vector))
    (ellipse 
      (with-plist-string-reads obj ((x :cx) (y :cy) (rx :rx) (ry :ry))
        (get-points-from-ellipse x y rx ry :curve-resolution curve-resolution)))
    (circle
      (with-plist-string-reads obj ((x :cx) (y :cy) (r :r))
        (get-points-from-ellipse x y r r :curve-resolution curve-resolution)))))

(define-condition unsupported-path-command (error)
  ((text :initarg :text :reader text))
  (:documentation "Thrown when an unsupported action/feature is parsed in a path."))

(defun get-points-from-path (str-data &key (curve-resolution 10) ignore-errors)
  "Given a string describing an SVG path, do our best to revtrieve points along
  that path. Bezier curves are approximated as accurately as needed (defined by
  :curve-resolution).

  Arcs (A/a) commands are not supported.
  Z/z in the middle of the path is not supported.

  Both these cases can be ignored with :ignore-errors t.
  
  If the path generates an arc between x1,y1 and x2,y2, we just ignore the whole
  arc thing and set x2,y2 as the next point in the path.

  If Z/z ends the path in the middle, we silently return the current set of 
  points without continuing the path. The idea here is we are generating
  polygons so breaks or cutouts are not acceptable."
  (let ((commands (cl-ppcre:split "(?=[a-zA-Z])" 
                                  (cl-ppcre::regex-replace-all "\\s+" str-data "")))
        (points nil)
        (first-point nil)
        (cur-point nil)
        (is-closed nil)
        (last-anchor nil))
    (dolist (cmd-str commands)
      ;; check if we have an "already closed" parsing error
      (when is-closed
        (if ignore-errors
            (return)
            (error 'unsupported-path-command :text (format nil "Path tried to run commands after closing. This is unsupported by this library. Pass in :ignore-errors t to work around this: ~a" str-data))))

      ;; this (let) splits the command from "M-113-20" to
      ;; ("M" "-113" "-20")
      (let* ((cmd-parts (cl-ppcre:split "((?<=[A-Za-z])|(?=\-)|,)" cmd-str))
             (cmd (aref (car cmd-parts) 0))
             (args (mapcar (lambda (a) (read-from-string a)) (cdr cmd-parts)))
             (cur-x (car cur-point))
             (cur-y (cadr cur-point)))
        ;; process the commands (http://www.w3.org/TR/SVG/paths.html)
        (case cmd
          (#\M
           (setf cur-point args)
           (push cur-point points))
          (#\m
           (setf cur-point (list (+ cur-x (car args))
                                 (+ cur-y (cadr args))))
           (push cur-point points))
          (#\L
           (setf cur-point args)
           (push cur-point points))
          (#\l
           (setf cur-point (list (+ cur-x (car args))
                                 (+ cur-y (cadr args))))
           (push cur-point points))
          (#\H
           (setf (car cur-point) (car args))
           (push cur-point points))
          (#\h
           (setf (car cur-point) (+ cur-x (car args)))
           (push cur-point points))
          (#\V
           (setf (cadr cur-point) (car args))
           (push cur-point points))
          (#\v
           (setf (cadr cur-point) (+ cur-y (car args)))
           (push cur-point points))
          (#\C
           (let ((x1 (car args))
                 (y1 (cadr args))
                 (x2 (nth 2 args))
                 (y2 (nth 3 args))
                 (x (nth 4 args))
                 (y (nth 5 args)))
             (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                   last-anchor (list x2 y2)
                   cur-point (list x y))))
          (#\c
           (let ((x1 (+ (car args) cur-x))
                 (y1 (+ (cadr args) cur-y))
                 (x2 (+ (nth 2 args) cur-x))
                 (y2 (+ (nth 3 args) cur-y))
                 (x (+ (nth 4 args) cur-x))
                 (y (+ (nth 5 args) cur-y)))
             (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                   last-anchor (list x2 y2)
                   cur-point (list x y))))
          (#\S
           (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                 (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                 (x2 (car args))
                 (y2 (cadr args))
                 (x (nth 2 args))
                 (y (nth 3 args)))
             (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                   last-anchor (list x2 y2)
                   cur-point (list x y))))
          (#\s
           (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                 (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                 (x2 (+ (car args) cur-x))
                 (y2 (+ (cadr args) cur-y))
                 (x (+ (nth 2 args) cur-x))
                 (y (+ (nth 3 args) cur-y)))
             (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                   last-anchor (list x2 y2)
                   cur-point (list x y))))
          (#\Q
           (let ((x1 (car args))
                 (y1 (cadr args))
                 (x (nth 2 args))
                 (y (nth 3 args)))
             (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                   last-anchor (list x1 y1)
                   cur-point (list x y))))
          (#\q
           (let ((x1 (+ (car args) cur-x))
                 (y1 (+ (cadr args) cur-y))
                 (x (+ (nth 2 args) cur-x))
                 (y (+ (nth 3 args) cur-y)))
             (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                   last-anchor (list x1 y1)
                   cur-point (list x y))))
          (#\T
           (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                 (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                 (x (car args))
                 (y (cadr args)))
             (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                   last-anchor (list x1 y1)
                   cur-point (list x y))))
          (#\t
           (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                 (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                 (x (+ (car args) cur-x))
                 (y (+ (cadr args) cur-y)))
             (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                   last-anchor (list x1 y1)
                   cur-point (list x y))))
          (#\A
           ;; not implemented, so if ignoring errors, just grab the next x,y values and keep going
           (if ignore-errors
               (push (list (nth 5 args)
                           (nth 6 args)) points)
               (error 'unsupported-path-command :text (format nil "Actions A/a (elliptical arc) are not supported yet. Please form and add them yourself if you need them: ~a" str-data))))
          (#\a
           ;; not implemented, so if ignoring errors, just grab the next x,y values and keep going
           (if ignore-errors
               (push (list (nth 5 args)
                           (nth 6 args)) points)
               (error 'unsupported-path-command :text (format nil "Actions A/a (elliptical arc) are not supported yet. Please form and add them yourself if you need them: ~a" str-data))))
          (#\z (setf is-closed t))
          (#\Z (setf is-closed t))))
      (when (= (length points) 1)
        (setf first-point (car points))))
    (reverse (if (equal (car points) first-point)
                 (cdr points)
                 points))))

(defun bezier-cubic (x1 y1 x2 y2 ax1 ay1 ax2 ay2 &key (resolution 10))
  "Sample resolution points off of a cubic bezier curve from (x1,y1) to (x2,y2)
  using anchor points (ax1,ay1) (ax2,ay2)."
  (let ((points nil))
    (flet ((cubic (t-val p0 p1 p2 p3)
             (+ (* (expt (- 1 t-val) 3) p0)
                (* 3 (expt (- 1 t-val) 2) t-val p1)
                (* 3 (- 1 t-val) (expt t-val 2) p2)
                (* (expt t-val 3) p3))))
      (dotimes (i resolution)
        (let ((t-val (* (1+ i) (/ 1 resolution))))
          (push (list (cubic t-val x1 ax1 ax2 x2)
                      (cubic t-val y1 ay1 ay2 y2))
                points))))
    points))

(defun bezier-quadratic (x1 y1 x2 y2 ax1 ay1 &key (resolution 10))
  "Sample resolution points off of a quadratic bezier curve from (x1,y1) to
  (x2,y2) using anchor points (ax1,ay1) (ax2,ay2)."
  (let ((points nil))
    (flet ((quadratic (t-val p0 p1 p2)
             (+ (* (expt (- 1 t-val) 2) p0)
                (* 2 (- 1 t-val) t-val p1)
                (* (expt t-val 2) p2))))
      (dotimes (i resolution)
        (let ((t-val (* (1+ i) (/ 1 resolution))))
          (push (list (quadratic t-val x1 ax1 x2)
                      (quadratic t-val y1 ay1 y2)) points))))
    points))

(defun get-points-from-ellipse (x y rx ry &key (curve-resolution 20))
  "Calculate curve-resolution points along an ellipse. Can be used for circles
  too (when rx == ry)."
  (let ((points (make-array curve-resolution)))
    (dotimes (i curve-resolution)
      (let ((rad (* i (/ (* 2 PI) curve-resolution))))
        (setf (aref points i)
              (list (coerce (+ x (* (cos rad) rx)) 'single-float)
                    (coerce (+ y (* (sin rad) ry)) 'single-float)))))
    points))

(defun apply-transformations (points node groups)
  (declare (ignore node groups))
  points)

(defun tagname (obj)
  "Get the tag name of an object."
  (if (listp obj)
      (tagname (car obj))
      obj))

(defun get-node-attr (node attr-name)
  "Given a node, get the attribute stored under attr-name."
  (let ((val nil))
    (dolist (attr (cadr node))
      (when (equal (car attr) attr-name)
        (setf val (cadr attr))
        (return)))
    val))

(defun parse-transform (transform)
  "Turn a transform(...) into a matrix that can be multiplied with other
  transform matricies."
  transform)

(defun parse-svg-nodes (nodes &optional parent-group (next-id 0))
  "Given an SVG doc read via xmls:parse, return two things:

    1. A list of plist objects describing ALL the objects found in the SVG file.
       Each object stores the group it's part of along with its attributes and
       transformations.
    2. A list of plist objects describing ALL the groups found, each storing its
       group id (created if not explicit) and any transformations that group has.
  
  The idea is that given this data, we can easily generate polygons for each
  object and then apply transformations to it starting with its top-level group
  and working down to the object's transformations itself."
  (let ((objs nil)
        (groups nil))
    (dolist (node (cddr nodes))
      (let ((tag (tagname node)))
        (if (equal tag "g")
            (let* ((gid (get-node-attr node "id"))
                   (gid (list (if gid gid (incf next-id))))
                   (full-gid (if parent-group
                                 (append parent-group gid)
                                 gid)))
              (multiple-value-bind (sub-nodes sub-groups) (parse-svg-nodes node full-gid next-id)
                (setf objs (append sub-nodes objs))
                (push (list :group gid :transform (parse-transform (get-node-attr node "transform")) :groups sub-groups) groups)))
            (let* ((gid parent-group)
                   (obj (list :type tag :group gid))
                   (attrs (append (case (intern (string-upcase tag))
                                    (rect (list "x" "y" "width" "height"))
                                    (polygon (list "points"))
                                    (path (list "d"))
                                    (ellipse (list "cx" "cy" "rx" "ry"))
                                    (circle (list "cx" "cy" "r"))
                                    (t nil))
                                  (list "transform" "fill"))))
              (when attrs
                (push (append obj (loop for attr in attrs
                                        for val = (get-node-attr node attr)
                                        if val append (list (read-from-string (format nil ":~a" attr)) val)))
                      objs))))))
    (values objs groups)))

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
  returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun parse-svg-string (svg-str &key (curve-resolution 10) ignore-errors)
  "Parses an SVG string, creating the nodes and groups from the SVG, then
  converts each object into a set of points using the data in that object and
  the transformations from the groups the object belongs to (and the object's
  own transformations).

  SVG object curve resolutions can be set via :curve-resolution (the higher the
  value, the more accurate curves are). :ignore-errors t ignores some errors
  with unimplemented features in the SVG parsing...setting this to T will most
  likely be OK, but may lose some of the data."
  (multiple-value-bind (nodes groups)
      (parse-svg-nodes (xmls:parse svg-str))
    (mapcar (lambda (node)
              (let* ((points (convert-to-points node :curve-resolution curve-resolution :ignore-errors ignore-errors))
                     (points (apply-transformations points node groups)))
                (append node (list :point-data points))))
            nodes)))

(defun parse-svg-file (filename &key (curve-resolution 10) ignore-errors)
  "Simple wrapper around parse-svg-string.
  
  SVG object curve resolutions can be set via :curve-resolution (the higher the
  value, the more accurate curves are). :ignore-errors t ignores some errors
  with unimplemented features in the SVG parsing...setting this to T will most
  likely be OK, but may lose some of the data."
  (parse-svg-string (file-contents filename) :curve-resolution curve-resolution :ignore-errors ignore-errors))


