(in-package :svg-load)

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


