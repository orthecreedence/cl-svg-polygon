(dolist (p '(xmls split-sequence))
  (ql:quickload p))

(defpackage :svg-load
  (:use :cl)
  (:export :parse-groups))
(in-package :svg-load)

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
  returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defparameter *doc* (xmls:parse (file-contents "house2.svg")))

(defun tagname (obj)
  (if (listp obj)
      (tagname (car obj))
      obj))

(defun get-node-attr (node attr-name)
  (let ((val nil))
    (dolist (attr (cadr node))
      (when (equal (car attr) attr-name)
        (setf val (cadr attr))
        (return)))
    val))

(defun parse-transform (transform)
  transform)

(defun parse-svg-nodes (nodes &optional parent-group (next-id 0))
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
              (multiple-value-bind (sub-nodes sub-groups) (parse-groups node full-gid next-id)
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
                                    (t nil)) (list "transform" "fill"))))
              (when attrs
                (push (append obj
                              (loop for attr in attrs
                                    for val = (get-node-attr node attr)
                                    if val
                                       append (list (read-from-string (format nil ":~a" attr)) val)))
                      objs))))))
    (values objs groups)))

(defmacro with-plist-string-reads (plist bindings &body body)
  `(let ,(loop for binding in bindings collect (list (car binding)
                                                     `(read-from-string (getf ,plist ,(cadr binding)))))
     ,@body))

(defun convert-to-points (obj &key curve-resolution)
  (case (intern (string-upcase (getf obj :type)))
    (rect
      (with-plist-string-reads obj ((x :x) (y :y) (w :width) (h :height)) 
        (vector (list x y) (list (+ x w) (- y h)))))
    (polygon
      (let* ((pairs (split-sequence:split-sequence #\space (getf obj :points)))
             (points (loop for pair in pairs
                           if (find #\, pair) collect (split-sequence:split-sequence #\, pair))))
        (coerce points 'vector)))
    (path
      (get-points-from-path (getf obj :d) :curve-resolution curve-resolution))
    (ellipse 
      (with-plist-string-reads obj ((x :cx) (y :cy) (rx :rx) (ry :ry))
        (get-points-from-ellipse x y rx ry :curve-resolution curve-resolution)))
    (circle
      (with-plist-string-reads obj ((x :cx) (y :cy) (r :r))
        (get-points-from-ellipse x y r r :curve-resolution curve-resolution)))))

(defun get-points-from-path (str-data &key curve-resolution))

(defun get-points-from-ellipse (x y rx ry &key curve-resolution))



