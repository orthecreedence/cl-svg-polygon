(in-package :svg-load)

(defun parse-transform (transform)
  "Turn a transform(...) into an easily-parsable list structure."
  ;; convert "translate(-10,-20) scale(2) rotate(45) translate(5,10)" into
  ;; "(translate -10 -20) (scale 2) (rotate 45) (translate 5 10)"
  ;; (ie read-from-string'able)
  (let* ((transform (cl-ppcre::regex-replace-all "([a-z]+)\\(" transform "(\\1 "))
         (transform (cl-ppcre::regex-replace-all "," transform " ")))
    (read-from-string (format nil "( ~a )" transform))))

(defun get-transformations (object groups)
  "Given an SVG object and a tree of groups, grab all transformations, starting
   from the top down, into a flat list so they can be applied sequentially."
  (let ((object-transform (getf object :transform))
        (object-group (getf object :group))
        (transformations nil))
    (labels ((traverse-groups (path groups)
               (dolist (group groups)
                 (when (eql (car (getf group :group)) (car path))
                   (let* ((groups (getf group :groups))
                          (transform (getf group :transform))
                          (transform (if (listp transform) (car transform) transform)))
                     (when transform
                       (push transform transformations))
                     (when groups
                       (traverse-groups (cdr path) groups)))))))
      (traverse-groups object-group groups))
    (when object-transform
      (push object-transform transformations))
    (let ((flattened-transforms nil))
      (dolist (transforms transformations)
        (dolist (transform transforms)
          (push transform flattened-transforms)))
      flattened-transforms)))

(defun get-matrix-from-transformation (transformation)
  (macrolet ((idx (var idx default)
               (let ((name (gensym)))
                 `(let ((,name (nth ,idx ,var)))
                    (if ,name ,name ,default)))))
    (case (car transformation)
      (matrix (vector (nth 1 transformation) (nth 3 transformation) (nth 5 transformation)
                      (nth 2 transformation) (nth 4 transformation) (nth 6 transformation)
                      0 0 1))
      (translate (m-translate (nth 1 transformation) (idx transformation 2 0)))
      (scale (m-scale (nth 1 transformation) (idx transformation 2 0))))))

(defun apply-transformations (points object groups)
  "Apply all transformations for an object, starting from its top-level group
  and working down to the object itself."
  (let ((transformations (get-transformations object groups))
        (matrix (id-matrix 3)))
    (dolist (transform transformations)
      (setf matrix (mat* matrix (get-matrix-from-transformation transform))))
    matrix))

(multiple-value-bind (nodes groups)
    (parse-svg-nodes (xmls:parse (file-contents "world1.svg")))
  (progn
    (let ((node (nth 12 nodes)))
      (apply-transformations nil node groups))))

