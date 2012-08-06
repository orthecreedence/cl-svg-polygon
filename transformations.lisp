(in-package :cl-svg-polygon)

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
    transformations))

(defun get-matrix-from-transformation (transformation)
  "Given a transformation in list form (FN ARG1 ARG2 ...), turn it into a matrix
  which can be multipled to give the overall transformation for an object."
  (macrolet ((idx (var idx default)
               (let ((name (gensym)))
                 `(let ((,name (nth ,idx ,var)))
                    (if ,name ,name ,default)))))
    (let ((transformation (if (listp (car transformation))
                              (car transformation)
                              transformation)))
      (case (intern (write-to-string (car transformation)) :cl-svg-polygon)
        (matrix (vector (nth 1 transformation) (nth 3 transformation) (nth 5 transformation)
                        (nth 2 transformation) (nth 4 transformation) (nth 6 transformation)
                        0 0 1))
        (translate (m-translate (nth 1 transformation) (idx transformation 2 0)))
        (scale (m-scale (nth 1 transformation) (idx transformation 2 0)))
        (rotate (let ((angle (nth 1 transformation))
                      (center-x (idx transformation 2 0))
                      (center-y (idx transformation 3 0)))
                  (if (and (eq 0 center-x) (eq 0 center-y))
                      ;; just rotate, no offset funny business
                      (m-rotate angle)
                      (mat* (mat* (m-translate center-x center-y) (m-rotate angle)) (m-translate (- center-x) (- center-y))))))
        (skewx (m-skew (nth 1 transformation) :axis :x))
        (skewy (m-skew (nth 1 transformation) :axis :y))))))

(defun apply-transformations (points object groups &key scale)
  "Apply all transformations for an object, starting from its top-level group
  and working down to the object itself."
  (let ((transformations (get-transformations object groups))
        (matrix (id-matrix 3))
        (trans-points nil))
    (dolist (transform transformations)
      (setf matrix (mat* (get-matrix-from-transformation transform) matrix)))
    (when scale
      (setf matrix (mat* (m-scale (car scale) (cadr scale)) matrix)))
    (loop for p across points do
      (push (butlast (matv* matrix (append p '(1)))) trans-points))
    (values (reverse trans-points)
            matrix)))

