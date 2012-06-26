(in-package :svg-load)

(defun parse-transform (transform)
  "Turn a transform(...) into an easily-parsable list structure."
  ;; convert "translate(-10,-20) scale(2) rotate(45) translate(5,10)" into
  ;; "(translate -10 -20) (scale 2) (rotate 45) (translate 5 10)"
  ;; (ie read-from-string'able)
  (let* ((transform (cl-ppcre::regex-replace-all "([a-z]+)\\(" transform "(\\1 "))
         (transform (cl-ppcre::regex-replace-all "," transform " ")))
    (read-from-string (format nil "( ~a )" transform))))

(defun apply-transformations (points object groups)
  "Apply all transformations for an object, starting from its top-level group
  and working down to the object itself."
  points)
