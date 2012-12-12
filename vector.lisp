(in-package :cl-svg-polygon)

(defun norm (v)
  "Calculate a vector norm."
  (expt (loop for x in v sum (expt x 2)) .5))

(defun normalize (v)
  "Normalize a 2D vector"
  (let ((x (car v))
        (y (cadr v)))
    (let ((norm (norm v)))
      (list (/ x norm) (/ y norm)))))

(defun dot-prod (v1 v2)
  "Give the dot product of two 2D vectors."
  (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2))))

