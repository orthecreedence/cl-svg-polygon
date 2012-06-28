(in-package :cl-svg-polygon)

(defun id-matrix (dims)
  "Return a square identity matrix with the specified "
  (let ((array (make-array (* dims dims) :initial-element 0.0 :element-type 'single-float)))
    (dotimes (d dims)
      (setf (aref array (* d (1+ dims))) 1.0))
    array))

(defun mat* (m1 m2)
  "Multiply 3x3 matrices m1 by m2."
  (let ((new (make-array 9 :initial-element 0.0 :element-type 'single-float)))
    (dotimes (x 3)
      (dotimes (y 3)
        (let ((prod (+ (* (aref m1 (* x 3)) (aref m2 y))
                       (* (aref m1 (+ (* x 3) 1)) (aref m2 (+ y 3)))
                       (* (aref m1 (+ (* x 3) 2)) (aref m2 (+ y 6))))))
          (setf (aref new (+ y (* x 3))) (coerce prod 'single-float)))))
    new))

(defun matv* (m v)
  "Multiple a matrix by a vector, return the resulting vector."
  (let ((new (make-list 3))
        (vx (car v))
        (vy (cadr v))
        (vz 1))
    (dotimes (i 3)
      (setf (nth i new) (+ (* vx (aref m (* i 3)))
                           (* vy (aref m (+ (* i 3) 1)))
                           (* vz (aref m (+ (* i 3) 2))))))
    new))

(defun m-rotate (degrees)
  "Generate a rotation matrix."
  (let* ((matrix (id-matrix 3))
         (angle-rad (* (mod degrees 360) (/ PI 180)))
         (cos (coerce (cos angle-rad) 'single-float))
         (sin (coerce (sin angle-rad) 'single-float)))
    (setf (aref matrix 0) cos
          (aref matrix 1) (- sin)
          (aref matrix 3) sin
          (aref matrix 4) cos)
    matrix))

(defun m-scale (x y)
  "Generate a scaling matrix."
  (let ((matrix (id-matrix 3)))
    (setf (aref matrix 0)  (coerce x 'single-float)
          (aref matrix 4)  (coerce y 'single-float))
    matrix))
  
(defun m-translate (x y)
  "Generate a translation matrix."
  (let ((translatrix (id-matrix 3)))
    (setf (aref translatrix 2) (coerce x 'single-float)
          (aref translatrix 5) (coerce y 'single-float))
    translatrix))

(defun m-skew (degrees &key (axis :x))
  "Generate a skew matrix along the :axis axis (:x or :y)."
  (let ((matrix (id-matrix 3))
        (angle-rad (* (mod degrees 360) (/ PI 180)))
        (idx (if (equal axis :x) 1 3)))
    (setf (aref matrix idx) (tan angle-rad))
    matrix))

