(in-package :cl-svg-polygon)

(define-condition unsupported-path-command (error)
  ((text :initarg :text :reader text))
  (:documentation "Thrown when an unsupported action/feature is parsed in a path."))

(defun points-close-equal-p (point1 point2 &key (precision 10))
  "Determine if two points are (about) the same. Yes, this is open to
   interpretation, which is why it takes a precision argument =]."
  (flet ((round-point (point)
           (mapcar (lambda (x) (/ (floor (* x precision)) precision)) point)))
    (equal (round-point point1) (round-point point2))))

(defun replace-char (char rep str)
  "Replace all instances of char with rep in str (non-destructive)."
  (let ((new-str (make-string (length str))))
    (loop for i from 0
          for c across str do
      (setf (aref new-str i) (if (eq c char)
                                 rep
                                 c)))
    new-str))

(defmacro cmd-repeat (args-and-count &body body)
  "Some commands can repeat values with the command, namely the curve commands:
       c,1,2,4,4,5,5 c,8,8,3,4,3,1
    can be written as
       c,1,2,4,4,5,5,8,8,3,4,3,1
  yay. This macro helps alleviate some of the issues caused by this wonderful
  feature in the get-points-from-path function."
  (let ((i (gensym))
        (a (gensym))
        (args (car args-and-count))
        (count (cadr args-and-count)))
    `(dotimes (,i (floor (/ (length ,args) ,count)))
       ,@body
       (setf cur-x (car cur-point)
             cur-y (cadr cur-point))
       (dotimes (,a ,count)
         (setf ,args (cdr ,args))))))

(defun get-points-from-path (str-data &key (curve-resolution 10))
  "Given a string describing an SVG path, do our best to revtrieve points along
  that path. Bezier curves are approximated as accurately as needed (defined by
  :curve-resolution).

  If the path generates an arc between x1,y1 and x2,y2, we just ignore the whole
  arc thing and set x2,y2 as the next point in the path.

  If Z/z ends the path in the middle, we silently return the current set of 
  points without continuing the path. The idea here is we are generating
  polygons so breaks or cutouts are not acceptable."
  (let ((commands (cl-ppcre:split "(?=[a-zA-Z])" str-data))
        (scanner-empty-p (cl-ppcre:create-scanner "^[\s\n\r ]+$" :multi-line-mode t))
        (points nil)
        (parts nil)
        (first-point nil)
        (cur-point '(0 0))
        (last-anchor nil)
        (disconnected nil))
    (dolist (cmd-str commands)
      ;; this (let) splits the command from "M-113-20" to
      ;; ("M" "-113" "-20")
      (let* ((cmd-parts (cl-ppcre:split "( |,|(?<=[A-Za-z])|(?=\-))" cmd-str))
             (cmd (aref (car cmd-parts) 0))
             ;(forget (format t "cmd: ~s~%" cmd-parts))
             (args (mapcar (lambda (a)
                             (if (cl-ppcre:scan scanner-empty-p a)
                                 nil
                                 (read-from-string a)))
                           (cdr cmd-parts)))
             (cur-x (car cur-point))
             (cur-y (cadr cur-point)))
        ;; process the commands (http://www.w3.org/TR/SVG/paths.html)
        (case (if (eq cmd #\z)
                  (aref (string-upcase cmd) 0)
                  cmd)
          (#\M
           (cmd-repeat (args 2)
             (setf cur-point args)
             (push cur-point points)))
          (#\m
           (cmd-repeat (args 2)
             (setf cur-point (list (+ cur-x (car args))
                                   (+ cur-y (cadr args))))
             (push cur-point points)))
          (#\L
           (cmd-repeat (args 2)
             (setf cur-point args)
             (push cur-point points)))
          (#\l
           (cmd-repeat (args 2)
             (setf cur-point (list (+ cur-x (car args))
                                   (+ cur-y (cadr args))))
             (push cur-point points)))
          (#\H
           (cmd-repeat (args 1)
             (setf (car cur-point) (car args))
             (push cur-point points)))
          (#\h
           (cmd-repeat (args 1)
             (setf (car cur-point) (+ cur-x (car args)))
             (push cur-point points)))
          (#\V
           (cmd-repeat (args 1)
             (setf (cadr cur-point) (car args))
             (push cur-point points)))
          (#\v
           (cmd-repeat (args 1)
             (setf (cadr cur-point) (+ cur-y (car args)))
             (push cur-point points)))
          (#\C
           (cmd-repeat (args 6)
             (let ((x1 (car args))
                   (y1 (cadr args))
                   (x2 (nth 2 args))
                   (y2 (nth 3 args))
                   (x (nth 4 args))
                   (y (nth 5 args)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\c
           (cmd-repeat (args 6)
             (let ((x1 (+ (car args) cur-x))
                   (y1 (+ (cadr args) cur-y))
                   (x2 (+ (nth 2 args) cur-x))
                   (y2 (+ (nth 3 args) cur-y))
                   (x (+ (nth 4 args) cur-x))
                   (y (+ (nth 5 args) cur-y)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\S
           (cmd-repeat (args 4)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x2 (car args))
                   (y2 (cadr args))
                   (x (nth 2 args))
                   (y (nth 3 args)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\s
           (cmd-repeat (args 4)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x2 (+ (car args) cur-x))
                   (y2 (+ (cadr args) cur-y))
                   (x (+ (nth 2 args) cur-x))
                   (y (+ (nth 3 args) cur-y)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\Q
           (cmd-repeat (args 4)
             (let ((x1 (car args))
                   (y1 (cadr args))
                   (x (nth 2 args))
                   (y (nth 3 args)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\q
           (cmd-repeat (args 4)
             (let ((x1 (+ (car args) cur-x))
                   (y1 (+ (cadr args) cur-y))
                   (x (+ (nth 2 args) cur-x))
                   (y (+ (nth 3 args) cur-y)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\T
           (cmd-repeat (args 2)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x (car args))
                   (y (cadr args)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\t
           (cmd-repeat (args 2)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x (+ (car args) cur-x))
                   (y (+ (cadr args) cur-y)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\A
           (cmd-repeat (args 7)
             (let ((rx (car args))
                   (ry (cadr args))
                   (x-rot (caddr args))
                   (large-arc (cadddr args))
                   (sweep-flag (cadr (cdddr args)))
                   (x1 (car cur-point))
                   (y1 (cadr cur-point))
                   (x2 (+ (caddr (cdddr args)) (car cur-point)))
                   (y2 (+ (cadddr (cdddr args)) (cadr cur-point))))
               (setf points (append (elliptical-arc x1 y1 x2 y2 rx ry x-rot large-arc sweep-flag :resolution curve-resolution) points)
                     cur-point (list x2 y2)))))
          (#\a
           (cmd-repeat (args 7)
             (let ((rx (car args))
                   (ry (cadr args))
                   (x-rot (caddr args))
                   (large-arc (cadddr args))
                   (sweep-flag (cadr (cdddr args)))
                   (x1 (car cur-point))
                   (y1 (cadr cur-point))
                   (x2 (+ (caddr (cdddr args)) (car cur-point)))
                   (y2 (+ (cadddr (cdddr args)) (cadr cur-point))))
               (setf points (append (elliptical-arc x1 y1 x2 y2 rx ry x-rot large-arc sweep-flag :resolution curve-resolution) points)
                     cur-point (list x2 y2)))))
          (#\Z
           (push (coerce (reverse (if (points-close-equal-p (car points) first-point)
                                      (cdr points)
                                      points)) 'vector) parts)
           (setf points nil))))
      (when (= (length points) 1)
        (setf first-point (car points))))
    (when (not (zerop (length points)))
      ;; we have unfinished points. add them to the part list
      (setf disconnected t)
      (push (coerce (reverse (if (points-close-equal-p (car points) first-point)
                                 (cdr points)
                                 points)) 'vector) parts))
    (values (reverse parts) disconnected)))

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

(defun elliptical-arc (x1 y1 x2 y2 rx ry x-rotation large-arc-flag sweep-flag &key (resolution 10))
  "Calculate an arc in a path. Yuck."
  (let ((rot-mat-i (m-rotate x-rotation :reverse t))
        (rot-mat (m-rotate x-rotation)))
    ;; calculate a bunch of crap, mainly ellipse center x,y
    (let* ((xy-i (matv* rot-mat-i (list (/ (- x1 x2) 2)
                                        (/ (- y1 y2) 2))))
           (x-i (car xy-i))
           (y-i (cadr xy-i))
           (rx2 (expt rx 2))
           (ry2 (expt ry 2))
           (x-i2 (expt x-i 2))
           (y-i2 (expt y-i 2))
           (cxy-m (expt (/ (- (* rx2 ry2) (* rx2 y-i2) (* ry2 x-i2))
                           (+ (* rx2 y-i2) (* rx2 x-i2)))
                        .5))
           (cxy-m (if (eq large-arc-flag sweep-flag)
                      (- cxy-m)
                      cxy-m))
           (cx-i (* cxy-m (/ (* rx y-i) ry)))
           (cy-i (* cxy-m (/ (* ry x-i) (- rx))))
           (cxy (matv* rot-mat (list cx-i cy-i)))
           (cx (+ (car cxy) (/ (+ x1 x2) 2)))
           (cy (+ (cadr cxy) (/ (+ y1 y2) 2))))
      (flet ((angle (v1 v2)
               (let ((x1 (car v1))
                     (y1 (cadr v1))
                     (x2 (car v2))
                     (y2 (cadr v2)))
                 (let ((sign (if (< 0 (- (* x1 y2) (* y1 x2)))
                                 1
                                 -1)))
                   (* sign (acos (/ (dot-prod v1 v2)
                                    (* (norm v1) (norm v2)))))))))
        ;; calculate the start/delta angles
        (let ((theta-1 (angle (list 1 0) (list (/ (- x-i cx-i) rx)
                                               (/ (- y-i cy-i) ry))))
              (theta-delta (angle (list (/ (- x-i cx-i) rx)
                                        (/ (- y-i cy-i) ry))
                                  (list (/ (- (- x-i) cx-i) rx)
                                        (/ (- (- y-i) cy-i) ry)))))
          (let ((theta-step (/ theta-delta resolution))
                (points nil))
            ;; create our points for the ellipse. if this were a true
            ;; implementation, we'd do radii correction such that x2,y2 always
            ;; fall ON the ellipse path, but i truly do not care enough to
            ;; bother. if your SVG generator sucks, take it up with them, or
            ;; better yet do the proper calculations and issue a pull request.
            (dotimes (i resolution)
              (let ((angle (+ theta-1 (* theta-step i))))
                (let ((xy (matv* rot-mat (list (* rx (cos angle))
                                               (* ry (sin angle))))))
                  (push (list (+ (car xy) cx)
                              (+ (cadr xy) cy)) points))))
            ;; get the last point on there.
            (push (list x2 y2) points)
            (reverse points)))))))

