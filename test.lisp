(defparameter *tau* 6.283185307179586)

(defgeneric multiply (x y)
  (:documentation "Multiplies two things together."))
  
(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun make-point (x y)
  "Utility function. Creates a point at (x, y)"
  (make-instance 'point :x x :y y))

(defmethod multiply ((p point) s)
  (make-point (* (x p) s) (* (y p) s)))
  
(defun rad->xy (theta)
  "Utility function. Converts a radian measure to vector (list x y)"
  (make-point (cos theta) (sin theta)))
  
(defun make-square (half-width)
  "Utility function. Creates a list of 4 points arranged in a square"
  (let ((hw half-width) (-hw (* -1 half-width)))
  (list (make-point hw hw)
   (make-point hw -hw)
   (make-point -hw -hw)
   (make-point -hw hw))))
   
(defun make-circle (radius &optional (points 360)) 
  "Utility function. Creates a circle of 'points points"
  (loop for x from points downto 1 collect (multiply (rad->xy (/ *tau* x)) radius)))

(dolist (p (make-circle 1 4))
    (format t "~%x: ~a y:~a" (x p) (y p)))
    
(print -)