(in-package :bourtange)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))
(defun make-point (&optional (x 0) (y 0))
  "Utility function. Creates a point at (x, y)"
  (make-instance 'point :x x :y y))
(defmethod multiply ((p point) s)
  (make-point (* (x p) s) (* (y p) s)))
(defgeneric add (p1 p2)
  (:documentation "Adds to points together")
  (:method ((p1 point) (p2 point))
    (make-point (+ (x p1) (x p2)) (+ (y p1) (y p2)))))
(defun rad->xy (theta)
  "Utility function. Converts a radian measure to vector (list x y)"
  (make-point (cos theta) (sin theta)))
(defgeneric magnitude (p1)
  (:documentation "Returns the magnitude of the vector p1")
  (:method ((p1 point))
    (sqrt (+ (* (x p1) (x p1)) (* (y p1) (y p1))))))
(defgeneric distance-vector (p1 p2)
  (:documentation "Returns the distance between p1 and p2 as a vector")
  (:method ((p1 point) (p2 point))
    (make-point (- (x p2) (x p1)) (- (y p2) (y p1)))))
(defgeneric distance (p1 p2)
  (:documentation "Returns the absolute distance between p1 and p2")
  (:method ((p1 point) (p2 point))
    (magnitude (distance-vector p1 p2))))
(defgeneric unitize (p1)
  (:documentation "Returns the unit vector of v")
  (:method ((v point))
    (let ((mag (magnitude v)))
      (make-point (/ (x v) mag) (/ (y v) mag)))))
(defgeneric point-in-line-closest-to-point (p1 p2 p3)
  (:documentation "Returns the point on a line 'p2 - 'p1 closest to point 'p3")
  (:method ((p1 point) (p2 point) (p3 point))
    (let* ((d-p1-p2 (distance p1 p2))
           (mag2 (* d-p1-p2 d-p1-p2))
           (u (/ (- (* (- (x p3) (x p1)) (- (x p2) (x p1)))
                    (* (- (y p3) (y p1)) (- (y p2) (y p1))))
                 mag2)))
      (make-point (+ (x p1) (* u (- (x p2) (x p1))))
                  (+ (y p1) (* u (- (y p2) (y p1))))))))

(defun make-square (half-width)
  "Utility function. Creates a list of 4 points arranged in a square"
  (let ((hw half-width)
        (-hw (* -1 half-width)))
    (list (make-point hw hw)
          (make-point hw -hw)
          (make-point -hw -hw)
          (make-point -hw hw))))

(defun make-circle (&optional (radius 1) (points 360))
  "Creates a circle of 'points points"
  (loop for x from 1 to points collect (multiply (rad->xy (* x (/ *tau* points))) radius)))

(defun make-arc (&optional (radius 1) (start-radian 0) (end-radian *tau*) (points 360))
  "Creates an arc"
  (cons (multiply (rad->xy start-radian) radius)
        (loop for x from 2 to points
           collect (multiply (rad->xy (+ start-radian
                                         (* x (/ (- end-radian start-radian) points))))
                             radius))))

(defun draw-point-list (points &optional (primitive :polygon) (color (make-color)))
  "Draws a list of points."
  (gl:color (r color) (b color) (g color) (a color))
  (gl:with-primitive primitive
    (dolist (point points)
      (gl:vertex (x point) (y point) 0)))
  (gl:flush))

(defun offset-point-list (points p)
  "Adds a point to each point in the list and returns the new offset list"
  (mapcar (lambda (point) (add point p))
          points))

