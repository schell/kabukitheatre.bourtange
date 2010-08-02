(in-package :bourtange)

(defclass display-object ()
  ((origin :initarg :origin :initform (make-point) :accessor origin)
   (last-origin :initarg :last-origin :initform (make-point) :accessor last-origin)
   (color :initarg :color :initform (make-color) :accessor color)
   (primitive :initarg :primitive :initform :polygon :accessor primitive
              :documentation "The opengl primitive to draw this object with")
   (vertices :initarg :vertices :initform (make-square 10) :accessor vertices)))
(defgeneric initialize (object)
  (:documentation "Initializes a display-object")
  (:method ((object display-object))
    (setf (origin object) (make-point)
          (vertices object) (list (make-point)))))
(defgeneric draw (object)
  (:documentation "Draws the display-object with OpenGL"))
(defgeneric set-origin (object coords)
  (:documentation "Sets the origin of the object and returns the object")
  (:method ((object display-object) (coords point))
    (setf (origin object) coords)
    object))
(defgeneric collidedp (o1 o2)
  (:documentation "Checks to see if the objects collided, returns the object that collided or nil"))

(defun draw-list (list)
  (map nil #'draw list))

(defclass display-circle (display-object)
  ((radius :initarg :radius :initform 10 :accessor radius))
  (:default-initargs :vertices (make-circle)))
(defun make-display-circle (&optional (radius 1) (color (make-color)))
  "Utility function that creates a new circle of radius 'radius and color 'color"
  (make-instance 'display-circle :radius radius :vertices (make-circle) :color color))
;;
(defmethod draw ((object display-circle))
  (with-slots (color (scale radius) origin vertices primitive)
      object
    (with-slots (r g b a)
        color
      (let ((ox (x origin))
            (oy (y origin)))
        (gl:color r b g a)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:translate ox oy 0)
        (gl:scale scale scale 1)
        (gl:with-primitive primitive
          (dolist (vertex vertices)
            (gl:vertex (x vertex) (y vertex) 0)))
        (gl:flush))))
  (gl:load-identity))
;;
(defmethod collidedp ((o1 display-circle) (o2 display-circle))
  (let* ((combined-radii (+ (radius o1) (radius o2)))
         (broad-phase-hit (< (distance (origin o1) (origin o2)) combined-radii)))
    (if (not broad-phase-hit)
        (when (> (distance (last-origin o1) (origin o1)) (radius o2))
          (let* ((closest-point (point-in-line-closest-to-point (last-origin o1) (origin o1) (origin o2))))
            (return-from collidedp
              (when (< (distance closest-point (origin o2)) (radius o2))
                (list o1 o2)))))
        (list o1 o2))))
(defclass display-ring (display-circle)
  ((inner-radius :initarg :inner-radius :initform 100 :accessor inner-radius)
   (outer-radius :initarg :outer-radius :initform 100 :accessor outer-radius)))
(defun make-display-ring (outer-radius inner-radius &optional (color (make-color)))
  "Creates a ring"
  (let* ((inner (make-arc (/ inner-radius outer-radius)))
         (outer (make-arc))
         (vertices (mapcan #'list inner outer)))
    (make-instance 'display-ring :radius outer-radius
                   :outer-radius outer-radius
                   :inner-radius inner-radius
                   :vertices vertices
                   :color color)))
(defmethod draw ((this display-ring))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (x (origin this)) (y (origin this)) 0)
  (gl:scale (outer-radius this) (outer-radius this) 1)
  (draw-point-list (vertices this) :triangle-strip (color this))
  (gl:flush))

(defclass display-arc (display-circle)
  ((start-radian :initarg :start-radian :initform 0 :accessor start-radian)
   (end-radian :initarg :end-radian :initform *tau* :accessor end-radian))
  (:default-initargs :vertices (make-arc)))
(defun make-display-arc (radius &optional (start-radian 0) (end-radian *tau*) (color (make-color)))
  "Utility function to create an arc"
  (make-instance 'display-arc
                 :radius radius
                 :start-radian start-radian :end-radian end-radian
                 :color color
                 :vertices (make-arc radius start-radian end-radian)))

(defclass physical-object ()
  ((origin :initarg :origin :initform (make-point) :accessor origin)
   (velocity :initarg :velocity :initform (make-point) :accessor velocity)
   (acceleration :initarg :acceleration :initform (make-point) :accessor acceleration)
   (density :initarg :density :initform 1 :accessor density)))
(defgeneric apply-velocity (this milliseconds)
  (:documentation "Applies the this's velocity to its position")
  (:method ((this physical-object) milliseconds)
    (let* ((meters-moved (multiply (velocity this) milliseconds)))
      (setf (last-origin this) (origin this)
            (origin this) (add (origin this) meters-moved)))
    this))
(defgeneric apply-acceleration (this)
  (:documentation "Applies the this's acceleration to its velocity, returns this")
  (:method ((this physical-object))
    (setf (velocity this) (add (velocity this) (acceleration this)))
    this))
(defgeneric apply-all (this milliseconds)
  (:documentation "Applies acceleration and velocity to the this")
  (:method ((this physical-object) milliseconds)
    (apply-acceleration this)
    (apply-velocity this milliseconds)
    this))

