(in-package :bourtange)

(defclass color ()
  ((r :initarg :r :initform 1 :accessor r)
   (b :initarg :b :initform 1 :accessor b)
   (g :initarg :g :initform 1 :accessor g)
   (a :initarg :a :initform 1 :accessor a)))

(defun make-color (&optional (r 255) (b 255) (g 255) (a 255))
  "Creates a uint color rbga."
  (make-instance 'color
                 :r (/ r 255)
                 :b (/ b 255)
                 :g (/ g 255)
                 :a (/ a 255)))

