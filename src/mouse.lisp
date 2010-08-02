(in-package :bourtange)

;;;
;;;       o..o       the mouse...
;;;       (\/)S
;;;
(defclass mouse (display-circle)
  ((button :initarg :button :initform 0 :accessor button)
   (state :initarg :state :initform 0 :accessor state)
   (x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)
   (last-state :initarg :last-mouse-state :initform nil :accessor last-state)))
;;
(defgeneric clickedp (this)
  (:documentation "Return whether or not a click event has happened and is waiting to be cleared.")
  (:method ((this mouse))
    (and (eql (state this) :down) (not (eql (state this) (last-state this))))))
