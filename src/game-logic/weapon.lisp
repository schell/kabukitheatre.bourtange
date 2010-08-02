(in-package :bourtange)

;;;
;;;        ...
;;;       (_) :..*       the weapons!
;;;
(defclass weapon (unit)
  ((ndx :initarg :ndx :initform 1 :accessor ndx)))
;;
(defgeneric cooldown-radius (this)
  (:documentation "Returns the outer radius")
  (:method ((this weapon))
    (+ *core-size* (* 2 (ndx this)))))
;;
(defmethod cooldown ((this weapon) ms)
  (declare (ignore ms))
  (setf this (call-next-method))
  (setf (radius this) (cooldown-radius this))
  this)

(defclass core-blast (weapon)
  ((expansion-rate :initarg :expansion-rate :initform 1/10 :accessor expansion-rate
                   :documentation "Expands at 1px/millisecond"))
  (:documentation "coreblast is the main weapon, it sends out a shock wave")
  (:default-initargs
   :radius 10
   :density 0
   :defense 5
   :cooldown-time 7000
   :color (make-color 251 255 85 20)
   :outline-color (make-color 251 255 85)
   :vertices (make-circle)
   :outline (make-arc)))
(defmethod advance ((this core-blast) milliseconds)
  (setf (radius this) (+ (radius this) (* milliseconds (expansion-rate this))))
  (call-next-method)) ; return this core-blast
(defmethod renew ((this core-blast))
  (let ((new-core-blast (make-instance 'core-blast)))
    (setf (origin new-core-blast) (origin this)
          (ndx new-core-blast) (ndx this))
    new-core-blast))
(defmethod explode ((this core-blast))
  (setf (outline this)
        (make-arc 1 0 (* *tau*
                         (/ (death-time this)
                            (cooldown-time this)))))
  this)

