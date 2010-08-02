(in-package :bourtange)

(defclass baddy (unit)
  ()
  (:default-initargs
   :radius 5
   :density 5
   :color (make-color 108 108 108)
   :outline-color (make-color 251 38 0)
   :outline (make-arc)
   :vertices (make-circle)))
(defun create-random-baddies (number outer-radius inner-radius &optional (baddy-type 'baddy))
  "Creates 'number baddies to converge upon bourtange and collects them..."
  (loop repeat number
     for p = (multiply (rad->xy (random *tau*))
                       (+ inner-radius
                          (random (- outer-radius inner-radius))))
     collect (make-instance baddy-type :origin p :last-origin p)))
;;
(defgeneric get-resources (this)
  (:documentation "Returns the worth of this baddy in resources.")
  (:method ((this baddy))
    (* *tau* (* 1/100 (+ (strength this) (defense this))))))

(defmethod renew ((this baddy))
  (first (create-random-baddies 1 *outer-spawning-radius* *inner-spawning-radius*)))
;;
(defmethod advance ((this baddy) ms)
  (declare (ignore ms))
  "update the core's weapons"
  (setf this (call-next-method) ;what?
        (outline this) (make-arc 1 0 (life this)))
  (when (death-time this)
    (setf (color this) (make-color 255 255 255 (a (color this)))))
  this)

