(in-package :bourtange)

;;;
;;;      ()   ...the core
;;;
;;;
(defclass core (unit)
  ((weapons :initarg :weapons :initform (list ) :accessor weapons))
  (:default-initargs
   :radius 10
   :defense 100
   :color (make-color 98 255 85 75)
   :outline-color (make-color 98 255 85 255)
   :vertices (make-circle)
   :outline (make-arc)))
;;
(defmethod draw :before ((this core))
  ;;(mapc #'draw (weapons this))
  (map nil #'draw (weapons this)))
;;
(defmethod renew ((this core))
  nil) ; the core is dead, it does not renew :(
;;
(defmethod cooldown :before ((this core) ms)
  (declare (ignore ms))
  (setf (color this) (make-color 251 38 0 255)))
;;
(defmethod advance :before ((this core) milliseconds)
  (unless (death-time this)
    (setf (weapons this)
          (mapcar (lambda (weapon)
                    (advance weapon milliseconds))
                  (weapons this)))))
;;
(defmethod collidedp ((this core) (that unit))
  (if (call-next-method)
      (list this that)
      (let ((weapon (find-if (lambda (weapon) (collidedp weapon that))
                             (weapons this))))
        (when weapon (list weapon that)))))

;;
(defun draw-resources (resources &optional (point-size 10) (point (make-point (+ point-size (- (/ *screen-width* 2))) (- (/ *screen-height* 2) point-size))) (scale 1))
  "Draws the amount of resources available to the screen"
  (let* ((rows 8)
         (rest-point-size (* point-size (- resources (floor resources))))
         (points (loop for x from 0 to (1- resources) collect
                      (make-point
                       (* (mod x rows) (+ 2 point-size))
                       (- (* (floor (/ x rows)) (+ 2 point-size))))))
         (last-full-point (if (last points) (first (last points)) (make-point))))
    (gl:point-size 1 #+nil point-size)
    (gl:load-identity)
    (gl:translate (x point) (y point) 0)
    (gl:scale scale scale 1)
    (draw-point-list points :points (make-color 155 92 238))
    (gl:point-size rest-point-size)
    (draw-point-list (list
                      (make-point
                       (+ (x last-full-point) point-size 2)
                       (+ (y last-full-point))))
                     :points
                     (make-color 232 125 241))))

;;
(defun check-out-of-bounds (object)
  "Checks an object to assert it is out of bounds."
  (> *boundary* (distance (make-point) (origin object))))

;;
(defun gravitate-bodies (from-bodies toward-bodies)
  "Accelerates 'from-bodies toward 'toward-bodies. Does nothing with
  'from-bodies. If toward-bodies had acceleration, it is totally reset."
  (dolist (from from-bodies)
    (setf (acceleration from) (make-point))
    (loop for toward in toward-bodies
       for grav-accel = (get-grav-acceleration from toward)
       do (setf (acceleration from)
                (add (acceleration from) grav-accel)))))

