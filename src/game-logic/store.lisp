(in-package :bourtange)

;;;
;;;   ($) ($) ($) ($)    the weapon store...
;;;
;
(defclass weapon-store-item (unit)
  ((weapon-class :initarg :weapon-class :accessor weapon-class)
   (cost :initarg :cost :accessor cost)
   (background :initarg :background :initform (make-display-circle) :accessor background)))
;;
(defun make-weapon-store-item (&key item cost)
  (let ((wclass (make-instance item)))
    (make-instance 'weapon-store-item
      :weapon-class item
      :cost cost
      :vertices (vertices wclass)
      :outline (outline wclass)
      :radius 10
      :color (color wclass)
      :outline-color (outline-color wclass))))
;;
#+nil(defmethod draw ((this weapon-store-item))
  (call-next-method))
;;
(defgeneric copy (this)
  (:documentation "Copies this item")
  (:method ((this weapon-store-item))
    (make-weapon-store-item :item (weapon-class this) :cost (cost this))))

;;
(defclass weapon-store (unit)
  ((weapons :initform (list
                       (make-weapon-store-item
                        :item 'core
                        :cost 10)
                       (make-weapon-store-item
                        :item 'core-blast
                        :cost 10))
            :accessor weapons))
  (:default-initargs :radius 100
    :death-time 0
    :cooldown-time 500
    :vertices (make-circle)
    :outline (make-circle)
    :color (make-color 255 255 255 75)
    :outline-color (make-color 255 255 255 (* 0.2 255))))
;;
(defmethod draw :after ((this weapon-store))
  (map nil #'draw (weapons this)))
;;
(defmethod advance ((this weapon-store) milliseconds)
  (setf (death-time this) (min (cooldown-time this) (+ (death-time this) milliseconds))
        (a (color this)) (* 0.2 (/ (death-time this) (cooldown-time this))))
  this)
;;
(defmethod cooldown ((this weapon-store) milliseconds)
  (setf (death-time this) (max 0 (- (death-time this) milliseconds))
        (a (color this)) (* 0.2 (/ (death-time this) (cooldown-time this))))
  this)
;;
(defmethod collidedp ((this weapon-store) (that mouse))
  (let ((collided-weapon (find-if (lambda (weapon) (collidedp weapon that)) (weapons this))))
    (when collided-weapon
      (list collided-weapon that)))
  #+nil ;; for fun...
  (awhen (find-if (fun _ (collidedp _ that)) (weapons this))
    (list it that)))

;;
(defun set-store-position (program)
  (setf (origin (weapon-store program))
        (make-point
         (- (/ *screen-width* 2)  (radius (weapon-store program)))
         (- (/ *screen-height* 2) 12)))
  ;; LOOP IS FUN! \o/
  (loop with positions = (make-arc (radius (weapon-store program))
                                   *tau/2* *3tau/4*
                                   (length (weapons (weapon-store program))))
     for position in positions
     for weapon in (weapons (weapon-store program))
     do (setf (origin weapon)
              (add (origin (weapon-store program))
                   position)))
  program)
;;
(defun select-weapon (store mouse resources)
  (let ((weapon (car (collidedp store mouse))))
    (when (and weapon (<= (cost weapon) resources))
      (copy weapon))))
