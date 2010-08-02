(in-package :bourtange)

; a unit is our standard game unit
(defclass unit (display-circle physical-object)
  ((life :initarg :life :initform *tau* :accessor life :documentation "The unit's life meter.")
   (defense :initarg :defense :initform 1 :accessor defense :documentation "The unit's defense (how much damage it deflects).")
   (strength :initarg :strength :initform 1 :accessor strength :documentation "The unit's strength (how much damage it does).")
   (outline :initarg :outline :initform (make-arc) :accessor outline)
   (outline-color :initarg :outline-color :initform (make-color) :accessor outline-color)
   (cooldown-time :initarg :cooldown-time :initform 1000 :accessor cooldown-time :documentation "The time it takes to die.")
   (death-time :initarg :death-time :initform nil :accessor death-time :documentation "The time spent dead.")
   (explode-rate :initarg :explode-rate :initform 1/50 :accessor explode-rate :documentation "Some factor that progresses the death sequence.")
   (radius-at-death :initarg :radius-at-death :initform nil :accessor radius-at-death)))
(defun make-unit (radius color outline-color &optional (unit-type 'unit))
  (make-instance unit-type
                 :radius radius
                 :vertices (make-circle)
                 :outline (make-arc)
                 :color color
                 :outline-color outline-color))
(defmethod draw :after ((object unit))
  "Draw the outline."
  (let* ((color (outline-color object))
         (scale (radius object))
         (origin (origin object))
         (outline (outline object)))
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:translate (x origin) (y origin) 0)
    (gl:scale scale scale 1)
    (draw-point-list  outline :line-strip color))
  (gl:load-identity))
(defgeneric life-after-collision (this that)
  (:documentation "Returns the life of 'this after a collision with 'that")
  (:method ((this unit) (that unit))
    (let* ((life-this (life this))
           (percent (/ (strength that) (defense this))))
      (- life-this (* *tau* percent)))))

;;
(defgeneric renew (this)
  (:documentation "Renews the unit")
  (:method ((this unit))
    (setf (death-time this) nil
          (radius-at-death this) nil)
    this)) ; return this unit

;;
(defgeneric explode (this)
  (:documentation "Explodes the unit")
  (:method ((this unit))
    (unless (radius-at-death this)
      (setf (radius-at-death this)
            (radius this)))
    (setf (a (color this)) (- 255 (* 255 (/ (death-time this)
                                            (cooldown-time this))))
          (radius this) (+ (radius-at-death this)
                           (* (explode-rate this) (death-time this))))
    this))

;;
(defgeneric cooldown (this milliseconds)
  (:documentation "Cools the unit down by 'milliseconds")
  (:method ((this unit) milliseconds)
    (setf (death-time this)
          (if (death-time this)
              (+ (death-time this) milliseconds)
              0))
    (if (>= (death-time this) (cooldown-time this))
        (renew this)
        ;; return the unit (renewed or not)
        (explode this))))

;;
(defgeneric advance (this milliseconds)
  (:documentation "Steps the unit forward in time")
  (:method ((this unit) milliseconds)
    (when (> (distance (make-point) (origin this)) *boundary*)
      (setf (life this) 0))
    (when (> (radius this) *boundary*)
      (setf (life this) 0))
    (setf (outline this) (make-arc 1 0 (life this)))
    (if (not (minusp (life this)))
        ;; will return a renewed unit if cooldown-time has been reached
        (cooldown this milliseconds)
        (progn
          (apply-all this milliseconds)
          this))))
;;
(defgeneric collide (this that)
  (:documentation "Collides 'this with 'that, returns a list of this and that")
  (:method ((this unit) (that unit))
    (unless (or (death-time this) (death-time that))
      ;; set the life of the two objects
      (setf (life that) (life-after-collision that this)
            (life this) (life-after-collision this that)))
    (list this that)))

;;
(defgeneric get-grav-force (m1 m2)
  (:documentation "Returns the gravitational force between 'm1 and 'm2")
  (:method ((m1 unit) (m2 unit))
    (let ((distance (distance (origin m1) (origin m2)))
          (mass1 (sphere-mass (radius m1) (density m1)))
          (mass2 (sphere-mass (radius m2) (density m2))))
      (* *g* (/ (* mass1 mass2) (* distance distance))))))

;;
(defgeneric get-grav-acceleration (m1 m2)
  (:documentation "Returns the gravitational acceleration from 'm1 to 'm2 as a vector")
  (:method ((m1 unit) (m2 unit))
    (let ((force (get-grav-force m1 m2))
          (udv (unitize (distance-vector (origin m1)
                                         (origin m2)))))
      (make-point (* (x udv) force)
                  (* (y udv) force)))))
