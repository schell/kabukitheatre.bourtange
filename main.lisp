;;;; Bourtange - a fort defense game
;;;; Schell Scivally
;;;; Sat Jul  3 11:31:06 PDT 2010

(cl:defpackage :bourtange
  (:use :cl))
(in-package :bourtange)

;;;--------------------------------------
;;;  Parameters
;;;--------------------------------------
(defparameter *headline* "Fear not, lest ye fail Bourtange!")
(defparameter *tau* 6.283185307179586
  "1 revolution of a circle.")
(defparameter *tau/4* (/ *tau* 4))
(defparameter *tau/2* (/ *tau* 2))
(defparameter *3tau/4* (* 3 (/ *tau* 4)))
(defparameter *screen-width* 0)
(defparameter *screen-height* 0)
(defparameter *last-tick* nil)
(defparameter *g* 0.1
  "The gravitational constant.") ;px/second
(defparameter *inner-spawning-radius* 400
  "The start of the spawning belt. Baddies will start spawning past this number.")
(defparameter *outer-spawning-radius* 700
  "The end of the spawning belt. Baddies will not spawn this number past this.")
(defparameter *boundary* *outer-spawning-radius*
  "The outer boundary of a game.")
(defparameter *total-baddies* 50
  "The total number of baddies on the screen at any time.")
(defparameter *core-size* 10
  "The radius of a core.")

;;;--------------------------------------
;;;  Some utility functions
;;;--------------------------------------
(defun the-time ()
  "The internal real time measured in seconds"
  (/ (get-internal-real-time) internal-time-units-per-second ))
(defgeneric multiply (x y)
  (:documentation "Multiplies two things together."))
(defun filter (a b)
  "Filters out all items in a from b"
  (if (= 0 (length a))
      b
      (filter (remove (first a) a)
              (remove (first a) b))))
(defun sphere-volume (radius)
  "Returns the volume of a sphere with radius 'radius. (meters assumed)"
  (* (* 2/3 *tau*)
     (* radius (* radius radius))))
(defun sphere-mass (radius density)
  "Returns the mass of a sphere with 'radius and 'density. (kg assumed)"
  (if (eql density 0)
      0
      (/ (sphere-volume radius) density)))
;;;--------------------------------------
;;;  Game definitions
;;;--------------------------------------
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
(defparameter *circle* (make-circle))

(defun make-arc (&optional (radius 1) (start-radian 0) (end-radian *tau*) (points 360))
  "Creates an arc"
  (cons (multiply (rad->xy start-radian) radius)
        (loop for x from 2 to points
           collect (multiply (rad->xy (+ start-radian
                                         (* x (/ (- end-radian start-radian) points))))
                             radius))))

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

;--------------------------------------
;  Game logic
;--------------------------------------
; a unit is our standard game unit
(defclass unit (display-circle physical-object)
  ((life :initarg :life :initform *tau* :accessor life :documentation "The unit's life meter.")
   (defense :initarg :defense :initform 1 :accessor defense :documentation "The unit's defense (how much damage it deflects).")
   (strength :initarg :strength :initform 1 :accessor strength :documentation "The unit's strength (how much damage it does).")
   (outline :initarg :outline :initform (make-arc) :accessor outline)
   (outline-color :initarg :outline-color :initform (make-color) :accessor outline-color)
   (cooldown-time :initarg :cooldown-time :initform 1 :accessor cooldown-time :documentation "The time it takes to die.")
   (death-time :initarg :death-time :initform nil :accessor death-time :documentation "The time spent dead.")
   (explode-rate :initarg :explode-rate :initform 20 :accessor explode-rate :documentation "Some factor that progresses the death sequence.")
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
(defgeneric cooldown (this seconds)
  (:documentation "Cools the unit down by 'seconds")
  (:method ((this unit) seconds)
    (setf (death-time this)
          (if (death-time this)
              (+ (death-time this) seconds)
              0))
    (if (>= (death-time this) (cooldown-time this))
        (renew this)
        ;; return the unit (renewed or not)
        (explode this))))

;;
(defgeneric advance (this seconds)
  (:documentation "Steps the unit forward in time")
  (:method ((this unit) seconds)
    (when (> (distance (make-point) (origin this)) *boundary*)
      (setf (life this) 0))
    (when (> (radius this) *boundary*)
      (setf (life this) 0))
    (setf (outline this) (make-arc 1 0 (life this)))
    (if (<= (life this) 0)
        ;; will return a renewed unit if cooldown-time has been reached
        (cooldown this seconds)
        (progn
          (apply-all this seconds)
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

;;
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

;;;
;;;        ...
;;;       (_) :..*       the weapons!
;;;
(defclass weapon (unit)
  ((core :initarg :core :initform nil :accessor core)
   (ndx :initarg :ndx :initform 1 :accessor ndx)))
;;
(defgeneric cooldown-radius (this)
  (:documentation "Returns the outer radius")
  (:method ((this weapon))
    (+ *core-size* (* 2 (ndx this)))))
;;
(defmethod cooldown ((this weapon) ms)
  (declare (ignore ms))
  (setf this (call-next-method)
	(origin this) (origin (core this))
	(radius this) (cooldown-radius this))
  this)
;;
(defmethod renew ((this weapon))
  (format t "~%~a renew weapon" (the-time))
  (let ((new-weapon (make-instance (class-of this))))
    (setf (origin new-weapon) (origin (core this))
	  (core new-weapon) (core this)
          (ndx new-weapon) (ndx this))
    new-weapon))

;;
(defclass core-blast (weapon)
  ((expansion-rate :initarg :expansion-rate :initform 100 :accessor expansion-rate
                   :documentation "Expands at 1px/millisecond"))
  (:documentation "coreblast is the main weapon, it sends out a shock wave")
  (:default-initargs
   :radius 10
   :density 0
   :defense 5
   :cooldown-time 7
   :color (make-color 251 255 85 20)
   :outline-color (make-color 251 255 85)
   :vertices (make-circle)
   :outline (make-arc)))
(defmethod advance ((this core-blast) seconds)
  (setf (radius this) (+ (radius this) (* seconds (expansion-rate this))))
  (call-next-method)) ; return this core-blast
(defmethod explode ((this weapon))
  (setf (outline this)
        (make-arc 1 0 (* *tau*
                         (/ (death-time this)
                            (cooldown-time this)))))
  this)

;; advance
(defclass decoy (weapon)
  ()
  (:documentation "decoy is a planet that gets launched into space in a random direction
after cooldown. It attracts baddies through gravity.")
  (:default-initargs
   :radius 40
   :density 10
   :defense 50
   :cooldown-time 1
   :velocity (add (make-point -60.0 -60.0) (make-point (random 120.0) (random 120.0)))
   :color (make-color 255 95 85 20)
   :outline-color (make-color 255 95 85)
   :vertices (make-circle)
   :outline (make-arc)))
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
(defmethod advance :before ((this core) seconds)
  (unless (death-time this)
    (setf (weapons this)
          (mapcar (lambda (weapon)
                    (advance weapon seconds))
                  (weapons this)))))
;;
(defmethod collidedp ((this core) (that unit))
  (if (call-next-method)
      (list this that)
      (let ((weapon (find-if (lambda (weapon) (collidedp weapon that))
                             (weapons this))))
        (when weapon (list weapon that)))))
;;
(defmethod get-grav-acceleration ((that baddy) (this core))
  (let ((core-force (call-next-method)))
    (loop for weapon in (weapons this) 
	 for force = (get-grav-acceleration that weapon)
	 sum (x force) into x-force
	 sum (y force) into y-force
	 finally (return (make-point (+ (x core-force) x-force)
				     (+ (y core-force) y-force))))))
;;
(defun draw-resources (resources &optional 
		       (point-size 5.0) 
		       (point (make-point (+ point-size (- (/ *screen-width* 2))) (- (/ *screen-height* 2) point-size))))
  "Draws the amount of resources available to the screen"
  (let* ((rows 12)
         (rest-point-size (* point-size (- resources (floor resources))))
         (points (loop for x from 0 to (1- resources) collect
                      (make-point
                       (* (mod x rows) (+ (* 2.0 point-size) 1))
                       (- (* (floor (/ x rows)) (+ (* 2.0 point-size) 1))))))
         (last-full-point (if (last points) (first (last points)) (make-point))))
    (mapcar #'(lambda (p) 
		(progn
		  (gl:load-identity)
		  (gl:translate (+ (x p) (x point)) 
				(+ (y p) (y point)) 
				0)
		  (gl:scale point-size point-size 1)
		  (draw-point-list *circle* :polygon (make-color 155 92 238))))
	    points)
    (gl:load-identity)
    (gl:translate (+ (x point) (x last-full-point) (* 2 point-size) 1) 
		  (+ (y point) (y last-full-point)) 
		  0)
    (gl:scale rest-point-size rest-point-size 1)
    (draw-point-list *circle* :polygon (make-color 232 125 241))))
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
                        :cost 10)
		       (make-weapon-store-item
			:item 'decoy
			:cost 10))
            :accessor weapons))
  (:default-initargs :radius 100
    :death-time 0
    :cooldown-time 0.5
    :vertices (make-circle)
    :outline (make-circle)
    :color (make-color 255 255 255 75)
    :outline-color (make-color 255 255 255 (* 0.2 255))))
;;
(defmethod draw :after ((this weapon-store))
  (map nil #'draw (weapons this)))
;;
(defmethod advance ((this weapon-store) seconds)
  (setf (death-time this) (min (cooldown-time this) (+ (death-time this) seconds))
        (a (color this)) (* 0.2 (/ (death-time this) (cooldown-time this))))
  this)
;;
(defmethod cooldown ((this weapon-store) seconds)
  (setf (death-time this) (max 0 (- (death-time this) seconds))
        (a (color this)) (* 0.2 (/ (death-time this) (cooldown-time this))))
  this)
;;
(defmethod collidedp ((this weapon-store) (that mouse))
  (let ((collided-weapon (find-if (lambda (weapon) (collidedp weapon that)) (weapons this))))
    (when collided-weapon
      (list collided-weapon that))))
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
     do (setf (origin weapon) (add (origin (weapon-store program)) position)
	      (last-origin weapon) (origin weapon)))
  program)
;;
(defun select-weapon (store mouse resources)
  (let ((weapon (car (collidedp store mouse))))
    (when (and weapon (<= (cost weapon) resources))
      (copy weapon))))
;;;  ._________.
;;;  | 1010101 |      the program...
;;;  | 1001001 |
;;;  \---------/
(defclass program ()
  ((is-paused :initform nil :accessor is-paused)
   (spawning-belt :initform (make-display-ring *outer-spawning-radius* *inner-spawning-radius* (make-color 255 255 255 10)) :accessor spawning-belt)
   (baddies :initform (create-random-baddies *total-baddies* *outer-spawning-radius* *inner-spawning-radius*) :accessor baddies)
   (goodies :initform nil :accessor goodies)
   (dying-bodies :initform () :accessor dying-bodies)
   (resources :initform 100 :accessor resources)
   (mouse :initform (make-instance 'mouse :radius 2 :vertices (make-circle)) :accessor mouse)
   (weapon-store :initform (make-instance 'weapon-store) :accessor weapon-store)
   (selected-weapon :initform nil :accessor selected-weapon)
   (selected-core :initform nil :accessor selected-core)))
(defparameter *program* (make-instance 'program))
;
(defmethod draw ((this program))
  (draw (spawning-belt this))
  (draw-list (goodies this))
  (draw-list (baddies this))
  (draw-list (dying-bodies this))
  (draw-resources (resources this))
  (draw (weapon-store this))
  (when (selected-weapon this)
    (if (selected-core this)
        (let* ((core (selected-core this))
               (origin (origin core))
               (radius (+ (radius core) (* 2 (length (weapons core))) 2))
               (color (outline-color (selected-weapon this))))
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:translate (x origin) (y origin) 0)
          (gl:scale radius radius 1)
          (draw-point-list (vertices core) :line-loop color))
        (draw (selected-weapon this)))))
; advance our program! THESE are the main logic loop
(defmethod advance :before ((this program) seconds)
  ;; user interaction!
  (when (clickedp (mouse this))
    (setf (last-state (mouse this)) (state (mouse this)))
      (if (selected-weapon this)
          ;; place the weapon
          (if (eql (weapon-class (selected-weapon this)) 'core)
              (let ((new-core (make-instance 'core)))
                (setf (origin new-core) (origin (mouse this))
                      (goodies this) (append (goodies this) (list new-core))
                      (selected-weapon this) nil))
              (when (selected-core this)
                (let ((weapon (make-instance (weapon-class (selected-weapon this)))))
                  (setf (origin weapon) (origin (selected-core this))
			(core weapon) (selected-core this)
			(ndx weapon) (1+ (length (weapons (selected-core this))))
			(weapons (selected-core this)) (append
                                                        (weapons (selected-core this))
                                                        (list weapon))
			(selected-weapon this) nil
			(selected-core this) nil))))
          ;; select the weapon
          (when (setf (selected-weapon this)
                      (select-weapon (weapon-store this) (mouse this) (resources this)))
            ;; deduct the cash
            (setf (resources this) (- (resources this) (cost (selected-weapon this)))))))
  ;; if the user has selected a weapon
  (when (selected-weapon this)
    (when (not (eql (weapon-class (selected-weapon this)) 'core))
      (loop for x from 0 to (1- (length (goodies this))) do
                                        ; find the core we're attaching the upgrade to
           (if (collidedp (nth x (goodies this)) (mouse this))
               (progn
                 (setf (selected-core this) (nth x (goodies this)))
                 (return))
               (setf (selected-core this) nil))))
    (setf (origin (selected-weapon this)) (origin (mouse this))))
  ;; weapon store interaction
  (let ((collided (collidedp (weapon-store this) (mouse this))))
    (if collided
        (progn
           (draw-resources (cost (first collided))
                          4
                          (add (origin (first collided))
                               (make-point 0 (- 0 (radius (first collided)) 4))))
          (setf (weapon-store this) (advance (weapon-store this) seconds)))
        (setf (weapon-store this) (cooldown (weapon-store this) seconds)))))
;
(defmethod advance ((this program) seconds)
  ;; automagical game-logic
  ;; step out of the game loop if we're paused
  (when (is-paused this)
    (return-from advance this))
  ;; gravitate the baddies toward the goodies
  (gravitate-bodies (baddies this) (goodies this))
  ;; find collisions and handle them (collects)
  (loop for goody in (goodies this) do
       (loop for baddy in (baddies this)
          for collided = (collidedp goody baddy)
          when collided
          ;; since we're probably testing a collision with a baddy and a core
          ;; collidedp returns a list of the baddy and the weapon (unit)
          ;; that the baddy collided with, that way we can damage the weapon
          do (apply #'collide collided)))
  ;; advance the baddies, dead baddies will be renewed in 'advance
  (setf (baddies this)
        (mapcar (lambda (baddy) (advance baddy seconds))
                (baddies this)))
  ;; advance the goodies, dead goodies will be renewed in 'advance
  (setf (goodies this)
        (loop for goody in (goodies this)
           ;; right here we're probably advancing cores, which in turn
           ;; advance each core's weapons
           for advanced = (advance goody seconds)
           when advanced
           collect advanced))
  ;; add the just killed baddies resources to our pool
  (setf (resources this)
        (+ (resources this)
           (flet ((maybe-collect-resources (baddy)
                    (if (eql 0 (death-time baddy))
                        (get-resources baddy)
                        0)))
             (reduce #'+ (baddies this)
                     :key #'maybe-collect-resources
                     :initial-value 0))))  
  this) ; return this program

;;
(defun draw-display ()
  "Called every frame to draw things - this is our main game loop"
  (let ((time (the-time)))
    (unless *last-tick*
      (setf *last-tick* time))
    ;; update program
    (setf *program* (advance *program* (- time *last-tick*)))
    ;; draw it out
    (draw *program*)
    ;; set the last tick
    (setf *last-tick* time)))
;--------------------------------------
;  Gl/Glut stuff
;--------------------------------------
(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p))
  (:default-initargs :width 1400 :height 1100
                     :title *headline*
                     :pos-x 20 :pos-y 0
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
		     :tick-interval (round 1000 60))) ; milliseconds per tick

(defmethod glut:display-window :before ((win my-window))
  (gl:line-width 1)
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod glut:display ((this my-window))
  ;; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; draw code
  (draw-display)
  ;; swap the buffer onto the screen
  (glut:swap-buffers))

(defmethod glut:reshape ((this my-window) width height)
  (setf *screen-width* width
        *screen-height* height)
  (gl:viewport 0 0 width height)        ; reset the current viewport
  (gl:matrix-mode :projection)          ; select the projection matrix
  (gl:load-identity)                    ; reset the matrix
  (glu:ortho-2d -1 1 -1 1)
  ;; reset the projection matrix
  (gl:load-identity)
  (let ((/w (/ (/ width 2)))
        (/h (/ (/ height 2))))
    (gl:scale /w /h 1))
  (gl:matrix-mode :modelview)           ; select the modelview matrix
  (gl:load-identity)                    ; reset the matrix
  (set-store-position *program*))

(defmethod glut:keyboard ((this my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ;; pause the game
    (#\p
     (setf (is-paused *program*) (not (is-paused *program*))))
    ;; reset the program (new game)
    (#\r
     (setf *program* (level 1)))))                         

(defun update-mouse (mouse x y &optional button state)
  ;; transmute coordinates
  (let ((new-x (- x (/ *screen-width* 2)))
        (new-y (- (- y (/ *screen-height* 2)))))
    (setf (x (origin mouse)) new-x
          (y (origin mouse)) new-y))
  ;; set mouse position and stuff
  (when button
    (setf (button mouse) button))
  (when state
    (setf (last-state mouse) (state mouse))
    (setf (state mouse) state))
  mouse)

;; mouse mouse while down/up
(defmethod glut:mouse ((this my-window) button state x y)
  (setf (mouse *program*)
        (update-mouse (mouse *program*) x y button state)))

;; mouse move passively (no button down)
(defmethod glut:passive-motion ((this my-window) x y)
  (setf (mouse *program*)
        (update-mouse (mouse *program*) x y)))

(defmethod glut:tick ((this my-window))
  (glut:post-redisplay))
;;;--------------------------------------
;;;  Setup and go
;;;--------------------------------------
(defun level (number)
  "Sets up the program object for a specific level"
  (set-store-position 
   (case number
     (1 (let* ((program (make-instance 'program))
		       (core (make-instance 'core))
		       (decoy (make-instance 'decoy :core core)))
		  (setf (weapons core) (list decoy)
			(goodies program) (list core))
		  program))
     ;; level 0
     (otherwise (let* ((program (make-instance 'program))
		       (core (make-instance 'core))
		       (core-blast (make-instance 'core-blast :core core)))
		  (setf (weapons core) (list core-blast)
			(goodies program) (list core))
		  program)))))
(defun main ()
  (glut:display-window (make-instance 'my-window :program (setf *program* (level 1)))))

