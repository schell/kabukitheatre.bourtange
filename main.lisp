; Bourtange - a fort defense game
; Schell Scivally
; Sat Jul  3 11:31:06 PDT 2010
;--------------------------------------
;  Dependencies
;--------------------------------------
(require 'asdf)
(require 'asdf-install)
(asdf:load-system :cl-opengl) 
(asdf:load-system :cl-glu)    
(asdf:load-system :cl-glut)		
;--------------------------------------
;  Some utility functions
;--------------------------------------
(defun millitime ()
  "The internal real time measured in milliseconds"
  (* (get-internal-real-time) (/ 1000 internal-time-units-per-second)))
(defgeneric multiply (x y)
  (:documentation "Multiplies two things together."))
(defun filter (a b)
  "Filters out all items in a from b"
  (if (= 0 (length a)) b
    (filter (remove (first a) a) (remove (first a) b))))
;--------------------------------------
;  Game definitions
;--------------------------------------
(defparameter *tau* 6.283185307179586)
(defparameter *tau/4* (/ *tau* 4))
(defparameter *tau/2* (/ *tau* 2))
(defparameter *3tau/4* (* 3 (/ *tau* 4)))

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))
(defun make-point (&optional (x 0) (y 0))
  "Utility function. Creates a point at (x, y)"
  (make-instance 'point :x x :y y))
(defmethod multiply ((p point) s)
  (make-point (* (x p) s) (* (y p) s)))
(defgeneric add (p1 p2)
  (:documentation "Adds to points together"))
(defmethod add ((p1 point) (p2 point))
  (make-point (+ (x p1) (x p2)) (+ (y p1) (y p2))))
(defun rad->xy (theta)
  "Utility function. Converts a radian measure to vector (list x y)"
  (make-point (cos theta) (sin theta)))
(defgeneric magnitude (p1)
  (:documentation "Returns the magnitude of the vector p1"))
(defmethod magnitude ((p1 point))
  (sqrt (+ (* (x p1) (x p1)) (* (y p1) (y p1)))))
(defgeneric distance (p1 p2)
  (:documentation "Returns the distance between p1 and p2"))
(defmethod distance ((p1 point) (p2 point))
  (magnitude (make-point (- (x p2) (x p1)) (- (y p2) (y p1)))))
  
(defun make-square (half-width)
  "Utility function. Creates a list of 4 points arranged in a square"
  (let ((hw half-width) (-hw (* -1 half-width)))
  (list (make-point hw hw)
   (make-point hw -hw)
   (make-point -hw -hw)
   (make-point -hw hw))))
   
(defun make-circle (&optional (radius 1) (points 360)) 
  "Utility function. Creates a circle of 'points points"
  (loop for x from 1 to points collect (multiply (rad->xy (* x (/ *tau* points))) radius)))
  
(defun make-arc (&optional (radius 1) (start-radian 0) (end-radian *tau*) (points 360))
  "Utility function. Creates an arc"
  (cons (multiply (rad->xy start-radian) radius)
    (loop for x from 2 to points collect 
      (multiply (rad->xy (+ start-radian (* x (/ (- end-radian start-radian) points)))) radius))))
;       
; (defun make-ring (&optional (inner-radius 0.5) (radius 1) (start-radian 0) (end-radian *tau*) (points 360))
;   "Utility function. Creates an arched ring, inner radius is always 1"
;   (let* ((outer (make-arc radius start-radian end-radian points))
;    (inner (make-arc 1 start-radian end-radian points)))
;     (append outer (reverse inner) (list (first outer)))))
;     
(defclass color ()
  ((r :initarg :r :initform 1 :accessor r)
   (b :initarg :b :initform 1 :accessor b)
   (g :initarg :g :initform 1 :accessor g)
   (a :initarg :a :initform 1 :accessor a)))
(defun make-color (r b g &optional (a 1))
  "Utility function. Creates a color rbg"
  (make-instance 'color :r r :b b :g g :a a))
  
(defun draw-point-list (points &optional (primitive :polygon) (color (make-color 1 1 1 1)))
  "Utility function for drawing a list of points."
  (gl:color (r color) (b color) (g color) (a color))
  (gl:with-primitive primitive
    (dolist (point points)
      (gl:vertex (x point) (y point) 0)))
  (gl:flush))
(defun offset-point-list (points p)
  "Adds a point to each point in the list and returns the new offset list"
  (loop for x from 0 to (- (length points) 1) collect 
    (add (nth x points) p)))
   
(defclass display-object ()
  ((origin :initarg :origin :initform (make-point) :accessor origin)
   (color :initarg :color :initform (make-color 1 1 1 1) :accessor color)
   (primitive :initarg :primitive :initform :polygon :accessor primitive
    :documentation "The opengl primitive to draw this object with")
   (vertices :initarg :vertices :initform (make-square 10) :accessor vertices)))
(defgeneric initialize (object)
  (:documentation "Initializes a display-object"))
(defmethod initialize ((object display-object))
  (setf (origin object) (make-point))
  (setf (vertices object) (list (make-point))))
(defgeneric draw (object)
  (:documentation "Draws the display-object with OpenGL"))
(defgeneric set-origin (object coords)
  (:documentation "Sets the origin of the object and returns the object"))
(defmethod set-origin ((object display-object) (coords point))
  (setf (origin object) coords)
  object)
(defgeneric check-collision (o1 o2)
  (:documentation "Checks to see if the objects collided"))
  
(defun draw-list (the-list)
  (dolist (item the-list)
    (draw item)))

(defclass display-circle (display-object)
  ((radius :initarg :radius :initform 10 :accessor radius))
  (:default-initargs :vertices (make-circle)))
(defun make-display-circle (radius &optional (color (make-color 1 1 1 1)))
  "Utility function that creates a new circle of radius 'radius and color 'color"
  (make-instance 'display-circle :radius radius :vertices (make-circle) :color color))
(defmethod draw ((object display-circle))
  (let* ((color (color object)) 
   (scale (radius object))
   (r (r color)) 
   (g (g color)) 
   (b (b color))
   (a (a color))
   (origin (origin object))
   (ox (x origin))
   (oy (y origin))
   (vertices (vertices object))
   (primitive (primitive object)))
    (gl:color r b g a)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:translate ox oy 0)
    (gl:scale scale scale 1)
    (gl:with-primitive primitive
      (dolist (vertex vertices)
        (gl:vertex (x vertex) (y vertex) 0)))
    (gl:flush))
    (gl:load-identity))
(defmethod check-collision ((o1 display-circle) (o2 display-circle))
  (< (distance (origin o1) (origin o2)) (+ (radius o1) (radius o2))))
  
(defclass display-arc (display-circle)
  ((start-radian :initarg :start-radian :initform 0 :accessor start-radian)
   (end-radian :initarg :end-radian :initform *tau* :accessor end-radian))
  (:default-initargs :vertices (make-arc)))
(defun make-display-arc (radius &optional (start-radian 0) (end-radian *tau*) (color (make-color 1 1 1 1)))
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
(defgeneric apply-velocity (object)
  (:documentation "Applies the object's velocity to its position"))
(defmethod apply-velocity ((object physical-object))
  (setf (origin object) (add (origin object) (velocity object))))
(defgeneric apply-acceleration (object)
  (:documentation "Applies the object's acceleration to its velocity"))
(defmethod apply-acceleration ((object physical-object))
  (setf (velocity object) (add (velocity object) (acceleration object))))
(defgeneric apply-all (object)
  (:documentation "Applies acceleration and velocity to the object"))
(defmethod apply-all ((object physical-object))
  (apply-acceleration object)
  (apply-velocity object))
(defun apply-all-list (po-list)
  (dolist (object po-list)
    (apply-all object)))

(defclass event ()
  ((name :initarg :name :initform "event" :accessor name)))
(defgeneric out (e)
  (:documentation "Prints the event to stdout"))
(defmethod out ((e event))
  (format t "~%event :name ~a" (name e)))

(defclass mouse-event (event)
  ((button :initarg :button :initform 0 :accessor button)
   (state :initarg :state :initform 0 :accessor state)
   (x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y))
  (:default-initargs :name "mouse-event"))
(defun make-mouse-event (button state x y) 
  (make-instance 'mouse-event :button button :state state :x x :y y))
(defmethod out ((e mouse-event))
  (format t "~%mouse-event :name ~a :button ~a :state ~a :x ~a :y ~a"
   (name e) (button e) (state e) (x e) (y e)))
;--------------------------------------
;  Algos
;--------------------------------------

;--------------------------------------
;  Game logic
;--------------------------------------
; a unit is our standard game unit
(defclass unit (display-circle physical-object)
  ((life :initarg :life :initform *tau* :accessor life)
   (defense :initarg :defense :initform 1 :accessor defense)
   (strength :initarg :strength :initform *tau* :accessor strength)
   (outline :initarg :outline :initform (make-arc) :accessor outline)
   (outline-color :initarg :outline-color :initform (make-color 1 1 1 1) :accessor outline-color)
   (explode-radius :initform 50 :accessor explode-radius)))
(defun make-unit (radius color outline-color &optional (unit-type 'unit))
  (make-instance unit-type :radius radius :vertices (make-circle) :outline (make-arc) :color color :outline-color outline-color))
(defmethod draw ((object unit))
  (call-next-method)
  ; draw the outline
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
(defgeneric decrement-life (pc damage)
  (:documentation "Decrements the life of the unit by 'damage, returns new life count"))
(defmethod decrement-life ((pc unit) damage)
  (setf (outline pc) (make-arc 1 0 (- (life pc) damage)))
  (setf (life pc) (max 0 (- (life pc) damage))))
(defgeneric update-dying-unit (object radius)
  (:documentation "Updates a dying unit"))
(defmethod update-dying-unit ((object unit) radius)
  (setf (radius object) radius)
  (setf (a (color object)) (- 1.0 (/ (radius object) (explode-radius object))))
  (setf (a (outline-color object)) (- 1.0 (/ (radius object) (explode-radius object)))))
      
(defun update-unit (unit &optional &key life radius velocity acceleration origin color outline-color)
  "Updates the values of a unit"
  (if radius (setf (radius unit) radius))
  (if life 
    (setf (outline unit) 
      (make-arc 1 0 (max 0 (setf (life unit) life)))))
  (if velocity (setf (velocity unit) velocity))
  (if acceleration (setf (acceleration unit) acceleration))
  (if origin (setf (origin unit) origin))
  (if color (setf (color unit) color))
  (if outline-color (setf (outline-color unit) outline-color))
  unit)
  
(defclass baddy (unit) 
  ((damage :initarg :damage :initform (/ 1 100) :accessor damage))
  (:default-initargs :radius 10
   :color (make-color (/ 108 255) (/ 108 255) (/ 108 255))
   :outline-color (make-color (/ 251 255) (/ 38 255) 0)
   :outline (make-arc)
   :vertices (make-circle)))

;(weapons)
(defclass weapon (unit)
  ((parent-core :initarg :parent-core :initform nil :accessor parent-core)
   (death-time :initarg :death-time :initform nil :accessor death-time)
   (cooldown-time :initarg :cooldown-time :initform 10000 :accessor cooldown-time)))
(defgeneric cooldown (w)
  (:documentation "Cools the weapon down"))
(defgeneric renew (w)
  (:documentation "Renews the weapon"))
(defmethod cooldown ((w weapon))
  (if (death-time w) 
    (let ((been-cooling (- (millitime) (death-time w))))
      (if (>= been-cooling (cooldown-time w))
        (setf w (renew w))))
    (setf (death-time w) (millitime)))
  w) ; return the weapon (renewed or not)
  
; coreblast is the main weapon, it sends out a shock wave
(defclass core-blast (weapon)
  ((expansion-rate :initarg :expansion-rate :initform 16 :accessor expansion-rate))
  (:default-initargs :defense 100))
(defun make-core-blast (radius &optional core)
  (let ((blast (make-unit radius (make-color (/ 92 255) (/ 169 255) (/ 238 255) 0.3) (make-color (/ 92 255) (/ 221 255) (/ 238 255) 1) 'core-blast)))
    (setf (parent-core blast) core)
    blast))
(defmethod draw ((object core-blast))
  (if (eql (death-time object) nil)
    (call-next-method)))
(defmethod renew ((w core-blast))
  (make-core-blast 50 (parent-core w)))
;(/weapons)

(defclass core (unit)
  ((blast :initarg :blast :initform (make-core-blast 50) :accessor blast))
  (:default-initargs :radius 50
   :color (make-color (/ 231 255) (/ 250 255) (/ 216 255))
   :outline-color (make-color (/ 188 255) (/ 238 255) (/ 92 255))))
(defun make-core (radius)
  (let ((core (make-instance 'core :radius radius :outline (make-arc)
   :vertices (make-circle)
   :blast (make-core-blast radius))))
    (setf (parent-core (blast core)) core)
    core)) ; return the core after setting the blast's parent core
(defmethod draw ((object core))
  (draw (blast object))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (x (origin object)) (y (origin object)) 0)
  (gl:scale (* (radius object) 0.8) (* (radius object) 0.8) 1)
  (draw-point-list (vertices object) :polygon (color object))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (x (origin object)) (y (origin object)) 0)
  (gl:scale (radius object) (radius object) 1)
  (draw-point-list (outline object) :line-strip (outline-color object)))
  
(defun create-random-baddies (number inner-radius add-to-radius &optional (baddy-type 'baddy))
 "Creates 'number baddies to converge upon bourtange and collects them..."
 (loop for x from 1 to number collect 
   (let* ((p (multiply (rad->xy (random *tau*)) (+ inner-radius (random add-to-radius))))
    (mag (magnitude p)))
     (make-instance baddy-type :origin p 
      :velocity (multiply (multiply p (/ -1 mag)) (random 1.0))))))
      
(defclass program ()
  ((screen-width :accessor screen-width)
   (screen-height :accessor screen-height)
   (mouse-x :initarg :mouse-x :initform 0 :accessor mouse-x)
   (mouse-y :initarg :mouse-y :initform 0 :accessor mouse-y)
   (last-mouse-event :initarg :last-mouse-event :accessor last-mouse-event)
   (last-tick-time :initarg :last-tick-time :initform 0 :accessor last-tick-time)
   (player-core :initform (make-core 20) :accessor player-core)
   (baddies :initform (create-random-baddies 300 300 400) :accessor baddies)
   (just-killed-baddies :initform () :accessor just-killed-baddies)
   (dying-baddies :initform () :accessor dying-baddies)))
(defmethod draw ((object program))
  (draw (player-core object))
  (draw-list (dying-baddies object))
  (draw-list (baddies object)))
   
(defgeneric millis-since-last-tick (p)
  (:documentation "The number of milliseconds since the last set tick"))
(defmethod millis-since-last-tick ((p program))
  (- (millitime) (last-tick-time p)))
(defparameter *program* (make-instance 'program)
	"The main data structure for the game.")
	
(defun find-collided-objects (baddies object)
  "Finds baddies that collide with object and returns that list"
  (remove nil (loop for x from 0 to (- (length baddies) 1) collect
    (if (check-collision (nth x baddies) object) (nth x baddies) nil))))
    
(defun explode-baddies (baddies)
  "Explodinates some baddies"
  (remove nil 
    (loop for x from 0 to (- (length baddies) 1) collect
      (let ((baddy (nth x baddies)))
        (update-dying-unit baddy (+ (radius baddy) 1))
        (if (> (radius baddy) (explode-radius baddy)) nil baddy)))))

;(stepping functions)
(defun step-core-blast (blast baddies)
  "Progresses the life of the core blast weapon, returns dying baddies"
  (if (death-time blast) (return-from step-core-blast (list )))
  (update-unit blast :radius (+ (radius blast) (expansion-rate blast)))
  (let* ((hit-baddies (find-collided-objects baddies blast))
   (blast-strength (strength blast))
   (blast-life (life blast))
   (blast-defense (defense blast)))
    (remove nil
      (loop for x from 0 to (- (length hit-baddies) 1) collect 
        (let* ((baddy (nth x hit-baddies))
         (baddy-life-taken (* blast-strength (/ 1 (defense baddy))))
         (blast-life-taken (* (strength baddy) (/ 1 blast-defense))))
          (update-unit baddy :life (- (life baddy) baddy-life-taken))
          (update-unit blast :life (- (life blast) blast-life-taken))
          (if (<= 0 (life baddy)) baddy nil))))))
;(/stepping functions)	
  
(defun draw-display ()
  "Called every frame to draw things - this is our main game loop"
  ; update time
  (setf (last-tick-time *program*) (millitime))
  (apply-all-list (baddies *program*))
  (let* ((pc (player-core *program*))
   (baddies (baddies *program*))
   (blasted-baddies (step-core-blast (blast pc) baddies)))
    ;(format t "~%blasted: ~a" blasted-baddies)
    (setf (just-killed-baddies *program*) 
      (append  
        blasted-baddies
        (find-collided-objects baddies pc)))
  (if (<= (life (blast pc)) 0) (setf (blast pc) (cooldown (blast pc)))))
  ; remove just-killed-baddies from baddies
  (setf (baddies *program*) (filter (just-killed-baddies *program*) (baddies *program*)))
  ; add just-killed to dying
  (setf (dying-baddies *program*) 
    (append 
      (dying-baddies *program*)
      (just-killed-baddies *program*)))
  (explode-baddies (dying-baddies *program*))
  ; draw it out
  (draw *program*))
;--------------------------------------
;  Gl/Glut stuff
;--------------------------------------
(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p))
  (:default-initargs :width 1400 :height 1100
   :title "Do Not Faile Bourtange"
   :pos-x 20 :pos-y 0
   :mode '(:double :rgb :depth)
   :fullscreen nil
   :tick-interval (round 1000 60)))  ; milliseconds per tick

(defmethod glut:display-window :before ((win my-window))
  (gl:line-width 1) 
  (gl:shade-model :smooth)        
  (gl:clear-color 0 0 0 0)  
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))          

(defmethod glut:display ((win my-window))
  ; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ; reset the modelview matrix
  (gl:matrix-mode :projection)
  (gl:load-identity)              
  (let ((w (screen-width *program*)) (h (screen-height *program*)))
    (gl:scale (/ 1 w) (/ 1 h) 1))
  (gl:matrix-mode :modelview)
  ; draw code
  (draw-display)
  ; swap the buffer onto the screen
  (glut:swap-buffers))            

(defmethod glut:reshape ((win my-window) width height)
  (setf (screen-width *program*) width)
  (setf (screen-height *program*) height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  (glu:ortho-2d -1 1 -1 1)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity))             ; reset the matrix

(defmethod glut:keyboard ((win my-window) key xx yy)
  (case key
    ((#\Escape) 
    (glut:destroy-current-window)
	  (quit))))               ; when we get an 'f'

; mouse mouse while down/up
(defmethod glut:mouse ((win my-window) button state x y)
  ; do some stuff where we compare the last mouse event
  (setf (last-mouse-event *program*) (make-mouse-event button state x y)))
  
; mouse move passively (no button down)
(defmethod glut:passive-motion ((win my-window) x y)
  (setf (mouse-x *program*) x)
  (setf (mouse-y *program*) y))

(defmethod glut:tick ((win my-window))
  (glut:post-redisplay))        ; tell GLUT to redraw
;--------------------------------------
;  Setup and go
;--------------------------------------
(defun main ()
  (glut:display-window (make-instance 'my-window)))
  
(main)