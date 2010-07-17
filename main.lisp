; Bourtange - a fort defense game
; Schell Scivally
; Aaron Maus 
; Sat Jul  3 11:31:06 PDT 2010
;--------------------------------------
;  Dependencies
;--------------------------------------
(load "loadopengl.lisp")
;--------------------------------------
;  Some utility functions
;--------------------------------------
(defun millitime ()
  "The internal real time measured in milliseconds"
  (* (get-internal-real-time) (/ 1000 internal-time-units-per-second)))
(defgeneric multiply (x y)
  (:documentation "Multiplies two things together."))
;--------------------------------------
;  Game definitions
;--------------------------------------
(defparameter *tau* 6.283185307179586)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))
(defun make-point (x y)
  "Utility function. Creates a point at (x, y)"
  (make-instance 'point :x x :y y))
(defmethod multiply ((p point) s)
  (make-point (* (x p) s) (* (y p) s)))
(defun rad->xy (theta)
  "Utility function. Converts a radian measure to vector (list x y)"
  (make-point (cos theta) (sin theta)))
  
(defun make-square (half-width)
  "Utility function. Creates a list of 4 points arranged in a square"
  (let ((hw half-width) (-hw (* -1 half-width)))
  (list (make-point hw hw)
   (make-point hw -hw)
   (make-point -hw -hw)
   (make-point -hw hw))))
   
(defun make-circle (radius &optional (points 360)) 
  "Utility function. Creates a circle of 'points points"
  (loop for x from 1 to points collect (multiply (rad->xy (* x (/ *tau* points))) radius)))

(defclass color ()
  ((r :initarg :r :initform 1 :accessor r)
   (b :initarg :b :initform 1 :accessor b)
   (g :initarg :g :initform 1 :accessor g)))
(defun make-color (r b g)
  "Utility function. Creates a color rbg"
  (make-instance 'color :r r :b b :g g))
   
(defclass display-object ()
  ((origin :initarg :origin :initform (make-point 0 0) :accessor origin)
   (color :initarg :color :initform (make-color 1 1 1) :accessor color)
   (primitive :initarg :primitive :initform :polygon :accessor primitive
    :documentation "The opengl primitive to draw this object with")
   (vertices :initarg :vertices :initform (make-square 10) :accessor vertices)))
(defgeneric initialize (object)
  (:documentation "Initializes a display-object"))
(defmethod initialize ((object display-object))
  (setf (origin object) (make-point 0 0))
  (setf (vertices object) (list (make-point 0 0))))
(defgeneric draw (object)
  (:documentation "Draws the display-object with OpenGL"))
(defmethod draw ((object display-object))
  (let* ((color (color object)) 
   (r (r color)) 
   (g (g color)) 
   (b (b color))
   (origin (origin object))
   (ox (x origin))
   (oy (y origin))
   (vertices (vertices object))
   (primitive (primitive object)))
    (gl:color r b g)
    (gl:with-primitive primitive
      (dolist (vertex vertices)
        (gl:vertex (+ ox (x vertex)) (+ oy (y vertex)) 0)))
    (gl:flush)))

(defclass display-circle (display-object)
  ((radius :initarg :radius :initform 10 :accessor radius))
  (:default-initargs :vertices (make-circle 10)))

(defclass event ()
  ((name :initarg :name :initform "event" :accessor name)))
(defgeneric out (e)
  (:documentation "Prints the event to stdout"))
(defmethod out ((e event))
  (format t "~%event :name ~a" (name e)))

(defclass mouse-event (event)
  ((button :initarg :button :accessor button)
   (state :initarg :state :accessor state)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y))
  (:default-initargs :name "mouse-event"))
(defun make-mouse-event (button state x y) 
  (make-instance 'mouse-event :button button :state state :x x :y y))
(defmethod out ((e mouse-event))
  (format t "~%mouse-event :name ~a :button ~a :state ~a :x ~a :y ~a"
   (name e) (button e) (state e) (x e) (y e)))

(defclass program ()
  ((screen-width :accessor screen-width)
   (screen-height :accessor screen-height)
   (mouse-x :initarg :mouse-x :initform 0 :accessor mouse-x)
   (mouse-y :initarg :mouse-y :initform 0 :accessor mouse-y)
   (last-mouse-event :initarg :last-mouse-event :accessor last-mouse-event)
   (last-tick-time :initarg :last-tick-time :initform 0 :accessor last-tick-time)
   (display-list :initarg :display-list :accessor display-list
    :documentation "Holds the display information for this program.")))
(defgeneric millis-since-last-tick (p)
  (:documentation "The number of milliseconds since the last set tick"))
(defmethod millis-since-last-tick ((p program))
  (- (millitime) (last-tick-time p)))
;--------------------------------------
;  Game specific
;--------------------------------------
(defparameter *program* (make-instance 'program)
	"The main data structure for the game.")
;--------------------------------------
;  Gl/Glut stuff
;--------------------------------------
(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p))
  (:default-initargs :width 800 :height 600
   :title "Do Not Faile Bourtange"
   :pos-x 100 :pos-y 100
   :mode '(:double :rgb :depth)
   :fullscreen nil
   :tick-interval (round 1000 60)))  ; milliseconds per tick

(defmethod glut:display-window :before ((win my-window))
  (gl:shade-model :smooth)        ; enables smooth shading
  (gl:clear-color 0 0 0 0)        ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:enable :depth-test)         ; enable depth testing
  (gl:depth-func :lequal)         ; okay to write pixel if its depth
                                  ; is less-than-or-equal to the
                                  ; depth currently written
                                  ; really nice perspective correction
  (gl:hint :perspective-correction-hint :nicest)

  (when (fullscreen-p win)        ; check to see if fullscreen needed
    (glut:full-screen)))          ; if so, then tell GLUT

(defmethod glut:display ((win my-window))
                                  ; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)              ; reset the modelview matrix
  (let ((w (screen-width *program*)) (h (screen-height *program*)))
    (gl:scale (/ 1 w) (/ 1 h) 1))
  ;draw code
  (draw (make-instance 'display-circle :radius 20 :vertices (make-circle 20)))
  (glut:swap-buffers))            ; swap the buffer onto the screen

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
  (declare (ignore xx yy))
  (case key
    ((#\Escape) 
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
  ; update time
  (setf (last-tick-time *program*) (millitime))
  (glut:post-redisplay))        ; tell GLUT to redraw
;--------------------------------------
;  Setup and go
;--------------------------------------

(glut:display-window (make-instance 'my-window))