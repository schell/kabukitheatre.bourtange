; Bourtange - a fort defense game
; Schell Scivally
; Aaron Maus 
; Sat Jul  3 11:31:06 PDT 2010
;--------------------------------------
;  Dependencies
;--------------------------------------
(load "loadopengl.lisp")
(load "shapes.lisp")
;--------------------------------------
;  Some utility functions
;--------------------------------------
(defun queue (item place)
  "Queues the 'item to the back of 'place"
  (setf (cdr place) (cons item nil)))
;--------------------------------------
;  Game definitions
;--------------------------------------
(defclass program ()
  ((mouse-x :initarg :mouse-x :initform 0 :accessor mouse-x)
   (mouse-y :initarg :mouse-y :initform 0 :accessor mouse-y)
   (display-list :initarg :display-list :accessor display-list
    :documentation "Holds the display information for this program.")))

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))
(defun make-point (x y)
  "Global utility function. Creates a point at (x, y)"
  (make-instance 'point :x x :y y))

(defclass display-object ()
  ((origin :initarg :origin :initform (make-point 0 0) :accessor origin)
   (vertices :initarg :vertices :initform (list (make-point 0 0)) :accessor vertices)))
(defgeneric initialize (object)
  (:documentation "Initializes a display-object"))
(defmethod initialize ((object display-object))
  (setf (origin object) (make-point 0 0))
  (setf (vertices object) (list (make-point 0 0))))
(defgeneric draw (object)
  (:documentation "Draws the display-object with OpenGL"))
(defmethod draw ((object display-object))
  (dolist (vertex (vertices object))
    (format t "~%x:~a y:~a" (x vertex) (y vertex))))

(defclass display-circle (display-object)
  ((radius :initarg :origin :initform 1 :accessor radius)))
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
  ;draw code
  
  (glut:swap-buffers))            ; swap the buffer onto the screen

(defmethod glut:reshape ((win my-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity))             ; reset the matrix

(defmethod glut:keyboard ((win my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\Escape) 
	  (quit))))               ; when we get an 'f'
		
(defmethod glut:mouse ((win my-window) button state x y))
  (setf (mouse-x *program*) x)
  (setf (mouse-y *program*) y))

(defmethod glut:tick ((win my-window))
  (glut:post-redisplay))        ; tell GLUT to redraw
;--------------------------------------
;  Setup and go
;--------------------------------------
(defparameter *program* (make-instance 'program)
	"The main data structure for the game.")
(glut:display-window (make-instance 'my-window))