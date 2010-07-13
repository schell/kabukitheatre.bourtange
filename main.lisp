#!
; Bourtange - a fort defense game
; Schell Scivally
; Aaron Maus 
; Sat Jul  3 11:31:06 PDT 2010
; 
; things we need - asdf, asdf-install, cffi, babel, alexandria, trivial-features

;; load opengl
(require 'asdf)
(require 'asdf-install)
(asdf:load-system :cl-opengl)   ; load OpenGL bindings
(asdf:load-system :cl-glu)      ; load GLU bindings
(asdf:load-system :cl-glut)		; load GLUT bindings
	
; define our window
(defclass my-window (glut:window)
	()
	(:default-initargs :width 400 :height 300
		:title "Ontbreek geen Bourtange!!!"
		:mode '(:double :rgb :depth)))

; initialization method
(defmethod glut:display-window :before ((win my-window))
	; prepare opengl
	(gl:shade-model :smooth)
	(gl:clear-color 0 0 0 0)
	(gl:clear-depth 1)
	(gl:enable :depth-test)		; even though this is 2d
	(gl:depth-func :lequal)
	(gl:hint :perspective-correction-hint :nicest))
	
(defun pl (s)
	(format t (concatenate `string s "~1%")))

; additional glut meths
(defmethod glut:display ((win my-window))
	(gl:clear :color-buffer-bit :depth-buffer-bit)
	(gl:load-identity))

(defmethod glut:reshape ((win my-window) width height)
	; prepare viewport
	(gl:viewport 0 0 width height)
	; prepare projection
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(glu:perspective 45 (/ width (max height 1)) 1/10 100)
	; switch to model view
	(gl:matrix-mode :modelview)
	(gl:load-identity))

(pl "Bourtange!")

(glut:display-window (make-instance 'my-window))