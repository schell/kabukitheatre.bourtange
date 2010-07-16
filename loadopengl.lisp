(require 'asdf)
(require 'asdf-install)
(asdf:load-system :cl-opengl)   ; load OpenGL bindings
(asdf:load-system :cl-glu)      ; load GLU bindings
(asdf:load-system :cl-glut)		; load GLUT bindings