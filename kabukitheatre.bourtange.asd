(asdf:defsystem kabukitheatre.bourtange
  :version "0"
  :description "A fort defense game in Lisp in 30 days."
  :maintainer "Schell Scivally <schell@efnx.com>"
  :author "Schell Scivally <schell@efnx.com>"
  :licence "BSD-style"
  :depends-on (cl-opengl cl-glu cl-glut)
  :serial t
  :components ((:file "main")))
