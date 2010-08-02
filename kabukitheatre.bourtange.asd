(asdf:defsystem kabukitheatre.bourtange
  :version "0"
  :description "A fort defense game in Lisp in 30 days."
  :maintainer "Schell Scivally <schell@efnx.com>"
  :author "Schell Scivally <schell@efnx.com>"
  :licence "BSD-style"
  :depends-on (cl-opengl cl-glu cl-glut)
  :serial t
  :components
  ((module "src"
           :serial t
           :components
           ((:file "package")
            (:file "parameters")
            (:file "utils")
            (:file "color")
            (:file "geometry")
            (:file "objects")
            (:file "mouse")
            (module "game-logic"
                    :serial t
                    :components
                    ((:file "unit")
                     (:file "baddy")
                     (:file "weapon")
                     (:file "core")
                     (:file "store")
                     (:file "program")))
            (:file "window")))))
