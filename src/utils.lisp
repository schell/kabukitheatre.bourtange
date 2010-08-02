(in-package :bourtange)

;;;--------------------------------------
;;;  Some utility functions
;;;--------------------------------------
(defun millitime ()
  "The internal real time measured in milliseconds"
  (* (get-internal-real-time) (/ 1000 internal-time-units-per-second)))
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

