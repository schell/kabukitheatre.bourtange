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
(defparameter *g* .0001
  "The gravitational constant.") ;px/milisecond
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

