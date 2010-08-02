(in-package :bourtange)

;;;  ._________.
;;;  | 1010101 |      the program...
;;;  | 1001001 |
;;;  \---------/
(defclass program ()
  ((is-paused :initform nil :accessor is-paused)
   (spawning-belt :initform (make-display-ring *outer-spawning-radius* *inner-spawning-radius* (make-color 255 255 255 10)) :accessor spawning-belt)
   (baddies :initform (create-random-baddies *total-baddies* *outer-spawning-radius* *inner-spawning-radius*) :accessor baddies)
   (goodies :initform (list (make-instance 'core :weapons (list (make-instance 'core-blast)))) :accessor goodies)
   (dying-bodies :initform () :accessor dying-bodies)
   (resources :initform 100 :accessor resources)
   (mouse :initform (make-instance 'mouse :radius 2 :vertices (make-circle)) :accessor mouse)
   (weapon-store :initform (make-instance 'weapon-store) :accessor weapon-store)
   (selected-weapon :initform nil :accessor selected-weapon)
   (selected-core :initform nil :accessor selected-core)))
(defun make-program ()
  (make-instance 'program)) ; return the program object
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
; advance our program! THIS is the main logic loop
(defmethod advance ((this program) milliseconds)
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
        (mapcar (lambda (baddy) (advance baddy milliseconds))
                (baddies this)))
  ;; advance the goodies, dead goodies will be renewed in 'advance
  (setf (goodies this)
        (loop for goody in (goodies this)
           ;; right here we're probably advancing cores, which in turn
           ;; advance each core's weapons
           for advanced = (advance goody milliseconds)
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
                  (setf (origin weapon) (origin (selected-core this)))
                  (setf (ndx weapon) (1+ (length (weapons (selected-core this)))))
                  (setf (weapons (selected-core this)) (append
                                                        (weapons (selected-core this))
                                                        (list weapon)))
                  (setf (selected-weapon this) nil)
                  (setf (selected-core this) nil))))
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
  (let ((collided (collidedp (weapon-store this) (mouse this))))
    (if collided
        (progn
          (draw-resources (cost (first collided))
                          4
                          (add (origin (first collided))
                               (make-point 0 (- 0 (radius (first collided)) 4))))
          (setf (weapon-store this) (advance (weapon-store this) milliseconds)))
        (setf (weapon-store this) (cooldown (weapon-store this) milliseconds))))
  this) ; return this program

;;
(defun draw-display (program)
  "Called every frame to draw things - this is our main game loop"
  ;; find milliseconds since last tick
  (let ((time (millitime)))
    (unless *last-tick*
      (setf *last-tick* time))
    ;; update program
    (setf program (advance program (- time *last-tick*)))
    ;; draw it out
    (draw program)
    ;; set the last tick
    (setf *last-tick* time)
    program))
