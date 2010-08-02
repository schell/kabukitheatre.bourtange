(in-package :bourtange)

;--------------------------------------
;  Gl/Glut stuff
;--------------------------------------
(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (program :initarg :program :accessor program))
  (:default-initargs :width 1400 :height 1100
                     :title *headline*
                     :pos-x 20 :pos-y 0
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
                     :tick-interval (round 1000 60))) ; milliseconds per tick

(defmethod glut:display-window :before ((win my-window))
  (gl:line-width 1)
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod glut:display ((this my-window))
  ;; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; reset the modelview matrix
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((/w (/ (/ *screen-width* 2)))
        (/h (/ (/ *screen-height* 2))))
    (gl:scale /w /h 1))
  (gl:matrix-mode :modelview)
  ;; draw code
  (draw-display (program this))
  ;; swap the buffer onto the screen
  (glut:swap-buffers))

(defmethod glut:reshape ((this my-window) width height)
  (setf *screen-width* width
        *screen-height* height)
  (gl:viewport 0 0 width height)        ; reset the current viewport
  (gl:matrix-mode :projection)          ; select the projection matrix
  (gl:load-identity)                    ; reset the matrix
  (glu:ortho-2d -1 1 -1 1)
  (gl:matrix-mode :modelview)           ; select the modelview matrix
  (gl:load-identity)                    ; reset the matrix
  (set-store-position (program this)))

(defmethod glut:keyboard ((this my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ;; pause the game
    (#\p
     (setf (is-paused (program this)) (not (is-paused (program this)))))
    ;; reset the program (new game)
    (#\r
     (setf (program this) (set-store-position (make-program))))
    (#\Escape
     (glut:destroy-current-window)
     ;; THERE IS NO QUIT
     (quit))))                          ; when we get an 'f'

(defun update-mouse (mouse x y &optional button state)
  ;; transmute coordinates
  (let ((new-x (- x (/ *screen-width* 2)))
        (new-y (- (- y (/ *screen-height* 2)))))
    (setf (x (origin mouse)) new-x
          (y (origin mouse)) new-y))
  ;; set mouse position and stuff
  (when button
    (setf (button mouse) button))
  (when state
    (setf (last-state mouse) (state mouse))
    (setf (state mouse) state))
  mouse)

;;; mouse mouse while down/up
(defmethod glut:mouse ((this my-window) button state x y)
  (setf (mouse (program this))
        (update-mouse (mouse (program this)) x y button state)))

;; mouse move passively (no button down)
(defmethod glut:passive-motion ((this my-window) x y)
  (setf (mouse (program this))
        (update-mouse (mouse (program this)) x y)))

(defmethod glut:tick ((win my-window))
  (glut:post-redisplay))        ; tell GLUT to redraw

;;;--------------------------------------
;;;  Setup and go
;;;--------------------------------------
(defun main ()
  (glut:display-window (make-instance 'my-window :program (make-program))))
