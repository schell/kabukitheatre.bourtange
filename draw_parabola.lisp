;;loads parabola.lisp file, includings functions therein.  Needed for the function (parabola used below.  Notice that lisp likes forward slashes.
(load "c:/program files/clisp-2.48/work/parabola.lisp")


(defparameter *which-vertex* 0)
(defparameter *height* 500)
(defparameter *width* 500)

;;Outputs a list of n ordered pairs for the parabola through the inputted points xy1, xy2, xy3. Starting at start, the 'x' element of each ordered pair increases by incr.
(defun parabola-points (xy1 xy2 xy3 n start incr)
  (let (pbola-function a b c point-list)
    (setf pbola-function (parabola xy1 xy2 xy3))
    (setf a (elt pbola-function 0))
    (setf b (elt pbola-function 1))
    (setf c (elt pbola-function 2))
    (dotimes (i n)
      (let ((x (+ start (* i incr))))
	(setf point-list (append point-list (list (list x (+ (* a (sqr x)) (* b x) c)))))))
    point-list))


;;Defines the class for a parabola with 2 slots: 1 for the function and 1 for the list of points, with initializers, and accessors.
(defclass paraobj () ((function :initarg :function :accessor para-function)
		      (point-list :initarg :point-list :accessor para-points)
		      (defing-points :initarg :defing-points :accessor para-def-points)))

(defclass genobj () ((xcoord :initarg :xcoord :accessor xcoord)
		     (ycoord :initarg :ycoord :accessor ycoord)
		     (points :initarg :points :accessor objpoints)
		     (color :initarg :color :accessor objcolor)
		     (style :initarg :style :accessor objstyle)))

(defun make-genobj (x1 y1 point-list color1 style1)
  (make-instance 'genobj
		 :xcoord x1
		 :ycoord y1
		 :points point-list
		 :color color1
		 :style style1))

(defmethod draw-genobj ((gobj genobj))
  (gl:color (elt (objcolor gobj) 0) (elt (objcolor gobj) 1) (elt (objcolor gobj) 2))
  (gl:begin (objstyle gobj))
  (let ((gpoints (objpoints gobj)))
    (dotimes (i (length gpoints))
      (gl:vertex (elt (elt gpoints i) 0) (elt (elt gpoints i) 1))))
  (gl:end))

(defmethod move-genobj-by ((gobj genobj) x1 y1)
  (setf (xcoord gobj) (+ (xcoord gobj) x1))
  (setf (ycoord gobj) (+ (ycoord gobj) y1))
  (let ((old-points (objpoints gobj)))
    (dotimes (i (length old-points))
      (setf (elt (elt old-points i) 0) (+ (elt (elt old-points i) 0) x1))
      (setf (elt (elt old-points i) 1) (+ (elt (elt old-points i) 1) y1)))
    (setf (objpoints gobj) old-points))
  nil)
  
  

(defparameter *moon-base* (make-genobj 250 0 (append (circle 250 0 35 20) 
						     (append (circle 258 0 25 20)
							     (circle 250 0 15 20)))
						     (list 1 1 1) :line-loop))
				


(defun make-parabola (xy1 xy2 xy3 n start incr)
  (make-instance 'paraobj
		 :function (parabola xy1 xy2 xy3)
		 :point-list (parabola-points xy1 xy2 xy3 n start incr)
		 :defing-points (list xy1 xy2 xy3)))

(defparameter *the-parabola* (make-parabola '(0 250) '(2 5) '(3 8) 10 250 10))

(defmethod change-point ((pobj paraobj) point x1 y1)
  (let ((new-def-points (para-def-points pobj)))
    (setf (elt new-def-points point) (list x1 y1))
    (setf (para-def-points pobj) new-def-points)
    (setf (para-points pobj) (parabola-points (elt new-def-points 0) (elt new-def-points 1) (elt new-def-points 2) 100 0 5))
    (setf (para-function pobj) (parabola (elt new-def-points 0) (elt new-def-points 1) (elt new-def-points 2))))
  nil)
  

;;Does the same job as the previous draw function, but takes a paraobj class object.
(defmethod draw-paraobj ((pobj paraobj))
  (let ((points (para-points pobj)))
    (dotimes (i (length points))
	 (gl:vertex (elt (elt points i) 1) (elt (elt points i) 0)))))
				  
;;makes the window class	 
(defclass my-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
                     :mode '(:single :rgb) :title "draw_parabola.lisp"))

;;Supposedly clears the window 'before' and sets smoothings stuffs.
(defmethod glut:display-window :before ((w my-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth))

;;AFIK, when the window displays the first time, clears it, and draws.
;(defmethod glut:display ((w my-window))
 ; (gl:clear :color-buffer)
 ; (gl:with-primitives :line-strip
 ;   (gl:color 1 0 0)
 ;   (draw-paraobj *the-parabola*))
 ; (gl:with-primitives :points
 ;   (gl:color 0 1 1)
 ;   (gl:vertex (elt *the-vertex* 0) (elt *the-vertex* 1))))

(defmethod glut:display ((w my-window))
  (draw-genobj *moon-base*))



 ;; (Reshapes and redraws if the window is resized.
;(defmethod glut:reshape ((w my-window) width height)
;  (gl:viewport 0 0 width height)
;  (gl:matrix-mode :projection)
;  (gl:load-identity)
;  (if (<= width height)
;      (glu:ortho-2d 0 30 0 (* 30 (/ height width)))
;      (glu:ortho-2d 0 (* 30 (/ width height)) 0 30))
;  (gl:matrix-mode :modelview))

(defmethod glut:reshape ((w my-window) width height)
  (glu:ortho-2d 0 width 0 height))
			 


;;listens for a keypress.
(defmethod glut:keyboard ((w my-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:special ((w my-window) key x y)
  (declare (ignore x y))
  (case key
    (:KEY-UP (move-genobj-by *moon-base* 0 2)
	     (glut:post-redisplay))))

(defmethod glut:mouse ((w my-window) button state x1 y1)
  (case button
    (:left-button 
     (if (eq state :down)
	 (mouse-down 0 x1 y1)))
    (:right-button
     (if (eq state :down)
	 (mouse-down 1 x1 y1)))))

(defun mouse-down (button x2 y2)
  (if (= button 0)
      (if (= *which-vertex* 0)
	  (progn
	    (change-point *the-parabola* 1 (- 500 y2) x2)
	    (setf *which-vertex* 1)
	    (gl:begin :line-loop)
	    (gl:color 0 1 1)
	    (draw-circle x2 (- 500 y2) 10 10)
	    (gl:end))
	  (progn
	    (change-point *the-parabola* 2 (- 500 y2) x2)
	    (gl:clear :color-buffer)
	    (gl:begin :line-strip)
	    (gl:color 1 0 0)
	    (draw-paraobj *the-parabola*)
	    (gl:end)
	    (draw-genobj *the-circle*)
	    (setf *which-vertex* 0))))
  (if (= button 1)
      (progn
	(change-point *the-parabola* 2 (- 500 y2) x2)
	(change-point *the-parabola* 1 (- 475 y2) (- 250 (/ (- x2 250) 25)))
	(gl:clear :color-buffer)
	(gl:begin :line-strip)
	(gl:color 0 1 0)
	(draw-paraobj *the-parabola*)
	(gl:end))))

(defun circle (x y r n)
  (let (circle-points cur-radius)
    (dotimes (i n)
      (setf cur-radius (* i (/ (* 2 pi) n)))
      (setf circle-points (append circle-points (list (list (+ (* (cos cur-radius) r) x) (+ (* (sin cur-radius) r) y))))))
    circle-points))
      
(defun draw-circle (x1 y1 r1 n1)
  (let ((points (circle x1 y1 r1 n1)))
    (dotimes (i (length points))
      (gl:vertex (elt (elt points i) 0) (elt (elt points i) 1)))))
     	  
(defun rb-smooth ()
  (glut:display-window (make-instance 'my-window)))