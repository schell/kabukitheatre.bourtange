(load "loadopengl.lisp")

;;Just a quicky to clean up squaring.
(defun sqr (x)
  (* x x))

;;Input of 3 binomial pairs, outputs the coefficients A,B, and C for y = Ax^2 + Bx +c, the parabola through the three inputted points.
(defun parabola (xy1 xy2 xy3);defines function, name, and arguments
  (if (not (and (listp xy1) (listp xy2) (listp xy3)));checks to make sure each argument is a list, returns nil otherwise
      (return-from parabola nil))
  (if (not (and (= (length xy1) 2) (= (length xy2) 2) (= (length xy3))));checks to make sure each argument list has 2 elements, returns nil otherwise
      (return-from parabola nil))
  (let (a b c x1 y1 x2 y2 x3 y3);let declares local vars only available to this function
    (setf x1 (elt xy1 0));sets local variables to the x and y values of each of the arguments
    (setf y1 (elt xy1 1))
    (setf x2 (elt xy2 0))
    (setf y2 (elt xy2 1))
    (setf x3 (elt xy3 0))
    (setf y3 (elt xy3 1))
    (dolist (i (list x1 x2 x3 y1 y2 y3));checks to make sure all x's and y's are numbers, returns nil otherwise
      (if (not (numberp i))
	  (return-from parabola nil)))
    ;;calculations for the coefficients A, B, and C
    (if (not (or (= x1 x2) (= x1 x3) (= x2 x3)))
	(progn
	  (setf b (/ (- y2 y3 (/ (* (- y1 y2) (- (sqr x2) (sqr x3))) (- (sqr x1) (sqr x2)))) (- (+ x2 (/ (* (- x2 x1) (-  (sqr x2) (sqr x3))) (- (sqr x1) (sqr x2)))) x3)))
	  (setf a (/ (- y1 y2 (* b (- x1 x2))) (- (sqr x1) (sqr x2))))
	  (setf c (- y1 (* a (sqr x1)) (* b x1)))
	  (list a b c))
	(progn
	  (if (= x1 x2)
	      (setf x2 (+ x2 1)))
	  (if (= x1 x3)
	      (setf x3 (+ x3 1)))
	  (if (= x2 x3)
	      (setf x3 (+ x3 1)))
	  (parabola (list x1 y1) (list x2 y2) (list x3 y3))))))

(defparameter *height* 500)
(defparameter *width* 800)
(defparameter *object-list* (list ))
(defparameter *gunfire-list* (list ))
(defparameter *missile-list* (list ))
(defparameter *missile-speed* 30)
(defparameter *gunfire-speed* 60)
(defparameter *gunfire-rate* 100)
(defparameter *gunfiring* nil)
(defparameter *gunfire-coords* (list 0 0))
(defparameter *exiting* nil)
(defparameter *missile-mag* 10)
(defparameter *missile-max* 10)
(defparameter *missile-refresh-rate* 500)
(defparameter *missile-list* nil)
(defparameter *missile-count* 0)
(defparameter *missile-dmg* 600)
(defparameter *rain-frequency* 2500)
(defparameter *rain-number* 1)
(defparameter *rain-size* 15)
(defparameter *rain-speed* 4)
(defparameter *moon-base-hp* 5000)
(defparameter *moon-base-hp-max* 5000)

(defun end-game ()
  (setf *exiting* T)
  (glut:destroy-current-window))

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

(defun random-between (n m)
  (let (num)
    (if (> m n)
      (setf num (+ n (random (float(- m n))))))
    (if (= m n)
	(setf num m))
    (if (< m n)
      (setf num (+ m (random (float (- n m))))))
    num))

(defun line-intersect (line1 line2)
  (let (m1 m2 x1 y1 x2 y2 a1 b1 a2 b2 intx inty xmin xmax)
    (setf x1 (elt (elt line1 0) 0))
    (setf y1 (elt (elt line1 0) 1))
    (setf x2 (elt (elt line1 1) 0))
    (setf y2 (elt (elt line1 1) 1))
    (setf a1 (elt (elt line2 0) 0))
    (setf b1 (elt (elt line2 0) 1))
    (setf a2 (elt (elt line2 1) 0))
    (setf b2 (elt (elt line2 1) 1))
    (setf xmin (min x1 x2 a1 a2))
    (setf xmax (max x1 x2 a1 a2))
    (if (and (= x1 x2) (= a1 a2))
	(if (and (= x1 a1) 
		 (or (and (>= (max y1 y2) (min b1 b2)) (<= (min y1 y2) (max b1 b2))) 
		     (and (<= (max y1 y2) (min b1 b2)) (>= (min y1 y2) (max b1 b2)))))
	    (return-from line-intersect (list x1 (/ (+ (min (max y1 y2) (max b1 b2)) (max (min y1 y2) (min b1 b2))) 2)))
	    (return-from line-intersect nil))
	(return-from line-intersect nil))
    (if (= x1 x2)
	(progn
	  (setf intx x1)
	  (setf m2 (/ (- b2 b1) (- a2 a1)))
	  (setf inty (- (+ (* m2 x1) b1) (* m2 a1)))
	  (return-from line-intersect (list intx inty))))
    (if (= a1 a2)
	(progn
	  (setf intx a1)
	  (setf m1 (/ (- y2 y1) (- x2 x1)))
	  (setf inty (- (+ (* m1 a1) y1) (* m1 x1)))
	  (return-from line-intersect (list intx inty))))
    (setf m1 (/ (- y2 y1) (- x2 x1)))
    (setf m2 (/ (- b2 b1) (- a2 a1)))
    (setf intx (/ (- (+ (* m1 x1) b1) y1 (* m2 a1)) (- m1 m2)))
    (setf inty (+ (* m1 (- intx x1)) y1))
    (if (and (>= intx xmin) (<= intx xmax))
	(list intx inty)
	nil)))

(defun angle-between-2pts (point1 point2)
  (let ((x1 (elt point1 0)) (y1 (elt point1 1)) (x2 (elt point2 0)) (y2 (elt point2 1)) (angle 0))
    (if (> x2 x1)
	(progn
	  (if (> y2 y1)
	      (setf angle (atan (/ (- y2 y1) (- x2 x1)))))
	  (if (< y2 y1)
	      (setf angle (- (* 2 pi) (atan (/ (- y1 y2) (- x2 x1))))))
	  (if (= y2 y1)
	      (setf angle 0))))
    (if (< x2 x1)
	(progn
	  (if (> y2 y1)
	      (setf angle (- pi (atan (/ (- y2 y1) (- x1 x2))))))
	  (if (< y2 y1)
	      (setf angle (+ pi (atan (/ (- y1 y2) (- x1 x2))))))
	  (if (= y2 y1)
	      (setf angle pi))))
    (if (= x2 x1)
	(progn
	  (if (> y2 y1)
	      (setf angle (/ pi 2)))
	  (if (< y2 y1)
	      (setf angle (* (/ pi 2) 3)))
	  (if (= y2 y1)
	      (setf angle nil))))
    angle))

(defun distance (point1 point2)
  (sqrt (+ (sqr (- (elt point1 0) (elt point2 0))) (sqr (- (elt point1 1) (elt point2 1))))))

(defun defline (point1 point2)
  (let (m yint)
    (if (= (elt point1 0) (elt point2 0))
	(return-from defline (list nil (elt point1 0))))
    (setf m (/ (- (elt point1 1) (elt point2 1)) (- (elt point1 0) (elt point2 0))))
    (setf yint (- (elt point1 1) (* m (elt point1 0))))
    (list m yint)))

(defun inside-trianglep (point triangle)
  (let (ang1 ang2 tang1 tang2)
    (setf ang1 (angle-between-2pts (elt triangle 0) point))
    (setf ang2 (angle-between-2pts (elt triangle 1) point))
    (setf tang1 (list (angle-between-2pts (elt triangle 0) (elt triangle 1)) (angle-between-2pts (elt triangle 0) (elt triangle 2))))
    (setf tang1 (list (min (elt tang1 0) (elt tang1 1)) (max (elt tang1 0) (elt tang1 1))))
    (setf tang2 (list (angle-between-2pts (elt triangle 1) (elt triangle 2)) (angle-between-2pts (elt triangle 1) (elt triangle 0))))
    (setf tang2 (list (min (elt tang2 0) (elt tang2 1)) (max (elt tang2 0) (elt tang2 1))))
    (if (> (- (elt tang1 1) (elt tang1 0)) pi)
	(if (> (- (elt tang2 1) (elt tang2 0)) pi)
	    (if (and (or (<= ang1 (elt tang1 0)) (>= ang1 (elt tang1 1))) (or (<= ang2 (elt tang2 0)) (>= ang2 (elt tang2 1))))
		(return-from inside-trianglep t))
	    (if (and (or (<= ang1 (elt tang1 0)) (>= ang1 (elt tang1 1))) (and (>= ang2 (elt tang2 0)) (<= ang2 (elt tang2 1))))
		(return-from inside-trianglep t)))
	(if (> (- (elt tang2 1) (elt tang2 0)) pi)
	    (if (and (and (>= ang1 (elt tang1 0)) (<= ang1 (elt tang1 1))) (or (<= ang2 (elt tang2 0)) (>= ang2 (elt tang2 1))))
		(return-from inside-trianglep t))
	    (if (and (and (>= ang1 (elt tang1 0)) (<= ang1 (elt tang1 1))) (and (>= ang2 (elt tang2 0)) (<= ang2 (elt tang2 1))))
		(return-from inside-trianglep t)))))
  nil)

(defun triangulize-points (points center)
  (let (triangles)
    (dotimes (i (length points))
      (if (< i (- (length points) 1))
	  (setf triangles (append triangles (list (list center (elt points i) (elt points (+ i 1))))))
	  (setf triangles (append triangles (list (list center (elt points 0) (elt points i)))))))
    triangles))

(defun triangle-area (triangle)
  (if (or (equal (elt triangle 0) (elt triangle 1)) (equal (elt triangle 0) (elt triangle 2)) (equal (elt triangle 1) (elt triangle 2)))
      (return-from triangle-area 0))
  (* (distance (elt triangle 0) (elt triangle 1)) 
     (sin (abs (- (angle-between-2pts (elt triangle 0) (elt triangle 1))
		  (angle-between-2pts (elt triangle 0) (elt triangle 2)))))
     (distance (elt triangle 0) (elt triangle 2))
     (/ 1 2)))

(defun point-shape-area (point-list center1)
  (let ((shape-triangles (triangulize-points point-list center1)) (area 0))
    (dotimes (i (length shape-triangles))
      (setf area (+ area (triangle-area (elt shape-triangles i)))))
    area))

(defun triangles-area (triangle-list)
  (let ((area 0))
    (dotimes (i (length triangle-list))
      (setf area (+ area (triangle-area (elt triangle-list i)))))
    area))

(defun circle (x y r n)
  (let (circle-points cur-radius)
    (dotimes (i n)
      (setf cur-radius (* i (/ (* 2 pi) n)))
      (setf circle-points (append circle-points (list (list (+ (* (cos cur-radius) r) x) (+ (* (sin cur-radius) r) y))))))
    circle-points))

(defun line (point1 point2 n)
  (let ((line-function (defline point1 point2)) (line-points nil) (rise 0) (run 0) (incr 0))
    (setf rise (- (elt point2 1) (elt point1 1)))
    (setf run (- (elt point2 0) (elt point1 0)))
    (setf incr (/ run n))
    (dotimes (i (+ 1 n))
      (setf line-points (append line-points (list (list (+ (elt point1 0) (* i incr)) (+ (elt point1 1) (* (elt line-function 0) (+ (elt point1 0) (* i incr)))))))))
    line-points))

(defun star5 (x y r)
  (let (star-points cur-radius)
    (dotimes (i 7)
      (setf cur-radius (* i (/ (* 2 pi) 7) 3))
      (setf star-points (append star-points (list (list (+ (* (cos cur-radius) r) x) (+ (* (sin cur-radius) r) y))))))
    star-points))

(defun meteor (x y r n)
  (let (meteor-points cur-degree cur-radius)
    (dotimes (i n)
      (setf cur-degree (+ (* i(/ (* 2 pi) n)) (if (= (random 2) 1)
						   (random (/ (/ (* 2 pi) n) 3))
						   (* -1 (random (/ (/ (* 2 pi) n) 3))))))
      (setf cur-radius (+ (/ r 10) (random (* 2 r))))
      (setf meteor-points (append meteor-points (list (list (+ (* (cos cur-degree) cur-radius) x) (+ (* (sin cur-degree) cur-radius) y))))))
    meteor-points))

(defun convex-meteor (x y r n)
  (let (meteor-points cur-degree cur-radius)
    (dotimes (i n)
      (setf cur-degree (+ (* i(/ (* 2 pi) n)) (if (= (random 2) 1)
						   (random (/ (/ (* 2 pi) n) 3))
						   (* -1 (random (/ (/ (* 2 pi) n) 3))))))
      (setf cur-radius (random-between r (* 2 r)))
      (setf meteor-points (append meteor-points (list (list (+ (* (cos cur-degree) cur-radius) x) (+ (* (sin cur-degree) cur-radius) y))))))
    meteor-points))

(defun draw-circle (x1 y1 r1 n1)
  (let ((points (circle x1 y1 r1 n1)))
    (dotimes (i (length points))
      (gl:vertex (elt (elt points i) 0) (elt (elt points i) 1)))))

;;Defines the class for a parabola with 2 slots: 1 for the function and 1 for the list of points, with initializers, and accessors.
(defclass paraobj () ((function :initarg :function :accessor para-function)
		      (point-list :initarg :point-list :accessor para-points)
		      (defing-points :initarg :defing-points :accessor para-def-points)))

(defclass genobj () ((xcoord :initarg :xcoord :accessor xcoord)
		     (ycoord :initarg :ycoord :accessor ycoord)
		     (xvel :initarg :xvel :accessor xvel)
		     (yvel :initarg :yvel :accessor yvel)
		     (path :initarg :path :accessor objpath)
		     (rotv :initarg :rotv :accessor rotv)
		     (points :initarg :points :accessor objpoints)
		     (area :initarg :area :accessor objarea)
		     (color :initarg :color :accessor objcolor)
		     (style :initarg :style :accessor objstyle)))

(defun make-genobj (x1 y1 xv1 yv1 rotv1 point-list color1 style1)
  (make-instance 'genobj
		 :xcoord x1
		 :ycoord y1
		 :xvel xv1
		 :yvel yv1
		 :path nil
		 :rotv rotv1
		 :points point-list
		 :area (point-shape-area point-list (list x1 y1))
		 :color color1
		 :style style1))

(defun make-missile (x1 y1 xv1 yv1 rotv1 path1 point-list color1 style1)
  (make-instance 'genobj
		 :xcoord x1
		 :ycoord y1
		 :xvel xv1
		 :yvel yv1
		 :path path1
		 :rotv rotv1
		 :points point-list
		 :area (point-shape-area point-list (list x1 y1))
		 :color color1
		 :style style1))

(defparameter *moon-base* (make-genobj (/ *width* 2) 0 0 0 0 (list (list (- (/ *width* 2) 20) 1) (list (- (/ *width* 2) 15) 25) (list (+ (/ *width* 2) 15) 25) (list (+ (/ *width* 2) 20) 1))(list 1 1 1) :line-loop))

(defparameter *moon-base2* (make-genobj (/ *width* 2) 0 0 0 0 (line (list 0 1) (list *width* 1) 100) (list 1 1 1) :line-strip))


(defmethod coords ((gobj genobj))
  (list (xcoord gobj) (ycoord gobj)))

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

(defmethod move-genobj-to ((gobj genobj) x1 y1)
  (let ((old-points (objpoints gobj))
	(xdiff (- x1 (xcoord gobj)))
	(ydiff (- y1 (ycoord gobj))))
    (setf (xcoord gobj) x1)
    (setf (ycoord gobj) y1)
    (dotimes (i (length old-points))
      (setf (elt (elt old-points i) 0) (+ (elt (elt old-points i) 0) xdiff))
      (setf (elt (elt old-points i) 1) (+ (elt (elt old-points i) 1) ydiff)))
    (setf (objpoints gobj) old-points)))

(defun create-circle-obj (x1 y1 xv1 yv1 rotv1 r n)
  (setf *object-list* (append *object-list* (list (make-genobj x1 y1 xv1 yv1 rotv1 (circle x1 y1 r n) (list 1 1 1) :line-loop)))))

(defparameter *missile-mag-circles* (list (make-genobj 291 6 0 0 0 (circle 291 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 302 6 0 0 0 (circle 302 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 313 6 0 0 0 (circle 313 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 324 6 0 0 0 (circle 324 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 335 6 0 0 0 (circle 335 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 346 6 0 0 0 (circle 346 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 357 6 0 0 0 (circle 357 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 368 6 0 0 0 (circle 368 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 379 6 0 0 0 (circle 379 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 390 6 0 0 0 (circle 390 6 5 8) (list 0 1 1) :line-loop)
					  (make-genobj 401 6 0 0 0 (circle 401 6 5 8) (list 0 1 1) :line-loop)))

(defun create-missile (path1)
  (setf *missile-list* (append *missile-list* (list (list *missile-count* (make-missile (/ *width* 2) 0 0 0 1 path1 (star5 (/ *width* 2) 0 10) (list 1 0 0) :line-loop)))))
  (setf *missile-count* (+ *missile-count* 1))
  nil)

(defun create-meteor-obj (x1 y1 xv1 yv1 rotv1 r n)
  (setf *object-list* (append *object-list* (list (make-genobj x1 y1 xv1 yv1 rotv1 (meteor x1 y1 r n) (list 1 1 1) :line-loop)))))

(defun create-gunfire-obj (x1 y1 vel rotv1 l)
  (let (xv1 yv1 angle1)
    (setf angle1 (angle-between-2pts (list (/ *width* 2) 0) (list x1 y1)))
    (setf xv1 (* vel (cos angle1)))
    (setf yv1 (* vel (sin angle1)))
    (setf *gunfire-list* (append *gunfire-list* (list (make-genobj (/ *width* 2) 0 xv1 yv1 rotv1 (list (list (/ *width* 2) 0) (list (+ (/ *width* 2) (* l (cos angle1))) (* l (sin angle1)))) (list 1 1 1) :line-strip))))))

(defun create-convex-meteor-obj (x1 y1 xv1 yv1 rotv1 r n)
  (setf *object-list* (append *object-list* (list (make-genobj x1 y1 xv1 yv1 rotv1 (convex-meteor x1 y1 r n) (list 1 1 1) :line-loop)))))

(defun physics ()
  (dotimes (i (length *object-list*))
    (if (not (= (rotv (elt *object-list* i)) 0))
	(rotate-genobj-by (elt *object-list* i) (rotv (elt *object-list* i))))
    (if (not (and (= (xvel (elt *object-list* i)) 0) (= (yvel (elt *object-list* i)) 0)))
	(move-genobj-by (elt *object-list* i) (xvel (elt *object-list* i)) (yvel (elt *object-list* i))))))

(defun draw-object-list ()
  (dotimes (i (length *object-list*))
    (draw-genobj (elt *object-list* i))))

(defun draw-missile-list ()
  (dotimes (i (length *missile-list*))
    (rotate-genobj-by (elt (elt *missile-list* i) 1) 0.6)
    (draw-genobj (elt (elt *missile-list* i) 1))))

(defun draw-gunfire-list ()
  (dotimes (i (length *gunfire-list*))
    (draw-genobj (elt *gunfire-list* i))))

(defun draw-missile-mag ()
  (dotimes (i *missile-mag*)
    (draw-genobj (elt *missile-mag-circles* i))))

(defmethod rotate-genobj-by ((gobj genobj) s)
  (let (cur-s cur-r center cur-x cur-y)
    (setf center (list (xcoord gobj) (ycoord gobj)))
    (dotimes (i (length (objpoints gobj)))
      (setf cur-x (elt (elt (objpoints gobj) i) 0))
      (setf cur-y (elt (elt (objpoints gobj) i) 1))
      (setf cur-r (sqrt (+ (sqr (- cur-x (elt center 0))) (sqr (- cur-y (elt center 1))))))
      (setf cur-s (angle-between-2pts center (list cur-x cur-y)))
      (setf cur-s (+ cur-s s))
      (setf (elt (elt (objpoints gobj) i) 0) (+ (* cur-r (cos cur-s)) (elt center 0)))
      (setf (elt (elt (objpoints gobj) i) 1) (+ (* cur-r (sin cur-s)) (elt center 1))))))

(defun rotate-about-center (center point s)
  (let (cur-s cur-r cur-x cur-y)
    (setf cur-x (elt point 0))
    (setf cur-y (elt point 1))
    (setf cur-r (sqrt (+ (sqr (- cur-x (elt center 0))) (sqr (- cur-y (elt center 1))))))
    (setf cur-s (angle-between-2pts center (list cur-x cur-y)))
    (setf cur-s (+ cur-s s))
    (setf cur-x (+ (* cur-r (cos cur-s)) (elt center 0)))
    (setf cur-y (+ (* cur-r (sin cur-s)) (elt center 1)))
    (list cur-x cur-y)))    
	    
(defmethod get-genobj-traj ((gobj genobj))
  (let (trajectory-list pre-vertices xvel1 yvel1 rvel1 center)
    (setf pre-vertices (objpoints gobj))
    (setf xvel1 (xvel gobj))
    (setf yvel1 (yvel gobj))
    (setf rvel1 (rotv gobj))
    (setf center (list (+ (xcoord gobj) xvel1) (+ (ycoord gobj) yvel1)))    
    (dotimes (i (length pre-vertices))
      (let ((cur-x (elt (elt pre-vertices i) 0)) (cur-y (elt (elt pre-vertices i) 1)) (post-point nil))
	(setf cur-x (+ cur-x xvel1))
	(setf cur-y (+ cur-y yvel1))
	(setf cur-x (elt (rotate-about-center center (list cur-x cur-y) rvel1) 0))
	(setf cur-y (elt (rotate-about-center center (list cur-x cur-y) rvel1) 1))
	(setf post-point (list cur-x cur-y))
	(setf trajectory-list (append trajectory-list (list (list (elt pre-vertices i) post-point))))))
    trajectory-list))

(defmethod triangulize ((gobj genobj))
  (let ((points (objpoints gobj)) (center (list (xcoord gobj) (ycoord gobj))) (triangles nil))
    (dotimes (i (length points))
      (if (< i (- (length points) 1))
	  (setf triangles (append triangles (list (list center (elt points i) (elt points (+ i 1))))))
	  (setf triangles (append triangles (list (list center (elt points 0) (elt points i)))))))
    triangles))

(defmethod genobj-area ((gobj genobj))
  (let ((objtriangles (triangulize gobj)) (area 0))
    (dotimes (i (length objtriangles))
      (setf area (+ area (triangle-area (elt objtriangles i)))))
    area))

(defmethod shrink-genobj ((gobj genobj) percent)
  (let ((points (objpoints gobj)) (point-pos 0) (point1 nil) (center (coords gobj)) (angle 0) (distance 0))
    (setf point-pos (random (length points)))
    (setf point1 (elt points point-pos))
    (setf angle (+ (angle-between-2pts center point1) (random-between -0.1 0.1)))
    (setf distance (* (distance center point1) percent))
    (setf (elt (objpoints gobj) point-pos) (list (+ (elt center 0) (* distance (cos angle))) (+ (elt center 1) (* distance (sin angle)))))))
     
(defmethod collision-test ((gobj1 genobj) (gobj2 genobj))
  (let ((triangles2 (triangulize gobj2)) (points1 (objpoints gobj1)))
    (dotimes (i (length points1))
      (dotimes (j (length triangles2))
	(if (inside-trianglep (elt points1 i) (elt triangles2 j))
	    (progn
	      (return-from collision-test t))))))
  nil)

(defmethod floor-collision-test ((gobj1 genobj))
  (let ((points1 (objpoints gobj1)))
    (dotimes (i (length points1))
      (if (<= (elt (elt points1 i) 1) 1)
	  (return-from floor-collision-test t))))
  nil)
  
(defmethod damage-obj ((gobj genobj) damage)
  (setf (objarea gobj) (- (objarea gobj) damage)))

(defun remove-damaged-objs ()
  (let ((objects-to-remove nil))
    (dotimes (i (length *object-list*))
      (if (<= (objarea (elt *object-list* i)) 0)
	  (setf objects-to-remove (append objects-to-remove (list i)))))
    (setf *object-list* (remove-ns-from-list *object-list* objects-to-remove))))

(defun collision-detection ()
  (let ((missile-collisions nil) (meteor-collisions nil) (moon-base-collisions nil))
    (dotimes (i (length *object-list*))
      (if (<= (distance (coords (elt *object-list* i)) (list (/ *width* 2) 25)) 50)
	  (if (collision-test *moon-base* (elt *object-list* i))
	      (setf moon-base-collisions (append moon-base-collisions (list i)))))
      (if (<= (ycoord (elt *object-list* i)) 50)
	  (if (floor-collision-test (elt *object-list* i))
	      (setf moon-base-collisions (append moon-base-collisions (list i)))))
      (dotimes (j (length *missile-list*))
	(if (<= (distance (coords (elt (elt *missile-list* j) 1)) (coords (elt *object-list* i))) 50)
	    (if (collision-test (elt (elt *missile-list* j) 1) (elt *object-list* i))
		(progn
		  (setf missile-collisions (append missile-collisions (list (elt (elt *missile-list* j) 0))))
		  (setf meteor-collisions (append meteor-collisions (list i))))))))
    (remove-duplicates moon-base-collisions :test #'=)
    (list meteor-collisions missile-collisions moon-base-collisions)))

(defun remove-n-from-list (listo n)
  (let ((temp-list nil))
    (dotimes (i (length listo))
      (if (not (= i n))
	  (setf temp-list (append temp-list (list (elt listo i))))))
    temp-list))

(defun remove-ns-from-list (listo nlist)
  (let ((temp-list listo) (temp-nlist (sort nlist '>)))
    (dotimes (j (length temp-nlist))
      (setf temp-list (remove-n-from-list temp-list (elt temp-nlist j))))
    temp-list))

(defun remove-missiles (remove-list)
  (dotimes (i (length remove-list))
    (setf *missile-list* (remove-if #'(lambda (x) (= (elt x 0) (elt remove-list i))) *missile-list*))))
    

(defun collision-physics ()
  (let ((collisions (collision-detection )))
    (dotimes (i (length (elt collisions 0)))
      (shrink-genobj (elt *object-list* (elt (elt collisions 0) i)) 0.3)
      (damage-obj (elt *object-list* (elt (elt collisions 0) i)) *missile-dmg*))
    (dotimes (j (length (elt collisions 2)))
      (setf *moon-base-hp* (- *moon-base-hp* (objarea (elt *object-list* (elt (elt collisions 2) j)))))
      (setf (objarea (elt *object-list* (elt (elt collisions 2) j))) -1))
    (remove-damaged-objs)
    (if (not (= (length (elt collisions 1)) 0))
	(remove-missiles (elt collisions 1)))))
	
(defun make-parabola (xy1 xy2 xy3 n start incr)
  (make-instance 'paraobj
		 :function (parabola xy1 xy2 xy3)
		 :point-list (parabola-points xy1 xy2 xy3 n start incr)
		 :defing-points (list xy1 xy2 xy3)))

(defparameter *the-parabola* (make-parabola (list 0 (/ *width* 2)) '(2 5) '(3 8) 10 (/ *width* 2) 10))

(defun move-missiles (n)
  (let ((remove-list nil))
    (dotimes (j (length *missile-list*))
      (let ((missile-path (objpath (elt (elt *missile-list* j) 1))) (current-position (coords (elt (elt *missile-list* j) 1))) (current-distance 0) (current-angle 0))
	(dotimes (i (length missile-path))
	  (setf current-distance (distance current-position (list (elt (elt missile-path i) 1) (elt (elt missile-path i) 0))))
	  (if (and (> (elt (elt missile-path i) 0) (elt current-position 1)) (>= current-distance n))
	      (if (> current-distance (+ n 1))
		  (progn
		    (setf current-angle (angle-between-2pts current-position (list (elt (elt missile-path i) 1) (elt (elt missile-path i) 0))))
		    (move-genobj-to (elt (elt *missile-list* j) 1) (+ (elt current-position 0) (* n (cos current-angle))) (+ (elt current-position 1) (* n (sin current-angle))))
		    (return nil))
		  (progn
		    (move-genobj-to (elt (elt *missile-list* j) 1) (elt (elt missile-path i) 1) (elt (elt missile-path i) 0))
		    (return nil))))
	  (if (> (ycoord (elt (elt *missile-list* j) 1)) (+ *height* 10))
	     (setf remove-list (append remove-list (list (elt (elt *missile-list* j) 0))))))))
    (remove-missiles remove-list))
  nil)

(defun move-gunfire ()
  (dotimes (i (length *gunfire-list*))
    (move-genobj-by (elt *gunfire-list* i) (xvel (elt *gunfire-list* i)) (yvel (elt *gunfire-list* i)))))

(defmethod change-point ((pobj paraobj) point x1 y1)
  (let ((new-def-points (para-def-points pobj)))
    (setf (elt new-def-points point) (list x1 y1))
    (setf (para-def-points pobj) new-def-points)
    (setf (para-points pobj) (parabola-points (elt new-def-points 0) (elt new-def-points 1) (elt new-def-points 2) 100 0 (+ (/ *height* 100) 1)))
    (setf (para-function pobj) (parabola (elt new-def-points 0) (elt new-def-points 1) (elt new-def-points 2))))
  nil)

(defmethod draw-paraobj ((pobj paraobj))
  (let ((points (para-points pobj)))
    (gl:color 1 0 0)
    (gl:begin :points)
    (dotimes (i (length points))
	 (gl:vertex (elt (elt points i) 1) (elt (elt points i) 0))))
    (gl:end))

(defun random-meteor-rain (size speed)
  (let ((start (list (random-between 0 *width*) (+ *height* 100))) (finish (list (random-between 0 *width*) 0)) (angle 0) (xvel1 0) (yvel1 0))
    (setf angle (angle-between-2pts start finish))
    (setf xvel1 (* (cos angle) speed))
    (setf yvel1 (* (sin angle) speed))
    (create-convex-meteor-obj (elt start 0) (elt start 1) xvel1 yvel1 (random-between -0.2 0.2) (random-between (/ size 2) (* size 2)) (random-between 5 10))))
 
(defun make-it-rain (n size speed)
  (dotimes (i n)
    (random-meteor-rain size speed)))
    				  
;;makes the window class	 
(defclass my-window (glut:window)
  ()
  (:default-initargs :width *width* :height *height* :pos-x 10 :pos-y 10
                     :mode '(:double :rgb :depth) :title "bourtange.lisp"))

(defmethod glut:display-window :before ((w my-window))
  (if (not *exiting*)
      (progn
	(glut:timer-func 30 (cffi:callback update) 0)
	(glut:timer-func *missile-refresh-rate* (cffi:callback reload) 0)
	(glut:timer-func *rain-frequency* (cffi:callback raining) 0)
	(glut:timer-func *gunfire-rate* (cffi:callback gunfiring) 0)
	(gl:clear-color 0 0 0 0)
	(gl:shade-model :smooth))))

(defmethod glut:display ((w my-window))
  (draw-object-list)
  (draw-missile-list)
  (draw-gunfire-list)
  (draw-paraobj *the-parabola*)
  (draw-missile-mag)
  (draw-genobj *moon-base2*)
  (setf (objcolor *moon-base*) (list 1 (/ *moon-base-hp* *moon-base-hp-max*) (/ *moon-base-hp* *moon-base-hp-max*)))
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
    (setf *exiting* T)
    (#\Esc (glut:destroy-current-window))))



(defmethod glut:special ((w my-window) key x y)
  (declare (ignore x y))
  (case key
    (:KEY-UP (move-genobj-by *moon-base* 0 2)
	     (glut:post-redisplay))))

(defun mouse-down (button x2 y2)
  (if (= button 0)
      (setf *gunfiring* t))
      ;(create-gunfire-obj x2 (- *height* y2) *gunfire-speed* 0 5))
      ;(create-convex-meteor-obj x2 (- *height* y2) (random-between -1 1) (random-between -3 -0.5) (random-between -0.3 0.3) (random-between 5 20) (+ 5 (random 5))))
  (if (and (= button 1) (> *missile-mag* 0))
      (progn
	(setf *missile-mag* (- *missile-mag* 1))
	(create-missile (para-points *the-parabola*)))))

(defun mouse-up (button x2 y2)
  (if (= button 0)
	(setf *gunfiring* nil)))

(defmethod glut:mouse ((w my-window) button state x1 y1)
  (case button
    (:left-button 
     (if (and (eq state :down) (not (eq state :up)))
	 (mouse-down 0 x1 y1))
     (if (eq state :up)
	 (mouse-up 0 x1 y1)))
    (:right-button
     (if (eq state :down)
	 (mouse-down 1 x1 y1)))))

(defmethod glut:passive-motion ((w my-window) x1 y1)
  (setf *gunfire-coords* (list x1 y1))
  (change-parabola x1 y1))

(defmethod glut:motion ((w my-window) x1 y1)
  (setf *gunfire-coords* (list x1 y1))
  (change-parabola x1 y1))

(defun change-parabola (x1 y1)
  (change-point *the-parabola* 2 (- *height* y1) x1)
  (change-point *the-parabola* 1 (- *height* y1 25) (- (/ *width* 2) (/ (- x1 (/ *width* 2)) 25))))
     	  
(defun rb-smooth ()
  (setf *exiting* nil)
  (defparameter *the-parabola* (make-parabola (list 0 (/ *width* 2)) '(2 5) '(3 8) 10 (/ *width* 2) 10))
  (defparameter *object-list* (list ))
  (defparameter *missile-list* nil)
  (defparameter *gunfire-list* nil)
  (defparameter *missile-count* 0)
  (defparameter *missile-mag* 10)
  (defparameter *moon-base-hp* 5000)
  (defparameter *missile-speed* 30)
  (defparameter *gunfire-speed* 50)
  (defparameter *gunfire-rate* 100)
  (defparameter *gunfiring* nil)
  (defparameter *gunfire-coords* (list 0 0))
  (glut:display-window (make-instance 'my-window)))

(cffi:defcallback update :void ((value :int))
  (declare (ignore value))
  (if (<= *moon-base-hp* 0)
      (end-game ))
  (if (and (not *exiting*) (= 1 (glut:get-window)))
      (progn 
	(collision-physics )
	(physics )
	(move-missiles *missile-speed*)
	(move-gunfire)
	(glut:swap-buffers )
	(gl:clear :color-buffer)
	(glut:post-redisplay )
	(glut:timer-func 30 (cffi:callback update) 0))))

(cffi:defcallback reload :void ((value :int))
  (declare (ignore value))
  (if (and (not *exiting*) (= 1 (glut:get-window)))
      (progn
	(if (< *missile-mag* *missile-max*)
	    (setf *missile-mag* (+ *missile-mag* 1)))
	(glut:timer-func *missile-refresh-rate* (cffi:callback reload) 0))))

(cffi:defcallback raining :void ((value :int))
  (declare (ignore value))
  (if (and (not *exiting*) (= 1 (glut:get-window)))
      (progn
	(make-it-rain *rain-number* *rain-size* *rain-speed*)
	(glut:timer-func *rain-frequency* (cffi:callback raining) 0))))

(cffi:defcallback gunfiring :void ((value :int))
  (declare (ignore value))
  (if (and (not *exiting*) (=  1 (glut:get-window)))
      (progn
	(if *gunfiring*
	    (create-gunfire-obj (elt *gunfire-coords* 0) (- *height* (elt *gunfire-coords* 1)) *gunfire-speed* 0 6))
	(glut:timer-func *gunfire-rate* (cffi:callback gunfiring) 0))))

  

  
     
  
