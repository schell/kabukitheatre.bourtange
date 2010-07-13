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
    (setf b (/ (- y2 y3 (/ (* (- y1 y2) (- (sqr x2) (sqr x3))) (- (sqr x1) (sqr x2)))) (- (+ x2 (/ (* (- x2 x1) (-  (sqr x2) (sqr x3))) (- (sqr x1) (sqr x2)))) x3)))
    (setf a (/ (- y1 y2 (* b (- x1 x2))) (- (sqr x1) (sqr x2))))
    (setf c (- y1 (* a (sqr x1)) (* b x1)))
    (list a b c)))