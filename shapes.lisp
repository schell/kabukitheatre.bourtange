(defclass shape ()
	((type
		:initarg :type
		:initform "shape")
  	 (vertices
		:initarg :vertices
		:initform (list :x 0 :y 0))))
	
(slot-value 
 	(make-instance 'shape 
		:type "sometype" 
		:vertices (list :x 10 :y 10)) 
	'vertices)