;;This is a bit more verbose than the original version, but it has
;;a few advantages.  First, multiple simulations can be run in parallel
;;because all state is encapsulated.  Second, it's easier to parametrize
;;separate simulations to test different conditions.  Third, it should
;;be easier to build in different animal and plant types
(defclass world ()
  ((width :initform 100 :reader width :initarg :width)
   (height :initform 30 :reader height :initarg :height)
   (jungle :initform '(45 10 10 10) :reader jungle :initarg :jungle)
   (animals :accessor animals :initarg :animals)
   (plants :accessor plants :initarg :plants :initform (make-hash-table :test #'equal))
   (num-days :accessor num-days :initform 0)))

(defmethod initialize-instance :after ((w world) &key)
  (setf (animals w) 
	(list (make-instance 'animal 
			     :pos-x (ash (width w) -1)
			     :pos-y (ash (height w) -1)
			     :energy 1000))))
(defgeneric draw (w))
(defgeneric update (w &key))
(defgeneric evolve (w))

(defmethod draw ((w world))
  (fresh-line)
  (with-accessors ((num-days num-days) (height height) (width width) 
		   (animals animals) (plants plants)) w
    (format t "World after ~d days" num-days)
    (loop for y below height
       do (progn (fresh-line)
		 (princ "|")
		 (loop for x below width
		    do (let ((location (cons x y)))
			 (princ (cond ((some (lambda (animal)
					       (equal location (world-position animal)))
					     animals) #\M)
				      ((gethash location plants) #\*)
				      (t #\space)))))
		 (princ "|")))))

(defmethod update ((w world) &key ((:iterations iter) 1) )
  (with-accessors ((num-days num-days) (animals animals)) w
    (loop for i from 0 below iter
	 do (progn
	      (incf num-days)
	      (setf animals (remove-if (lambda (animal)
					 (<= (energy animal) 0))
				       animals))
	      (mapc (lambda (animal)
		      (turn animal)
		      (move animal w)
		      (eat animal w)
		      (reproduce animal w))
		    animals)
	      (reproduce (make-instance 'plant) w)))))


(defmethod evolve ((w world))
  (draw w)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (update w :iterations x)
		   (update w))
	       (evolve w)))))
  w)

(defclass living ()
  ((energy :accessor energy :initarg :energy)
   (pos-x :accessor pos-x :initarg :pos-x)
   (pos-y :accessor pos-y :initarg :pos-y)))

(defgeneric world-position (thing))
(defgeneric reproduce (the-thing the-world))

(defmethod world-position ((thing living))
  (cons (pos-x thing) (pos-y thing)))

(defclass plant (living) ())

(defmethod initialize-instance :after ((p plant) &key)
  (setf (energy p) 80))

(defmethod reproduce ((p plant) (w world))
  (with-accessors ((plants plants) (jungle jungle) (width width) (height height)) w
    (labels ((random-plant (left top arg-width arg-height)
	       (make-instance 'plant
			      :pos-x (+ left (random arg-width))
			      :pos-y (+ top (random arg-height)))))
      (let ((jungle-plant (apply #'random-plant jungle))
	    (misc-plant (random-plant 0 0 width height)))
	(setf (gethash (world-position jungle-plant) plants) jungle-plant)
	(setf (gethash (world-position misc-plant) plants) misc-plant)))))

(defclass animal (living)
  ((dir :accessor dir :initarg :dir :initform 0)
   (r-energy :reader r-energy :initform 200 :initarg :r-energy)
   (genes :initarg :genes :accessor genes :initform (loop repeat 8 collecting (1+ (random 10))))))

(defgeneric clone (the-animal))
(defgeneric move (the-animal the-world))
(defgeneric turn (the-animal))
(defgeneric eat (the-animal the-world))
(defgeneric reproduce? (the-animal))
(defgeneric mutate (the-animal))

(defmethod clone ((a animal))
  (make-instance 'animal
		 :energy (energy a) :pos-x (pos-x a)
		 :pos-y (pos-y a) :dir (dir a) :r-energy (r-energy a)
		 :genes (copy-list (genes a))))

(defmethod mutate ((a animal))
  (with-accessors ((genes genes)) a
    (let ((mutation (random (length genes))))
      (setf (nth mutation genes)
	    (max 1 (+ (nth mutation genes) (random 3) -1)))))
  a)

(defmethod move ((a animal) (w world))
  (with-accessors ((x pos-x) (y pos-y) (dir dir) (e energy)) a
    (setf x (mod (+ x
		    (cond ((and (>= dir 2) (< dir 5)) 1)
			  ((or (= dir 1) (= dir 5)) 0)
			  (t -1)))
		 (width w)))
    (setf y (mod (+ y
		    (cond ((and (>= dir 0) (< dir 3)) -1)
			  ((and (>= dir 4) (< dir 7)) 1)
			  (t 0)))
		 (height w)))
    (decf e)))

(defmethod turn ((a animal))
  (with-accessors ((genes genes) (dir dir)) a
    (let ((x (random (apply #'+ genes))))
      (labels ((angle (arg-genes x)
		 (let ((xnu (- x (car arg-genes))))
		   (if (< xnu 0)
		       0
		       (1+ (angle (cdr arg-genes) xnu))))))
	(setf dir
	      (mod (+ dir (angle genes x))
		   8))))))

(defmethod eat ((a animal) (w world))
  (with-accessors ((plants plants)) w
    (let* ((pos (world-position a))
	   (p (gethash pos plants)))
      (when (not (null p))
	(incf (energy a) (energy p))
	(remhash pos plants)))))

(defmethod reproduce? ((a animal))
  (with-accessors ((energy energy) (r-energy r-energy)) a
    (> energy r-energy)))

(defmethod reproduce ((a animal) (w world))
  (when (reproduce? a)
    (with-accessors ((e energy)) a
      (setf e (ash e -1))
      (push (mutate (clone a)) (animals w)))))

(defun run-simulation ()
  (evolve (make-instance 'world)))
