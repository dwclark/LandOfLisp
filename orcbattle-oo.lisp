(defgeneric dead? (x))
(defgeneric show (x))
(defgeneric attack (first second))

(defclass player ()
  ((health :initform 30 :accessor health)
   (agility :initform 30 :accessor agility)
   (strength :initform 30 :accessor strength)))

(defmethod dead? ((p player))
  (<= (health p) 0))

(defmethod show ((p player))
  (format t "~&You are a valiant knight with a health of ~d, an agility of ~d, and a strength of ~d"
	  (health p) (agility p) (strength p)))

(defun max-attribute (p)
  (let ((vec (vector 'health 'agility 'strength)))
    (sort vec #'< :key (lambda (a)
			 (slot-value p a)))
    (elt vec 2)))

(defmethod attack ((p player) monsters)
  (format t "~&Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  ;;SBCL buffers output, which is quite stupid for stdout in my opinion
  ;;the next call flushes output before waiting on reading
  (finish-output *standard-output*)
  (case (read)
    (s (hit (pick monsters)
	    (+ 2 (randval (ash (strength p) -1)))))
    (d (let ((x (randval (truncate (/ (strength p) 6)))))
	 (format t "Your double swing has a strength of ~d~&" x)
	 (hit (pick monsters) x)
	 (unless (dead? monsters)
	   (hit (pick monsters) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ (strength p) 3)))))
		 (unless (dead? monsters)
		   (hit (random-monster monsters) 1))))))

(defclass monster () 
  ((health :initform (randval 10) :accessor health)))
(defgeneric hit (monster x))

(defmethod hit ((m monster) x)
  (decf (health m) x)
  (if (dead? m)
      (format t "You killed the ~A!" (type-of m))
      (format t "You hit the ~A, knocking off ~d health points!" (type-of m) x)))

(defmethod show ((m monster))
  (format t "A fierce ~A" (type-of m)))

(defmethod dead? ((m monster))
  (<= (health m) 0))

(defclass hydra (monster) ())

(defmethod show ((m hydra))
  (format t "A malicious hydra with ~d heads." (health m)))

(defmethod hit ((m hydra) x)
  (decf (health m) x)
  (if (dead? m)
      (princ "The corpse of a fully decapitated and decapacitated hydra falls to the floor!")
      (format t "You lop off ~d of the hydra's heads! " x)))

(defmethod attack ((m hydra) (p player))
  (let ((x (randval (ash (health m) -1))))
    (format t "A hydra attacks you with ~d of its heads! It also grows back one more head! " x)
    (incf (health m))
    (decf (health p) x)))

(defclass brigand (monster) ())

(defmethod attack ((m brigand) (p player))
  (let ((a (max-attribute p)))
    (format t "A brigand hits you with his weapon, taking off 2 ~(~a~) points. "
	    (prin1-to-string a))
    (decf (slot-value p a) 2))) 

(defclass orc (monster)
  ((club-level :initform (randval 8) :accessor club-level)))

(defmethod show ((m orc))
  (format t "A wicked orc with a a level ~A club" (club-level m)))

(defmethod attack ((m orc) (p player))
  (let ((x (randval (club-level m))))
    (format t "An orc swings his club at you and knocks off ~d of your health points. " x)
    (decf (health p) x)))

(defclass slime-mold (monster)
  ((sliminess :initform (randval 5) :accessor sliminess)))

(defmethod show ((m slime-mold))
  (format t "A slime mold with a sliminess of ~d" (sliminess m)))

(defmethod attack ((m slime-mold) (p player))
  (let ((x (randval (sliminess m))))
    (format t "A slime mold wraps around your legs and decreases your agility by ~d! " x)
    (decf (agility p) x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf (health p)))))

(defmethod dead? ((monsters list))
  (every #'dead? monsters))

(defmethod show (monsters)
  (format t "~&Your foes:")
  (let ((x 0))
    (map 'list 
	 (lambda (m)
	   (format t "~&    ~d." (incf x))
	   (if (dead? m)
	       (princ " **dead**")
	       (progn (format t " (Health=~d) " (health m))
		      (show m))))
	 monsters)))
  
(defclass orc-battle-game ()
  ((monsters :initform (init-monsters) :accessor monsters)
   (the-player :initform (make-instance 'player) :accessor the-player)))

(defun init-monsters ()
  (let* ((num-monsters 12)
	 (all-types (vector 'orc 'hydra 'slime-mold 'brigand))
	 (ret-vec (make-array num-monsters)))
    (dotimes (i num-monsters)
      (setf (aref ret-vec i) (make-instance (elt all-types (random (length all-types))))))
    ret-vec))

(defun orc-battle ()
  (with-accessors ((monsters monsters) (the-player the-player)) (make-instance 'orc-battle-game)
    (game-loop the-player monsters)
    (when (dead? the-player)
      (format t "You have been killed. Game Over."))
    (when (dead? monsters)
      (format t "Congratulations!  You have vanquished all of your foes."))))

(defun game-loop (p monsters)
  (unless (or (dead? p) (dead? monsters))
    (show p)
    (dotimes (k (1+ (truncate (/ (max 0 (agility p)) 15))))
      (unless (dead? monsters)
	(show monsters)
	(attack p monsters)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (dead? m) (attack m p)))
	 monsters)
    (game-loop p monsters)))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster (monsters)
  (let ((not-dead (remove-if #'dead? monsters)))
    (elt not-dead (random (length not-dead)))))

(defun pick (monsters)
  (format t "~&Monster #: ")
  ;;SBCL buffers output, which is quite stupid for stdout in my opinion
  ;;the next call flushes output before waiting on reading
  (finish-output *standard-output*)
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x (length monsters))))
	(progn (princ "That is not a valid monster number.")
	       (pick monsters))
	(let ((m (aref monsters (1- x))))
	  (if (dead? m)
	      (progn
		(princ "That monster is alread dead")
		(pick monsters))
	      m)))))
