(defclass lazy-value ()
  ((initialized :initform nil :accessor initialized)
   (value :initform nil :accessor value)))

(defmacro access-lazy-value (val &body body)
  `(if (initialized ,val)
       (value ,val)
       (progn
	 (setf (initialized ,val) t)
	 (setf (value ,val) (progn ,@body)))))

(defclass game-move ()
  ((source :initform nil :initarg :source :reader source)
   (destination :initform nil :initarg :destination :reader destination)
   (tree :initarg :tree :reader tree)))

(defgeneric attacking? (move))
(defmethod attacking? ((move game-move))
  (not (null (source move))))

(defclass game-tree ()
  ((board :initform nil :initarg :board :reader board)
   (current-player :initform nil :initarg :current-player :reader current-player)
   (spare-dice :initform nil :initarg :spare-dice :reader spare-dice)
   (first-move :initform nil :initarg :first-move :reader first-move)
   (moves :initform (make-instance 'lazy-value))
   (score-func :initform #'score-by-winner :initarg :score-func :reader score-func)
   (ai-level :initform -1 :initarg :ai-level :reader ai-level)))

(defgeneric make-tree (parent the-board the-player the-spare-dice the-first-move))
(defmethod make-tree ((parent game-tree) the-board the-player the-spare-dice the-first-move)
  (make-instance (type-of parent) 
		 :board the-board :current-player the-player
		 :spare-dice the-spare-dice :first-move the-first-move
		 :ai-level (ai-level parent) :score-func (score-func parent)))

(defmethod print-object ((the-tree game-tree) stream)
  (print-unreadable-object (the-tree stream :type t)
    (format stream "~a ~a ~a ~a" 
	    (ary (board the-tree)) (current-player the-tree) 
	    (spare-dice the-tree) (first-move the-tree))))

(defgeneric moves (tree))
(defmethod moves ((tree game-tree))
  (with-slots (moves) tree
    (access-lazy-value moves
      (let ((ret-list nil))
	(with-accessors ((board board) (current-player current-player)) tree
	  (loop for src-idx from 0 below (hexnum board)
	     do (let ((src-cell (aref (ary board) src-idx)))
		  (if (eq (player src-cell) current-player)
		      (loop for dst-idx in (neighbors board src-idx)
			 do (let ((dst-cell (aref (ary board) dst-idx)))
			      (if (and (not (eq (player dst-cell) current-player))
				       (> (dice src-cell) (dice dst-cell)))
				  (push
				   (make-instance 'game-move :source src-idx :destination dst-idx
						  :tree (make-tree tree (attack board src-idx dst-idx)
								   current-player (+ (spare-dice tree) (dice dst-cell)) nil))
				   ret-list))))))))
	(if (not (first-move tree))
	    (push
	     (make-instance 'game-move :tree (make-tree tree (add-new-dice (board tree) (current-player tree) (1- (spare-dice tree)))
							(mod (1+ (current-player tree)) (num-players (board tree))) 0 t))
	     ret-list))
	ret-list))))

(defun score-by-winner (board player)
  (let ((w (winners board)))
    (if (member player w)
	(/ 1 (length w))
	0)))

(defun score-by-threat (board player)
  (loop for hex across (ary board)
       for pos from 0
       sum (if (eq (player hex) player)
	       (if (threatened pos board)
		   1
		   2)
	       -1)))

(defun threatened (pos board)
  (with-accessors ((ary ary)) board
    (let ((cell (aref ary pos)))
      (loop for n in (neighbors board pos)
	 do (let ((ncell (aref ary n)))
	      (when (and (not (eq (player cell) (player ncell)))
			 (> (dice ncell) (dice cell)))
		(return t)))))))

(defgeneric rate-position (tree player &key search-depth))
(defmethod rate-position ((tree game-tree) player &key search-depth)
  (if (and (can-move? tree) (not (zerop search-depth)))
      (apply (if (eq (current-player tree) player)
		 #'max
		 #'min)
	     (get-ratings tree player :search-depth search-depth))
      (funcall (score-func tree) (board tree) player)))

(defgeneric get-ratings (tree player &key search-depth))
(defmethod get-ratings ((tree game-tree) player &key search-depth)
  (mapcar (lambda (move)
	    (rate-position (tree move) player :search-depth (1- search-depth)))
	  (moves tree)))

(defun test-moves ()
  (let* ((the-board (make-instance 'board-2 :ary #((1 1) (1 2) (1 1) (0 3))))
	 (the-game (make-instance 'game-tree :board the-board 
				  :current-player 0 :spare-dice 0 :first-move t))
	 (the-game-2 (make-instance 'game-tree :board the-board 
				    :current-player 0 :spare-dice 0 :first-move nil)))
    (assert (= 3 (length (moves the-game))))
    (assert (= 4 (length (moves the-game-2))))))

(defgeneric can-move? (tree))
(defmethod can-move? ((tree game-tree))
  (not (null (moves tree))))

(defgeneric play-vs-human (tree))
(defmethod play-vs-human ((tree game-tree))
  (print-info tree)
  (if (can-move? tree)
      (play-vs-human (handle-human tree))
      (announce-winner (board tree))))

(defgeneric handle-computer (tree))
(defmethod handle-computer ((tree game-tree))
  (let ((ratings (get-ratings tree (current-player tree) :search-depth (ai-level tree))))
    (tree (nth (position (apply #'max ratings) ratings) (moves tree)))))

(defgeneric play-vs-computer (tree))
(defmethod play-vs-computer ((tree game-tree))
  (print-info tree)
  (cond ((not (can-move? tree)) (announce-winner (board tree)))
	((zerop (current-player tree)) (play-vs-computer (handle-human tree)))
	(t (play-vs-computer (handle-computer tree)))))

(defgeneric print-info (tree))
(defmethod print-info ((tree game-tree))
  (fresh-line)
  (format t "Current player = ~a" (letter (current-player tree)))
  (draw (board tree)))

(defgeneric handle-human (tree))
(defmethod handle-human ((tree game-tree))
  (fresh-line)
  (princ "Choose your move:")
  (loop for move in (moves tree)
     for n from 1
     do (if (attacking? move) 
	    (format t "~&~a. ~a -> ~a" n (source move) (destination move))
	    (format t "~&~a. end move" n)))
  (fresh-line)
  
  (let ((selection (read)))
    (tree (nth (1- selection) (moves tree)))))

(defclass memoizing-game-tree (game-tree)
  ((tree-memoizer :initform nil :reader tree-memoizer)
   (rate-position-memoizer :initform nil :reader rate-position-memoizer)))

(defmethod initialize-instance :after ((tree memoizing-game-tree) &key init-caches)
  (if init-caches
      (progn
	(setf (slot-value tree 'tree-memoizer) (make-hash-table :test #'equalp))
	(setf (slot-value tree 'rate-position-memoizer) (make-hash-table :test #'equalp)))))

(defmacro memoized (cache state &body body)
  (let ((tmp-state (gensym))
	(contents (gensym))
	(is-there? (gensym)))
    `(let ((,tmp-state ,state))
       (multiple-value-bind (,contents ,is-there?) (gethash ,tmp-state ,cache)
	 (if ,is-there?
	     ,contents
	     (setf (gethash ,tmp-state ,cache) (progn ,@body)))))))

(defmethod make-tree ((parent memoizing-game-tree) the-board the-player the-spare-dice the-first-move)
  (memoized (tree-memoizer parent) (list (ary the-board) the-player the-spare-dice the-first-move)
    (let ((new-tree (call-next-method)))
      (setf (slot-value new-tree 'tree-memoizer) (tree-memoizer parent))
      (setf (slot-value new-tree 'rate-position-memoizer) (rate-position-memoizer parent))
      new-tree)))

(defmethod rate-position ((tree memoizing-game-tree) player &key search-depth)
  (let ((player-cache (memoized (rate-position-memoizer tree) player (make-hash-table))))
    (memoized player-cache tree (call-next-method))))

(defun dump-cache (the-cache)
  (loop for k being the hash-keys in the-cache using (hash-value v)
       do (format t "~&~a ~a" k v)))

(defclass board ()
  ((num-players :initarg :num-players :reader num-players)
   (max-dice :initarg :max-dice :reader max-dice)
   (side-length :initarg :side-length :reader side-length)
   (hexnum :reader hexnum)
   (ary :initarg :ary :initform nil :reader ary)))

(defmethod initialize-instance :after ((b board) &key)
  (setf (slot-value b 'hexnum) (* (side-length b) (side-length b)))
  (if (null (ary b))
      (with-accessors ((players num-players) (dice max-dice) (hexnum hexnum)) b
	(setf (slot-value b 'ary) 
	      (loop with the-ary = (make-array (list hexnum)) 
		 for i from 0 below hexnum
		 do (progn
		      (setf (aref the-ary i) 
			    (list (random players) (1+ (random dice)))))
		 finally (return the-ary))))))

(defgeneric winners (the-board))
(defmethod winners ((the-board board))
  (let* ((players-all (loop for cell across (ary the-board) collect (player cell)))
	 (players (remove-duplicates players-all))
	 (player-counts (loop for player in players collect (cons player (count player players-all))))
	 (sorted-counts (sort player-counts #'> :key (lambda (x) (cdr x))))
	 (max-count (cdr (first sorted-counts)))
	 (all-max (remove-if-not (lambda (x) (= max-count (cdr x))) sorted-counts)))
    (mapcar (lambda (x) (car x)) all-max)))

(defun test-winners ()
  (let* ((the-board-1 (make-instance 'board-2 :ary #((1 0) (1 0) (1 0) (1 0))))
	 (w-1 (winners the-board-1))
	 (the-board-2 (make-instance 'board-2 :ary #((1 0) (0 0) (0 0) (1 0))))
	 (w-2 (winners the-board-2))
	 (the-board-3 (make-instance 'board-2 :ary #((0 1) (0 1) (1 1) (0 1))))
	 (w-3 (winners the-board-3)))
    (assert (= 1 (length w-1)))
    (assert (= 1 (first w-1)))
    (assert (= 2 (length w-2)))
    (assert (= 0 (first w-3)))))

(defgeneric announce-winner (board))
(defmethod announce-winner ((the-board board))
  (fresh-line)
  (let ((w (winners the-board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'letter w))
	(format t "The winner is ~a" (letter (first w))))))

(defclass board-2 (board)
  ((num-players :initform 2)
   (max-dice :initform 3)
   (side-length :initform 2)))

(defclass board-3 (board-2)
  ((num-players :initform 2)
   (max-dice :initform 3)
   (side-length :initform 3)))

(defclass board-4 (board)
  ((num-players :initform 2)
   (max-dice :initform 3)
   (side-length :initform 4)))

(defclass board-5 (board)
  ((num-players :initform 2)
   (max-dice :initform 3)
   (side-length :initform 5)))

(defmethod print-object ((the-board board) stream)
  (print-unreadable-object (the-board stream :type t)
    (with-accessors ((ary ary)) the-board
      (format stream "~a" ary))))

(defgeneric index (b x y))
(defmethod index ((b board) x y)
  (+ x (* (side-length b) y)))

(defgeneric pos (b x y))
(defmethod pos ((b board) x y)
  (aref (ary b) (index b x y)))
    
(defgeneric clone (b))
(defmethod clone ((b board))
  (make-instance (type-of b)
		 :num-players (num-players b) :max-dice (max-dice b)
		 :side-length (side-length b) :ary (map 'vector #'copy-list (ary b))))

(defgeneric same (first second))
(defmethod same ((b1 board) (b2 board))
  (and (= (num-players b1) (num-players b2))
       (= (max-dice b1) (max-dice b2))
       (= (side-length b1) (side-length b2))
       (equalp (ary b1) (ary b2))))

(defun player (cell)
  (first cell))

(defun (setf player) (val cell)
  (setf (first cell) val))

(defun letter (p)
  (code-char (+ 97 p)))

(defun dice (cell)
  (second cell))

(defun (setf dice) (val cell)
  (setf (second cell) val))

(defgeneric draw (b))
(defmethod draw ((b board))
  (with-accessors ((side-length side-length)) b
    (loop for y below side-length
       do (progn (fresh-line)
		 (loop repeat (- side-length y)
		    do (princ "  "))
		 (loop for x below side-length
		    for cell = (pos b x y)
		    do (format t "~a-~a " (letter (player cell)) (dice cell)))))))

(defgeneric neighbors (b idx))
(defmethod neighbors ((b board) idx)
  (with-accessors ((side-length side-length)) b
    (let* ((hexnum (hexnum b))
	   (up (- idx side-length))
	   (down (+ idx side-length)))
      (loop for p in (append (list up down)
			     (unless (zerop (mod idx side-length))
			       (list (1- up) (1- idx)))
			     (unless (zerop (mod (1+ idx) side-length))
			       (list (1+ idx) (1+ down))))
	 when (and (>= p 0) (< p hexnum))
	 collect p))))

(defun test-neighbors ()
  (let ((b1 (make-instance 'board-2))
	(b2 (make-instance 'board-3)))
    (assert (equalp (list 0 3) (sort (neighbors b1 2) #'<)))
    (assert (equalp (list 1 2 3) (sort (neighbors b1 0) #'<)))
    (assert (equalp (list 0 1 3 5 7 8) (sort (neighbors b2 4) #'<)))))

(defgeneric attack (the-board src dst))
(defmethod attack ((the-board board) src dst)
  (let* ((ret-board (clone the-board))
	 (ary (ary ret-board))
	 (src-cell (aref ary src))
	 (new-dice (1- (dice src-cell)))
	 (dst-cell (aref ary dst)))
    (setf (dice src-cell) 1)
    (setf (player dst-cell) (player src-cell))
    (setf (dice dst-cell) new-dice)
    ret-board))

(defun test-attack ()
  (let* ((board (make-instance 'board-2 :ary #((0 3) (0 3) (1 3) (1 1))))
	 (next (attack board 1 3)))
    (assert (same next (make-instance 'board-2 :ary #((0 3) (0 1) (1 3) (0 2)))))))

(defgeneric add-new-dice (board player spare-dice))
(defmethod add-new-dice ((the-board board) the-player total-spare-dice)
  (let ((ret-board (clone the-board)))
    (loop with spare-dice = total-spare-dice
       for cell across (ary ret-board)
       do (if (and (not (zerop spare-dice))
		   (eq (player cell) the-player)
		   (< (dice cell) (max-dice the-board)))
	      (progn 
		(incf (dice cell))
		(decf spare-dice))))
    ret-board))
			  
(defun test-add-new-dice ()
  (let* ((initial (make-instance 'board-2 :ary #((0 1) (1 3) (0 2) (1 1))))
	 (next (add-new-dice initial 0 2))
	 (should-be (make-instance 'board-2 :ary #((0 2) (1 3) (0 3) (1 1)))))
    (assert (same should-be next))))

(defun test-all ()
  (test-attack)
  (test-add-new-dice)
  (test-winners)
  (test-neighbors)
  (test-moves))

(defclass ab-game-tree (memoizing-game-tree) ())

(defgeneric ratings-max (tree player upper lower search-depth))
(defmethod ratings-max ((tree ab-game-tree) player upper lower search-depth)
  (labels ((f (moves lower)
	     (unless (null moves)
	       (let ((x (rate-position (tree (first moves))
				       player :upper upper :lower lower
				       :search-depth (1- search-depth))))
		 (if (>= x upper)
		     (list x)
		     (cons x (f (cdr moves) (max x lower))))))))
    (f (moves tree) lower)))

(defgeneric ratings-min (tree player upper lower search-depth))
(defmethod ratings-min ((tree ab-game-tree) player upper lower search-depth)
  (labels ((f (moves upper)
	     (unless (null moves)
	       (let ((x (rate-position (tree (first moves))
				       player :upper upper :lower lower
				       :search-depth (1- search-depth))))
		 (if (<= x lower)
		     (list x)
		     (cons x (f (cdr moves) (min x upper))))))))
    (f (moves tree) upper)))

(defmethod rate-position ((tree ab-game-tree) player &key upper lower search-depth)
  (if (and (can-move? tree) (not (zerop search-depth)))
      (if (eq (current-player tree) player)
	  (apply #'max (ratings-max tree player upper lower search-depth))
	  (apply #'min (ratings-min tree player upper lower search-depth)))
      (funcall (score-func tree) (board tree) player)))

(defmethod handle-computer ((tree ab-game-tree))
  (let ((ratings (ratings-max tree (current-player tree) 
			      most-positive-fixnum most-negative-fixnum 
			      (ai-level tree))))
    (tree (nth (position (apply #'max ratings) ratings) (moves tree)))))

			 

;3 player
;(play-vs-computer (make-instance 'memoizing-game-tree :init-caches t :board (make-instance 'board-3) :current-player 0 :spare-dice 0 :first-move t))
;2 player
;(play-vs-computer (make-instance 'game-tree :board (make-instance 'board-2) :current-player 0 :spare-dice 0 :first-move t))
;4 player
;(play-vs-computer (make-instance 'memoizing-game-tree :ai-level 4 :init-caches t :board (make-instance 'board-4) :current-player 0 :spare-dice 0 :first-move t))
;4 player, better scoring
;(play-vs-computer (make-instance 'memoizing-game-tree :ai-level 4 :score-func #'score-by-threat :init-caches t :board (make-instance 'board-4) :current-player 0 :spare-dice 0 :first-move t))
;5 player, with ab pruning
;(play-vs-computer (make-instance 'ab-game-tree :ai-level 4 :score-func #'score-by-threat :init-caches t :board (make-instance 'board-5) :current-player 0 :spare-dice 0 :first-move t))
