(defclass lazy-value ()
  ((initialized :initform nil :accessor initialized)
   (value :initform nil :accessor value)))

(defmacro access-lazy-value (val &body body)
  `(if (initialized ,val)
       (value ,val)
       (progn
	 (setf (initialized ,val) t)
	 (setf (value ,val) (progn ,@body)))))

(defclass game-tree ()
  ((state :initarg :state :reader state)
   (move :initform nil :initarg :move :reader move)
   (ai-level :initform -1 :initarg :ai-level :reader ai-level)
   (moves :initform (make-instance 'lazy-value))))

(defgeneric make-tree (parent &key move state))
(defmethod make-tree ((parent game-tree) &key move state)
  (make-instance (type-of parent) :move move :state state :ai-level (ai-level parent)))

(defmethod print-object ((the-tree game-tree) stream)
  (print-unreadable-object (the-tree stream :type t)
    (format stream "~a ~a ~a ~a" 
	    (ary (board the-tree)) (current-player the-tree) 
	    (spare-dice the-tree) (first-move the-tree))))

(defgeneric source (tree))
(defmethod source ((tree game-tree))
  (first (move tree)))

(defgeneric destination (tree))
(defmethod destination ((tree game-tree))
  (second (move tree)))

(defun package-state (the-board the-player the-spare-dice the-first-move)
  (vector the-board the-player the-spare-dice the-first-move))

(defgeneric board (tree))
(defmethod board ((tree game-tree))
  (aref (state tree) 0))

(defgeneric current-player (tree))
(defmethod current-player ((tree game-tree))
  (aref (state tree) 1))

(defgeneric spare-dice (tree))
(defmethod spare-dice ((tree game-tree))
  (aref (state tree) 2))

(defgeneric first-move (tree))
(defmethod first-move ((tree game-tree))
  (aref (state tree) 3))
  
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
				  (push (make-tree tree 
						   :move (list src-idx dst-idx)
						   :state (package-state (attack board src-idx dst-idx)
									 current-player
									 (+ (spare-dice tree) (dice dst-cell))
									 nil))
					ret-list))))))))
	(if (not (first-move tree))
	    (push (make-tree tree 
			     :state (package-state (add-new-dice (board tree) (current-player tree) (1- (spare-dice tree)))
						   (mod (1+ (current-player tree)) (num-players (board tree)))
						   0 t))
		  ret-list))
	ret-list))))

(defgeneric passing? (tree))
(defmethod passing? ((tree game-tree))
  (null (move tree)))

(defgeneric attacking? (tree))
(defmethod attacking? ((tree game-tree))
  (not (null (move tree))))

(defgeneric rate-position (tree player search-depth))
(defmethod rate-position ((tree game-tree) player search-depth)
  (if (and (can-move? tree) (not (zerop search-depth)))
      (apply (if (eq (current-player tree) player)
		 #'max
		 #'min)
	     (get-ratings tree player search-depth))
      (let ((w (winners (board tree))))
	(if (member player w)
	    (/ 1 (length w))
	    0))))

(defgeneric get-ratings (tree player search-depth))
(defmethod get-ratings ((tree game-tree) player search-depth)
  (mapcar (lambda (move)
	    (rate-position move player (1- search-depth)))
	  (moves tree)))

(defun test-moves ()
  (let* ((the-board (make-instance 'board-2 :ary #((1 1) (1 2) (1 1) (0 3))))
	 (the-game (make-instance 'game-tree :state (package-state the-board 0 0 t)))
	 (the-game-2 (make-instance 'game-tree :state (package-state the-board 0 0 nil))))
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
  (let ((ratings (get-ratings tree (current-player tree) (ai-level tree))))
    (nth (position (apply #'max ratings) ratings) (moves tree))))

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
    (nth (1- selection) (moves tree))))

(defclass memoizing-game-tree (game-tree)
  ((tree-memoizer :initform nil :reader tree-memoizer)
   (rate-position-memoizer :initform nil :reader rate-position-memoizer)))

(defmethod initialize-instance :after ((tree memoizing-game-tree) &key init-caches)
  (if init-caches
      (progn
	(setf (slot-value tree 'tree-memoizer) (make-hash-table :test #'equalp))
	(setf (slot-value tree 'rate-position-memoizer) (make-hash-table :test #'equalp)))))
      
(defmethod make-tree ((parent memoizing-game-tree) &key move state)
  (let ((new-tree (call-next-method parent :move move :state state)))
    (setf (slot-value new-tree 'tree-memoizer) (tree-memoizer parent))
    (setf (slot-value new-tree 'rate-position-memoizer) (rate-position-memoizer parent))
    new-tree))

(defmacro memoized (cache state &body body)
  (let ((tmp-state (gensym)))
    `(let ((,tmp-state ,state))
       (or (gethash ,tmp-state ,cache)
	   (setf (gethash ,tmp-state ,cache) (progn ,@body))))))

(defmethod moves ((tree memoizing-game-tree))
  (with-accessors ((cache tree-memoizer)) tree
    (memoized cache (state tree) (call-next-method))))

(defmethod rate-position ((tree memoizing-game-tree) player search-depth)
  (with-accessors ((cache rate-position-memoizer)) tree
    (memoized cache (cons player (state tree)) (call-next-method))))
	   
(defclass board ()
  ((info :initarg :info)
   (ary :initarg :ary :initform nil :reader ary)))

(defun package-info (num-players max-dice side-length)
  (vector num-players max-dice side-length))

(defgeneric num-players (the-board))
(defmethod num-players ((the-board board))
  (with-slots (info) the-board
    (aref info 0)))

(defgeneric max-dice (the-board))
(defmethod max-dice ((the-board board))
  (with-slots (info) the-board
    (aref info 1)))

(defgeneric side-length (the-board))
(defmethod side-length ((the-board board))
  (with-slots (info) the-board
    (aref info 2)))

(defgeneric hexnum (the-board))
(defmethod hexnum ((the-board board))
  (* (side-length the-board) (side-length the-board)))

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
  ((info :initform (package-info 2 3 2))))

(defclass board-3 (board)
  ((info :initform (package-info 2 3 3))))

(defclass board-4 (board)
  ((info :initform (package-info 2 3 4))))

(defmethod initialize-instance :after ((b board) &key)
  (if (null (ary b))
      (with-accessors ((num-players num-players) (max-dice max-dice)) b
	(let ((hexnum (hexnum b)))
	  (setf (slot-value b 'ary) 
		(loop with the-ary = (make-array (list hexnum)) 
		   for i from 0 below hexnum
		   do (progn
			(setf (aref the-ary i) 
			      (list (random num-players) (1+ (random max-dice)))))
		   finally (return the-ary)))))))

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
    
(defgeneric clone (the-board))
(defmethod clone ((the-board board))
  (make-instance (type-of the-board)
		 :info (slot-value the-board 'info)
		 :ary (map 'vector #'copy-list (ary the-board))))

(defgeneric same (first second))
(defmethod same ((first board) (second board))
  (and (eq (type-of first) (type-of second))
       (equalp (ary first) (ary second))))

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
	(b2 (make-instance 'board :info (package-info 2 3 3))))
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
