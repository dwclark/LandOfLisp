(load "visualizegraphs.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *player-pos* -1)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *all-nodes*
  (loop for i from 1 to *node-num*
     collect i))
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;;makes an alist of edges
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))

;;finds all edges (as cons cells) that have node
;;in the car
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;;returns a list of nodes reachable from node
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;;Possibly confusing aspect of this function is that it is not trying
;;to find isolated nodes.  It is grouping together all nodes that are
;;reachable from a given node and _that_ is an island.  Then try
;;again with the leftovers.  Keep going until there are no leftovers.
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

;;Checks find-islands, ensures that each node is assigned
;;to one and only one island. (not in book)
(defun check-nodes-assigned-to-islands (islands)
  (let ((island-nodes (sort (apply #'append islands) #'<)))
    (if (not (equal island-nodes *all-nodes*))
	(error "Some nodes are not assigned or are assigned multiple times")
	t)))

;;Makes an edge pair between the first island and the second
;;island.  Repeats by cdr-ing down the list of islands so that
;;each island has at least one connection to another island
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

;;Appends the bridges between the islands to the edge list
;;that lists the bridges/roads on each island
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;;Returns an odd alist.  A sample entry in the alist will be
;;something like (1 (2) (4) (15)).  This is done so that extra
;;information (like cop location) can be added to sublists
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  ;;returns list of conses (edge-pairs) with all
			  ;;duplicates removed that are direct edges of node1
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  ;;all this next line really does is get back the list of nodes.
	  ;;i.e. a list of numbers from 1 -> *node-num* in this case
	  (remove-duplicates (mapcar #'car edge-list))))

;;filters edge-alist by adding cop locations
;;to the sub lists of the original alist.  This is 
;;done in a functional style, so a new alist is created
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))
		  
;;Makes city edges.  One possibly confusing point is
;;that add-cops annotates the edge list and then returns
;;the annotated edge-list, it creates no side effects
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		     collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 ;;create cop edges by filtering out a certain random number of edges
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;;Gives numerical list of all direct neighbors to a given node.
;;Note that all cop info is left out.
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))

;;Returns a list of lists.  Each sub list will have the
;;node number as the initial element, then it will have the conditions
;;at that node.  For example: (6 WUMPUS SIRENS!).  Empty
;;nodes will just have the node number in the list
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
			 collect (random-node))))
    (loop for n from 1 to *node-num*
	 collect (append (list n)
			 (cond ((eql n wumpus) '(wumpus))
			       ((within-two n wumpus edge-alist) '(blood!)))
			 (cond ((member n glow-worms) '(glow-worm))
			       ((some (lambda (worm)
					(within-one n worm edge-alist))
				      glow-worms)
				'(lights!)))
			 (when (some #'cdr (cdr (assoc n edge-alist)))
			   '(sirens!))))))

;;Find a node that has no associated information attached to it
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

;;TODO: Start here
(defun known-city-nodes ()
  ;;if it's a visited node then either add a '*' (if it's where we are at
  ;;now, or just return the node itself.  If we have not visited the
  ;;node, append a '?'.
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*))
		      n))
		(list node '?)))
	  ;;returns a list of all nodes which is the visited nodes,
	  ;;plus nodes that are connected to visited nodes.
	  (remove-duplicates
	   (append *visited-nodes*
		   ;;mapcan appends any list returned from lambda onto
		   ;;the list it will return
		   (mapcan (lambda (node)
			     ;;extracts the nodes visible from a given node
			     (mapcar #'car 
				     (cdr (assoc node *congestion-city-edges*))))
			   *visited-nodes*)))))

;;Map over each of the visited nodes.  For each of the visited nodes
;;splice a new list consisting of the node number as first element
;;and then each node reachable by that node as a single element list.
;;a sample return value from known-city-edges might be:
;;((29 (23) (28) (14) (15) (12)) (23 (29)))
;;Here, 23 was the first visited node, and it only has an edge to 29.
;;Then 29 was visited, which has edges to 23, 28, 14, 15, and 12
(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

;;used to figure out if we can go to the new position (it is one of the
;;edges reachable from current location).  If we an go there, use handle-new-place
;;to go there, otherwise display an error messagexs
(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
	(handle-new-place edge pos charging)
	(princ "That location does not exist"))))

;;Basically all state management is localized to this function
;;This function is what manages the game rules and calculates
;;if you win or not.
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over"))
	  ((member 'wumpus node)
	   (if charging
	       (princ "You found the wumpus!")
	       (princ "You ran into the wumpus")))
	  (charging (princ "You wasted your last bullet. Game Over"))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into the Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))
	  

;;faster connected functions from chapter 9
(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
	       (unless (gethash node visited)
		 (setf (gethash node visited) t)
		 (mapc (lambda (edge)
			 (traverse edge))
		       (gethash node edge-tab)))))
      (traverse node))
    visited))
