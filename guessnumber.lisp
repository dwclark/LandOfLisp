;;Main things learned:
;;Use of ash function for binary searches
;;Using idempotent functions instead of local variables

(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (setf *small* 1)
  (setf *big* 100)
  (guess-my-number))


