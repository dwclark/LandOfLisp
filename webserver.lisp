(require :sb-bsd-sockets)

(defmacro with-socket ((var open-form) &body body)
  `(let ((,var ,open-form))
     (unwind-protect (progn ,@body)
       (sb-bsd-sockets:socket-close ,var))))

(defclass single-threaded-server ()
  ((ip-address :initarg :ip-address :reader ip-address :initform #(127 0 0 1))
   (port :initarg :port :reader port :initform 8080)
   (backlog :initarg :backlog :reader backlog :initform 5)
   (service-func :initarg :service-func :reader service-func)))

(defgeneric run (server))

(defun log-it (message)
  (princ message)
  (fresh-line)
  (finish-output *standard-output*))

(defmethod run ((server single-threaded-server))
  (with-socket (s-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
    (with-accessors ((ip ip-address) (port port) (num backlog) (func service-func)) server
      (sb-bsd-sockets:socket-bind s-socket ip port)
      (sb-bsd-sockets:socket-listen s-socket num)
      (let ((keep-going t))
	(loop while keep-going
	     do
	     (with-socket (comm-sock (sb-bsd-sockets:socket-accept s-socket))
	       (let ((stream (sb-bsd-sockets:socket-make-stream comm-sock :input t :output t :buffering :none)))
		 (setf keep-going (funcall func stream)))))))))

(defclass http-request ()
  ((url :initarg :url :reader url)
   (path :initarg :path :reader path)
   (headers :initarg :headers :reader headers)
   (params :initarg :params :reader params)))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16 :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (first lst)
		 (#\% (cons (http-char (second lst) (third lst))
			    (f (nthcdr 3 lst))))
		 (#\+ (cons #\space (f (rest lst))))
		 (otherwise (cons (first lst) (f (rest lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun parse-headers (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (parse-headers stream)))))
	   
(defun parse-content-params (stream headers)
  (let ((length (rest (assoc 'content-length headers))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(defun http-request-parser (stream)
  (let* ((url (parse-url (read-line stream)))
	 (path (first url))
	 (headers (parse-headers stream))
	 (params (append (cdr url)
			 (parse-content-params stream headers))))
    (make-instance 'http-request :url url :path path
		   :headers headers :params params)))

(defun http-request-handler (stream)
  (let ((req (http-request-parser stream)))
    (log-it (first (url req)))
    (if (equal (first (url req)) "quit")
	nil
	(progn
	  (dispatch req stream)
	  t))))
			
(defun dispatch (req stream)
  (with-accessors ((path path) (headers headers) (params params)) req
    (if (equal path "greeting")
	(let ((name (assoc 'name params)))
	  (if (not name)
	      (princ "<html><form>What is your name?<input name='name'/></form></html>" stream)
	      (format stream "<html>Nice to meet you, ~a!</html>" (rest name))))
	(princ "Sorry...I don't know that page." stream))))

(defun dump-stream (stream)
  (let ((read (read-line stream)))
    (when read
      (dump-stream stream)))
  (princ "Sorry, I don't know that page" stream))
