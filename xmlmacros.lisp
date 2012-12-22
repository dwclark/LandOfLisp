(defun begin-tag (name lst)
  (format t "<~a~{ ~a=\"~a\"~}>" name lst))

(defun end-tag (name)
  (format t "</~a>" name))

(defmacro tag (name attrs &body body)
  `(progn 
     (begin-tag ',(string-downcase `,name) (list ,@(loop for obj in attrs
						    for i from 1
						    collect (if (oddp i) (string-downcase obj) obj))))
     ,@body
     (end-tag ',(string-downcase `,name))))

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   "xmlns:xlink" "http://www.w3.org/1999/xlink"
		   height ,height
		   width ,width)
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))

(defun svg-style (color)
  (format nil "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
	  (append color (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
		  cy (cdr center)
		  r radius
		  style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}" points)
		       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))

(defun make-random-walk ()
  (with-open-file (*standard-output* "randomwalk.svg"
				     :direction :output 
				     :if-exists :supersede)
    (svg 400 200
      (loop repeat 10
	 do (polygon (append (list 0 200)
			     (loop for x from 0
				for y in (random-walk 100 400)
				append (list x y))
			     (list 400 200))
		     (loop repeat 3
			collect (random 256)))))))
