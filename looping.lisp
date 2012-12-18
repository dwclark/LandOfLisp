(loop for i in '(1 2 3 4 5 3 2 1)
     minimize i)

(loop for i below 5
     append (list i))

(loop for i below 10
     sum i)

(loop 
   with my-list = (list (cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 2) (cons 'e 7))
   with biggest = (loop for cell in my-list maximize (cdr cell))
   for cell in my-list
   when (= biggest (cdr cell))
   return cell)
     
(loop for cell in (list (cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 2) (cons 'e 7))
   maximize (cdr cell))

(loop for i from 0 to 10
     collect i)

(loop for i from 0 to 10
     count (evenp i))

(defun make-hashes ()
  (let ((hm (make-hash-table :test #'equal)))
    (setf (gethash "one" hm) 1)
    (setf (gethash "two" hm) 2)
    (setf (gethash "three" hm) 3)
    hm))

(loop for key being the hash-keys in (make-hashes) using (hash-value value)
   do (format t "~A: ~A~&" key value))
