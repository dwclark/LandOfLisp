(defpackage :lazy
  (:use :common-lisp)
  (:shadow :cons :car :cdr :list :mapcar :mapcan :nth :find-if :null :nil)
  (:export :make :force :cons :car :cdr :list :from-list
	   :take :take-all :mapcar :mapcan :nth :find-if :null :nil))

(in-package :lazy)

(defmacro make (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced cl:nil)
	   (,value cl:nil))
       (lambda ()
	 (if ,forced
	     ,value
	     (progn
	       (setf ,forced t)
	       (setf ,value (progn ,@body))))))))

(defun force (lazified)
  (funcall lazified))

(defun assert-true (v)
  (if (not (eq t v))
      (error "assert-true failed")))

(defun test-force ()
  (let ((lazified (make (1+ 1))))
    (assert-true (= 2 (force lazified)))))

(defun nil ()
  (make cl:nil))

(defun null (val)
  (eq cl:nil (force val)))

(defmacro cons (f s)
  `(make (cl:cons ,f ,s)))

(defun car (lazy-cons)
  (cl:car (force lazy-cons)))

(defun cdr (lazy-cons)
  (cl:cdr (force lazy-cons)))

(defun test-cons ()
  (assert-true (= 2 (car (cons 2 3))))
  (assert-true (= 3 (cdr (cons 2 3)))))

(defun from-list (lst)
  (make (when lst
	  (cl:cons (cl:car lst) (from-list (cl:cdr lst))))))

(defun list (&rest args) (from-list args))

(defun take (n lst)
  (unless (or (zerop n) (null lst))
    (cl:cons (car lst) (take (1- n) (cdr lst)))))

(defun take-all (lst)
  (unless (null lst)
    (cl:cons (car lst) (take-all (cdr lst)))))

(defun test-take ()
  (assert-true (equal (cl:list 1 2 3)
		      (take 3 (list 1 2 3 4 5))))
  (assert-true (equal (cl:list 1 2 3 4 5)
		      (take 5 (list 1 2 3 4 5))))
  (assert-true (equal (cl:list 1 2 3 4)
		      (take-all (list 1 2 3 4))))
  (assert-true (eq cl:nil
		   (take 10 (nil))))
  (assert-true (eq cl:nil
		   (take-all (nil)))))

(defun mapcar (func lst)
  (make (unless (null lst)
	  (cl:cons (funcall func (car lst))
		   (mapcar func (cdr lst))))))

(defun test-mapcar ()
  (assert-true (equal (cl:list 1 2 3 4 5)
		      (take-all (mapcar (lambda (x) (1+ x)) (list 0 1 2 3 4))))))

(defun mapcan (func lst)
  (labels ((inner-func (current)
 	     (if (null current)
 		 (force (mapcan func (cdr lst)))
 		 (cl:cons (car current) (make (inner-func (cdr current)))))))
    (make (unless (null lst)
	    (inner-func (funcall func (car lst)))))))

(defun test-mapcan ()
  (assert-true (equal (cl:list 1 1 1 2 1 3 1 4)
		      (take-all (mapcan (lambda (x) (list 1 x)) (list 1 2 3 4))))))

(defun nth (n lst)
  (if (zerop n)
      (car lst)
      (nth (1- n) (cdr lst))))

(defun test-nth ()
  (assert-true (= 4 (nth 4 (list 0 1 2 3 4 5)))))

(defun find-if (func lst)
  (if (not (null lst))
      (let ((val (car lst)))
	(if (funcall func val)
	    val
	    (find-if func (cdr lst))))))

(defun test-find-if ()
  (assert-true (= 3 (find-if (lambda (x) (= x 3)) (list 1 2 3 4 5 6))))
  (assert-true (cl:null (find-if (lambda (x) (= x 99)) (list 1 2 3 89))))
  (assert-true (= 65 (find-if (lambda (x) (> x 64)) (list 63 64 65 66)))))

(defun find-all (func lst)
  (labels ((found-func (val)
	     (if val
		 (cl:cons (car lst) (find-all func (cdr lst)))
		 (force (find-all func (cdr lst))))))
    (make (unless (null lst)
	    (found-func (funcall func (car lst)))))))

(defun test-find-all ()
  (assert-true (equal (cl:list 1 3 5 7)
		      (take 4 (find-all #'oddp (list 1 2 3 4 5 6 7 8 9 10)))))
  (assert-true (equal (cl:list 1 3 5 7 9)
		      (take 10 (find-all #'oddp (list 1 2 3 4 5 6 7 8 9 10)))))
  (assert-true (equal (cl:list 1 3 5 7 9)
		      (take-all (find-all #'oddp (list 1 2 3 4 5 6 7 8 9 10))))))

(defun test-all ()
  (test-force)
  (test-cons)
  (test-take)
  (test-mapcar)
  (test-nth)
  (test-find-if)
  (test-mapcan))
