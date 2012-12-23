(defpackage :lazy
  (:use :common-lisp)
  (:shadow :cons :car :cdr :nil :null :list :mapcar :mapcan :find-if :nth)
  (:export :lazify :force :cons :car :cdr :nil :null :from-list
	   :take :take-all :list :mapcar :mapcan :find-if :nth))

(in-package :lazy)

(defmacro lazify (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced cl:nil)
	   (,value cl:nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t))
	 ,value))))

(defun force (value)
  (funcall value))

(defmacro cons (f s)
  `(lazify (cl:cons ,f ,s)))

(defun car (x)
  (cl:car (force x)))

(defun cdr (x)
  (cl:cdr (force x)))

(defun nil ()
  (lazify cl:nil))

(defun null (x)
  (not (force x)))

(defun from-list (lst)
  (lazify (when lst
	    (cl:cons (cl:car lst) (from-list (cl:cdr lst))))))

(defun list (&rest lst) (from-list lst))

(defun take (n lst)
  (unless (or (zerop n) (null lst))
    (cl:cons (car lst) (take (1- n) (cdr lst)))))

(defun take-all (lst)
  (unless (null lst)
    (cl:cons (car lst) (take-all (cdr lst)))))

(defun mapcar (fun lst)
  (lazify (unless (null lst)
	    (cl:cons (funcall fun (car lst))
		     (mapcar fun (cdr lst))))))

(defun mapcan (fun lst)
  (labels ((f (lst-cur)
	     (if (null lst-cur)
		 (force (mapcan fun (cdr lst)))
		 (cl:cons (car lst-cur) (lazify (f (cdr lst-cur)))))))
    (lazify (unless (null lst)
	      (f (funcall fun (car lst)))))))

(defun find-if (fun lst)
  (unless (null lst)
    (let ((x (car lst)))
      (if (funcall fun x)
	  x
	  (find-if fun (cdr lst))))))

(defun nth (n lst)
  (if (zerop n)
      (car lst)
      (nth (1- n) (cdr lst))))
