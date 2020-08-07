;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch04p.lisp
;;;;
;;;;   Started:            Fri Sep 10 19:01:51 2004
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(defpackage ch04p (:use common-lisp)
	    (:shadow last nth remove-duplicates))

(in-package ch04p)

;;;
;;;    4.7.1
;;;
(defun replicate (expr n)
  "Return a list consisting of N copies of EXPR"
  (assert (typep n '(integer 0 *))
	  (n)
	  "N should be a non-negative integer.")
  (labels ((replicate-aux (n)
	     (cond ((zerop n) '())
		   (t (cons expr (replicate-aux (1- n)))) )))
    (replicate-aux n)))

(defun factorial (n)
  "Compute N!"
  (assert (typep n '(integer 0 *))
	  (n)
	  "N should be a non-negative integer.")
  (labels ((factorial-aux (n r)
	     (cond ((zerop n) r)
		   (t (factorial-aux (1- n) (* r n)))) ))
    (factorial-aux n 1)))







;;;
;;;    4.7.7
;;;
(defun nth (n l)
  "Return Nth element in list L."
  (assert (typep n '(integer 0 *))
	  (n)
	  "N should be a non-negative integer.")
  (assert (typep l 'list)
	  (l)
	  "~S should be a list."
	  l)
  (cond ((zerop n) (car l))
	(t (nth (1- n) (cdr l)))) )

;;;
;;;    4.7.8
;;;
(defun last (l &optional (n 1))
  "Return last CONS in list L."
  (assert (typep l 'list)
	  (l)
	  "~S should be a list."
	  l)
  (assert (typep n '(integer 0 *))
	  (n)
	  "N should be a non-negative integer.")
  (labels ((last-aux (l i)
	     (cond ((zerop i) l)
		   (t (last-aux (cdr l) (1- i)))) ))
    (last-aux l (max (- (length l) n) 0))))

;;;
;;;    4.7.9
;;;
(defun remove-duplicates (l)
  "Remove duplicate top-level elements from a list."
  (assert (typep l 'list)
	  (l)
	  "~S should be a list."
	  l)
  (labels ((remove-duplicates-aux (l)
	     (cond ((null l) '())
		   ((member (car l) (cdr l)) (remove-duplicates-aux (cdr l)))
		   (t (cons (car l) (remove-duplicates-aux (cdr l)))) )))
    (remove-duplicates-aux l)))

;;;
;;;    4.7.10
;;;    Vol. I notes pg. 380
(deftype transaction-type ()
  '(or real interest-rate))

(deftype interest-rate ()
  '(satisfies interest-rate-p))

;; (defun interest-rate-p (l)
;;   (and (listp l)
;;        (= (length l) 1)
;;        (numberp (car l))
;;        (plusp (car l))))

(defun interest-rate-p (obj)
  (typep obj '(cons (real 0) null)))

;;;
;;;    Negative balance compounded?
;;;    (check-book -20 '((1.1))) => -22.0
;;;    
(defun check-book (balance transaction-list)
  (assert (typep balance 'real)
          (balance)
          "BALANCE should be a number.")
  (assert (and (listp transaction-list)
               (every #'(lambda (x)
                      (typep x 'transaction-type)) transaction-list))
          (transaction-list)
          "TRANSACTION-LIST should be a list of valid transactions.")
  (labels ((check-book-aux (b l)
             (cond ((null l) b)
                   ((numberp (car l)) (check-book-aux (+ b (car l)) (cdr l)))
                   (t (check-book-aux (* b (caar l)) (cdr l)))) ))
    (check-book-aux balance transaction-list)))

;;;
;;;    4.7.11
;;;
(let ((minimum-balance 500)
      (penalty -0.10))
  (defun now-account (balance transaction-list)
    (assert (typep balance 'real)
	    (balance)
	    "BALANCE should be a number.")
    (assert (and (listp transaction-list)
		 (every #'(lambda (x)
			    (typep x 'transaction-type)) transaction-list))
	    (transaction-list)
	    "TRANSACTION-LIST should be a list of valid transactions.")
    (labels ((now-account-aux (b l)
	     (cond ((null l) b)
		   ((numberp (car l))
		    (if (and (< balance minimum-balance)
			     (minusp (car l)))
			(now-account-aux (+ b (car l) penalty) (cdr l))
			(now-account-aux (+ b (car l)) (cdr l))))
		   (t (if (< balance minimum-balance)
			  (now-account-aux b (cdr l))
			  (now-account-aux (* b (caar l)) (cdr l)))) )))
      (now-account-aux balance transaction-list)))) 

;;;
;;;    4.7.12
;;;    This fails the master test suite (2010).
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
          (pattern)
          "PATTERN should be a list.")
  (assert (listp input)
          (input)
          "INPUT should be a list.")
  (labels ((matchp-aux (p i)
             (cond ((null p) (null i))
                   ((null i) (equal p '(*wild*)))
                   ((eql (car p) (car i)) (matchp-aux (cdr p) (cdr i)))
                   ((eql (car p) '*wild*)
                    (or (matchp-aux (cdr p) i)
                        (matchp-aux p (cdr i))))
                   (t nil))))
    (matchp-aux pattern input)))

;;;
;;;    4.7.13
;;;
(defun count-occurrences (elt obj)
  "Determine the number times ELT occurs in OBJ."
  (assert (atom elt)
	  (elt)
	  "ELT should be an atom not: ~S"
	  elt)
  (labels ((count-occurrences-aux (obj)
	     (cond ((eql obj elt) 1)
		   ((atom obj) 0)
		   (t (+ (count-occurrences-aux (car obj))
			 (count-occurrences-aux (cdr obj)))) )))
    (count-occurrences-aux obj)))

;;;
;;;    4.7.14
;;;
(defun tree-addition (n obj)
  "Add a number N to each number in a tree."
  (assert (numberp n)
	  (n)
	  "N should be a number not: ~S"
	  n)
  (labels ((tree-addition-aux (obj)
	     (cond ((null obj) '())
		   ((numberp obj) (+ n obj))
		   ((atom obj) (error "Non-numeric atom: ~S" obj))
		   (t (cons (tree-addition-aux (car obj))
			    (tree-addition-aux (cdr obj)))) )))
    (tree-addition-aux obj)))

;;;
;;;    4.7.15
;;;
(defun tree-average (obj)
  (let ((sum 0)
	(count 0))
    (labels ((tree-average-aux (obj)
	       (cond ((null obj) t)
		     ((numberp obj) (incf sum obj) (incf count))
		     ((atom obj) (error "Non-numeric atom: ~S" obj))
		     (t (tree-average-aux (car obj))
			(tree-average-aux (cdr obj)))) ))
      (tree-average-aux obj)
      (if (zerop count)
	  0
	  (/ sum count)))) )

(defun tree-average (obj)
  (labels ((tree-average-aux (obj)
             (cond ((null obj) (list 0 0))
                   ((numberp obj) (list obj 1))
                   ((atom obj) (error "Non-numeric atom: ~S" obj))
                   (t (mapcar #'+
                              (tree-average-aux (car obj))
                              (tree-average-aux (cdr obj)))) ))
           (average (l)
             (if (zerop (cadr l))
                 0
                 (/ (car l) (cadr l)))) )
    (average (tree-average-aux obj))))

;;;
;;;    This solution uses nothing beyond what's covered up to ch. 4!
;;;    
(defun tree-average (obj)
    (average (tree-average-aux obj)))

(defun tree-average-aux (obj)
  "Accumulate a sum TOTAL and COUNT of all of the numbers in a tree as a list (TOTAL COUNT)."
  (cond ((null obj) (list 0 0))
	((numberp obj) (list obj 1))
	((atom obj) (error "Non-numeric atom: ~S" obj))
	(t (list-add (tree-average-aux (car obj))
		     (tree-average-aux (cdr obj)))) ))

(defun list-add (l1 l2)
  "Add the corresponding elements of two lists of two numbers."
  (list (+ (first l1) (first l2))
	(+ (second l1) (second l2))))

(defun average (l)
  "Compute the average of a sum TOTAL of COUNT numbers where L is (TOTAL COUNT)." 
  (if (zerop (second l))
      0
      (/ (first l) (second l))))
