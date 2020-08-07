;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Thu Jul  5 18:19:16 2007
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
(load "/Users/dsletten/lisp/packages/test.fasl")

(defpackage ch04
  (:use common-lisp test)
  (:shadow reverse append nth last remove-duplicates combine-results)) ;; Ooops! Seibel uses COMBINE-RESULTS in test package.

(in-package ch04)

;;;
;;;    4.7.1
;;;
(defun replicate (obj n)
  (assert (and (>= n 0) (integerp n)) (n) "Argument should be non-negative integer.")
  (if (zerop n)
      '()
      (cons obj (replicate obj (1- n)))) )

(deftest test-replicate ()
  (check
   (equal (replicate 'a 5) '(a a a a a))
   (eq (type-of (handler-case (replicate 'a -2)
		  (simple-error (e) (print e))))
       'simple-error)))

(defun factorial (n)
  (assert (and (>= n 0) (integerp n)) (n) "Argument should be non-negative integer.")
  (if (zerop n)
      1
      (* n (factorial (1- n)))) )

;;;
;;;    4.7.2
;;;    See roman.lisp

;;;
;;;    4.7.3
;;;
(defun make-change (money)
  (cond ((= money 0) '())
	((< money 5) (list (pluralize money 'penny 'pennies)))
	((< money 10) (cons (pluralize (truncate money 5) 'nickel 'nickels)
			    (make-change (rem money 5))))
	((< money 25) (cons (pluralize (truncate money 10) 'dime 'nickels)
			    (make-change (rem money 10))))
	((< money 50) (cons (pluralize (truncate money 25) 'quarter 'quarters)
			    (make-change (rem money 25))))
	((< money 100) (cons (pluralize (truncate money 50) 'half-dollar 'half-dollars)
			     (make-change (rem money 50))))
	(t (cons (pluralize (truncate money 100) 'dollar 'dollars)
		 (make-change (rem money 100)))) ))

(defun pluralize (value singular plural)
  (if (= value 1)
      (list value singular)
      (list value plural)))

;;;
;;;    4.7.4
;;;    
(defconstant us-currency '((100 dollar dollars)
			   (50 half-dollar half-dollars)
			   (25 quarter quarters)
			   (10 dime dimes)
			   (5 nickel nickels)
			   (1 penny pennies)))

;; (defun make-change (money currency-list)
;;   (cond ((endp currency-list) '())
;; 	((= money 0) '())
;; 	((>= money (caar currency-list))
;; 	 (cons (list (truncate money (caar currency-list))
;; 		     (cadar currency-list))
;; 	       (make-change (rem money (caar currency-list))
;; 			    (rest currency-list))))
;; 	(t (make-change money (rest currency-list)))) )

(defun make-change-2 (money currency-list)
  (cond ((endp currency-list) '())
	((= money 0) '())
	(t (destructuring-bind ((value currency plural) &rest tail) currency-list
	     (if (>= money value)
		 (cons (pluralize (truncate money value) currency plural)
		       (make-change-2 (rem money value) tail))
		 (make-change-2 money tail)))) ))

;(loop for i from 1 to 200 do (format t "~D ~A~%" i (make-change-2 i us-currency)))

;;;
;;;    4.7.5
;;;
(defun reverse (l)
  "Create a new list with the elements of L in reverse order."
  (labels ((reverse-aux (l result)
	     (if (endp l)
		 result
		 (reverse-aux (rest l) (cons (first l) result)))) )
    (reverse-aux l '())))

(deftest test-reverse ()
  (check
   (equal (reverse '(a b c d)) (cl:reverse '(a b c d)))
   (equal (reverse '(1)) (cl:reverse '(1)))
   (equal (reverse '()) (cl:reverse '()))
   (equal (reverse '(a b (c d))) (cl:reverse '(a b (c d)))) ))

;;;
;;;    4.7.6
;;;
(defun append (l1 l2)
  (if (endp l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

(deftest test-append ()
  (check
   (equal (append '(1 2 3) '(4 5 6)) (cl:append '(1 2 3) '(4 5 6)))
   (equal (append '() '(4 5 6)) (cl:append '() '(4 5 6)))
   (equal (append '(1 2 3) '()) (cl:append '(1 2 3) '()))))

;;;
;;;    4.7.7
;;;
(defun nth (n l)
  (if (zerop n)
      (first l)
      (nth (1- n) (rest l))))

(deftest test-nth ()
  (check
   (equal (nth 2 '(a b c d)) (cl:nth 2 '(a b c d)))
   (equal (nth 0 '(a b c d)) (cl:nth 0 '(a b c d)))
   (equal (nth 9 '(a b c d)) (cl:nth 9 '(a b c d)))
   (equal (nth 2 '(a (b c) d)) (cl:nth 2 '(a (b c) d)))))

;;;
;;;    4.7.8
;;;
(defun last (l)
  (if (atom (cdr l))
      l
      (last (cdr l))))

(deftest test-last ()
  (check
   (equal (last '(a b c)) (cl:last '(a b c)))
   (equal (last '(a b . c)) (cl:last '(a b . c)))
   (equal (last '()) (cl:last '()))))

;;;
;;;    Ripped off from CLHS entry for LAST!!
;;;    
(defun last (l &optional (n 1))
  (labels ((last-aux (l result i)
             (if (atom l)
                 result
                 (if (>= i n)
                     (last-aux (rest l) (rest result) (1+ i))
                     (last-aux (rest l) result (1+ i)))) ))
    (last-aux l l 0)))

(deftest test-last ()
  (check
   (equal (last '(a b c d) 2) (cl:last '(a b c d) 2))
   (equal (last '(a b c d) 0) (cl:last '(a b c d) 0))
   (equal (last '(a b c . d) 0) (cl:last '(a b c . d) 0))
   (equal (last '(a b c d) 1) (cl:last '(a b c d) 1))
   (equal (last '(a b c . d) 1) (cl:last '(a b c . d) 1))
   (equal (last '(a b c d) 4) (cl:last '(a b c d) 4))
   (equal (last '(a b c . d) 4) (cl:last '(a b c . d) 4))
   (equal (last '(a b c d) 20) (cl:last '(a b c d) 20))
   (equal (last '(a b c d) 20) (cl:last '(a b c d) 20))))

;;;
;;;    4.7.9
;;;
(defun remove-duplicates (l)
  (cond ((endp l) '())
	((member (first l) (rest l)) (remove-duplicates (rest l)))
	(t (cons (first l) (remove-duplicates (rest l)))) ))

(deftest test-remove-duplicates ()
    (check
     (equal (remove-duplicates '(a b c d a b f g)) (cl:remove-duplicates '(a b c d a b f g)))
     (equal (remove-duplicates '(a b c d f g)) (cl:remove-duplicates '(a b c d f g)))
     (equal (remove-duplicates '()) (cl:remove-duplicates '()))
     (equal (remove-duplicates '((a b) c d (a b) f g)) (cl:remove-duplicates '((a b) c d (a b) f g)))) )

;;;
;;;    4.7.10
;;;
(defun check-book (balance transactions)
  (assert (numberp balance) (balance) "BALANCE should be a number.")
  (assert (listp transactions) (transactions) "TRANSACTIONS should be a list.")
  (cond ((endp transactions) balance)
	((numberp (first transactions)) (check-book (+ balance (first transactions)) (rest transactions)))
	((listp (first transactions))
	 (assert (= (length (first transactions)) 1) () "Bad interest rate.")
	 (check-book (* balance (first (first transactions))) (rest transactions)))
	(t (error "Unrecognized transaction: ~A" (first transactions)))) )

(deftest test-check-book ()
  (check
   (= (check-book 100 '(100 50 -75)) 175)
   (= (check-book 100 '(-17.50 -1.73 -7.5)) 73.27)
   (= (check-book 100 '(100 50 -50 (1.1))) 220.0)
   (= (check-book 100 '((1.1) 100 50 -50 (1.1))) 231.0)))

;;;
;;;    4.7.11
;;;
(defvar *minimum-balance* 500)
(defvar *debit-charge* -0.10)
(defun now-account (balance transactions)
  (assert (numberp balance) (balance) "BALANCE should be a number.")
  (assert (listp transactions) (transactions) "TRANSACTIONS should be a list.")
  (cond ((endp transactions) balance)
	((numberp (first transactions))
	 (if (and (debitp (first transactions)) (< balance *minimum-balance*))
	     (now-account (+ balance (first transactions) *debit-charge*) (rest transactions))
	     (now-account (+ balance (first transactions)) (rest transactions))))
	((listp (first transactions))
	 (assert (= (length (first transactions)) 1) () "Bad interest rate.")
	 (if (< balance *minimum-balance*)
	     (now-account balance (rest transactions))
	     (now-account (* balance (first (first transactions))) (rest transactions))))
	(t (error "Unrecognized transaction: ~A" (first transactions)))) )

(defun debitp (n)
  (minusp n))

(deftest test-now-account ()
  (check
   (= (now-account 100 '(100 50 -50 (1.1))) 199.9)
   (= (now-account 100 '(500 50 -50 (1.1))) 660.0)))

;;;
;;;    4.7.12
;;;    This passes the master test suite (2010).
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (if (eql (first pattern) '*wild*) ; This test isn't absolutely necessary here in Lisp since (first '()) is defined. But it makes sense (It is necessary in Oz).
                          (matchp (rest pattern) input)
                          nil))
        ((eql (first pattern) (first input))
         (matchp (rest pattern) (rest input)))
        ((eql (first pattern) '*wild*)
         (or (matchp (rest pattern) input)
             (matchp pattern (rest input))))
        (t nil)))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (matchp '(a *wild*) '(a b c))
   (matchp '(a *wild*) '(a))
   (matchp '(a *wild* b) '(a b c d b))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (matchp '(*wild*) '(a b c))
   (matchp '(*wild*) '())
   (matchp '(*wild* *wild*) '()))) ;All other versions (including Slade's!) fail this test except for the one above.

(defun slade-matchp (pattern list)
  (cond ((and (endp pattern) (endp list)) t) ; !!
	((equalp (car pattern) (car list))
	 (slade-matchp (cdr pattern) (cdr list)))
	((eq (car pattern) '*wild*)
	 (cond ((endp list) (endp (cdr pattern)))
	       (t (or (slade-matchp (cdr pattern) list)
		      (slade-matchp pattern (cdr list))))))
	(t nil)))
;;;
;;;    4.7.13
;;;
(defun count-occurrences (obj tree)
  (cond	((eql obj tree) 1)
	((atom tree) 0)
	(t (+ (count-occurrences obj (car tree))
	      (count-occurrences obj (cdr tree)))) ))

(deftest test-count-occurrences ()
  (check
   (= (count-occurrences 'a '(a ((a b)) d c (a))) 3)
   (= (count-occurrences 'z '(a ((a b)) d c (a))) 0)))

;;;
;;;    4.7.14
;;;
(defun tree-addition (n tree)
  (cond ((null tree) '())
	((atom tree) (+ n tree))
	(t (cons (tree-addition n (car tree))
		 (tree-addition n (cdr tree)))) ))

(deftest test-tree-addition ()
  (check
   (equal (tree-addition 2 '(5 4 3 2 1)) '(7 6 5 4 3))
   (equal (tree-addition 3 '(1 2 (3 (4 (5) 6) (7)) 8 (9))) '(4 5 (6 (7 (8) 9) (10)) 11 (12)))
   (equal (tree-addition 5 '(((((1)))))) '(((((6))))))))

;;;
;;;    4.7.15
;;;
(defun tree-average (tree)
  (float (average (tree-average-aux tree))))
;  (average (tree-average-aux tree)))

(defun average (l)
  (/ (first l) (second l)))

;; (defun tree-average-aux (tree sum count)
;;   (cond ((null tree) (list 0 0))
;; 	((atom tree) (list tree 1))
;; 	(t (combine-results (tree-average-aux (car tree) sum count)
;; 			    (tree-average-aux (cdr tree) sum count)))) )

(defun tree-average-aux (tree)
  (cond ((null tree) (list 0 0))
	((atom tree) (list tree 1))
	(t (combine-results (tree-average-aux (car tree))
			    (tree-average-aux (cdr tree)))) ))

(defun combine-results (l1 l2)
  (list (+ (first l1) (first l2)) (+ (second l1) (second l2))))

;; * (tree-average-aux '(1 2 3))

;; (6 3)
;; * (tree-average-aux '(1 2 (3 (4 (5) 6) (7)) 8 (9)))

;; (45 9)
;; * (tree-average-aux '(((((((1))))))))

;;;
;;;    Slade cheats (using LET from ch. 5). Plus his version is
;;;    broken:
;;;    1. Don't use ENDP when traversing a tree. The arg may be
;;;       a non-nil atom.
;;;    2. The args COUNT and TOTAL to LEAF-AVERAGE are always both 0!
;;;       The accumulation occurs in ADD-MERGE, not as a result of
;;;       (+ total l) and (+ count 1). These parameters may be
;;;       disposed of as with TREE-AVERAGE-AUX above.
;;;       
(defun tree-average-slade (num-tree)
  (let ((pair (leaf-average num-tree 0 0)))
    (/ (car pair) (cdr pair))))

(defun leaf-average (l count total)
  (cond ((null l) '(0 . 0))
;  (cond ((endp l) '(0 . 0))
	((atom l) (cons (+ total l) (+ count 1)))
	(t (add-merge (leaf-average (car l) count total)
		      (leaf-average (cdr l) count total)))))

(defun add-merge (x y)
  (cons (+ (car x) (car y))
	(+ (cdr x) (cdr y))))
