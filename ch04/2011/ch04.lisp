;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Fri Jun 17 01:16:23 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch04
  (:use :common-lisp :test)
  (:shadow :reverse :append :nth :last :remove-duplicates))

(in-package :ch04)

;;;
;;;    4.7.1
;;;
(defun factorial (n)
  (check-type n (integer 0))
  (labels ((fact (n) ; The only purpose for using LABELS is to avoid calling CHECK-TYPE for every recursive call!
             (if (zerop n)
                 1
                 (* n (fact (1- n)))) ))
    (fact n)))

(defun replicate (obj n)
  (check-type n (integer 0))
  (labels ((repl (n)
             (if (zerop n)
                 '()
                 (cons obj (repl (1- n)))) ))
    (repl n)))

;;;
;;;    4.7.3
;;;
(defun make-change (money)
  (cond ((= money 0) '())
        ((< money 5) (list (list money 'penny)))
        ((< money 10) (multiple-value-bind (quot rem)
                          (truncate money 5)
                        (cons (list quot 'nickel) (make-change rem))))
        ((< money 25) (multiple-value-bind (quot rem)
                          (truncate money 10)
                        (cons (list quot 'dime) (make-change rem))))
        ((< money 50) (multiple-value-bind (quot rem)
                          (truncate money 25)
                        (cons (list quot 'quarter) (make-change rem))))
        ((< money 100) (multiple-value-bind (quot rem)
                           (truncate money 50)
                        (cons (list quot 'half-dollar) (make-change rem))))
        (t (multiple-value-bind (quot rem)
               (truncate money 100)
             (cons (list quot 'dollar) (make-change rem)))) ))

(defun make-change (money)
  (cond ((= money 0) '())
        ((< money 5) (list (list money (if (= money 1) 'penny 'pennies))))
        ((< money 10) (multiple-value-bind (quot rem)
                          (truncate money 5)
                        (cons (list quot 'nickel) (make-change rem))))
        ((< money 25) (multiple-value-bind (quot rem)
                          (truncate money 10)
                        (cons (list quot (if (= quot 1) 'dime 'dimes)) (make-change rem))))
        ((< money 50) (multiple-value-bind (quot rem)
                          (truncate money 25)
                        (cons (list quot 'quarter) (make-change rem))))
        ((< money 100) (multiple-value-bind (quot rem)
                           (truncate money 50)
                        (cons (list quot 'half-dollar) (make-change rem))))
        (t (multiple-value-bind (quot rem)
               (truncate money 100)
             (cons (list quot (if (= quot 1) 'dollar 'dollars)) (make-change rem)))) ))

;;;
;;;    4.7.4
;;;
(defclass coin ()
  ((value :reader value :initarg :value)
   (name :reader name :initarg :name)
   (plural :reader plural :initarg :plural)))

(defmethod print-object ((c coin) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~A ~D" (name c) (value c))))

(defun make-coin (value name plural)
  (make-instance 'coin :value value :name name :plural plural))

(defvar *coin-list* '((1 penny pennies)
                      (100 dollar dollars)
                      (50 half-dollar half-dollars)
                      (25 quarter quarters)
                      (5 nickel nickels)
                      (10 dime dimes)))

(defvar *us-currency* (sort (mapcar #'(lambda (entry) (apply #'make-coin entry)) *coin-list*) #'(lambda (x y) (> (value x) (value y)))) )

;;;
;;;    CURRENCY-LIST must be sorted in decreasing order.
;;;    
(defun make-change (money currency-list)
  (cond ((zerop money) '())
        ((endp currency-list) '())
        ((<= (value (first currency-list)) money)
         (multiple-value-bind (quot rem)
             (truncate money (value (first currency-list)))
           (cons (list quot (if (= quot 1) (name (first currency-list)) (plural (first currency-list))))
                 (make-change rem (rest currency-list)))) )
        (t (make-change money (rest currency-list)))) )

(deftest test-make-change ()
  (check
   (equal (make-change 94 *us-currency*) '((1 HALF-DOLLAR) (1 QUARTER) (1 DIME) (1 NICKEL) (4 PENNIES)))
   (equal (make-change 372 *us-currency*) '((3 DOLLARS) (1 HALF-DOLLAR) (2 DIMES) (2 PENNIES)))) )

;;;
;;;    4.7.5
;;;
(defun reverse (l)
  (labels ((reverse-tr (l result)
             (if (endp l)
                 result
                 (reverse-tr (rest l) (cons (first l) result)))) )
    (reverse-tr l '())))

(deftest test-reverse ()
  (check
   (equal (reverse '()) (cl:reverse '()))
   (equal (reverse '(a b c)) (cl:reverse '(a b c)))
   (equal (reverse '(1 (2 3) 4)) (cl:reverse '(1 (2 3) 4)))) )

;;;
;;;    4.7.6
;;;
(defun append (l1 l2)
  (if (endp l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

(deftest test-append ()
  (check
   (equal (append '(1 2 3) '(a b c)) (cl:append '(1 2 3) '(a b c)))
   (equal (append '() '(a b c)) (cl:append '() '(a b c)))
   (equal (append '(1 2 3) '()) (cl:append '(1 2 3) '()))) )

;;;
;;;    4.7.7
;;;
(defun nth (n l)
  (if (zerop n)
      (first l)
      (nth (1- n) (rest l))))

(deftest test-nth ()
  (check
   (equal (nth 0 '(a b c)) (cl:nth 0 '(a b c)))
   (equal (nth 1 '(a b c)) (cl:nth 1 '(a b c)))
   (equal (nth 8 '(a b c)) (cl:nth 8 '(a b c)))) )

;;;
;;;    4.7.8
;;;
(defun last (l)
  (if (atom (rest l))
      l
      (last (rest l))))

(deftest test-last ()
  (check
   (equal (last '(a b c)) (cl:last '(a b c)))
   (equal (last '(a b . c)) (cl:last '(a b . c)))
   (equal (last '()) (cl:last '()))) )

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
   (equal (remove-duplicates '(a b c d e f g)) (cl:remove-duplicates '(a b c d e f g)))
   (equal (remove-duplicates '()) (cl:remove-duplicates '()))) )

;;;
;;;    4.7.10
;;;
(defun check-book (balance transaction-list)
  (cond ((not (numberp balance)) (error "Non-numeric balance: ~A" balance))
        ((not (listp transaction-list)) (error "Transaction list is not a list: ~A" transaction-list))
        (t (process-transaction-list balance transaction-list))))

(defun process-transaction-list (balance transaction-list)
  (if (endp transaction-list)
      balance
      (process-transaction-list (process balance (first transaction-list)) (rest transaction-list))))

(defun transactionp (obj)
  (or (numberp obj)
      (and (consp obj)
           (numberp (first obj))
           (plusp (first obj))
           (null (rest obj)))) )

(defun process (balance transaction)
  (if (transactionp transaction)
      (typecase transaction
        (number (+ balance transaction))
        (cons (* balance (first transaction))))
      (error "Invalid transaction: ~A" transaction)))

(deftest test-check-book ()
  (check
   (= (check-book 100 '(100 50 -75)) 175)
   (= (check-book 100 '(-17.50 -1.73 -7.5)) 73.27)
   (= (check-book 100 '(100 50 -50 (1.1))) 220.0)
   (= (check-book 100 '((1.1) 100 50 -50 (1.1))) 231.0)
   (eq (type-of (handler-case (check-book 100 -17.50)
                  (error (e) (print e))))
       'simple-error)
   (eq (type-of (handler-case (check-book 'balance '(-17.50 -1.73 -7.5))
                  (error (e) (print e))))
       'simple-error)))

;;;
;;;    4.7.11
;;;
(defparameter *penalty* 0.1)
(defparameter *minimum-balance* 500)

(defun now-account (balance transaction-list)
  (cond ((not (numberp balance)) (error "Non-numeric balance: ~A" balance))
        ((not (listp transaction-list)) (error "Transaction list is not a list: ~A" transaction-list))
        (t (process-now-account-transaction-list balance transaction-list))))

(defun process-now-account-transaction-list (balance transaction-list)
  (if (endp transaction-list)
      balance
      (process-now-account-transaction-list (process-now-account-transaction balance (first transaction-list)) (rest transaction-list))))

(defun process-now-account-transaction (balance transaction)
  (if (transactionp transaction)
      (typecase transaction
        (number (if (and (minusp transaction)
                         (< balance *minimum-balance*))
                    (+ balance transaction (- *penalty*))
                    (+ balance transaction)))
        (cons (if (> balance *minimum-balance*)
                  (* balance (first transaction))
                  balance)))
      (error "Invalid transaction: ~A" transaction)))

(deftest test-now-account ()
  (check
   (= (now-account 100 '(100 50 -75)) 174.9)
   (= (now-account 100 '(-17.50 -1.73 -7.5)) 72.97)
   (= (now-account 100 '(100 50 -50 (1.1))) 199.9)
   (= (now-account 500 '(100 50 -50 (1.1))) 660.0)
   (= (now-account 100 '((1.1) 100 50 -50 (1.1))) 199.9)
   (eq (type-of (handler-case (now-account 100 -17.50)
                  (error (e) (print e))))
       'simple-error)
   (eq (type-of (handler-case (now-account 'balance '(-17.50 -1.73 -7.5))
                  (error (e) (print e))))
       'simple-error)))


;;;
;;;    4.7.12
;;;    Final 2010 version is the best.
;;;    Both of these pass the master test suite (2010).
;;;
(defun matchp (pattern input)
  (cond ((null input) (or (null pattern) (and (wildp (first pattern)) (matchp (rest pattern) input))) )
        ((null pattern) nil)
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (matchp pattern (rest input))))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (and (not (endp input))
                                          (matchp pattern (rest input)))) )
        ((endp input) nil)
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun wildp (s)
  (eq s '*wild*))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c d) '(a b c)))
   (not (matchp '(a b c) '(a b c d)))
   (matchp '(a *wild*) '(a b c))
   (matchp '(a *wild*) '(a))
   (matchp '(a *wild* b) '(a b c d b))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (matchp '(*wild*) '(a b c))))

;;;
;;;    4.7.13
;;;
(defun count-occurrences (obj tree)
  (cond ((equal obj tree) 1)
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
;        ((numberp tree) (+ n tree))
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
  (apply #'/ (tree-sum-count tree)))

(defun tree-sum-count (tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list tree 1))
        (t (mapcar #'+ (tree-sum-count (car tree)) (tree-sum-count (cdr tree)))) ))

(deftest test-tree-average ()
  (check
   (= (tree-average '(1 2 (3 (4 (5) 6) (7)) 8 (9))) 5)
   (= (tree-average '(((((1)))))) 1)))

   