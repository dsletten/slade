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
;;;;   Started:            Fri Feb 27 22:54:21 2009
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
;;;;   Notes: See handler-case use in 2007 version.
;;;;
;;;;

(defpackage ch04 (:use common-lisp) (:shadow append nth last remove-duplicates))

(in-package ch04)

(defconstant us-currency '((100 dollar)
                           (50 half-dollar)
                           (25 quarter)
                           (10 dime)
                           (5 nickel)
                           (1 penny)))

(defun make-change (money currency-list)
  (if (zerop money)
      '()
      (destructuring-bind ((value denomination) . rest) currency-list
        (if (>= money value)
            (cons (list (truncate money value) denomination)
                  (make-change (rem money value) rest))
            (make-change money rest)))) )

(defun make-change (money currency-list)
  (loop for (value denomination) in currency-list
        with m = money
        when (>= m value)
        collect (list (truncate m value) denomination)
        and do (setf m (rem m value))))

;;;
;;;    4.7.1
;;;
(defun replicate (obj count)
  (cond ((or (not (integerp count)) (minusp count)) (error "COUNT must be a non-negative integer."))
        ((zerop count) '())
        (t (cons obj (replicate obj (1- count)))) ))

(defun replicate (obj count)
  (if (or (not (integerp count)) (minusp count))
      (error "COUNT must be a non-negative integer.")
      (loop repeat count
            collect obj)))

(defun replicate (obj count)
  (assert (typep count '(integer 0 *))
          (count)
          "COUNT must be a non-negative integer.")
  (loop repeat count
        collect obj))
;;;
;;;    4.7.2
;;;

;;;
;;;    4.7.3
;;;
(defconstant us-currency-plural '((100 dollar dollars)
                                  (50 half-dollar dollars)
                                  (25 quarter quarters)
                                  (10 dime dimes)
                                  (5 nickel nickels)
                                  (1 penny pennies)))

(defun make-change (money currency-list)
  (if (zerop money)
      '()
      (destructuring-bind ((value denomination plural) . rest) currency-list
        (if (>= money value)
            (cons (change-list (truncate money value) denomination plural)
                  (make-change (rem money value) rest))
            (make-change money rest)))) )

(defun change-list (count singular plural)
  (if (= count 1)
      (list count singular)
      (list count plural)))

(defun make-change (money currency-list)
  (loop for (value denomination plural) in currency-list
        with m = money
        when (>= m value)
        collect (change-list (truncate m value) denomination plural)
        and do (setf m (rem m value))))

;;;
;;;    4.7.6
;;;
(defun append (l1 l2)
  (if (endp l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

;;;
;;;    4.7.7
;;;
(defun nth (index list)
  (cond ((endp list) nil)
        ((zerop index) (first list))
        (t (nth (1- index) (rest list)))) )

;;;
;;;    4.7.8
;;;
(defun last (list)
  (if (atom (rest list))
      list
      (last (rest list))))

;;;
;;;    4.7.9
;;;
(defun remove-duplicates (list)
  (cond ((endp list) '())
        ((member (first list) (rest list))
         (remove-duplicates (rest list)))
        (t (cons (first list)
                 (remove-duplicates (rest list)))) ))

;;;
;;;    4.7.10
;;;    See assertions in ch04p.lisp
;;;
(defun check-book (balance transactions)
  (cond ((not (numberp balance)) (error "BALANCE must be a number."))
        ((not (listp transactions)) (error "TRANSACTIONS must be a list."))
        ((endp transactions) balance)
        ((numberp (first transactions))
         (check-book (+ balance (first transactions)) (rest transactions)))
        ((listp (first transactions))
         (if (and (= (length (first transactions)) 1)
                  (numberp (caar transactions))
                  (plusp (caar transactions)))
             (check-book (* balance (caar transactions)) (rest transactions))
             (error "Bad interest rate specifier.")))
        (t (error "Invalid transaction."))))

;;;
;;;    4.7.11
;;;
(defconstant balance-threshold 500)
(defconstant charge 0.1)
(defun now-account (balance transactions)
  (cond ((not (numberp balance)) (error "BALANCE must be a number."))
        ((not (listp transactions)) (error "TRANSACTIONS must be a list."))
        ((endp transactions) balance)
        ((numberp (first transactions))
         (if (and (minusp (first transactions)) (< balance balance-threshold))
             (now-account (+ balance (first transactions) (- charge)) (rest transactions))
             (now-account (+ balance (first transactions)) (rest transactions))))
        ((listp (first transactions))
         (if (and (= (length (first transactions)) 1)
                  (numberp (caar transactions))
                  (plusp (caar transactions)))
             (if (>= balance balance-threshold)
                 (now-account (* balance (caar transactions)) (rest transactions))
                 (now-account balance (rest transactions)))
             (error "Bad interest rate specifier.")))
        (t (error "Invalid transaction."))))

;;;
;;;    4.7.12
;;;    Look at 2007
;;;    This fails the master test suite (2010).
;;;
(defconstant wild-card '*wild*)
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (and (eq (first pattern) wild-card)
                           (endp (rest pattern))))
        ((eq (first pattern) (first input))
         (matchp (rest pattern) (rest input)))
        ((eq (first pattern) wild-card)
         (or (matchp (rest pattern) input)
             (matchp pattern (rest input))))
        (t nil)))

;;;
;;;    4.7.13
;;;
(defun count-occurrences (obj tree)
  (cond ((eql obj tree) 1)
        ((atom tree) 0)
        (t (+ (count-occurrences obj (car tree))
              (count-occurrences obj (cdr tree)))) ))

;;;
;;;    4.7.14
;;;
(defun tree-addition (val tree)
  (cond ((null tree) '())
        ((atom tree) (+ val tree))
        (t (cons (tree-addition val (car tree))
                 (tree-addition val (cdr tree)))) ))

;;;
;;;    4.7.15
;;;
(defun average (l)
  (/ (first l) (second l)))

;; (defun tree-average (tree)
;;   (average (tree-average-aux tree 0 0)))

(defun tree-average (tree)
  (average (tree-average-aux tree)))

(defun tree-average-aux (tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list tree 1))
        (t (combine-results (tree-average-aux (car tree))
                            (tree-average-aux (cdr tree)))) ))

;;;
;;;    See 2007 why SUM/COUNT args are pointless!
;;;    
;; (defun tree-average-aux (tree sum count)
;;   (cond ((null tree) (list sum count)) ; Think about why ch04p version works w/ (list 0 0) here.
;;         ((atom tree) (list (+ sum tree) (1+ count))) ; and (list tree 1) here.
;;         (t (combine-results (tree-average-aux (car tree) sum count)
;;                             (tree-average-aux (cdr tree) sum count)))) )

(defun combine-results (result1 result2)
  (list (+ (first result1) (first result2))
        (+ (second result1) (second result2))))
