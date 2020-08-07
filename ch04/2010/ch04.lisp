;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Wed Jun 23 00:38:59 2010
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
(load "/Users/dsletten/lisp/packages/test")

(defpackage ch04 (:use common-lisp test) (:shadow reverse append nth last remove-duplicates))

(in-package ch04)

;;;
;;;    4.7.1
;;;
(defun replicate (obj n)
  (check-type n integer) ; Negative integer?!?
  (if (zerop n)
      '()
      (cons obj (replicate obj (1- n)))) )

(defun fact (n)
  (check-type n integer)
  (if (zerop n)
      1
      (* n (fact (1- n)))) )

;;;
;;;    4.7.2
;;;

;;;
;;;    4.7.3
;;;    Only pennies, dimes, dollars can be plural
;;;
(defun make-change (money)
  (cond ((= money 0) '())
        ((< money 5) (list (list money (if (= money 1) 'penny 'pennies))))
        ((< money 10) (cons (list (truncate money 5) 'nickel)
                            (make-change (rem money 5))))
        ((< money 25) (cons (list (truncate money 10)
                                  (if (= (truncate money 10) 1)
                                      'dime
                                      'dimes))
                            (make-change (rem money 10))))
        ((< money 50) (cons (list (truncate money 25) 'quarter)
                            (make-change (rem money 25))))
        ((< money 100) (cons (list (truncate money 50) 'half-dollar)
                             (make-change (rem money 50))))
        (t (cons (list (truncate money 100)
                       (if (= (truncate money 100) 1)
                           'dollar
                           'dollars))
                 (make-change (rem money 100)))) ))

(defun plural-list (n singular plural)
  (if (= n 1)
      (list n singular)
      (list n plural)))

(defun make-change (money)
  (cond ((= money 0) '())
        ((< money 5) (list (plural-list money 'penny 'pennies)))
        ((< money 10) (cons (plural-list (truncate money 5) 'nickel 'nickels)
                            (make-change (rem money 5))))
        ((< money 25) (cons (plural-list (truncate money 10) 'dime 'dimes)
                            (make-change (rem money 10))))
        ((< money 50) (cons (plural-list (truncate money 25) 'quarter 'quarters)
                            (make-change (rem money 25))))
        ((< money 100) (cons (plural-list (truncate money 50) 'half-dollar 'half-dollars)
                             (make-change (rem money 50))))
        (t (cons (plural-list (truncate money 100) 'dollar 'dollars)
                 (make-change (rem money 100)))) ))

;;;
;;;    4.7.4
;;;
(defconstant us-currency '((100 dollar) (50 half-dollar) (25 quarter) (10 dime) (5 nickel) (1 penny pennies)))

(defun get-plural (n denomination)
  (if (= n 1)
      (list n (second denomination))
      (let ((plural (third denomination)))
        (if plural
            (list n plural)
            (list n (read-from-string (format nil "~As" (second denomination)))) ))))

(defun new-make-change (money currency-list)
  (cond ((endp currency-list) '())
        ((= money 0) '())
        ((>= money (first (first currency-list)))
         (cons (get-plural (truncate money (first (first currency-list))) (first currency-list))
               (new-make-change (rem money (first (first currency-list))) (rest currency-list))))
        (t (new-make-change money (rest currency-list)))) )

;; (defun get-value (denomination)
;;   (first denomination))

;; (defun get-singular (denomination)
;;   (second denomination))

;; (defun get-plural (denomination)
;;   (third denomination))

(defun get-plural (n singular plural)
  (if (= n 1)
      singular
      (if plural
          plural
          (values (read-from-string (format nil "~As" singular)))) ))

(defun new-make-change (money currency-list)
  (if (or (endp currency-list) (= money 0))
      '()
      (destructuring-bind ((value singular &optional plural) . rest) currency-list
        (cond ((>= money value)
               (let ((count (truncate money value))
                     (remainder (rem money value)))
                 (cons (list count (get-plural count singular plural))
                       (new-make-change remainder rest))))
        (t (new-make-change money rest)))) ))

;;;
;;;    4.7.5
;;;
(defun reverse (l)
  (reverse-aux l '()))

(defun reverse-aux (l result)
  (if (endp l)
      result
      (reverse-aux (rest l) (cons (first l) result))))

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
(defun nth (n l)
  (if (zerop n)
      (first l)
      (nth (1- n) (rest l))))

;;;
;;;    4.7.8
;;;
(defun last (l)
  (if (atom (cdr l))
      l
      (last (cdr l))))

;;;
;;;    4.7.9
;;;
(defun remove-duplicates (l)
  (cond ((endp l) '())
        ((member (first l) (rest l)) (remove-duplicates (rest l)))
        (t (cons (first l) (remove-duplicates (rest l)))) ))

;;;
;;;    4.7.10
;;;
(defun check-book (balance transactions)
  (cond ((not (numberp balance)) (cons 'error--non-numeric-balance balance))
        ((not (listp transactions)) (cons 'error--atom-instead-of-list transactions))
        ((endp transactions) balance)
        ((numberp (first transactions)) (check-book (+ balance (first transactions))
                                                    (rest transactions)))
        ((listp (first transactions)) (if (not (and (= (length (first transactions)) 1)
                                                    (numberp (first (first transactions)))) )
                                          (cons 'error--invalid-interest-expression (first transactions))
                                          (check-book (* balance (first (first transactions)))
                                                      (rest transactions))))
        (t (cons 'error--transaction-list-error transactions))))

;;;
;;;    4.7.11
;;;
(defconstant penalty 0.1)
(defconstant penalty-minimum-balance 500)
(defconstant interest-minimum-balance 500)

(defun now-account (balance transactions)
  (cond ((not (numberp balance)) (cons 'error--non-numeric-balance balance))
        ((not (listp transactions)) (cons 'error--atom-instead-of-list transactions))
        ((endp transactions) balance)
        ((numberp (first transactions)) (if (and (minusp (first transactions))
                                                 (< balance penalty-minimum-balance))
                                            (now-account (+ balance (first transactions) (- penalty))
                                                        (rest transactions))
                                            (now-account (+ balance (first transactions))
                                                        (rest transactions))))
        ((listp (first transactions)) (if (not (and (= (length (first transactions)) 1)
                                                    (numberp (first (first transactions)))) )
                                          (cons 'error--invalid-interest-expression (first transactions))
                                          (if (>= balance interest-minimum-balance)
                                              (now-account (* balance (first (first transactions)))
                                                          (rest transactions))
                                              (now-account balance (rest transactions)))) )
        (t (cons 'error--transaction-list-error transactions))))

;;;
;;;    4.7.12
;;;
(defun wildp (obj)
  (eq obj '*wild*))

;; (defun matchp (pattern input)
;;   (cond ((endp pattern) (endp input))
;;         ((endp input) nil) ; This is the first version to get this simplification right? <-- Ha! Broken...
;;         ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
;;         ((wildp (first pattern)) (or (matchp (rest pattern) input)
;;                                      (matchp pattern (rest input))))
;;         (t nil)))

;;;
;;;    The 3 below all pass the master test suite.
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (and (not (endp input)) (matchp pattern (rest input)))) )
        ((endp input) nil)
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

;;;
;;;    Alternatively...
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (and (wildp (first pattern))
                           (matchp (rest pattern) input)))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (matchp pattern (rest input))))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

;;;
;;;   5 cases:
;;;   1. PATTERN is '()
;;;   2. PATTERN has wild first elt.
;;;      2a. INPUT is '(). (Any remaining elts in PATTERN must be wild for match to succeed, i.e.,
;;;          eventually case 1. Otherwise, eventually case 3.)
;;;      2b. 0 elts in INPUT match wildcard. Try to match rest of PATTERN.
;;;      2c. 1+ elts in INPUT match wildcard . Try to match wildcard against elts of INPUT.
;;;
;;;Does order of 2b, 2c matter????
;;;
;;;   The remaining cases invovle a PATTERN with a literal first elt.
;;;   3. PATTERN is neither empty nor has a wild first elt. An empty INPUT list has nothing to
;;;      match the first elt of PATTERN, thus the match fails. This case would be eliminated by
;;;      case 4. except in the pathological case where pattern contains NIL (the empty list)
;;;      as an elt. We must distinguish an empty INPUT rather than match (FIRST '()) w/
;;;      (FIRST '(() ...)).
;;;   Neither PATTERN nor INPUT is empty below.
;;;   4. PATTERN has literal first elt that matches first elt of INPUT. Keep checking.
;;;   5. PATTERN has literal first elt that does not match first elt of INPUT. Fail.
;;;
;;;   Note: 2a. and 2b. must be handled separately, otherwise
;;;         2a. will mistakenly fall through to 2c. and into an
;;;         infinite loop.
;;;
;;;         Specifically, (matchp '(*wild* a) '()) must fail at 2a. not 2b.
;;;
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))  ;1
        ((wildp (first pattern)) ;2
         (if (endp input)
             (matchp (rest pattern) input) ;2a
             (or (matchp (rest pattern) input) ;2b
                 (matchp pattern (rest input)))) ) ;2c
        ((endp input) nil) ;3
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
        (t nil))) ;5

;; (deftest test-matchp ()
;;   (check
;;    (matchp '(a b c) '(a b c))
;;    (not (matchp '(a b c) '(a b c d)))
;;    (not (matchp '(a b c d) '(a b c)))
;;    (matchp '(a *wild*) '(a b c))
;;    (matchp '(a *wild*) '(a))
;;    (matchp '(a *wild* b) '(a b c d b))
;;    (not (matchp '(a *wild* b) '(a b c d e)))
;;    (matchp '(*wild* b *wild*) '(a b c d e))
;;    (matchp '(*wild*) '(a b c))
;;    (matchp '(a b *wild* c d) '(a b c d))
;;    (not (matchp '(a b ()) '(a b)))) )

;;;
;;;    Master test suite. Culled from all implementations (190824)
;;;    
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
   (matchp '(a b *wild* c d) '(a b c d))
   (not (matchp '(a b ()) '(a b)))
   (matchp '(*wild*) '())
   (matchp '(*wild* *wild*) '())
   (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*)
           '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
   (matchp '(*wild* a *wild*) '(b c a d))
   (matchp '(a b *wild* c d *wild*) '(a b c d c d c d))
   (matchp '(*wild* a *wild* a) '(a a a a a a a))
   (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d))))

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
(defun tree-addition (n tree)
  (cond ((null tree) '())
        ((numberp tree) (+ n tree))
        (t (cons (tree-addition n (car tree))
                 (tree-addition n (cdr tree)))) ))

;;;
;;;    4.7.15
;;;
(defun tree-average (tree)
  (average (tree-average-aux 0 0 tree)))

(defun average (result)
  (/ (first result) (second result)))

(defun tree-average-aux (sum count tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list (+ sum tree) (1+ count)))
        (t (combine-results (tree-average-aux sum count (car tree))
                            (tree-average-aux 0 0 (cdr tree)))) ))

;;;
;;;    Conflict with TEST::COMBINE-RESULTS
;;;    
;; (defun combine-results (result1 result2)
;;   (list (+ (first result1) (first result2))
;;         (+ (second result1) (second result2))))

;; ;;;
;; ;;;    See 040910
;; ;;;    
;; (defun combine-results (result1 result2)
;;   (mapcar #'+ result1 result2))