;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Fri Jun  8 00:43:43 2007
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

(defpackage ch02
  (:use common-lisp test)
  (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun add2 (x) (+ x 2))

(defun add5 (x) (+ x 5))

(defun double (x) (* 2 x))

(defun min-abs4 (a b c d)
  (min (abs a) (abs b) (abs c) (abs d)))

(defun max-abs4 (a b c d)
  (max (abs a) (abs b) (abs c) (abs d)))

;;
;;    REDUCE vs. APPLY
;;    
(defun min-abs (&rest nums)
  (apply #'min (mapcar #'abs nums)))

(defun max-abs (&rest nums)
  (apply #'max (mapcar #'abs nums)))

;;;
;;;    2.11.3
;;;
(defun ajoutez (x y) (+ x y))

(defun retranchez (x y) (- x y))

(defun hochstmas (x y) (max x y))

(defun multiplizieren (x y) (* x y))

(defun njia-ya-kutokea () (sb-ext:quit))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
	  (cl:floor (- (* 13/5 m) 1/5))
	  y
	  (cl:floor y 4)
	  (cl:floor c 4)
	  (- (* 2 c))
	  (- (* (1+ l) (cl:floor m 11))))
       7))

;;;
;;;    2.11.5
;;;
;; (defun signum (x)
;;   (cond ((plusp x) 1)
;; 	((minusp x) -1)
;; 	((zerop x) 0)))

(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

;; (defun signum (x)
;;   (if (zerop x)
;;       x
;;       (/ x (abs x))))

;; (defun interest-rate (money)
;;   (cond ((<= money 0) 0)
;; 	((< money 1000) 2)
;; 	((< money 10000) 5)
;; 	((< money 100000) 7)
;; 	(t 10)))

(defun interest-rate (money)
  (or (and (<= money 0) 0)
      (and (< money 1000) 2)
      (and (< money 10000) 5)
      (and (< money 100000) 7)
      10))

;;;
;;;    2.11.7
;;;
;; (defun go-to-movie-p (age cash)
;;   (or (and (< age 12)
;; 	   (> cash 3.0))
;;       (and (>= age 12)
;; 	   (< age 65)
;; 	   (> cash 7.0))
;;       (and (not (< age 65))
;; 	   (> cash 4.5))))

(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.0))
	((< age 65) (> cash 7.0))
	(t (> cash 4.5))))

;;;
;;;    2.11.9
;;;
;; (and p1 p2 p3 p4) -> 
;; (cond ((not p1))
;;       ((not p2))
;;       ((not p3))
;;       (t p4))

;; (or p1 p2 p3 p4) ->
;; (cond (p1)
;;       (p2)
;;       (p3)
;;       (p4))

;;;
;;;    2.11.10
;;;
;; (defun floor (x)
;;   (if (minusp x)
;;       (- (floor (1+ (- x)))) ; Fails for negative integers
;;       (values (truncate x))))

;; (defun ceiling (x)
;;   (if (plusp x)
;;       (- (ceiling (1- (- x)))) ; Fails for positive integers
;;       (values (truncate x))))

(defun floor (x)
  (cond ((= x (truncate x)) x)
	((minusp x) (- (floor (1+ (- x)))) )
	(t (values (truncate x)))) )

(deftest test-floor ()
  (check
   (= (floor 17) (cl:floor 17))
   (= (floor 17.0) (cl:floor 17.0))
   (= (floor 8.2) (cl:floor 8.2))
   (= (floor 0) (cl:floor 0))
   (= (floor -9.7) (cl:floor -9.7))
   (= (floor -13) (cl:floor -13))
   (= (floor -13.0) (cl:floor -13.0))))

(defun ceiling (x)
  (cond ((= x (truncate x)) x)
	((plusp x) (- (ceiling (1- (- x)))) )
	(t (values (truncate x)))) )

(deftest test-ceiling ()
  (check
   (= (ceiling 17) (cl:ceiling 17))
   (= (ceiling 17.0) (cl:ceiling 17.0))
   (= (ceiling 8.2) (cl:ceiling 8.2))
   (= (ceiling 0) (cl:ceiling 0))
   (= (ceiling -9.7) (cl:ceiling -9.7))
   (= (ceiling -13) (cl:ceiling -13))
   (= (ceiling -13.0) (cl:ceiling -13.0))))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)) t)
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day funny-month year)
  (zeller day funny-month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))
