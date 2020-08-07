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
;;;;   Started:            Fri Mar 31 23:52:29 2006
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
(defun add2 (x)
  (+ x 2))

(deftest test-add2 ()
  (check
   (= (add2 3) 5)
   (= (add2 -1) 1)
   (= (add2 0) 2)
   (= (add2 3.2) 5.2)))

(defun add5 (x)
  (+ x 5))

(deftest test-add5 ()
  (check
   (= (add5 3) 8)
   (= (add5 -1) 4)
   (= (add5 0) 5)
   (= (add5 3.2) 8.2)))

(defun double (x)
  (* x 2))

(deftest test-double ()
  (check
   (= (double 3) 6)
   (= (double -1) -2)
   (= (double 0) 0)
   (= (double 3.2) 6.4)))

(defun min-abs (&rest nums)
  (reduce #'min (mapcar #'abs nums)))

(deftest test-min-abs ()
  (check
   (= (min-abs 3 5 -2 -8) 2)
   (= (min-abs -1) 1)
   (= (min-abs 0) 0)
   (= (min-abs 3.2 -9.8 0.1 -0.02) 0.02)))

(defun max-abs (&rest nums)
  (reduce #'max (mapcar #'abs nums)))

(deftest test-max-abs ()
  (check
   (= (max-abs 3 5 -2 -8) 8)
   (= (max-abs -1) 1)
   (= (max-abs 0) 0)
   (= (max-abs 3.2 -9.8 0.1 -0.02) 9.8)))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
	  (cl:floor (- (* 13/5 m) 1/5))
	  y
	  (cl:floor y 4)
	  (cl:floor c 4)
	  (* -2 c)
	  (- (* (1+ l) (cl:floor m 11))))
       7))

(deftest test-zeller ()
  (check
   (= (zeller 1 7 19 96 1) 0)
   (= (zeller 16 5 19 99 0) 5)
   (= (zeller 1 4 19 96 1) 6)
   (= (zeller 11 7 20 1 0) 2)))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (or (and (zerop x) x)
      (/ (abs x) x)))

(deftest test-signum ()
  (check
   (= (signum -1.2) (cl:signum -1.2))
   (= (signum 0) (cl:signum 0))
   (= (signum 0.0) (cl:signum 0.0))
   (= (signum 99) (cl:signum 99))))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (cond ((integerp x) x) ; Fails for -6.0
	((minusp x) (- (floor (1+ (- x)))) )
	(t (truncate x))))

(deftest test-floor ()
  (check
   (= (floor 17) (cl:floor 17))
   (= (floor 8.2) (cl:floor 8.2))
   (= (floor 0) (cl:floor 0))
   (= (floor -9.7) (cl:floor -9.7))
   (= (floor -13) (cl:floor -13))))

(defun ceiling (x)
  (cond ((integerp x) x) ; Fails for 6.0
	((plusp x) (- (ceiling (1- (- x)))) )
	(t (truncate x))))

(deftest test-ceiling ()
  (check
   (= (ceiling 17) (cl:ceiling 17))
   (= (ceiling 8.2) (cl:ceiling 8.2))
   (= (ceiling 0) (cl:ceiling 0))
   (= (ceiling -9.7) (cl:ceiling -9.7))
   (= (ceiling -13) (cl:ceiling -13))))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (or (zerop (mod y 400))
      (and (zerop (mod y 4))
	   (not (zerop (mod y 100)))) ))

;;;
;;;    I like this one better.
;;;    
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)) t)
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )

(deftest test-leap-year-p ()
  (check
   (leap-year-p 2000)
   (leap-year-p 2004)
   (leap-year-p 1996)
   (not (leap-year-p 1900))
   (not (leap-year-p 1999))
   (not (leap-year-p 2006))))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (n m y)
  (zeller n m (truncate y 100) (mod y 100) (if (leap-year-p y) 1 0)))

(deftest test-son-of-zeller ()
  (check
   (= (son-of-zeller 1 7 1996) 0)
   (= (son-of-zeller 16 5 1999) 5)
   (= (son-of-zeller 1 4 1996) 6)
   (= (son-of-zeller 11 7 2001) 2)))

(deftest test-all ()
  (test-add2)
  (test-add5)
  (test-double)
  (test-min-abs)
  (test-max-abs)
  (test-zeller)
  (test-signum)
  (test-floor)
  (test-ceiling)
  (test-leap-year-p)
  (test-son-of-zeller))
