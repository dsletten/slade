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
;;;;   Started:            Wed Sep 23 02:04:26 2009
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
(load "/Users/dsletten/lisp/packages/test.dfsl")

(defpackage ch02 (:use common-lisp test) (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun add2 (x) (+ x 2))
(defun add5 (x) (+ x 5))
(defun double (x) (* x 2))
(defun min-abs4 (w x y z) (min (abs w) (abs x) (abs y) (abs z)))
(defun max-abs4 (w x y z) (max (abs w) (abs x) (abs y) (abs z)))

(deftest test-add2 ()
  (check
   (= (add2 5) 7)
   (= (add2 0) 2)
   (= (add2 1.0) 3.0)
   (= (add2 -9) -7)))

(deftest test-add5 ()
  (check
   (= (add5 5) 10)
   (= (add5 0) 5)
   (= (add5 2.5) 7.5)
   (= (add5 -1) 4)))

(deftest test-double ()
  (check
   (= (double 7) 14)
   (= (double 0) 0)
   (= (double 7.0) 14.0)))

(deftest test-min-abs4 ()
  (check
   (= (min-abs4 3 5 -2 -8) 2)))

(deftest test-max-abs4 ()
  (check
   (= (max-abs4 3 5 -2 -8) 8)))

(defun min-abs (&rest args)
  (apply #'min (mapcar #'abs args)))

(defun max-abs (&rest args)
  (apply #'max (mapcar #'abs args)))

(deftest test-min-abs ()
  (check
   (= (min-abs 3 5 -2 -8) 2)
   (= (min-abs 1) 1)
   (= (min-abs 0 0 0 0 0 0) 0)))

(deftest test-max-abs ()
  (check
   (= (max-abs 3 5 -2 -8) 8)
   (= (max-abs 1) 1)
   (= (max-abs 0 0 0 0 0 0) 0)))

;;;
;;;    2.11.3
;;;
(defun adiciu (x y)
  (+ x y))

(defun subtrahu (x y)
  (- x y))

(defun multipliku (x y)
  (* x y))

(defun dividu (x y)
  (/ x y))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
          (cl:floor (1- (* 13 m)) 5)
          y
          (cl:floor y 4)
          (cl:floor c 4)
          (- (* 2 c))
          (- (* (1+ l) (cl:floor m 11))))
       7))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (or (and (plusp x) 1)
      (and (zerop x) 0)
      (and (minusp x) -1)))

(deftest test-signum ()
  (check
   (= (signum 2.3) (cl:signum 2.3))
   (= (signum 0) (cl:signum 0))
   (= (signum -9.4) (cl:signum -9.4))))

(defun interest-rate (money)
  (or (and (<= money      0) 0)
      (and (< money    1000) 2)
      (and (< money   10000) 5)
      (and (< money  100000) 7)
                            10))

(deftest test-interest-rate ()
  (check
   (= (interest-rate -5) 0)
   (= (interest-rate 0.0) 0)
   (= (interest-rate 99) 2)
   (= (interest-rate 2438) 5)
   (= (interest-rate 86744) 7)
   (= (interest-rate 612738) 10)))

;;;
;;;    2.11.7
;;;
(defun go-to-movie-base-p (age cash)
  (or (and (< age 12) (> cash 3.0))
      (and (>= age 12) (< age 65) (> cash 7.0))
      (and (not (< age 65)) (> cash 4.5))))

(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.0))
        ((< age 65) (> cash 7.0))
        (t (> cash 4.5))))

(deftest test-go-to-movie-p ()
  (check
   (eq (go-to-movie-p -1 100) (go-to-movie-base-p -1 100))
   (eq (go-to-movie-p 10 2.5) (go-to-movie-base-p 10 2.5))
   (eq (go-to-movie-p 10 4) (go-to-movie-base-p 10 4))
   (eq (go-to-movie-p 39 4) (go-to-movie-base-p 39 4))
   (eq (go-to-movie-p 39 8) (go-to-movie-base-p 39 8))
   (eq (go-to-movie-p 65 4) (go-to-movie-base-p 65 4))
   (eq (go-to-movie-p 65 5) (go-to-movie-base-p 65 5))
   (eq (go-to-movie-p 66 5) (go-to-movie-base-p 66 5))))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (cond ((>= x 0) (truncate x))
        ((= x (truncate x)) x)
        (t (1- (truncate x)))) )

(deftest test-floor ()
  (check
   (= (floor 15.3) (cl:floor 15.3))
   (= (floor 0.0) (cl:floor 0.0))
   (= (floor -2.0) (cl:floor -2.0))
   (= (floor -2.1) (cl:floor -2.1))))

(defun ceiling (x)
  (- (floor (- x))))

(deftest test-ceiling ()
  (check
   (= (ceiling 15.3) (cl:ceiling 15.3))
   (= (ceiling 0.0) (cl:ceiling 0.0))
   (= (ceiling -2.0) (cl:ceiling -2.0))
   (= (ceiling -2.1) (cl:ceiling -2.1))))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(deftest test-leap-year-p ()
  (check
   (leap-year-p 2000)
   (leap-year-p 2004)
   (leap-year-p 1600)
   (leap-year-p 1972)
   (not (leap-year-p 1900))
   (not (leap-year-p 2009))))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))
