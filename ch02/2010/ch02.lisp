;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Tue Jun 15 15:51:49 2010
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

(defpackage ch02 (:use common-lisp test) (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun add2 (x)
  "Add 2 to the given argument."
  (+ x 2))

(defun add5 (x)
  "Add 5 to the given argument."
  (+ x 5))

(defun double (x)
  "Double the given argument."
  (* x 2))

(defun min-abs4 (a b c d)
  "Find minimum of absolute values of 4 args."
  (min (abs a) (abs b) (abs c) (abs d)))

(defun max-abs4 (a b c d)
  "Find maximum of absolute values of 4 args."
  (max (abs a) (abs b) (abs c) (abs d)))

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
   (= (add5 1.0) 6.0)
   (= (add5 -9) -4)))

(deftest test-min-abs4 ()
  (check
   (= (min-abs4 3 5 -2 -8) 2)
   (= (min-abs4 3 5 2 8) 2)
   (= (min-abs4 -3 -5 -2 -8) 2)
   (= (min-abs4 1 1 1 1) 1)))

(deftest test-max-abs4 ()
  (check
   (= (max-abs4 3 5 -2 -8) 8)
   (= (max-abs4 3 5 2 8) 8)
   (= (max-abs4 -3 -5 -2 -8) 8)
   (= (max-abs4 1 1 1 1) 1)))

(defun min-abs (&rest args)
  (compound #'min #'abs args))

(defun max-abs (&rest args)
  (compound #'max #'abs args))

(defun compound (f1 f2 args)
  (reduce f1 (mapcar f2 args)))

(deftest test-min-abs ()
  (check
   (= (min-abs 3 5 -2 -8) 2)
   (= (min-abs 3 5 2 8) 2)
   (= (min-abs 5 -2 -8) 2)
   (= (min-abs 2 8) 2)
   (= (min-abs -3 -5 -2 -8) 2)
   (= (min-abs 1 1 1 1) 1)))

(deftest test-max-abs ()
  (check
   (= (max-abs 3 5 -2 -8) 8)
   (= (max-abs 3 5 2 8) 8)
   (= (max-abs 5 -2 -8) 8)
   (= (max-abs 2 8) 8)
   (= (max-abs -3 -5 -2 -8) 8)
   (= (max-abs 1 1 1 1) 1)))

;;;
;;;    2.11.3
;;;
;; (defun ajoutez (x y)
;;   "French addition"
;;   (+ x y))

;; (defun retranchez (x y)
;;   "French subtraction"
;;   (- x y))

;; (defun hochstmas (x y)
;;   "German maximum"
;;   (max x y))

;; (defun multiplizieren (x y)
;;   "German multiplication"
;;   (* x y))

;; (defun njia-ya-kutokea ()
;;   "Swahili exit"
;;   (sb-ext:quit))

(setf (symbol-function 'ajoutez) #'+)
(setf (symbol-function 'retranchez) #'-)
(setf (symbol-function 'hochstmas) #'max)
(setf (symbol-function 'multiplizieren) #'*)
(setf (symbol-function 'njia-ya-kutokea) #'sb-ext:quit)

(deftest test-ajoutez ()
  (check
   (= (ajoutez 2 5) (+ 2 5))))

(deftest test-retranchez ()
  (check
   (= (retranchez 7 2) (- 7 2))))

(deftest test-hochstmas ()
  (check
   (= (hochstmas 4 7) (max 4 7))))

(deftest test-multiplizieren ()
  (check
   (= (multiplizieren 5 3) (* 5 3))))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (- (+ n
             (cl:floor (1- (* 13 m)) 5)
             y
             (cl:floor y 4)
             (cl:floor c 4))
          (* 2 c)
          (* (1+ l) (cl:floor m 11)))
       7))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

(defun interest-rate (money)
  (or (and (<= money     0) 0)
      (and (< money   1000) 2)
      (and (< money  10000) 5)
      (and (< money 100000) 7)
                           10))

;;;
;;;    2.11.7
;;;
(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.00))
        ((< age 65) (> cash 7.00))
        (t (> cash 4.50))))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (if (and (minusp x) (not (integerp (rational x))))
      (- (floor (- (1- x))))
      (truncate x)))

(deftest test-floor ()
  (check
   (= (floor 8.0) (cl:floor 8.0))
   (= (floor 8.2) (cl:floor 8.2))
   (= (floor 0.0) (cl:floor 0.0))
   (= (floor -8.0) (cl:floor -8.0))
   (= (floor -8.2) (cl:floor -8.2))))

(defun ceiling (x)
  (- (floor (- x))))

;;;
;;;    This is unnecessary
;;;    
;; (defun ceiling (x)
;;   (if (minusp x)
;;       (truncate x)
;;       (- (floor (- x)))) )

(deftest test-ceiling ()
  (check
   (= (ceiling 8.0) (cl:ceiling 8.0))
   (= (ceiling 8.2) (cl:ceiling 8.2))
   (= (ceiling 0.0) (cl:ceiling 0.0))
   (= (ceiling -8.0) (cl:ceiling -8.0))
   (= (ceiling -8.2) (cl:ceiling -8.2))))

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
   (leap-year-p 1996)
   (leap-year-p 1972)
   (not (leap-year-p 1900))
   (not (leap-year-p 2009))))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (rem year 100) (if (leap-year-p year) 1 0)))