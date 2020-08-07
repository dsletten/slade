;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Jan 31 22:46:58 2004
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

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
	  (floor (- (* 2.6 m) 0.2))
	  y
	  (floor y 4)
	  (floor c 4)
	  (* -2 c)
	  (- (* (1+ l) (floor m 11))))
       7))


;;;
;;;    2.11.5
;;;
(defun my-signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

(defun interest-rate (money)
  (or (and (<= money 0) 0)
      (and (< money 1000) 2)
      (and (< money 10000) 5)
      (and (< money 100000) 7)
      10))

;;;
;;;    2.11.7
;;;
(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3))
	((< age 65) (> cash 7))
	(t (> cash 4.5))))

;;;
;;;    2.11.10
;;;
(defun my-floor (x)
  "Returns largest integer less than or equal to X."
  (assert (numberp x) (x) "Argument should be a number.")
  (if (minusp x)
      (if (= x (truncate x))
	  x
	  (1- (truncate x)))
      (truncate x)))

(defun my-ceiling (x)
  "Returns smallest integer greater than or equal to X."
  (assert (numberp x) (x) "Argument should be a number.")
  (if (plusp x)
      (- (my-floor (- x)))
      (truncate x)))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
	   (not (zerop (mod year 100)))) ))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (n m y)
    (mod (+ n
	  (floor (- (* 2.6 m) 0.2))
	  (mod y 100)
	  (floor (mod y 100) 4)
	  (floor (truncate y 100) 4)
	  (* -2 (truncate y 100))
	  (- (* (1+ (if (leap-year-p y) 1 0)) (floor m 11))))
       7))
