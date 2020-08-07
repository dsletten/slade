;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch02p.lisp
;;;;
;;;;   Started:            Thu Sep  2 12:53:50 2004
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

(defpackage ch02p (:use common-lisp common-lisp-user)
	    (:shadow signum floor ceiling))

(in-package ch02p)

;;;
;;;    2.11.4
;;;    
(defun zeller (n m c y l)
  (mod (+ n
	  (floor (- (* 13/5 m) 0.2))
	  y
	  (floor y 4)
	  (floor c 4)
	  (* -2 c)
	  (- (* (1+ l) (floor m 11))))
       7))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

(defun signum (x)
  "My signum function."
  (or (and (zerop x) x)
      (/ (abs x) x)))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (cond ((integerp x) x)
	((>= x 0) (truncate x))
	(t (truncate (1- x)))) )

(defun ceiling (x)
  (cond ((integerp x) x)
	((<= x 0) (truncate x))
	(t (truncate (1+ x)))) )

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)) t)
	((zerop (mod y 100)) nil)
	((zerop (mod y 4)) t)
	(t nil)))

;;;
;;;    Minimalist
;;;    
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)))
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )

;;;
;;;    Not quite so terse.
;;;    
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)) t)
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )

;; (defun leap-year-p (y)
;;   (dolist (l '((400 t) (100 nil) (4 t)))
;;     (when (zerop (mod y (first l)))
;;       (return-from leap-year-p (second l))))
;;   nil)

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day month* year)
  (mod (+ day
	  (floor (- (* 13/5 month*) 0.2))
	  (mod year 100)
	  (floor (mod year 100) 4)
	  (floor (floor year 100) 4)
	  (* -2 (floor year 100))
	  (- (* (if (leap-year-p year) 2 1) (floor month* 11))))
       7))
