;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch03p.lisp
;;;;
;;;;   Started:            Thu Sep  9 10:34:17 2004
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

(defpackage ch03p (:use common-lisp))

(in-package ch03p)

;;;
;;;    3.14.4
;;;
(defun no-zeros (l)
  (remove 0 l))

(defun collect-numbers (x l)
  (if (numberp x)
      (cons x l)
      l))

(let ((verb-list '(is am are have has go went gone)))
  (defun verb-find (sentence)
    (remove-if-not #'(lambda (word)
		       (member word verb-list))
		   sentence)))

;;;
;;;    3.14.6
;;;
(defun last-atom (l)
  "Returns the last atom in a list."
  (let ((last-cons (last l)))
    (if (null (cdr last-cons))
	(car last-cons)
	(cdr last-cons))))

;;;
;;;    3.14.11
;;;
(let ((zeller-months '((march . 1)
		       (april . 2)
		       (may . 3)
		       (june . 4)
		       (july . 5)
		       (august . 6)
		       (september . 7)
		       (october . 8)
		       (november. 9)
		       (december. 10)
		       (january . 11)
		       (february . 12)))
      (days-of-week
       '(sunday monday tuesday wednesday thursday friday saturday)))
  (defun zeller (month day year)
    (let ((month* (cdr (assoc month zeller-months))))
      (nth (mod (+ day
		   (floor (- (* 13/5 month*) 0.2))
		   (mod year 100)
		   (floor (mod year 100) 4)
		   (floor (floor year 100) 4)
		   (* -2 (floor year 100))
		   (- (* (if (leap-year-p year) 2 1) (floor month* 11))))
		7) days-of-week))))

(defun leap-year-p (y)
  (cond ((zerop (mod y 400)))
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )
