;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               zeller.lisp
;;;
;;;   STARTED:            Tue Feb 26 17:49:33 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

;;;
;;;    Bad implementation. No clear meaning for values.
;;;
; From ch. 02
; (defun zeller-b (day-of-month month year)
;   (let ((n day-of-month)
; 	(m month)
; 	(c (truncate year 100))
; 	(y (mod year 100))
; 	(l (if (leap-yearp year) 1 0)))
;     (mod (+ n
; 	    (floor (- (* (/ 13 5) m) 0.2))
; 	    y
; 	    (floor y 4)
; 	    (floor c 4)
; 	    (- (* 2 c))
; 	    (- (* (+ 1 l)
; 		  (floor m 11))))
; 	 7)) )

(defun leap-yearp (year)
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) )

(defun zeller (date)
  (mod (+ (car date)
	  (floor (- (* 13/5 (cadr date)) 0.2))
	  (mod (caddr date) 100)
	  (floor (mod (caddr date) 100) 4)
	  (floor (truncate (caddr date) 100) 4)
	  (- (* 2 (truncate (caddr date) 100)))
	  (- (* (1+ (if (leap-yearp (caddr date))
				    1
				    0))
		(floor (cadr date) 11))))
       7) )
