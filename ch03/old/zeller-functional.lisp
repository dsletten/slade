#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               zeller-functional.lisp
;;;
;;;   STARTED:            Fri Jan  4 00:06:26 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Determine day of week given command-line args DAY MONTH YEAR
;;;      (All numeric). Uses King's Zeller algorithm.
;;;
;;;   CALLING SEQUENCE:
;;;   E.g., 16 July 1999
;;;   zeller 16 7 1999
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES: Functional programming style. Only side effect is FORMAT.
;;;
;;;

(defconstant days-of-week '("Saturday" "Sunday" "Monday" "Tuesday" 
			    "Wednesday" "Thursday" "Friday" "Saturday"))

(defun zeller (month day year)
  (zeller-aux-1 day
		(+ month (* 12 (+ (- 1 (truncate month 3))
				  (truncate month 6)
				  (truncate month 9))))
		year) )

;;;
;;;    This exists as a stepping stone on the way to ZELLER-AUX so that the
;;;    computation of M above need only be done once and no LET form is
;;;    needed. This does seem a bit bizarre!
;;;    
(defun zeller-aux-1 (q m year)
  (zeller-aux q
	      m
	      (mod (- year (truncate m 13)) 100)
	      (truncate (- year (truncate m 13)) 100)) )

(defun zeller-aux (q m K J)
  (mod (+ q
	  (truncate (* 26 (+ m 1)) 10)
	  K
	  (truncate K 4)
	  (truncate J 4)
	  (* 5 J))
       7) )

(defun zeller-to-king (h)
  (+ h (* 7 (truncate (- 6 h) 6))) )

(defun day-of-week (i)
  (format t "~A~%" (nth i days-of-week)) )

(day-of-week (zeller-to-king (zeller (read-from-string (second *args*))
				     (read-from-string (first *args*))
				     (read-from-string (third *args*)))) )



