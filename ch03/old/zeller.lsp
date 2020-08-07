;#!/usr/local/bin/clisp

;;
;   NAME:               zeller.lsp
;
;   STARTED:            010329
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;
(defun zeller (n m c y l)
  (mod (- (+ n
	     (floor (- (* (/ 13 5) m)
		       0.2))
	     y
	     (floor (/ y 4.0))
	     (floor (/ c 4.0)))
	  (* 2 c)
	  (* (+ 1 l)
	     (floor (/ m 11.0))))
       7) )

(defun leap-yearp (year)
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) )

;;;In Perl:
;;;if ( ($year % 400 == 0)  ||  ($year % 100 != 0)  ||  ($year % 4 == 0) ) {...

(defun son-of-zeller-a (n m y)
  (zeller n 
	  m 
	  (floor (/ y 100)) 
	  (mod y 100) 
	  (if (leap-yearp y)
	      1
	      0)) )

(defun son-of-zeller-b (n m y)
  (mod (- (+ n
	     (floor (- (* (/ 13 5) m)
		       0.2))
	     (mod y 100)
	     (floor (/ (mod y 100) 4))
	     (floor (/ (floor (/ y 100)) 4)))
	  (* 2 (floor (/ y 100)))
	  (* (1+ (if (leap-yearp y)
		     1
		     0))
	     (floor (/ m 11))))
       7) )

(defun son-of-zeller-c (n m y)
  (let ((cent (floor (/ y 100)))
	(year (mod y 100))
	(leap (if (leap-yearp y) 1 0)))
    (mod (- (+ n
	       (floor (- (* (/ 13 5) m)
			 0.2))
	       year
	       (floor (/ year 4))
	       (floor (/ cent 4)))
	    (* 2 cent)
	    (* (1+ leap) (floor (/ m 11))))
	 7)) )
	       