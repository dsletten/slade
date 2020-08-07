;#!/usr/local/bin/clisp

;;
;   NAME:               zeller.lisp
;
;   STARTED:            010717 (010329)
;   MODIFICATIONS:
;   030703 Changed LEAP-YEARP, et al., to LEAP-YEAR-P in order to follow
;          Lisp convention.
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

;;;
;;;    2.11.4
;;;    
;
;    First attempt.
;    (Don't need to use / along with floor!)
;    
; (defun zeller (n m c y l)
;   (mod (- (+ n
; 	     (floor (- (* (/ 13 5) m)
; 		       0.2))
; 	     y
; 	     (floor (/ y 4.0))
; 	     (floor (/ c 4.0)))
; 	  (* 2 c)
; 	  (* (+ 1 l)
; 	     (floor (/ m 11.0))))
;        7) )

;;;
;;;    Function: zeller
;;;
;;;    Calculate Zeller's congruence
;;;
;;;    Input:
;;;        n - day of month
;;;        m - number of month (1 = Mar, . . ., 10 = Dec, 11 = Jan, 12 = Feb)
;;;        c - year / 100
;;;        y - year % 100
;;;        l - leap year flag (1 = yes, 0 = no)
;;;
;;;    Output:
;;;        Day of week (0 = Sunday, . . ., 6 = Saturday)
;;;        
(defun zeller (n m c y l)
  (mod (+ n
	  (floor (- (* (/ 13 5) m) 0.2))
	  y
	  (floor y 4)
	  (floor c 4)
	  (- (* 2 c))
	  (- (* (+ 1 l)
		(floor m 11))))
       7) )

(defun test-zeller ()
  (format t "10 January 1861: ~D~%" (zeller 10 11 18 61 0))
  (shell "cal 1 1861")

  (format t "31 July 1899: ~D~%" (zeller 31 5 18 99 0))
  (shell "cal 7 1899")

  (format t "28 February 1900: ~D~%" (zeller 28 12 19 00 0))
  (shell "cal 2 1900")

  (format t "1 March 1900: ~D~%" (zeller 1 1 19 00 0))
  (shell "cal 3 1900")

  (format t "29 February 1904: ~D~%" (zeller 29 12 19 04 1))
  (shell "cal 2 1904")

  (format t "1 March 1904: ~D~%" (zeller 1 1 19 04 1))
  (shell "cal 3 1904")

  (format t "16 July 1999: ~D~%" (zeller 16 5 19 99 0))
  (shell "cal 7 1999")

  (format t "31 December 1999: ~D~%" (zeller 31 10 19 99 0))
  (shell "cal 12 1999")

  (format t "29 February 2000: ~D~%" (zeller 29 12 20 00 1))
  (shell "cal 2 2000")

  (format t "1 March 2000: ~D~%" (zeller 1 1 20 00 1))
  (shell "cal 3 2000") )

;;;
;;;    2.11.11
;;;    
(defun leap-year-p (year)
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) )    ;We only get here if above clause is
				       ; false i.e., (mod year 100) is not 0.
				       ; This is why we need the '&&' below.

;;;
;;;    Not nearly as slick as above
;;;
(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
	   (not (zerop (mod year 100)))) ))

(defun test-leap-year-p ()
  (dolist (year '((1600 "")
		  (1700 "not ")
		  (1800 "not ")
		  (1900 "not ")
		  (1901 "not ")
		  (1902 "not ")
		  (1903 "not ")
		  (1904 "")
		  (1920 "")
		  (1970 "not ")
		  (1972 "")
		  (2000 "")
		  (2001 "not ")))
    (format t "~D is ~Aa leap year: ~A~%" (car year) (cadr year)
	    (leap-year-p (car year)))) )

;;;In Perl:
;;;if ( ($year % 400 == 0)  ||  ($year % 100 != 0)  ||  ($year % 4 == 0) ) {...
;;;WRONG!
;;;   This is correct:
;;;if ( ($year % 400 == 0)  ||  (($year % 100 != 0)  &&  ($year % 4 == 0)) ) {...
;;;   King's Java version:
;;;if ( (year % 4 == 0)  &&  ((year % 100 != 0) || (year % 400 == 0)) ) {...

;;;
;;;    2.11.12
;;;
(defun zeller-a (n m y)
  (mod (+ n
	  (floor (- (* (/ 13 5) m) 0.2))
	  (mod y 100)
	  (floor (mod y 100) 4)
	  (floor (truncate y 100) 4)
	  (- (* 2 (truncate y 100)))
	  (- (* (+ 1 (if (leap-year-p y)
			 1
		         0))
		(floor m 11))))
       7) )

;;;
;;;    Here's a cheap way!
;;;    
(defun son-of-zeller-a (n m y)
  (zeller n 
	  m 
	  (floor y 100) 
	  (mod y 100) 
	  (if (leap-year-p y)
	      1
	      0)) )

(defun zeller-b (day-of-month month year)
  (let ((n day-of-month)
	(m month)
	(c (truncate year 100))
	(y (mod year 100))
	(l (if (leap-year-p year) 1 0)))
    (mod (+ n
	    (floor (- (* (/ 13 5) m) 0.2))
	    y
	    (floor y 4)
	    (floor c 4)
	    (- (* 2 c))
	    (- (* (+ 1 l)
		  (floor m 11))))
	 7)) )

