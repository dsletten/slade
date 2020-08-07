;#!/usr/local/bin/clisp

;;
;   NAME:               zeller.lisp
;
;   STARTED:            010914
;   MODIFICATIONS:
;
;   PURPOSE:    This file builds on the version created in ch 2 (namely
;               zeller-b).
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
;;;    Function: zeller
;;;
;;;    Calculate Zeller's congruence
;;;
;;;    Input:
;;;        month - name of month (as a symbol)
;;;        day of month - 1-31
;;;        year - assumed >= 1582
;;;
;;;    Output:
;;;        Day of week as a symbol
;;;
(let ((months       '(march april may june july august september
		      october november december january february))
      (days-of-week '(sunday monday tuesday wednesday thursday friday saturday)))
  
  (defun zeller (month day-of-month year)
    (let ((n day-of-month)
	  (m (month-number month months))
	  (c (truncate year 100))
	  (y (mod year 100))
	  (l (if (leap-yearp year) 1 0)))
      (nth (mod (+ n
		   (floor (- (* (/ 13 5) m) 0.2))
		   y
		   (floor y 4)
		   (floor c 4)
		   (- (* 2 c))
		   (- (* (+ 1 l)
			 (floor m 11))))
		7) days-of-week)) )

  (defun month-number (month month-list)
    (cond ((eq month (car month-list)) 1)
	  (t (1+ (month-number month (cdr month-list)))) ) )

  (defun test-month-number ()
    (format t "March is month 1 => ~A~%" (month-number 'march months))
    (format t "October is month 8 => ~A~%" (month-number 'october months))
    (format t "February is month 12 => ~A~%" (month-number 'february months)) ) )

(defun test-zeller ()
  (format t "10 January 1861: ~D~%" (zeller 'january 10 1861))
  (shell "cal 1 1861")

  (format t "31 July 1899: ~D~%" (zeller 'july 31 1899))
  (shell "cal 7 1899")

  (format t "28 February 1900: ~D~%" (zeller 'february 28 1900))
  (shell "cal 2 1900")

  (format t "1 March 1900: ~D~%" (zeller 'march 1 1900))
  (shell "cal 3 1900")

  (format t "29 February 1904: ~D~%" (zeller 'february 29 1904))
  (shell "cal 2 1904")

  (format t "1 March 1904: ~D~%" (zeller 'march 1 1904))
  (shell "cal 3 1904")

  (format t "16 July 1999: ~D~%" (zeller 'july 16 1999))
  (shell "cal 7 1999")

  (format t "31 December 1999: ~D~%" (zeller 'december 31 1999))
  (shell "cal 12 1999")

  (format t "29 February 2000: ~D~%" (zeller 'february 29 2000))
  (shell "cal 2 2000")

  (format t "1 March 2000: ~D~%" (zeller 'march 1 2000))
  (shell "cal 3 2000") )

;;;
;;;    2.11.11
;;;    
(defun leap-yearp (year)
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) )    ;We only get here if above clause is false
				       ; i.e., (mod year 100) is not 0. This is
				       ; why we need the '&&' below.

(defun test-leap-yearp ()
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
	    (leap-yearp (car year)))) )

