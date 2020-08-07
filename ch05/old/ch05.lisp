;#!/usr/local/bin/clisp

;;
;   NAME:               ch05.lisp
;
;   STARTED:            011001
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

;;;
;;;    5.7.5
;;;
;; Original
; (defun make-change (money)
;   (cond ((= money 0) nil)
; 	((< money 5)   (cons (list money (if (> money 1)
; 					     'pennies
; 					     'penny))
; 			     (make-change (mod money 1))))
; 	((< money 10)  (cons (list (truncate money 5) 'nickel)
; 			     (make-change (mod money 5))))
; 	((< money 25)  (let ((n-dimes (truncate money 10)))
; 			 (cons (list n-dimes (if (> n-dimes 1)
; 						 'dimes
; 						 'dime))
; 			     (make-change (mod money 10)))) )
; 	((< money 50)  (cons (list (truncate money 25) 'quarter)
; 			     (make-change (mod money 25))))
; 	((< money 100) (cons (list (truncate money 50) 'half-dollar)
; 			     (make-change (mod money 50))))
; 	(t             (let ((n-dollars (truncate money 100)))
; 			 (cons (list n-dollars (if (> n-dollars 1)
; 						   'dollars
; 						   'dollar))
; 			     (make-change (mod money 100)))) )) )

(define-modify-macro modf (divisor) mod)

(defun make-change (money)
  (let ((change-list ()))
    (cond ((>= money 100)
	   (let ((n-coins (truncate money 100)))
	     (modf money 100)
	     (push (list n-coins
			 (or (and (> n-coins 1)
				  'dollars)
			     'dollar))
		   change-list))))
    (cond ((>= money 50)
	   (modf money 50)
	   (push (list 1 'half-dollar) change-list)))
    (cond ((>= money 25)
	   (modf money 25)
	   (push (list 1 'quarter) change-list)))
    (cond ((>= money 10)
	   (let ((n-coins (truncate money 10)))
	     (modf money 10)
	     (push (list n-coins
			 (or (and (> n-coins 1)
				  'dimes)
			     'dime))
		   change-list))))
    (cond ((>= money 5)
	   (modf money 5)
	   (push (list 1 'nickel) change-list)))
    (cond ((> money 0)
	   (push (list money
		       (or (and (> money 1)
				'pennies)
			   'penny))
		 change-list)))
    change-list) )


;;;
;;;    5.7.6
;;;    
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


(defun leap-yearp (year)
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) )


(let ((days-of-week '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
		      "Friday" "Saturday"))
      (months '((january 11)
		(february 12)
		(march 1)
		(april 2)
		(may 3)
		(june 4)
		(july 5)
		(august 6)
		(september 7)
		(october 8)
		(november 9)
		(december 10))))
  (defun zeller (month day-of-month year)
    (let ((n day-of-month)
	  (m (second (assoc month months)))
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
		7) days-of-week)) ))

;;;
;;;    5.7.7
;;;
(let ((count 0)
      (args ()))
  (defun a+ (x y)
    (incf count)
    (push (list x y) args)
    (+ x y) )

  (defun count+ ()
    (prog1 count (setf count 0)) )

  (defun args+ ()
    (prog1 args (setf args nil)) ))


;;;
;;;    5.7.8
;;;
(defun make-date (month day year)
  (cons month (cons day (cons year ()))) )

(defun date-month (date)
  (car date) )

(defun date-day (date)
  (cadr date) )

(defun date-year (date)
  (caddr date) )

(defun (setf date-month) (new-month date)
  (setf (car date) new-month) )

(defun set-day (date new-day)
  (setf (cadr date) new-day) )

(defsetf date-day set-day)

(defsetf date-year (date) (new-year)
  `(setf (caddr ,date) ,new-year) )

;;;
;;;    5.7.9
;;;    (See Slade's solution)
;;;
(defun (setf last) (new-last l)
  (rplacd (nthcdr (- (length l) 2) l) new-last) )
