;#!/usr/local/bin/clisp

;;
;   NAME:               ch02.lisp
;
;   STARTED:            010717 (010330)
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
;;;    2.11.5
;;;
;;;(defun signum (x)
;;;  (cond ((plusp x) 1)
;;;	((minusp x) -1)
;;;	((zerop x) 0)) )
(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)) )

;;;(defun interest-rate (money)
;;;  (cond ((<= money 0) 0)
;;;	((< money 1000) 2)
;;;	((< money 10000) 5)
;;;	((< money 100000) 7)
;;;	(t 10)) )
(defun interest-rate (money)
  (or (and (<= money 0) 0)
      (and (< money 1000) 2)
      (and (< money 10000) 5)
      (and (< money 100000) 7)
      10) )

;;;
;;;    2.11.6
;;;
;;;    This version returns a value of the same numeric type as x
;;;    whereas the one above always returns an integer.
;;;
(defun signum-1 (x)
  (if (zerop x)
      x
      (/ x (abs x))) )

;;;
;;;    2.11.7
;;;
;;; (defun go-to-moviep (age cash)
;;;   (or (and (< age 12)
;;; 	   (> cash 3.00))
;;;       (and (>= age 12)   ;This predicate is not necessary. ;; True, but _only_ because the "Adult" price is greater than the "Child" price.
;;; 	   (< age 65)
;;; 	   (> cash 7.00))
;;;       (and (not (< age 65))
;;; 	   (> cash 4.50))) )
(defun go-to-moviep (age cash)
  (cond ((< age 12) (> cash 3.00))
	((< age 65) (> cash 7.00))
	(t (> cash 4.50))) )

;;;
;;;    2.11.10
;;;
;;;    These versions represent my attempt to be extra slick.
;;;    Little did I realize that despite their 'elegance', they
;;;    in fact resulted in an infinite recursion. Doh!
;;;    
; (defun my-floor (x)
;   (cond ((minusp x) (- (my-ceiling (- x))))
; 	(t (truncate x))) )

; (defun my-ceiling (x)
;   (cond ((plusp x) (- (my-floor (- x))))
; 	(t (truncate x))) )

;;;
;;;    These two work.
;;;    
; (defun my-floor (x)
;   (cond ((minusp x) (if (= x (truncate x))
; 			x
; 		        (1- (truncate x))))
; 	(t (truncate x))) )

; (defun my-ceiling (x)
;   (cond ((plusp x) (if (= x (truncate x))
; 		       x
; 		       (1+ (truncate x))))
; 	(t (truncate x))) )

;;;
;;;    More concise.
;;;    (These also return a single value in all cases as a result
;;;     of the +.)
;;;
;;;     Function: my-floor
;;;
;;;     Computes 'floor' function, returning greatest integer
;;;     less than or equal to input value.
;;;
;;;     Input:
;;;         x - any real number
;;;         
;;;     Output:
;;;         The 'floor' of x:
;;;           -If x is nonnegative, merely truncate x.
;;;           -If x is negative but already an integer, merely truncate x.
;;;           -Otherwise, truncate x and subtract 1.
;;;     (Cannot test whether x is an integer using INTEGERP, since arbitrary
;;;      floating-point arguments are accepted. E.g., (integerp 2.0) => NIL)
;;;         
(defun my-floor (x)
  (cond ((and (minusp x)
	      (/= x (truncate x)))
	 (1- (truncate x)))
	(t (+ (truncate x)))) )

;;;
;;;    Even clearer
;;;
(defun my-floor (x)
  (if (and (minusp x) (/= x (truncate x)))
      (1- (truncate x))
      (+ (truncate x))))

;;;
;;;    Maybe not clearer...
;;;
(defun my-floor (x)
  (truncate (if (and (minusp x) (/= x (truncate x)))
		(1- x)
		x)))

;;;
;;;    Recursive!
;;;    
(defun my-floor (x)
  (if (and (minusp x) (/= x (truncate x)))
      (1- (- (my-floor (- x)))))
      (+ (truncate x))))

(defun test-my-floor ()
  (dolist (x '(-2.9 -1.5 -1.4 -2.0 -2 -0.1 0 0.2 0.9 1 1.0 1.8 2.2))
    (format t "x = ~A my-floor: ~A floor: ~A~%" x (my-floor x) (floor x))) )
;;;
;;;     Function: my-ceiling
;;;
;;;     Computes 'ceiling' function, returning smallest integer
;;;     greater than or equal to input value.
;;;
;;;     Input:
;;;         x - any real number
;;;         
;;;     Output:
;;;         The 'ceiling' of x:
;;;           -If x is nonpositive, merely truncate x.
;;;           -If x is positive but already an integer, merely truncate x.
;;;           -Otherwise, truncate x and add 1.
;;;         
(defun my-ceiling (x)
  (cond ((and (plusp x)
	      (/= x (truncate x)))
	 (1+ (truncate x)))
	(t (+ (truncate x)))) )

(defun test-my-ceiling ()
  (dolist (x '(-2.9 -1.5 -1.4 -2.0 -2 -0.1 0 0.2 0.9 1 1.0 1.8 2.2))
    (format t "x = ~A my-ceiling: ~A ceiling: ~A~%" x (my-ceiling x) (ceiling x))) )
;;;
;;;    2.11.11, 2.11.12
;;;
;;;    See zeller.lsp
;;;
