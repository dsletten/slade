;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               fractions.lisp
;;;
;;;   STARTED:            Wed Mar  6 18:53:03 2002
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

(defclass fraction ()
  ((numerator :initarg :numerator :reader get-numerator)
   (denominator :initarg :denominator :reader get-denominator))
  (:documentation "A class representing fractions.") )


(defun make-fraction (&optional (num 0) (denom 1))
  (assert (typep num 'integer)
	  (num)
	  "Numerator should be an integer.")
  (assert (typep denom 'integer)
	  (denom)
	  "Denominator should be an integer.")
  (normalize (make-instance 'fraction :numerator num :denominator denom)) )

(defmethod normalize ((f fraction))
  (let ((num (get-numerator f))
	(denom (get-denominator f)))
    (assert (and (typep num 'integer) ;Is it even possible for this to fail?!
		 (typep denom 'integer))
	    ()
	    "Numerator and denominator should be integers.")
    (if (minusp denom)
	(setf denom (- denom) num (- num)))
    (let ((common-divisor (gcd num denom)))
      (cond ((zerop common-divisor)
	     (make-instance 'fraction
			    :numerator 0
			    :denominator 1))
	    (t (make-instance 'fraction
			      :numerator (/ num common-divisor)
			      :denominator (/ denom common-divisor)))) )) )

; (defgeneric make-fraction (num denom)
;   (:method ((num number) (denom number))
; 	   (make-instance 'fraction :numerator num :denominator denom)) )

;;;
;;;   toString()
;;;   
(defmethod print-object ((obj fraction) stream)
  (if (= (get-denominator obj) 1)
      (format stream "~A" (get-numerator obj))
      (format stream "~A / ~A" (get-numerator obj) (get-denominator obj))) )

(defmethod fraction-equals ((obj1 fraction) (obj2 fraction))
  (and (= (get-numerator obj1)
	  (get-numerator obj2))
       (= (get-denominator obj1)
	  (get-denominator obj2))) )

(defmethod fraction-add ((obj1 fraction) (obj2 fraction))
  (normalize (make-fraction
	      (+ (* (get-numerator obj1) (get-denominator obj2))
		 (* (get-numerator obj2) (get-denominator obj1)))
	      (* (get-denominator obj1) (get-denominator obj2)))) )

(defmethod fraction-subtract ((f1 fraction) (f2 fraction))
  (normalize (make-fraction
	      (- (* (get-numerator f1) (get-denominator f2))
		 (* (get-numerator f2) (get-denominator f1)))
	      (* (get-denominator f1) (get-denominator f2)))) )

(defmethod fraction-multiply ((f1 fraction) (f2 fraction))
  (normalize (make-fraction (* (get-numerator f1)
			       (get-numerator f2))
			    (* (get-denominator f1)
			       (get-denominator f2)))) )

(defmethod fraction-divide ((f1 fraction) (f2 fraction))
  (normalize (fraction-multiply f1
				(fraction-reciprocal f2))) )

(defmethod fraction-reciprocal ((f fraction))
  (make-fraction (get-denominator f) (get-numerator f)) )

(defmethod fraction-abs ((f fraction))
  (make-fraction (abs (get-numerator f))
		 (abs (get-denominator f))) )

