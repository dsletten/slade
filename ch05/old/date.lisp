;#!/usr/local/bin/clisp

;;
;   NAME:               date.lisp
;
;   STARTED:            020104
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
(defun make-date (month day year)
  (list month day year) )

;(defun make-date (month day year)
;  `(,month ,day ,year) ) <-- This is different than above?

(defun date-month (date)
  (first date) )

(defun date-day (date)
  (second date) )

(defun date-year (date)
  (third date) )

;;;
;;;    Type 1
;;;    
(defun (setf date-month) (new-month date)
  (setf (first date) new-month) )

;;;
;;;    Type 2
;;;
(defun set-day (date new-day)   ;<-- This guy is independently accessible!
  (setf (second date) new-day) )

(defsetf date-day set-day)

;;;
;;;    Type 3
;;;
(defsetf date-year (date) (new-year)
  `(setf (third ,date) ,new-year) )
