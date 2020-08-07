;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               trains.lisp
;;;
;;;   STARTED:            Wed Mar  6 18:00:10 2002
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

(defclass railroad-car ()
  ((weight :initarg :weight :reader get-weight :writer set-weight)
   (year :initarg :year :reader get-year)) )

(defclass box-car (railroad-car)
  ((length :initarg :length :reader get-length)
   (height :initarg :height :reader get-height)
   (width :initarg :width :reader get-width))
  (:documentation "A box car from a train.") )

(defgeneric volume (obj)
  (:method ((obj box-car))
	   (* (get-length obj)
	      (get-height obj)
	      (get-width obj))) )

(defun make-box-car (&key (length 20) (height 10) (width 9) (year 1900)
		     (weight 12000))
  (make-instance 'box-car :length length :height height :width width
		 :year year :weight weight) )

(setf b1 (make-instance 'box-car :length 20 :height 10 :width 9))
(setf b2 (make-instance 'box-car :length 20 :height 10 :width 9 :year 1950
			:weight 18000))

(setf b3 (make-box-car))

(set-weight 21000 b2) ;!!