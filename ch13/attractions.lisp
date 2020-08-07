;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               attractions.lisp
;;;
;;;   STARTED:            Fri Mar  8 00:34:57 2002
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
;;;   NOTES: Based on thread from Winston's 'On to Java'
;;;
;;;

;;;
;;;    It's really annoying that a :writer is called with the args reversed!
;;;    (set-minutes 90 <object>) not (set-minutes <object> 90)
;;;    
(defclass attraction ()
  ((minutes :initarg :minutes :initform 75 :accessor attraction-minutes)) )

(defun make-attraction (&optional minutes)
  (if minutes
      (make-instance 'attraction :minutes minutes)
      (make-instance 'attraction)) )

(defclass movie (attraction)
  ((script :initarg :script :initform 5)
   (acting :initarg :acting :initform 5)
   (direction :initarg :direction :initform 5)) )

;;;
;;;    This sucks...Is there a better way?
;;;    
; (defun make-movie (&optional (script 5) (acting 5) (direction 5) minutes)
;   (if minutes
;       (make-instance 'movie
; 		     :script script :acting acting
; 		     :direction direction :minutes minutes)
;       (make-instance 'movie
; 		     :script script :acting acting :direction direction)) )
;;;
;;;   Mega-constructor !!!!!!????
;;;   (Includes 0-arg constructor)
;;;   
(defun make-movie (&rest args)
  (cond (args
	 (multiple-value-bind (script acting direction minutes)
	                      (values-list args)
	   (if minutes
	       (make-instance 'movie
			      :script script :acting acting
			      :direction direction :minutes minutes)
	       (make-instance 'movie
			      :script script :acting acting
			      :direction direction))))
	(t (make-instance 'movie))) )

;;;
;;;    MAKE-INSTANCE is not a function!
;;;    
; (defun make-movie (&rest args)
;   (cond (args
; 	 (multiple-value-bind (script acting direction minutes)
; 	                      (values-list args)
; 	   (apply #'make-instance 'movie
; 		  :script script :acting acting
; 		  :direction direction (and minutes
; 					    (list :minutes minutes)))) )
; 	(t (make-instance 'movie))) )
      
; (defgeneric make-movie (&rest args)
;   (:method (args (eql nil))
; 	   (make-instance 'movie))
;   (:method (&rest args)
; 	   (multiple-value-bind (script acting direction minutes)
; 	                        (values-list args)
; 	     (if minutes
; 		 (make-instance 'movie
; 				:script script :acting acting
; 				:direction direction :minutes minutes)
; 		 (make-instance 'movie
; 				:script script :acting acting
; 				:direction direction)))) )


(defmethod initialize-instance :after ((m movie) &rest args)
	   (format t "~S: ~A" m args) )
