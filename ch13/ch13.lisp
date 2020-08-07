;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch13.lisp
;;;
;;;   STARTED:            Sat Mar  9 03:59:01 2002
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

;;;
;;;    13.10.1
;;;
(defmacro defpredicate (class)
  `(defgeneric ,(read-from-string (format nil "~A-p" class)) (obj)
     (:method ((obj ,class)) t)
     (:method (obj) nil)) )

;;;
;;;    13.10.4
;;;
(defclass queue ()
  ((pipeline :initform nil :accessor queue-pipeline)) )

(defgeneric queuep (obj)
  (:method ((obj queue)) t)
  (:method (obj) nil) )

(defun queue-empty-p (q)
  (null (queue-pipeline q)) )

(defmethod enqueue ((q queue) obj)
;  (rplacd (last (queue-pipeline q)) (list obj)) )
  (setf (queue-pipeline q) (reverse (cons obj (reverse (queue-pipeline q)))) ) )

(defmethod dequeue ((q queue))
  (pop (queue-pipeline q)) )

(defmethod q-head ((q queue))
  (car (queue-pipeline q)) )

(defmethod print-object ((q queue) stream)
  (format stream "#<Queue ~A>" (queue-pipeline q)) )

