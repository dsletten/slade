;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               queue.lisp
;;;;
;;;;   Started:            Wed Feb 15 10:03:40 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes: Inefficient naive queue using APPEND.
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :queue-2 (:use :common-lisp :test))

(in-package :queue-2)

(defclass queue ()
  ((elements :initform '())))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "(~D)" (length (slot-value q 'elements)))) )

(defun queuep (obj)
  (typep obj 'queue))

(defgeneric emptyp (queue))
(defmethod emptyp ((q queue))
  (null (slot-value q 'elements)))

(defgeneric enqueue (queue elt))
(defmethod enqueue ((q queue) elt)
  (with-slots (elements) q
    (setf elements (append elements (list elt)))) )
;  (nconc (slot-value q 'elements) (list elt)))

(defgeneric dequeue (queue))
(defmethod dequeue ((q queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (pop (slot-value q 'elements))))

(defgeneric head (queue))
(defmethod head ((q queue))
  (first (slot-value q 'elements)))
