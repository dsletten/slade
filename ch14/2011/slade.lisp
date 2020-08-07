;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               slade.lisp
;;;;
;;;;   Started:            Wed Jan 26 19:25:44 2011
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
;;;;   Notes:
;;;;
;;;;

(defpackage slade (:use common-lisp))

(in-package slade)

(defclass hashtable ()
  ((capacity :accessor capacity :initarg :capacity :initform 10)
   (test :reader test :initarg :test :initform #'eql)
   (table :accessor table)))

(defmethod initialize-instance :after ((obj hashtable) &key)
  (setf (table obj) (make-array (capacity obj) :initial-element nil)))

(defmethod print-object ((obj hashtable) stream)
  (format stream "#<HASHTABLE ~S>" (table obj)))

(defun hashtablep (obj)
  (typecase obj
    (hashtable t)
    (otherwise nil)))

(defun make-hashtable (&rest args)
  (apply #'make-instance 'hashtable args))

(defun key (bucket)
  (car bucket))

(defun val (bucket)
  (cdr bucket))

(defun compression-map (h n)
  (mod n (capacity h)))

(defun get-index (h k)
  (let ((bucket (compression-map h (sxhash k)))
        (table (table h))
        (n (capacity h)))
    (do ((i bucket (next-index h i))
         (j 0 (1+ j)))
        ((= j n) (error "Hashtable full!"))
      (cond ((null (aref table i)) (return i))
            ((funcall (test h) (key (aref table i)) k) (return i)))) ))

(defun next-index (h i)
  (mod (1+ i) (capacity h)))

(defun hashtable-put (h key val)
  (setf (aref (table h) (get-index h key)) (cons key val)))

(defun hashtable-get (h key)
  (val (aref (table h) (get-index h key))))

