;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch13.lisp
;;;;
;;;;   Started:            Tue Feb  1 23:29:32 2011
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

(defpackage ch13 (:use common-lisp))

(in-package ch13)

(defclass symbol-generator ()
  ((prefix :reader prefix :initarg :prefix :initform "G")
   (counter :accessor counter :initform 0)
   (symbols :accessor symbols :initform '())))

(defun symbol-generator-p (obj)
  (typep obj 'symbol-generator))

(defmethod print-object ((obj symbol-generator) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~D" (prefix obj) (counter obj))))

(defun make-symbol-generator (&rest args)
  (apply #'make-instance 'symbol-generator args))

(defgeneric generate-symbol (obj))

(defmethod generate-symbol ((sg symbol-generator))
  (let ((sym (make-symbol (format nil "~A~D" (prefix sg) (counter sg)))) )
    (push sym (symbols sg))
    (incf (counter sg))
    sym))

(defmethod previous-symbol ((sg symbol-generator))
  (first (symbols sg)))

(defmethod reset ((sg symbol-generator))
  (setf (symbols sg) '()
        (counter sg) 0))

(defclass property-list ()
  ((plist :accessor plist :initform (empty-plist))
   (name :reader name :initarg :name)))

(defun empty-plist ()
  (cons (cons '() '()) '()))

(defun make-plist (name)
  (make-instance 'property-list :name name))

(defun property-list-p (obj)
  (typep obj 'property-list))

(defmethod print-object ((obj property-list) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (name obj))))

(defmethod symbol-lookup ((obj property-list) (sym symbol))
  (let ((