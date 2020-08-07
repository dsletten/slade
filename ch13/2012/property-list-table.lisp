;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               property-list-table.lisp
;;;;
;;;;   Started:            Sat Feb 11 18:08:08 2012
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
;;;;   Notes: This is really more like the implementation in ch. 14!!
;;;;   (Sort of... This is a hash of plists. Slade's is a hash of hashes.)
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :property-list-table (:use :common-lisp :lang :test))

(in-package :property-list-table)

(defclass property-list-table ()
  ((table :reader table :initform (make-hash-table))
   (name :reader name :initarg :name)))

(defun make-property-list-table (name)
  (make-instance 'property-list-table :name name))

(defmethod print-object ((table property-list-table) stream)
  (print-unreadable-object (table stream :type t)
    (format stream "~A (~D)" (name table) (hash-table-count (table table)))) )

(defun property-list-table-p (obj)
  (typep obj 'property-list-table))

(defgeneric get-plist (table symbol))
(defmethod get-plist ((table property-list-table) (symbol symbol))
  (gethash symbol (table table)))

(defgeneric (setf get-plist) (plist table symbol))
(defmethod (setf get-plist) (plist (table property-list-table) (symbol symbol))
  (setf (gethash symbol (table table)) plist))

(defgeneric symbols (table))
(defmethod symbols ((table property-list-table))
  (keys (table table)))

(defgeneric clear-table (table))
(defmethod clear-table ((table property-list-table))
  (clrhash (table table))
  table)

(defgeneric get-property (table symbol property))
(defmethod get-property ((table property-list-table) (symbol symbol) (property symbol))
  (getf (get-plist table symbol) property))

;;;
;;;    /Applications/LispWorks\ Personal\ 4.4.6/Library/lib/4-4-0-0/manual/online/web/CLHS/Body/05_abb.htm
;;;    
(defgeneric set-property (table symbol property value))
(defmethod set-property ((table property-list-table) (symbol symbol) (property symbol) value)
;  (let ((plist (get-plist table symbol)))
  (setf (getf (get-plist table symbol) property) value))

(defgeneric pprint-plist (table symbol))
(defmethod pprint-plist ((table property-list-table) (symbol symbol))
  (format t "~S~%" symbol)
  (dotuples ((property value) (get-plist table symbol))
    (format t "~5T~A~20T~A~%" property value)))

(defgeneric pprint-table (table))
(defmethod pprint-table ((table property-list-table))
  (dolist (symbol (symbols table))
    (pprint-plist table symbol)))

(defgeneric remove-property (table symbol property))
(defmethod remove-property ((table property-list-table) (symbol symbol) (property symbol))
  (remf (get-plist table symbol) property)
  (when (null (get-plist table symbol))
    (remhash symbol (table table))))


  