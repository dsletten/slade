;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               property-list-table.lisp
;;;;
;;;;   Started:            Sat Feb 25 14:36:07 2012
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
;;;;   Notes: This is really more like the implementation in ch. 13!!
;;;;   (Sort of... This is a plist of plists. Slade's is an alist of alists.)
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :property-list-table
  (:shadowing-import-from :collections :intersection :set :set-difference :subsetp :union)
  (:use :collections :common-lisp :lang :test))

(in-package :property-list-table)

(defclass property-list-table ()
  ((plist :reader plist :initform '())
   (name :reader name :initarg :name)))

;;;
;;;    Needed for REMOVE-PROPERTY below when top-level plist becomes empty: single-elt cons -> nil
;;;     (i.e., plists for all symbols have been cleared)
;;;     
(defgeneric (setf plist) (plist table))
(defmethod (setf plist) (plist (table property-list-table))
  (setf (slot-value table 'plist) plist))

(defun make-property-list-table (name)
  (make-instance 'property-list-table :name name))

(defmethod print-object ((table property-list-table) stream)
  (print-unreadable-object (table stream :type t)
    (format stream "~A (~D)" (name table) (length (get-symbols table)))) )

(defun property-list-table-p (obj)
  (typep obj 'property-list-table))

(defgeneric get-plist (table symbol))
(defmethod get-plist ((table property-list-table) (symbol symbol))
  (getf (plist table) symbol))

;;;
;;;    Needed for REMOVE-PROPERTY below when plist for a symbol becomes empty: single-elt cons -> nil
;;;    
(defgeneric (setf get-plist) (plist table symbol))
(defmethod (setf get-plist) (plist (table property-list-table) (symbol symbol))
  (setf (getf (plist table) symbol) plist))

(defgeneric add-property (table symbol property value))
(defmethod add-property ((table property-list-table) (symbol symbol) property value)
  (with-slots (plist) table
    (setf (getf (getf plist symbol) property) value)))

(defgeneric get-property (table symbol property))
(defmethod get-property ((table property-list-table) (symbol symbol) property)
  (getf (getf (plist table) symbol) property))

(defgeneric get-symbols (table))
(defmethod get-symbols ((table property-list-table))
  (let ((symbols (make-linked-queue)))
    (dotuples ((symbol plist) (plist table))
      (enqueue symbols symbol))
    (elements symbols)))

(defgeneric clear-property-list-table (table))
(defmethod clear-property-list-table ((table property-list-table))
  (setf (slot-value table 'plist) '())
  table)

(defgeneric pprint-plist (table symbol))
(defmethod pprint-plist ((table property-list-table) (symbol symbol))
  (format t "~A~%" symbol)
  (dotuples ((property value) (get-plist table symbol))
    (format t "~5T~A ~20T~A~%" property value)))

(defgeneric pprint-property-list-table (table))
(defmethod pprint-property-list-table ((table property-list-table))
  (dolist (symbol (get-symbols table))
    (pprint-plist table symbol)))

(defgeneric remove-property (table symbol property))
(defmethod remove-property ((table property-list-table) (symbol symbol) property)
  (remf (get-plist table symbol) property)
  (when (null (get-plist table symbol))
    (remf (plist table) symbol)))

