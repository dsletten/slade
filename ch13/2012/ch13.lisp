;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch13.lisp
;;;;
;;;;   Started:            Tue Feb 14 20:12:29 2012
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch13 (:use :common-lisp :test))

(in-package :ch13)

;;;
;;;    13.10.1
;;;
(defmacro defpredicate (class-name)
  (let ((predicate-name (make-predicate-name class-name)))
    `(defun ,predicate-name (obj)
       (typep obj ',class-name))))

(defun make-predicate-name (symbol)
  (let ((name (symbol-name symbol)))
    (if (find #\- name)
        (intern (concatenate 'string name "-P"))
        (intern (concatenate 'string name "P")))) )
        