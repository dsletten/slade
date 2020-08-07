;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-load-toplevel.lisp
;;;;
;;;;   Started:            Sun Apr 22 12:49:33 2012
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
(defpackage :test-load-toplevel (:use :common-lisp))

(in-package :test-load-toplevel)

(eval-when (:compile-toplevel :execute)
  (defun foo (x)
    (* x 9)))

;;;
;;;    This signals UNDEFINED-FUNCTION when FASL is loaded. FOO is undefined.
;;;    In other words, the function above is not included in the object code.
;;;    
(defun bar (x)
  (foo (+ x 2)))
