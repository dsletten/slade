;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Tue Nov 23 11:41:41 2004
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

(defpackage ch09 (:use common-lisp))

(in-package ch09)

;;;
;;;    9.12.2
;;;    Ha!!
;;;
(defun i-average (l)
  (loop for elt in l
	counting elt into count
	summing elt into sum
	finally (return (/ sum count))))

;;;
;;;    9.12.3
;;;
(defun remove-pairs-list (list)
  (let ((prev (car list))
	(result (list (car list))))
    (dolist (this (cdr list) (nreverse result))
      (unless (eql this prev)
	(push this result))
      (setf prev this))))

(defun remove-pairs (string)
  (coerce (remove-pairs-list (coerce string 'list)) 'string))
  