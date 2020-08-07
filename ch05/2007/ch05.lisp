;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Sun Jul 15 18:39:26 2007
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

(defpackage ch05 (:use common-lisp))

(in-package ch05)

;;;
;;;    5.7.7
;;;
(let ((store '())
      (count 0))
  (defun a+ (x y)
    (push (list x y) store)
    (incf count)
    (+ x y))
  (defun count+ ()
    (let ((i count))
      (setf count 0)
      i))
  (defun args+ ()
    (let ((s store))
      (setf store '())
      s)))

;;;
;;;    5.7.8
;;;
(defun make-date (month day year)
  (cons month (cons day (cons year '()))))

(defun date-month (date) (car date))

(defun date-day (date) (cadr date))

(defun date-year (date) (caddr date))

(defun (setf date-month) (month date)
  (setf (car date) month))

(defun set-day (date day)
  (setf (cadr date) day))

(defsetf date-day set-day)

(defsetf date-year (date) (year)
  `(setf (caddr ,date) ,year))

