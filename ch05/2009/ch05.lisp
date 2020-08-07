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
;;;;   Started:            Thu Mar  5 01:02:19 2009
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

(defpackage ch05 (:use common-lisp) (:shadow last))

(in-package ch05)

;;;
;;;    5.7.7
;;;
(let ((args '())
      (i 0))
  (defun a+ (x y)
    (incf i)
    (push (list x y) args)
    (+ x y))
  (defun count+ ()
    (prog1 i (setf i 0)))
  (defun args+ ()
    (prog1 args (setf args '()))) )

;;;
;;;    5.7.9
;;;
;; (defun (setf last) (cons list)
;;   (

;;;
;;;    This doesn't work for single-element list. We aren't holding onto the variable
;;;    whose referent is LIST, so we can't reassign it simply by returning CONS.
(defun set-last (list cons)
  (cond ((endp list) '()) ; Initial LIST is empty
        ((endp (rest list)) cons) ; Initial LIST is single-element list
        ((endp (cddr list)) (setf (cdr list) cons))
        (t (set-last (rest list) cons))))

;;;
;;;    Now we can let the recursion proceed to the final CONS in any case...
;;;    
(defun set-last (list cons)
  (cond ((endp list) '()) ; Initial LIST is empty
        ((endp (rest list)) (setf (car list) (car cons)
                                  (cdr list) (cdr cons))) ; Initial LIST is single-element list
        (t (set-last (rest list) cons))))

(defun last (l)
  (cl:last l))
;;;
;;;    SBCL won't allow this w/o shadowing...
;;;    Clozure doesn't complain.
;;;    
(defsetf last set-last)