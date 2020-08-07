;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Thu Sep 30 01:45:36 2010
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
;;;    The results of TYPE-OF are sufficiently implementation-dependent
;;;    that this function is worthless.
;;;    
(defun check-arg-type (arg)
  (let (result
        (type (type-of arg)))
    (tagbody
     (cond ((eq type 'fixnum) (go fixnum))
           ((eq type 'bignum) (go bignum))
           ((eq type 'string) (go string))
           ((eq type 'character) (go character))
           ((eq type 'symbol) (go symbol))
           ((eq type 'complex) (go complex))
           ((eq type 'float) (go float))
           ((eq type 'single-float) (go single-float))
           ((eq type 'rational) (go rational))
           ((eq type 'function) (go function))
           ((eq type 'sequence) (go sequence))
           ((eq type 'list) (go list))
           ((eq type 'cons) (go cons)))
       (setf result 'who-knows)
       (go finish)
       fixnum bignum
       (setf result 'integer)
       (go finish)
       string character symbol
       (setf result 'text)
       (go finish)
       complex float single-float rational
       (setf result 'non-integer)
       (go finish)
       function
       (setf result 'code)
       (go finish)
       sequence list cons
       (setf result 'collection)
       finish)
    result))
           
(defun check-arg-type (arg)
  (typecase arg
    ((or fixnum bignum) 'integer)
    ((or string character symbol) 'text)
    ((or complex float rational) 'non-integer)
    (function 'code)
    ((or list cons) 'list)
    (sequence 'like-a-list)
    (otherwise 'who-knows)))