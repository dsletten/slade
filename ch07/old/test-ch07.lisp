;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               test-ch07.lisp
;;;
;;;   STARTED:            Mon Nov 26 23:04:45 2001
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;
(load "/Users/dsletten/lisp/programs/utils.lisp")
(load "ch07.lisp")

(test 'column-print `(((("Is" "this" "not" "pung?") 5 ,*standard-output*)
		     ,"     Is
     this
     not
     pung?")))