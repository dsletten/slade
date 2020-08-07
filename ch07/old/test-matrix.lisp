;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               test-matrix.lisp
;;;
;;;   STARTED:            Sat Dec  1 00:33:33 2001
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
(load "matrix.lisp")

(test 'make-matrix '((((1 2 3 4 5 6) 2) ((1 2) (3 4) (5 6)))
		     (((1 2 3 4 5 6) 3) ((1 2 3) (4 5 6)))
		     (((a b c d e) 3) ((a b c) (d e)))
		     (((1 2 3 4 5 6 7 8 9 10 11 12) 2) ((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
		     (((1 2 3 4 5 6 7 8 9 10 11 12) 3) ((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
		     (((1 2 3 4 5 6 7 8 9 10 11 12) 4) ((1 2 3 4) (5 6 7 8) (9 10 11 12)))
		     (((1 2 3 4 5 6 7 8 9 10 11 12) 6) ((1 2 3 4 5 6) (7 8 9 10 11 12)))
		     (((1 2 3 4 5 6 7 8 9 10 11 12) 5) ((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )


