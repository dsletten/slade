;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               multi-column-print.lisp
;;;;
;;;;   Started:            Tue Nov  1 21:18:12 2011
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
(load "/Users/dsletten/lisp/packages/matrix.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :multi-column-print (:use :common-lisp :matrix :test))

(in-package :multi-column-print)

(defun multi-column-print (l &key (columns 1) (width (truncate 80 columns)) (stream *standard-output*) (indent 0) (align :left))
  (let ((*standard-output* stream))
    (print-2d-array (seq-to-array l :columns columns :direction :by-column) :width width :align align :indent indent)))
