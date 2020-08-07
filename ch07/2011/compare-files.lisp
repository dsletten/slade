;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               compare-files.lisp
;;;;
;;;;   Started:            Thu Sep 22 00:24:38 2011
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

(defpackage :compare-files (:use :common-lisp :test))

(in-package :compare-files)

(defun compare-files (file1 file2)
  (with-open-file (in1 file1 :if-does-not-exist nil)
    (with-open-file (in2 file2 :if-does-not-exist nil)
      (cond ((null in1) (warn "~A does not exist.~%" file1))
            ((null in2) (warn "~A does not exist.~%" file2))
            (t (do ((line1 (read-line in1 nil) (read-line in1 nil))
                    (line2 (read-line in2 nil) (read-line in2 nil)))
                   ((or (null line1) (null line2)))
                 (unless (string= line1 line2)
                   (format t "1: ~S~%" line1)
                   (format t "2: ~S~2%" line2)))) ))))

