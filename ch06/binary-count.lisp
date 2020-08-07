;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               binary-count.lisp
;;;;
;;;;   Started:            Mon Oct 11 01:03:45 2004
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
;;;;   Notes: This program is derived from Slade's MSORT example
;;;;   (Ex. 6.8.3). It illustrates an odd way to display a number in binary
;;;;   where a list of lists represents each bit. A 0 bit is indicated by
;;;;   NIL and a 1 bit in the 2^k column is indicated by a sublist of 2^k *'s
;;;;   in that position.
;;;;
;;;;

(defpackage binary-count (:use common-lisp))

(in-package binary-count)

(defun binary-count (n)
  (binary-count-aux (make-list n :initial-element '*) '()))

(defun binary-count-aux (l bin-list)
  (if (null l)
      bin-list
      (binary-count-aux (cdr l)	(binary-add (list (car l)) bin-list))))

(defun binary-count-2 (n)
  (nreverse (binary-count-aux-2 n '())))

(defun binary-count-aux-2 (n bin-list)
  (if (zerop n)
      bin-list
      (binary-count-aux-2 (1- n) (binary-add (list '*) bin-list))))

(defun binary-add (l bin-list)
  (cond ((null bin-list) (list l))
	((null (car bin-list)) (cons l (cdr bin-list)))
	(t (cons '() (binary-add (append l (car bin-list))
				 (cdr bin-list)))) ))

(dotimes (i 15)
  (format t "~&~2D ~S~%" i (binary-count-2 i)))