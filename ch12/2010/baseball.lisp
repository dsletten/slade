;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               baseball.lisp
;;;;
;;;;   Started:            Mon Dec 20 08:42:40 2010
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
(load "/Users/dsletten/lisp/books/Slade/ch12/2010/date")

(defpackage baseball (:use common-lisp))

(in-package baseball)

;; (defvar *null-game* nil)

;; (defun null-game ()
;;   (if (null *null-game*)
;;       (setf *null-game* (make-game :next *null-game*)

(defstruct game
  (date (date:make-date) :type date:date)
  home
  visitor
  (score (make-score) :type score)
  (next nil :type (or game null)))
;  (next nil :type game)) ;; Impossible to initialize!

(defstruct score
  (home 0 :type (integer 0 50))
  (visitor 0 :type (integer 0 50)))

