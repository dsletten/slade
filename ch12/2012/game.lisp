;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               game.lisp
;;;;
;;;;   Started:            Sun Feb  5 03:32:09 2012
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
(in-package :guess)

(defstruct (game :conc-name)
  tree
  (cache (make-hash-table :test #'equalp))
  category
  (determiner-flag t)
  (back-up nil))

(defstruct (question :conc-name (:predicate questionp))
  text
  yes
  no)

(defstruct (answer :conc-name (:predicate answerp))
  value
  question)

(defmethod initialize-instance :after ((a answer) &rest init-args)
(print :after)
  (when (null (question a))
    (setf (question a) a)))

