;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Wed Nov 10 12:52:56 2010
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

(defpackage ch07 (:use common-lisp))

(in-package ch07)

(defun split-list (l n)
  (split-list-aux l n n))

(defun split-list-aux (l n i)
;(print (list l n i))
  (cond ((endp l) (if (zerop n)
                      '()
                      (cons '() (split-list-aux l (1- n) i))))
        ((zerop i) (split-list-aux l n n))
;;         ((zerop i) (if (endp l)
;;                        '()
;;                        (split-list-aux l n n)))
        (t (let ((result (split-list-aux (rest l) n (1- i))))
             (cons (cons (first l) (first result))
                   (rest result)))) ))