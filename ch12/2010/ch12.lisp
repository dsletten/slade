;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch12.lisp
;;;;
;;;;   Started:            Sun Jan  9 20:32:38 2011
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

(defpackage ch12
  (:use common-lisp)
  (:shadow +))

(in-package ch12)

(defun + (x y)
  (typecase x
    (string (concatenate 'string x y))
    (number (cl:+ x y))
    (vector (map 'vector #'cl:+ x y))
    (list (append x y))
    (function #'(lambda (&rest args) (funcall x (apply y args)))) ))

(fmakunbound '+)

(defgeneric + (x y)
  (:documentation "Add two objects in appropriate way for their types."))

(defmethod + ((s1 string) (s2 string))
  (concatenate 'string s1 s2))

(defmethod + ((x number) (y number))
  (cl:+ x y))

(defmethod + ((u vector) (v vector))
  (map 'vector #'cl:+ u v))

(defmethod + ((l1 list) (l2 list))
  (append l1 l2))

(defmethod + ((l1 (eql nil)) (l2 list))
  l2)

(defmethod + ((l1 list) (l2 (eql nil)))
  l1)

(defmethod + ((f1 function) (f2 function))
  #'(lambda (&rest args)
      (funcall f1 (apply f2 args))))

(defun f (x) (* x x))
(defun g (x y) (+ x y))
;(funcall (+ #'f #'g) 1 2)
