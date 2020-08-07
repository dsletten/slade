;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sun Apr 10 01:46:11 2011
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
(load "/Users/dsletten/lisp/packages/test")

(defpackage ch02 (:use common-lisp test) (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun make-add (n)
  (setf (symbol-function (intern (format nil "ADD~D" n))) #'(lambda (x) (+ x n))))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
          (cl:floor (1- (* 13 m)) 5)
          y
          (cl:floor y 4)
          (cl:floor c 4)
          (* -2 c)
          (* -1 (1+ l) (cl:floor m 11)))
       7))

(deftest test-zeller ()
  (check
   (= (zeller 1 7 19 96 1) 0)
   (= (zeller 11 7 20 1 0) 2)
   (= (zeller 16 5 19 99 0) 5)
   (= (zeller 24 12 19 96 1) 6)))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (cond ((zerop x) x)
        (t (/ x (abs x)))) )

(fmakunbound 'signum)

(defgeneric signum (x))

(defmethod signum ((x (eql 0))) x)
(defmethod signum ((x (eql 0.0))) x)
(defmethod signum ((x real)) (/ x (abs x)))

(deftest test-signum ()
  (check
   (= (signum 8) 1)
   (= (signum 8.0) 1.0)
   (= (signum -9) -1)
   (= (signum 2/3) 1)
   (= (signum 0.0) 0.0)
   (= (signum 0) 0)))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (multiple-value-bind (q r) (truncate x)
    (if (minusp r)
        (1- q)
        q)))

(defun ceiling (x)
  (multiple-value-bind (q r) (truncate x)
    (if (plusp r)
        (1+ q)
        q)))

(deftest test-floor ()
  (check
   (= (floor 8.2) (cl:floor 8.2))
   (= (floor 8.0) (cl:floor 8.0))
   (= (floor -8.2) (cl:floor -8.2))))

(deftest test-ceiling ()
  (check
   (= (ceiling 8.2) (cl:ceiling 8.2))
   (= (ceiling 8.0) (cl:ceiling 8.0))
   (= (ceiling -8.2) (cl:ceiling -8.2))))
