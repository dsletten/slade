;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               random-generator.lisp
;;;;
;;;;   Started:            Sat Mar  3 17:42:43 2012
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

(defpackage :random-generator
  (:use :common-lisp :test)
  (:export :random-generator :make-random-generator :next-random-val))

(in-package :random-generator)

(defclass random-generator ()
  ((seed :initarg :seed)
   (range :reader range :initarg :range)
   (function)))

(defun make-random-generator (&optional (range 100) (seed (mod (get-universal-time) range)))
  (make-instance 'random-generator :range range :seed seed))

(defvar *modulus* 65536)
(defvar *multiplier* 25173)
(defvar *increment* 13849)
(defmethod initialize-instance :after ((rg random-generator) &key)
  (setf (slot-value rg 'function)
        #'(lambda ()
            (with-slots (seed range) rg
              (setf seed (mod (+ *increment* (* *multiplier* seed)) *modulus*))
              (values (cast (/ (* seed range) *modulus*) range)))) ))

(defun cast (x range)
  (typecase range
    (integer (floor x))
    (float (float x range))
    (otherwise x)))

(defgeneric next-random-val (generator))
(defmethod next-random-val ((rg random-generator))
  (funcall (slot-value rg 'function)))

(defun test-random (rg n)
  (let ((trials (make-array (range rg) :initial-element 0))) ; Float range?!
    (dotimes (i n trials)
      (incf (aref trials (next-random-val rg)))) ))

(defun chi-square (trials expected)
  (coerce (loop for trial across trials
                summing (/ (square-diff trial expected) expected))
          'double-float))

(defun square-diff (x y)
  (let ((diff (- x y)))
    (* diff diff)))

;; QUIZ(46): (random-generator::test-random (make-random-generator 10) 100)
;; #(9 10 11 13 10 14 5 6 10 12)
;; QUIZ(47): (random-generator::chi-square * 10)
;; 7.2d0
;; QUIZ(48): (random-generator::test-random (make-random-generator 10) 200)
;; #(20 24 17 24 17 17 18 19 20 24)
;; QUIZ(49): (random-generator::chi-square * 20)
;; 4.0d0
;; QUIZ(50): (random-generator::test-random (make-random-generator 10) 300)
;; #(20 26 32 36 25 36 35 28 31 31)
;; QUIZ(51): (random-generator::chi-square * 30)
;; 8.266666666666667d0
;; QUIZ(52): (random-generator::test-random (make-random-generator 10) 1000)
;; #(94 98 97 98 109 113 92 117 87 95)
;; QUIZ(53): (random-generator::chi-square * 100)
;; 8.5d0
