;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               quiz.lisp
;;;;
;;;;   Started:            Sat Feb 12 01:25:15 2011
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

(defpackage quiz (:use common-lisp))

(in-package quiz)

(defvar *operators* (vector '+ '- '* '/))

(defun generate-problem (operator op1 op2)
  (ccase operator
    (+ (list op1 op2 (+ op1 op2)))
    (- (if (< op1 op2)
           (list op2 op1 (- op2 op1))
           (list op1 op2 (- op1 op2))))
    (* (list op1 op2 (* op1 op2)))
    (/ (if (zerop op1)
           (if (zerop op2)
               '()
               (list (* op1 op2) op1 op2))
           (list (* op1 op2) op2 op1)))) )

(let ((random-state (make-random-state t)))
  (defun get-random-operand (n)
    (random n random-state))
  (defun get-random-operator ()
    (svref *operators* (random 4 random-state))))

(defun ask ()
  (let* ((operator (get-random-operator))
         (op1 (get-random-operand 20))
         (op2 (get-random-operand 20)))
    (destructuring-bind (x y answer) (generate-problem operator op1 op2)
      (format t "How much is ~D ~A ~D? " x operator y)
      (force-output)
      (let ((response (read)))
        (if (= answer response)
            t
            nil)))) )

