;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               quiz.lisp
;;;;
;;;;   Started:            Wed Feb 29 23:38:02 2012
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
(load "/Users/dsletten/lisp/books/Slade/ch14/2012/random-generator.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :quiz
  (:use :common-lisp :lang :random-generator :test))

(in-package :quiz)

(defun drill (operator size)
  (let ((rg (make-random-generator size))
        (correct 0)
        (total 0))
    (loop
       (let ((result (ask-question operator rg)))
         (case result
           (quit (print-score correct total)
                 (return))
           (otherwise
            (when result (incf correct))
            (incf total)))) )))

(defun ask-question (operator rg)
  (let ((op1 (1+ (next-random-val rg)))
        (op2 (1+ (next-random-val rg))))
    (when (eq operator '/)
      (setf op1 (* op1 op2)))
    (let ((question (form-question operator op1 op2))
          (first-try t)
          (*read-eval* nil))
      (loop
         (handler-case
             (let ((response (prompt question)))
               (cond ((quitp response) (return 'quit))
                     ((not (numberp response)) (chide response))
                     ((= response (funcall operator op1 op2)) (correct-answer-reply) (return first-try))
                     (t (incorrect-answer-reply) (setf first-try nil))))
           (reader-error (e) (format t "Naughty!~2%")))) )))

(defun print-score (correct total)
  (format t "~%You got ~D right on the first try out of ~D problem~:P.~%" correct total))

(defun quitp (response)
  (member response '(q quit stop exit done)))

(defun form-question (operator op1 op2)
  (format nil "How much is ~D ~A ~D? " op1 (operator-name operator) op2))

(defun operator-name (operator)
  (ccase operator
    (* "times")
    (- "minus")
    (+ "plus")
    (/ "divided by")))

(defun chide (response)
  (format t "~A is not a number.~2%" response))

(defvar *correct-replies* ["Right!" "OK. That's good." "Just what I would have said!" "Close enough. (In fact, exactly right.)" "Great! Super! Let's keep going..." "Of course! (Why didn't I think of that?)" "Yep. Nice work."])
(defvar *correct-rg* (make-random-generator (length *correct-replies*)))
(defun correct-answer-reply ()
  (format t "~A~2%" (svref *correct-replies* (next-random-val *correct-rg*))))

(defvar *incorrect-replies* ["Wrong. Try again." "In a word: no." "Not quite right. One more time." "Try again. You can get it right."])
(defvar *incorrect-rg* (make-random-generator (length *incorrect-replies*)))
(defun incorrect-answer-reply ()
  (format t "~A~2%" (svref *incorrect-replies* (next-random-val *incorrect-rg*))))
