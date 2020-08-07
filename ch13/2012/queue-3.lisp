;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               queue.lisp
;;;;
;;;;   Started:            Wed Feb 15 10:03:40 2012
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
;;;;   Notes: TCONC using only functions (Not CLOS).
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :queue-3 (:use :common-lisp :test))

(in-package :queue-3)

(defun make-queue ()
  (list nil nil))

;; (defun queuep (obj)
;;   (typep obj 'queue))

(defun emptyp (q)
  (null (first q)))

(defun enqueue (q elt)
  (let ((l (list elt)))
    (if (emptyp q)
        (setf (first q) l
              (second q) l)
        (setf (rest (second q)) l
              (second q) l))))

(defun dequeue (q)
  (cond ((emptyp q) (error "Queue is empty."))
        ((eq (first q) (second q)) (prog1 (first (first q))
                                     (clear-queue q)))
        (t (pop (first q)))) )

(defun clear-queue (q)
  (setf (first q) nil
        (second q) nil))

(defun head (q)
  (first (first q)))

