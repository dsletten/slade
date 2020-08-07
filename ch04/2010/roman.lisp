;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               roman.lisp
;;;;
;;;;   Started:            Wed Jun 23 15:00:25 2010
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

(defpackage roman (:use common-lisp))

(in-package roman)

;;;
;;;    This will exhaustively check for false negative. What about false positives?!
;;;    
;(loop for i from 1 upto 4000 unless (check-roman (mapcar #'(lambda (ch) (intern (string ch))) (coerce (format nil "~@R" i) 'list))) do (print i))

(defun check-roman (candidate)
  (let ((length (length candidate))
        (index 0)
        (state 0))
    (flet ((next-symbol ()
             (if (< index length)
                 (prog1 (nth index candidate)
                   (incf index))
                 'end-of-expression)))
      (loop
         (case state
           (0 (case (next-symbol)
                (i (setf state 'i1))
                (v (setf state 'v1))
                (x (setf state 'x1))
                (l (setf state 'l1))
                (c (setf state 'c1))
                (d (setf state 'd1))
                (m (setf state 'm1))
                (otherwise (return nil))))
           (i1 (case (next-symbol)
                 (i (setf state 'i2))
                 ((v x) (setf state 'i3))
                 (end-of-expression (return t))
                 (otherwise (return nil))))
           (i2 (case (next-symbol)
                 (i (setf state 'i3))
                 (end-of-expression (return t))
                 (otherwise (return nil))))
           (i3 (case (next-symbol)
                 (end-of-expression (return t))
                 (otherwise (return nil))))
           (v1 (case (next-symbol)
                 (i (setf state 'v2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (v2 (case (next-symbol)
                 (i (setf state 'i2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (x1 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x2))
                 ((l c) (setf state 'x3))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (x2 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x3))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (x3 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (l1 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'l2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (l2 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (c1 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c2))
                 ((d m) (setf state 'c3))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (c2 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c3))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (c3 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (d1 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'd2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (d2 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (m1 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c1))
                 (d (setf state 'd1))
                 (m (setf state 'm2))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (m2 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c1))
                 (d (setf state 'd1))
                 (m (setf state 'm3))
                 (end-of-expression (return t))
                 (otherwise (return nil))) )
           (m3 (case (next-symbol)
                 (i (setf state 'i1))
                 (v (setf state 'v1))
                 (x (setf state 'x1))
                 (l (setf state 'l1))
                 (c (setf state 'c1))
                 (d (setf state 'd1))
                 (end-of-expression (return t))
                 (otherwise (return nil)))) )))) )

(defvar *roman-transition-table* '((0 (i i1) (v v1) (x x1) (l l1) (c c1) (d d1) (m m1))
                                   (i1 (i i2) (v i3) (x i3))
                                   (i2 (i i3))
                                   (i3)
                                   (v1 (i v2))
                                   (v2 (i i2))
                                   (x1 (i i1) (v v1) (x x2) (l x3) (c x3))
                                   (x2 (i i1) (v v1) (x x3))
                                   (x3 (i i1) (v v1))
                                   (l1 (i i1) (v v1) (x l2))
                                   (l2 (i i1) (v v1) (x x2))
                                   (c1 (i i1) (v v1) (x x1) (l l1) (c c2) (d c3) (m c3))
                                   (c2 (i i1) (v v1) (x x1) (l l1) (c c3))
                                   (c3 (i i1) (v v1) (x x1) (l l1))
                                   (d1 (i i1) (v v1) (x x1) (l l1) (c d2))
                                   (d2 (i i1) (v v1) (x x1) (l l1) (c c2))
                                   (m1 (i i1) (v v1) (x x1) (l l1) (c c1) (d d1) (m m2))
                                   (m2 (i i1) (v v1) (x x1) (l l1) (c c1) (d d1) (m m3))
                                   (m3 (i i1) (v v1) (x x1) (l l1) (c c1) (d d1))))

(defun check-roman-2 (candidate)
  (acceptp candidate *roman-transition-table* 0))

(defun acceptp (candidate transition-table state)
  (if (null candidate)
      t
      (let ((new-state (transition (first candidate) transition-table state)))
        (if new-state
            (acceptp (rest candidate) transition-table new-state)
            nil))))

(defun transition (input transition-table state)
  (let ((entry (assoc input (rest (assoc state transition-table)))))
    (if (null entry)
        nil
        (second entry))))
