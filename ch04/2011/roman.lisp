;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               roman.lisp
;;;;
;;;;   Started:            Mon Jun 20 23:15:27 2011
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
;;;;   Notes: See ch. 10 for some minor improvements.
;;;;
;;;;

(defpackage :roman (:use :common-lisp))

(in-package :roman)

(defconstant roman-numeral-alist '((i 1)
                                   (v 5)
                                   (x 10)
                                   (l 50)
                                   (c 100)
                                   (d 500)
                                   (m 1000)))

(defun roman-value (roman)
  (second (assoc roman roman-numeral-alist)))

(defun roman->arabic (roman-string)
  (let ((roman-list (to-roman-list roman-string)))
    (if (recognize roman-list)
        (convert roman-list)
        'invalid-roman-numeral)))

(defun convert (roman-list)
  (cond ((endp roman-list) 0)
        ((endp (rest roman-list)) (roman-value (first roman-list)))
        (t (destructuring-bind (this next . rest) roman-list
             (if (< (roman-value this) (roman-value next))
                 (+ (- (roman-value this))
                    (convert (rest roman-list)))
                 (+ (roman-value this)
                    (convert (rest roman-list)))) ))))

(defconstant state-machine '((0 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m2))
                             (i1 (i i2) (v v1) (x x1))
                             (i2 (i i3))
                             (i3)
                             (i4 (i i2))
                             (v1)
                             (v2 (i i4))
                             (x1)
                             (x2 (i i1) (v v2) (x x3) (l l1) (c c1))
                             (x3 (i i1) (v v2) (x x4))
                             (x4 (i i1) (v v2))
                             (l1 (i i1) (v v2))
                             (l2 (i i1) (v v2) (x x2))
                             (c1 (i i1) (v v2))
                             (c2 (i i1) (v v2) (x x2) (l l2) (c c3) (d d1) (m m1))
                             (c3 (i i1) (v v2) (x x2) (l l2) (c c4))
                             (c4 (i i1) (v v2) (x x2) (l l2))
                             (d1 (i i1) (v v2) (x x2) (l l2))
                             (d2 (i i1) (v v2) (x x2) (l l2) (c c2))
                             (m1 (i i1) (v v2) (x x2) (l l2))
                             (m2 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m3))
                             (m3 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m4))
                             (m4 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2))))

;; (defun transition (start input)
;;   (let ((map (rest (assoc start state-machine))))
;;     (second (assoc input map))))

(defun recognize (roman-list &optional (state (get-state 0)))
  (if (endp roman-list)
      t
      (let ((next (transition state (first roman-list))))
        (if (null next)
            nil
            (recognize (rest roman-list) next)))) )

(defun to-roman-list (s)
  (map 'list #'(lambda (ch) (intern (string (char-upcase ch)))) s))

;(loop for i from 1 to 40 do (print (recognize (to-roman-list (format nil "~@R" i)))))
;(loop for i from 1 to 400 do (unless (recognize (to-roman-list (format nil "~@R" i))) (print i)))

(defclass state ()
  ((name :reader name :initarg :name)
   (transition-map :accessor transition-map :initform (make-hash-table))))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "~A" (name state))))

(defun make-state (name)
  (make-instance 'state :name name))

(defgeneric add-transition (state transition))
(defmethod add-transition ((state state) (transition list))
  (setf (gethash (first transition) (transition-map state)) (second transition)))

(defgeneric transition (state input))
(defmethod transition ((state state) (input symbol))
  (get-state (gethash input (transition-map state))))

(defvar *state-map* (make-hash-table))
(defun initialize ()
  (dolist (row state-machine)
    (let* ((name (first row))
           (state (gethash name *state-map*)))
      (when (null state)
        (setf state (make-state name)
              (gethash name *state-map*) state))
      (dolist (transition (rest row))
        (add-transition state transition)))) )

(defun get-state (name)
  (gethash name *state-map*))