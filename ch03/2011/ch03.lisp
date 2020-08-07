;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Mon Jun 13 23:35:36 2011
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

(defpackage :ch03 (:use :common-lisp :test) (:shadow :pairlis))

(in-package :ch03)

;;;
;;;    3.14.4
;;;
(defun remove-zeros (l)
  (cond ((endp l) '())
        ((and (numberp (first l)) (zerop (first l))) (remove-zeros (rest l)))
        (t (cons (first l) (remove-zeros (rest l)))) ))

(defun remove-zeros (l)
  (if (endp l)
      '()
      (destructuring-bind (first . rest) l
        (if (and (numberp first) (zerop first))
            (remove-zeros rest)
            (cons first (remove-zeros rest)))) ))

(defun remove-zeros (l)
;  (remove-if #'(lambda (elt) (eql elt 0)) l))
  (remove-if #'(lambda (elt) (and (numberp elt) (zerop elt))) l))

(defun remove-zeros (l)
  (loop for elt in l
        unless (and (numberp elt) (zerop elt))
;        unless (eql elt 0)
        collect elt))

(defun remove-zeros (l)
  (let ((result '()))
    (dolist (elt l (nreverse result))
      (unless (and (numberp elt) (zerop elt))
        (push elt result)))) )

(deftest test-remove-zeros ()
  (check
   (equal (remove-zeros '(a b c d e)) '(A B C D E))
   (equal (remove-zeros '(1 0 2 0 3)) '(1 2 3))
   (equal (remove-zeros '(1 0.0 2 0 3)) '(1 2 3))))

(defun adjoin-number (obj l)
  (if (numberp obj)
      (cons obj l)
      l))

(defun adjoin-number (obj l)
  (or (and (numberp obj) (cons obj l))
      l))

(deftest test-adjoin-number ()
  (check
   (equal (adjoin-number 1 '(2 3 4 5)) '(1 2 3 4 5))
   (equal (adjoin-number 'a '(2 3 4 5)) '(2 3 4 5))))

(defvar *verb-list* '(is am are have has go went gone))

(defun verb-find (sentence)
  (cond ((endp sentence) '())
        ((verbp (first sentence)) (cons (first sentence) (verb-find (rest sentence))))
        (t (verb-find (rest sentence)))) )

(defun verb-find (sentence)
  (remove-if-not #'verbp sentence))

(defun verb-find (sentence)
  (loop for word in sentence
        when (verbp word)
        collect word))

(defun verbp (word)
  (member word *verb-list*))

(deftest test-verb-find ()
  (check
   (equal (verb-find '(tom went to the store)) '(went))
   (equal (verb-find '(tom went to the store and mary went to town)) '(went went))
   (equal (verb-find '(have you gone to the store)) '(have gone))))

;;;
;;;    3.14.5
;;;
(defun proper-list-p (l)
  (cond ((null l) t)
        ((atom l) nil)
        (t (proper-list-p (rest l)))) )

(defun proper-list-p (l)
  (tailp '() l))

(deftest test-proper-list-p ()
  (check
   (not (proper-list-p 'x))
   (not (proper-list-p 9))
   (proper-list-p '(a b c))
   (not (proper-list-p '(a b . c)))) )

;;;
;;;    3.14.6
;;;
(defun last-atom (obj)
  (cond ((atom obj) obj)
        ((atom (cdr obj)) (or (last-atom (cdr obj)) ; NIL pun
                              (last-atom (car obj))))
        (t (last-atom (cdr obj)))) )

(deftest test-last-atom ()
  (check
   (equal (last-atom '(a b c)) 'C)
   (equal (last-atom '(a b . c)) 'C)
   (equal (last-atom '(a b (c))) 'C)
   (equal (last-atom '(a b ((c)))) 'C)))

;;;
;;;    3.14.7
;;;
(defun pairlis (keys vals)
  (mapcar #'cons keys vals))

(defun pairlis (keys vals)
  (loop for key in keys
        for val in vals
        collect (cons key val)))

(defun pairlis (keys vals)
  (if (or (endp keys) (endp vals))
      '()
      (cons (cons (first keys) (first vals))
            (pairlis (rest keys) (rest vals)))) )

;;;
;;;    Inspired by "old" ch03.lisp
;;;    
(defun pairlis (keys vals)
  (if (or (endp keys) (endp vals))
      '()
      (acons (first keys) (first vals) (pairlis (rest keys) (rest vals)))) )

(deftest test-pairlis ()
  (check 
   (equal (pairlis '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))) )

(defun reverse-pairlis (keys vals)
  (do ((keys keys (rest keys))
       (vals vals (rest vals))
       (result '() (cons (cons (first keys) (first vals)) result)))
      ((or (endp keys) (endp vals)) result)))

(defun reverse-pairlis (keys vals)
  (labels ((pairlis (keys vals result)
             (if (or (endp keys) (endp vals))
                 result
                 (pairlis (rest keys) (rest vals) (cons (cons (first keys) (first vals)) result))) ))
    (pairlis keys vals '())))

(deftest test-reverse-pairlis ()
  (check 
   (equal (reverse-pairlis '(a b c) '(1 2 3)) '((C . 3) (B . 2) (A . 1)))) )

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex children)
  (pairlis '(name age weight sex children) (list name age weight sex children)))

(defun make-person (&rest args)
  (pairlis '(name age weight sex children) args))

(defun get-name (person) (cdr (assoc 'name person)))
(defun get-age (person) (cdr (assoc 'age person)))
(defun get-weight (person) (cdr (assoc 'weight person)))
(defun get-sex (person) (cdr (assoc 'sex person)))
(defun get-children (person) (cdr (assoc 'children person)))

(defclass person ()
  ((name :accessor name :initarg :name :type string)
   (age :accessor age :initarg :age :type (integer 0 120))
   (weight :accessor weight :initarg :weight :type (integer 0 1000))
   (sex :accessor sex :initarg :sex)
   (children :accessor children :initarg :children)))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~A sex: ~S age: ~A" (name p) (sex p) (age p))))

(defgeneric add-child (person child))

(defmethod add-child ((p person) (c person))
  (setf (children p) (cons c (children p))))
