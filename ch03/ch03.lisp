;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Tue Feb 17 04:12:10 2004
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

;;;
;;;    3.14.4
;;;
(defun no-zeros (l)
  (remove 0 l))

(defun no-zeros (l)
  (cond ((null l) l)
	((and (numberp (car l)) (zerop (car l)))
	 (no-zeros (cdr l)))
	(t (cons (car l) (no-zeros (cdr l)))) ))

(defun collect-numbers (obj l)
  (cond ((numberp obj) (cons obj l))
	(t l)))

(let ((verb-list '(is am are have has go went gone)))
  (defun verb-find (l)
    (cond ((null l) l)
	  ((member (car l) verb-list) (cons (car l) (verb-find (cdr l))))
	  (t (verb-find (cdr l)))) )

  (defun verb-find2 (l)
    (remove-if-not #'(lambda (w) (member w verb-list)) l)))

;;;
;;;    3.14.5
;;;    This fails: (proper-listp '()) (Should it?)
;;;    See 'old' version.
;;;
(defun proper-listp (l)
  (cond ((atom l) nil)
	((null (cdr l)) t)
	(t (proper-listp (cdr l)))) )

;;;
;;;    3.14.6
;;;
(defun last-atom (l)
  (cond ((atom l) l)
	((null (cdr l)) (car l))
	(t (last-atom (cdr l)))) )

;;;
;;;    3.14.7
;;;
(defun my-pairlis (keys vals)
  (mapcar #'cons keys vals))

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex children)
  (pairlis '(name age weight sex children)
	   (list name age weight sex children)))

(defun get-name (person)
  (cdr (assoc 'name person)))

(defun get-age (person)
  (cdr (assoc 'age person)))

(defun get-weight (person)
  (cdr (assoc 'weight person)))

(defun get-sex (person)
  (cdr (assoc 'sex person)))

(defun get-children (person)
  (cdr (assoc 'children person)))

;;;
;;;    3.14.9
;;;
(defun make-person2 (name age weight sex children)
  (setf (get name 'age) age)
  (setf (get name 'weight) weight)
  (setf (get name 'sex) sex)
  (setf (get name 'children) children))

(defun get-age2 (person)
  (get person 'age))

(defun get-weight2 (person)
  (get person 'weight))

(defun get-sex2 (person)
  (get person 'sex))

(defun get-children2 (person)
  (get person 'children))

