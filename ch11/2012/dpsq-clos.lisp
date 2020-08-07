;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               dpsq-clos.lisp
;;;;
;;;;   Started:            Sat Jan 21 03:07:13 2012
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

(defpackage :dpsq-clos (:use :common-lisp :test))

(in-package :dpsq-clos)

(defclass person ()
  ((name :accessor name :initarg :name)
   (sex :accessor sex :initarg :sex)
   (spouse :accessor spouse)
   (ex-spouses :accessor ex-spouses :initform '())
   (father :accessor father)
   (mother :accessor mother)
   (children :accessor children :initform '())
   (siblings :accessor siblings :initform '())
   (jobs :accessor jobs :initform '())))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A~@[ spouse: ~A~]~@[ siblings: ~A~]~@[ children: ~A~]"
            (name p)
            (and (slot-boundp p 'spouse) (name (spouse p)))
            (mapcar #'name (siblings p))
            (mapcar #'name (children p)))) )

(defmethod (setf spouse) :before ((p2 person) (p1 person))
  (declare (ignore p2))
  (when (and (slot-boundp p1 'spouse)
             (slot-boundp (spouse p1) 'spouse))
    (pushnew p1 (ex-spouses (spouse p1)))
    (slot-makunbound (spouse p1) 'spouse)))

(defmethod (setf spouse) :after ((p2 person) (p1 person))
  (setf (slot-value p2 'spouse) p1))

;; (defmethod (setf spouse) :after ((p2 person) (p1 person) &optional recursivep)
;;   (unless recursivep
;;     (setf (spouse p2) p1)))

(defgeneric add-sibling (person sibling))

(defmethod add-sibling ((person person) (new-sibling person))
  (with-slots (siblings) person
    (setf siblings (adjoin new-sibling siblings)))
  (with-slots (siblings) new-sibling
    (setf siblings (adjoin person siblings)))
  (dolist (existing-sibling (siblings person))
    (unless (or (eq existing-sibling new-sibling)
                (member new-sibling (siblings existing-sibling)))
      (add-sibling existing-sibling new-sibling))))

(defgeneric add-child (parent child))

(defmethod add-child ((parent person) (child person))
  (setf (children parent) (adjoin child (children parent))))

(defclass job ()
  ((instances :initform '() :allocation :class)
   (name :reader name :initarg :name)))

(defmethod initialize-instance :after ((j job) &rest args)
  (declare (ignore args))
  (with-slots (instances) j
    (pushnew j instances)))

(defvar *carpenter* (make-instance 'job :name 'carpenter))
(defvar *plumber* (make-instance 'job :name 'plumber))
(defvar *programmer* (make-instance 'job :name 'programmer))
