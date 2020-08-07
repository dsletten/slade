;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch14.lisp
;;;;
;;;;   Started:            Tue Mar 13 00:43:07 2012
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

(defpackage :ch14
  (:use :common-lisp :test)
  (:shadow :array-dimension :array-dimensions :array-total-size :aref :array :arrayp))

(in-package :ch14)

(defclass array () ())

(defun arrayp (obj)
  (typep obj 'array))

(defgeneric aref (a &rest indices))
(defgeneric (setf aref) (val a &rest indices))
(defgeneric array-fill (a val))
(defgeneric array-dimensions (a))

(defgeneric array-dimension (a i))
(defmethod array-dimension ((a array) (i integer))
  (nth i (array-dimensions a)))

(defgeneric array-total-size (a))
(defmethod array-total-size ((a array))
  (apply #'* (array-dimensions a)))

(defclass 2d-array (array)
  ((x-dim :initarg :x-dim)
   (y-dim :initarg :y-dim)
   (2d-array)))

(defmethod initialize-instance :after ((a 2d-array) &rest args &key (initial-element 0d0))
  (declare (ignore args))
  (with-slots (x-dim y-dim 2d-array) a
    (setf 2d-array (make-array x-dim))
    (dotimes (i x-dim)
      (setf (cl:aref 2d-array i) (make-array y-dim :initial-element initial-element)))) )

(defmethod print-object ((a 2d-array) stream)
  (with-slots (x-dim y-dim) a
    (dotimes (i x-dim)
      (dotimes (j y-dim)
        (format stream "~A " (aref a i j)))
        (format stream "~%"))))

(defun make-2d-array (x y)
  (make-instance '2d-array :x-dim x :y-dim y))

(defmethod aref ((a 2d-array) &rest indices)
  (with-slots (2d-array) a
    (destructuring-bind (i j) indices
      (cl:aref (cl:aref 2d-array i) j))))

(defmethod (setf aref) (val (a 2d-array) &rest indices)
  (with-slots (2d-array) a
    (destructuring-bind (i j) indices
      (setf (cl:aref (cl:aref 2d-array i) j) val))))

(defmethod array-fill ((a 2d-array) val)
  (destructuring-bind (x y) (array-dimensions a)
    (dotimes (i x)
      (dotimes (j y)
        (setf (aref a i j) val)))) )

(defmethod array-dimensions ((a 2d-array))
  (with-slots (x-dim y-dim) a
    (list x-dim y-dim)))

(defclass 3d-array (array)
  ((x-dim :initarg :x-dim)
   (y-dim :initarg :y-dim)
   (z-dim :initarg :z-dim)
   (3d-array)))

(defmethod initialize-instance :after ((a 3d-array) &rest args &key (initial-element 0d0))
  (declare (ignore args))
  (with-slots (x-dim y-dim z-dim 3d-array) a
    (setf 3d-array (make-array x-dim))
    (dotimes (i x-dim)
      (setf (cl:aref 3d-array i) (make-array y-dim))
      (dotimes (j y-dim)
        (setf (cl:aref (cl:aref 3d-array i) j) (make-array z-dim :initial-element initial-element)))) ))

(defmethod print-object ((a 3d-array) stream)
  (with-slots (x-dim y-dim z-dim 3d-array) a
    (dotimes (i x-dim)
      (dotimes (j y-dim)
        (dotimes (k z-dim)
          (format stream "~A " (cl:aref (cl:aref (cl:aref 3d-array i) j) k)))
        (format stream "~%"))
      (format stream "~%"))))

(defun make-3d-array (x y z)
  (make-instance '3d-array :x-dim x :y-dim y :z-dim z))

(defmethod aref ((a 3d-array) &rest indices)
  (with-slots (3d-array) a
    (destructuring-bind (i j k) indices
      (cl:aref (cl:aref (cl:aref 3d-array i) j) k))))

(defmethod (setf aref) (val (a 3d-array) &rest indices)
  (with-slots (3d-array) a
    (destructuring-bind (i j k) indices
      (setf (cl:aref (cl:aref (cl:aref 3d-array i) j) k) val))))

(defmethod array-fill ((a 3d-array) val)
  (destructuring-bind (x y z) (array-dimensions a)
    (dotimes (i x)
      (dotimes (j y)
        (dotimes (k z)
          (setf (aref a i j k) val)))) ))

(defmethod array-dimensions ((a 3d-array))
  (with-slots (x-dim y-dim z-dim) a
    (list x-dim y-dim z-dim)))
