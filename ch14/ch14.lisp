;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch14.lisp
;;;
;;;   STARTED:            Tue Mar 12 20:05:41 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

;;;
;;;    14.8.4
;;;
(defclass 3d-array ()
  ((rows :initarg :rows :reader rows)
   (columns :initarg :columns :reader columns)
   (levels :initarg :levels :reader levels)
   (the-array :accessor the-array)) )

(defun make-3d-array (x y z)
  (let ((new-array (make-instance '3d-array :rows x :columns y :levels z)))
    (setf (the-array new-array) (make-array x))
    (dotimes (i x)
      (setf (aref (the-array new-array) i) (make-array y))
      (dotimes (j y)
	(setf (aref (aref (the-array new-array) i) j)
	      (make-array z))))
    new-array) )

(defgeneric 3d-array-p (obj)
  (:method ((obj 3d-array)) t)
  (:method (obj) nil) )

(defmethod print-object ((obj 3d-array) stream)
  (format stream "#<3d-array (~D X ~D X ~D)~% ~A>"
	  (rows obj) (columns obj) (levels obj) (the-array obj)) )

(defmethod 3d-ref ((obj 3d-array) (x integer) (y integer) (z integer))
  (aref (aref (aref (the-array obj) x) y) z) )

;                          v----VAL comes first!
(defmethod (setf 3d-ref) (val (obj 3d-array) (x integer) (y integer)
			  (z integer))
  (setf (aref (aref (aref (the-array obj) x) y) z) val) )

(defmethod 3d-array-fill ((obj 3d-array) val)
  (dotimes (i (rows obj) (the-array obj))
    (dotimes (j (columns obj))
      (dotimes (k (levels obj))
	(setf (3d-ref obj i j k) val)))) )

;;;
;;;    14.8.5
;;;
(defclass nd-array ()
  ((dimensions :initarg :dimensions :initform () :accessor nd-array-dimensions)
   (the-array :initform #() :accessor the-array)) )

(defun make-nd-array (&rest dimensions)
  (let ((new-nd-array (make-instance 'nd-array :dimensions dimensions)))
    (setf (the-array new-nd-array) (make-nd-array-aux dimensions))
    new-nd-array) )

(defun make-nd-array-aux (dim-list)
  (cond ((null dim-list) nil)
	(t (let ((new-array (make-array (car dim-list))))
	     (dotimes (i (car dim-list) new-array)
	       (setf (aref new-array i)
		     (make-nd-array-aux (cdr dim-list)))) ))) )

(defmethod nd-array-fill ((obj nd-array) val)
  (setf (the-array obj) (nd-array-fill-aux (nd-array-dimensions obj) val)) )

(defun nd-array-fill-aux (dim-list val)
  (cond ((null dim-list) val)
	(t (let ((new-array (make-array (car dim-list))))
	     (dotimes (i (car dim-list) new-array)
	       (setf (aref new-array i)
		     (nd-array-fill-aux (cdr dim-list) val)))) )) )
;;;
;;;    14.8.6
;;;
(defclass my-hash-table ()
  ((size :initform 10 :initarg :size :accessor size)
   (table :initform #() :initarg :table :accessor the-table)) )

(defgeneric my-hash-table-p (obj)
  (:method ((obj my-hash-table)) t)
  (:method (obj) nil) )

(defun make-my-hash-table (size)
  (make-instance 'my-hash-table :size size :table (make-array size)) )
;   (let ((htable (make-instance 'my-hash-table :size size)))
;     (setf (the-table htable) (make-array size))
;     htable) )

(defmethod next-number ((table my-hash-table) (number integer))
  (let ((new-number (1+ number)))
    (cond ((= new-number (size table)) 0)
	  (t new-number))) )

(defmethod get-bucket ((table my-hash-table) (number integer))
  (aref (the-table table) number) )

(defmethod get-number ((table my-hash-table) key number)
  (let ((new-number (or number
			(mod (sxhash key) (size table)))) )
    (let ((bucket (get-bucket table new-number)))
      (cond ((null bucket) new-number)
	    ((eq key (car bucket)) new-number)
	    (t (get-number table key (next-number table new-number)))) )) )

(defmethod table-get ((table my-hash-table) key)
  (cdr (get-bucket table (get-number table key nil))) )

(defmethod table-put ((table my-hash-table) key value)
  (setf (aref (the-table table) (get-number table key nil))
	(cons key value)) )
