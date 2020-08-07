;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch14.lisp
;;;;
;;;;   Started:            Mon Jan  3 00:06:42 2011
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

(defpackage ch14 (:use common-lisp))

(in-package ch14)

(defclass 2d-array ()
  ((row-count :initarg :row-count :reader row-count)
   (column-count :initarg :column-count :reader column-count)
   (2d-array :accessor 2d-array))
  (:documentation "A two-dimensional array (vector of vectors)."))

(defmethod initialize-instance :after ((a 2d-array) &rest args &key (initial-element 0d0))
  (declare (ignore args))
  (setf (2d-array a) (make-sequence 'vector (row-count a)))
  (loop for i from 0 below (row-count a)
        do (setf (aref (2d-array a) i) (make-sequence 'vector (column-count a) :initial-element initial-element))))

(defmethod print-object ((a 2d-array) stream)
  (dotimes (i (row-count a))
    (dotimes (j (column-count a))
      (format stream "~S " (2d-ref a i j)))
    (terpri stream)))

(defun 2d-array-p (obj)
  (typep obj '2d-array))

(defgeneric 2d-ref (a i j))
(defgeneric (setf 2d-ref) (val a i j))

(defmethod 2d-ref ((a 2d-array) (i integer) (j integer))
  (aref (aref (2d-array a) i) j))

;;;
;;;    Compare syntax of definition vs. invocation:
;;;    (setf (2d-ref *a* 2 2) 'foo)
;;;    
(defmethod (setf 2d-ref) (val (a 2d-array) (i integer) (j integer))
  (setf (aref (aref (2d-array a) i) j) val))

(defgeneric 2d-array-fill (a val))

(defmethod 2d-array-fill ((a 2d-array) val)
;  (setf (2d-array a) (map 'vector 
  (map nil #'(lambda (row) (fill row val)) (2d-array a)))

(defgeneric add (a b))

(defmethod add ((a 2d-array) (b 2d-array))
  (assert (and (= (row-count a) (row-count b))
               (= (column-count a) (column-count b))))
  (let ((result (make-instance '2d-array :row-count (row-count a) :column-count (column-count a))))
    (loop for i from 0 below (row-count a)
          do (loop for j from 0 below (column-count a)
                   do (setf (2d-ref result i j) (+ (2d-ref a i j) (2d-ref b i j))))
          finally (return result))))

;;;;
;;;;    Hash table implementation of property lists.
;;;;    
(defvar *property-list* (make-hash-table))

(defun property-check (node)
  (let ((plist (gethash node *property-list*)))
    (if plist
        plist
        (setf (gethash node *property-list*) (make-hash-table)))) )

;;
;;    GET-PROPERTY adds (indirectly) a hash table for a given NODE
;;    if that NODE is not already in *PROPERTY-LIST*.
;;    
(defun get-property (node property)
  (gethash property (property-check node)))

(defun (setf get-property) (value node property)
  (setf (gethash property (property-check node)) value))

(defun pp-plist ()
  (pp-properties *property-list* nil 0))

(defgeneric pp-properties (key value indent))

(defmethod pp-properties (property value (indent integer))
  (format t "~%~vT~A:~vT~A" indent property (+ indent 15) value))

(defmethod pp-properties ((node hash-table) value (indent integer))
  (declare (ignore value))
  (loop for k being each hash-key in node
        using (hash-value v)
        do (pp-properties k v indent)))

;; (defmethod pp-properties ((node hash-table) value (indent integer))
;;   (declare (ignore value))
;;   (with-hash-table-iterator (next node)
;;     (loop (multiple-value-bind (more key val) (next)
;;             (unless more (return))
;;             (pp-properties key val indent)))) )

;; (defmethod pp-properties ((key hash-table) value (indent integer))
;;   (declare (ignore value))
;;   (with-hash-table-iterator (get-bucket key)
;;     (labels ((next-bucket (bucket &optional bucket-key bucket-value)
;;                (when bucket
;;                  (pp-properties bucket-key bucket-value indent)
;;                  (multiple-value-call #'next-bucket (get-bucket)))) )
;;       (multiple-value-call #'next-bucket (get-bucket)))) )


(defmethod pp-properties (node (properties hash-table) (indent integer))
  (format t "~%~vT~A:" indent node)
  (loop for k being each hash-key in properties
        using (hash-value v)
        do (pp-properties k v (+ indent 5))))

;; (defmethod pp-properties (node (properties hash-table) (indent integer))
;;   (format t "~%~vT~A:" indent node)
;;   (with-hash-table-iterator (next properties)
;;     (loop (multiple-value-bind (more key val) (next)
;;             (unless more (return))
;;             (pp-properties key val (+ indent 5)))) ))
    
;; (defmethod pp-properties (key (value hash-table) (indent integer))
;;   (with-hash-table-iterator (get-bucket value)
;;     (labels ((next-bucket (bucket &optional bucket-key bucket-value)
;;                (when bucket
;;                  (pp-properties bucket-key bucket-value (+ indent 5))
;;                  (multiple-value-call #'next-bucket (get-bucket)))) )
;;       (format t "~%~vT~A:" indent key)
;;       (multiple-value-call #'next-bucket (get-bucket)))) )


;;;
;;;    Random number generator (based on Knuth)
;;;
(defclass random ()
  ((seed :accessor seed :initarg :seed :documentation "Initial random state")
   (range :accessor range :initarg :range :documentation "Upper range of random numbers")
   (generator :accessor generator :documentation "Generator function"))
  (:documentation "A random number generator"))

;;;
;;;    This either requires the test or should use SETF on SLOT-VALUE to avoid infinite recursion?
;;;    
;; (defmethod (setf seed) :after (val (obj random))
;;   (declare (ignore val))
;;   (when (> (seed obj) 65536)
;;     (setf (seed obj) (mod (seed obj) 65536))))

(defmethod initialize-instance :after ((obj random) &key)
  (let ((modulus 65536)
        (multiplier 25173)
        (increment 13849))
    (setf (generator obj)
          #'(lambda ()
              (setf (seed obj)
                    (mod (+ increment (* multiplier (seed obj)))
                         modulus))
              (values (cast (/ (* (seed obj) (range obj)) modulus)
                            (range obj)))) )))

(defun cast (x y)
  (typecase y
    (integer (floor x))
    (float (float x y))
    (otherwise x)))

(defun make-random (&optional (range 100) (seed (mod (get-universal-time) range)))
  (make-instance 'random :range range :seed seed))

(defmethod next-rand ((obj random))
  (funcall (generator obj)))

(defun test-rand (random n)
  (let ((test (make-array (range random) :initial-element 0)))
    (dotimes (i n test)
      (incf (aref test (next-rand random)))) ))

(defun chi-squared (v expected)
  (loop for i from 0 below (length v)
        summing (/ (square-diff (svref v i) expected) expected)))

(defun square-diff (x y)
  (let ((diff (- x y)))
    (* diff diff)))
