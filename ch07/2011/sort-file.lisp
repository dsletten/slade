;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               sort-file.lisp
;;;;
;;;;   Started:            Fri Sep 23 01:07:56 2011
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/io.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :sort-file (:use :common-lisp :lang :io :test))

(in-package :sort-file)

(defun sort-file (in out criteria)
  (let ((lines (read-file in)))
    (sort-lines lines criteria)))

(defun sort-lines (lines criteria)
  (sort lines #'(lambda (line1 line2)
                  (dolist (criterion criteria)
                    (destructuring-bind (start end test) criterion
                      (let ((s1 (subseq line1 start end))
                            (s2 (subseq line2 start end)))
                        (format t "Testing: (~A ~A ~A)~%" test s1 s2)
                        (when (funcall test s1 s2)
                          (return t))
                        (when (funcall test s2 s1)
                          (return nil)))) ))))

(defclass record ()
  ((first-name :reader first-name :initarg :first-name)
   (last-name :reader last-name :initarg :last-name)
   (phone-number :reader phone-number :initarg :phone-number)
   (address :reader address :initarg :address)))

(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (format stream "~A, ~A ~A ~A" (last-name r) (first-name r) (phone-number r) (address r))))

(defclass address ()
  ((street-number :reader street-number :initarg :street-number)
   (street-name :reader street-name :initarg :street-name)
   (unit :reader unit :initarg :unit :initform nil)))

(defmethod print-object ((a address) stream)
  (print-unreadable-object (a stream :type t :identity t)
    (format stream "~A ~A" (street-number a) (street-name a))
    (when (unit a)
      (format stream "(~A)" (unit a)))) )

(defun sort-file (in out criteria)
  (let ((records (mapcar #'parse-record (read-file in))))
    (write-file out (mapcar #'serialize-record (sort-records records criteria)) t)))

(defun parse-record (line)
  (destructuring-bind (first last phone address) (split line #\|)
    (make-instance 'record :last-name last :first-name first :phone-number phone :address (parse-address address))))

(defun parse-address (address-string)
  (let* ((trimmed-address (string-trim " " address-string))
         (i (position #\space trimmed-address))
         (number (subseq trimmed-address 0 i))
         (has-unit (position #\, trimmed-address))
         (street (subseq trimmed-address (1+ i) has-unit))
         (unit (if has-unit (string-trim " " (subseq trimmed-address (1+ has-unit))) nil)))
    (make-instance 'address :street-number number :street-name street :unit unit)))

(defun serialize-record (record)
  (format nil "~A|~A|~A|~5@A ~A~:[~;, ~:*~A~]"
          (first-name record)
          (last-name record)
          (phone-number record)
          (street-number (address record))
          (street-name (address record))
          (unit (address record))))

(defun sort-records (records criteria)
  (sort records #'(lambda (record1 record2)
                    (dolist (criterion criteria)
                      (destructuring-bind (slot-name test) criterion
                        (let ((s1 (slot-value record1 slot-name))
                              (s2 (slot-value record2 slot-name)))
;                          (format t "Testing: (~A ~A ~A)~%" test s1 s2)
                          (when (funcall test s1 s2)
                            (return t))
                          (when (funcall test s2 s1)
                            (return nil)))) ))))

;;;
;;;    More complex sorts
;;;    
;(sort-file "records.txt" "records.out" '((address (lambda (address1 address2) (string< (street-number address1) (street-number address2))))))
;(sort-file "records.txt" "records.out" '((address (lambda (address1 address2) (< (parse-integer (street-number address1)) (parse-integer (street-number address2)))))))

;;;
;;;    Why not?
;;;    
(defun address< (address1 address2)
  (< (parse-integer (street-number address1))
     (parse-integer (street-number address2))))

;(sort-file "records.txt" "records.out" '((address address<)))
