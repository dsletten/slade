;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Mon Oct  4 22:05:22 2004
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

(defpackage ch07 (:use common-lisp))

(in-package ch07)

;;;
;;;    7.9.1
;;;
(defun column-print (l col stream)
  (cond ((null l) nil)
        (t (format stream "~VT~A~%" col (first l))
           (column-print (rest l) col stream))))

;;;
;;;    7.9.2
;;;
(defun split-list (l n)
  (labels ((split-list-aux (l i)
             (cond ((null l) (list '()))
                   ((zerop i) (cons '() (split-list-aux l n)))
                   (t (let ((result (split-list-aux (rest l)
                                                    (1- i))))
                        (cons (cons (first l)
                                    (first result))
                              (rest result)))) )))
    (split-list-aux l n)))

;;;
;;;    Doesn't quite work if last column is shorter. MAPCAR stops too soon.
;;;    
;; (defun transpose (l)
;;   (apply #'mapcar #'list l))
(defun transpose (l)
  (cond ((every #'null l) '())
                                        ;  (cond ((null l) '())
        (t (cons (transpose-aux l) (transpose (strip l)))) ))

;;;
;;;    Accumulate list of first elements from each sublist in list of lists.
;;;    
(defun transpose-aux (l)
  (cond ((null l) '())
        ((null (car l)) (transpose-aux (cdr l))) ;Handle incomplete columns.
        (t (cons (caar l) (transpose-aux (cdr l)))) ))

;;;
;;;    Remove first element from each sublist in list of lists.
;;;    
(defun strip (l)
  (cond ((null l) '())
        (t (cons (rest (first l))
                 (strip (rest l)))) ))

(defun multi-column-print (l &key
                           (columns 1)
                           (width (floor (/ 80 columns)))
                           (stream *standard-output*)
;                           (stream *standard-input*) ; ??!!!
                           (indent 0))
  (dolist (row (transpose (split-list l columns)))
;  (dolist (row (transpose (split-list l (ceiling (length l) columns))))
    (format stream "~VT~@?~%" indent (format nil "~~{~~~DA~~}" width) row)))
