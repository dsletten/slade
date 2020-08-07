;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               foo.lisp
;;;;
;;;;   Started:            Sat Aug 28 15:53:29 2010
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

(defpackage foo (:use common-lisp))

(in-package foo)

(defun split-list (l cols)
  (let* ((length (length l))
         (full-column (ceiling length cols))
         (full-column-count (rem length cols)))
    (collect-elts l full-column 0 full-column-count 1)))

;; (defun collect-elts (l col-size i full-column-count j)
;;   (cond ((endp l) '())
;;         ((= i col-size)
;;          (if (= j full-column-count)
;;              (cons '() (collect-elts l (1- col-size) 0 full-column-count (1+ j)))
;;              (cons '() (collect-elts l col-size 0 full-column-count (1+ j)))))
;;         (t (let ((result (collect-elts (rest l) col-size (1+ i) full-column-count j)))
;;              (cons (cons (first l) (first result)) (rest result)))) ))

(defun collect-elts (l col-size i full-column-count j)
  (cond ((endp l) '())
        ((= i col-size)
         (if (= j full-column-count)
             (cons '() (collect-elts l (1- col-size) 0 full-column-count (1+ j)))
             (cons '() (collect-elts l col-size 0 full-column-count (1+ j)))))
        (t (insert (first l) (collect-elts (rest l) col-size (1+ i) full-column-count j)))) )

(defun insert (elt result)
  (cons (cons elt (first result)) (rest result)))

(defun transpose (l)
  (cond ((every #'null l) '()) ; Any other way to test this??
        (t (cons (cars l) (transpose (cdrs l)))) ))

(defun cars (l)
  (cond ((endp l) '())
        (t (cons (first (first l)) (cars (rest l)))) ))

(defun cdrs (l)
  (cond ((endp l) '())
        (t (cons (rest (first l)) (cdrs (rest l)))) ))

(defun multi-column-print (l &key (columns 1) (width (/ 80 columns)) (stream *standard-output*) (indent 0))
  (dolist (row (transpose (split-list l columns)))
    (format stream "~V@T" indent)
    (dolist (elt row)
      (when elt
        (format stream "~VA" width elt)))
    (format stream "~%")))
