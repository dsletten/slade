;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               boyer-moore-simplified.lisp
;;;;
;;;;   Started:            Fri Mar 27 03:56:39 2009
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
(load "/Users/dsletten/lisp/packages/test")
(load "/Users/dsletten/lisp/packages/lang")

(defpackage boyer-moore-simplified (:use common-lisp test lang))

(in-package boyer-moore-simplified)

(defvar *sigma* 256)
(defconstant non-pattern-char -1)

(defun build-last-function (pattern)
  (let ((last (make-array *sigma* :initial-element non-pattern-char)))
    (dotimes (i (length pattern) last)
      (setf (aref last (char-code (char pattern i))) i))))

(defun get-last (a ch)
  (aref a (char-code ch)))

;;;
;;;    Use hashtable. Simply encode chars from PATTERN and ignore others.
;;;    The associated GET-LAST function returns the correct value for
;;;    these "missing" elements.
;;;    
(defun build-last-function (pattern)
  (let ((last (make-hash-table)))
    (dotimes (i (length pattern) last)
      (setf (gethash (char pattern i) last) i))))

(defun get-last (h ch)
  (or (gethash ch h)
      non-pattern-char))
      
(defun boyer-moore-match (text pattern)
  (let ((last (build-last-function pattern))
        (n (length text))
        (m (length pattern)))
    (flet ((shift (i j)
             (- m (min j (1+ (get-last last (char text i)))) )))
      (if (zerop m)
          0
          (do ((i (1- m))
               (j (1- m)))
              ((> i (1- n)) nil)
            (cond ((char= (char text i) (char pattern j))
                   (when (zerop j)
                     (return-from boyer-moore-match i))
                   (decf i)
                   (decf j))
                  (t (incf i (shift i j))
                     (setf j (1- m)))) )))) )

(defun boyer-moore-match (text pattern)
  (let ((last (build-last-function pattern))
        (n (length text))
        (m (length pattern)))
    (labels ((boyer-moore-match-aux (i j)
(format t "~A~%~A~V,0T^ ~D ~D~%" text (make-string (- i j) :initial-element #\-) i i j)
               (cond ((> i (1- n)) nil)
                     ((char= (char text i) (char pattern j))
                      (if (zerop j)
                          i
                          (boyer-moore-match-aux (1- i) (1- j))))
                     (t (boyer-moore-match-aux (+ i (shift i j)) (1- m)))) )
             (shift (i j)
(print (list j (1+ (get-last last (char text i))))) (terpri)
               (- m (min j (1+ (get-last last (char text i)))) )))
      (if (zerop m)
          0
          (boyer-moore-match-aux (1- m) (1- m)))) ))

(defun boyer-moore-match-all (text pattern)
  (let ((last (build-last-function pattern))
        (n (length text))
        (m (length pattern)))
    (labels ((boyer-moore-match-aux (i j result)
               (print (list i j result))
               (cond ((> i (1- n)) (nreverse result))
                     ((char= (char text i) (char pattern j))
                      (if (zerop j)
                          (boyer-moore-match-aux (+ i m) (1- m) (cons i result))
                          (boyer-moore-match-aux (1- i) (1- j) result)))
                     (t (boyer-moore-match-aux (+ i (shift i j)) (1- m) result))) )
             (shift (i j)
               (- m (min j (1+ (get-last last (char text i)))) )))
      (if (zerop m)
          0 ; Should be list of all indices!
          (boyer-moore-match-aux (1- m) (1- m) '()))) ))

(defun print-last-function (last)
  (let ((keys (sort (keys last) #'char<)))
    (dolist (key keys)
      (format t "~C" key))
    (terpri)
    (dolist (key keys)
      (format t "~D" (gethash key last)))
    (terpri)))


;; (defun boyer-moore-match (text pattern)
;;   (let* ((last (build-last-function pattern))
;;          (n (length text))
;;          (m (length pattern))
;;          (i (1- m)))
;;     (cond ((zerop m) 0)
;;           ((< n m) nil)
;;           (t (do ((j (1- m)))
;;                  ((> i (1- n)) nil)
;;                (cond ((char= (char text i) (char pattern j))
;;                       (when (zerop j) (return-from boyer-moore-match i))
;;                       (decf i) (decf j))
;;                      (t (incf i (- m (min j (1+ (aref last (char-code (char text i)))) )))
;;                         (setf j (1- m)))) )))) )

(deftest test-boyer-moore-match ()
  (check
;   (not (boyer-moore-match 10 "asdf"))
;   (not (boyer-moore-match "asdf" 10))
   (boyer-moore-match "" "")
   (not (boyer-moore-match "ffo" "foo"))
   (boyer-moore-match "ffoo" "foo")
   (boyer-moore-match "foo" "foo")
   (not (boyer-moore-match  "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (boyer-moore-match "peter piper picked a peck of pretty pickled peppers" "pickle")
   (boyer-moore-match "a" "")
   (not (boyer-moore-match "" "a"))))
