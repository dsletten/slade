;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               revise-file.lisp
;;;;
;;;;   Started:            Tue Sep 20 23:41:14 2011
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

(defpackage :revise-file (:use :common-lisp :test))

(in-package :revise-file)

(defun revise-file (input output pattern replacement)
  (with-open-file (in input)
    (with-open-file (out output :direction :output :if-exists :supersede)
      (do ((broadcast (make-broadcast-stream *standard-output* out))
           (line (read-line in nil) (read-line in nil)))
          ((null line))
        (format t "old: ~A~%" line)
        (format t "new: ")
        (replace-pattern line pattern replacement broadcast)
        (terpri)))) )

(defun replace-pattern (s pattern replacement stream)
  (replace-pattern-aux s pattern replacement stream 0 (search pattern s)))

(defun replace-pattern-aux (s pattern replacement stream start index)
  (cond (index (replace-match s replacement stream start index)
               (replace-pattern-aux s
                                    pattern
                                    replacement
                                    stream
                                    (+ index (length pattern))
                                    (search pattern s :start2 (+ index (length pattern)))) )
        (t (write-line (subseq s start) stream))))

(defun replace-match (s replacement stream start index)
  (write-string (subseq s start index) stream)
  (write-string replacement stream))

;; (defun revise-file (input output pattern replacement)
;;   (with-open-file (in input)
;;     (with-open-file (out output :direction :output :if-exists :supersede)
;;       (do ((line (read-line in nil) (read-line in nil)))
;;           ((null line))
;;         (write-line (replace-pattern line pattern replacement) out)))) )

;; (defun replace-pattern (s pattern replacement)
;;   (with-output-to-string (result)
;;     (replace-pattern-aux s pattern replacement result 0 (search pattern s))))

;; (defun replace-pattern-aux (s pattern replacement result start index)
;;   (cond (index (replace-match s replacement result start index)
;;                (replace-pattern-aux s
;;                                     pattern
;;                                     replacement
;;                                     result
;;                                     (+ index (length pattern))
;;                                     (search pattern s :start2 (+ index (length pattern)))) )
;;         (t (write-string (subseq s start) result))))

;; (defun replace-match (s replacement result start index)
;;   (write-string (subseq s start index) result)
;;   (write-string replacement result))

