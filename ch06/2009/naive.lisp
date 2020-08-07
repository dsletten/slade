;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               naive.lisp
;;;;
;;;;   Started:            Fri Mar 13 16:04:01 2009
;;;;   Modifications:
;;;;
;;;;   Purpose: Find all occurrences of PATTERN within TEXT.
;;;;
;;;;
;;;;
;;;;   Notes:
;;;;   Introduction to Algorithms (2e) ch. 32
;;;;   Order of parameters is opposite that in original.lisp!
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage naive (:use common-lisp test))

(in-package naive)

(defun string-matcher (text pattern)
  (let ((n (length text))
        (m (length pattern)))
    (loop for s from 0 upto (- n m)
          when (string= pattern text :start2 s :end2 (+ s m))
          collect s)))
;          do (format t "Pattern occurs with shift ~D~%" s))))

(defun string-matcher (text pattern)
  (let ((n (length text))
        (m (length pattern))
        (result '()))
    (dotimes (s (1+ (- n m)) (nreverse result))
      (when (string= pattern text :start2 s :end2 (+ s m))
        (push s result)))) )

(defun string-matcher (text pattern)
  (let* ((n (length text))
         (m (length pattern))
         (limit (- n m)))
    (labels ((string-matcher-aux (s result)
               (cond ((> s limit) (nreverse result))
                     ((string= pattern text :start2 s :end2 (+ s m))
                      (string-matcher-aux (1+ s) (cons s result)))
                     (t (string-matcher-aux (1+ s) result)))) )
      (string-matcher-aux 0 '()))) )

(defun string-matcher (text pattern)
  (let* ((n (length text))
         (m (length pattern))
         (limit (- n m))
         (result '()))
    (do ((s 0 (1+ s)))
        ((> s limit) (nreverse result))
      (when (string= pattern text :start2 s :end2 (+ s m))
        (push s result)))) )

(defun string-matcher (text pattern)
  (do* ((n (length text))
        (m (length pattern))
        (limit (- n m))
        (result '())
        (s 0 (1+ s)))
       ((> s limit) (nreverse result))
    (when (string= pattern text :start2 s :end2 (+ s m))
      (push s result))))

(deftest test-string-matcher ()
  (check
;   (not (string-matcher 10 "asdf"))
;   (not (string-matcher "asdf" 10))
   (string-matcher "" "")
   (not (string-matcher "ffo" "foo"))
   (string-matcher "ffoo" "foo")
   (string-matcher "foo" "foo")
   (not (string-matcher  "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (string-matcher "peter piper picked a peck of pretty pickled peppers" "pickle")
   (string-matcher "a" "")
   (not (string-matcher "" "a"))))
