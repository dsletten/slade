;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               spell-correct.lisp
;;;;
;;;;   Started:            Sun Mar  8 03:03:00 2009
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
;;;;   Notes: Attempts to find word in dictionary. If this fails
;;;;   try the heuristics to see if matching word can be found.
;;;;   Terminate as soon as successful. Result is a single word
;;;;   or NIL.
;;;;
(load "/Users/dsletten/lisp/packages/io")

(defpackage spell-correct (:use common-lisp io))

(in-package spell-correct)

(defun load-dictionary ()
  (coerce (read-file "/Users/dsletten/lisp/books/Slade/ch06/wordlists/words") 'vector))

(defvar *dictionary* (load-dictionary))

;;;
;;;    The words in the dictionary are in alphabetical order but not
;;;    ASCII-betical order. Case-insensitive comparisons must be used.
;;;    
(defun binary-search (s)
  (labels ((binary-search-aux (low high)
;             (print (list low high))
             (if (< high low)
                 nil
                 (let ((middle (truncate (+ low high) 2)))
;                   (print (aref *dictionary* middle))
                   (cond ((string-equal s (aref *dictionary* middle)) middle)
                         ((string-lessp s (aref *dictionary* middle)) (binary-search-aux low (1- middle)))
                         (t (binary-search-aux (1+ middle) high)))) )))
    (binary-search-aux 0 (1- (length *dictionary*)))) )

;; (defun word-deletion (s)
;;   (loop for i from 0 below (length s)
;;         for index = (binary-search (remove (char s i) s :start i :count 1))
;;         when index do (return index)
;;         finally (return nil)))

(defun word-deletion (s)
  (let ((s1 (subseq s 1)))
    (dotimes (i (length s) nil)
      (unless (zerop i) (setf (char s1 (1- i)) (char s (1- i))))
      (let ((index (binary-search s1)))
        (when index (return index)))) ))
    
(defun word-double (s)
  (let ((s1 (make-string (1+ (length s)))) )
    (setf (subseq s1 1 (length s1)) s)
    (dotimes (i (length s) nil)
      (setf (char s1 i) (char s i))
      (let ((index (binary-search s1)))
        (when index (return index)))) ))

(defun word-transposition (s)
  (let ((s1 (copy-seq s)))
    (dotimes (i (1- (length s)) nil)
      (rotatef (char s1 i) (char s1 (1+ i)))
      (let ((index (binary-search s1)))
        (when index (return index)))
      (rotatef (char s1 i) (char s1 (1+ i)))) ))

(defun word-replace (s)
  (let ((s1 (copy-seq s)))
    (dotimes (i (length s1) nil)
      (dotimes (j (1+ (- (char-code #\z) (char-code #\a))))
        (setf (char s1 i) (code-char (+ j (char-code #\a))))
        (let ((index (binary-search s1)))
          (when index (return-from word-replace index))))
      (setf (char s1 i) (char s i)))) )

(defun word-insertion (s)
  (let ((s1 (make-string (1+ (length s)))) )
    (setf (subseq s1 1 (length s1)) s)
    (dotimes (i (length s1) nil)
      (unless (zerop i) (setf (char s1 (1- i)) (char s (1- i))))
      (dotimes (j (1+ (- (char-code #\z) (char-code #\a))))
        (setf (char s1 i) (code-char (+ j (char-code #\a))))
        (let ((index (binary-search s1)))
          (when index (return-from word-insertion index)))) )))

(defun spell-correct (s)
  (let ((result (or (binary-search s)
                    (word-deletion s)
                    (word-double s)
                    (word-transposition s)
                    (word-insertion s))))
    (if result
        (aref *dictionary* result)
        nil)))


