;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               spell-correct2.lisp
;;;;
;;;;   Started:            Sat Apr  4 02:07:39 2009
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
(load "/Users/dsletten/lisp/packages/io")

(defpackage spell-correct2 (:use common-lisp io))

(in-package spell-correct2)

(defun load-dictionary ()
  (coerce (read-file "/Users/dsletten/lisp/books/Slade/ch06/wordlists/words") 'vector))

(defvar *dictionary* (load-dictionary))
(defvar *heuristics* '(word-deletion word-double word-transposition word-insertion))
(defvar *suggestions* (make-hash-table :test #'equal))

(defun spell-correct (s)
  (let ((match (binary-search s)))
    (if match
        match
        (let ((suggestions (gethash s *suggestions*)))
          (if suggestions
              suggestions
              (let ((suggestions (sort (remove-duplicates (loop for f in *heuristics*
                                                                for suggestion = (funcall f s)
                                                                nconc suggestion)
                                                          :test #'string=)
                                       #'string-lessp)))
                (cond (suggestions (setf (gethash s *suggestions*) suggestions)
                                   suggestions)
                      (t nil)))) ))))

;;;
;;;    The words in the dictionary are in alphabetical order but not
;;;    ASCII-betical order. Case-insensitive comparisons must be used.
;;;    
(defun binary-search (s)
  (labels ((binary-search-aux (low high)
             (if (< high low)
                 nil
                 (let* ((middle (truncate (+ low high) 2))
                        (word (aref *dictionary* middle)))
                   (cond ((string-equal s word) word)
                         ((string-lessp s word) (binary-search-aux low (1- middle)))
                         (t (binary-search-aux (1+ middle) high)))) )))
    (binary-search-aux 0 (1- (length *dictionary*)))) )

;; (defun word-deletion (s)
;;   (loop for i from 0 below (length s)
;;         for word = (binary-search (remove (char s i) s :start i :count 1))
;;         when word collect word))

(defun word-deletion (s)
  (let ((s1 (subseq s 1))
        (result '()))
    (dotimes (i (length s) result)
      (unless (zerop i) (setf (char s1 (1- i)) (char s (1- i))))
      (let ((word (binary-search s1)))
        (when word (push word result)))) ))
    
(defun word-double (s)
  (let ((s1 (make-string (1+ (length s))))
        (result '()))
    (setf (subseq s1 1 (length s1)) s)
    (dotimes (i (length s) result)
      (setf (char s1 i) (char s i))
      (let ((word (binary-search s1)))
        (when word (push word result)))) ))

(defun word-transposition (s)
  (let ((s1 (copy-seq s))
        (result '()))
    (dotimes (i (1- (length s)) result)
      (rotatef (char s1 i) (char s1 (1+ i)))
      (let ((word (binary-search s1)))
        (when word (push word result)))
      (rotatef (char s1 i) (char s1 (1+ i)))) ))

(defun word-insertion (s)
  (let ((s1 (make-string (1+ (length s))))
        (result '()))
    (setf (subseq s1 1 (length s1)) s)
    (dotimes (i (length s1) result)
      (unless (zerop i) (setf (char s1 (1- i)) (char s (1- i))))
      (dotimes (j (1+ (- (char-code #\z) (char-code #\a))))
        (setf (char s1 i) (code-char (+ j (char-code #\a))))
        (let ((word (binary-search s1)))
          (when word (push word result)))) )))



