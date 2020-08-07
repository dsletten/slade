;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Sun Nov  7 22:14:05 2004
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

(defpackage ch08 (:use common-lisp) (:shadow reverse))

(in-package ch08)

;;;
;;;    8.7.3
;;;
(defun string-encode (text key n)
  (let ((i 0)
        (len (length key)))
    (map 'string
         #'(lambda (ch)
             (prog1
                 (code-char (+ (char-code ch)
                               (rem (char-code (char key (mod i len)))
                                    n)))
               (incf i)))
         text)))

(defun string-decode (text key n)
  (let ((i 0)
        (len (length key)))
    (map 'string
         #'(lambda (ch)
             (prog1
                 (code-char (- (char-code ch)
                               (rem (char-code (char key (mod i len)))
                                    n)))
               (incf i)))
         text)))
  
;;;
;;;    8.7.4
;;;    See cartesian-product.lisp
;;;
(defun make-deck (a b)
  (mapcan #'(lambda (elt-b)
              (mapcar #'(lambda (elt-a)
                          (cons elt-a elt-b))
                      a))
          b))

;; (defvar *suits* '(clubs diamonds hearts spades))
;; (defvar *ranks* '(ten jack queen king ace))
;; (make-deck *ranks* *suits*)

;;;
;;;    8.7.5
;;;
(defun reverse (l)
  (labels ((reverse-aux (l result)
             (if (endp l)
                 result
                 (reverse-aux (rest l) (cons (first l) result)))) )
    (reverse-aux l '())))

;;;
;;;    8.7.6
;;;    This is good, but see the better 'old' version--it CONSes less.
;;;
(defun power (s)
  (if (null s)
      (list '())
      (add-power-elt (first s) (power (rest s)))) )

(defun add-power-elt (elt s)
  (if (null s)
      '()
      (cons (cons elt (first s))
            (cons (first s) (add-power-elt elt (rest s)))) ))
