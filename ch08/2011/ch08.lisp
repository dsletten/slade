;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Sat Oct 15 03:59:46 2011
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
(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch08 (:use :common-lisp :collections :lang :test) (:shadow :reverse))

(in-package :ch08)

;;;
;;;    8.7.3
;;;
(defun string-encode (text key divisor)
  (map 'string
       #'(lambda (ch n) (code-char (+ (char-code ch) n)))
       text
       (cycle #'(lambda (ch) (rem (char-code ch) divisor)) (length text) key)))

(defun string-decode (encoded key divisor)
  (map 'string
       #'(lambda (ch n) (code-char (- (char-code ch) n)))
       encoded
       (cycle #'(lambda (ch) (rem (char-code ch) divisor)) (length encoded) key)))

;;;
;;;    Can't MAP over circular list!
;;;    This works fine with: SBCL, CLISP, Clozure
;;;    ABCL does not cope!!
;;;    
;; (defun string-encode (text key divisor)
;;   (map 'string
;;        #'(lambda (ch n) (code-char (+ (char-code ch) n)))
;;        text
;;        (make-circular-list (coerce key 'list))))

;; (defun string-decode (encoded key divisor)
;;   (map 'string
;;        #'(lambda (ch n) (code-char (- (char-code ch) n)))
;;        encoded
;;        (make-circular-list (coerce key 'list))))

(defun string-encode (text key divisor)
  (let ((key-buffer (make-circular-list (coerce key 'list))))
    (map 'string
         #'(lambda (ch) (char-encode ch (rem (char-code (pop key-buffer)) divisor)))
         text)))

(defun string-decode (encoded key divisor)
  (let ((key-buffer (make-circular-list (coerce key 'list))))
    (map 'string
         #'(lambda (ch) (char-decode ch (rem (char-code (pop key-buffer)) divisor)))
         encoded)))

(defun char-encode (ch n)
  (char-transform ch n #'+))

(defun char-decode (ch n)
  (char-transform ch n #'-))

(defun char-transform (ch n f)
  (code-char (funcall f (char-code ch) n)))

(defun string-encode (text key divisor)
  (map 'string
       #'(lambda (ch n) (char-encode ch n))
       text
       (cycle #'(lambda (ch) (rem (char-code ch) divisor)) (length text) key)))

(defun string-decode (encoded key divisor)
  (map 'string
       #'(lambda (ch n) (char-decode ch n))
       encoded
       (cycle #'(lambda (ch) (rem (char-code ch) divisor)) (length encoded) key)))

(defun string-encode (text key divisor)
  (let ((key-buffer (make-ring-buffer key)))
    (map 'string
         #'(lambda (ch) (char-encode ch (rem (char-code (next key-buffer)) divisor)))
         text)))

(defun string-decode (encoded key divisor)
  (let ((key-buffer (make-ring-buffer key)))
    (map 'string
         #'(lambda (ch) (char-decode ch (rem (char-code (next key-buffer)) divisor)))
         encoded)))

;;;
;;;    8.7.4
;;;
(defvar *suits* '(clubs diamonds hearts spades))
(defvar *ranks* (append '(ace king queen jack) (loop for i from 10 downto 2 collect (read-from-string (format nil "~R" i)))) )

(defun make-deck (ranks suits)
  (loop for suit in suits
        nconc (loop for rank in ranks
                    collect (cons rank suit))))

(defun cartesian-product (a b)
  (labels ((product (u v)
             (cond ((endp v) '())
                   ((endp u) (product a (rest v)))
                   (t (cons (cons (first u) (first v)) (product (rest u) v)))) ))
    (product a b)))

;;;
;;;    8.7.6
;;;    
(defun power-set (set)
  (cond ((null set) (list '()))
        (t (add-elt (first set) (power-set (rest set)))) ))

(defun add-elt (elt set)
  (cond ((null set) '())
        (t (list* (cons elt (first set)) (first set) (add-elt elt (rest set)))) ))
