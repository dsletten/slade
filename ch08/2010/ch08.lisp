;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Tue Sep  7 17:01:31 2010
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

(defpackage ch08 (:use common-lisp))

(in-package ch08)

;;;
;;;    8.7.4
;;;
(defconstant suits '(clubs diamonds hearts spades))
(defconstant ranks (loop for i from 2 upto 10
                         collect (read-from-string (format nil "~R" i)) into result
                         finally (return (append result '(jack queen king ace)))) )

(defun make-deck ()
  (cartesian-product ranks suits))

(defun cartesian-product (a b)
  (mapcan #'(lambda (x) (mapcar #'(lambda (y) (list x y)) b)) a))

(defun cartesian-product (a b)
  (loop for x in a
        nconc (loop for y in b
                    collect (list x y))))

(defun cartesian-product (a b)
  (let ((result '()))
    (dolist (x a (nreverse result))
      (dolist (y b)
        (push (list x y) result)))) )

;;;
;;;    This version avoids the NCONC or NREVERSE required in the other versions,
;;;    however, it is not tail recursive, and it forces the definition of
;;;    SPREAD-ELT away from its most natural expression, i.e., with only 2 args.
;;;    
(defun cartesian-product (a b)
  (cond ((endp a) '())
        (t (spread-elt (first a) b (cartesian-product (rest a) b)))) )

(defun spread-elt (elt l result)
  (cond ((endp l) result)
        (t (cons (list elt (first l)) (spread-elt elt (rest l) result)))) )

(defun cartesian-product (a b)
  (labels ((cartesian-product-tr (a result)
             (cond ((endp a) (nreverse result))
                   (t (cartesian-product-tr (rest a) (spread-elt (first a) b result)))) )
           (spread-elt (elt l result)
             (cond ((endp l) result)
                   (t (spread-elt elt (rest l) (cons (list elt (first l)) result)))) ))
    (cartesian-product-tr a '())))

;;;
;;;    8.7.6
;;;
;; (defun power (set)
;;   (cond ((null set) (list '()))
;;         (t (add-elt (first set) (power (rest set)) (power (rest set)))) )) ; <-- What is this nonsense?!

;; (defun add-elt (elt l result)
;;   (cond ((endp l) result)
;;         (t (cons (cons elt (first l)) (add-elt elt (rest l) result)))) )

;;
;;    Fixed based on 2019 version!
;;    
(defun power (set)
  (cond ((null set) (list '()))
        (t (add-elt (first set) (power (rest set)))) ))

;;;
;;;    SET is being used as both L and RESULT from above!
;;;    
(defun add-elt (elt set)
  (labels ((add (set1)
             (cond ((endp set1) set)
                   (t (cons (cons elt (first set1)) (add elt (rest set1)))) )))
    (add set)))

