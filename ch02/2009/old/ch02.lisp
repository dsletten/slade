;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Fri Feb 13 02:54:22 2009
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
(load "/Users/dsletten/lisp/packages/test.fasl")

(defpackage ch02 (:use common-lisp test) (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun add2 (x) (+ x 2))
(defun add5 (x) (+ x 5))
(defun double (x) (* x 2))
(defun min-abs4 (a b c d) (min (abs a) (abs b) (abs c) (abs d)))
(defun max-abs4 (a b c d) (max (abs a) (abs b) (abs c) (abs d)))

(defun min-abs (&rest args)
  (reduce #'min (mapcar #'abs args)))

(defun max-abs (&rest args)
  (reduce #'max (mapcar #'abs args)))

;;;
;;;    2.11.3
;;;
(defun ajoutez (x y)
  "French addition."
  (+ x y))

(defun retranchez (x y)
  "French subtraction."
  (- x y))

(defun hochstmas (x y)
  "German maximum."
  (max x y))

(defun multiplizieren (x y)
  "German multiplication."
  (* x y))

(defun njia-ya-kutokea ()
  "Swahili exit."
  (sb-ext:quit))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
          (cl:floor (* 1/5 (1- (* 13 m)))) ;See 2008 version!
          y
          (cl:floor y 4)
          (cl:floor c 4)
          (- (* 2 c))
          (- (* (1+ l) (cl:floor m 11))))
       7))

;; (defun zeller (n m c y l)
;;   (mod (+ n (floor (- (* 13/5 m) 1/5)) y (floor y 4) (floor c 4)
;;           (- (* 2 c)) (- (* (1+ l) (floor m 11))))
;;        7))

;;;
;;;    2.11.5
;;;    See 2006
;;;
(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

;; (defun signum (x)
;;   (or (and (plusp x) 1)
;;       (and (minusp x) -1)
;;       0))

(defun interest-rate (money)
  (or (and (<= money 0) 0)
      (and (< money 1000) 2)
      (and (< money 10000) 5)
      (and (< money 100000) 7)
      10))

;;;
;;;    2.11.7
;;;
(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.00))
        ((< age 65) (> cash 7.00))
        (t (> cash 4.50))))

;;;
;;;    2.11.10
;;;    See 2008 version!
;;;
(defun floor (x)
  (if (minusp x)
      (multiple-value-bind (floor rem)
          (if (integerp (rational x))
              (floor (- x))
              (floor (1+ (- x))))
        (if (integerp (rational x))
            (values (- floor) rem)
            (values (- floor) (- 1 rem))))
      (truncate x)))

;; (defun floor (x)
;;   (if (and (minusp x) (not (integerp (rational x))))
;;       (multiple-value-bind (floor rem)
;;           (truncate (1- x))
;;         (values floor (+ 1 rem)))
;;       (truncate x)))

;; (defun floor (x)
;;   (if (minusp x)
;;       (if (integerp (rational x))
;;           (- (floor (- x)))
;;           (- (floor (1+ (- x)))) )
;;       (truncate x)))

(defun floor (x)
  (let ((floor (truncate x)))
    (if (and (minusp x) (not (= x floor)))
        (1- floor)
        floor)))

(defun floor (x)
  (multiple-value-bind (floor rem)
      (truncate x)
    (if (and (minusp x) (not (zerop rem)))
        (1- floor)
        floor)))

(defun floor (x)
  (multiple-value-bind (floor rem)
      (truncate x)
    (cond ((zerop rem) floor)
          ((minusp x) (1- floor))
          (t floor))))

;;;
;;;    Inspired by 2008 version of FLOOR
;;;    
(defun ceiling (p &optional (d 1))
  (multiple-value-bind (q r)
      (truncate p d)
    (if (plusp (* r d))
        (values (1+ q) (- r d))
        (values q r))))

(defun ceiling (p &optional (d 1))
  (multiple-value-bind (q r)
      (truncate p d)
    (if (or (zerop r) ; Integer?
            (if (plusp d)
                (minusp p)
                (plusp p)))
        (values q r)
        (values (1+ q) (- r d)))) )

(defun check (&rest args)
  (multiple-value-bind (q1 r1)
      (apply #'ceiling  args)
    (multiple-value-bind (q2 r2)
        (apply #'cl:ceiling args)
      (and (= q1 q2) (= r1 r2)))) )

;;;
;;;    2.11.11
;;;
(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (n m year)
  (zeller n m (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))
