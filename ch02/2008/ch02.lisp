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
;;;;   Started:            Mon Apr  7 16:12:33 2008
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

(defpackage ch02
  (:use common-lisp test)
  (:shadow signum floor ceiling))

(in-package ch02)

;;;
;;;    2.11.2
;;;
(defun add2 (x)
  (+ x 2))

(defun add5 (x)
  (+ x 5))

(defun double (x)
  (* x 2))

(defun min-abs4 (a b c d)
  (min (abs a) (abs b) (abs c) (abs d)))

(defun max-abs4 (a b c d)
  (max (abs a) (abs b) (abs c) (abs d)))

(defun min-abs (&rest args)
  (apply #'min (mapcar #'abs args)))

(defun max-abs (&rest args)
  (apply #'max (mapcar #'abs args)))

;;;
;;;    2.11.3
;;;
(defun ajoutez (x y)
  (+ x y))

(defun retranchez (x y)
  (- x y))

(defun hochstmas (x y)
  (max x y))

(defun multiplizieren (x y)
  (* x y))

(defun njia-ya-kutokea ()
  (sb-ext:quit))

(setf (symbol-function 'ajoutez) #'+)
(setf (symbol-function 'retranchez) #'-)
(setf (symbol-function 'hochstmas) #'max)
(setf (symbol-function 'multiplizieren) #'*)
(setf (symbol-function 'njia-ya-kutokea) #'sb-ext:quit)

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
          (floor (1- (* 13 m)) 5) ; Yet another simplification!
          y
          (floor y 4)
          (floor c 4)
          (- (* 2 c))
          (- (* (1+ l) (floor m 11))))
       7))

(deftest test-zeller ()
  (check
   (= (zeller 1 7 19 96 1) 0)))

;;;
;;;    2.11.5
;;;
;; (defun signum (x)
;;   (cond ((plusp x) 1)
;; 	((minusp x) -1)
;; 	((zerop x) 0)))

(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

(deftest test-signum ()
  (check
   (= (signum 8) (cl:signum 8))
   (= (signum 0) (cl:signum 0))
   (= (signum -3) (cl:signum -3))))

;; (defun interest-rate (money)
;;   (cond ((<= money 0)      0)
;; 	((< money  1000)   2)
;; 	((< money  10000)  5)
;; 	((< money  100000) 7)
;; 	(t                 10)))

(defun interest-rate (money)
  (or (and (<= money 0)     0)
      (and (< money 1000)   2)
      (and (< money 10000)  5)
      (and (< money 100000) 7)
                            10))

;;;
;;;    2.11.7
;;;
;; (defun go-to-movie-p (age cash)
;;   (or (and (< age 12)
;; 	   (> cash 3.00))
;;       (and (>= age 12)
;; 	   (< age 65)
;; 	   (> cash 7.00))
;;       (and (not (< age 65))
;; 	   (> cash 4.50))))

(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.00))
        ((< age 65) (> cash 7.00))
;;	((and (>= age 12) (< age 65)) (> cash 7.00))  D'oh!
        (t (> cash 4.50))))

(deftest test-go-to-movie-p ()
  (check
   (go-to-movie-p 5 4)
   (go-to-movie-p 16 8)
   (go-to-movie-p 85 5)
   (not (go-to-movie-p 8 2))
   (not (go-to-movie-p 24 4))
   (not (go-to-movie-p 95 4))))

;;;
;;;    2.11.9
;;;
;; (and p1 p2 p3 p4) ->
;; (cond ((not p1))
;;       ((not p2))
;;       ((not p3))
;;       (t p4))
;;
;; (or p1 p2 p3 p4) ->
;; 
;; (cond (p1)
;;       (p2)
;;       (p3)
;;       (p4))
;;       
;; (cond (p1)
;;       (p2)
;;       (p3)
;;       (t p4))
;;       
;; There is a trivial difference between the preceding two COND's. In the case where all of the
;; predicates evaluate to false, the first COND returns NIL, which is COND's value when no test
;; clause is true, whereas the second COND returns the value of P4. This difference is only evident
;; when an unusual expression is chosen for P4, for example, (values nil 8 2):
;; (or (values nil 8 2)) => NIL; 8; 2
;; (cond ((values nil 8 2))) => NIL
;; (cond (t (values nil 8 2))) => NIL; 8; 2

(defun trunc (p d)
  (if (< p d)
      (values 0 p)
      (multiple-value-bind (q r)
          (trunc (- p d) d)
        (values (1+ q) r))))
;;;
;;;    2.11.10 (See old 2009 version for analogous CEILING)
;;;
;;;    Wikipedia (http://en.wikipedia.org/wiki/Floor_function):
;;;    Given x = k + f (for any integer k, in particular, where k = (truncate x)):
;;;    (floor (+ k f)) == (+ k (floor f))
;;;    Thus, (floor x) == (floor (+ k f)) == (+ (truncate x) (floor f))
;;;    Furthermore, (minusp x) => -1 < f <= 0, so (floor f) == -1 unless x is an integer.
;;;    
(defun floor (p &optional (d 1))
  (multiple-value-bind (q r)
      (truncate p d)
    (if (minusp (* r d))
        (values (1- q) (+ r d))
        (values q r))))

;; (defun floor (r &optional (d 1 d-supplied-p))
;;   (if d-supplied-p
;;       (multiple-value-bind (q rem)
;; 	  (truncate r d)
;; 	(if (minusp (* rem d))
;; 	    (values (1- q) (+ d rem))
;; 	    (values q rem)))
;; ;; 	(cond ((zerop rem) (values q rem))
;; ;; 	      ((minusp (* r d)) (values (1- q) (+ d rem)))
;; ;; 	      (t (values q rem)))) ; (>= (/ r d) 0) => (floor r d) == (truncate r d)
;;       (multiple-value-bind (q rem)
;; 	  (truncate r)
;; 	(if (minusp rem)
;; 	    (values (1- q) (1+ rem))
;; 	    (values q rem)))) ) ; (> r 0) or (integerp r)

;; (defun floor (r &optional (d 1 d-supplied-p))
;;   (if d-supplied-p
;;       (if (< r 0)
;; 	  (multiple-value-bind (q rem)
;; 	      (floor (- r) d)
;; 	    (values (1- (- q)) (+ q rem)))
;; 	  (truncate r d))
;;       (if (< r 0)
;; 	  (multiple-value-bind (q rem)
;; 	      (floor (-  (1- r)))
;; 	    (values (1- (- q)) (- 1 rem)))
;; ;	  (- (floor (- r)))
;; 	  (truncate r))))

(defun ceiling (r &optional (d 1 d-supplied-p))
  (if d-supplied-p
      (if (>= r 0) ; Infinite loop: (> r 
          (multiple-value-bind (q rem)
              (ceiling (- r) d)
            (values (1- (- q)) rem))
          (truncate r d))
      (if (>= r 0)
          (multiple-value-bind (q rem)
              (ceiling (- r))
            (values (1- (- q)) rem))
          (truncate r))))

(defun values-test (f1 f2 r)
  (multiple-value-bind (q1 r1)
      (funcall f1 r)
    (multiple-value-bind (q2 r2)
        (funcall f2 r)
      (and (= q1 q2) (= r1 r2)))) )

(defun values-test2 (f1 f2 r d)
  (multiple-value-bind (q1 r1)
      (funcall f1 r d)
    (multiple-value-bind (q2 r2)
        (funcall f2 r d)
      (and (= q1 q2) (= r1 r2)))) )

(deftest test-floor ()
  (check
   (values-test #'floor #'cl:floor 9)
   (values-test #'floor #'cl:floor 0)
   (values-test #'floor #'cl:floor -9)
   (values-test #'floor #'cl:floor 9.8)
   (values-test #'floor #'cl:floor -9.8)
   (values-test2 #'floor #'cl:floor 98 1)
   (values-test2 #'floor #'cl:floor 0 1)
   (values-test2 #'floor #'cl:floor -98 1)
   (values-test2 #'floor #'cl:floor 98 -1)
   (values-test2 #'floor #'cl:floor -98 -1)
   (values-test2 #'floor #'cl:floor 98 10)
   (values-test2 #'floor #'cl:floor -98 10)
   (values-test2 #'floor #'cl:floor 98 -10)
   (values-test2 #'floor #'cl:floor -98 -10)))

(deftest test-ceiling ()
  (check
   (values-test #'ceiling #'cl:ceiling 9.0)
   (values-test #'ceiling #'cl:ceiling 9)
   (values-test #'ceiling #'cl:ceiling 9.8)
   (values-test #'ceiling #'cl:ceiling -9.8)
   (values-test2 #'ceiling #'cl:ceiling 98 10)
   (values-test2 #'ceiling #'cl:ceiling -98 10)))

