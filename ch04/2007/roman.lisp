;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               roman.lisp
;;;;
;;;;   Started:            Wed Jul  4 18:13:56 2007
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

(defpackage roman (:use common-lisp test))

(in-package roman)

(defconstant roman-numerals '((i 1)
                              (v 5)
                              (x 10)
                              (l 50)
                              (c 100)
                              (d 500)
                              (m 1000)))

(defconstant arabic-numerals '((1000 m)
                               (900 c m)
                               (500 d)
                               (400 c d)
                               (100 c)
                               (90 x c)
                               (50 l)
                               (40 x l)
                               (10 x)
                               (9 i x)
                               (5 v)
                               (4 i v)
                               (1 i)))
;(defconstant arabic-numerals (sort (reverse roman-numerals) #'(lambda (x y) (< (second x) (second y)))) )

(defun arabic->roman (n)
  (labels ((arabic->roman-aux (n num-list)
             (cond ((endp num-list) '())
                   ((zerop n) '())
                   (t (destructuring-bind ((arabic &rest roman) &rest tail) num-list
                        (if (>= n arabic)
                            (cons roman (arabic->roman-aux (- n arabic) num-list))
                            (arabic->roman-aux n tail)))) )))
    (if (> n 3999)
        (error "Too big.")
        (apply #'append (arabic->roman-aux n arabic-numerals)))) )

;;;
;;;    Cheap way (But not as cheap as using FORMAT!)
;;;    
(defvar *roman->arabic* (make-hash-table :test #'equal))
(loop for i from 1 upto 3999
   do (let ((roman (arabic->roman i)))
        (setf (gethash roman *roman->arabic*) i)))

(defun roman->arabic-hash (roman)
  (gethash roman *roman->arabic*))

(defconstant roman-numerals-2 '(((m) 1000)
                                ((c m) 900)
                                ((d) 500)
                                ((c d) 400)
                                ((c) 100)
                                ((x c) 90)
                                ((l) 50)
                                ((x l) 40)
                                ((x) 10)
                                ((i x) 9)
                                ((v) 5)
                                ((i v) 4)
                                ((i) 1)))

;;;
;;;    This works with valid roman numerals. Additional logic is added below
;;;    to filter out invalid input.
;;;    
;; (defun roman->arabic (roman)
;;   (labels ((roman->arabic-aux (roman num-list)
;; 	     (cond ((endp roman) 0)
;; 		   ((endp num-list) nil) ; Error if we get here
;; 		   (t (destructuring-bind ((target value) &rest tail) num-list
;; 			(multiple-value-bind (match rest) (starts-with target roman)
;; 			  (if match
;; 			      (+ value (roman->arabic-aux rest num-list))
;; 			      (roman->arabic-aux roman tail)))) ))))
;;     (roman->arabic-aux roman roman-numerals-2)))

(defconstant roman-numerals-3 '(((m) 1000 3 t)
                                ((c m) 900 1 nil)
                                ((d) 500 1 nil)
                                ((c d) 400 1 nil)
                                ((c) 100 3 t)
                                ((x c) 90 1 nil)
                                ((l) 50 1 nil)
                                ((x l) 40 1 nil)
                                ((x) 10 3 t)
                                ((i x) 9 1 nil)
                                ((v) 5 1 nil)
                                ((i v) 4 1 nil)
                                ((i) 1 3 t))) ; This needs to be T here even though there
                                              ; is no "next" digit below I. It doesn't really
                                              ; matter in Common Lisp, but in Oz since nil doesn't
                                              ; have a cdr it causes problems if the else clause
                                              ; below is executed after testing ALLOW-NEXT-P.

(defun roman->arabic (roman)
  (labels ((roman->arabic-aux (roman num-list)
             (cond ((endp roman) 0)
                   ((endp num-list) nil) ; Error if we get here
                   (t (destructuring-bind ((target value count allow-next-p) &rest tail) num-list
                        (multiple-value-bind (match rest) (starts-with target roman)
                          (if match
                              (if (= count 1)
                                  (if allow-next-p
                                      (+ value (roman->arabic-aux rest tail))
                                      (+ value (roman->arabic-aux rest (rest tail))))
                                  (+ value (roman->arabic-aux rest (cons (list target value (1- count) allow-next-p) tail))))
                              (roman->arabic-aux roman tail)))) ))))
    (handler-case (roman->arabic-aux roman roman-numerals-3)
      (simple-type-error (e) (format t "Invalid roman numeral: ~A~%" roman)))) )

;;;
;;;    See PREFIXP in LANG package.
;;;    
(defun starts-with (target list)
  "Does LIST start with TARGET? If so return T and the tail of LIST following the match."
  (cond ((endp target) (values t list))
        ((endp list) (values nil '()))
        ((eql (first list) (first target)) (starts-with (rest target) (rest list)))
        (t (values nil list))))

;; (starts-with '(i v) '(i v))

;; T
;; NIL
;; * (starts-with '(c m) '(c m x x))

;; T
;; (X X)
;; * (starts-with '(m) '(c m x x))

;; NIL
;; (C M X X)

(defun numeral-to-decimal (numeral)
  (second (assoc numeral roman-numerals)))

;;;
;;;    Derived from Slade's original.
;;;    Incomplete!
;;;    (roman-to-decimal '(i i x)) => 10
;;;    
(defun roman-to-decimal (num-list)
  (cond ((endp num-list) 0)
        ((null (rest num-list)) (numeral-to-decimal (first num-list)))
        ((< (numeral-to-decimal (first num-list))
            (numeral-to-decimal (second num-list)))
         (- (roman-to-decimal (rest num-list))
            (numeral-to-decimal (first num-list))))
        (t (+ (roman-to-decimal (rest num-list))
              (numeral-to-decimal (first num-list)))) ))

(deftest test-roman-to-decimal ()
  (check
   (= (roman-to-decimal '(m c m l x x x i v)) 1984)
   (= (roman-to-decimal '(m m v i i)) 2007)))

;;;
;;;    4.7.2
;;;
(defun roman-to-decimal (num-list)
  (cond ((endp num-list) 0)
        ((null (rest num-list)) (numeral-to-decimal (first num-list)))
        (t (destructuring-bind (first second &rest tail) num-list
             (if (< (numeral-to-decimal first) (numeral-to-decimal second))
                 (if (< (* 10 (numeral-to-decimal first)) (numeral-to-decimal second))
                     (error "Invalid sequence: ~A ~A." first second)
                     (- (roman-to-decimal (rest num-list))
                        (numeral-to-decimal first)))
                 (+ (roman-to-decimal (rest num-list))
                    (numeral-to-decimal first)))) )))

(deftest test-roman-to-decimal ()
  (check
   (= (roman-to-decimal '(m c m l x x x i v)) 1984)
   (= (roman-to-decimal '(m m v i i)) 2007)))
