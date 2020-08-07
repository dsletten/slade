;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Mon Jul  5 01:21:46 2010
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

(defpackage ch05 (:use common-lisp test) (:shadow last))

(in-package ch05)

;;;
;;;    5.7.6
;;;

;;
;;    Order of parameters different from SON-OF-ZELLER!!
;;    
(let ((months '((january 11)
                (february 12)
                (march 1)
                (april 2)
                (may 3)
                (june 4)
                (july 5)
                (august 6)
                (september 7)
                (october 8)
                (november 9)
                (december 10)))
      (days-of-week '(sunday monday tuesday wednesday thursday friday saturday)))
  (defun niece-of-zeller (month day year)
    (nth (son-of-zeller day (second (assoc month months)) year) days-of-week)))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (rem year 100) (if (leap-year-p year) 1 0)))
 
(defun zeller (n m c y l)
  (mod (- (+ n
             (cl:floor (1- (* 13 m)) 5)
             y
             (cl:floor y 4)
             (cl:floor c 4))
          (* 2 c)
          (* (1+ l) (cl:floor m 11)))
       7))

(deftest test-niece-of-zeller ()
  (check
   (equal (niece-of-zeller 'september 1 1996) 'sunday)
   (equal (niece-of-zeller 'september 2 1996) 'monday)
   (equal (niece-of-zeller 'september 3 1996) 'tuesday)
   (equal (niece-of-zeller 'september 4 1996) 'wednesday)
   (equal (niece-of-zeller 'september 5 1996) 'thursday)
   (equal (niece-of-zeller 'september 6 1996) 'friday)
   (equal (niece-of-zeller 'september 7 1996) 'saturday)
   (equal (niece-of-zeller 'september 8 1996) 'sunday)))

;;;
;;;    5.7.7
;;;
(let ((count 0)
      (args '()))
  (defun a+ (x y)
    (incf count)
    (push (list x y) args)
    (+ x y))

  (defun count+ ()
    (let ((result count))
      (setf count 0)
      result))

  (defun args+ ()
    (let ((result args))
      (setf args '())
      result)))

;;;
;;;    5.7.8
;;;
(defpackage ch05-1 (:use common-lisp test))

(in-package ch05-1)

(defun make-date (month day year)
  (cons month (cons day (cons year '()))) )

(defun date-month (date)
  (car date))

(defun date-day (date)
  (cadr date))

(defun date-year (date)
  (caddr date))

(defun (setf date-month) (month date)
  (setf (car date) month))

(defun (setf date-day) (day date)
  (setf (cadr date) day))

(defun (setf date-year) (year date)
  (setf (caddr date) year))

(defpackage ch05-2 (:use common-lisp test))

(in-package ch05-2)

(defun make-date (month day year)
  (cons month (cons day (cons year '()))) )

(defun date-month (date)
  (car date))

(defun date-day (date)
  (cadr date))

(defun date-year (date)
  (caddr date))

(defun set-month (date month)
  (setf (car date) month))

(defun set-day (date day)
  (setf (cadr date) day))

(defun set-year (date year)
  (setf (caddr date) year))

(defsetf date-month set-month)
(defsetf date-day set-day)
(defsetf date-year set-year)

(defpackage ch05-3 (:use common-lisp test))

(in-package ch05-3)

(defun make-date (month day year)
  (cons month (cons day (cons year '()))) )

(defun date-month (date)
  (car date))

(defun date-day (date)
  (cadr date))

(defun date-year (date)
  (caddr date))

(defsetf date-month (date) (month)
  `(setf (car ,date) ,month))

(defsetf date-day (date) (day)
  `(setf (cadr ,date) ,day))

(defsetf date-year (date) (year)
  `(setf (caddr ,date) ,year))

;;;
;;;    5.7.9
;;;
;;;    This isn't exactly quite right:
;;;    -What about optional 2nd arg to LAST? (Especially 0!!)
;;;    -What if final CONS is dotted pair?
;;;    -What is return value?
;;;
;;;    Compare NCONC:
;;;    (nconc (list 1 2 3 4) (list 9 8 7)) => (1 2 3 4 9 8 7)
;;;    
;;;    (let ((l (list 1 2 3 4)))
;;;      (setf (last l) (list 9 8 7))
;;;      l) => (1 2 3 9 8 7)

;;;
(in-package :ch05)

(setf (symbol-function 'last) #'cl:last)

;; (defun set-last (list cons)
;; ;  (setf (last list) cons))
;;   (let ((last2 (last list 2)))
;;     (setf (cdr last2) cons))
;;   list)

(defun set-last (list cons)
  (cond ((null (cdr list)) (setf (car list) (car cons)
                                 (cdr list) (cdr cons))
         list)
        ((null (cddr list)) (setf (cdr list) cons)
         list)
        (t (set-last (cdr list) cons))))

;(defsetf last set-last)

;; (defun replace-last (list cons)
;;   (if (atom (cdr list))
;;       cons
;;       (cons (car list) (set-last (cdr list) cons))))

;;;
;;;    Slade's version is incredibly inefficient (recursively calling LAST!),
;;;    but it demonstrates that the SETF function can be recursive.
;;;    
;; (defun (setf last) (value object)
;;   (cond ((eq (last object) (cdr object))
;;          (setf (cdr object) value))
;;         (t (setf (last (cdr object)) value))))
(defun (setf last) (cons list)
  (cond ((null (rest list)) (setf (first list) (first cons)
                                  (rest list) (rest cons)))
        ((null (rest (rest list))) (setf (rest list) cons) list)
        (t (setf (last (rest list)) cons))))

;;;
;;;    Is this more appropriate?
;;;    
(defun (setf last) (cons list)
  (cond ((null (cdr list)) (setf (car list) (car cons)
                                 (cdr list) (cdr cons)))
        ((null (cddr list)) (setf (cdr list) cons) list)
        (t (setf (last (cdr list)) cons))))

;;;
;;;    How about the return value?
;;;    
(defun (setf last) (cons list)
  (labels ((set-aux (list)
             (cond ((null (rest list)) (setf (first list) (first cons)
                                             (rest list) (rest cons)))
                   ((null (rest (rest list))) (setf (rest list) cons))
                   (t (set-aux (last (rest list)))) )))
    (set-aux list)
    list))

;;;
;;;    See CLHS LAST
;;;    (What if (= n 0) ??)
;;;    
(defun (setf last) (cons list &optional (n 1))
  (check-type n (integer 0))
  (if (atom (cdr list))
      (setf (car list) (car cons) (cdr list) (cdr cons))
      (do ((l list (cdr l))
           (r list)
           (i 0 (1+ i)))
          ((atom (cdr l)) (setf (cdr r) cons))
        (when (>= i n)
          (pop r)))) )

#|
(let ((l (list 1 2 3 4)))
  (setf (last l 0) (list 9 8 7)) l) => (1 2 3 4 9 8 7)
(let ((l (list 1 2 3 4)))
  (setf (last l) (list 9 8 7)) l) => (1 2 3 9 8 7)
(let ((l (list 1 2 3 4)))
  (setf (last l 2) (list 9 8 7)) l) => (1 2 9 8 7)
(let ((l (list 1 2 3 4)))
  (setf (last l 3) (list 9 8 7)) l) => (1 9 8 7)
(let ((l (list 1 2 3 4)))
  (setf (last l 4) (list 9 8 7)) l) => (1 9 8 7)

(let ((l (list 1)))
  (setf (last l 0) (list 9 8 7)) l) => (9 8 7)
(let ((l (list 1)))
  (setf (last l) (list 9 8 7)) l) => (9 8 7)
(let ((l (list 1)))
  (setf (last l 2) (list 9 8 7)) l) => (9 8 7)

(let ((l (cons 1 2)))
  (setf (last l 0) (list 9 8 7)) l) => (9 8 7)
(let ((l (cons 1 2)))
  (setf (last l) (list 9 8 7)) l) => (9 8 7)
(let ((l (cons 1 2)))
  (setf (last l 2) (list 9 8 7)) l) => (9 8 7)
  |#