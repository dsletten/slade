;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Wed Jun 29 00:32:30 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch05 (:use :common-lisp :test) (:shadow :last))

(in-package :ch05)

;;;
;;;    5.7.7
;;;
(let ((count 0)
      (args '()))
  (defun a+ (x y)
    (push (list x y) args)
    (incf count)
    (+ x y))

  (defun count+ ()
    (prog1 count (setf count 0)))

  (defun args+ ()
    (prog1 args (setf args '()))) )

;;;
;;;    5.7.8
;;;
(defun make-date (day month year)
  (list day month year))

(defun date-day (date)
  (first date))

(defun date-month (date)
  (second date))

(defun date-year (date)
  (third date))

(defun print-date (date)
  (format t "~2,'0D/~2,'0D/~4,'0D" (date-month date) (date-day date) (date-year date)))

(defun make-date (day month year)
  (cons day (cons month (cons year '()))) )

;; (defun date-day (date)
;;   (nth 0 date))

;; (defun date-month (date)
;;   (nth 1 date))

;; (defun date-year (date)
;;   (nth 2 date))

(defun date-day (date)
  (car date))

(defun date-month (date)
  (cadr date))

(defun date-year (date)
  (caddr date))

;;;
;;;    I.
;;;    
(defun (setf date-day) (day date)
  (setf (car date) day))

(defun (setf date-month) (month date)
  (setf (cadr date) month))

(defun (setf date-year) (year date)
  (setf (caddr date) year))

;;;
;;;    II.
;;;
(defun set-day (date day)
  (setf (car date) day))

(defsetf date-day set-day)

(defun set-month (date month)
  (setf (cadr date) month))

(defsetf date-month set-month)

(defun set-year (date year)
  (setf (caddr date) year))

(defsetf date-year set-year)

;;;
;;;    III.
;;;
(defsetf date-day (date) (day)
  (list 'setf (list 'car date) day))

(defsetf date-month (date) (month)
  (list 'setf (list 'cadr date) month))

(defsetf date-year (date) (year)
  (list 'setf (list 'caddr date) year))

(defsetf date-day (date) (day)
  `(setf (car ,date) ,day))

(defsetf date-month (date) (month)
  `(setf (cadr ,date) ,month))

(defsetf date-year (date) (year)
  `(setf (caddr ,date) ,year))

(defclass date ()
  ((day :accessor day :initarg :day)
   (month :accessor month :initarg :month)
   (year :accessor year :initarg :year)))

(defmethod print-object ((d date) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "~2,'0D/~2,'0D/~4,'0D" (month d) (day d) (year d))))

;;;
;;;    5.7.9
;;;
(defun last (&rest args)
  (apply #'cl:last args))

;;;
;;;    Not quite right...treats OBJ as atom?
;;;    Same behavior as (setf lastguy) below.
;;;    
(defun (setf last) (obj l)
  (cond ((atom (cdr l)) (setf (car l) obj))
;        ((atom (cddr l)) (setf (cdr l) (cons obj (cddr l))))
        (t (setf (last (cdr l)) obj))))

;;;
;;;    When the 2nd arg to LAST is 0 it returns the final atom in the list.
;;;    Therefore, (setf last) should replace only that atom in this case.
;;;
;;;    The other special case is when LIST is a single CONS cell. Its contents
;;;    are overwritten with the contents of the new value.
;;;    
;;;    Otherwise the existing N CONSes should be replaced by NEW-CONS.
;;;    
(defun (setf last) (new-cons list &optional (n 1))
  (check-type n (integer 0))
  (if (atom (cdr list))
      (if (zerop n)
          (setf (cdr list) new-cons)
          (setf (car list) (car new-cons)
                (cdr list) (cdr new-cons)))
      (do ((l list (cdr l))
           (r list)
           (i 0 (1+ i)))
          ((atom (cdr l)) (setf (cdr r) new-cons))
        (when (>= i n)
          (pop r)))) )

(defun (setf last) (new-cons list &optional (n 1))
  (check-type n (integer 0))
  (do ((l list (cdr l))
       (r list)
       (i 0 (1+ i)))
      ((atom (cdr l)) (if (and (zerop i) (not (zerop n)))
                          (setf (car r) (car new-cons)
                                (cdr r) (cdr new-cons))
                          (setf (cdr r) new-cons)))
    (when (>= i n)
      (pop r))))

(deftest test-setf-last ()
  (check
   (equal (let ((l (list 1))) (setf (last l) '(8)) l)
          '(8))
   (equal (let ((l (list 1))) (setf (last l) '(8 9 10)) l)
          '(8 9 10))
   (equal (let ((l (list 1))) (setf (last l 0) 8) l)
          '(1 . 8))
   (equal (let ((l (list 1))) (setf (last l 0) '(8)) l)
          '(1 8))
   (equal (let ((l (list 1 2 3 4))) (setf (last l) 8) l)
          '(1 2 3 . 8))
   (equal (let ((l (list 1 2 3 4))) (setf (last l) '(8)) l)
          '(1 2 3 8))
   (equal (let ((l (list 1 2 3 4))) (setf (last l 0) '(8)) l)
          '(1 2 3 4 8))
   (equal (let ((l (list 1 2 3 4))) (setf (last l 0) '(8 9 10)) l)
          '(1 2 3 4 8 9 10))
   (equal (let ((l (list 1 2 3 4))) (setf (last l) '(8 9 10)) l)
          '(1 2 3 8 9 10))
   (equal (let ((l (list 1 2 3 4))) (setf (last l 2) '(8 9 10)) l)
          '(1 2 8 9 10))
   (equal (let ((l (list 1 2 3 4))) (setf (last l 3) '(8 9 10)) l)
          '(1 8 9 10))
   (equal (let ((l (list 1 2 3 4))) (setf (last l 4) '(8 9 10)) l)
          '(1 8 9 10))))

;;;
;;;    See CLHS DEFINE-SETF-EXPANDER entry.
;;;    
(defun lastguy (x) (car (last x)))

(define-setf-expander lastguy (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (rplaca (last ,getter) ,store) ,store)
              `(lastguy ,getter))))) 