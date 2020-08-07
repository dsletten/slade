;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sat Jun 19 17:05:19 2010
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

(defpackage ch03 (:use common-lisp test) (:shadow pairlis))

(in-package ch03)

;;;
;;;    3.14.4
;;;
(defun no-zeros (l)
  "Removes all top-level zeros from L."
  (remove 0 l))

(deftest test-no-zeros ()
  (check
   (equal (no-zeros '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros '(1 0 (2 0) 3)) '(1 (2 0) 3))
   (equal (no-zeros #1='(a b c d e)) #1#)))

(defun no-zeros (l)
  (cond ((endp l) '())
        ((eq (first l) 0) (no-zeros (rest l)))
        (t (cons (first l) (no-zeros (rest l)))) ))

(defun collect-numbers (obj l)
  "If OBJ is a number add it to the front of L."
  (if (numberp obj)
      (cons obj l)
      l))

(deftest test-collect-numbers ()
  (check
   (equal (collect-numbers 1 '(2 3 4 5)) '(1 2 3 4 5))
   (equal (collect-numbers 'a #1='(2 3 4 5)) #1#)))

(defvar *verb-list* '(is am are have has go went gone) "List of recognized verbs.")

(defun verb-find (sentence)
  "Return a list of all of the verbs in the list of words SENTENCE."
  (remove nil (mapcar #'verbp sentence)))

(defun verbp (word)
  (if (member word *verb-list*)
      word
      nil))

(deftest test-verb-find ()
  (check
   (equal (verb-find '(tom went to the store)) '(went))
   (equal (verb-find '(tom went to the store and mary went to town)) '(went went))
   (equal (verb-find '(have you gone to the store)) '(have gone))))

(defun verb-find (sentence)
  (loop for word in sentence
;       when (verbp word)
       when (member word *verb-list*)
       collect word))

;;;
;;;    Too clever?
;;;    
(defun verbp (word)
  (first (member word *verb-list*)))

(defun verb-find (sentence)
  (cond ((endp sentence) '())
        ((verbp (first sentence)) (cons (first sentence)
                                        (verb-find (rest sentence))))
        (t (verb-find (rest sentence)))) )

;;;
;;;    3.14.5
;;;
;;    Didn't read spec adequately!!
;; (defun proper-list-p (l)
;;   "Is L a proper list?"
;;   (null (cdr (last l))))

(defun proper-list-p (l)
  "Is L a proper list?"
  (if (consp l)
      (null (cdr (last l)))
      nil))

(deftest test-proper-list-p ()
  (check
   (not (proper-list-p 'x))
   (not (proper-list-p 9))
   (proper-list-p '(a b c))
   (not (proper-list-p '(a b . c)))) )

(defun proper-list-p (l)
  (cond ((null l) t)
        ((atom l) nil)
        (t (proper-list-p (rest l)))) )

;;;
;;;    3.14.6
;;;
;;;    What about (a b c (d e))? See 2009
;;;
(defun last-atom (list)
  "Returns the last atom in a list."
  (if (proper-list-p list)
      (car (last list))
      (cdr (last list))))

(deftest test-last-atom ()
  (check
   (eq (last-atom '(a b c)) 'c)
   (eq (last-atom '(d e . f)) 'f)))

;;;
;;;    3.14.7
;;;
(defun pairlis (keys vals)
  (mapcar #'cons keys vals))

(deftest test-pairlis ()
  (check
   (equal (pairlis '(a b c) '(1 2 3))
          '((a . 1) (b . 2) (c . 3)))) )

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex children)
  (pairlis '(name age weight sex children)
           (list name age weight sex children)))

(defun get-name (person)
  (cdr (assoc 'name person)))

(defun get-age (person)
  (cdr (assoc 'age person)))

(defun get-weight (person)
  (cdr (assoc 'weight person)))

(defun get-sex (person)
  (cdr (assoc 'sex person)))

(defun get-children (person)
  (cdr (assoc 'children person)))

(defvar *joe* (make-person 'joe 35 150 'male '(irving mabel)))

;;;
;;;    3.14.9
;;;
(defun make-person2 (name age weight sex children)
  (setf (get name 'age) age
        (get name 'weight) weight
        (get name 'sex) sex
        (get name 'children) children))

(defun get-age2 (person)
  (get person 'age))

(defun get-weight2 (person)
  (get person 'weight))

(defun get-sex2 (person)
  (get person 'sex))

(defun get-children2 (person)
  (get person 'children))

(make-person2 'beth 23 110 'female '())

;;;
;;;    3.14.10
;;;
(defun get-name+age (person)
  (list (get-name person)
        (get-age person)))

(defun get-name+age2 (person)
  (list person (get-age2 person)))

(defun age-of-children (person)
  (mapcar #'get-name+age (get-children person)))

(defun age-of-children2 (person)
  (mapcar #'get-name+age2 (get-children2 person)))

(defvar *irving* (make-person 'irving 12 100 'male '()))
(defvar *mabel* (make-person 'mabel 10 85 'female '()))
(setf *joe* (make-person 'joe 35 150 'male (list *irving* *mabel*))) ; <-- Children must exist prior to parent! See 2009

;;;
;;;    3.14.11
;;;
(defvar *months* '((january 11)
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
(defvar *days-of-week* '(sunday monday tuesday wednesday thursday friday saturday))
;;
;;    Order of parameters different from SON-OF-ZELLER!!
;;    
(defun daughter-of-zeller (month day year)
  (nth (son-of-zeller day (second (assoc month *months*)) year) *days-of-week*))

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

(deftest test-daughter-of-zeller ()
  (check
   (equal (daughter-of-zeller 'september 1 1996) 'sunday)
   (equal (daughter-of-zeller 'september 2 1996) 'monday)
   (equal (daughter-of-zeller 'september 3 1996) 'tuesday)
   (equal (daughter-of-zeller 'september 4 1996) 'wednesday)
   (equal (daughter-of-zeller 'september 5 1996) 'thursday)
   (equal (daughter-of-zeller 'september 6 1996) 'friday)
   (equal (daughter-of-zeller 'september 7 1996) 'saturday)
   (equal (daughter-of-zeller 'september 8 1996) 'sunday)))
