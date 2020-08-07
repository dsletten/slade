;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Tue Jul  3 17:02:03 2007
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

(defpackage ch03
  (:use common-lisp test)
  (:shadow pairlis))

(in-package ch03)

;;;
;;;    3.14.4
;;;
(defun no-zeros-1 (l)
  "Remove all top-level zeros from the list L. Both float and fixnum zeros are removed."
  (remove 0 l :test #'(lambda (x y) (if (numberp y) (= x y) nil))))

(deftest test-no-zeros-1 ()
  (check
   (equal (no-zeros-1 '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros-1 '(a b c d e)) '(A B C D E))))

(defun no-zeros-2 (l)
  "Remove all top-level zeros from the list L. Both float and fixnum zeros are removed."
  (remove-if #'(lambda (elt) (if (numberp elt) (zerop elt) nil)) l))

(deftest test-no-zeros-2 ()
  (check
   (equal (no-zeros-2 '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros-2 '(a b c d e)) '(A B C D E))))

(defun no-zeros-3 (l)
  "Remove all top-level zeros from the list L. Both float and fixnum zeros are removed."
  (cond ((endp l) '())
	((and (numberp (first l)) (zerop (first l))) (no-zeros-3 (rest l)))
	(t (cons (first l) (no-zeros-3 (rest l)))) ))

(deftest test-no-zeros-3 ()
  (check
   (equal (no-zeros-3 '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros-3 '(a b c d e)) '(A B C D E))))

(defun no-zeros-4 (l)
  "Remove all top-level zeros from the list L. Both float and fixnum zeros are removed."
  (loop for elt in l
	unless (and (numberp elt) (zerop elt))
	collect elt))

(deftest test-no-zeros-4 ()
  (check
   (equal (no-zeros-4 '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros-4 '(a b c d e)) '(A B C D E))))

(defun collect-numbers (obj l)
  "Add OBJ to front of list L if it is a number."
  (if (numberp obj)
      (cons obj l)
      l))

(deftest test-collect-numbers ()
  (check
   (equal (collect-numbers 1 '(2 3 4 5)) '(1 2 3 4 5))
   (equal (collect-numbers 'a '(2 3 4 5)) '(2 3 4 5))))

;;;
;;;    See VERB-FIND-LAME in ch3.lisp.old!
;;;    
(defvar *verb-list* '(is am are have has go went gone did))

(defun verbp (word)
  (member word *verb-list*))

(defun verb-find-1 (word-list)
  "Return list of verbs which are elements of WORD-LIST"
  (remove-if-not #'verbp word-list))

(deftest test-verb-find-1 ()
  (check (equal (verb-find-1 '(tom went to the store)) '(went))
	 (equal (verb-find-1 '(where did you go joe dimaggio)) '(did go))
	 (equal (verb-find-1 '(tom went to the store and mary went to town)) '(went went))
	 (equal (verb-find-1 '(have you gone to the store)) '(have gone))))

(defun verb-find-2 (word-list)
  "Return list of verbs which are elements of WORD-LIST"
  (cond ((endp word-list) '())
	((verbp (first word-list)) (cons (first word-list) (verb-find-2 (rest word-list))))
	(t (verb-find-2 (rest word-list)))) )

(deftest test-verb-find-2 ()
  (check (equal (verb-find-2 '(tom went to the store)) '(went))
	 (equal (verb-find-2 '(where did you go joe dimaggio)) '(did go))
	 (equal (verb-find-2 '(tom went to the store and mary went to town)) '(went went))
	 (equal (verb-find-2 '(have you gone to the store)) '(have gone))))

(defun verb-find-3 (word-list)
  "Return list of verbs which are elements of WORD-LIST"
  (loop for word in word-list
	when (verbp word)
	collect word))

(deftest test-verb-find-3 ()
  (check (equal (verb-find-3 '(tom went to the store)) '(went))
	 (equal (verb-find-3 '(where did you go joe dimaggio)) '(did go))
	 (equal (verb-find-3 '(tom went to the store and mary went to town)) '(went went))
	 (equal (verb-find-3 '(have you gone to the store)) '(have gone))))

;;;
;;;    3.14.5
;;;
(defun proper-list-p-1 (obj)
  "Is OBJ a proper list?"
  (if (listp obj)
      (null (cdr (last obj)))
      nil))

(deftest test-proper-list-p-1 ()
  (check
   (eq (proper-list-p-1 'x) nil)
   (eq (proper-list-p-1 9) nil)
   (eq (proper-list-p-1 '(a b c)) t)
   (eq (proper-list-p-1 '(a b . c)) nil)))

(defun proper-list-p-2 (obj)
  "Is OBJ a proper list?"
  (if (listp obj)
      (tailp '() obj)
      nil))

(deftest test-proper-list-p-2 ()
  (check
   (eq (proper-list-p-2 'x) nil)
   (eq (proper-list-p-2 9) nil)
   (eq (proper-list-p-2 '(a b c)) t)
   (eq (proper-list-p-2 '(a b . c)) nil)))

(defun proper-list-p-3 (obj)
  "Is OBJ a proper list?"
  (cond ((null obj) t)
	((atom obj) nil)
	(t (proper-list-p-3 (rest obj)))) )

(deftest test-proper-list-p-3 ()
  (check
   (eq (proper-list-p-3 'x) nil)
   (eq (proper-list-p-3 9) nil)
   (eq (proper-list-p-3 '(a b c)) t)
   (eq (proper-list-p-3 '(a b . c)) nil)))

;;;
;;;    3.14.6
;;;
(defun last-atom-1 (l)
  "Return the last atom in a list of atoms."
  (if (proper-list-p-1 l)
      (car (last l))
      (cdr (last l))))

(deftest test-last-atom-1 ()
  (check
   (eql (last-atom-1 '(a b c)) 'c)
   (eql (last-atom-1 '(d e . f)) 'f)))

;;;    Compare LAST-ATOM ch03.lisp 040217
(defun last-atom-2 (l)
  "Return the last atom in a list of atoms."
  (cond ((null (cdr l)) (car l))
	((atom (cdr l)) (cdr l))
	(t (last-atom-2 (cdr l)))) )

(deftest test-last-atom-2 ()
  (check
   (eql (last-atom-2 '(a b c)) 'c)
   (eql (last-atom-2 '(d e . f)) 'f)))

;;;
;;;    3.14.7
;;;
(defun pairlis (keys vals)
  "Create an association list given KEYS and VALS."
  (mapcar #'cons keys vals))

(deftest test-pairlis ()
  (check
   (equal (pairlis '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))
   (equal (pairlis '(a b c) '(1 2)) '((a . 1) (b . 2)))
   (equal (pairlis '(a b) '(1 2 3)) '((a . 1) (b . 2)))) )

(defun pairlis-1 (keys vals)
  (cond ((endp keys) '())
	((endp vals) '())
	(t (cons (cons (first keys) (first vals)) (pairlis-1 (rest keys) (rest vals)))) ))

(deftest test-pairlis-1 ()
  (check
   (equal (pairlis-1 '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))
   (equal (pairlis-1 '(a b c) '(1 2)) '((a . 1) (b . 2)))
   (equal (pairlis-1 '(a b) '(1 2 3)) '((a . 1) (b . 2))) ))

(defun pairlis-1a (keys vals)
  (cond ((endp keys) '())
	((endp vals) '())
	(t (acons (first keys) (first vals) (pairlis-1 (rest keys) (rest vals)))) ))

(deftest test-pairlis-1a ()
  (check
   (equal (pairlis-1a '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))
   (equal (pairlis-1a '(a b c) '(1 2)) '((a . 1) (b . 2)))
   (equal (pairlis-1a '(a b) '(1 2 3)) '((a . 1) (b . 2))) ))

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex children)
  "Create a PERSON from the given values."
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

(deftest test-person ()
  (let ((joe (make-person 'joe 35 150 'male '(irving mabel))))
    (check
     (eq (get-name joe) 'joe)
     (= (get-age joe) 35)
     (= (get-weight joe) 150)
     (eq (get-sex joe) 'male)
     (equal (get-children joe) '(irving mabel)))) )
     
;;;
;;;    3.14.9
;;;
(defun make-person2 (name age weight sex children)
  "Create a PERSON from the given values."
  (mapc #'(lambda (prop val) (setf (get name prop) val))
	'(age weight sex children)
	(list age weight sex children)))

;; (defun get-name2 (person)
;;   (cdr (assoc 'name person)))

(defun get-age2 (name)
  (get name 'age))

(defun get-weight2 (name)
  (get name 'weight))

(defun get-sex2 (name)
  (get name 'sex))

(defun get-children2 (name)
  (get name 'children))

(make-person2 'joe 35 150 'male '(irving mabel))
(make-person2 'irving 7 50 'male '())
(make-person2 'mabel 3 30 'female '())

(deftest test-person2 ()
  (check
   (= (get-age2 'joe) 35)
   (= (get-weight2 'joe) 150)
   (eq (get-sex2 'joe) 'male)
   (equal (get-children2 'joe) '(irving mabel))))

;;;
;;;    3.14.10
;;;
(defun get-name+age (person)
  (list (get-name person) (get-age person)))

(defun get-name+age-p (name)
  (list name (get-age2 name)))

;;;
;;;    This exercise is ill-defined. MAKE-PERSON creates a PERSON data structure
;;;    that must be stored in a variable whereas MAKE-PERSON2 simply stores that
;;;    person's info in the property list of the symbol which is the person's name.
;;;    The storage mechanism is completely different in the two cases, so that when
;;;    we access the children's names (which are symbols) we don't have access to
;;;    the variables that would hold the first type of person (a-list PERSON).
;;;    
(defun age-of-children (person)
  (loop for child in (get-children person)
	collect (get-name+age child)))

(defun age-of-children-p (name)
  (loop for child in (get-children2 name)
	collect (get-name+age-p child)))

;;;
;;;    3.14.11
;;;

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n
	  (cl:floor (- (* 13/5 m) 1/5)) ; Equivalent? (cl:floor (- (* 13 m) 1) 5) [It's equivalent for integer m: 1 <= m <= 12 anyway!]
	  y
	  (cl:floor y 4)
	  (cl:floor c 4)
	  (- (* 2 c))
	  (- (* (1+ l) (cl:floor m 11))))
       7))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (cond ((zerop (mod y 400)) t)
	((zerop (mod y 100)) nil)
	(t (zerop (mod y 4)))) )

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day funny-month year)
  (zeller day funny-month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))

(defvar *months* (pairlis '(january february march april may june july august september october november december)
			  '(11 12 1 2 3 4 5 6 7 8 9 10)))
(defvar *weekdays* '(sunday monday tuesday wednesday thursday friday saturday))
(defun daughter-of-zeller (month day year)
  (nth (son-of-zeller day (cdr (assoc month *months*)) year) *weekdays*))

(deftest test-daughter-of-zeller ()
  (check
   (eq (daughter-of-zeller 'september 1 1996) 'sunday)
   (eq (daughter-of-zeller 'february 24 1996) 'saturday)
   (eq (daughter-of-zeller 'september 11 2001) 'tuesday)
   (eq (daughter-of-zeller 'july 16 1999) 'friday)
   (eq (daughter-of-zeller 'july 4 2007) 'wednesday)))
