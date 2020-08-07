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
;;;;   Started:            Mon Feb 23 03:11:00 2009
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
(load "/Users/dsletten/lisp/packages/test.dfsl")

(defpackage ch03 (:use common-lisp test) (:shadow pairlis))

(in-package ch03)

;;;
;;;    3.14.4
;;;
(defun no-zeros (l)
  (remove 0 l))

(defun no-zeros (l)
  (mapcan #'(lambda (elt) (if (and (numberp elt) (zerop elt)) '() (list elt))) l))

(defun no-zeros (l)
  (cond ((endp l) '())
        ((and (numberp (first l)) (zerop (first l))) (no-zeros (rest l)))
        (t (cons (first l) (no-zeros (rest l)))) ))

(defun no-zeros (l)
  (loop for elt in l
        unless (and (numberp elt) (zerop elt))
;;        unless (zerop elt)  Oops...
        collect elt))

(deftest test-no-zeros ()
  (check
   (equal (no-zeros '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros '(a b c d e)) '(A B C D E))))

(defun collect-numbers (elt list)
  (if (numberp elt)
      (cons elt list)
      list))

(deftest test-collect-numbers ()
  (check
   (equal (collect-numbers 1 '(2 3 4 5)) '(1 2 3 4 5))
   (equal (collect-numbers 'a '(2 3 4 5)) '(2 3 4 5))))

(let ((verb-list '(is am are have has go went gone)))
  (defun verbp (word)
    (if (member word verb-list)
        word
        nil)))

;;;
;;; ?!
;; (defun verbp (word)
;;   (member word verb-list))

(defun verb-find (sentence)
  (remove nil (mapcar #'verbp sentence)))

(defun verb-find (sentence)
  (if (endp sentence)
      '()
      (let ((word (first sentence)))
        (if (verbp word)
            (cons word (verb-find (rest sentence)))
            (verb-find (rest sentence)))) ))

(defun verb-find (sentence)
  (loop for word in sentence
        when (verbp word)
        collect word))

(deftest test-verb-find ()
  (check
   (equal (verb-find '(tom went to the store)) '(went))
   (equal (verb-find '(tom went to the store and mary went to town)) '(went went))
   (equal (verb-find '(have you gone to the store)) '(have gone))))
   
;;;
;;;    3.14.5
;;;
(defun proper-list-p (obj)
  (cond ((not (listp obj)) nil)
        (t (null (cdr (last obj)))) ))

(defun proper-list-p (obj)
  (cond ((not (listp obj)) nil)
        (:otherwise (tailp nil obj))))

(defun proper-list-p (obj)
  (cond ((not (listp obj)) nil)
        ((null obj) t)
        (t (proper-list-p (cdr obj))) ))

;;
;;    Ooops...
;;    
;; (defun proper-list-p (obj)
;;   (cond ((not (listp obj)) nil)
;;         (t (proper-list-p (cdr obj))) ))
  
(deftest test-proper-list-p ()
  (check
   (not (proper-list-p 'x))
   (not (proper-list-p 9))
   (proper-list-p '(a b c))
   (not (proper-list-p '(a b . c)))) )

;;;
;;;    3.14.6
;;;

;;;
;;;    Yeehaw!
;;;    
(defun last-atom (l)
  (if (null (cdr (last l)))
      (car (last l))
      (cdr (last l))))

;;;
;;;    This one is just as bad since PROPER-LIST-P CDRs
;;;    all the way to the end too!
;;;    
(defun last-atom (l)
  (if (proper-list-p l)
      (car (last l))
      (cdr (last l))))

(defun last-atom (l)
  (cond ((null (cdr l)) (if (atom (car l))
                            (car l)
                            (last-atom (car l))))
        ((atom (cdr l)) (cdr l))
        (t (last-atom (cdr l)))) )
  
(deftest test-last-atom ()
  (check
   (equal (last-atom '(a b c)) 'c)
   (equal (last-atom '(1 2 . 3)) 3)
   (equal (last-atom '(a b c (d e))) 'e)))
  

;;;
;;;    3.14.7
;;;
(defun pairlis (keys vals)
  (mapcar #'cons keys vals))

;;;
;;;    CL:PAIRLIS throws error if unequal lengths.
;;;    
(defun pairlis (keys vals)
  (cond ((endp keys) '())
        ((endp vals) '())
        (t (cons (cons (first keys) (first vals))
                 (pairlis (rest keys) (rest vals)))) ))

;;;
;;;    Duh...
;;;    
(defun pairlis (keys vals)
  (cond ((endp keys) '())
        ((endp vals) '())
        (t (acons (first keys) (first vals) (pairlis (rest keys) (rest vals)))) ))

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

;;;
;;;    3.14.9
;;;
(defun make-person2 (name age weight sex children)
  (setf (get name 'age) age
        (get name 'weight) weight
        (get name 'sex) sex
        (get name 'children) children))

(defun make-person2 (name age weight sex children)
  (mapc #'(lambda (key val) (setf (get name key) val))
        '(age weight sex children)
        (list age weight sex children)))
  
(defun get-age2 (name)
  (get name 'age))
  
(defun get-weight2 (name)
  (get name 'weight))
  
(defun get-sex2 (name)
  (get name 'sex))
  
(defun get-children2 (name)
  (get name 'children))
  
;;;
;;;    3.14.10
;;;
(defun get-name+age (person)
  (list (get-name person) (get-age person)))

(defun age-of-children (person)
;;  (mapcar #'get-name+age (get-children person)))
  (mapcar #'(lambda (child) (get-name+age (symbol-value child))) (get-children person)))

;;;
;;;    Bootstrap problem here. I'd like to make the children list the actual PERSON
;;;    objects, but then I have to define them first.
;;;    
(defvar *joe* (make-person 'joe 35 150 'male '(*irving* *mabel*)))
(defvar *mabel* (make-person 'mabel 10 75 'female '()))
(defvar *irving* (make-person 'irving 12 94 'male '()))

;;;
;;;    3.14.11
;;;
(defvar *months* '((january 11) (february 12) (march 1) (april 2) (may 3) (june 4) (july 5) (august 6) (september 7) (october 8) (november 9) (december 10)))
(defvar *days* '(sunday monday tuesday wednesday thursday friday saturday))

(defun daughter-of-zeller (month day year)
  (nth (son-of-zeller day (second (assoc month *months*)) year) *days*))

(defun zeller (n m c y l)
  (mod (+ n
          (cl:floor (* 1/5 (1- (* 13 m))))
          y
          (cl:floor y 4)
          (cl:floor c 4)
          (- (* 2 c))
          (- (* (1+ l) (cl:floor m 11))))
       7))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

(defun son-of-zeller (n m year)
  (zeller n m (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))
