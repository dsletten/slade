;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Wed Jul  7 17:03:58 2010
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
(load "/Users/dsletten/lisp/packages/test")

(defpackage ch06 (:use common-lisp test) (:shadow string-equal string-lessp merge))

(in-package ch06)

;;;
;;;    6.8.2
;;;
(defun lastchar (s)
  (assert (stringp s))
  (let ((length (length s)))
    (if (zerop length)
        ""
        (char s (1- length)))) )

(defun capitalize (s)
  (assert (stringp s))
  (let ((length (length s)))
    (if (zerop length)
        ""
        (string-capitalize s :end 1))))

(defun string-equal (s1 s2)
  (if (= (length s1) (length s2))
      (every #'char-equal s1 s2)
      nil))

(defun string-lessp (s1 s2)
  (let ((length1 (length s1))
        (length2 (length s2)))
    (labels ((string-lessp-aux (i j)
               (cond ((= i length1) t)  ; Wrong. This fails (string-lessp "" "")
                     ((= j length2) nil)
                     ((char-equal (char s1 i) (char s2 j)) (string-lessp-aux (1+ i) (1+ j)))
                     (t (char-lessp (char s1 i) (char s2 j)))) ))
      (string-lessp-aux 0 0))))

(deftest test-lastchar ()
  (check
   (equal (lastchar "a string") #\g)
   (equal (lastchar "") "")))

(deftest test-capitalize ()
  (check
   (equal (capitalize "lisp") "Lisp")
   (equal (capitalize "is this not pung?") "Is this not pung?")))

(deftest test-string-equal ()
  (check
   (string-equal "LISP" "lisp")
   (not (string-equal "LISP" "LISS"))))

(deftest test-string-lessp ()
  (check
   (string-lessp "alpha" "beta")
   (not (string-lessp "beta" "alpha"))
   (string-lessp "Alpha" "beta")
   (string-lessp "alphabet" "alphabetize")
   (not (string-lessp "alphabetize" "alphabet"))
   (string-lessp "" "pung")
   (not (string-lessp "pung" ""))))

;;;
;;;    6.8.3
;;;
(defun merge (l1 l2)
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((numberp (first l1))
         (assert (numberp (first l2)))
         (if (< (first l2) (first l1))
             (cons (first l2) (merge l1 (rest l2)))
             (cons (first l1) (merge (rest l1) l2))))
        ((characterp (first l1))
         (assert (characterp (first l2)))
         (if (char< (first l2) (first l1))
             (cons (first l2) (merge l1 (rest l2)))
             (cons (first l1) (merge (rest l1) l2))))
        (t nil)))

;;;
;;;    Improved based on Slade's version.
;;;
(defun merge (l1 l2)
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((inorderp (first l1) (first l2))
         (cons (first l1) (merge (rest l1) l2)))
        (t (cons (first l2) (merge l1 (rest l2)))) ))

(defun inorderp (elt1 elt2)
  (cond ((numberp elt1) (assert (numberp elt2)) (<= elt1 elt2))
        ((characterp elt1) (assert (characterp elt2)) (char<= elt1 elt2))
        ((stringp elt1) (assert (stringp elt2)) (string<= elt1 elt2))
        ((symbolp elt1) (assert (symbolp elt2)) (string<= (symbol-name elt1) (symbol-name elt2)))
        (t (error "Unrecognized type."))))

(deftest test-inorderp ()
  (check
   (inorderp 3 8)
   (inorderp 8 8)
   (not (inorderp 8 3))
   (inorderp #\a #\k)
   (inorderp #\a #\a)
   (not (inorderp #\k #\a))
   (inorderp "foo" "pung")
   (inorderp "pung" "pung")
   (not (inorderp "pung" "foo"))
   (inorderp 'foo 'pung)
   (inorderp 'pung 'pung)
   (not (inorderp 'pung 'foo))))

(deftest test-merge ()
  (check
   (equal (merge '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
   (equal (merge '(1 3 5) '()) '(1 3 5))
   (equal (merge '() '(2 4 6)) '(2 4 6))
   (equal (merge '(#\b #\d #\f) '(#\a #\c #\e)) '(#\a #\b #\c #\d #\e #\f))
   (equal (merge '("Is" "pung?") '("not" "this")) '("Is" "not" "pung?" "this"))
   (equal (merge '(is pung) '(not this)) '(is not pung this))))

;;;
;;;    Complicated (slick?) mutual recursion here.
;;;    
(defun partition (l l1 l2)
  (cond ((endp l) (merge (merge-sort l1) (merge-sort l2)))
        (t (partition (rest l) (cons (first l) l2) l1))))

(defun merge-sort (l)
  (cond ((endp l) '())
        ((endp (rest l)) l)
        (t (partition l '() '()))) )

;;;
;;;    This may not be as slick, but it's clearer.
;;;
(defun merge-sort (l)
  (cond ((endp l) '())
        ((endp (rest l)) l)
        (t (multiple-value-bind (l1 l2)
               (partition l '() '())
             (merge (merge-sort l1) (merge-sort l2)))) ))

(defun partition (l l1 l2)
  (cond ((endp l) (values l1 l2))
        (t (partition (rest l) (cons (first l) l2) l1))))

;;;
;;;    6.8.4
;;;
(defun roman-char-to-decimal (s)
  (roman-to-decimal (map 'list #'(lambda (ch) (read-from-string (string ch))) s)))

;;;
;;;    6.8.5
;;;
(defun string-reverse (s)
  (string-reverse-aux (coerce s 'list) '()))

(defun string-reverse-aux (in out)
  (cond ((endp in) (coerce out 'string))
        (t (string-reverse-aux (rest in) (cons (first in) out)))) )

(deftest test-string-reverse ()
  (check
   (equal (string-reverse "hello") "olleh")
   (equal (string-reverse "") "")))

(defun string-reverse-2 (s)
  (string-reverse-aux-2 s (make-string (length s)) 0))

(defun string-reverse-aux-2 (s1 s2 i)
  (cond ((= i (length s1)) s2)
        (t (setf (char s2 (- (length s2) (1+ i))) (char s1 i))
           (string-reverse-aux-2 s1 s2 (1+ i)))) )

(deftest test-string-reverse-2 ()
  (check
   (equal (string-reverse-2 "hello") "olleh")
   (equal (string-reverse-2 "") "")))
   