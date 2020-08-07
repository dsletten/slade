;#!/usr/local/bin/clisp

;;
;   NAME:               ch03.lisp
;
;   STARTED:            010414 (010907)
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;

;;;
;;;    3.14.4
;;;    
(defun no-zeros-cheap (l)
  "Remove all zeros from a given list."
  (remove 0 l) )

(defun test-no-zeros-cheap ()
  (let ((test-lists '(((1 0 2 0 3) (1 2 3))
		      ((a b c d e) (a b c d e))
		      ((0.0 g 9 0 1 k) (0.0 g 9 1 k)))) )
    (dolist (l test-lists)
      (let ((validate (cadr l))
	    (value (no-zeros-cheap (car l))))
	(if (equal validate value)
	    (format t "~&Good check: ~A => ~A~%" (car l) value)
	    (format t "~&Check failed: ~A => ~A [Should be ~A]~%"
		    (car l) value validate)))) ) )

;(defun no-zeros (l)
;  (cond ((null l) nil)
;	((and (numberp (car l))
;	      (zerop (car l)))
;	 (no-zeros (cdr l)))
;	(t (cons (car l) (no-zeros (cdr l)))) ) )

;;;
;;;    This won't work on 0.0
;;;    
(defun no-zeros (l)
  "Remove all zeros from a given list."
  (cond ((null l) nil)
	((equal (car l) 0) (no-zeros (cdr l)))
	(t (cons (car l) (no-zeros (cdr l)))) ) )

(defun test-no-zeros ()
  (let ((test-lists '(((1 0 2 0 3) (1 2 3))
		      ((a b c d e) (a b c d e))
		      ((0.0 g 9 0 1 k) (0.0 g 9 1 k)))) )
    (dolist (l test-lists)
      (let ((validate (cadr l))
	    (value (no-zeros (car l))))
	(if (equal validate value)
	    (format t "~&Good check: ~A => ~A~%" (car l) value)
	    (format t "~&Check failed: ~A => ~A [Should be ~A]~%"
		    (car l) value validate)))) ) )

(defun collect-numbers (elt list)
  "Add a number to the front of a list."
  (cond ((numberp elt) (cons elt list))
	(t list)) )

(defun test-collect-numbers ()
  (let ((test-data '(((1 (2 3 4 5)) (1 2 3 4 5))
		     ((a (2 3 4 5)) (2 3 4 5))
		     ((9 (a b c d)) (9 a b c d)))) )
    (dolist (l test-data)
      (let ((validate (cadr l))
	    (value (collect-numbers (caar l) (cadar l))))
	(if (equal validate value)
	    (format t "~&Good check: ~A ~A => ~A~%" (caar l) (cadar l) value)
	    (format t "~&Check failed: ~A ~A => ~A [Should be ~A]~%"
		    (caar l) (cadar l) value validate)))) ) )

(let ((verb-list '(is am are have has go went gone)))
  (defun verb-find-cheap (sentence-list)
    "Locate the 'verbs' in a given sentence."
    (remove nil (mapcar #'(lambda (x)
			    (cond ((member x verb-list) x)
				  (t nil)))
			sentence-list)) )

  (defun verb-find-cheaper (sentence-list)
    "Locate the 'verbs' in a given sentence."
    )

  (defun verb-find-lame (sentence)
    "Locate the 'verbs' in a given sentence."
    (intersection sentence verb-list) )

  (defun verb-find (sentence-list)
    "Locate the 'verbs' in a given sentence."
    (cond ((null sentence-list) nil)
	  ((member (car sentence-list) verb-list)
	   (cons (car sentence-list) (verb-find (cdr sentence-list))))
	  (t (verb-find (cdr sentence-list)))) ))

(let ((test-sentences '(((now is the time for all good men to come to the aid of their country)
			 (is))
			((we have nothing to fear but fear itself)
			 (have))
			((is this not pung)
			 (is))
			((tom went to the store)
			 (went))
			((have you gone to the store)
			 (have gone)))) )
  (defun test-verb-find ()
    (let ((functions '(verb-find-cheap verb-find-lame verb-find)))
      (dolist (f functions)
	(format t "~&Testing ~A~%" f)
	(dolist (sentence test-sentences)
	  (let ((validate (cadr sentence))
		(value    (funcall f (car sentence))))
	    (if (equal validate value)
		(format t "~&Good check: ~A => ~A~%" (car sentence) value)
		(format t "~&Check failed: ~A => ~A [Should be ~A]~%"
			(car sentence) value validate)))) )) ) )

;;;
;;;    3.14.5
;;;
(defun proper-listp (x)
  "Is argument a proper list?"
  (cond ((null x) t)
	((atom x) nil)
	(t (proper-listp (cdr x)))) )

; Old version:
; (defun proper-listp (l)
;   "Predicate determines whether or not its argument is a proper list."
;   (cond ((atom l) nil)   ;((not (listp l)) l) would be better here.
; 	(t (null (cdr (last l))))) )

(defun proper-listp-1 (x)
  (tailp () x) )

;;
;;    CLHS says that first arg to LAST should be a list, but it also
;;    provides a sample possible definition of LAST which indicates
;;    that it works if first arg is an atom too. So the following
;;    definition appears legitimate for all types tested below.
;;    
(defun proper-listp-2 (x)
  (null (last x 0)) )

(let ((test-data '((x nil)
		   (9 nil)
		   ((a b c) t)
		   (nil t)
		   ((a b (c) ((d) e)) t)
		   ((a b . c) nil))))
  (defun test-proper-listp ()
    (let ((functions '(proper-listp proper-listp-1 proper-listp-2)))
      (dolist (f functions)
	(format t "~&Testing ~A~%" f)
	(dolist (test test-data)
	  (let ((validate (cadr test))
		(value (funcall f (car test))))
	    (if (equal validate value)
		(format t "~&Good check: ~A => ~A~%" (car test) value)
		(format t "~&Check failed: ~A => ~A [Should be ~A]~%"
			(car test) value validate)))) )) ) )



;;;
;;;    3.14.6
;;;    (This problem is ill-defined for nested lists.)
;;;    
(defun last-atom-cheap (l)
  (cond ((proper-listp l) (car (last l)))
	(t (last l 0))) )

;;
;;    Returns last element in list or dotted list. This could be a list itself
;;    in a nested list. Atomic arguments are returned as is.
;;    
(defun last-atom (l)
  "Return the last atom in the given list argument."
  (cond ((atom l) l)
	((null (cdr l)) (car l))
	(t (last-atom (cdr l)))) )

;;
;;    Returns last atom in list regardless of nesting. Atomic arguments are
;;    treated as invalid inputs.
;;    
(defun last-atom-1 (l)
  (cond ((not (listp l)) nil)
	((proper-listp l)
	 (let ((last-elt (car (last l))))
	   (if (atom last-elt)
	       last-elt
	       (last-atom-1 last-elt))))
	(t (cdr (last l)))) )

(let ((test-data '(((a b c) c)
		   ((1 2 3 . 4) 4)
		   (("Pung" "Foo" ("Bar" "Baz")) "Baz"))))
  (defun test-last-atom ()
    (let ((functions '(last-atom-cheap last-atom last-atom-1)))
      (dolist (f functions)
	(format t "~&Testing ~A~%" f)
	(dolist (l test-data)
	  (let ((validate (cadr l))
		(value (funcall f (car l))))
	    (if (equal validate value)
		(format t "~&Good check: ~A => ~A~%" (car l) value)
		(format t "~&Check failed: ~A => ~A [Should be ~A]~%"
			(car l) value validate)))) )) ) )


;;;
;;;    3.14.7
;;;    (The first two return a list in the opposite order of built-in pairlis.
;;;     The third returns a list in the same order.)
;;;
(defun pairlis-1 (keys vals)
  "Create an association list of corresponding pairs of elements from two argument lists."
  (mapcar #'cons keys vals) )

(defun pairlis-2 (keys vals)
  (cond ((or (null keys) (null vals)) nil)
	(t (cons (cons (car keys)
		       (car vals))
		 (pairlis-2 (cdr keys) (cdr vals)))) ) )

(defun pairlis-2a (keys vals)
  (cond ((or (null keys) (null vals)) nil)
	(t (acons (car keys)
		  (car vals)
		  (pairlis-2a (cdr keys) (cdr vals)))) ) )

(defun pairlis-3 (keys vals &optional result)
  (cond ((or (null keys) (null vals)) result)
	(t (pairlis-3 (cdr keys) (cdr vals) (cons (cons (car keys)
							(car vals))
						  result)))) )

(let ((test-data '(((a b c)
		    (1 2 3)
		    ((a . 1) (b . 2) (c . 3)))
		   (((a) (b) (c))
		    ((1) (2) (3))
		    (((a) 1) ((b) 2) ((c) 3)))
		   (("Pung" "Bar")
		    ("Foo" "Baz")
		    (("Pung" . "Foo") ("Bar" . "Baz")))
		   ((usa germany france)
		    (|washington, d.c.| berlin paris)
		    ((usa . |washington, d.c.|)
		     (germany . berlin)
		     (france . paris)))) ))

  ;;
  ;;    To test for 'correctness' of output we consider the lists as sets,
  ;;    i.e., ordering is not important.
  ;;    
  (defun test-pairlis ()
    (let ((functions '(pairlis-1 pairlis-2 pairlis-2a pairlis-3)))
      (dolist (f functions)
	(format t "~&Testing ~A~%" f)
	(dolist (pair test-data)
	  (let* ((keys (car pair))
		 (vals (cadr pair))
		 (validate (caddr pair))
		 (value (funcall f keys vals)))
	    (if (or (equal (intersection validate value :test #'equal)
			   value)
		    (equal (intersection validate value :test #'equal)
			   validate))
		(format t "~&Good check: ~A ~A => ~A~%" keys vals value)
		(format t "~&Check failed: ~A ~A => ~A [Should be ~A]~%"
			keys vals value validate)))) )) ) )
	    

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex children)
  (pairlis '(name age weight sex children)
	   (list name age weight sex children)) )

(defun get-name (person)
  (cdr (assoc 'name person)) )

(defun get-age (person)
  (cdr (assoc 'age person)) )

(defun get-weight (person)
  (cdr (assoc 'weight person)) )

(defun get-sex (person)
  (cdr (assoc 'sex person)) )

(defun get-children (person)
  (cdr (assoc 'children person)) )


;;;
;;;    3.14.9
;;;    
(defun make-person2 (name age weight sex children)
  (setf (symbol-plist name) (list 'age age
				  'weight weight
				  'sex sex
				  'children children)) )

(defun make-person2b (name age weight sex children)
  (setf (symbol-plist name) (mapcan #'list
				    '(age weight sex children)
				    (list age weight sex children))) )
				    
(defun get-age2 (person)
  (get person 'age) )

(defun get-weight2 (person)
  (get person 'weight) )

(defun get-sex2 (person)
  (get person 'sex) )

(defun get-children2 (person)
  (get person 'children) )

;;;
;;;    3.14.10
;;;    
