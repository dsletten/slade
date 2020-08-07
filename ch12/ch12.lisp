;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch12.lisp
;;;
;;;   STARTED:            Tue Feb 26 17:49:28 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

;;;
;;;    12.6.2
;;;
(defun msort (l)
  (msort-aux l ()) )

(defun msort-aux (l temp-list)
  (cond ((null l) (msort-aux-1 temp-list ()))
	(t (msort-aux (cdr l)
		      (msort-add (list (car l)) temp-list)))) )

(defun msort-add (l temp-list)
  (cond ((null temp-list) (list l))
	((null (car temp-list)) (cons l (cdr temp-list)))
	(t (cons () (msort-add (lmerge l (car temp-list))
			       (cdr temp-list)))) ) )

(defun msort-aux-1 (l temp-list)
  (cond ((null l) temp-list)
	(t (msort-aux-1 (cdr l) (lmerge (car l) temp-list)))) )

(defun lmerge (a b)
  (cond ((null a) b)
	((null b) a)
	((inorderp (car a) (car b))
	 (cons (car a) (lmerge (cdr a) b)))
	(t (cons (car b) (lmerge a (cdr b)))) ) )

; (defun inorderp (a b)
;   (cond ((numberp (car a))
; 	 (<= (car a) (car b)))
; 	((characterp (car a))
; 	 (char<= (car a) (car b)))
; 	(t (<= (games-behind (car a) (car b)) 0))) )
; ; 	(t (<= (games-behind (car a) (car b))
; ; 	       (games-behind (car b) (car a))))) )

(load "/Users/dsletten/lisp/programs/utils.lisp")

(defgeneric inorderp (a b)
  (:documentation "Generic function to sort two objects regardless of type.")
  (:method ((a number) (b number))
	   (<= a b))
  (:method ((a character) (b character))
	   (char<= a b))
  (:method ((a string) (b string))
	   (string<= a b))
  (:method ((a symbol) (b symbol))
	   (string-lessp (string a) (string b)))
  (:method ((a cons) (b cons))
	   (inorderp (car (msort a)) (car (msort b)))) )
  
(test 'msort '((((4 2 5 3 4)) (2 3 4 4 5))
	       (((#\w #\b #\t #\p)) (#\b #\p #\t #\w))
	       ((("one" "two" "three" "four" "five"))
		("five" "four" "one" "three" "two"))
	       (((one two three four five))
		 (five four one three two))
	       ((((1 2 3) (3 4 5) (2 3 4)))
		((1 2 3) (2 3 4) (3 4 5)))
	       ((((2 1 3) (3 5 4) (3 2 4))) ;Cool!
		((2 1 3) (3 2 4) (3 5 4)))) )

;;;
;;;    12.6.5
;;;
(defstruct person
  "Structure representing a person."
  (name "")
  (age 0)
  (weight 0)
  (sex nil)
  (children ()))

;;;
;;;    12.6.7
;;;
(defgeneric g+ (a b)
  (:documentation "Generic addition function.")
  (:method ((a number) (b number))
	   (+ a b))
  (:method ((a character) (b character))
	   (format nil "~A~A" a b))
  (:method ((a string) (b string))
	   (concatenate 'string a b))
  (:method ((a list) (b list))
	   (append a b))  )

(test 'g+ '(((5 9) 14)
	    ((#\p #\f) "pf")
	    (("pung" "Foo") "pungFoo")
	    (((1 2 3) (4 5 6)) (1 2 3 4 5 6))))

;;;
;;;    12.6.8
;;;
(defgeneric g+ (a &rest args)
  (:documentation "Generic addition of arbitrary number of args.")
  (:method ((a number) &rest args)
	   (if args
	       (cond ((numberp (car args))
		      (+ a (apply #'g+ args)))
		     ((listp (car args))
		      'error)
		     (t (concatenate 'string
				     (format nil "~A" a)
				     (apply #'g+ args))))
	       a))
  (:method ((a character) &rest args)
	   (if args
	       (cond ((numberp (car args))
		      (concatenate 'string
				   (format nil "~A" a)
				   (format nil "~A" (car args))
				   (apply #'g+ (cdr args))))
		     ((listp (car args))
		      'error)
		     (t (concatenate 'string
				     (format nil "~A" a)
				     (apply #'g+ args))))
	       (format nil "~A" a))) )

;;;
;;;    12.6.10
;;;
(defun current-time ()
  (multiple-value-bind (seconds minutes hours)
      (get-decoded-time)
    (let ((am-pm (if (>= hours 12) "pm" "am")))
      (if (> hours 12) (decf hours 12))
      (format t "~2,'0D:~2,'0D:~2,'0D ~A~%" hours minutes seconds am-pm))) )
;Slade uses (mod hours 12) rather than the IF above.

;;;
;;;    12.6.11
;;;
(defun current-date ()
  (multiple-value-bind (s m h day month year day-of-week)
      (get-decoded-time)
    (let ((day-string (nth day-of-week
			   '("Monday" "Tuesday" "Wednesday" "Thursday"
			     "Friday" "Saturday" "Sunday")))
	  (month-string (nth (1- month)
			     '("January" "February" "March" "April"
			       "May" "June" "July" "August"
			       "September" "October" "November" "December"))))
      (format t "~A, ~A ~D, ~D~%" day-string month-string day year))) )

;;;
;;;    Slade's version (sort of-he screws up the CASE forms)
;;;    
; (defun current-date ()
;   (multiple-value-bind (s m h day month year day-of-week)
;       (get-decoded-time)
;       (format t "~A, ~A ~D, ~D~%"
; 	      (case day-of-week
; 		(0 "Monday")
; 		(1 "Tuesday")
; 		(2 "Wednesday")
; 		(3 "Thursday")
; 		(4 "Friday")
; 		(5 "Saturday")
; 		(6 "Sunday"))
; 	      (case month
; 		(1 "January")
; 		(2 "February")
; 		(3 "March")
; 		(4 "April")
; 		(5 "May")
; 		(6 "June")
; 		(7 "July")
; 		(8 "August")
; 		(9 "September")
; 		(10 "October")
; 		(11 "November")
; 		(12 "December"))
; 	      day year)) )
