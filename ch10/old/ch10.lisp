;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch10.lisp
;;;
;;;   STARTED:            Sun Dec 23 15:40:37 2001
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
;;;    2.11.11
;;;    
; (defun leap-yearp (year)
;   (check-type year (integer 1582 *) "an integer starting with 1582")
;   (cond ((zerop (mod year 400)))
; 	((zerop (mod year 100)) nil)
; 	(t (zerop (mod year 4)))) ) 

;
;    1600.0 is OK (vs. 1600)
;    
(defun leap-yearp (year)
  (assert (and (typep year '(real 1582 *))
	       (zerop (rem year 1)))
	  (year)
	  "Year must be an integer not less than 1582.")
  (cond ((zerop (mod year 400)))
	((zerop (mod year 100)) nil)
	(t (zerop (mod year 4)))) ) 

;;;
;;;    3.14.8
;;;
; (defun make-person (name age weight sex children)
;   (check-type name (or symbol string) "either a symbol or a string")
;   (check-type age (real (0) 120) "a positive number")
;   (check-type weight (real (0) 500) "a positive number")
;   (check-type sex (member male female m f "male" "female" "m" "f" "M" "F")
; 	      "either 'male' or 'female'")
;   (check-type children list "a list of children")
;   (pairlis '(name age weight sex children)
; 	   (list name age weight sex children)) )
;    The check for SEX above won't work as planned since MEMBER tests using
;    EQ. None of the strings will match.


; (defun make-person (name age weight sex children)
;   (assert (typep name '(or symbol string))
; 	  (name)
; 	  "Name must be either a symbol or a string.")
;   (assert (typep age '(real (0) 120))
; 	  (age)
; 	  "Age must be a reasonable positive number.")
;   (assert (typep weight '(real (0) 500))
; 	  (weight)
; 	  "Weight must be a reasonable positive number.")
;   (assert (typep sex '(member m f male female))
; 	  (sex)
; 	  "Sex must be either m/male or f/female.")
;   (assert (typep children 'list)
; 	  (children)
; 	  "Children must be a list.")
;   (pairlis '(name age weight sex children)
; 	   (list name age weight sex children)) )

(defun make-person (name age weight sex children)
  (assert (typep name '(or symbol string))
	  (name)
	  "Name must be either a symbol or a string.")
  (assert (typep age '(real (0) 120))
	  (age)
	  "Age must be a reasonable positive number.")
  (assert (typep weight '(real (0) 500))
	  (weight)
	  "Weight must be a reasonable positive number.")
  (assert (member sex '(m f male female "m" "f" "male" "female")
		  :test #'equalp)
	  (sex)
	  "Sex must be either m/male or f/female.")
  (assert (typep children 'list)
	  (children)
	  "Children must be a list.")
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
; (defun make-person2 (name age weight sex children)
;   (check-type name symbol "a symbol")
;   (check-type age (real (0) 120) "a positive number")
;   (check-type weight (real (0) 500) "a positive number")
;   (check-type sex (member male female m f "male" "female" "m" "f" "M" "F")
; 	      "either 'male' or 'female'")
;   (check-type children list "a list of children")
;   (setf (symbol-plist name) (list 'age age
; 				  'weight weight
; 				  'sex sex
; 				  'children children)) )

(defun make-person2 (name age weight sex children)
  (assert (typep name 'symbol)
	  (name)
	  "Name must be a symbol.") ;Duh! Strings don't have property lists...
  (assert (typep age '(real (0) 120))
	  (age)
	  "Age must be a reasonable positive number.")
  (assert (typep weight '(real (0) 500))
	  (weigth)
	  "Weight must be a reasonable positive number.")
  (assert (member sex '(male female m f "male" "female" "m" "f")
		  :test #'equalp)
	  (sex)
	  "Sex must be either m/male or f/female.")
  (assert (typep children 'list)
	  (children)
	  "Children must be a list of children.")
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
;;;    Function: zeller
;;;
;;;    Calculate Zeller's congruence
;;;
;;;    Input:
;;;        month - name of month (as a symbol)
;;;        day of month - 1-31
;;;        year - assumed >= 1582
;;;
;;;    Output:
;;;        Day of week as a symbol
;;;
; (let ((months       '(march april may june july august september
; 		      october november december january february))
;       (days-of-week '(sunday monday tuesday wednesday
; 		      thursday friday saturday)))

;   (deftype month-type ()
;     `(member ,@months) )  ;My first backquote!!
  
;   (defun zeller (month day-of-month year)
;     (check-type month month-type "the full name of a month")
;     (check-type day-of-month (integer 1 31)
; 		"an integer between 1 and 31 inclusive") ;Upper limit should be
; 							 ;month-specific (and
; 							 ;leap year-specific)
;     (check-type year (integer 1582 *) "an integer starting with 1582")
;     (let ((n day-of-month)
; 	  (m (month-number month months))
; 	  (c (truncate year 100))
; 	  (y (mod year 100))
; 	  (l (if (leap-yearp year) 1 0)))
;       (nth (mod (+ n
; 		   (floor (- (* (/ 13 5) m) 0.2))
; 		   y
; 		   (floor y 4)
; 		   (floor c 4)
; 		   (- (* 2 c))
; 		   (- (* (+ 1 l)
; 			 (floor m 11))))
; 		7) days-of-week)) )

;   (defun month-number (month month-list)
;     (cond ((eq month (car month-list)) 1)
; 	  (t (1+ (month-number month (cdr month-list)))) ) ))

(let ((months       '(march april may june july august september
		      october november december january february))
      (days-of-week '(sunday monday tuesday wednesday
		      thursday friday saturday))
      (days-per-month '((march 31)
			(april 30)
			(may 31)
			(june 30)
			(july 31)
			(august 31)
			(september 30)
			(october 31)
			(november 30)
			(december 31)
			(january 31)
			(february 28))))

  (deftype month-type ()
    `(member ,@months) )  ;My first backquote!!
  
  (defun zeller (month day-of-month year)
    (assert (typep month 'month-type)
	    (month)
	    "Month should name a month of the year.")
    (assert (and (typep year '(real 1582 *))
		 (zerop (rem year 1)))
	    (year)
	    "Year should be an integer no earlier than 1582.")
    (let ((days-in-month (cond ((eq month 'february)
				(if (leap-yearp year)
					   29
					   28))
			       (t (second (assoc month days-per-month)))) ))
      (assert (typep day-of-month `(integer 1 ,days-in-month))
	      (day-of-month)
	      "Day of month should be an integer between 1 and ~D."
	      days-in-month))
    (let ((n day-of-month)
	  (m (month-number month months))
	  (c (truncate year 100))
	  (y (mod year 100))
	  (l (if (leap-yearp year) 1 0)))
      (nth (mod (+ n
		   (floor (- (* (/ 13 5) m) 0.2))
		   y
		   (floor y 4)
		   (floor c 4)
		   (- (* 2 c))
		   (- (* (+ 1 l)
			 (floor m 11))))
		7) days-of-week)) )

  (defun month-number (month month-list)
    (cond ((eq month (car month-list)) 1)
	  (t (1+ (month-number month (cdr month-list)))) ) ))

;;;
;;;    4.7.4
;;;
(defconstant us-currency '((100 dollar      dollars)
			   (50  half-dollar half-dollars)
			   (25  quarter     quarters)
			   (10  dime        dimes)
			   (5   nickel      nickels)
			   (1   penny       pennies)))

; (defun make-change (money currency-list)
;   (check-type money (integer 0 *) "a non-negative integer")
;   (cond ((= money 0) nil)
; 	((>= money (caar currency-list))
; 	 (let ((n-coins (truncate money (caar currency-list))))
; 	   (cons (list n-coins (if (> n-coins 1)
; 				   (caddar currency-list)
; 				   (cadar currency-list)))
; 		 (make-change (mod money (caar currency-list))
; 			      (cdr currency-list)))) )
; 	(t (make-change money (cdr currency-list)))) )

(defun make-change (money currency-list)
  (assert (typep money '(integer 0 *))
	  (money)
	  "Money should be a non-negative integer.")
  (cond ((= money 0) nil)
	((>= money (caar currency-list))
	 (let ((n-coins (truncate money (caar currency-list))))
	   (cons (list n-coins (if (> n-coins 1)
				   (caddar currency-list)
				   (cadar currency-list)))
		 (make-change (mod money (caar currency-list))
			      (cdr currency-list)))) )
	(t (make-change money (cdr currency-list)))) )

;;;
;;;    4.7.10
;;;
;;;    One need only test balance once to determine whether it's a number.
;;;    All recursive calls involve an argument that is the output of + or *.
;;;    As long as the transactions list is a proper list it only needs to be
;;;    checked once too. Thus it is safe to remove these tests from the
;;;    recursive function.
;;;  
; (defun check-book (balance transactions)
; ;   (cond ((not (numberp balance))
; ; 	 (format t "Error--Non-numeric balance: ~A~%" balance))
; ; 	((or (not (listp transactions))
; ; 	     (not (null (last transactions 0))))
; ; 	 (format t "Error--Invalid transaction list: ~A~%" transactions))
;   (check-type balance number "a number")
;   (check-type transactions list "a list")
;   (check-book-aux balance transactions) )

(defun check-book (balance transactions)
  (assert (typep balance '(real 0 *))
	  (balance)
	  "Balance must be a non-negative number.")
  (assert (typep transactions 'list)
	  (transactions)
	  "Transactions should be a list.")
  (check-book-aux balance transactions) )

;;;
;;;    To verify that an interest rate transaction is a list of one number we
;;;    use (not (null (cdr transaction)))) to check the length rather than
;;;    (/= (length transaction) 1)). This second test would not eliminate a
;;;    list such as (1.1 . 5).
;;;    
(defun check-book-aux (balance transactions)
  (let ((transaction (car transactions)))
    (cond ((null transactions) balance)
	  ((numberp transaction) (check-book-aux (+ transaction balance)
						 (cdr transactions)))
	  ((listp transaction)
	   (let ((interest-rate (car transaction)))
	     (assert (typep interest-rate '(real 1.0 *))
		     (interest-rate)
		     "Invalid interest rate: ~A~%" interest-rate)
	     (assert (null (cdr transaction))
		     (transaction)
		     "Interest rate should be represented by single-element ~
                      list.")
	     (check-book-aux (* balance interest-rate)
			     (cdr transactions))))
	  (t (warn "Error--Invalid transaction: ~A~%" transaction)))) )

;;;
;;;    4.7.11 (Derived from solution to 4.7.10)
;;;
(defun now-account (balance transactions)
;   (cond ((not (numberp balance))
; 	 (format t "Error--Non-numeric balance: ~A~%" balance))
; 	((or (not (listp transactions))
; 	     (not (null (last transactions 0))))
; 	 (format t "Error--Invalid transaction list: ~A~%" transactions))
; 	(t (now-account-aux balance transactions))) )
  (check-type balance number "a number")
  (check-type transactions list "a list")
  (now-account-aux balance transactions) )
  
(defconstant balance-limit 500)
(defconstant penalty 0.10)

(defun now-account-aux (balance transactions)
  (let ((transaction (car transactions)))
    (cond ((null transactions) balance)
	  ((numberp transaction)
	   (if (and (minusp transaction)
		    (< balance balance-limit))
	       (now-account-aux (+ transaction balance (- penalty))
				(cdr transactions))
	       (now-account-aux (+ transaction balance)
				(cdr transactions))))
	  ((listp transaction)
	   (let ((interest-rate (car transaction)))
	     (cond ((or (not (numberp interest-rate))
			(minusp interest-rate)
			(not (null (cdr transaction))))
		    (format t "Error--Invalid interest rate: ~A~%" interest-rate))
		   ((< balance balance-limit)
		    (now-account-aux balance (cdr transactions)))
		   (t (now-account-aux (* balance interest-rate)
				       (cdr transactions)))) ))
	  (t (format t "Error--Invalid transaction: ~A~%" transaction)))) )


;;;
;;;    Return copy of string with the characters at INDEX-1 and INDEX-2
;;;    swapped.
;;;
;;;    Example: (string-swap "hello there" 3 5) => "hel olthere"
;;;
;;;    (Do some kind of error-checking...)
;;;
(defun string-swap (word-string index-1 index-2)
  (assert (typep word-string 'string)
	  (word-string)
	  "WORD-STRING should be a string.")
  (assert (typep index-1 `(integer 0 (,(length word-string))))
	  (index-1)
	  "INDEX-1 must be an integer between 0 and the length of ~
           WORD-STRING.")
  (assert (typep index-2 `(integer 0 (,(length word-string))))
	  (index-2)
	  "INDEX-2 must be an integer between 0 and the length of ~
           WORD-STRING.")
  (let ((string-copy (copy-seq word-string)))
    (rotatef (char string-copy index-1)
	     (char string-copy index-2))
    string-copy) )

;;;
;;;    Return copy of string S with character CH inserted at 0-based index I.
;;;    
(defun string-insert (s ch i)
  "Insert a character into a given string following the character at the specified position."
  (assert (typep s 'string)
	  (s)
	  "S must be a string.")
  (assert (typep ch 'character)
	  (ch)
	  "CH must be a character.")
  (assert (typep i `(integer 0 (,(length s))))
	  (i)
	  "I must be an integer between 0 and the length of the string.")
  (concatenate 'string
	       (subseq s 0 i)
	       (string ch)
	       (subseq s i)) )
