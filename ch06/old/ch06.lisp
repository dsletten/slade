;#!/usr/local/bin/clisp

;;
;   NAME:               ch06.lisp
;
;   STARTED:            011014
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
(load "~/lisp/programs/utils")
;(load "/home/httpd/cgi-bin/utils")

(defun compare-char ()
  "Demonstrate the differences between the various character comparison predicates."
  (let ((char-data '(#\a #\A #\b #\B #\c #\C))
	(functions '(char= char-equal
		     char< char-lessp
		     char> char-greaterp
		     char/= char-not-equal
		     char>= char-not-lessp
		     char<= char-not-greaterp)))
    (format t "~20T~A~%" (join (mapcar #'(lambda (ch)
					   (concatenate 'string
							"#\\"
							(string ch)))
				       char-data) "   "))
    (dolist (f functions)
      (format t "~&~A" f)
      (format t "~20T~A~%" (join (mapcar #'(lambda (x)
					     (funcall f #\B x))
					 char-data) " ")))) )


(defun compare-string ()
  "Demonstrate the differences between the various string comparison predicates."
  (let ((string-data '("alpha" "ALPHA" "beta" "Beta" "BETA" "BET" "GAMMA"))
	(functions '(string= string-equal
		     string< string-lessp
		     string> string-greaterp
		     string/= string-not-equal
		     string>= string-not-lessp
		     string<= string-not-greaterp)))
    (format t "~20T~A~%" (join string-data "   "))
    (dolist (f functions)
      (format t "~&~A" f)
      (format t "~20T~A~%" (join (mapcar #'(lambda (x)
					     (funcall f "BETA" x))
					 string-data) " ")))) )


;;;
;;;    6.8.2
;;;
;(defun lastchar (s)
;  "Return the last character in a given string."
;  (char s (1- (length s))) )

;;;
;;;    The above version fails given "".
;;;    
(defun lastchar (s)
  "Return the last character in a given string."
  (if (string= s "")
      s
      (char s (1- (length s)))) )

(defun lastchar-1 (s) ;Duh... See above.
  (if (string= s "")
      s
      (character (subseq s (1- (length s)))) ) )

(defun lastchar-2 (s)
  (cond ((zerop (length s)) s)
	((= (length s) 1) (character s))
	(t (lastchar-2 (subseq s 1)))) )

(defun lastchar-3 (s)
  (or (first (last (coerce s 'list)))
      "") )

(dolist (f '(lastchar lastchar-1 lastchar-2 lastchar-3))
  (test f
	'((("") "")
	  (("This is pung") #\g)
	  (("a") #\a))))

(defun capitalize (s)
  "Capitalize the first character of a string."
  (if (string= s "")
      s
      (concatenate 'string
		   (string (char-upcase (char s 0)))
		   (subseq s 1))) )

(defun capitalize-1 (s)
  (flet ((capitalize-aux (char-list)
	   (if (string= s "")
	       s
	       (coerce (cons (char-upcase (car char-list))
			     (mapcar #'char-downcase (cdr char-list)))
		       'string)) ))
    (capitalize-aux (coerce s 'list))) )

(defun capitalize-2 (s)
  (if (string= s "")
      s
      (substitute (char-upcase (char s 0))
		  (char s 0)
		  (map 'string #'char-downcase s)
		  :count 1
		  :test #'char-equal)) )

(dolist (f '(capitalize capitalize-1 capitalize-2))
  (test f
	'((("pung foo") "Pung foo")
	  (("scooby doo") "Scooby doo")
	  (("SCOOBY DOO") "Scooby doo")
	  (("") "")
	  (("a") "A")
	  (("100 bottles of beer on the wall")
	   "100 bottles of beer on the wall"))) )

(defun string-equalp (s1 s2)
  "Determine whether two given strings are equal without regard to case."
  (cond ((string= s1 "") (string= s2 ""))
	((string= s2 "") (string= s1 ""))
	((not (char-equal (char s1 0)
			  (char s2 0)))
	 nil)
	(t (string-equalp (subseq s1 1)
			  (subseq s2 1)))) )
;(defun my-string-equalp (s1 s2)
;  (cond ((string= s1 "") (string= s2 ""))
;	((string= s2 "") nil)
;	((char-equal (char s1 0) (char s2 0))
;	 (my-string-equalp (subseq s1 1)
;			   (subseq s2 1)))
;	(t nil)) )

(test 'string-equalp
      '((("pung" "PUNG") t)
	(("a" "A") t)
	(("" "") t)
	(("" "foo") nil)
	(("pung" "pun") nil)
	(("pung" "punk") nil)))

;(defun my-string-lessp (s1 s2)
;  "Determine whether the first of two given strings precedes the second in terms of lexicographical order."
;  (cond ((string= s1 "") t)
;	((string= s2 "") nil)
;	((char-lessp (char s1 0)
;		     (char s2 0))
;	 t)
;	((char-equal (char s1 0)
;		     (char s2 0))
;	 (my-string-lessp (subseq s1 1)
;			  (subseq s2 1)))
;	(t nil)) )

(defun my-string-lessp (s1 s2)
  (cond ((string= s1 "") (not (string= s2 "")))
	((string= s2 "") nil)
	((char-lessp (char s1 0) (char s2 0)) t)
	((char-greaterp (char s1 0) (char s2 0)) nil)
	(t (my-string-lessp (subseq s1 1)
			    (subseq s2 1)))) )
      
(test 'my-string-lessp
      '((("alpha" "beta") t)
	(("beta" "alpha") nil)
	(("alpha" "BETA") t)
	(("alphabet" "alphabetize") t)
	(("alphabetize" "alphabet") nil)))

;;;
;;;    6.8.3 Merge sort
;;;    (Assumes input L is a list of numbers or chars.)
;;;    Slade's implementation is a bizarre twist of the basic merge-sort
;;;    algorithm (which itself is very straightforward). The function
;;;    MSORT-ADD and the function MSORT-AUX-1 both call LMERGE. MSORT-ADD has
;;;    already sorted all sublists prior to MSORT-AUX-1 being called.
;;;    MSORT-AUX-1 merely merges all of the sublists.
;;;    
(defun msort (l)
  (if (or (every #'numberp l)
	  (every #'characterp l))
      (msort-aux l '())
      'error) )

;;;
;;;    Split the original list up into a list of sorted sublists.
;;;    
(defun msort-aux (l temp-list)
  (cond ((null l) (format t "~S~%" temp-list) (msort-aux-1 temp-list))
;  (cond ((null l) (format t "~S~%" temp-list) (msort-aux-1 temp-list '()))
	(t (msort-aux (cdr l)
		      (msort-add (list (car l)) temp-list)))) )

;;;
;;;    l is a single-element list when we get here from MSORT-AUX. It will
;;;    contain multiple sorted elements during recursive calls to MSORT-ADD.
;;;    
;;;    TEMP-LIST is a list of properly-sorted lists or NILs (or perhaps NIL
;;;    itself).
;;;
;;;    This function ultimately produces a list of ordered sublists prior to
;;;    the final call of MSORT-AUX. The structure of this list is based on
;;;    the length of the initial argument to MSORT. Let n be (length l). Let
;;;    k be (floor (log n 2)). Then the final list created by MSORT-ADD will
;;;    contain k + 1 sublists, each of which will contain up to
;;;    1, 2, 4, ..., (expt 2 k) elements. The contents of these sublists will
;;;    total the original list as though recording its length in binary.
;;;    Examples:
; (msort '(7 3)) => (NIL (3 7))
; (msort '(7 3 9)) =>((9) (3 7))
; (msort '(7 3 9 1)) => (NIL NIL (1 3 7 9))
; (msort '(7 3 9 1 11)) =>((11) NIL (1 3 7 9))
; (msort '(7 3 9 1 11 4)) => (NIL (4 11) (1 3 7 9))
; (msort '(7 3 9 1 11 4 5)) => ((5) (4 11) (1 3 7 9))
; (msort '(7 3 9 1 11 4 5 2)) => (NIL NIL NIL (1 2 3 4 5 7 9 11))
; (msort '(7 3 9 1 11 4 5 2 10)) => ((10) NIL NIL (1 2 3 4 5 7 9 11))
; (msort '(7 3 9 1 11 4 5 2 10 12)) => (NIL (10 12) NIL (1 2 3 4 5 7 9 11))
; (msort '(7 3 9 1 11 4 5 2 10 12 8)) => ((8) (10 12) NIL (1 2 3 4 5 7 9 11))
; (msort '(7 3 9 1 11 4 5 2 10 12 8 15)) => (NIL NIL (8 10 12 15) (1 2 3 4 5 7 9 11))
;;;    
(defun msort-add (l temp-list)
  (cond ((null temp-list) (list l))
        ((null (car temp-list)) (cons l (cdr temp-list)))
        (t (cons '() (msort-add (lmerge l (car temp-list))
                                (cdr temp-list)))) ) )

;;;
;;;    The final stage. Once we enter here we don't call any of the other
;;;    (top-level) functions before generating the result.
;;;
;;;    l is a list of lists. The sublists are either empty or are already
;;;    sorted themselves. This function CDRs down the list folding each sublist
;;;    in with the accumulator.
;;;    
(defun msort-aux-1 (l)
  (reduce #'lmerge l))

;; (defun msort-aux-1 (l temp-list)
;;   (cond ((null l) temp-list)
;; 	(t (msort-aux-1 (cdr l) (lmerge (car l) temp-list)))) )

;;;
;;;    All of the definitions below involve (not) > or <= as a test to order
;;;    elements rather than merely <. This is needed to duplicate the behavior
;;;    of Lisp's MERGE function which performs a stable sort. In other words,
;;;    given a from l1 and b from l2, a ends up in front of b in the final list
;;;    if (< a b) of course, but the same is true if (= a b).
;;;    
(defun lmerge (l1 l2)
  (lmerge-aux l1 l2 '()) )
;;;
;;;    Assumes list is homogeneous.
;;;    (Tail-recursive)
;;;    
(defun lmerge-aux (l1 l2 result)
  (cond ((null l1) (append (reverse result) l2))
	((null l2) (append (reverse result) l1))
	((characterp (car l1))
	 (if (char<= (car l1)
		     (car l2))
	     (lmerge-aux (cdr l1) l2 (cons (car l1) result))
	     (lmerge-aux l1 (cdr l2) (cons (car l2) result))))
	((numberp (car l1))
	 (if (<= (car l1)
		 (car l2))
	     (lmerge-aux (cdr l1) l2 (cons (car l1) result))
	     (lmerge-aux l1 (cdr l2) (cons (car l2) result)))) ) )

;
; Not tail-recursive
; 
; (defun lmerge (l1 l2)
;   (cond ((null l1) l2)
; 	((null l2) l1)
; 	((characterp (car l1))
; 	 (if (characterp (car l2))
; 	     (if (char> (car l1) (car l2))
; 		 (cons (car l2) (lmerge l1 (cdr l2)))
; 		 (cons (car l1) (lmerge (cdr l1) l2)))
; 	     'error))
; 	((numberp (car l1))
; 	 (if (numberp (car l2))
; 	     (if (> (car l1) (car l2))
; 		 (cons (car l2) (lmerge l1 (cdr l2)))
; 		 (cons (car l1) (lmerge (cdr l1) l2)))
; 	     'error))
; 	(t 'error)) )

;
; Mutually recursive
; 
; (defun lmerge (l1 l2)
;   (cond ((null l1) l2)
; 	((null l2) l1)
; 	(t (lmerge-aux (car l1) l1 (car l2) l2))) )

; (defun lmerge-aux (elt1 l1 elt2 l2)
;   (cond ((and (characterp elt1)
; 	      (characterp elt2))
; 	 (if (char> elt1 elt2)
; 	     (cons elt2 (lmerge l1 (cdr l2)))
; 	     (cons elt1 (lmerge (cdr l1) l2))))
; 	((and (numberp elt1)
; 	      (numberp elt2))
; 	 (if (> elt1 elt2)
; 	     (cons elt2 (lmerge l1 (cdr l2)))
; 	     (cons elt1 (lmerge (cdr l1) l2))))
; 	(t 'error)) )

;;;
;;;    Slade's version is pretty slick. Lesson: abstract out commonality,
;;;    which may involve a predicate.
;;;
(defun lmerge (a b)
  (cond ((null a) b)
	((null b) a)
	((inorderp a b)
	 (cons (car a) (lmerge (cdr a) b)))
	(t (cons (car b) (lmerge a (cdr b)))) ) ) ; Slade flips the order here!

(defun inorderp (a b)
  (cond ((numberp (car a))
	 (<= (car a) (car b)))
	((characterp (car a))
	 (char<= (car a) (car b)))) )


;;;
;;;    6.8.5
;;;
(defun string-reverse (s)
  (labels ((string-reverse-aux (s result)
	     (cond ((string= s "") result)
		   (t (string-reverse-aux (subseq s 1)
					  (concatenate 'string
						       (subseq s 0 1)
						       result)))) ))
    (string-reverse-aux s "")) )


;;;
;;;    Slade's version (paraphrased here) should be more efficient
;;;    since it only coerces once rather than concatenating repeatedly.
;;;    
;(defun string-reverse (s)
;  (labels ((string-reverse-aux (s result)
;	     (cond ((string= s "") result)
;		   (t (string-reverse-aux (subseq s 1)
;					  (cons (char s 0) result)))) ))
;    (coerce (string-reverse-aux s nil) 'string)) )

