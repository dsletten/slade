;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch07p.lisp
;;;;
;;;;   Started:            Fri Oct 22 16:50:38 2004
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

(defpackage ch07p (:use common-lisp) (:shadow format))

(in-package ch07p)

;;;
;;;    Ex. 7.9.8
;;;
(defun format (stream control-string &rest args)
  (write-string (format-replace control-string args 0) stream)
  nil)

(defun format-replace (control-string args args-index)
  (let ((twiddle (position #\~ control-string)))
    (if twiddle
	(concatenate 'string
		     (subseq control-string 0 twiddle)
		     (replace-directive (subseq control-string twiddle)
					args
					args-index))
	control-string)))

(defun replace-directive (control-string args args-index)
  (let* ((seq-index 2)
	 (this-arg (nth args-index args))
	 (next-arg-index (1+ args-index))
	 (replacement (case (char-upcase (char control-string 1))
			(#\% (decf next-arg-index) (string #\Newline))
			(#\~ (decf next-arg-index) "~")
			(#\P (pluralizep this-arg))
			(#\@ (cond ((char-equal (char control-string 2) #\P)
				    (incf seq-index)
				    (special-pluralizep this-arg))
				   (t "")))
			(#\: (decf next-arg-index)
			     (incf seq-index)
			     (case (char-upcase (char control-string 2))
			       (#\P (pluralizep (nth (1- args-index) args)))
			       (#\@ (cond ((char-equal (char control-string 3)
						       #\P)
					   (incf seq-index)
					   (special-pluralizep
					    (nth (1- args-index) args)))
					  (t "")))) )
			(#\D (write-to-string this-arg))
			(#\B (write-to-string this-arg :base 2))
			(#\O (write-to-string this-arg :base 8))
			(#\X (write-to-string this-arg :base 16))
			(#\A (princ-to-string this-arg))
			(#\S (prin1-to-string this-arg))
			(#\C (string this-arg)))) )
    (concatenate 'string
		 replacement
		 (format-replace (subseq control-string seq-index)
				 args next-arg-index))))

(defun pluralizep (n)
  (if (= n 1) "" "s"))

(defun special-pluralizep (n)
  (if (= n 1) "y" "ies"))

;;;
;;;    7.9.10
;;;
(defun revise-file (in-filename out-filename target-string replacement)
  (with-open-file (in in-filename)
    (with-open-file (out out-filename
			 :direction :output
			 :if-exists :supersede)
      (loop for line = (read-line in nil nil)
	    while line
	    do (write-line
		(replace-line line target-string replacement) out)))) )

(defun replace-line (line target replacement)
  (let ((new (replace-target line target replacement)))
    (unless (string= line new)
      (common-lisp:format t "old: ~A~%new: ~A~2%" line new))
    new))

(defun replace-target (s target replacement)
  (let ((pos (search target s)))
    (if pos
	(concatenate 'string
		     (subseq s 0 pos)
		     replacement
		     (replace-target (subseq s (+ pos (length target)))
				     target
				     replacement))
	s)))

;;;
;;;    7.9.11
;;;
(defun compare-file (file1 file2)
  (with-open-file (f1 file1)
    (with-open-file (f2 file2)
      (loop for line1 = (read-line f1 nil nil)
	    for line2 = (read-line f2 nil nil)
	    while (or line1 line2)
	    do (unless (string= line1 line2)
		 (when line1 (common-lisp:format t "1: ~S~%" line1))
		 (when line2 (common-lisp:format t "2: ~S~2%" line2)))) )))

;;;
;;;    7.9.12
;;;
;;;    Slade's specification is unclear. My version expects the comparison
;;;    to be indicated by a symbol naming a string, character, or numeric
;;;    inequality test (either case-sensitive or -insensitive for strings/
;;;    characters). His appears to take an actual function arg, but since
;;;    it is passed in a quoted list it is unevaluated. Thus, #'string< simply
;;;    represents the list (function string<). I'm just eliminating the list.
;;;
;;;    I also use bounding indices to delimit fields rather than an index and
;;;    a length as Slade does. Thus, his example becomes:
;;;    (filesort "names_space.txt" "names_space.out"
;;;              '((11 20 string-lessp) (0 11 string-lessp)))
;;;
;;;    My version accepts an arbitrary number of criteria by which to sort.
;;;
;;;    The basic idea is to take each (inequality) test arg in turn and compare
;;;    using an appropriate equality predicate. If the two elements are equal
;;;    in the given sense, then the next test is used as a tiebreaker. If the
;;;    two elements aren't equal with respect to the first test, then we simply
;;;    apply the given test.
;;;    
(defun get-equality-test (pred)
  (ecase pred
    ((string< string>) #'string=)
    ((string-lessp string-greaterp) #'string-equal)
    ((char< char>) #'char=)
    ((char-lessp char-greaterp) #'char-equal)
    ((< >) #'=)))

(defun filesort (in-filename out-filename sort-criteria)
  (with-open-file (in in-filename)
    (with-open-file (out out-filename :direction :output
			 :if-exists :supersede)
      (let* ((in-lines (loop for line = (read-line in nil nil)
			     while line collect line))
 	     (out-lines (sort in-lines (sort-by-criteria sort-criteria))))
	(dolist (line out-lines)
	  (write-line line out)))) ))

(defun sort-by-criteria (criteria)
  (destructuring-bind (start end test) (car criteria)
    (ecase test
      ((string< string> string-lessp string-greaterp)
       (build-string-test start end test (rest criteria)))
      ((char< char> char-lessp char-greaterp)
       (build-char-test start test (rest criteria)))
      ((< >)
       (build-numeric-test start end test (rest criteria)))) ))

(defun build-string-test (start end test other-criteria)
  (cond ((null other-criteria)
	 #'(lambda (a b)
	     (funcall test (subseq a start end) (subseq b start end))))
	(t #'(lambda (a b)
	       (let ((s1 (subseq a start end))
		     (s2 (subseq b start end)))
		 (if (funcall (get-equality-test test) s1 s2)
		     (funcall (sort-by-criteria other-criteria) a b)
		     (funcall test s1 s2)))) )))

(defun build-char-test (index test other-criteria)
  (cond ((null other-criteria)
	 #'(lambda (a b)
	     (funcall test (char a index) (char b index))))
	(t #'(lambda (a b)
	       (let ((ch1 (char a index))
		     (ch2 (char b index)))
		 (if (funcall (get-equality-test test) ch1 ch2)
		     (funcall (sort-by-criteria other-criteria) a b)
		     (funcall test ch1 ch2)))) )))

;;;
;;;    Error-checking for invalid numbers?
;;;    
(defun build-numeric-test (start end test other-criteria)
  (cond ((null other-criteria)
	 #'(lambda (a b)
	     (funcall test
		      (read-from-string (subseq a start end))
		      (read-from-string (subseq b start end)))) )
	(t #'(lambda (a b)
	       (let ((n1 (read-from-string (subseq a start end)))
		     (n2 (read-from-string (subseq b start end))))
		 (if (funcall (get-equality-test test) n1 n2)
		     (funcall (sort-by-criteria other-criteria) a b)
		     (funcall test n1 n2)))) )))

;;;
;;;    7.9.13
;;;
(in-package common-lisp-user)
(defun eliza ()
  (format t "~&Hi.~2%")
  (process-reply)
  (format t "End of ELIZA.~2%"))

;;;
;;;    Assume user will get bored before stack overflows?!
(defun process-reply ()
  (format t "--> ")
  (let ((reply (read)))
    (cond ((member reply '(nil q quit)) nil)
	  ((not (listp reply))
	   (format t "~&*** Give input as a (list). Type Q to quit. ***~%")
	   (process-reply))
	  ((script-match reply *master-script2*)
;	  ((script-match reply *master-script*)
	   (process-reply))
	  (t nil)))) ;?!

(defun script-match (reply script)
  (cond ((null script) nil)
	((let ((match (matchp (caar script) reply)))
	   (cond (match (fix-reply match (cadar script)))
		 (t (script-match reply (cdr script)))) ))))

;;;
;;;    What are we 'fixing' here? (See next exercise)
;;;    Why MATCH parameter?!
;;;    
(defun fix-reply (match response)
  (format t "~A~%" response)
  t)

(defparameter *master-script*
  '(((*wild* laundry *wild*)
     (When my clothes get too dirty I just burn them.))
    ((I am *wild*)
     (Do you think I care about that))
    ((I want you *wild*)
     (Why do you want me *wild*))
    ((Do you *wild*)
     (Why should you care about me))
    ((*wild* year *wild*)
     (If I'm lucky I'll graduate before the turn of the century.))
    ((*wild* mother *wild*)
     (Don't make any cracks about my mother. She's a saint.))
    ((My name *wild*)
     (Glad to meet you. My friends call me Dr. Death.))
    ((No *wild*)
     (Well pardon me for living.))
    ((*wild* sick)
     (I think this room has lead paint. It makes you crazy.))
    ((*wild*)
     (Really.))))

;;;
;;;    4.7.12
;;;
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
	  (pattern)
	  "PATTERN should be a list.")
  (assert (listp input)
	  (input)
	  "INPUT should be a list.")
  (labels ((matchp-aux (p i)
	     (cond ((null p) (null i))
		   ((null i) (equal p '(*wild*)))
		   ((eql (car p) (car i)) (matchp-aux (cdr p) (cdr i)))
		   ((eql (car p) '*wild*)
		    (or (matchp-aux (cdr p) i)
			(matchp-aux p (cdr i))))
		   (t nil))))
    (matchp-aux pattern input)))

(defparameter *master-script2*
  '(((*wild* name *wild*)
     (I am hans and this is franz and we are here to pump you up!))
    ((*wild* weak *wild*)
     (We are not impressed by your flabitude))
    ((*wild* strong *wild*)
     (We are not girlie men))
    ((*wild* gym *wild*)
     (Oh so you are pumped up too))
    ((*wild* you are *wild*)
     (Why do you think we care what you think? Ja...))
    ((can i *wild*)
     (no you are too much of a girlie man))
    ((do you *wild*)
     (of course not))
    ((will you *wild*)
     (We will have to ask our cousin Arnold.))
    ((are you *wild*)
     (Nevermind. We are not talking about ourselves all of the time here.))
    ((i am *wild*)
     (Oh listen to the girlie man complaining.))
    ((*wild* problem *wild*)
     (Are you looking for trouble? Because we have found it for you.))
    ((what *wild*)
     (Do you always ask so many girlie questions?))
    ((when *wild*)
     (We would never tell that to someone like you))
    ((*wild* again)
     (We are not afraid of repeating ourselves.))
    ((*wild* how *wild*)
     (How? Why do you expect us to know?))
    ((who *wild*)
     (Maybe Franz. Maybe me.))
    ((you *wild*)
     (Listen to me now and here me later.
      We are not afraid of your flabby taunting.))
    ((*wild*)
     (Wait until we're done with this set.))))

;;;
;;;    7.9.14
;;;    Had to use EQL rather than EQUAL so that things like (DON'T) can match,
;;;    i.e. (DON (QUOTE T))
;;;    
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
	  (pattern)
	  "PATTERN should be a list.")
  (assert (listp input)
	  (input)
	  "INPUT should be a list.")
  (labels ((matchp-aux (p i)
	     (cond ((null p) (null i))
		   ((null i) (equal p '(*wild*)))
		   ((equal p '(*wild*)) i)
		   ((equal (car p) (car i)) (matchp-aux (cdr p) (cdr i)))
		   ((equal (car p) '*wild*)
		    (or (matchp-aux (cdr p) i)
			(matchp-aux p (cdr i))))
		   (t nil))))
    (matchp-aux pattern input)))

;;;
;;;    Modified based on Slade's version. If there are more than one *wild*
;;;    instance in a pattern, all matching elements get put in one list:
;;;    (matchp '(*wild* like *wild* coach) '(i don't like my crazy coach)) =>
;;;      (I DON 'T MY CRAZY)
;;;    
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
	  (pattern)
	  "PATTERN should be a list.")
  (assert (listp input)
	  (input)
	  "INPUT should be a list.")
  (labels ((matchp-aux (p i)
	     (cond ((null p) (null i))
		   ((null i) (equal p '(*wild*)))
		   ((equal p '(*wild*)) i)
		   ((equal (car p) (car i)) (matchp-aux (cdr p) (cdr i)))
		   ((equal (car p) '*wild*)
		    (or (matchp-aux (cdr p) i)
			(let ((result (matchp-aux p (cdr i))))
			  (if (not (null result))
			      (if (listp result)
				  (cons (car i) result)
				  (list (car i)))
			      nil))))
		   (t nil))))
    (matchp-aux pattern input)))

(defun fix-reply (match response)
  (format t "~A~%" (pronoun-shift response))
  t)

;; (let ((person-map '((i you)
;; 		    (you i)
;; 		    (me you)
;; 		    (my your)
;; 		    (your my)
;; 		    (mine yours)
;; 		    (yours mine)
;; 		    (am are)
;; 		    (are am)
;; 		    (was were)
;; 		    (were was))))
;;   (defun person-shift (sentence)
;;     (cond ((endp sentence) '())
;; 	  (t (let ((map (assoc (first sentence) person-map)))
;; 	       (if map
;; 		   (cons (second map) (person-shift (rest sentence)))
;; 		   (cons (first sentence)
;; 			 (person-shift (rest sentence)))) )))) )

;;;
;;;    Build map and its inverse. Some problems since not really injective:
;;;    I -> you, me -> you
;;;    E.g., (person-shift '(i gave the book to you)) =>
;;;            (YOU GAVE THE BOOK TO I)
;;;            
(let ((person-map (mapcan #'(lambda (l)
			      (list (cons (first l) (second l))
				    (cons (second l) (first l))))
			  '((i you)
			    (me you)
			    (my your)
			    (mine yours)
			    (am are)
			    (was were)))) )
  (defun person-shift (sentence)
    (sublis person-map sentence)))
