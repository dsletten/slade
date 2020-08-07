;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Tue Apr 27 03:57:22 2004
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
(load "/Users/dsletten/lisp/programs/utils.lisp")

;;;
;;;    Fix Slade's bullshit version.
;;;    
(defun string-searchp (pattern text)
  (cond ((not (and (stringp pattern) (stringp text))) nil)
	((string= text "") (string= pattern ""))
	((string= pattern "") (string= text ""))
	(t (let* ((ch0 (char pattern 0))
		  (plen (length pattern))
		  (last-chance (- (length text) plen)))
	     (labels ((search-aux (i)
			(let ((index (position ch0 text :start i)))
			  (cond ((null index) nil)
				((> index last-chance) nil)
				((string= pattern text :start2 index
					  :end2 (+ index plen)) index)
				(t (search-aux (1+ index)))) )))
	       (search-aux 0)))) ))

;;;
;;;    From scratch
;;;    
(defun string-searchp-1 (pattern text)
  (cond ((not (and (stringp pattern) (stringp text))) nil)
	((string= text "") (string= pattern ""))
	((string= pattern "") (string= text ""))
	(t (let ((ch0 (char pattern 0))
		 (plen (length pattern))
		 (tlen (length text)))
	     (labels ((search-aux (i)
			(cond ((= i tlen) nil)
			      ((char= (char text i) ch0)
			       (if (match 0 i) ;Could really start with
					       ;1, (1+ i).
				   i
				   (search-aux (1+ i))))
			      (t (search-aux (1+ i)))))
		      (match (p-i t-i)
			(cond ((= p-i plen) t-i)
			      ((= t-i tlen) nil) ;Could stop completely here.
			      ((char= (char pattern p-i) (char text t-i))
			       (match (1+ p-i) (1+ t-i)))
			      (t nil))))
	       (search-aux 0)))) ))

;;;
;;;    6.8.2
;;;
(defun lastchar (s)
  (char s (1- (length s)))) ;<-- Fails w/ ""

(defun capitalize (s)
  (string-capitalize s :end 1))

;;
;;    How to do this w/o SUBSEQ?
;;    Only alternative is to find length of each string...But not an issue
;;    with strings as it is with lists?
;;    MAP will stop on shorter one--can't tell if other is longer.
;;    
(defun my-string-equalp (s1 s2)
  (cond ((string= s1 "") (string= s2 ""))
	((string= s2 "") nil)
	((char-not-equal (char s1 0) (char s2 0)) nil)
	(t (my-string-equalp (subseq s1 1) (subseq s2 1)))) )

(defun my-string-equalp-2 (s1 s2)
  (if (/= (length s1) (length s2))
      nil
      (every #'identity (map 'list #'(lambda (ch1 ch2)
				       (char-equal ch1 ch2))
			     s1 s2))))

(defun my-string-equalp-3 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (if (/= l1 l2)
	nil
	(do ((i 0 (1+ i)))
	    ((= i l1) t)
	  (when (char-not-equal (char s1 i) (char s2 i))
	    (return nil)))) ))

(defun my-string-equalp-4 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (if (/= l1 l2)
	nil
	(dotimes (i l1 t)
	  (when (char-not-equal (char s1 i) (char s2 i))
	    (return nil)))) ))

(defun my-string-lessp (s1 s2)
;  (cond ((string= s1 "") t) <--string-not-greaterp!
  (cond ((string= s1 "") (not (string= s2 "")))
	((string= s2 "") nil)
	((char-lessp (char s1 0) (char s2 0)) t)
	((char-greaterp (char s1 0) (char s2 0)) nil)
	(t (my-string-lessp (subseq s1 1) (subseq s2 1)))) )

;; (defun my-string-lessp-2 (s1 s2)
;;   (if (<= (length s1) (length s2))
;;       (every #'identity (map 'list #'(lambda (ch1 ch2)
;; 				       (char-lessp ch1 ch2))
;; 			     s1 s2))

(defun my-string-lessp-3 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (do ((i 0 (1+ i)))
	(())
      (cond ((= i l1) (return (/= i l2)))
	    ((= i l2) (return nil))
	    ((char-lessp (char s1 i) (char s2 i)) (return t))
	    ((char-equal (char s1 i) (char s2 i)) t) ;;...
	    ((char-greaterp (char s1 i) (char s2 i)) (return nil)))) ))

(defun my-string-lessp-4 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (labels ((string-lessp-aux (i)
	       (cond ((= i l1) (/= i l2))
		     ((= i l2) nil)
		     ((char-lessp (char s1 i) (char s2 i)) t)
		     ((char-greaterp (char s1 i) (char s2 i)) nil)
		     (t (string-lessp-aux (1+ i)))) ))
      (string-lessp-aux 0))))

(defun my-string-lessp-5 (s1 s2)
  (labels ((string-lessp-aux (sl1 sl2)
	     (cond ((null sl1) (not (null sl2)))
		   ((null sl2) nil)
		   ((char-lessp (car sl1) (car sl2)) t)
		   ((char-greaterp (car sl1) (car sl2)) nil)
		   (t (string-lessp-aux (cdr sl1) (cdr sl2)))) ))
    (string-lessp-aux (coerce s1 'list) (coerce s2 'list))))
  
(defun my-string-lessp-6 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2))
	(i 0))
    (while t
      (cond ((= i l1) (return (/= i l2)))
	    ((= i l2) (return nil))
	    ((char-lessp (char s1 i) (char s2 i)) (return t))
	    ((char-equal (char s1 i) (char s2 i)) (incf i))
	    ((char-greaterp (char s1 i) (char s2 i)) (return nil)))) ))

(defun my-string-lessp-7 (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2))
	(i 0))
    (loop
     (cond ((= i l1) (return (/= i l2)))
	   ((= i l2) (return nil))
	   ((char-lessp (char s1 i) (char s2 i)) (return t))
	   ((char-equal (char s1 i) (char s2 i)) (incf i))
	   ((char-greaterp (char s1 i) (char s2 i)) (return nil)))) ))

(dolist (f '(my-string-lessp ;my-string-lessp-2
	     my-string-lessp-3 my-string-lessp-4
	     my-string-lessp-5 my-string-lessp-6
	     my-string-lessp-7))
  (test f
	'((("alpha" "beta") t)
	  (("beta" "alpha") nil)
	  (("alpha" "BETA") t)
	  (("alphabet" "alphabet") nil)
	  (("alphabet" "alphabetize") t)
	  (("alphabetize" "alphabet") nil))))

;;;
;;;    6.8.3
;;;
(defun msort (l)
  (msort-aux l '()))

(defun msort-aux (l tmplist)
  (cond ((null l) (msort-aux-1 tmplist '()))
        (t (msort-aux (cdr l)
                      (msort-add (list (car l)) tmplist)))) )

;;;
;;;    See old version.
;;;    
(defun msort-aux-1 (l tmplist)
  (cond ((null l) tmplist)
        (t (msort-aux-1 (cdr l)
                        (lmerge (car l) tmplist)))) )

(defun msort-add (l tmplist)
  (cond ((null tmplist) (list l))
        ((null (car tmplist)) (cons l (cdr tmplist)))
        (t (cons '() (msort-add (lmerge l (car tmplist))
                                (cdr tmplist)))) ))

(defun lmerge (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((inorderp (car l1) (car l2))
         (cons (car l1) (lmerge (cdr l1) l2)))
        (t (cons (car l2) (lmerge l1 (cdr l2)))) ))

(defun inorderp (elt1 elt2)
  (typecase elt1
    (character (char<= elt1 elt2))
    (number (<= elt1 elt2))))


;;;
;;;    6.8.5
;;;
(defun string-reverse (s)
  (let* ((len (length s))
	 (s1 (make-string (length s))))
    (dotimes (i len s1)
      (setf (char s1 i) (char s (- len i 1)))) ))