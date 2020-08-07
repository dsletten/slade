;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Fri Jul 20 18:16:44 2007
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch06 (:use :common-lisp :test))

(in-package :ch06)

(defun string-search (pattern text)
  (if (and (stringp pattern) (stringp text))
      (let* ((pattern-length (length pattern))
	     (text-length (length text))
	     (pattern-limit (- text-length pattern-length)))
	(labels ((string-search-aux (i)
		   (cond ((> i pattern-limit) nil)
			 ((string= pattern text :start2 i :end2 (+ i pattern-length)))
			 (t (let ((j (position (char pattern 0) text :start (1+ i))))
			      (if j
				  (string-search-aux j)
				  nil)))) ))
	  (string-search-aux 0))) ; I don't like this...No reason to believe that match will occur at 0.
      nil))

(deftest test-string-search ()
  (check
   (not (string-search 10 "asdf"))
   (not (string-search "asdf" 10))
   (string-search "" "")
   (string-search "" "a")
   (not (string-search "a" ""))
   (not (string-search "foo" "ffo"))
   (string-search "foo" "ffoo")
   (string-search "foo" "foo")
   (not (string-search "pickler" "peter piper picked a peck of pretty pickled peppers"))
   (string-search "pickle" "peter piper picked a peck of pretty pickled peppers")))

;;;
;;;    200319
;;;    
(defun string-search (pattern text)
  (if (and (stringp pattern) (stringp text))
      (cond ((string= pattern "") t) ; Empty patern matches anything.
	    ((string= text "") nil)  ; No non-empty pattern matches empty text.
	    (t (let* ((first-char (char pattern 0))
		      (pattern-length (length pattern))
		      (text-length (length text))
		      (pattern-limit (- text-length pattern-length)))
		 (labels ((string-search-aux (i)
			    (cond ((> i pattern-limit) nil)
				  (t (let ((j (position first-char text :start i)))
				       (if (null j)
					   nil
					   (or (string= pattern text :start2 j :end2 (+ j pattern-length)) 
					       (string-search-aux (1+ j)))) )))) )
		   (string-search-aux 0)))) )
      nil))


