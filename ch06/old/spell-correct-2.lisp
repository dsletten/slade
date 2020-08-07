;#!/usr/local/bin/clisp

;;
;   NAME:               spell-correct.lisp
;
;   STARTED:            (011007?) This update to spell-correct.lisp allows
;                                 for multiple possible results.
;   MODIFICATIONS:
;   040701 Experimented with different versions of CHAR-SWAP (Inspired by
;          Oz implementation).
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
;(load "/home/httpd/cgi-bin/utils")
(load "/Users/dsletten/lisp/programs/utils.lisp")

(defun spell-correct (word)
  (when (symbolp word)
      (setf word (string word)))
  (let ((word-length (length word)))
    (cond ((string= word "") nil)
	  ((spell-check word))
	  (t (sort (remove-duplicates
		    (flatten (list* (char-delete-p word word-length)
				    (char-transpose-p word (1- word-length))
				    (char-double-p word word-length)
				    (char-insert-p word word-length)
				    (char-delete-insert-p word word-length)
				    nil))
		    :test #'string-equal)
		   #'(lambda (s1 s2)
		       (string< (string s1)
				(string s2)))) ))) )

(defun correct (word)
  (let ((word-list (spell-correct word)))
    (cond ((null word-list) (format t "~&The word '~A' does not appear in the ~
                                       dictionary and no alternatives were ~
                                       found.~%" word))
	  ((atom word-list) (format t "~&The word '~A' is in the dictionary.~%"
				    word))
	  (t (format t "~&The word '~A' is not in the dictionary. The following ~
                        possible alternatives were found:~%" word)
	     (do ((i 1 (1+ i))
		  (words word-list (cdr words)))
		 ((null words) nil)
	       (format t "~5T~2D. ~(~A~)~%" i (car words)))
	     (let ((choice (1- (get-num "Please choose an alternative: "
					#'(lambda (x)
					    (<= 1 x (length word-list)))) )))
;; 	     (let ((choice (1- (get-num "Please choose an alternative: "
;; 					1 (length word-list)))) )
	       (format t "You chose '~A' rather than '~A'.~%" (nth choice word-list)
		       word)))) ) )

;(defun flatten (tree &optional results)
;  (cond ((null tree) results)
;	((atom tree) tree)
;	(t (flatten (car tree)
;		    (flatten (cdr tree)))) ) )

;;;
;;;   This kills embedded NIL's
;;;   
(defun flatten (tree)
 (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree)))) ) )

; (defun flatten (tree)
;   (cond ((and (listp tree)
; 	      (null (cdr tree))) tree)
; 	((atom tree) (list tree))
; 	(t (append (flatten (car tree))
; 		   (flatten (cdr tree)))) ) )

(defun spell-check (word)
  (when (stringp word)
      (setf word (intern word)))
  (cond ((get word 'isa-word) word)
	(t nil)) )

;;;
;;;    This works from end back towards start of string.
;;;    
(defun char-delete-p (word-string index &optional results)
  (cond ((zerop index) results)
	(t (let* ((new-word (concatenate 'string
					 (subseq word-string 0 (1- index))
					 (subseq word-string index)))
		  (check-word (spell-check new-word)))
	     (if check-word
		 (char-delete-p word-string
				(1- index)
				(cons check-word results))
		 (char-delete-p word-string (1- index) results)))) ) )

;;;
;;;    Return copy of string with the characters at INDEX-1 and INDEX-2
;;;    swapped.
;;;
;;;    Example: (char-swap "hello there" 3 5) => "hel olthere"
;;;
;;;    (Do some kind of error-checking...) See ch10.lisp
;;;
(defun char-swap1 (word-string index-1 index-2)
  (let ((i 0))
    (map 'string
	 #'(lambda (ch)
	     (prog1
		 (cond ((= i index-1) (elt word-string index-2))
		       ((= i index-2) (elt word-string index-1))
		       (t ch))
	       (incf i)))
	 word-string)))

(defun char-swap2 (word-string index-1 index-2)
  (let ((s (make-string (length word-string))))
    (dotimes (i (length s) s)
      (setf (char s i)
	    (cond ((= i index-1) (char word-string index-2))
		  ((= i index-2) (char word-string index-1))
		  (t (char word-string i)))) )))

(defun char-swap3 (word-string index-1 index-2)
  (let ((s (loop for i from 0 to (1- (length word-string))
		 collect (cond ((= i index-1) (elt word-string index-2))
			       ((= i index-2) (elt word-string index-1))
			       (t (elt word-string i)))) ))
    (coerce s 'string)))

(defun char-swap4 (word-string index-1 index-2)
  (let ((s (make-string (length word-string))))
    (loop for i from 0 to (1- (length word-string))
	  do (setf (char s i)
		   (cond ((= i index-1) (char word-string index-2))
			 ((= i index-2) (char word-string index-1))
			 (t (char word-string i)))) )
    s))

(defun char-swap (word-string index-1 index-2)
  (let ((string-copy (copy-seq word-string)))
    (rotatef (elt string-copy index-1) (elt string-copy index-2))
    string-copy) )

(dolist (f '(char-swap char-swap1 char-swap2 char-swap3 char-swap4))
  (test f '( (("hello there" 0 1) "ehllo there")
	     (("hello there" 0 10) "eello therh")
	     (("hello there" 2 4) "heoll there")
	     (("hello there" 4 2) "heoll there")
	     (("hello there" 3 3) "hello there"))))

;;;
;;;    Test word with successive pairs transposed. Work from end towards
;;;    start of string.
;;;    
(defun char-transpose-p (word-string index &optional results)
  (cond ((zerop index) results)
	(t (let ((check-word (spell-check (char-swap word-string
						     index (1- index)))) )
	     (if check-word
		 (char-transpose-p word-string
				   (1- index)
				   (cons check-word results))
		 (char-transpose-p word-string (1- index) results)))) ) )

;;;
;;;    Return copy of string S with character CH inserted at 0-based index I.
;;;    
(defun string-insert (s ch i)
  "Insert a character into a given string following the character at the specified position."
  (cond ((or (minusp i)
	     (> i (length s)))
	 'string-insert-index-error)
	(t (concatenate 'string
			(subseq s 0 i)
			(string ch)
			(subseq s i)))) )

(defun char-double-p (word-string index &optional results)
  (cond ((zerop index) results)
	(t (let ((check-word
		  (spell-check (string-insert word-string
					      (elt word-string (1- index))
					      index))))
	     (if check-word 
		 (char-double-p word-string
				(1- index)
				(cons check-word results))
		 (char-double-p word-string (1- index) results)))) ) )

(defun char-insert-p (word-string index &optional results)
  (cond ((minusp index) results)
	(t (let ((insert-results (char-insert-check word-string index #\A)))
	     (if insert-results
		 (char-insert-p word-string (1- index) (cons insert-results results))
		 (char-insert-p word-string (1- index) results)))) ) )

(defun char-insert-check (word-string index new-char &optional results)
  (cond ((char> new-char #\Z) results)
	(t (let ((check-word (spell-check (string-insert word-string
							 new-char
							 index))))
	     (if check-word
		 (char-insert-check word-string
				    index
				    (inc-char new-char)
				    (cons check-word results))
		 (char-insert-check word-string
				    index
				    (inc-char new-char)
				    results)))) ) )

(defun inc-char (ch)
  "Return the character that follows the given input character."
  (code-char (1+ (char-code ch))) )


(defun char-delete-insert-p (word-string index &optional results)
  (cond ((zerop index) results)
	(t (let* ((cut-word (concatenate 'string
					 (subseq word-string 0 (1- index))
					 (subseq word-string index)))
		  (insert-results (char-insert-p cut-word (length cut-word))))
	     (if insert-results
		 (char-delete-insert-p word-string
				       (1- index)
				       (cons insert-results results))
		 (char-delete-insert-p word-string (1- index) results)))) ) )

(defun tag-word (word)
  (setf (get word 'isa-word) t)
  word)

(defparameter *dictionary* '() "List of properly spelled words (symbols).")

(defun read-dictionary ()
  (with-open-file (in-stream "words")
    (do ((in-line (read-line in-stream nil nil)
		  (read-line in-stream nil nil)))
	((not in-line))
      (push (read-from-string in-line) *dictionary*))))

(read-dictionary)

(mapcar #'tag-word *dictionary*)

(defun fix-spelling ()
  (format t "Enter a word to test: ")
  (let ((s (make-string-input-stream (read-line))))
    (do ()
	((not (listen s)))
      (correct (read s)))) )
