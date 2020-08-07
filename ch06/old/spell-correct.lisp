;#!/usr/local/bin/clisp

;;
;   NAME:               spell-correct.lisp
;
;   STARTED:            011007
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

(defun spell-correct (word)
  (if (symbolp word)
      (setf word (string word)))
;   (if (not (stringp word))
;       (setf word (format nil "~A" word)))
  (let ((word-length (length word)))
    (cond ((string= word "") nil)
	  ((spell-check word))
	  ((char-delete-p word word-length))
	  ((char-transpose-p word (1- word-length)))
	  ((char-double-p word word-length))
	  ((char-insert-p word word-length))
	  (t nil))) )

(defun spell-check (word)
  (if (stringp word)
      (setf word (intern word)))
  (cond ((get word 'isa-word) word)
	(t nil)) )

;;;
;;;    This works from end back towards start of string.
;;;    
(defun char-delete-p (word-string index)
  (cond ((zerop index) nil)
	((let ((new-word (concatenate 'string
				      (subseq word-string 0 (1- index))
				      (subseq word-string index))))
	   (spell-check new-word)))
	(t (char-delete-p word-string (1- index)))) )

;;;
;;;    Return copy of string with the substring from INDEX-1 to INDEX-2
;;;    (inclusive) reversed.
;;;
;;;    Example: (string-swap "hello there" 3 5) => "hel olthere"
;;;    
; (defun string-swap (word-string index-1 index-2)
;   (if (< index-2 index-1)
;       (rotatef index-1 index-2))
;   (concatenate 'string
; 	       (subseq word-string 0 index-1)
; 	       (reverse (subseq word-string index-1 (1+ index-2)))
; 	       (subseq word-string (1+ index-2))) )

;;;
;;;    Return copy of string with the characters at INDEX-1 and INDEX-2
;;;    swapped.
;;;
;;;    Example: (string-swap "hello there" 3 5) => "hel olthere"
;;;
;;;    (Do some kind of error-checking...)
;;;
;*** - Attempt to modify a read-only string: "COMUPTER"
;(defun string-swap (word-string index-1 index-2)
;  (rotatef (elt word-string index-1) (elt word-string index-2))
;    word-string) 

(defun string-swap (word-string index-1 index-2)
  (let ((string-copy (copy-seq word-string)))      ;Why???????????????  <-- Can't modify symbol's name!
    (rotatef (elt string-copy index-1) (elt string-copy index-2))
    string-copy) )

(defun char-transpose-p (word-string index)
  (cond ((zerop index) nil)
	((spell-check (string-swap word-string index (1- index))))
	(t (char-transpose-p word-string (1- index)))) )

(defun string-insert (s ch i)
  (cond ((or (minusp i)
	     (> i (length s)))
	 'string-insert-index-error)
	(t (concatenate 'string
			(subseq s 0 i)
			(string ch)
			(subseq s i)))) )

(defun char-double-p (word-string index)
  (cond ((zerop index) nil)
	((spell-check (string-insert word-string
				     (elt word-string (1- index))
				     index)))
	(t (char-double-p word-string (1- index)))) )

(defun char-insert-p (word-string index)
  (cond ((minusp index) nil)
	((char-insert-check word-string index #\A))
	(t (char-insert-p word-string (1- index)))) )

(defun char-insert-check (word-string index new-char)
  (cond ((char> new-char #\Z) nil)
	((spell-check (string-insert word-string new-char index)))
	(t (char-insert-check word-string index (inc-char new-char)))) )

(defun inc-char (ch)
  (code-char (1+ (char-code ch))) )


(defun tag-word (word)
  (setf (get word 'isa-word) t)
  word)

(mapcar #'tag-word '(commuter computer computation computing compute computers))
