;#!/usr/local/bin/clisp

;;
;   NAME:               ch09.lisp
;
;   STARTED:            011221
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
;;;    9.12.2
;;;    
(defun i-average (numlist)
  (let ((count 0)
	(total 0))
    (loop
     (when (null numlist) (return (and (not (zerop count))
					(/ total count))))
     (incf count)
     (incf total (pop numlist)))) )

;;;
;;;    9.12.3
;;;    (See Slade's cheap-ass solution!)
;;;
;;;    This version treats lists and strings as separate types of data.
;;;    
(defun r-remove-pairs (seq)
  (typecase seq
    (string (cond ((string= seq "") "")
		  ((string= (subseq seq 1) "") seq)
		  ((string= (subseq seq 0 1)
			    (subseq seq 1 2))
		   (r-remove-pairs (subseq seq 1)))
		  (t (concatenate 'string (subseq seq 0 1)
				  (r-remove-pairs (subseq seq 1)))) ))
    (list (cond ((null seq) nil)
		((null (cdr seq)) seq)
		((eql (car seq)
		      (cadr seq))
		 (r-remove-pairs (cdr seq)))
		(t (cons (car seq) (r-remove-pairs (cdr seq)))) ))) )

;(r-remove-pairs "baa baa black sheep")
;"ba ba black shep"

;;;
;;;    This version treats strings and lists both as types of sequences.
;;;    
(defun remove-pairs (seq)
  (typecase seq
    (string (remove-pairs-from-sequence seq "" 'string))
    (list (remove-pairs-from-sequence seq nil 'list))) )

(defun remove-pairs-from-sequence (seq end-element seq-type)
  (cond ((or (equal seq end-element)
	     (equal (subseq seq 1) end-element))
	 seq)
	((equal (subseq seq 0 1)
		(subseq seq 1 2))
	 (remove-pairs-from-sequence (subseq seq 1) end-element seq-type))
	(t (concatenate seq-type (subseq seq 0 1)
			(remove-pairs-from-sequence (subseq seq 1)
						    end-element
						    seq-type))) ) )

;;;
;;;    9.12.4
;;;    (See soundex.lisp)
;;;    