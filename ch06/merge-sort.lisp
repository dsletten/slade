;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               merge-sort.lisp
;;;
;;;   STARTED:            Thu Jan 17 00:07:16 2002
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
;;;   References - Horowitz/Sahni pg. 352
;;;                Java Data Structures/Algorithms pg. 448
;;;                Mastering Algorithms with C pg. ?
;;;                Foundations of CS pg. 68-78, 83, 124-132
;;;
;;;
(defun lmerge (l1 l2)
  (cond ((null l1) l2)
	((null l2) l1)
	((inorderp (car l1) (car l2))
	 (cons (car l1) (lmerge (cdr l1) l2)))
	(t (cons (car l2) (lmerge l1 (cdr l2)))) ) )

(defun inorderp (x y)
  (typecase x
    (number (<= x y))
    (character (char<= x y))) )

; (defun msort (l)
;   (msort-aux l (length l)) )

; (defun msort-aux (l length)
;   (cond ((<= length 1) l)
; 	(t (partition l (truncate length 2) length))) )

; (defun partition (l mid length)
;   (lmerge (msort-aux (subseq l 0 mid) mid)
; 	  (msort-aux (subseq l mid) (- length mid))) )

(defun msort (l)
  (if (null (cdr l)) ; I.e., (<= (length l) 1)
      l
      (multiple-value-call #'msort-aux (partition l))))

(defun msort-aux (l1 l2)
  (lmerge (msort l1)
	  (msort l2)) )

(defun partition (l)
 (partition-aux l '() '()) )

;;;
;;;    This was suggested by the example in 'Foundations'.
;;;    Split a list into odd/even-numbered elements.
;;;    
(defun partition-aux (l l1 l2)
  (if (null l)
      (values l1 l2)
      (partition-aux (cdr l) l2 (cons (car l) l1))))

(defun vec-msort (v)
  (let ((len (length v)))
    (if (= len 1)
	v
	(vec-merge (vec-msort (subseq v 0 (ceiling len 2)))
		   (vec-msort (subseq v (ceiling len 2)))) )))

(defun vec-merge (v1 v2)
  (do ((result (make-array (+ (length v1) (length v2))))
       (i 0 (1+ i))
       (i1 0)
       (i2 0))
      ((= i (length result)) result)
    (cond ((= i2 (length v2))
	   (setf (aref result i) (aref v1 i1))
	   (incf i1))
	  ((= i1 (length v1))
	   (setf (aref result i) (aref v2 i2))
	   (incf i2))
	  ((inorderp (aref v1 i1) (aref v2 i2))
	   (setf (aref result i) (aref v1 i1))
	   (incf i1))
	  (t
	   (setf (aref result i) (aref v2 i2))
	   (incf i2)))) )

(defun seq-msort (seq)
  (let ((len (length seq)))
    (if (= len 1)
	seq
	(seq-merge (seq-msort (subseq seq 0 (ceiling len 2)))
		   (seq-msort (subseq seq (ceiling len 2)))) )))

(defun seq-merge (seq1 seq2)
  (do ((result (make-sequence (typecase seq1 ;Is this the right way to do this?
				(list 'list)
				(string 'string)
				(vector 'vector))
			      (+ (length seq1) (length seq2))))
       (i 0 (1+ i))
       (i1 0)
       (i2 0))
      ((= i (length result)) result)
    (cond ((= i2 (length seq2))
	   (setf (elt result i) (elt seq1 i1))
	   (incf i1))
	  ((= i1 (length seq1))
	   (setf (elt result i) (elt seq2 i2))
	   (incf i2))
	  ((inorderp (elt seq1 i1) (elt seq2 i2))
	   (setf (elt result i) (elt seq1 i1))
	   (incf i1))
	  (t
	   (setf (elt result i) (elt seq2 i2))
	   (incf i2)))) )

(defun msort-2 (l)
  (msort-aux-2 l '()))

(defun msort-aux-2 (l result)
  (if (null l)
      result
      (msort-aux-2 (cdr l) (lmerge (list (car l)) result))))
