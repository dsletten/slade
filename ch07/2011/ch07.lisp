;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Mon Aug 22 22:53:29 2011
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch07 (:use :common-lisp :lang :test))

(in-package :ch07)

;;;
;;;    7.9.1
;;;
(defun column-print (text column &optional (stream *standard-output*))
  (cond ((endp text) nil)
        (t (format stream "~v,0@T~A~%" column (first text)) ; Makes everybody happy
;        (t (format stream "~v,0T~A~%" column (first text)) ; SBCL doesn't like! First line is offset by 2 chars?!
           (column-print (rest text) column stream))))

(defun reverse-length (l)
  (labels ((reverse-length-aux (l reverse length)
             (if (endp l)
                 (values reverse length)
                 (reverse-length-aux (rest l) (cons (first l) reverse) (1+ length)))) )
    (reverse-length-aux l '() 0)))

;;;
;;;    7.9.2
;;;    
;; (defun list-to-columns (l cols)
;;   (let* ((length (length l))
;;          (rows (ceiling length cols))
;;          (long-cols (rem length cols)))
;;     (labels ((collect-long-cols (l n long-cols)
;;                (cond ((endp l) '())
;;                      ((zerop long-cols) (collect-short-cols l (1- rows)))
;;                      ((zerop n) (add-new-col (collect-long-cols l rows (1- long-cols))))
;;                      (t (add-elt-to-result (first l) (collect-long-cols (rest l) (1- n) long-cols)))) )
;;              (collect-short-cols (l n)
;;                (cond ((endp l) '())
;;                      ((zerop n) (add-new-col (collect-short-cols l (1- rows))))
;;                      (t (add-elt-to-result (first l) (collect-short-cols (rest l) (1- n)))) ))
;;              (add-elt-to-result (elt result)
;;                (cons (cons elt (first result)) (rest result)))
;;              (add-new-col (result)
;;                (cons '() result)))
;;       (if (zerop long-cols)
;;           (collect-long-cols l rows cols)
;;           (collect-long-cols l rows long-cols)))) )

;;;
;;;    Gather elements of L into COLS subsequences which will become the columns output by
;;;    MULTI-COLUMN-PRINT. The columns will each consist of ROWS elements. If the length of
;;;    L is not divisible by COLS then there will be 1+ incomplete columns of ROWS-1 elements.
;;;
;;;    There is no way to do this without finding (length l) up front?!
;;;    
(defun list-to-columns (l cols)
  (let* ((length (length l))
         (rows (ceiling length cols))
         (long-cols (if (zerop (rem length cols)) cols (rem length cols))))
;;         (long-cols (+ (mod (+ (mod length cols) (- cols 1)) cols) 1))) ; Map 0 to COLS, i.e., if divisible by COLS, then they're all long.
    (labels ((collect-long-cols (l n long-cols)
               (cond ((endp l) '(()))
                     ((zerop long-cols) (collect-short-cols l (1- rows)))
                     ((zerop n) (add-new-col (collect-long-cols l rows (1- long-cols))))
                     (t (add-elt-to-result (first l) (collect-long-cols (rest l) (1- n) long-cols)))) )
             (collect-short-cols (l n)
               (cond ((endp l) '(()))
                     ((zerop n) (add-new-col (collect-short-cols l (1- rows))))
                     (t (add-elt-to-result (first l) (collect-short-cols (rest l) (1- n)))) ))
             (add-elt-to-result (elt result)
               (cons (cons elt (first result)) (rest result)))
             (add-new-col (result)
               (cons '() result)))
      (collect-long-cols l rows long-cols))))

(defun list-to-columns (l cols)
  (do* ((length (length l))
        (rows (ceiling length cols))
        (long-cols (if (zerop (rem length cols)) cols (rem length cols)))
        (col 0 (1+ col))
        (start 0 end)
        (end (if (zerop long-cols) (1- rows) rows)
             (if (> long-cols 0) (+ end rows) (+ end (1- rows))))
        (result '()))
       ((= col cols) (nreverse result))
    (push (subseq l start end) result)
    (when (> long-cols 0)
      (decf long-cols))))

(defun list-to-columns (l cols)
  (loop with length = (length l)
        with rows = (ceiling length cols)
        with long-cols = (if (zerop (rem length cols)) cols (rem length cols))
        for col from 1 to cols
        for start = 0 then end
        for end = (if (zerop long-cols) (1- rows) rows) then (if (> long-cols 0) (+ end rows) (+ end (1- rows)))
        collect (subseq l start end)
        when (> long-cols 0)
        do (decf long-cols)))

;;;
;;;    STREAM = NIL?!
;;;
;;;(dotimes (n 15) (multi-column-print (loop for i from 1 to n collect i) :columns 3) (terpri))
;; (defun multi-column-print (l &key (columns 1) (width (truncate 80 columns)) (stream *standard-output*) (indent 0))
;;   (let ((table (list-to-columns l columns)))
;;     (labels ((multi-column-print-aux (table)
;;                (unless (null (first table))
;;                  (format stream "~v,0@T~@?" indent (format nil "~~{~~~DA~~}~~%" width) (get-firsts table))
;;                  (multi-column-print-aux (get-rests table)))) )
;;       (multi-column-print-aux table))))

(defun multi-column-print (l &key (columns 1) (width (truncate 80 columns)) (stream *standard-output*) (indent 0))
  (let ((table (list-to-columns l columns)))
    (labels ((multi-column-print-aux (table)
               (unless (null (first table))
                 (multiple-value-bind (column rests) (firsts-rests table)
                   (format stream "~v,0@T~@?" indent (format nil "~~{~~~DA~~}~~%" width) column)
                   (multi-column-print-aux rests)))) )
      (multi-column-print-aux table))))

;;;
;;;    These have been superseded by LANG:FIRSTS-RESTS!
;;;    
(defun get-firsts (tree)
  (if (or (endp tree)
          (endp (first tree)))
      '()
      (cons (first (first tree)) (get-firsts (rest tree)))) )

;; (defun get-firsts (tree)
;;   (mapcar #'first tree)) ; remove NIL!

(defun get-firsts (tree)
  (let ((result '()))
    (dolist (list tree (nreverse result))
      (unless (endp list)
        (push (first list) result)))) )

(defun get-firsts (tree)
  (loop for list in tree
        unless (endp list)
        collect (first list)))

(defun get-rests (tree)
  (if (endp tree)
      '()
      (cons (rest (first tree)) (get-rests (rest tree)))) )

(defun get-rests (tree)
  (mapcar #'rest tree))

(defun make-random-word ()
  (let ((s (make-string (+ (random 10) 3))))
    (dotimes (i (length s))
      (setf (char s i) (random-letter)))
    (if (zerop (random 2))
        s
        (string-capitalize s))))

(defun random-letter ()
  (code-char (+ (char-code #\a) (random 26))))

;(multi-column-print (sort (loop for i from 1 to 29 collect (make-random-word)) #'string<) :columns 8 :width 18)

;;;
;;;    7.9.5
;;;
(defun romans (limit digits)
  (loop for i from 1 to (min limit 3999)
        for roman = (format nil "~@R" i)
        when (= (length roman) digits)
        collect roman))

;;;
;;;    This does not adequately validate LIMIT. Error if > 3999.
;;;    Order of RESULT is reversed as well.
;;;    
(defun romans (limit digits)
  (if (zerop limit)
      '()
      (let ((roman (format nil "~@R" limit)))
        (if (= (length roman) digits)
            (cons roman (romans (1- limit) digits))
            (romans (1- limit) digits)))) )

(defun romans (limit digits)
  (labels ((romans-aux (i result)
             (if (zerop i)
                 result
                 (let ((roman (format nil "~@R" i)))
                   (if (= (length roman) digits)
                       (romans-aux (1- i) (cons roman result))
                       (romans-aux (1- i) result)))) ))
    (romans-aux (min limit 3999) '())))

;;;
;;;    7.9.7
;;;
(defun split-fill (left pad right &optional (stream *standard-output*))
  (format stream "~80,,,v<~A~;~A~>" pad left right))

;;;
;;;    7.9.8
;;;
;;;    See format.lisp
;;;    
