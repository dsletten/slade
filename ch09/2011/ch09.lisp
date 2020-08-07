;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Thu Nov 17 23:51:46 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch09 (:use :common-lisp :test))

(in-package :ch09)

;; (do ((i 0 (1+ i)))
;;     ((= i 10))
;;   (go b)
;;   a
;;   (princ i)
;;   (go c)
;;   b
;;   (princ "i: ")
;;   (go a)
;;   c
;;   (terpri))

;;;
;;;    9.12.3
;;;
(defun compress-adjacent (seq)
  (labels ((compress-list (l)
             (cond ((endp l) '())
                   ((endp (rest l)) l)
                   ((eql (first l) (second l)) (compress-list (rest l)))
                   (t (cons (first l) (compress-list (rest l)))) ))
;;            (compress-string (s i stream)
;;              (cond ((>= i (1- (length s))) (get-output-stream-string stream))
;;                    ((char= (char s i) (char s (1+ i))) (compress-string s (1+ i) stream))
;;                    (t (write-char (char s i) stream) (compress-string s (1+ i) stream))))
           (compress-string (i stream)
             (cond ((= i (length seq)) (get-output-stream-string stream))
                   (t (write-char (char seq i) stream)
                      (compress-string (find-next-char-pos (1+ i) (char seq i)) stream))))
           (find-next-char-pos (i ch)
             (cond ((= i (length seq)) i)
                   ((char= ch (char seq i)) (find-next-char-pos (1+ i) ch))
                   (t i)))
;;            (compress-vector (v)
;;              (coerce (compress-list (coerce v 'list)) 'vector)))
           (compress-vector (i result)
             (cond ((= i (length seq)) result)
                   (t (vector-push (aref seq i) result)
                      (compress-vector (find-next-elt-pos (1+ i) (aref seq i)) result))))
           (find-next-elt-pos (i elt)
             (cond ((= i (length seq)) i)
                   ((eql elt (aref seq i)) (find-next-elt-pos (1+ i) elt))
                   (t i))))
  (typecase seq
    (list (compress-list seq))
    (string (compress-string 0 (make-string-output-stream)))
    (vector (compress-vector 0 (make-array (length seq) :adjustable t :fill-pointer 0)))) ))

;;;
;;;    Lists
;;;    
(defun remove-pairs (l)
  (labels ((remove-pairs-aux (elt l result)
             (cond ((endp l) (cons elt result))
                   ((eql elt (first l)) (remove-pairs-aux elt (rest l) result))
                   (t (remove-pairs-aux (first l) (rest l) (cons elt result)))) ))
    (if (endp l)
        '()
        (nreverse (remove-pairs-aux (first l) (rest l) '()))) ))

(defun remove-pairs-iter (l)
  (if (endp l)
      '()
      (do ((elt (first l) (if (eql elt (first l)) elt (first l)))
           (l (rest l) (rest l))
           (result '() (if (eql elt (first l)) result (cons elt result))))
          ((endp l) (nreverse (cons elt result)))) ))

(defun compress-list (l)
  (if (endp l)
      '()
      (loop for cons1 on l
            for cons2 on (rest l)
            for result = (if (eql (first cons1) (first cons2)) result (cons (first cons1) '())) then (if (eql (first cons1) (first cons2)) result (cons (first cons1) result))
            finally (return (nreverse (cons (first cons1) result)))) ))

(defun compress-list (l)
  (flet ((keep-if-different (l1 l2 result)
           (if (eql (first l1) (first l2))
               result
               (cons (first l1) result))))
    (if (endp l)
        '()
        (loop for cons1 on l
              for cons2 on (rest l)
              for result = (keep-if-different cons1 cons2 '()) then (keep-if-different cons1 cons2 result)
              finally (return (nreverse (cons (first cons1) result)))) )))

(defun compress-list (l)
  (if (endp l)
      '()
      (do ((l1 l (rest l1))
           (l2 (rest l) (rest l2))
           (result '() (if (eql (first l1) (first l2)) result (cons (first l1) result))))
          ((endp l2) (nreverse (cons (first l1) result)))) ))

;;;
;;;    Strings
;;;    
(defun compress-string (s)
  (if (zerop (length s))
      s
      (with-output-to-string (result)
        (do ((i 0 (1+ i))
             (j 1 (1+ j)))
            ((= j (length s)) (write-char (char s i) result))
          (unless (char= (char s i) (char s j))
            (write-char (char s i) result)))) ))

(defun compress-string (s)
  (do ((i 0 (find-next-char-pos s (1+ i) (char s i)))
       (output (make-string-output-stream)))
      ((= i (length s)) (get-output-stream-string output))
    (write-char (char s i) output)))

(defun find-next-char-pos (s i ch)
  (do ((i i (1+ i)))
      ((= i (length s)) i)
    (unless (char= (char s i) ch)
      (return i))))

;;;
;;;    Vectors
;;;    
(defun compress-vector (v)
  (if (zerop (length v))
      v
      (do ((i 0 (1+ i))
           (j 1 (1+ j))
           (result (make-array (length v) :adjustable t :fill-pointer 0)))
          ((= j (length v)) (vector-push (aref v i) result) result)
        (unless (eql (aref v i) (aref v j))
          (vector-push (aref v i) result)))) )

(defun compress-vector (v)
  (do ((i 0 (find-next-elt-pos v (1+ i) (aref v i)))
       (output (make-array (length v) :adjustable t :fill-pointer 0)))
      ((= i (length v)) output)
    (vector-push (aref v i) output)))

(defun find-next-elt-pos (v i elt)
  (do ((i i (1+ i)))
      ((= i (length v)) i)
    (unless (eql (aref v i) elt)
      (return i))))

(deftest test-compress-adjacent ()
  (check
   (equal (compress-adjacent '()) NIL)
   (equal (compress-adjacent '(a a a b b b)) '(A B))
   (equal (compress-adjacent '(a)) '(A))
   (equal (compress-adjacent '(a b b a a b a c)) '(A B A B A C))
   (equal (compress-adjacent '(a b b a a b a c c)) '(A B A B A C))
   (string= (compress-adjacent "bookkeeper") "bokeper")
   (string= (compress-adjacent "aaabbb") "ab")
   (string= (compress-adjacent "abbaabac") "ababac")
   (string= (compress-adjacent "abbaabacc") "ababac")
   (string= (compress-adjacent "aAabBB") "aAabB")
   (string= (compress-adjacent "a") "a")
   (string= (compress-adjacent "") "")
   (equalp (compress-adjacent (vector 1 2 3 1 1 2)) #(1 2 3 1 2))
   (equalp (compress-adjacent (vector 1 2 3 1 1 2 2)) #(1 2 3 1 2))
   (equalp (compress-adjacent (vector 1 2 3)) #(1 2 3))
   (equalp (compress-adjacent (vector)) #())
   (equalp (compress-adjacent (vector 1)) #(1))))
