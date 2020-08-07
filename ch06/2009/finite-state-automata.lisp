;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               finite-state-automata.lisp
;;;;
;;;;   Started:            Sat Mar 14 19:54:26 2009
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
(load "/Users/dsletten/lisp/packages/test")

(defpackage finite-state-automata (:use common-lisp test))

(in-package finite-state-automata)

(defvar *iso-latin-1* (coerce (loop for i from 0 upto 255 collect (code-char i)) 'vector))

(defun string-matcher (text pattern &optional (alphabet *iso-latin-1*))
  (finite-automaton-matcher text
                            (transition-function-sparse (compute-transition-table-sparse pattern
                                                                                         alphabet))
                            (length pattern)))

(defun suffixp (p k q a)
;  (print (subseq p 0 k))
;  (print (subseq (concatenate 'string (subseq p 0 q) (string a)) (- (+ q 1) k)))
  (string= p (concatenate 'string (subseq p 0 q) (string a)) :start1 0 :end1 k :start2 (- (+ q 1) k)))

;;;
;;;    Is P[1..k] a suffix of P[1..q]a?
;;;    
;;;    Is P[0..(k-1)] a suffix of P[0..q]a?
(defun suffixp (p k q a)
  (loop for i from 0 below (1- k)
do (print (list (char p i) (char p (+ (- (+ q 1) k) i))))
        always (char= (char p i) (char p (+ (- (+ q 1) k) i)))
        finally (return (char= (char p (1- k)) a))))

(defun string-suffix-p (s1 s2)
;  (print (list s1 s2))
  (if (<= (length s1) (length s2))
      (loop for i from (1- (length s1)) downto 0
            for j from (1- (length s2)) downto 0
            always (char= (char s1 i) (char s2 j))
            finally (return t))
      nil))

;;;
;;;    This is a specialized test for string suffixes. Only works in context
;;;    of building finite state automaton based on P.
;;;    
;;;    Is the substring of P consisting of the first K chars a suffix of
;;;    the first Q chars of P concatenated with char A?
;;;
;;;    K  01234
;;;    P   pung
;;;    Q  01234 #\x
;;;
;;;    "pung" "pun" #\g
;;;      4      3       => T
;;;
;;;    "pu"   "pung" #\p => NIL
;;;     2       4
;;;
;;;    (zerop k) => ""
;;;
;;;    In other words:
;;;      (string-suffix-p (subseq p 0 k)
;;;                       (concatenate 'string (subseq p 0 q) (string a)))
;;;
;;;    However, this function avoids making 3 copies of the string P.
;;;                                     
(defun automaton-string-suffix-p (p k q a)
  (cond ((zerop k) t)
        ((char= (char p (1- k)) a)
         (loop for i from (- k 2) downto 0
               for j from (1- q) downto 0
               always (char= (char p i) (char p j))
               finally (return t) ))
        (t nil)))

(defun string-suffix-p-3 (s1 s2)
  (and (<= (length s1) (length s2))
       (every #'char= (reverse s1) (reverse s2))))

(defun finite-automaton-matcher (text delta m)
  (loop for i from 0 below (length text)
        for q = (funcall delta 0 (char text i)) then (funcall delta q (char text i))
;do (print (list i q))
        when (= q m)
        collect (1+ (- i m))))

(defun compute-transition-table (pattern alphabet)
  (let* ((m (length pattern))
         (delta (make-array (1+ m))))
    (loop for q from 0 upto m
          do (setf (aref delta q) (make-hash-table))
;          (print q)
             (loop for a across alphabet
                   do (loop for k from (min m (+ q 1)) downto 0
;                   do (loop for k from (min (+ m 1) (+ q 2)) downto 0
;                            do (print (list (subseq pattern 0 k) (concatenate 'string (subseq pattern 0 q) (string a))))
                            until (automaton-string-suffix-p pattern k q a)
;                            until (suffixp pattern k q a)
;                            until (search (subseq pattern 0 k) (concatenate 'string (subseq pattern 0 q) (string a)))
;                            until (string= pattern (concatenate 'string (subseq pattern 0 q) (string a)) :start1 0 :end1 k)
                            finally (setf (gethash a (aref delta q)) k))))
;                            finally (progn (format t "a ~C k ~D~%" a k) (setf (gethash a (aref delta q)) k)))))
    delta))

(defun transition-function (table)
  #'(lambda (q a)
      (gethash a (aref table q))))

;; (defun compute-transition-function (pattern alphabet)
;;   (let* ((m (length pattern))
;;          (delta (make-array (1+ m))))
;;     (loop for q from 0 upto m
;;           do (setf (aref delta q) (make-hash-table))
;; ;          (print q)
;;              (loop for a across alphabet
;;                    do (loop for k from (min m (+ q 1)) downto 0
;; ;                   do (loop for k from (min (+ m 1) (+ q 2)) downto 0
;; ;                            do (print (list (subseq pattern 0 k) (concatenate 'string (subseq pattern 0 q) (string a))))
;;                             until (suffixp pattern k q a)
;; ;                            until (search (subseq pattern 0 k) (concatenate 'string (subseq pattern 0 q) (string a)))
;; ;                            until (string= pattern (concatenate 'string (subseq pattern 0 q) (string a)) :start1 0 :end1 k)
;;                             finally (setf (gethash a (aref delta q)) k))))
;; ;                            finally (progn (format t "a ~C k ~D~%" a k) (setf (gethash a (aref delta q)) k)))))
;;     #'(lambda (q a)
;;         (gethash a (aref delta q)))) )

(defun compute-transition-table-subseq (pattern alphabet)
  (let* ((m (length pattern))
         (delta (make-array (1+ m))))
    (loop for q from 0 upto m
          do (setf (aref delta q) (make-hash-table))
             (loop for a across alphabet
                   do (loop for k from (min m (+ q 1)) downto 0
                            until (string-suffix-p (subseq pattern 0 k)
                                                   (concatenate 'string (subseq pattern 0 q) (string a)))
                            finally (setf (gethash a (aref delta q)) k))))
    delta))

;;;
;;;    Doesn't work with the sparse version. First hash-map may not contain all keys...
;;;    
(defun dump-transition-table (delta)
  (let ((keys (sort (loop for k being the hash-keys in (aref delta 0) collect k) #'char<)))
    (terpri)
    (format t "   ")
    (loop for k in keys
          do (format t " ~S " k))
    (format t "~%")
    (loop for q from 0 below (length delta)
          do (format t "~2,D " q)
             (loop for k in keys
                   do (format t "  ~D  " (gethash k (aref delta q))))
             (format t "~%"))))

;;;
;;;    Don't add hash entry for state 0. Just let NIL represent 0.
;;;    
(defun compute-transition-table-sparse (pattern alphabet)
  (let* ((m (length pattern))
         (delta (make-array (1+ m))))
    (loop for q from 0 upto m
          do (setf (aref delta q) (make-hash-table))
             (loop for a across alphabet
                   do (loop for k from (min m (+ q 1)) downto 0
                            until (automaton-string-suffix-p pattern k q a)
                            finally (unless (zerop k) (setf (gethash a (aref delta q)) k)))) )
    delta))

(defun transition-function-sparse (table)
  #'(lambda (q a)
      (or (gethash a (aref table q)) 0)))


(deftest test-string-matcher ()
  (check
;   (not (string-matcher 10 "asdf"))
;   (not (string-matcher "asdf" 10))
   (string-matcher "" "")
   (not (string-matcher "ffo" "foo"))
   (string-matcher "ffoo" "foo")
   (string-matcher "foo" "foo")
   (not (string-matcher  "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (string-matcher "peter piper picked a peck of pretty pickled peppers" "pickle")
   (string-matcher "a" "")
   (not (string-matcher "" "a"))))

#|
Example from the book
(dump-transition-table (compute-transition-table-subseq "ababaca" "abc"))

    #\a  #\b  #\c 
 0   1    0    0  
 1   1    2    0  
 2   3    0    0  
 3   1    4    0  
 4   5    0    0  
 5   1    4    6  
 6   7    0    0  
 7   1    2    0  
NIL
? (setf *delta* (transition-function (compute-transition-table "ababaca" "abc")))
#<CCL:COMPILED-LEXICAL-CLOSURE (:INTERNAL TRANSITION-FUNCTION) #x8F330F6>
? (finite-automaton-matcher "abababacaba" *delta* 7)
(2)
? 

(string-matcher "Is this not pung?" "pung")
(12)
? (string-matcher "Is this not ppung?" "pung")
(13)
? (string-matcher "Is this not pun?" "pung")
NIL
? (string-matcher "Is this not pun?" "")
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) ; Why not 0?

|#