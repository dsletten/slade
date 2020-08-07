;#!/usr/local/bin/clisp

;;
;   NAME:               ch08.lisp
;
;   STARTED:            011211
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

(defun repeat-trio (stream ch1 ch2 ch3 count)
  (labels ((pr-ch1 (count)
             (when (plusp count)
               (write-char ch1 stream)
               (pr-ch2 (1- count))) )
           (pr-ch2 (count)
             (when (plusp count)
               (write-char ch2 stream)
               (pr-ch3 (1- count))) )
           (pr-ch3 (count)
             (when (plusp count)
               (write-char ch3 stream)
               (pr-ch1 (1- count))) ))
    (pr-ch1 count)) )

(defun repeat (stream char-string count)
  (labels ((print-char (count)
             (when (plusp count)
               (write-char (char char-string
                                 (mod count
                                      (length char-string))))
               (print-char (1- count)))) )
    (print-char count)) )

	   
;;;
;;;    8.7.3
;;;    (These functions are a generalization of the ROT-13 technique)
;;;    

;;;
;;;    Bind both ENCODE and DECODE to the same key.
;;;    
(let ((n 0))
  (defun encode (ch)
    (code-char (mod (+ (char-code ch) n) 256)) )

  (defun decode (ch)
    (code-char (mod (- (+ 256 (char-code ch)) n) 256)) )

  (defun set-salt (x)
    (setf n x) ))

(defun string-encode (s n)
  (set-salt n)
  (map 'string #'encode s) )
(defun string-decode (s n)
  (set-salt n)
  (map 'string #'decode s) )

(defparameter *key-text* "Is this not pung?")
;(defparameter *key-text* "He who hesitates is last.")

;;;
;;;    If i is large enough, the code of each letter of the key text itself 
;;;    will be added to the string. E.g., (mod (char-code #\w) 120) => 
;;;    (char-code #\w). This still guarantees a changing offset, however, the 
;;;    offset is likely to be large compared to when i is small.
;;;
;;;    Nonetheless, as long as the key text and string are composed of ASCII
;;;    chars, there is no danger of the encoded char's code being greater than
;;;    255 (i.e., non-ISO Latin 1).
;;;
;;;    In general, the larger i is, the greater the variability of the offset.
;;;    For instance, if i = 2, then (mod n i) is only 0 or 1. But if i = 7,
;;;    then (mod n i) could be 0, . . ., 6. Likewise, the offset will have
;;;    greater variability if i is prime. By contrast, if i = 6, then
;;;    potentially every 3rd char is effectively (n mod 2) and potentially
;;;    every 2nd char is (n mod 3)--both small values of the divisor. Whereas
;;;    if i = 5, the divisor is always 5.
;;;    
(defun string-encode-1 (s key i)
  (labels ((string-encode (s k)
             (cond ((string= k "") (string-encode s key))
                   ((string= s "") "")
                   (t (concatenate
                       'string
                       (string (code-char (mod (+ (char-code (char s 0))
                                                  (mod (char-code (char k 0))
                                                       i))
                                               256)))
                       (string-encode (subseq s 1)
                                      (subseq k 1)))) ) ))
    (string-encode s key)) )

(defun string-decode-1 (s key i)
  (labels ((string-decode (s k)
             (cond ((string= k "") (string-decode s key))
                   ((string= s "") "")
                   (t (concatenate
                       'string
                       (string (code-char (mod (- (+ (char-code (char s 0))
                                                     256)
                                                  (mod (char-code (char k 0))
                                                       i))
                                               256)))
                       (string-decode (subseq s 1)
                                      (subseq k 1)))) ) ))
    (string-decode s key)) )

;;;
;;;    Slade's version doesn't work if (> (length string) (length key)) !!!!
;;;    
(defun slade-encode (char n)
  (code-char (+ (char-code char) n)) )
(defun slade-decode (char n)
  (code-char (- (char-code char) n)) )
(defun new-encode (char1 char2 m)
  (slade-encode char1 (mod (char-code char2) m)) )
(defun new-string-encode (string key m)
  (map 'string #'(lambda (ch1 ch2) (new-encode ch1 ch2 m))
       string key) )
(defun new-decode (char1 char2 m)
  (slade-decode char1 (mod (char-code char2) m)) )
(defun new-string-decode (string key m)
  (map 'string #'(lambda (ch1 ch2) (new-decode ch1 ch2 m))
       string key) )

;;;
;;;    8.7.4
;;;
(defun make-deck (ranks suits)
  (mapcan #'(lambda (r)
              (pairlis (make-list (length suits) :initial-element r)
                       suits))
          ranks) )

(defun make-deck-1 (ranks suits)
  (mapcan #'(lambda (r)
              (mapcar #'(lambda (s)
                          (cons r s))
                      suits))
          ranks) )

(defun make-deck-2 (ranks suits)
  (apply #'append (map 'list #'(lambda (suit)
                                 (map 'list #'(lambda (rank)
                                                (cons rank suit))
                                      ranks))
                       suits)) )

(defun make-deck-3 (ranks suits)
  (labels ((make-ranks (ranks suit deck)
             (cond ((null ranks) deck)
                   (t (make-ranks (cdr ranks)
                                  suit
                                  (cons (cons (car ranks) suit) deck)))) )
           (make-suits (suits deck)
             (cond ((null suits) deck)
                   (t (make-suits (cdr suits)
                                  (make-ranks (reverse ranks)
                                              (car suits)
                                              deck)))) ))
    (make-suits (reverse suits) ())) )
;;;
;;;    8.7.5
;;;
(defun my-reverse (l)
  (labels ((reverse-aux (l result)
             (cond ((null l) result)
                   (t (reverse-aux (cdr l) (cons (car l) result)))) ))
    (reverse-aux l ())) )

;;;
;;;    8.7.6
;;;
;;;    (= (length power-set) (expt 2 (length l))) should be true.
;;;
;;;    Performance comparison of the 3 versions below:
; (time (power-set '(a b c d e f g h i j k l)))
;     This test causes a segmentation fault on Titanium!! 020208
;     See the tail-recursive version in
;     ~/lisp/programs/combinatorics/power-set.lisp
;     

; Real time: 0.14032 sec.
; Run time: 0.13 sec.
; Space: 1261412 Bytes
; GC: 2, GC time: 0.03 sec.

; Real time: 0.008651 sec.
; Run time: 0.01 sec.
; Space: 100400 Bytes

; Real time: 0.057875 sec.
; Run time: 0.05 sec.
; Space: 65528 Bytes

; (defun power-set (l)
;   (cond ((null l) (list ()))
; 	(t (append (mapcar #'(lambda (set)
; 			       (cons (car l) set))
; 			   (power-set (cdr l)))
; 		     (power-set (cdr l)))) ) )

; (defun power-set (l)
;   (cond ((null l) (list ()))
; 	(t (let ((p (power-set (cdr l))))
; 	     (append (mapcar #'(lambda (set)
; 				 (cons (car l) set))
; 			     p)
; 		     p)))) )

(defun power-set (l)
  (cond ((null l) (list ()))
        (t (power-set-2 (car l) (power-set (cdr l)))) ) )

(defun power-set-2 (elt p)
  (labels ((power-set-aux (elt p p1)
             (cond ((null p1) p)
                   (t (cons (cons elt (car p1))
                            (power-set-aux elt p (cdr p1)))) ) ))
    (power-set-aux elt p p)) )

; (defun power-set-2 (elt p &optional (p1 p))
;   (cond ((null p1) p)
; 	(t (cons (cons elt (car p1))
; 		 (power-set-2 elt p (cdr p1)))) ) )

;;;
;;;    8.7.7
;;;    
(defun bad-length (l)
  (flet ((count-length (l)
           (cond ((null l) 0)
                 (t (+ 1 (bad-length (cdr l)))) )))
    (count-length l)) )
