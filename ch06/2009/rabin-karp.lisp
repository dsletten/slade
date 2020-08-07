;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               rabin-karp.lisp
;;;;
;;;;   Started:            Fri Mar 13 17:51:44 2009
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
;;;;   Notes: See ~/lisp/programs/horners.lisp
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage rabin-karp (:use common-lisp test))

(in-package rabin-karp)

;;;
;;;    Mastering Algorithms with Perl (ch. 9 pg. 364) suggests these values for d, q
;;;
;;;    Empty patern should result in list (0..n)!
;;;    
(defun string-matcher (text pattern)
  (if (string= text "")
      (and (string= pattern "") (list 0)) ; Kludge for empty TEXT
      (rabin-karp-matcher text pattern 256)))

(defun string-matcher-2 (text pattern)
  (if (string= text "")
      (and (string= pattern "") (list 0)) ; Kludge for empty TEXT
      (rabin-karp-mod-matcher text pattern 256 8355961)))

(defun string-matcher-3 (text pattern)
  (if (string= text "")
      (and (string= pattern "") (list 0)) ; Kludge for empty TEXT
      (rabin-karp-reduce-matcher text pattern 256)))
;;;
;;;    The encoded PATTERN (P-ENC) and TEXT fragments (T-ENC) can
;;;    be very large numbers...definitely bignums.
;;;    
(defun rabin-karp-matcher (text pattern d)
  (let* ((n (length text))
         (m (length pattern))
         (h (expt d (1- m)))
         (p-enc (horners d (map 'list #'char-code pattern))))
    (flet ((shift (t-enc s)
             (if (= (+ s m) n)
                 t-enc ; Throwaway value...
                 (+ (* d (- t-enc (* (char-value text s) h)))
                    (char-value text (+ s m)))) ))
      (loop for s from 0 upto (- n m)
            and t-enc = (horners d (map 'list #'char-code (subseq text 0 m))) then (shift t-enc s)
            when (= p-enc t-enc)
            collect s))))

;;;@ weird-reduce
(defun rabin-karp-reduce-matcher (text pattern d)
  (let* ((n (length text))
         (m (length pattern))
         (h (expt d (1- m)))
         (p-enc (horners d (map 'list #'char-code pattern))))
    (flet ((shift (t-enc s)
             (if (= (+ s m) n)
                 t-enc ; Throwaway value...
                 (+ (* d (- t-enc (* (char-value text s) h)))
                    (char-value text (+ s m)))) ))
      (nreverse (second (reduce #'(lambda (a s)
                                    (destructuring-bind (t-enc matches) a
                                      (if (= p-enc t-enc)
                                          (list (shift t-enc s) (cons s matches))
                                          (list (shift t-enc s) matches))))
                                (loop for i from 0 upto (- n m) collect i)
                                :initial-value (list (horners d (map 'list #'char-code (subseq text 0 m))) '()))) ))))

(defun horners (x coefficients)
  (reduce #'(lambda (a b)
              (+ (* a x) b))
          coefficients
          :initial-value 0))

(defun char-value (s i)
  (char-code (char s i)))

;;;
;;;    The modulus keeps P-ENC and T-ENC smaller but introduces possible
;;;    spurious hits. Have to confirm via STRING=.
;;;    
(defun rabin-karp-mod-matcher (text pattern d q)
  (let* ((n (length text))
         (m (length pattern))
         (h (mod (expt d (1- m)) q))
         (pmod (horners-mod d (map 'list #'char-code pattern) q)))
    (flet ((shift (tmod s)
             (if (= (+ s m) n)
                 tmod ; Throwaway value...
                 (mod (+ (* d (- tmod (* (char-value text s) h)))
                         (char-value text (+ s m)))
                      q))))
      (loop for s from 0 upto (- n m)
            and tmod = (horners-mod d (map 'list #'char-code (subseq text 0 m)) q) then (shift tmod s)
            when (and (= pmod tmod) (string= pattern text :start2 s :end2 (+ s m)))
            collect s))))

(defun horners-mod (x coefficients q)
  (reduce #'(lambda (a b)
              (mod (+ (* a x) b) q))
          coefficients
          :initial-value 0))

;; (defun rabin-karp-mod-matcher (text pattern d q)
;;   (let* ((n (length text))
;;          (m (length pattern))
;;          (h (mod (expt d (1- m)) q))
;;          (pmod (preprocess pattern m d q)))
;;     (flet ((shift (tmod s)
;;              (if (= (+ s m) n)
;;                  tmod ; Throwaway value...
;;                  (mod (+ (* d (- tmod (* (char-value text s) h)))
;;                          (char-value text (+ s m)))
;;                       q))))
;;       (loop for s from 0 upto (- n m)
;;             and tmod = (preprocess text m d q) then (shift tmod s)
;;             when (and (= pmod tmod) (string= pattern text :start2 s :end2 (+ s m)))
;;             collect s))))

;; (defun rabin-karp-matcher (text pattern d q)
;;   (let* ((n (length text))
;;          (m (length pattern))
;;          (h (mod (expt d (1- m)) q))
;;          (pmod (preprocess pattern m d q))
;;          (tmod (preprocess text m d q)))
;;     (loop for s from 0 upto (- n m)
;;           when (and (= pmod tmod) (string= pattern text :start2 s :end2 (+ s m)))
;;             collect s
;;           end
;;           when (< s (- n m))
;;           do (setf tmod (shift tmod text m s d q h)))) )

;; (defun preprocess (s length d q)
;;   (labels ((preprocess-aux (result i)
;;              (if (= i length)
;;                  result
;;                  (preprocess-aux (mod (+ (* d result)
;;                                          (char-value s i))
;;                                       q)
;;                                  (1+ i)))) )
;;     (preprocess-aux 0 0)))

;; (defun shift (old s length i d q h)
;;   (mod (+ (* d (- old (* (char-value s i) h)))
;;           (char-value s (+ i length)))
;;        q))

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

(deftest test-string-matcher-2 ()
  (check
;   (not (string-matcher 10 "asdf"))
;   (not (string-matcher "asdf" 10))
   (string-matcher-2 "" "")
   (not (string-matcher-2 "ffo" "foo"))
   (string-matcher-2 "ffoo" "foo")
   (string-matcher-2 "foo" "foo")
   (not (string-matcher-2  "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (string-matcher-2 "peter piper picked a peck of pretty pickled peppers" "pickle")
   (string-matcher-2 "a" "")
   (not (string-matcher-2 "" "a"))))

(deftest test-string-matcher-3 ()
  (check
;   (not (string-matcher-3 10 "asdf"))
;   (not (string-matcher-3 "asdf" 10))
   (string-matcher-3 "" "")
   (not (string-matcher-3 "ffo" "foo"))
   (string-matcher-3 "ffoo" "foo")
   (string-matcher-3 "foo" "foo")
   (not (string-matcher-3  "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (string-matcher-3 "peter piper picked a peck of pretty pickled peppers" "pickle")
   (string-matcher-3 "a" "")
   (not (string-matcher-3 "" "a"))))
