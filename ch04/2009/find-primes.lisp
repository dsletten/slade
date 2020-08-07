;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               find-primes.lisp
;;;;
;;;;   Started:            Sun Mar  1 01:25:35 2009
;;;;   Modifications:
;;;;
;;;;   Purpose: Find the first COUNT primes. Accumulate in a vector.
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

(defpackage find-primes (:use common-lisp))

(in-package find-primes)

(defun dividesp (d n)
  (zerop (rem n d)))

(defun find-primes (count)
  (labels ((primep (n index primes)
             (cond ((= index (length primes)) t)
                   ((> (expt (aref primes index) 2) n) t)
                   ((dividesp (aref primes index) n) nil)
                   (t (primep n (1+ index) primes)))) )
    (let ((primes (make-array count :fill-pointer 0)))
      (do ((i 2 (+ i step))
           (step 1 2))
          ((= (length primes) count) primes)
        (when (primep i 0 primes)
          (vector-push i primes)))) ))

(defun find-primes (count)
  (let ((primes (make-array count :fill-pointer 0)))
    (labels ((primep (n)
               (loop for prime across primes
                     until (> (* prime prime) n)
                     when (dividesp prime n) do (return nil)
                     finally (return t))))
      (do ((i 2 (+ i step))
           (step 1 2))
          ((= (length primes) count) primes)
        (when (primep i)
          (vector-push i primes)))) ))
