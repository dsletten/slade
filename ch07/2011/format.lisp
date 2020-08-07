;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               format.lisp
;;;;
;;;;   Started:            Thu Sep 15 23:44:19 2011
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
;;;;   Notes: See Clozure 1.5 format.lisp ~/lisp/clozure/ccl_1.5/lib/format.lisp
;;;;   SBCL ~/lisp/sbcl/sbcl-1.0.43/src/code/target-format.lisp
;;;;   CLISP ~/lisp/clisp-2.38/clisp-2.38/src/format.lisp
;;;;
;;;;   From Tom Faulhaber's Clojure implementation:
;;;;   http://www.jimloy.com/math/billion.htm
;;;;   http://www.grammarbook.com/numbers/numbers.asp
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :format (:use :common-lisp :test) (:shadow :format))

(in-package :format)


;;;
;;;    7.9.8
;;;
;;;    What about ~2A, ~2,'0D???
;;;    
(defun format (stream control &rest args)
  (do ((directive-position (position #\~ control) (position #\~ control :start directive-position))
       (arg-index 0)
       (out (or stream (make-string-output-stream)))
       (literal-index 0 directive-position))
      ((null directive-position) (write-string (subseq control literal-index) out) (if stream nil (get-output-stream-string out)))
    (multiple-value-bind (replacement advance consume) (process-directive control directive-position arg-index args)
      (output-with-replacement out control literal-index directive-position replacement)
      (incf directive-position (+ 2 advance))
      (when consume (incf arg-index)))) )

;;;
;;;    Partially handles recursive calls, e.g., @R, :R, but not both @:R
;;;
;;;    Recursive call (i.e., with :/@ modifier assigns new value to CONSUME in the process of assigning REPLACEMENT. Is that
;;;    kosher? (It's legal, but is it reasonable?):
;;;    (let* ((x 1) (y (setf x 2))) (list x y))
;;;    
(defun process-directive (control directive-position arg-index args &optional modifier)
;  (print (list directive-position arg))
  (let* ((default-arg (nth arg-index args))
         (consume t)
         (replacement (case (char-upcase (char control (1+ directive-position)))
                        (#\A (princ-to-string default-arg))
                        (#\B (binary default-arg))
                        (#\C (string default-arg))
                        (#\D (decimal default-arg))
                        (#\E (exponential-float default-arg))
                        (#\F (fixed-float default-arg))
                        (#\G (if (or (> (abs default-arg) (expt 10 8))
                                     (< (abs default-arg) (expt 10 -8)))
                                 (exponential-float default-arg)
                                 (fixed-float default-arg)))
                                        ;      (#\I)
                        (#\O (octal default-arg))
                        (#\P (case modifier
                               (#\: (setf consume nil)
                                    (if (eql (nth (1- arg-index) args) 1) "" "s"))
                               (#\@ (if (eql default-arg 1) "y" "ies"))
                               (otherwise (if (eql default-arg 1) "" "s"))))
                        (#\R (case modifier
                               (#\: (ordinal default-arg))
                               (#\@ (concatenate 'string "(roman " (decimal default-arg) ")"))
                               (otherwise (cardinal default-arg))))
                        (#\S (prin1-to-string default-arg))
;                        (#\T)
                        (#\W (write-to-string default-arg))
                        (#\X (hex default-arg))
                        (#\: (setf modifier t)
                             (multiple-value-bind (replacement advance consume1) (process-directive control (1+ directive-position) arg-index args #\:)
                               (declare (ignore advance))
                               (setf consume consume1)
                               replacement))
                        (#\@ (setf modifier t)
                             (process-directive control (1+ directive-position) arg-index args #\@))
                        (#\% (setf consume nil)
                             (string #\newline))
                        (#\$ (fixed-float default-arg 2))
                        (#\~ (setf consume nil)
                             "~")
                        (otherwise (setf consume nil) ""))))
    (values replacement (if modifier 1 0) consume)))

(defun output-with-replacement (stream control before directive-position replacement)
  (write-string (subseq control before directive-position) stream)
  (write-string replacement stream))

;; (defun binary (n)
;;   (assert (and (integerp n) (not (minusp n))) (n))
;;   (coerce (reverse (loop for m = n then (truncate m 2)
;;                          collect (digit-char (mod m 2))
;;                          until (zerop m)))
;;           'string))

(defun radix-string (n radix)
  (assert (integerp n) (n))
  (cond ((minusp n) (concatenate 'string "-" (radix-string (- n) radix)))
        ((zerop n) "0")
        (t (do ((m n (truncate m radix))
                (result '() (cons (digit-char (mod m radix) radix) result)))
               ((zerop m) (coerce result 'string)))) ))

(defun decimal (n)
  (radix-string n 10))

(defun binary (n)
  (radix-string n 2))

(defun hex (n)
  (radix-string n 16))

(defun octal (n)
  (radix-string n 8))

(defun exponential-float (x &optional (precision 5))
  (let ((magnitude (floor (log x 10))))
    (with-output-to-string (s)
      (write-string (fixed-float (coerce (/ x (expt 10 magnitude)) 'double-float) precision) s)
      (write-string "e" s)
      (if (minusp magnitude)
          (write-char #\- s)
          (write-char #\+ s))
      (write-string (decimal (abs magnitude)) s))))

(defun fixed-float (x &optional (precision 5))
  (multiple-value-bind (whole frac) (truncate x)
;;  (multiple-value-bind (whole frac) (truncate (round x precision)) Have to round after to avoid losing precision
    (with-output-to-string (s)
      (if (minusp whole)
          (progn (write-string "-" s)
                 (write-string (decimal (- whole)) s))
          (write-string (decimal whole) s))
      (write-string "." s)
      (write-string (round-fraction-string (abs frac) precision) s))))
;;       (do ((frac (* (round (abs frac) precision) 10) (* frac 10))
;;            (i 0 (1+ i)))
;;           ((= i precision))
;;         (write-char (digit-char (mod (truncate frac) 10)) s)))) )

;; (defun round (x &optional (precision 2))
;;   (if (minusp x)
;;       (- (round (- x) precision))
;;       (let ((magnitude (expt 10 precision)))
;;         (coerce (/ (truncate (+ (* x magnitude) 0.5d0)) magnitude) 'double-float))))

;;;
;;;    Round 0.3456 to 3 decimal places:
;;;    0.3456 => 345.6 + 0.5 => 346.1 => 346 => "346"
;;;
;;;    Effectively, round first (converting part of fraction to an integer), then use integer as source for digits rather than
;;;    converting back to a float.
;;;    
(defun round-fraction-string (frac &optional (precision 2))
  (do ((i 0 (1+ i))
       (result (make-string precision :initial-element #\0))
       (n (truncate (+ (* frac (expt 10 precision)) 0.5)) (truncate n 10)))
      ((= i precision) (reverse result))
    (setf (char result i) (digit-char (mod n 10)))))

;(loop for i from 0 to 99 unless (string= (cardinal i) (cl:format nil "~R" i)) collect i)

(defun cardinal (n)
  (assert (integerp n) (n))
  (if (minusp n)
      (with-output-to-string (result)
        (write-string "negative " result) ; CLISP says "minus"
        (write-string (cardinal (- n)) result))
      (cond ((<= 0 n 19) (ecase n
                           (0 "zero")
                           (1 "one")
                           (2 "two")
                           (3 "three")
                           (4 "four")
                           (5 "five")
                           (6 "six")
                           (7 "seven")
                           (8 "eight")
                           (9 "nine")
                           (10 "ten")
                           (11 "eleven")
                           (12 "twelve")
                           (13 "thirteen")
                           (14 "fourteen")
                           (15 "fifteen")
                           (16 "sixteen")
                           (17 "seventeen")
                           (18 "eighteen")
                           (19 "nineteen")))
            ((<= n 99) (with-output-to-string (result)
                         (write-string (ecase (truncate n 10)
                                         (2 "twenty")
                                         (3 "thirty")
                                         (4 "forty")
                                         (5 "fifty")
                                         (6 "sixty")
                                         (7 "seventy")
                                         (8 "eighty")
                                         (9 "ninety"))
                                       result)
                         (unless (zerop (mod n 10))
                           (write-string "-" result)
                           (write-string (cardinal (mod n 10)) result))))
            (t (really-big-cardinal n)))) )

;; (defun really-big-cardinal (n)
;;   (do ((step 1 3)
;;        (power 2 (+ power step))
;;        (period (* 10 10) (* period (expt 10 step))))
;;       ((<= n (1- (* period (expt 10 step)))) (really-big-cardinal-to-string n period power))))

;; (defun really-big-cardinal-to-string (n period power)
;;   (with-output-to-string (result)
;;     (write-string (cardinal (truncate n period)) result)
;;     (write-string " " result)
;;     (write-string (period-name power) result)
;;     (unless (zerop (mod n period))
;;       (write-string " " result)
;;       (write-string (cardinal (mod n period)) result))))

(defun period-name (power)
  (ecase power
    (2 "hundred")
    (3 "thousand")
    (6 "million")
    (9 "billion")
    (12 "trillion")
    (15 "quadrillion")
    (18 "quintillion")
    (21 "sextillion")
    (24 "septillion")
    (27 "octillion")
    (30 "nonillion")
    (33 "decillion")
    (36 "undecillion")
    (39 "duodecillion")
    (42 "tredecillion")
    (45 "quattuordecillion")
    (48 "quindecillion")
    (51 "sexdecillion")
    (54 "septendecillion")
    (57 "octodecillion")
    (60 "novemdecillion")
    (63 "vigintillion")))

;;;
;;;    The logic of these COND tests is kind of weird...
;;;    
(defun ordinal (n)
  (assert (integerp n) (n))
  (cond ((minusp n) (with-output-to-string (result)
                      (write-string "negative " result)
                      (write-string (ordinal (- n)) result)))
        ((> n 100) (really-big-ordinal n))
        ((<= 0 n 9) (ecase n
                      (0 "zeroth")
                      (1 "first")
                      (2 "second")
                      (3 "third")
                      (4 "fourth")
                      (5 "fifth")
                      (6 "sixth")
                      (7 "seventh")
                      (8 "eighth")
                      (9 "ninth")))
        ((= n 12) "twelfth") ; Weird special case!
        ((<= n 19) (concatenate 'string (cardinal n) "th"))
        ((zerop (mod n 10)) (case n
                              (20 "twentieth")
                              (30 "thirtieth")
                              (40 "fortieth")
                              (50 "fiftieth")
                              (60 "sixtieth")
                              (70 "seventieth")
                              (80 "eightieth")
                              (90 "ninetieth")))
        (t (with-output-to-string (result)
             (write-string (cardinal (- n (mod n 10))) result)
             (write-string "-" result)
             (write-string (ordinal (mod n 10)) result)))) )

;; (defun really-big-ordinal (n)
;;   (do ((step 1 3)
;;        (power 2 (+ power step))
;;        (period (* 10 10) (* period (expt 10 step))))
;;       ((<= n (1- (* period (expt 10 step)))) (really-big-ordinal-to-string n period power))))

;; (defun really-big-ordinal-to-string (n period power)
;;   (with-output-to-string (result)
;;     (write-string (cardinal (truncate n period)) result)
;;     (write-string " " result)
;;     (write-string (period-name power) result)
;;     (cond ((zerop (mod n period)) (write-string "th" result))
;;           (t (write-string " " result)
;;              (write-string (ordinal (mod n period)) result)))) )

(defun handle-big-number (n recur terminal)
  (do ((step 1 3)
       (power 2 (+ power step))
       (period (* 10 10) (* period (expt 10 step))))
      ((<= n (1- (* period (expt 10 step)))) (handle-big-number-to-string n period power recur terminal))))

(defun handle-big-number-to-string (n period power recur terminal)
  (with-output-to-string (result)
    (write-string (cardinal (truncate n period)) result)
    (write-string " " result)
    (write-string (period-name power) result)
    (cond ((zerop (mod n period)) (write-string terminal result))
          (t (write-string " " result)
             (write-string (funcall recur (mod n period)) result)))) )

(defun really-big-cardinal (n)
  (handle-big-number n #'cardinal ""))

(defun really-big-ordinal (n)
  (handle-big-number n #'ordinal "th"))


  
;(dotimes (i 100) (when (string/= (format nil "~R" i) (cl:format nil "~R" i)) (print i)))
;(dotimes (i 100) (when (string/= (format nil "~:R" i) (cl:format nil "~:R" i)) (print i)))
;(format nil "~:R" (truncate (* pi (expt 10 40))))

;;;
;;;    Test ~R/~:R
;;;    
(defun test-random (directive)
  (dotimes (i 100)
    (let ((n (truncate (* (random 1d0) (expt 10 (random 67)))) ))
      (when (string/= (format nil directive n) (cl:format nil directive n))
        (print n)))) )
