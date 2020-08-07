;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch16.lisp
;;;;
;;;;   Started:            Fri Aug 31 19:37:43 2012
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

(defpackage :ch16 (:use :common-lisp :test))

(in-package :ch16)

(defun rmapcar (f list)
  (if (endp list)
      '()
      (cons (funcall f (first list))
            (rmapcar f (rest list)))) )

(defun trmapcar (f list)
  (labels ((trmapcar- (list result)
             (if (endp list)
                 (reverse result)
                 (trmapcar- (rest list) (cons (funcall f (first list)) result)))) )
    (trmapcar- '() list)))

(defun trmapcarn (f list)
  (labels ((trmapcarn- (list result)
             (if (endp list)
                 (nreverse result)
                 (trmapcarn- (rest list) (cons (funcall f (first list)) result)))) )
    (trmapcarn- '() list)))

(defun loopcar (f list)
  (loop for elt in list
        collect (funcall f elt)))

;;;
;;;    Allegro CL 8.2 Express user
;;;    Tail recursive versions are faster than built-in MAPCAR?
;;;    
;; CH16(80): (compile 'rmapcar)
;; RMAPCAR
;; NIL
;; NIL
;; CH16(82): (compile 'trmapcar)
;; TRMAPCAR
;; NIL
;; NIL
;; CH16(83): (compile 'trmapcarn)
;; TRMAPCARN
;; NIL
;; NIL
;; CH16(84): (compile 'loopcar)
;; LOOPCAR
;; NIL
;; NIL
;; CH16(85): (time (dotimes (i 100) (rmapcar #'1+ #[1 1000])))
;; ; cpu time (non-gc) 0.004690 sec user, 0.001249 sec system
;; ; cpu time (gc)     0.022112 sec user, 0.001469 sec system
;; ; cpu time (total)  0.026802 sec user, 0.002718 sec system
;; ; real time  0.055147 sec
;; ; space allocation:
;; ;  200,100 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(86): (time (dotimes (i 10000) (rmapcar #'1+ #[1 1000])))
;; ; cpu time (non-gc) 0.435972 sec user, 0.004966 sec system
;; ; cpu time (gc)     0.106273 sec user, 0.001591 sec system
;; ; cpu time (total)  0.542245 sec user, 0.006557 sec system
;; ; real time  2.232870 sec
;; ; space allocation:
;; ;  20,010,000 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(87): (time (dotimes (i 10000) (rmapcar #'1+ #[1 100000])))
;; Error: Stack overflow (signal 1000)
;;   [condition type: SYNCHRONOUS-OPERATING-SYSTEM-SIGNAL]

;; Restart actions (select using :continue):
;;  0: continue computation
;;  1: Return to Top Level (an "abort" restart).
;;  2: Abort entirely from this (lisp) process.
;; [1c] CH16(88): :res
;; CH16(91): (time (dotimes (i 10000) (trmapcar #'1+ #[1 10000])))
;; ; cpu time (non-gc) 1.296556 sec user, 0.016192 sec system
;; ; cpu time (gc)     1.035830 sec user, 0.012415 sec system
;; ; cpu time (total)  2.332386 sec user, 0.028607 sec system
;; ; real time  10.125884 sec
;; ; space allocation:
;; ;  200,010,000 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(92): (time (dotimes (i 10000) (trmapcarn #'1+ #[1 10000])))
;; ; cpu time (non-gc) 1.810156 sec user, 0.016250 sec system
;; ; cpu time (gc)     0.281005 sec user, 0.004593 sec system
;; ; cpu time (total)  2.091161 sec user, 0.020843 sec system
;; ; real time  8.951292 sec
;; ; space allocation:
;; ;  100,010,000 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(93): (time (dotimes (i 10000) (rmapcar #'1+ #[1 10000])))
;; ; cpu time (non-gc) 4.754592 sec user, 0.046802 sec system
;; ; cpu time (gc)     3.266800 sec user, 0.031106 sec system
;; ; cpu time (total)  8.021392 sec user, 0.077908 sec system
;; ; real time  35.770866 sec
;; ; space allocation:
;; ;  200,010,000 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(94): (time (dotimes (i 10000) (mapcar #'1+ #[1 10000])))
;; ; cpu time (non-gc) 2.642836 sec user, 0.026375 sec system
;; ; cpu time (gc)     0.557322 sec user, 0.008985 sec system
;; ; cpu time (total)  3.200158 sec user, 0.035360 sec system
;; ; real time  13.903498 sec
;; ; space allocation:
;; ;  200,010,000 cons cells, 0 other bytes, 0 static bytes
;; NIL
;; CH16(95): (time (dotimes (i 10000) (loopcar #'1+ #[1 10000])))
;; ; cpu time (non-gc) 3.118650 sec user, 0.031030 sec system
;; ; cpu time (gc)     1.587045 sec user, 0.017388 sec system
;; ; cpu time (total)  4.705695 sec user, 0.048418 sec system
;; ; real time  21.371231 sec
;; ; space allocation:
;; ;  200,020,000 cons cells, 0 other bytes, 0 static bytes
;; NIL

(defun twos-complement (n)
  (1+ (lognot n)))

(defun binary-to-bits (n &optional (width 8))
;; (defun binary-to-bits (n &optional (width (ceiling (log (abs n) 2))))
  (if (minusp n)
      (flip-bits (binary-to-bits (lognot n) width))
      (format nil "~V,'0B" width n)))

(defun flip-bits (s)
  (map 'string #'flip-bit s))

(defun flip-bit (ch)
  (ecase ch
    (#\0 #\1)
    (#\1 #\0)))

(defun test-logical (x y)
;  (dolist (f '(logand logior logxor logeqv lognand lognor logandc1 logandc2 logorc1 logorc2))
;  (dolist (f '(logand logandc1 logandc2 logeqv logior lognand lognor lognot logorc1 logorc2 logxor)) D'oh!
  (dolist (f '(logand logandc1 logandc2 logeqv logior lognand lognor logorc1 logorc2 logxor))
    (format t "(~A ~15T ~A ~A) ~25T=> ~A~%" (string-downcase (symbol-name f))
            (binary-to-bits x)
            (binary-to-bits y)
            (binary-to-bits (funcall f x y)))) )
