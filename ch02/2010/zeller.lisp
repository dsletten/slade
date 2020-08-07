#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               zeller.lisp
;;;;
;;;;   Started:            Mon Jul 26 21:37:37 2010
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
;;;;   Notes: Uses King's algorithm JPB ch. 2 ex. 5
;;;;
;;;;

(load "/Users/dsletten/lisp/packages/lang")
(load "/Users/dsletten/lisp/packages/shell")

(defun zeller (year month day)
  (let* ((m (+ (mod (+ month (- 12 (1+ 2))) 12) (1+ 2)))
         (y (- year (truncate (- m month) 12)))
         (j (truncate y 100))
         (k (mod y 100))
         (q day))
    (mod (+ q
            (truncate (* 26 (1+ m)) 10)
            k
            (truncate k 4)
            (truncate j 4)
            (* 5 j))
         7)))

(defun shift-day-of-week (h)
  (aref lang:day-names (1- (+ (mod (+ h (- 7 1)) 7) 1))))

(let ((year (read-from-string (shell:get-argv 0)))
      (month (read-from-string (shell:get-argv 1)))
      (day (read-from-string (shell:get-argv 2))))
  (format t "~A~%" (shift-day-of-week (zeller year month day))))
