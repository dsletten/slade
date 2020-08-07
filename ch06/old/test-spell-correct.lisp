;#!/usr/local/bin/clisp

;;
;   NAME:               test-spell-correct.l
;
;   STARTED:            011016
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
(load "spell-correct-2")
(load "/home/httpd/cgi-bin/utils")

(test 'inc-char
      '(((#\a) #\b)
	((#\A) #\B)
	((#\E) #\F)
	((#\z) #\{)
	((#\1) #\2)))

(defun test-inc-char ()
  (let ((test-data '((#\a #\b)
		     (#\A #\B)
		     (#\E #\F)
		     (#\z #\{)
		     (#\1 #\2))))
    (dolist (test test-data)
      (let* ((ch (car test))
	     (check-val (cadr test))
	     (value (inc-char ch)))
	(if (equal value check-val)
	    (format t "~&Test passed: ~A => ~A~%" ch value)
	    (format t "~&Test FAILED: ~A => ~A [Should be ~A]~%"
		    ch value check-val)))) ) )

(test 'string-insert
      '((("pung" #\k 3) "punkg")
	(("What a swat!" #\e 9) "What a sweat!")))