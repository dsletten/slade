;#!/usr/local/bin/clisp

;;
;   NAME:               find-primes.lisp
;
;   STARTED:            010921
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

(defun find-primes (n-primes)
  (labels ((find-primes-aux (i my-primes)
	     (let ((this-prime (car my-primes)))
	       (cond ((null my-primes) t)
		     ((> (* this-prime this-prime) i) t)
		     ((zerop (mod i this-prime)) nil)
		     (t (find-primes-aux i (cdr my-primes)))) ) ) )
    (let ((primes '()))
      (do ((i 2 (+ i step))
	   (step 1 2))
	  ((>= (length primes) n-primes) (reverse primes))
	(if (find-primes-aux i (reverse primes))
	    (push i primes)))) ) )

(dolist (i (find-primes 20))
  (format t "~&~D~%" i))
