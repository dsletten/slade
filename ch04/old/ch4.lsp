;#!/usr/local/bin/clisp

;;
;   NAME:               ch4.lsp
;
;   STARTED:            010418
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


;;;
;;;    8.42
;;;
(defun my-subst (new old tree)
  (cond ((equal old tree) new)
	((atom tree) tree)
	(t (cons (my-subst new old (first tree))
		 (my-subst new old (rest tree)))) ) )

;;
;;    My original (only works if new/old are atoms)
;;    (Neither one works with lists if eql is used instead of equal)
(defun my-subst-1 (new old tree)
  (cond ((null tree) nil)
	((atom tree) (if (equal old tree)
			 new
		         tree))
	(t (cons (my-subst-1 new old (first tree))
		 (my-subst-1 new old (rest tree)))) ) )









;;;
;;;    4.7.5
;;;
(defun my-reverse (l)
  (my-reverse-aux () l) )

(defun my-reverse-aux (new old)
  (cond ((null old) new)
	(t (my-reverse-aux (cons (car old) new)
			   (cdr old)))) )
