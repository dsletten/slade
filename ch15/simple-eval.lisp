;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               simple-eval.lisp
;;;
;;;   STARTED:            Tue Apr  9 23:32:07 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

(defun simple-eval (exp env)
  (cond ((numberp exp) exp)
	((stringp exp) exp)
	((characterp exp) exp)
	((atom exp)
	 (simple-env-lookup exp env))
	((eq (car exp) 'quote)
	 (cadr exp))
	((eq (car exp) 'cond)
	 (simple-eval-cond (cdr exp) env))
	(t (simple-apply (car exp)
			 (simple-eval-list (cdr exp) env)
			 env))) )

(defun simple-env-lookup (id env)
  (cdr (assoc id env)) )

(defun simple-eval-cond (exp env)
  (cond ((simple-eval (caar exp) env)
	 (simple-eval (cadar exp) env))
	(t (simple-eval-cond (cdr exp) env))) )

(defun simple-eval-list (exp env)
  (cond ((null exp) ())
	(t (cons (simple-eval (car exp) env)
		 (simple-eval-list (cdr exp) env)))) )

(defun simple-apply (proc args env)
  (cond ((atom proc)
	 (case proc
	   (car (caar args))
	   (cdr (cdar args))
	   (cons (cons (car args) (cadr args)))
	   (atom (atom (car args)))
	   (eq (eq (car args) (cadr args)))
	   (otherwise (simple-apply (simple-eval proc env)
				    args
				    env))))
	((eq (car proc) 'lambda)
	 (simple-eval (caddr proc)
		      (simple-pairlis (cadr proc) args env)))
	((eq (car proc) 'defun)
	 (simple-apply (caddr proc)
		       args
		       (cons (cons (cadr proc) (caddr proc)) env)))) )

(defun simple-pairlis (x y env)
  (cond ((null x) env)
	(t (cons (cons (car x) (car y))
		 (simple-pairlis (cdr x) (cdr y) env)))) )
