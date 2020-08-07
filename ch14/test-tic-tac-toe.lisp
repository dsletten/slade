;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               test-tic-tac-toe.lisp
;;;
;;;   STARTED:            Tue Mar 12 18:05:56 2002
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
(load "~/lisp/programs/utils.lisp")
(load "tic-tac-toe.lisp")

(defconstant row-tests-3 '(((#2A((nil nil nil)
				 (#\X #\X #\X)
				 (#\O nil #\O))) #\X)
			   ((#2A((nil nil nil)
				 (#\O #\O #\O)
				 (#\X nil #\X))) #\O)
			   ((#2A((#\X #\X #\X)
				 (nil nil nil)
				 (#\O nil #\O))) #\X)
			   ((#2A((#\O #\O #\O)
				 (nil nil nil)
				 (#\X nil #\X))) #\O)
			   ((#2A((nil #\X #\X)
				 (#\X #\O #\X)
				 (#\O nil #\O))) nil)
			   ((#2A((nil nil nil)
				 (#\O nil #\O)
				 (#\X #\X #\X))) #\X)
			   ((#2A((nil #\X nil)
				 (#\X nil #\X)
				 (#\O #\O #\O))) #\O)))

(defconstant column-tests-3 '(((#2A((nil #\X nil)
				    (nil #\X nil)
				    (#\O #\X #\O))) #\X)
			      ((#2A((nil #\O nil)
				    (nil #\O nil)
				    (#\X #\O #\X))) #\O)
			      ((#2A((#\X nil #\O)
				    (#\X nil nil)
				    (#\X nil #\O))) #\X)
			      ((#2A((#\O nil #\X)
				    (#\O nil nil)
				    (#\O nil #\X))) #\O)
			      ((#2A((nil #\X #\X)
				    (#\X #\O #\X)
				    (#\O nil #\O))) nil)
			      ((#2A((nil #\O #\X)
				    (#\O nil #\X)
				    (#\X #\O #\X))) #\X)
			      ((#2A((nil #\X #\O)
				    (#\X nil #\O)
				    (#\O #\X #\O))) #\O)))

(defconstant diagonal-tests-3 '(((#2A((#\X nil nil)
				      (nil #\X nil)
				      (#\O #\O #\X))) #\X)
				((#2A((#\O nil nil)
				      (nil #\O nil)
				      (#\X #\X #\O))) #\O)
				((#2A((#\X nil #\O)
				      (nil #\X nil)
				      (#\X nil #\O))) nil)
				((#2A((#\O nil #\X)
				      (#\O #\X nil)
				      (#\X nil #\X))) #\X)
				((#2A((nil #\X #\O)
				      (#\X #\O #\X)
				      (#\O nil #\O))) #\O)))

(defconstant diagonal-tests-3-aux '(((#2A((#\X nil nil)
					  (nil #\X nil)
					  (#\O #\O #\X)) #\X 2 1 0) nil)
				    ((#2A((#\X nil nil)
					  (nil #\X nil)
					  (#\O #\O #\X)) #\X 0 1 2) #\X)
				    ((#2A((#\X nil nil)
					  (nil #\X nil)
					  (#\O #\O #\X)) #\O 2 1 0) nil)
				    ((#2A((#\X nil nil)
					  (nil #\X nil)
					  (#\O #\O #\X)) #\O 0 1 2) nil)
				    ((#2A((#\O nil nil)
					  (nil #\O nil)
					  (#\X #\X #\O)) #\O 0 1 2) #\O)
				    ((#2A((#\O nil nil)
					  (nil #\O nil)
					  (#\X #\X #\O)) #\O 2 1 0) nil)
				    ((#2A((#\O nil nil)
					  (nil #\O nil)
					  (#\X #\X #\O)) #\X 0 1 2) nil)
				    ((#2A((#\O nil nil)
					  (nil #\O nil)
					  (#\X #\X #\O)) #\X 2 1 0) nil)
				    
				    ((#2A((#\X nil #\O)
					  (nil #\X nil)
					  (#\X nil #\O)) #\X 0 1 2) nil)
				    ((#2A((#\O nil #\X)
					  (#\O #\X nil)
					  (#\X nil #\X)) #\X 2 1 0) #\X)
				    ((#2A((nil #\X #\O)
					  (#\X #\O #\X)
					  (#\O nil #\O)) #\O 2 1 0) #\O)))

(test 'check-rows-3-aux (mapcan #'(lambda (l)
				    (let ((result (second l)))
				      (list
				       (cons (cons (caar l) (list #\X))
					     (list (and result
							(if (eq result #\X)
							    #\X
							    nil))))
				       (cons (cons (caar l) (list #\O))
					     (list (and result
							(if (eq result #\O)
							    #\O
							    nil)))) )))
				row-tests-3))
					  
(test 'check-rows-3 row-tests-3)

(test 'check-columns-3 column-tests-3)

(test 'check-columns-3-aux (mapcan #'(lambda (l)
				    (let ((result (second l)))
				      (list
				       (cons (cons (caar l) (list #\X))
					     (list (and result
							(if (eq result #\X)
							    #\X
							    nil))))
				       (cons (cons (caar l) (list #\O))
					     (list (and result
							(if (eq result #\O)
							    #\O
							    nil)))) )))
				column-tests-3))

(test 'check-diagonals-3-test diagonal-tests-3-aux)

(test 'check-diagonals-3 diagonal-tests-3)

(test '3-in-row (append row-tests-3 column-tests-3 diagonal-tests-3))

(defconstant row-tests-2 '(((#2A((#\X #\X nil)
				 (nil nil nil)
				 (#\O #\X #\O))) #\X)
			   ((#2A((#\X nil #\X)
				 (nil nil nil)
				 (#\O #\X #\O))) #\X)
			   ((#2A((nil #\X #\X)
				 (nil nil nil)
				 (#\O #\X #\O))) #\X)
			   ((#2A((#\O #\O nil)
				 (nil nil nil)
				 (#\X #\O #\X))) #\O)
			   ((#2A((#\O nil #\O)
				 (nil nil nil)
				 (#\X #\O #\X))) #\O)
			   ((#2A((nil #\O #\O)
				 (nil nil nil)
				 (#\X #\O #\X))) #\O)))
; 			   ((#2A((nil #\O nil)
; 				 (nil #\O nil)
; 				 (#\X #\O #\X))) #\O)
; 			   ((#2A((#\X nil #\O)
; 				 (#\X nil nil)
; 				 (#\X nil #\O))) #\X)
; 			   ((#2A((#\O nil #\X)
; 				 (#\O nil nil)
; 				 (#\O nil #\X))) #\O)
; 			   ((#2A((nil #\X #\X)
; 				 (#\X #\O #\X)
; 				 (#\O nil #\O))) nil)
; 			   ((#2A((nil #\O #\X)
; 				 (#\O nil #\X)
; 				 (#\X #\O #\X))) #\X)
; 			   ((#2A((nil #\X #\O)
; 				 (#\X nil #\O)
; 				 (#\O #\X #\O))) #\O)))

;(test '2-in-row (append row-tests-2 column-tests-2 diagonal-tests-2))

(test 'check-rows-2 row-tests-2)

(defconstant column-tests-2 '(((#2A((nil #\X nil)
				    (nil #\X nil)
				    (#\O nil #\O))) #\X)
			      ((#2A((nil #\X nil)
				    (nil nil nil)
				    (#\O #\X #\O))) #\X)
			      ((#2A((nil nil nil)
				    (nil #\X nil)
				    (#\O #\X #\O))) #\X)
			      ((#2A((nil #\O nil)
				    (nil #\O nil)
				    (#\X nil #\X))) #\O)
			      ((#2A((nil #\O nil)
				    (nil nil nil)
				    (#\X #\O #\X))) #\O)
			      ((#2A((nil nil nil)
				    (nil #\O nil)
				    (#\X #\O #\X))) #\O)))
; 			      ((#2A((#\X nil #\O)
; 				    (#\X nil nil)
; 				    (#\X nil #\O))) #\X)
; 			      ((#2A((#\O nil #\X)
; 				    (#\O nil nil)
; 				    (#\O nil #\X))) #\O)
; 			      ((#2A((nil #\X #\X)
; 				    (#\X #\O #\X)
; 				    (#\O nil #\O))) nil)
; 			      ((#2A((nil #\O #\X)
; 				    (#\O nil #\X)
; 				    (#\X #\O #\X))) #\X)
; 			      ((#2A((nil #\X #\O)
; 				    (#\X nil #\O)
; 				    (#\O #\X #\O))) #\O)))

(test 'check-columns-2 column-tests-2)

(defconstant diagonal-tests-2 '(((#2A((#\X nil nil)
				      (nil #\X nil)
				      (#\O #\O nil))) #\X)
				((#2A((#\X nil nil)
				      (nil nil nil)
				      (#\O #\O #\X))) #\X)
				((#2A((nil nil nil)
				      (nil #\X nil)
				      (#\O #\O #\X))) #\X)
				((#2A((nil nil nil)
				      (nil #\O nil)
				      (#\X #\X #\O))) #\O)
				((#2A((#\O nil nil)
				      (nil #\O nil)
				      (#\X #\X nil))) #\O)
				((#2A((#\O nil nil)
				      (nil nil nil)
				      (#\X #\X #\O))) #\O)
				((#2A((#\X nil #\O)
				      (nil #\X nil)
				      (#\X nil #\O))) nil)
				((#2A((#\O nil nil)
				      (#\O #\X nil)
				      (#\X nil #\X))) #\X)
				((#2A((#\O nil #\X)
				      (#\O nil nil)
				      (#\X nil #\X))) #\X)
				((#2A((#\O nil #\X)
				      (#\O #\X nil)
				      (nil nil #\X))) #\X)
				((#2A((nil #\X nil)
				      (#\X #\O #\X)
				      (#\O nil #\O))) #\O)
				((#2A((nil #\X #\O)
				      (#\X nil #\X)
				      (#\O nil #\O))) #\O)
				((#2A((nil #\X #\O)
				      (#\X #\O #\X)
				      (nil nil #\O))) #\O)))

(test 'check-diagonals-2 diagonal-tests-2)

(defconstant check-corner-sneak '(((#2A((#\X nil nil)
					(nil #\O nil)
					(nil #\X nil))) #(2 0))
				  ((#2A((#\X nil nil)
					(nil #\O #\X)
					(nil nil nil))) #(0 2))
				  ((#2A((nil nil #\X)
					(nil #\O nil)
					(nil #\X nil))) #(2 2))
				  ((#2A((nil nil #\X)
					(#\X #\O nil)
					(nil nil nil))) #(0 0))
				  ((#2A((nil #\X nil)
					(nil #\O nil)
					(nil nil #\X))) #(0 2))
				  ((#2A((nil nil nil)
					(#\X #\O nil)
					(nil nil #\X))) #(2 0))
				  ((#2A((nil #\X nil)
					(nil #\O nil)
					(#\X nil nil))) #(0 0))
				  ((#2A((nil nil nil)
					(nil #\O #\X)
					(#\X nil nil))) #(2 2))))

(test 'corner-sneak-attack check-corner-sneak)