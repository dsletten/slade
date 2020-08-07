;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch11.lisp
;;;
;;;   STARTED:            Wed Jan  2 17:56:30 2002
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

;;;
;;;    Wow...
;;;    
; [15]> (macroexpand-1 '(do ((i 0 (1+ i))) ((> i 5) 'foo)))
; (BLOCK NIL
;  (LET ((I 0))
;   (TAGBODY #:G1056 (IF (> I 5) (GO #:G1057)) (PSETQ I (1+ I)) (GO #:G1056)
;    #:G1057 (RETURN-FROM NIL (PROGN 'FOO))))) ;
; T

; [53]> (macroexpand '(rotatef (second x) (sixth x) (fourth x)))
; (LET* ((#:G1165 X) (#:G1167 X) (#:G1169 X))
;  (MULTIPLE-VALUE-BIND (#:G1170) (SECOND #:G1165)
;   (MULTIPLE-VALUE-BIND (#:G1166) (SIXTH #:G1167)
;    (MULTIPLE-VALUE-BIND (#:G1168) (FOURTH #:G1169)
;     (SYSTEM::%RPLACA (CDR #:G1165) #:G1166)
;     (SYSTEM::%RPLACA (CDR (CDDDDR #:G1167)) #:G1168)
;     (SYSTEM::%RPLACA (CDDDR #:G1169) #:G1170))))
;  NIL) ;
; T

;;;
;;;    Compare:
;;;
; (defun square (x)
;   (* x x) )

; (square (+ 3 4)) => (square 7) => (* 7 7) => 49

; (defmacro square-m (x)
;   (list '* x x) )

; OR

; (defmacro square-m (x)
;   `(* ,x ,x) )

; (macroexpand '(square-m (+ 3 4))) => (* (+ 3 4) (+ 3 4)) ;

;;;
;;;    To avoid the multiple evaluation:
;;;    (However, this may cause variable capture!)
;;;    
; (defmacro square-m (x)
;   (list 'let
; 	(list (list 'x-val x))
; 	(list '* 'x-val 'x-val)) )

; OR

; (defmacro square-m (x)
;   `(let ((x-val ,x))
;     (* x-val x-val)) )

;;;
;;;    11.10.2
;;;
(defmacro swap (x y)
  (list 'let (list (list 'x1 x)
		   (list 'y1 y))
	(list 'setf x 'x1 y 'y1)) )

(defmacro swap (x y)
  `(let ((x1 ,x)
	 (y1 ,y))
    (setf ,x x1 ,y y1)) )

;Duh!
(defmacro swap (x y)
  (list 'psetf x y y x) )

(defmacro swap (x y)
  `(psetf ,x ,y ,y ,x) )

;;;
;;;    11.10.5
;;;
(defmacro while (test &body body)
  (append (list 'do (list)
		(list (list 'not test)))
	  body) )

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body) )

(defmacro while-1 (test result &body body)
  `(do ()
       ((not ,test) ,result)
     ,@body) )

;;;
;;;    11.10.6
;;;
(defmacro until (test &body body)
  (append (list 'do (list)
		(list test))
	  body) )

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body) )

(defmacro until-1 (test result &body body)
  `(do ()
      (,test ,result)
     ,@body) )

;;;
;;;    Macro practice--
;;;

;;;
;;;    Slade 2.11.5
;;;    
(defmacro signum-m (x)
  (list 'cond
	(list (list 'plusp x) 1)
	(list (list 'minusp x) -1)
	(list (list 'zerop x) 0)) )

(defmacro signum-m (x)
  `(cond ((plusp ,x) 1)
         ((minusp ,x) -1)
         ((zerop ,x) 0)) )

(defmacro signum-m (x)
  (list 'or
	(list 'and (list 'plusp x) 1)
	(list 'and (list 'minusp x) -1)
	(list 'and (list 'zerop x) 0)) )

(defmacro signum-m (x)
  `(or (and (plusp ,x) 1)
       (and (minusp ,x) -1)
       (and (zerop ,x) 0)) )

;;
;;    Avoid multiple evaluation.
;;    
(defmacro signum-m (x)
  `(let ((val ,x))
    (or (and (plusp val) 1)
        (and (minusp val) -1)
        (and (zerop val) 0))) )
