;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Tue Mar 23 04:24:09 2004
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

;;;
;;;    5.7.5
;;;
(defun make-change (money)
  (let ((result '()))
    (when (>= money 100)
      (push (list (truncate money 100) 'dollars) result)
      (setf money (rem money 100)))
    (when (>= money 50)
      (push (list (truncate money 50) 'half-dollar) result)
      (setf money (rem money 50)))
    (when (>= money 25)
      (push (list (truncate money 25) 'quarter) result)
      (setf money (rem money 25)))
    (when (>= money 10)
      (push (list (truncate money 10) 'dimes) result)
      (setf money (rem money 10)))
    (when (>= money 5)
      (push (list (truncate money 5) 'nickel) result)
      (setf money (rem money 5)))
    (push (list money 'pennies) result)
    (reverse result)))

;;;
;;;    5.7.7
;;;
(let ((i 0)
      (args-list '()))
  (defun a+ (x y)
    (incf i)
    (push (list x y) args-list)
    (+ x y))

  (defun count+ ()
    (prog1 i (setf i 0)))

  (defun args+ ()
    (prog1 args-list
      (setf args-list '()))) )
    
;;;
;;;    5.7.8
;;;
(defun make-date (month day year)
  (cons month (cons day (cons year '()))) )

(defun date-month (date)
  (car date))

(defun date-day (date)
  (cadr date))

(defun date-day (year)
  (caddr date))

(defun (setf date-month) (new-month date)
  (setf (car date) new-month))

(defun (setf date-day) (new-day date)
  (setf (cadr date) new-day))

(defun (setf date-year) (new-year date)
  (setf (caddr date) new-year))

;;;
;;;    5.7.9
;;;
(defun (setf last) (obj l)
;  (setf (nthcdr (- (length l) 2) l) obj)) No NTHCDR SETF!
  (rplacd (nthcdr (- (length l) 2) l) obj))