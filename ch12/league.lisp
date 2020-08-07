;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               league.lisp
;;;
;;;   STARTED:            Thu Feb 28 19:36:08 2002
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
(defun msort (l)
  (msort-aux l ()) )

(defun msort-aux (l temp-list)
  (cond ((null l) (msort-aux-1 temp-list ()))
	(t (msort-aux (cdr l)
		      (msort-add (list (car l)) temp-list)))) )

(defun msort-add (l temp-list)
  (cond ((null temp-list) (list l))
	((null (car temp-list)) (cons l (cdr temp-list)))
	(t (cons () (msort-add (lmerge l (car temp-list))
			       (cdr temp-list)))) ) )

(defun msort-aux-1 (l temp-list)
  (cond ((null l) temp-list)
	(t (msort-aux-1 (cdr l) (lmerge (car l) temp-list)))) )

(defun lmerge (a b)
  (cond ((null a) b)
	((null b) a)
	((inorderp a b)
	 (cons (car a) (lmerge (cdr a) b)))
	(t (cons (car b) (lmerge a (cdr b)))) ) )

(defun inorderp (a b)
  (cond ((numberp (car a))
	 (<= (car a) (car b)))
	((characterp (car a))
	 (char<= (car a) (car b)))
	(t (<= (games-behind (car a) (car b)) 0))) )
; 	(t (<= (games-behind (car a) (car b))
; 	       (games-behind (car b) (car a))))) )

(defstruct team
  name
  won
  lost
  average
  behind)

(defun games-behind (a b)
  (/ (+ (- (team-won b)
	   (team-won a))
	(- (team-lost a)
	   (team-lost b)))
     2) )

(defun game-average (team)
  (float (/ (team-won team)
	    (+ (team-won team)
	       (team-lost team)))) )

(defun print-standings (teams)
  (let* ((ranking (msort teams))
	 (top-team (first ranking)))
    (labels ((post-stats (team)
	       (setf (team-average team)
		     (game-average team))
	       (setf (team-behind team)
		     (games-behind team top-team)) )
	     (print-head ()
	       (format t "~&Team ~12TWon Lost Average Games Behind"))
	     (print-stats (team)
	       (format t "~&~A ~12T~A ~16T~A ~21T.~A ~29T~A"
		       (team-name team)
		       (team-won team)
		       (team-lost team)
		       (round-off (team-average team))
		       (team-behind team)) ))
      (mapc #'post-stats teams)
      (print-head)
      (mapc #'print-stats ranking))) )

(defun round-off (x)
  (truncate (* x 1000)) )

(defvar bears (make-team :name 'bears :won 7 :lost 12))
(defvar lions (make-team :name 'lions :won 4 :lost 14))
(defvar tigers (make-team :name 'tigers :won 9 :lost 6))
(defvar bulldogs (make-team :name 'bulldogs :won 12 :lost 4))
(defvar crimson (make-team :name 'crimson :won 8 :lost 8))

(defvar *league* (list bears lions tigers bulldogs crimson))

      