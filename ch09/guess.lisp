;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               guess.lisp
;;;;
;;;;   Started:            Tue Nov 23 11:57:12 2004
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
;;;;   Notes: Rewrite of Slade's binary guessing game. Binary tree is used
;;;;   to represent database. Internal nodes are questions, with yes and no
;;;;   branches. External nodes are answers. This is not a binary search
;;;;   tree--the nodes are not ordered. The tree is built using a variation of
;;;;   the BST list implementation used by SICP, etc...
;;;;
;;;;   This tends to produce trees which are biased towards the 'Yes' branch
;;;;   since the user tends to phrase new questions whose answers are
;;;;   affirmative for the new item.
;;;;
;;;;   See also Tatar ch. 6 and ch. 7 Ex. 8
;;;;

(defpackage guess (:use common-lisp))

(in-package guess)

(defun make-node (elt l r)
  (list elt l r))

(defun node-elt (bst)
  (first bst))

(defun node-no (bst)
  (second bst))

(defun node-yes (bst)
  (third bst))

(defun emptyp (bst)
  (null bst))

(defun answerp (bst)
  (and (emptyp (node-no bst))
       (emptyp (node-yes bst))))

(defvar *database* '())
(defvar *determiner-flag* nil)
(defvar *category* nil)
(defvar *backup-flag* nil)

(defun binary-init (item category flag)
  (setf *database* (make-node item '() '())
	*category* category
	*determiner-flag* flag
	*backup-flag* nil))

(defun binary ()
  (catch 'quit-action
    (start)))

(defun query (yes-action no-action else-action)
  (case (read)
    ((y yes t) (funcall yes-action))
    ((n no nil) (funcall no-action))
    ((q quit) (throw 'quit-action '*end-of-binary*))
    (otherwise
     (flush-format t "~&Type yes, no, or quit.~%")
     (funcall else-action))))

(defun start ()
  (flush-format t "~&Think of ~A~A.~%" (sub-determ *category*)
		*category*)
  (flush-format t "Have you got one yet? ")
  (query #'(lambda () (process-node *database*))
	 #'start
	 #'start))

(defun determiner (word)
  (if *determiner-flag*
      (sub-determ word)
      ""))

(defun sub-determ (word)
  (case (char-downcase (char word 0))
    ((#\a #\e #\i #\o #\u) "an ")
    (otherwise "a ")))

(defun process-node (node)
  (if (answerp node)
      (process-answer node)
      (process-question node)))

(defun process-answer (node)
  (let ((ans (node-elt node)))
    (flush-format t "Are you thinking of ~A~A? "
		  (determiner ans) ans)
    (query #'(lambda ()
	       (flush-format t "Flubbernuggets! I guessed it!~%")
	       (play-again?))
	   #'(lambda ()
	       (add-item node ans)
	       (play-again?))
	   #'(lambda ()
	       (process-answer node)))) )

(defun process-question (node)
  (let ((question (node-elt node)))
    (flush-format t "~A " question)
    (query #'(lambda ()
	       (process-node (node-yes node)))
	   #'(lambda ()
	       (process-node (node-no node)))
	   #'(lambda ()
	       (process-question node)))) )

(defun add-item (node old-item)
  (flush-format t "What ~A were you thinking of? " *category*)
  (let ((new-item (string-trim " " (read-line))))
    (flush-format t "Type a yes/no question that can distinguish ~
                     between ~A~A and ~A~A.~%=> "
		  (determiner old-item) old-item
		  (determiner new-item) new-item)
    (let ((new-question (string-trim " " (read-line)))
	  (response (get-response new-item)))
      (setf *backup-flag* t)
      (if (eq response 'yes)
	  (setf (first node) new-question
		(second node) (make-node old-item '() '())
		(third node) (make-node new-item '() '()))
	  (setf (first node) new-question
		(second node) (make-node new-item '() '())
		(third node) (make-node old-item '() '()))) )))

(defun get-response (item)
  (flush-format t "And how would you answer that question for ~
                   ~A~A? " (determiner item) item)
  (query #'(lambda () 'yes)
	 #'(lambda () 'no)
	 #'(lambda () (get-response item))))

(defun play-again? ()
  (flush-format t "Do you want to play again? ")
  (query #'start
	 #'(lambda ()
	     (save-data?)
	     (flush-format t "So long, ~A lover.~2%" *category*))
	 #'play-again?))

(defun save-data? ()
  (cond (*backup-flag*
	 (flush-format t "Do you wish to save the current ~A database? "
		       *category*)
	 (query #'back-up-data
		#'(lambda () nil)
		#'save-data?))
	(t nil)))

(defun back-up-data ()
  (let ((filename (concatenate 'string "binary." *category*)))
    (with-open-file (back-up filename :direction :output :if-exists :supersede)
      (format back-up "(setf *category* ~S)~%" *category*)
      (format back-up "(setf *determiner-flag* ~S)~%" *determiner-flag*)
      (format back-up "(setf *database* '~S)~%" *database*))
    (flush-format t "Backup complete.~%")
    (flush-format t "To restore data, type (load ~S)~%" filename))
  (setf *backup-flag* nil))
  
(defun flush-format (&rest args)
  (apply #'format args)
  (finish-output))
