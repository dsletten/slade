;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               guess.lisp
;;;;
;;;;   Started:            Tue Oct 12 04:06:04 2010
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
;;;;   Todo:
;;;;   -Understand flow of execution
;;;;   -Eliminate global vars
;;;;   -Eliminate CATCH/THROW
;;;;   -Convert to BST
;;;;   -Compare with earlier versions
;;;;   -Write Clojure version
;;;;   -Xah
;;;;

(defpackage guess (:use common-lisp))

(in-package guess)

(defvar *category*)
(defvar *determiner-flag*)
;(defvar *binary-tree*)

(defun binary-init (item category flag)
  (setf *category* category
        *determiner-flag* flag
        (get '*binary-tree* 'answer)   item
        (get '*binary-tree* 'question) nil
        (get '*binary-tree* 'yes)      nil
        (get '*binary-tree* 'no)       nil) )

(defun output (stream control &rest args)
  (apply #'format stream control args)
  (force-output))

(defun query (yes-action no-action else-action)
  (case (read)
    ((y yes t) (funcall yes-action))
    ((n no nil) (funcall no-action))
    ((q quit) (throw 'quit-action '*end-of-binary*))
    (otherwise (output t "~&Type Yes, No, or Quit.~%")
               (funcall else-action))) )

(defun start ()
  (output t "~&Think of ~A~A." (sub-determ *category*) *category*)
  (output t "~&Have you got one yet? ")
  (query #'(lambda () (process-node '*binary-tree*))
         #'start
         #'start))

(defun determiner (word)
  (cond ((null *determiner-flag*) "")
        (t (sub-determ word))) )
;;
;;    This adds a space on the end of 'an' or 'a' so that
;;    the function above can return an empty string if no
;;    determiner is needed.
;;    
(defun sub-determ (word)
  (let ((str (cond ((stringp word) word)
                   (t (string word)))) )
    (case (char-upcase (char str 0))
      ((#\A #\E #\I #\O #\U) "an ")
      (otherwise "a "))) )

;;
;;    A node should have either a question OR an answer.
;;    
(defun process-node (tree-node)
  (cond ((get tree-node 'question)
         (process-question (get tree-node 'question)
                           tree-node))
        ((get tree-node 'answer)
         (process-answer (get tree-node 'answer)
                         tree-node))
        (t 'error-in-process-node)) )
;;
;;    A node with a question should have both a YES node and a
;;    NO node.
;;    
(defun process-question (ques node)
  (output t "~&~A " ques)
  (query #'(lambda () (process-node (get node 'yes)))
         #'(lambda () (process-node (get node 'no)))
         #'(lambda () (process-question ques node))) )

(defun process-answer (ans node)
  (output t "~&Are you thinking of ~A~S? "
          (determiner ans) ans)
  (query #'(lambda ()
             (output t "~&Hot tomatoes!  I guessed it!~%")
             (play-again? nil))
         #'(lambda () (add-item node ans) (play-again? t))
         #'(lambda () (process-answer ans node))) )

(defun add-item (old-node old-item)
  (let ((yes-node (gensym "NODE"))
        (no-node (gensym "NODE"))
        (new-item nil)
        (new-question nil))
    (output t "~&What ~A were you thinking of? " *category*)
    (setf new-item (read))
    (output t "~&Type a yes/no question that can distinguish ~
                              between ~A~S and ~A~S.~%=> "
            (determiner old-item) old-item
            (determiner new-item) new-item)
    (clear-input)
    (setf new-question (read-line))
    (set-up-old-node old-node new-question yes-node no-node)
;    (setf back-up-flag t)
    (output t "~&And how would you answer that question for ~
                              ~A~S? " (determiner new-item) new-item)
    (tagbody again
       (query #'(lambda ()
                  (set-up-new-node yes-node new-item)
                  (set-up-new-node no-node old-item))
              #'(lambda ()
                  (set-up-new-node no-node new-item)
                  (set-up-new-node yes-node old-item))
              #'(lambda () (go again)))) ) )

;;
;;    This wipes out info from the old node (namely the ANSWER),
;;    but ADD-ITEM still retains the old info.
;;    
(defun set-up-old-node (old-node new-question yes-node no-node)
  (setf (get old-node 'question) new-question
        (get old-node 'answer) nil
        (get old-node 'yes) yes-node
        (get old-node 'no) no-node) )

(defun set-up-new-node (node item)
  (setf (get node 'answer) item) )

(defun play-again? (back-up-flag)
  (output t "~&Do you want to play again? ")
  (query #'start
         #'(lambda ()
             (save-data? back-up-flag)
             (output t "~&So long, ~A lover.~%~%" *category*))
         #'(lambda () (play-again? back-up-flag))))

(defun save-data? (back-up-flag)
  (cond ((null back-up-flag) nil)
        (t (output t "~&Do you wish to save the current ~A ~
                                     data base? " *category*)
           (query #'back-up-data
                  #'(lambda () nil)
                  #'(lambda () (save-data? back-up-flag)))) ))

(defun back-up-data ()
  (let ((filename (concatenate 'string
                               "binary."
                               *category*)))
    (output t "Backing up data to file: ~A~%" filename)
    (with-open-file (out filename
                     :direction :output
                     :if-exists :supersede)
      (format out "(setf *category* ~S)~%" *category*)
      (format out "(setf *determiner-flag* ~A)~%" *determiner-flag*)
      (back-up-node '*binary-tree* out))
    (output t "~&Backup complete.")
    (output t "~&To restore data, type (load ~S)~%" filename)))

(defun back-up-node (node stream)
  (let ((question (get node 'question))
        (answer (get node 'answer)))
    (cond ((and (null question) (null answer)) nil)
          (question
           (format stream "(setf (get '~A 'question) ~S)~%"
                   node question)
           (let ((yes-node (get node 'yes))
                 (no-node (get node 'no)))
             (format stream "(setf (get '~A 'yes) '~A)~%"
                     node yes-node)
             (back-up-node yes-node stream)
             (format stream "(setf (get '~A 'no) '~A) ~%"
                     node no-node)
             (back-up-node no-node stream)))
          (answer
           (format stream "(setf (get '~A 'answer) '~A)~%"
                   node answer))
          (t nil))))

(defun binary ()
  (catch 'quit-action
    (start)))
