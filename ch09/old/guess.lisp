;#!/usr/local/bin/clisp

;;
;   NAME:               guess.lisp
;
;   STARTED:            011219
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
;   NOTES: This is Slade's code with only minor cosmetic changes, e.g.,
;          indented properly!
;
;;
(defun binary-init (item category flag)
  (setf *category* category
        *determiner-flag* flag
        (get '*binary-tree* 'answer)   item
        (get '*binary-tree* 'question) nil
        (get '*binary-tree* 'yes)      nil
        (get '*binary-tree* 'no)       nil) )

(defun binary ()
  (catch 'quit-action
    (let ((back-up-flag nil))
      (labels ((query (yes-action no-action else-action)
                 (case (read *standard-input*)
                   ((y yes t) (funcall yes-action))
                   ((n no nil) (funcall no-action))
                   ((q quit) (throw 'quit-action '*end-of-binary*))
                   (otherwise (format t "~&Type Yes, No, or Quit.~%")
                              (funcall else-action))) )

               (start ()
                 (format t "~&Think of ~A~A." (sub-determ *category*)
                         *category*)
                 (format t "~&Have you got one yet? ")
                 (query #'(lambda () (process-node '*binary-tree*))
                        #'start
                        #'start) )

               (determiner (word)
                 (cond ((null *determiner-flag*) "")
                       (t (sub-determ word))) )
               ;;
               ;;    This adds a space on the end of 'an' or 'a' so that
               ;;    the function above can return an empty string if no
               ;;    determiner is needed.
               ;;    
               (sub-determ (word)
                 (let ((str (cond ((stringp word) word)
                                  (t (string word)))) )
                   (case (char-upcase (char str 0))
                     ((#\A #\E #\I #\O #\U) "an ")
                     (otherwise "a "))) )

               ;;
               ;;    A node should have either a question OR an answer.
               ;;    
               (process-node (tree-node)
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
               (process-question (ques node)
                 (format t "~&~A " ques)
                 (query #'(lambda () (process-node (get node 'yes)))
                        #'(lambda () (process-node (get node 'no)))
                        #'(lambda () (process-question ques node))) )

               (process-answer (ans node)
                 (format t "~&Are you thinking of ~A~S? "
                         (determiner ans) ans)
                 (query #'(lambda ()
                            (format t "~&Hot tomatoes!  I guessed it!~%")
                            (play-again?))
                        #'(lambda () (add-item node ans) (play-again?))
                        #'(lambda () (process-answer ans node))) )

               (add-item (old-node old-item)
                 (let ((yes-node (gensym "NODE"))
                       (no-node (gensym "NODE"))
                       (new-item nil)
                       (new-question nil))
                   (format t "~&What ~A were you thinking of? "
                           *category*)
                   (setf new-item (read *standard-input*))
                   (format t "~&Type a yes/no question that can distinguish ~
                              between ~A~S and ~A~S.~%=> "
                           (determiner old-item) old-item
                           (determiner new-item) new-item)
                   (clear-input *standard-input*)
                   (setf new-question (read-line *standard-input*))
                   (set-up-old-node old-node new-question yes-node no-node)
                   (setf back-up-flag t)
                   (format t "~&And how would you answer that question for ~
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
               (set-up-old-node (old-node new-question yes-node no-node)
                 (setf (get old-node 'question) new-question
                       (get old-node 'answer) nil
                       (get old-node 'yes) yes-node
                       (get old-node 'no) no-node) )

               (set-up-new-node (node item)
                 (setf (get node 'answer) item) )

               (play-again? ()
                 (format t "~&Do you want to play again? ")
                 (query #'start
                        #'(lambda ()
                            (save-data?)
                            (format t "~&So long, ~A lover.~%~%" *category*))
                        #'play-again?) )

               (save-data? ()
                 (cond ((null back-up-flag) nil)
                       (t (format t "~&Do you wish to save the current ~A ~
                                     data base? " *category*)
                          (query #'back-up-data
                                 #'(lambda () nil)
                                 #'save-data?))) )

               (back-up-data ()
                 (let ((filename (concatenate 'string
                                              "binary."
                                              *category*))
                       (stream nil))
                   (format t "Backing up data to file: ~A~%" filename)
                   (setf stream (open filename
                                      :direction :output
                                      :if-exists :supersede))
                   (format stream "(setf *category* ~S)~%" *category*)
                   (format stream "(setf *determiner-flag* ~A)~%"
                           *determiner-flag*)
                   (back-up-node '*binary-tree* stream)
                   (close stream)
                   (format t "~&Backup complete.")
                   (format t "~&To restore data, type (load ~S)~%" filename)) )

               (back-up-node (node stream)
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
                         (t nil))) ))
        (start)))) )
