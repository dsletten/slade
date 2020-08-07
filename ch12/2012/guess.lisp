;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               guess.lisp
;;;;
;;;;   Started:            Tue Nov 29 22:33:22 2011
;;;;   Modifications:      Sun Feb  5 03:09:44 2012 Copied from ch. 9 -- using structure rather than class
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
;;;;   (defvar *seed* (make-answer :value "cat"))
;;;;   (setf (question *seed*) *seed*)
;;;;   (make-game :category "animal" :tree *seed*)
;;;;   
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :guess (:use :common-lisp :test))

(load "/Users/dsletten/lisp/books/Slade/ch12/2012/game.lisp")

(in-package :guess)

(defun play (game)
  (catch 'quit
    (loop
       (format t "~%Think of ~A." (determiner-plus-word game))
       (when (query "~%Have you got one yet? ")
         (play-round game)
         (unless (play-again-p)
           (when (save-data-p game)
             (back-up-data game))
           (format t "~%So long, ~A lover.~2%" (category game))
           (return)))) ))

(defun determiner-plus-word (game &optional (word (category game)))
  (if (null (determiner-flag game))
      word
      (format nil "~A ~A" (determiner word) word)))

(defun determiner (word)
  (if (stringp word)
      (if (exceptional-word-p word)
          "an"
          (case (char-downcase (char word 0))
            ((#\a #\e #\i #\o #\u) "an")
            (otherwise "a")))
      (determiner (string word))))

(defun exceptional-word-p (word)
  (member word '("hour" "honor" "herb" "honest" "honorable" "honorary" "hourglass" "hourly") :test #'string-equal))

(defun query (prompt)
  (format t prompt)
  (force-output)
  (case (read)
    ((y yes t) t)
    ((n no nil) nil)
    ((q quit) (throw 'quit 'game-over))
    (otherwise (format t "~&Type Yes, No, or Quit.~%")
               (query prompt))))

(defun play-round (game)       
  (process-node game (tree game)))

(defgeneric process-node (game node))

(defmethod process-node ((g game) (q question))
  (if (query (format nil "~A " (text q)))
      (process-node g (yes q))
      (process-node g (no q))))

(defmethod process-node ((g game) (a answer))
  (if (query (format nil "~%Are you thinking of ~A? " (determiner-plus-word g (value a))))
      (brag)
      (add-new-entry g a)))

(defun add-new-entry (game old-answer)
  (format t "~%What ~A were you thinking of? " (category game))
  (force-output)
  (let ((new-val (get-answer)))
    (format t "~%Type a yes/no question that can distinguish between ~A and ~A.~%=> "
            (determiner-plus-word game (value old-answer)) (determiner-plus-word game new-val))
    (force-output)
    (let* ((new-question (make-question :text (check-for-question-mark (read-line)))) ; Empty question text?!
           (new-answer (make-answer :value new-val :question new-question))
           (new-side (if (query (format nil "~%And how would you answer that question for ~A? " (determiner-plus-word game new-val)))
                         'yes
                         'no)))
      (add-node game old-answer new-question new-answer new-side))))

(defun get-answer ()
  (let ((answer (string-trim " " (read-line))))
    (or (check-answer answer) (get-answer))))

(defun check-answer (answer)
  (cond ((string= answer "") nil)
        ((starts-with answer "a") (check-answer (string-trim " " (subseq answer 1))))
        ((starts-with answer "an") (check-answer (string-trim " " (subseq answer 2))))
        (t answer)))

(defun starts-with (phrase determiner)
  (let ((determiner-length (length determiner)))
    (and (> (length phrase) determiner-length)
         (string-equal (subseq phrase 0 determiner-length) determiner)
         (char= (char phrase determiner-length) #\space))))

(defun check-for-question-mark (question)
  (let ((new-question (string-trim " " question)))
    (if (char= (char new-question (1- (length new-question))) #\?)
        new-question
        (concatenate 'string new-question "?"))))

(defun play-again-p ()
  (query "~%Do you want to play again? "))

(defun save-data-p (game)
  (and (back-up game)
       (query (format nil "~%Do you wish to save the current ~A database? " (category game)))) )

(defvar *boasts* (vector "Hot tomatoes! I guessed it!"
                          "It was nothing, folks. I knew it all along."
                          "Aww. It was just a lucky guess."))
(defun brag ()
  (format t "~%~A~%" (aref *boasts* (random (length *boasts*)))) )

(defun back-up-data (game)
  (let ((file (make-pathname :name "binary" :type (category game))))
    (catch 'exists
      (when (probe-file file)
        (warn "Warning! The file '~A' already exists.~%" (file-namestring file))
        (unless (y-or-n-p "Do you wish to replace it? ")
          (warn "Back-up aborted. No files modified.~%")
          (throw 'exists nil)))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede)
        (format t "Backing up data to file: ~A~%" (file-namestring file))
        (print (category game) stream)
        (print (determiner-flag game) stream)
        (print (serialize (tree game)) stream))
      (setf (back-up game) nil)
      (format t "~%Backup complete.")
      (format t "~%To restore data, type (reconstitute ~S)~%" (category game)))) )

(defgeneric serialize (node))

(defmethod serialize ((q question))
  (list (text q) (serialize (yes q)) (serialize (no q))))

(defmethod serialize ((a answer))
  (value a))

(defun reconstitute (category)
  (let ((file (make-pathname :name "binary" :type category)))
    (with-open-file (stream file)
      (format t "Loading game ~A~%" (file-namestring file))
      (let ((cat (read stream))
            (determiner-flag (read stream))
            (tree (deserialize (read stream))))
        (unless (string-equal category cat)
          (warn "Category ~A incompatible with filename ~A.~%" cat (file-namestring file)))
        (let ((game (make-game :category cat :determiner-flag determiner-flag :tree tree)))
          (prep-cache game)
          game)))) )

(defun deserialize (tree)
  (cond ((atom tree) (make-answer :value tree)) ; This answer is not yet properly connected to its parent question. See PREP-CACHE.
        (t (make-question :text (first tree) :yes (deserialize (second tree)) :no (deserialize (third tree)))) ))

;;;
;;;    Prepare the answer cache and also fix the QUESTION slots of the answers so that they
;;;    point to their parent question.
;;;    
(defun prep-cache (game)
  (labels ((prep-cache-aux (question)
             (let ((yes-branch (yes question))
                   (no-branch (no question)))
               (if (answerp yes-branch)
                   (progn (setf (question yes-branch) question)
                          (add-answer-to-cache game yes-branch))
                   (prep-cache-aux yes-branch))
               (if (answerp no-branch)
                   (progn (setf (question no-branch) question)
                          (add-answer-to-cache game no-branch))
                   (prep-cache-aux no-branch)))) )
    (prep-cache-aux (tree game))))

(defgeneric has-answer-p (game answer))

(defmethod has-answer-p ((g game) (a answer))
  (gethash (value a) (cache g)))

(defgeneric add-answer-to-cache (game answer))

(defmethod add-answer-to-cache ((g game) (a answer))
  (if (has-answer-p g a)
      (error "~A already exists as an answer." (value a))
      (setf (gethash (value a) (cache g)) a)))

(defgeneric add-node (game old-answer new-question new-answer new-side))
(defmethod add-node ((g game) (old-answer answer) (new-question question) (new-answer answer) new-side)
  (if (has-answer-p g new-answer)
      (error "~A already exists as an answer." (value new-answer))
      (let ((parent (question old-answer)))
        (cond ((eq old-answer parent) (setf (tree g) new-question))
              ((eq old-answer (yes parent)) (setf (yes parent) new-question))
              (t (setf (no parent) new-question)))
        (setf (question old-answer) new-question)
        (ccase new-side
          (yes (setf (yes new-question) new-answer
                     (no new-question) old-answer))
          (no (setf (no new-question) new-answer
                    (yes new-question) old-answer)))
        (add-answer-to-cache g new-answer)
        (setf (back-up g) t))))
