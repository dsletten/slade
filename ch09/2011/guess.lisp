;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               guess.lisp
;;;;
;;;;   Started:            Tue Nov 29 22:33:22 2011
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
;;;;   (play (make-instance 'game :category "animal" :tree (make-instance 'answer :value "cat")))
;;;;
;;;;   Notes:
;;;;   - Tatar pg. 129, 134, 157
;;;;   - On Lisp (Compiled networks ch. 6)
;;;;   - Slade ex. 12.6.6
;;;;   - Touretzky (Discrimination net pg. 374 ch. 12)
;;;;   - PAIP ex. 3.5
;;;;   
;;;;   Hashtable vs. tree
;;;;   Randomize (Don't always start w/ same question!)
;;;;
;(load "/Users/dsletten/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :guess (:use :common-lisp :test))

;(load "/Users/dsletten/lisp/books/Slade/ch09/2011/game.lisp")
(load "/home/slytobias/lisp/books/Slade/ch09/2011/game.lisp")

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

(defun add-new-entry (game old-answer)
  (format t "~%What ~A were you thinking of? " (category game))
  (force-output)
  (let ((new-val (get-answer)))
    (format t "~%Type a yes/no question that can distinguish between ~A and ~A.~%=> "
            (determiner-plus-word game (value old-answer)) (determiner-plus-word game new-val))
    (force-output)
    (let* ((new-question (make-instance 'question :text (check-for-question-mark (read-line)))) ; Empty question text?!
           (new-answer (make-instance 'answer :value new-val :question new-question))
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

(defun reconstitute (category)
  (let ((file (make-pathname :name "binary" :type category)))
    (with-open-file (stream file)
      (format t "Loading game ~A~%" (file-namestring file))
      (let ((cat (read stream))
            (determiner-flag (read stream))
            (tree (deserialize-tree (read stream))))
        (unless (string-equal category cat)
          (warn "Category ~A incompatible with filename ~A.~%" cat (file-namestring file)))
;;         (let ((game (make-instance 'game :category cat :determiner-flag determiner-flag :tree tree)))
;;           (cache-node game tree)
;; ;;           (prep-cache game)
;;           game)))) )
        (make-instance 'game :category cat :determiner-flag determiner-flag :tree tree)))) )

(defun deserialize-tree (tree)
  (cond ((atom tree) (make-instance 'answer :value tree)) ; This answer is not yet properly connected to its parent question. See PREP-CACHE.
        (t (make-instance 'question :text (first tree) :yes (deserialize-tree (second tree)) :no (deserialize-tree (third tree)))) ))

;(deserialize-tree '("Does it have stripes?" "tiger" ("Is it a domestic animal?" "horse" "gorilla")))

;; (defun find-parent (node target)
;;   (cond ((answerp node) (and (equalp (value node) target) (values node nil)))
;;         ((and (answerp (yes node))
;;               (equalp (value (yes node)) target))
;;          (values node 'yes))
;;         ((and (answerp (no node))
;;               (equalp (value (no node)) target))
;;          (values node 'no))
;;         (t (multiple-value-bind (parent side) (find-parent (yes node) target)
;;              (if parent
;;                  (values parent side)
;;                  (find-parent (no node) target)))) ))


