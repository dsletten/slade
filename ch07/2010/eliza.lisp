;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               eliza.lisp
;;;;
;;;;   Started:            Mon Aug 23 15:04:48 2010
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
;;;;   Notes: Exercises 7.9.13
;;;;
;;;;

(defpackage eliza (:use common-lisp))

(in-package eliza)

(defun eliza (script)
  (format t "Hi.~%")
  (process-input script)
  (format t "End of Eliza.~%"))

(defparameter *prompt* ">>")
(defun show-prompt ()
  (format t "~A " *prompt*)
  (force-output))

(defun process-input (script)
  (show-prompt)
  (let ((input (read)))
    (cond ((member input '(nil q quit)) nil)
          ((not (listp input))
           (input-usage)
           (process-input script))
          (t (script-match input script)
             (process-input script)))) )
;          (t nil)))) ; Only get here if script has no default match.

(defun input-usage ()
  (format t "Give input as a list. (Type q to quit)~%"))

;;;
;;;    Find an appropriate response in the script based on user's input.
;;;    
(defun script-match (input script)
  (if (endp script)
      nil
      (let ((match (matchp (pattern (first script)) input)))
        (if match
            (fix-input (response (first script)))
;            (progn (fix-input (response (first script))) t)
            (script-match input (rest script)))) ))

(defun pattern (script-entry)
  (first script-entry))

(defun response (script-entry)
  (second script-entry))

(defun fix-input (response)
  (format t "~A~%" response))

(defvar *roomate-script* '(((*wild* laundry *wild*)
                            (when my clothes get too dirty i just burn them.))
                           ((i am *wild*)
                            (do you think i care about that))
                           ((do you *wild*)
                            (why should you care about me?))
                           ((*wild* year *wild*)
                            (if i'm lucky i'll graduate before the turn of the century.))
                           ((*wild* mother *wild*)
                            (don't make any cracks about my mother. she's a saint.))
                           ((my name *wild*)
                            (glad to meet you. my friends call me dr. death.))
                           ((no *wild*)
                            (well pardon me for living.))
                           ((*wild* sick)
                            (i think this room has lead paint. it makes you crazy.))
                           ((*wild*)
                            (really.))))

(defvar *star-wars-script* '(((*wild* try *wild*)
                              (no! try not. do. or do not. there is no try.))
                             ((*wild* dark *wild*)
                              (beware the dark side.))
                             ((*wild* plans)
                              (where are the rebel plans?))
                             ((*wild* little *wild*)
                              (size matters not.))
                             ((*wild* do you *wild*)
                              (hear you nothing that i say?))
                             ((*wild* ready *wild*)
                              (ready are you? what know you of ready?))
                             ((*wild* want *wild*)
                              (a jedi craves not these things.))
                             ((*wild* he *wild*)
                              (told you i did. reckless is he.))
                             ((*wild*)
                              (always in motion is the future.))))

(defun wildp (obj)
  (eq obj '*wild*))

(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (and (not (endp input)) (matchp pattern (rest input)))) )
        ((endp input) nil)
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

