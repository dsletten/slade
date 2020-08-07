;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               eliza2.lisp
;;;;
;;;;   Started:            Wed Sep  8 16:06:07 2010
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
;;;;   Notes: Exercises 7.9.14
;;;;
;;;;

(load "/Users/dsletten/lisp/packages/test")

(defpackage eliza2 (:use common-lisp test))

(in-package eliza2)

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

;;;
;;;    New version of MATCHP returns true for match and false for no match.
;;;    Specifically, for match with wildcard, returns subsequence of input that
;;;    matched the wildcard.
;;;
;;;    Assumes single wildcard per pattern at most.
;;;
;;;    Wildcard match with empty subsequence simply returns T:
;;;    (matchp '(a *wild*) '(a)) => T
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern))
         (or (matchp (rest pattern) input)
             (and (not (endp input))
                  (capture (first input) (matchp pattern (rest input)))) ))
        ((endp input) nil)
        ((equal (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (and (wildp (first pattern))
                           (matchp (rest pattern) input)))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (capture (first input) (matchp pattern (rest input)))) )
        ((equal (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun capture (elt result)
  (cond ((null result) nil)
        ((eq result t) (list elt))
        (t (cons elt result))))

;;;
;;;    New new version of MATCHP handles multiple wildcards. Returns list of lists on
;;;    success.
;;;

;;;
;;;    List returned on match, symbol FAIL otherwise.
;;;    Thus empty list is successful result.
;;;    List of lists contains matches for wild cards.
;;;
;;;    (matchp '(* b c * d) '(a b b c c d c d)) => ((a b) (c d c))
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (matchp (rest pattern) input))
               (t (let ((result (matchp (rest pattern) input)))
                    (if (eq result 'fail)
                        (let ((result (matchp pattern (rest input))))
                          (if (eq result 'fail)
                              result
                              (cons (cons (first input) (first result)) (rest result))))
                        (cons '() result)))) ))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t 'fail)))

(deftest test-matchp ()
  (check
   (equal (matchp '(a b c) '(a b c)) '())
   (equal (matchp '(a b c) '(a b c d)) 'fail)
   (equal (matchp '(a b c d) '(a b c)) 'fail)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '())    ;????
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) 'fail)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '(() (c d c d)))
   (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '(() (A A A A A)))
   (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '(() () (A A A A A B C D)))) )

;; (deftest test-matchp ()
;;   (check
;;    (matchp '(a b c) '(a b c))
;;    (not (matchp '(a b c) '(a b c d)))
;;    (not (matchp '(a b c d) '(a b c)))
;;    (matchp '(a *wild*) '(a b c))
;;    (matchp '(a *wild*) '(a))
;;    (matchp '(a *wild* b) '(a b c d b))
;;    (not (matchp '(a *wild* b) '(a b c d e)))
;;    (matchp '(*wild* b *wild*) '(a b c d e))
;;    (matchp '(*wild*) '(a b c))))

(defconstant pronoun-map '((i . you) (my . your) (me . you) (you . i) (your . my)))
(defun pronoun-shift (input)
  (sublis pronoun-map input))

(defconstant verb-map '((am . are) (are . am)))
(defun verb-shift (input)
  (sublis verb-map input))
