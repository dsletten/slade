;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               eliza.lisp
;;;;
;;;;   Started:            Tue Sep 27 23:51:36 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :eliza (:use :common-lisp :test))

(in-package :eliza)

;;;
;;;    4.7.12
;;;
(defun wildp (obj)
  (eq obj '*wild*))

(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (if (wildp (first pattern)) (matchp (rest pattern) input) nil))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (matchp pattern (rest input))))
        (t nil)))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (matchp '(a *wild*) '(a b c))
   (matchp '(a *wild*) '(a))
   (matchp '(a *wild* b) '(a b c d b))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (matchp '(*wild*) '(a b c))))

(defun eliza ()
  (format t "Hello.~2%")
  (loop
     (format t "--> ")
     (let ((reply (read-reply)))
       (if (quitp reply)
           (return)
           (process-reply reply))))
  (format t "Goodbye from Eliza.~2%"))

(defun sanitize (s)
  (remove #\, s))

(defun read-reply ()
  (handler-case 
      (let* ((*read-eval* nil)
             (input (string-trim " " (sanitize (read-line))))
             (stream (make-string-input-stream input)))
        (loop for token = (read stream nil)
              until (null token)
              when token collect it))
    (reader-error (e) (declare (ignore e))
                  (warn "Naughty!")
                  (read-reply))))

(defun read-all-from-string (s)
  (labels ((read-all-from-string-aux (i result)
             (if (= i (length s))
                 (nreverse result)
                 (multiple-value-bind (obj j) (read-from-string s nil nil :start i)
                   (read-all-from-string-aux j (cons obj result)))) ))
    (read-all-from-string-aux 0 '())))

(defun quitp (s)
  (or (equal s '(quit))
      (equal s '(q))))

(defun process-reply (reply)
  (write-line (script-match reply *master-script*))
  (terpri))

(defun script-match (input script)
  (if (endp script)
      nil
      (destructuring-bind ((pattern response) . rest) script
        (let ((match (matchp pattern input)))
          (if match
              response
              (script-match input rest)))) ))

(setf *master-script*
  '(((*wild* laundry *wild*)
     "When my clothes get too dirty I just burn them.")
    ((i am *wild*)
     "Do you think I care about that?")
    ((do you *wild*)
     "Why should you care about me?")
    ((*wild* year *wild*)
     "If I'm lucky I'll graduate before the turn of the century.")
    ((*wild* mother *wild*)
     "Don't make any cracks about my mother. She's a saint.")
    ((my name *wild*)
     "Glad to meet you. My friends call me Dr. Death.")
    ((no *wild*)
     "Well pardon me for living.")
    ((*wild* sick)
     "I think this room has lead paint. It makes you crazy.")
    ((*wild*)
     "Really.")))


