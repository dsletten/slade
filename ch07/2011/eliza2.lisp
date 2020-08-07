;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               eliza2.lisp
;;;;
;;;;   Started:            Sun Oct  2 02:01:29 2011
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

(defpackage :eliza2 (:use :common-lisp :test))

(in-package :eliza2)

;;;
;;;    4.7.12
;;;
(defun wildp (obj)
  (eq obj '*wild*))

;;;
;;;    Single wild card capture.
;;;    (More precisely, all matches for any wildcards captured in single result list.)
;;;    
;;;  #1
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input) (values t result) (values nil nil)))
;             (cond ((endp pattern) (values (endp input) result))
                   ((endp input) (if (wildp (first pattern))
                                     (matchp-aux (rest pattern) input result)
                                     (values nil nil)))
                   ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
                   ((wildp (first pattern))
                    (multiple-value-bind (match result) (matchp-aux (rest pattern) input result)
                      (if match
                          (values match result)
                          (multiple-value-bind (match result) (matchp-aux pattern (rest input) result)
                            (if match
                                (values match (cons (first input) result))
                                (values nil nil)))) ))
                   (t (values nil nil)))) )
    (matchp-aux pattern input '())))

;;;  #2
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   ((wildp (first pattern))
                    (cond ((endp input) (cons '() result))
                          (t (or (matchp-aux (rest pattern) input (cons '() result))
                                 (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) )))
                   (t nil))))
    (matchp-aux pattern input '())))

;;;  #3
(defun matchp (pattern input)
  (let ((matches '()))
    (labels ((matchp-aux (pattern input result)
               (cond ((endp pattern) (if (endp input) (or result t) nil))
                     ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                     ((wildp (first pattern))
                      (cond ((endp input) (cons '() result))
                            (t (let ((match (matchp-aux (rest pattern) input (cons '() result))))
                                 (when match (push match matches)))
                               (let ((match (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) )
                                 (when match (push match matches))))))
                   (t nil))))
    (matchp-aux pattern input '()))
    matches))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (equal (nth-value 1 (matchp '(a *wild*) '(a b c))) '(b c))
   (equal (nth-value 1 (matchp '(a *wild*) '(a))) '())
   (equal (nth-value 1 (matchp '(a *wild* b) '(a b c d b))) '(b c d))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (equal (nth-value 1 (matchp '(*wild*) '(a b c))) '(a b c))))

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



;;;  #4
(defun matchp (pattern input result)
  (cond ((endp pattern) (endp input))
        ((endp input) (if (wildp (first pattern)) (matchp (rest pattern) input result) nil))
        ((wildp (first pattern)) (remove nil (list (capture (first input) pattern (rest input) result)
                                                   (non-capture (rest pattern) input result))))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input) result))
        (t nil)))

(defun capture (elt pattern input result)
  (matchp pattern input (cons (cons elt (first result)) (rest result))))

(defun non-capture (pattern input result)
  (matchp pattern input (cons '() result)))

;;;  #5
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
;(print (list pattern input result))
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (matchp-aux (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (capture (first input) pattern (rest input) result)
                                                (non-capture (rest pattern) input result)))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t nil)))
           (capture (elt pattern input result)
             (matchp-aux pattern input (cons (cons elt (first result)) (rest result))))
           (non-capture (pattern input result)
             (matchp-aux pattern input (cons '() result))))
    (mapcar #'reverse (reverse (matchp-aux pattern input '())))))

;;;  #6
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
(print (list pattern input result))
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (matchp-aux (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))
                                                (matchp-aux (rest pattern) input (cons '() result))))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t nil))))
    (mapcar #'reverse (reverse (matchp-aux pattern input '(()))))))

;;;  #7
(defun matchp (pattern input)
  (labels ((ignore (pattern input result)
;             (print (list 'i pattern input result))
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (ignore (rest pattern) input (add-empty-group result)) nil))
                   ((wildp (first pattern)) (or (capture pattern (rest input) (add-elt-to-new-group (first input) result))
                                                (ignore (rest pattern) input (add-empty-group result))))
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil)))
           (capture (pattern input result)
;             (print (list 'c pattern input result))
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (ignore (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (capture pattern (rest input) (add-elt-to-current-group (first input) result))
                                                (ignore (rest pattern) input result)))
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (ignore pattern input '())))
      (if (symbolp result)
          result
          (mapcar #'reverse (reverse result)))) ))

(defun add-elt-to-current-group (elt result)
  (destructuring-bind (current-group . rest) result
    (cons (cons elt current-group) rest)))

(defun add-elt-to-new-group (elt result)
  (cons (list elt) result))

(defun add-empty-group (result)
  (cons '() result))

;;;  #8
(defun matchp (pattern input)
  (labels ((process (pattern input result &key (state :ignore))
;             (print (list 'i pattern input result))
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((endp input) (if (wildp (first pattern))
                                     (process (rest pattern) input (ecase state
                                                                     (:ignore (add-empty-group result))
                                                                     (:capture result)))
                                     nil))
                   ((wildp (first pattern)) (or (process pattern
                                                         (rest input)
                                                         (ecase state
                                                           (:ignore (add-elt-to-new-group (first input) result))
                                                           (:capture (add-elt-to-current-group (first input) result)))
                                                         :state :capture)
                                                (process (rest pattern)
                                                         input
                                                         (ecase state
                                                           (:ignore (add-empty-group result))
                                                           (:capture result))
                                                         :state :ignore)))
                   ((eql (first pattern) (first input)) (process (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (process pattern input '())))
      (if (symbolp result)
          result
          (mapcar #'reverse (reverse result)))) ))

(deftest test-matchp ()
  (check
   (equal (matchp '(a b c) '(a b c)) t)
   (equal (matchp '(a b c) '(a b c d)) nil)
   (equal (matchp '(a b c d) '(a b c)) nil)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '(()))
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) nil)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '((c d c d) ()))
   (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '((A A A A A) NIL))
   (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '((A A A A A) NIL (B C D)))) )

;;;
;;;    Modified from 2010
;;;    
;;;  #9
(defun match-greedy (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (let ((result (match-greedy (rest pattern) input)))
                               (if (eq result 'fail)
                                   result
                                   (cons '() result))))
               (t (let ((result (match-greedy pattern (rest input))))
                    (if (eq result 'fail)
                        (let ((result (match-greedy (rest pattern) input)))
                          (if (eq result 'fail)
                              result
                              (cons '() result)))
                        (cons (cons (first input) (first result)) (rest result)))) )))
        ((eql (first pattern) (first input)) (match-greedy (rest pattern) (rest input)))
        (t 'fail)))

;;;  #9a
(defun match-greedy (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (add-empty-group-or-fail (match-greedy (rest pattern) input)))
               (t (let ((result (match-greedy pattern (rest input))))
                    (if (eq result 'fail)
                        (add-empty-group-or-fail (match-greedy (rest pattern) input))
                        (add-elt-to-current-group (first input) result)))) ))
        ((eql (first pattern) (first input)) (match-greedy (rest pattern) (rest input)))
        (t 'fail)))

(defun add-empty-group-or-fail (result)
  (if (eq result 'fail)
      result
      (add-empty-group result)))

(deftest test-match-greedy ()
  (check
   (equal (match-greedy '(a b c) '(a b c)) '())
   (equal (match-greedy '(a b c) '(a b c d)) 'fail)
   (equal (match-greedy '(a b c d) '(a b c)) 'fail)
   (equal (match-greedy '(a *wild*) '(a b c)) '((b c)))
   (equal (match-greedy '(a *wild*) '(a)) '(()))
   (equal (match-greedy '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (match-greedy '(a *wild* b) '(a b c d e)) 'fail)
   (equal (match-greedy '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (match-greedy '(*wild*) '(a b c)) '((a b c)))
   (equal (match-greedy '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (match-greedy '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (match-greedy '(a b *wild* c d *wild*) '(a b c d c d c d)) '((c d c d) ()))
   (equal (match-greedy '(*wild* a *wild* a) '(a a a a a a a)) '((A A A A A) ()))
   (equal (match-greedy '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '((A A A A A) () (B C D)))) )
