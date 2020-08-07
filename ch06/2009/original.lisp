;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               original.lisp
;;;;
;;;;   Started:            Fri Mar 13 16:03:54 2009
;;;;   Modifications:
;;;;
;;;;   Purpose: Naive implementations of search string algorithm before
;;;;   consulting any references.
;;;;
;;;;   Notes: These all mostly make the mistake of searching all the way
;;;;   to the end of TEXT. If (length text) is N and (length pattern) is M
;;;;   We can stop searching at index (- n m)--PATTERN won't fit after
;;;;   that point!
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage original (:use common-lisp test))

(in-package original)

;;;
;;;    Determine whether the string PATTERN exists in the string TEXT
;;;
;;;    Initial version. Extra parameter POINT not actually necessary.
;;;    Rather than indexing TEXT with (+ point t-i) the 3rd version
;;;    has the top-level function STRING-SEARCH-AUX maintain the absolute
;;;    index into TEXT when it backtracks after MATCH-AT-POINT-P fails.
;;;
;;;    Also this version says that this fails: (string-search "" "")
;;;    vs. (search "" "") => 0
;;;    Simply reordering the top-level tests fixes that. (See 2nd version)
;;;
;;;@ recursion labels 
;; (defun string-search-1 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (let ((pattern-length (length pattern))
;;         (text-length (length text)))
;;     (labels ((string-search-aux (point t-i p-i)
;;                (if (= point text-length)
;;                    (= p-i pattern-length) ; Fixed: (string-search "" "") => T
;; ;                   nil ;Failure
;;                    (or (match-at-point-p point t-i p-i)
;;                        (string-search-aux (1+ point) 0 0))))
;;              (match-at-point-p (point t-i p-i)
;;                (cond ((= p-i pattern-length) point) ;Success
;;                      ((= (+ point t-i) text-length) nil) ;Failure
;;                      ((char= (char text (+ point t-i)) (char pattern p-i))
;;                       (match-at-point-p point (1+ t-i) (1+ p-i)))
;;                      (t nil))))
;;       (string-search-aux 0 0 0))))

;;;
;;;    D'oh! Don't need P-I, T-I above as args to STRING-SEARCH-AUX. They always
;;;    start as 0!
;;;    
(defun string-search-1 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((string-search-aux (point)
               (if (= point text-length)
                   (zerop pattern-length) ; Fixed: (string-search "" "") => T
;                   nil ;Failure
                   (or (match-at-point-p point 0 0)
                       (string-search-aux (1+ point))) ))
             (match-at-point-p (point t-i p-i)
               (cond ((= p-i pattern-length) point) ;Success
                     ((= (+ point t-i) text-length) nil) ;Failure
                     ((char= (char text (+ point t-i)) (char pattern p-i))
                      (match-at-point-p point (1+ t-i) (1+ p-i)))
                     (t nil))))
      (string-search-aux 0))))

;;;
;;;    (string-search "" "") => 0
;;;
;;;@ recursion labels
(defun string-search-2 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((string-search-aux (point t-i p-i)
               (cond ((match-at-point-p point t-i p-i))
                     ((= point text-length) nil)
                     (t (string-search-aux (1+ point) 0 0))))
             (match-at-point-p (point t-i p-i)
               (cond ((= p-i pattern-length) point) ;Success
                     ((= (+ point t-i) text-length) nil) ;Failure
                     ((char= (char text (+ point t-i)) (char pattern p-i))
                      (match-at-point-p point (1+ t-i) (1+ p-i)))
                     (t nil))))
      (string-search-aux 0 0 0))))

;;;
;;;    (string-search "" "") => NIL
;;;    Fixed: (string-search "" "") => 0
;;;
;;;@ recursion labels
(defun string-search-3 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((string-search-aux (t-i p-i)
               (cond ((= t-i text-length) (= p-i pattern-length))
;               (cond ((= t-i text-length) nil)
                     ((match-at-point-p t-i p-i) t-i)
                     (t (string-search-aux (1+ t-i) 0))) )
             (match-at-point-p (t-i p-i)
               (cond ((= p-i pattern-length) t)
                     ((= t-i text-length) nil)
                     ((char= (char text t-i) (char pattern p-i))
                      (match-at-point-p (1+ t-i) (1+ p-i)))
                     (t nil))))
      (string-search-aux 0 0))))

;;;
;;;    (string-search "" "") => 0
;;;
;;;@ recursion labels
;; (defun string-search-4 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (let ((pattern-length (length pattern))
;;         (text-length (length text)))
;;     (labels ((string-search-aux (t-i p-i)
;;                (cond ((match-at-point-p t-i p-i) t-i)
;;                      ((= t-i text-length) nil)
;;                      (t (string-search-aux (1+ t-i) 0))) )
;;              (match-at-point-p (t-i p-i)
;;                (cond ((= p-i pattern-length) t)
;;                      ((= t-i text-length) nil)
;;                      ((char= (char text t-i) (char pattern p-i))
;;                       (match-at-point-p (1+ t-i) (1+ p-i)))
;;                      (t nil))))
;;       (string-search-aux 0 0))))

;;;
;;;    Just as in STRING-SEARCH-1/2, we don't need P-I in 3/4!
;;;    
(defun string-search-4 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((string-search-aux (index)
               (cond ((match-at-point-p index 0) index)
                     ((= index text-length) nil)
                     (t (string-search-aux (1+ index)))) )
             (match-at-point-p (t-i p-i)
               (cond ((= p-i pattern-length) t)
                     ((= t-i text-length) nil)
                     ((char= (char text t-i) (char pattern p-i))
                      (match-at-point-p (1+ t-i) (1+ p-i)))
                     (t nil))))
      (string-search-aux 0))))

;;;
;;;     More straightforward by using DOTIMES to iterate across TEXT?
;;;     (string-search "" "") => NIL
;;;
;;;@ iteration labels
(defun string-search-5 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((match-at-point-p (t-i)
               (match-at-point-p-aux t-i 0))
             (match-at-point-p-aux (t-i p-i)
               (cond ((= p-i pattern-length) t) ;Success
                     ((= t-i text-length) nil) ;Failure
                     ((char= (char text t-i) (char pattern p-i))
                      (match-at-point-p-aux (1+ t-i) (1+ p-i)))
                     (t nil))))
      (dotimes (i text-length nil)
        (when (match-at-point-p i)
          (return i)))) ))

;;;
;;;    Some contortions w/ DOTIMES result value to get this to work:
;;;    (string-search "" "") => 0
;;;
;;;@ iteration labels
;; (defun string-search-6 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (let ((pattern-length (length pattern))
;;         (text-length (length text)))
;;     (labels ((match-at-point-p (t-i)
;;                (match-at-point-p-aux t-i 0))
;;              (match-at-point-p-aux (t-i p-i)
;;                (cond ((= p-i pattern-length) t)
;;                      ((= t-i text-length) nil)
;;                      ((char= (char text t-i) (char pattern p-i))
;;                       (match-at-point-p-aux (1+ t-i) (1+ p-i)))
;;                      (t nil))))
;;       (dotimes (i text-length (and (zerop i) (zerop pattern-length) i))
;;         (when (match-at-point-p i)
;;           (return i)))) ))

;;;
;;;    Stopping at the right place simplifies DOTIMES result value!!
;;;    
(defun string-search-6 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (labels ((match-at-point-p (t-i)
               (match-at-point-p-aux t-i 0))
             (match-at-point-p-aux (t-i p-i)
               (cond ((= p-i pattern-length) t)
                     ((= t-i text-length) nil)
                     ((char= (char text t-i) (char pattern p-i))
                      (match-at-point-p-aux (1+ t-i) (1+ p-i)))
                     (t nil))))
      (dotimes (i (1+ (- text-length pattern-length)))
        (when (match-at-point-p i)
          (return i)))) ))

;;;
;;;    These 2 don't work towards the end of TEXT when PATTERN
;;;    is longer than what's left!
;;;
;;;@ string search inefficient broken
;; (defun string-search-7 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (dotimes (i (length text) nil)
;;     (when (every #'char= pattern (subseq text i)) ; Bleah...
;;       (return i))))

;;;
;;;    Easily fixed!
;;;    See naive.lisp
;;;
;;;@ string search inefficient
(defun string-search-7 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (dotimes (i (1+ (- (length text) (length pattern))) nil)
    (when (every #'char= pattern (subseq text i)) ; Bleah...
      (return i))))

;;;@ list-processing mapping search broken
(defun string-search-8 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-list (coerce pattern 'list))
        (text-list (coerce text 'list)))
    (mapl #'(lambda (text-list)
              (when (every #'char= pattern-list text-list)
                (return-from string-search-8 text-list)))
          text-list)
    nil))

;;;
;;;    The next two treat the strings as lists.
;;;
;;;@ recursion string-as-list search
(defun string-search-9 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (labels ((matchp (char-list-1 char-list-2)
             (cond ((endp char-list-1) t)
                   ((endp char-list-2) nil)
                   ((char= (first char-list-1) (first char-list-2))
                    (matchp (rest char-list-1) (rest char-list-2)))
                   (t nil))))
    (let ((pattern-list (coerce pattern 'list))
          (text-list (coerce text 'list)))
      (mapl #'(lambda (text-list)
                (when (matchp pattern-list text-list)
                  (return-from string-search-9 text-list))) ; Yuck...
            text-list)
      (and (null pattern-list) (null text-list)))) )

;;;@ recursion string-as-list search
(defun string-search-10 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-list (coerce pattern 'list))
        (text-list (coerce text 'list)))
    (labels ((string-search-aux (text-list)
               (cond ((null text-list) (null pattern-list))
                     ((matchp pattern-list text-list))
                     (t (string-search-aux (rest text-list)))) )
             (matchp (char-list-1 char-list-2)
;(print (list char-list-1 char-list-2))
               (cond ((endp char-list-1) t)
                     ((endp char-list-2) nil)
                     ((char= (first char-list-1) (first char-list-2))
                      (matchp (rest char-list-1) (rest char-list-2)))
                     (t nil))))
      (string-search-aux text-list))))

;;;@ imperative ugly
;; (defun string-search-11 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (let ((pattern-length (length pattern))
;;         (text-length (length text)))
;;     (cond ((zerop pattern-length) 0)
;;           ((zerop text-length) nil)
;;           (t (do ((text-index 0 (1+ text-index)))
;;                  ((= text-index text-length) nil)
;;                (when (char= (char text text-index) (char pattern 0))
;;                  (do ((pattern-index 1 (1+ pattern-index))
;;                       (t-i1 (1+ text-index) (1+ t-i1)))
;;                      (nil)
;;                    (cond ((= pattern-index pattern-length) (return-from string-search-11 text-index))
;;                          ((or (= t-i1 text-length)
;;                               (char/= (char text t-i1) (char pattern pattern-index)))
;;                           (return nil)))) )))) ))

;; (defun string-search-11 (pattern text)
;;   (assert (and (stringp pattern) (stringp text))
;;           (pattern text)
;;           "Arguments should both be strings.")
;;   (let ((pattern-length (length pattern))
;;         (text-length (length text))
;;         (first-char (char pattern 0)))
;;     (if (zerop pattern-length)
;;         0
;;         (do ((text-index 0 (1+ text-index)))
;;             ((= text-index text-length) nil)
;;           (when (char= (char text text-index) first-char)
;;             (do ((pattern-index 1 (1+ pattern-index))
;;                  (t-i1 (1+ text-index) (1+ t-i1)))
;;                 (nil)
;;               (cond ((= pattern-index pattern-length) (return-from string-search-11 text-index))
;;                     ((or (= t-i1 text-length)
;;                          (char/= (char text t-i1) (char pattern pattern-index)))
;;                      (return nil)))) )))) )

;;;
;;;    Renamed index variables after looking at java.lang.String#indexOf()
;;;    
(defun string-search-11 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (if (zerop pattern-length)
        0
        (do ((first-char (char pattern 0))
             (i 0 (1+ i)))
            ((= i text-length) nil)
          (when (char= (char text i) first-char)
            (do ((j 1 (1+ j))
                 (k (1+ i) (1+ k)))
                (nil)
              (cond ((= j pattern-length) (return-from string-search-11 i))
                    ((or (= k text-length)
                         (char/= (char text k) (char pattern j)))
                     (return nil)))) )))) )

;;;@ loop
(defun string-search-12 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (let ((pattern-length (length pattern))
        (text-length (length text)))
    (if (zerop pattern-length)
        0
        (loop for i from 0 below text-length
              when (char= (char text i) (char pattern 0))
              do (loop for j from 1 below pattern-length
                       for k from (1+ i) below text-length
                       always (char= (char text k) (char pattern j))
                       finally (return-from string-search-12 (and (= j pattern-length) i)))
              finally (return nil)))) )

;;;@ loop
(defun string-search-13 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (if (zerop (length pattern))
      0
      (loop for i from 0 below (length text)
            when (match-at-p pattern text i)
            return i
            finally (return nil))))

(defun match-at-p (pattern text i)
  (loop for j from 0 below (length pattern)
        for k from i below (length text)
        always (char= (char text k) (char pattern j))
        finally (return (and (= j (length pattern)) i))))

;;;
;;;    This is shorter than 13 but kind of convoluted.
;;;    When evaluating (string-search-14 "" "") this allows
;;;    one call to MATCH-AT-P, which then immediately returns 0
;;;    since PATTERN is empty. If the loop used BELOW as in
;;;    13, then this loop would itself immediately exit with
;;;    the default value of NIL.
;;;    
;;;@ loop
(defun string-search-14 (pattern text)
  (assert (and (stringp pattern) (stringp text))
          (pattern text)
          "Arguments should both be strings.")
  (loop for i from 0 upto (length text)
        when (match-at-p pattern text i)
        return i))



;;;;;;;;  What the hell?!?!?!?!
;;;
;;; ?!??!
;;; 
(defmacro defsplice (test-name-form function-name-form)
   (let ((test-name test-name-form)
         (function-name function-name-form))
;;    (let ((test-name (eval test-name-form))
;;          (function-name (eval function-name-form)))
    `(deftest ,test-name ()
      (check
       (eq (type-of (handler-case (,function-name 10 "asdf")
		  (simple-error (e) (print e))))
       'simple-error)
       (eq (type-of (handler-case (,function-name "asdf" 10)
		  (simple-error (e) (print e))))
       'simple-error)
       (,function-name "" "")
       (not (,function-name "foo" "ffo"))
       (,function-name "foo" "ffoo")
       (,function-name "foo" "foo")
       (not (,function-name "pickler" "peter piper picked a peck of pretty pickled peppers"))
       (,function-name "pickle" "peter piper picked a peck of pretty pickled peppers")
       (,function-name "" "a")
       (not (,function-name "a" ""))))))

(defparameter *test* nil)
(defparameter *fn* nil)

  (loop for i from 1 to 14
        do (setf *test* (intern (concatenate 'string "TEST-STRING-SEARCH-" (format nil "~D" i)))
                 *fn* (intern (concatenate 'string "STRING-SEARCH-" (format nil "~D" i))))
;(print *test*)
;(print *fn*)
(print (eval `(defsplice ,*test* ,*fn*))))
            ; (defsplice *test* *fn*)))

