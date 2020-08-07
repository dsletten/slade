;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               spell-correct.lisp
;;;;
;;;;   Started:            Sat Jul 21 21:17:43 2007
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
(load "/Users/dsletten/lisp/packages/test")
(load "/Users/dsletten/lisp/packages/utils")

(defpackage spell-correct (:use common-lisp test utils))

(in-package spell-correct)

(defvar *dictionary* nil "The dictionary of properly-spelled words to which words are compared.")

(when (null *dictionary*)
  (setf *dictionary* (coerce (read-file "/Users/dsletten/lisp/books/Slade/ch06/wordlists/words") 'vector)))

(defvar *suggestions* (make-hash-table :test #'equalp))

(defun spell-match (word)
  (let* ((word (if (symbolp word) (string word) word))
         (word-length (length word)))
    (cond  ((string= word "") nil)
           ((word-check word))
           (t (let ((suggestions (gethash word *suggestions*)))
                (if suggestions
                    (progn (format t "Memoized!~%") suggestions)
                    (progn (setf (gethash word *suggestions*) '())
                           (let ((suggestion (cond ((word-deletion word word-length))
                                                   ((word-transposition word (1- word-length)))
                                                   ((word-double word (1- word-length)))
                                                   ((word-insertion word word-length))
                                                   (t nil))))
                             (when suggestion
                               (push suggestion (gethash word *suggestions*))))
                           (gethash word *suggestions*)))) ))))

(defun word-check (word)
  (binary-search *dictionary* word))

(defun binary-search (v s)
  (labels ((binary-search-aux (min max)
             (if (> min max)
                 nil
                 (let ((mid (truncate (+ min max) 2)))
                   (cond ((string-equal (svref v mid) s) s)
                         ((string-lessp s (svref v mid))
                          (binary-search-aux min (1- mid)))
                         (t (binary-search-aux (1+ mid) max)))) )))
    (binary-search-aux 0 (1- (length v)))) )
;;    (binary-search-aux 0 (length v))))  ; Should be (1- (length v)) ?? Yes!

(defun word-deletion (word-string index)
  (cond ((zerop index) nil)
        ((let ((new-word (concatenate 'string
                                      (subseq word-string 0 (1- index))
                                      (subseq word-string index))))
           (word-check new-word)))
        (t (word-deletion word-string (1- index)))))

(defun word-transposition (word-string index)
  (cond ((zerop index) nil)
        ((word-check (string-swap word-string index (1- index))))
        (t (word-transposition word-string (1- index)))) )

(defun string-swap (s i j)
  (let ((s1 (copy-seq s)))
    (rotatef (char s1 i) (char s1 j))
    s1))

(defun string-insert (s ch index)
  (labels ((insert-aux (result i j)
             (cond ((= i (length result)) result)
                   ((= i index)
                    (setf (char result i) ch)
                    (insert-aux result (1+ i) j))
                   (t (setf (char result i) (char s j))
                      (insert-aux result (1+ i) (1+ j)))) ))
    (let ((result (make-string (1+ (length s)))) )
      (insert-aux result 0 0))))
    
;; (let ((s "Is this not pung?")) (loop for i from 0 upto (length s) do (format t "~A~%" (string-insert s #\x i))))
;; xIs this not pung?
;; Ixs this not pung?
;; Isx this not pung?
;; Is xthis not pung?
;; Is txhis not pung?
;; Is thxis not pung?
;; Is thixs not pung?
;; Is thisx not pung?
;; Is this xnot pung?
;; Is this nxot pung?
;; Is this noxt pung?
;; Is this notx pung?
;; Is this not xpung?
;; Is this not pxung?
;; Is this not puxng?
;; Is this not punxg?
;; Is this not pungx?
;; Is this not pung?x

(defun word-double (word-string index)
  (cond ((minusp index) nil)
        ((word-check (string-insert word-string (char word-string index) index)))
        (t (word-double word-string (1- index)))) )

(defun word-insertion (word-string index)
  (cond ((minusp index) nil)
        ((word-insertion-check word-string index #\A))
        (t (word-insertion word-string (1- index)))) )

(defun word-insertion-check (word-string index new-char)
  (cond ((char> new-char #\Z) nil)
        ((word-check (string-insert word-string new-char index)))
        (t (word-insertion-check word-string index (next-char new-char)))) )

(defun next-char (ch)
  (code-char (1+ (char-code ch))))

                    
                      

;;;
;;;     Below is Slade's original code (basically)
;;;
;;;
;; (defun tag-word (word)
;;   (setf (get word 'isa-word) t)
;;   word)

;; (mapcar #'tag-word '(commuter computer computation computing compute computers))

;; (defun spell-match (word)
;;   (let* ((word (if (symbolp word) (string word) word))
;;   (word-length (length word)))
;;     (cond ((string= word "") nil)
;;    ((word-check word))
;;    ((word-deletion word word-length))
;;    ((word-transposition word (1- word-length)))
;;    ((word-double word (1- word-length)))
;;    ((word-insertion word word-length))
;;    (t nil))))

;; (defun word-check (word)
;;   (let ((symbol (read-from-string word)))
;;     (if (get symbol 'isa-word)
;;  symbol
;;  nil)))

;; (defun word-deletion (word-string index)
;;   (cond ((zerop index) nil)
;;  ((let ((new-word (concatenate 'string
;;                    (subseq word-string 0 (1- index))
;;                    (subseq word-string index))))
;;     (word-check new-word)))
;;  (t (word-deletion word-string (1- index)))))

;; (defun word-transposition (word-string index)
;;   (cond ((zerop index) nil)
;;  ((word-check (string-swap word-string index (1- index))))
;;  (t (word-transposition word-string (1- index)))) )

;; (defun string-swap (s i j)
;;   (let ((s1 (copy-seq s)))
;;     (rotatef (char s1 i) (char s1 j))
;;     s1))

;; (defun string-insert (s ch index)
;;   (labels ((insert-aux (result i j)
;;       (cond ((= i (length result)) result)
;;         ((= i index)
;;          (setf (char result i) ch)
;;          (insert-aux result (1+ i) j))
;;         (t (setf (char result i) (char s j))
;;            (insert-aux result (1+ i) (1+ j)))) ))
;;     (let ((result (make-string (1+ (length s)))) )
;;       (insert-aux result 0 0))))
    
;; (defun word-double (word-string index)
;;   (cond ((minusp index) nil)
;;  ((word-check (string-insert word-string (char word-string index) index)))
;;  (t (word-double word-string (1- index)))) )

;; (defun word-insertion (word-string index)
;;   (cond ((minusp index) nil)
;;  ((word-insertion-check word-string index #\A))
;;  (t (word-insertion word-string (1- index)))) )

;; (defun word-insertion-check (word-string index new-char)
;;   (cond ((char> new-char #\Z) nil)
;;  ((word-check (string-insert word-string new-char index)))
;;  (t (word-insertion-check word-string index (next-char new-char)))) )

;; (defun next-char (ch)
;;   (code-char (1+ (char-code ch))))

                    
                      

       