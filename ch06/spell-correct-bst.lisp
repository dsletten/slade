;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               spell-correct-bst.lisp
;;;;
;;;;   Started:            Wed Sep 22 16:20:24 2004
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
;;;;   Notes: This update to spell-correct-2.lisp uses binary search tree to
;;;;          hold words as strings not as symbols.
;;;;
;;;;
(load "/Users/dsletten/lisp/programs/utils.lisp")

(defpackage spell-correct-bst (:use common-lisp))

(in-package spell-correct-bst)

(defun spell-correct (word)
  (let ((word-length (length word)))
    (cond ((string= word "") nil)
          ((spell-check word))
          (t (let ((matches (char-delete-insert-p word
                                                  word-length
                                                  (char-insert-p word
                                                                 word-length
                                                                 (char-double-p word
                                                                                word-length
                                                                                (char-transpose-p word
                                                                                                  (1- word-length)
                                                                                                  (char-delete-p word
                                                                                                                 word-length
                                                                                                                 '()))) ))))
;          (format t "~S~%" matches)
               (sort (remove-duplicates matches :test #'string-equal)
                     #'(lambda (s1 s2)
                         (string-lessp (string s1)
                                       (string s2)))) )))) )

(defun correct (word)
  (let ((word-list (spell-correct word)))
    (cond ((null word-list)
           (format t "~&The word '~A' does not appear in the ~
                      dictionary and no alternatives were found.~%" word))
          ((atom word-list)
           (format t "~&The word '~A' is in the dictionary.~%"
                   word-list))
          (t
           (format t "~&The word '~A' is not in the dictionary. The following ~
                      possible alternatives were found:~%" word)
           (do ((i 1 (1+ i))
                (words word-list (cdr words)))
               ((null words) nil)
             (format t "~5T~2D. ~A~%" i (car words)))
           (let ((choice (1- (get-num "Please choose an alternative: "
                                      #'(lambda (x)
                                          (<= 1 x (length word-list)))) )))
             (format t "You chose '~A' rather than '~A'.~%"
                     (nth choice word-list) word)))) ) )

(defun spell-check (word)
  (search-tree word *dictionary*))

;;;
;;;    This works from end back towards start of string.
;;;    
(defun char-delete-p (word-string index results)
  (cond ((zerop index) results)
        (t (let* ((new-word (concatenate 'string
                                         (subseq word-string 0 (1- index))
                                         (subseq word-string index)))
                  (check-word (spell-check new-word)))
             (if check-word
                 (char-delete-p word-string
                                (1- index)
                                (cons check-word results))
                 (char-delete-p word-string (1- index) results)))) ) )

;;;
;;;    Return copy of string with the characters at INDEX-1 and INDEX-2
;;;    swapped. (Other variants of this function in spell-correct-2.lisp)
;;;
;;;    Example: (char-swap "hello there" 3 5) => "hel olthere"
;;;
(defun char-swap (word-string index-1 index-2)
  (let ((string-copy (copy-seq word-string)))
    (rotatef (elt string-copy index-1) (elt string-copy index-2))
    string-copy) )

;;;
;;;    Test word with successive pairs transposed. Work from end towards
;;;    start of string.
;;;    
(defun char-transpose-p (word-string index results)
  (cond ((zerop index) results)
        (t (let ((check-word (spell-check (char-swap word-string
                                                     index (1- index)))) )
             (if check-word
                 (char-transpose-p word-string
                                   (1- index)
                                   (cons check-word results))
                 (char-transpose-p word-string (1- index) results)))) ) )

;;;
;;;    Return copy of string S with character CH inserted at 0-based index I.
;;;    
(defun string-insert (s ch i)
  "Insert a character into a given string following the character at the specified position."
  (cond ((or (minusp i)
             (> i (length s)))
         'string-insert-index-error)
        (t (concatenate 'string
                        (subseq s 0 i)
                        (string ch)
                        (subseq s i)))) )

(defun char-double-p (word-string index results)
  (cond ((zerop index) results)
        (t (let ((check-word
                  (spell-check (string-insert word-string
                                              (elt word-string (1- index))
                                              index))))
             (if check-word 
                 (char-double-p word-string
                                (1- index)
                                (cons check-word results))
                 (char-double-p word-string (1- index) results)))) ) )

(defun char-insert-p (word-string index results)
  (cond ((minusp index) results)
        (t (char-insert-p word-string (1- index)
                          (char-insert-check word-string index #\A
                                             results)))) )

;;  (t (let ((insert-results (char-insert-check word-string index #\A)))
;;       (if insert-results
;;       (char-insert-p word-string (1- index) (cons insert-results results))
;;       (char-insert-p word-string (1- index) results)))) ) )

(defun char-insert-check (word-string index new-char results)
  (cond ((char> new-char #\Z) results)
        (t (let ((check-word (spell-check (string-insert word-string
                                                         new-char
                                                         index))))
             (if check-word
                 (char-insert-check word-string
                                    index
                                    (inc-char new-char)
                                    (cons check-word results))
                 (char-insert-check word-string
                                    index
                                    (inc-char new-char)
                                    results)))) ) )

(defun inc-char (ch)
  "Return the character that follows the given input character."
  (code-char (1+ (char-code ch))) )


(defun char-delete-insert-p (word-string index results)
  (cond ((zerop index) results)
        (t (let ((cut-word (concatenate 'string
                                        (subseq word-string 0 (1- index))
                                        (subseq word-string index))))
             (char-delete-insert-p word-string
                                   (1- index)
                                   (char-insert-p cut-word
                                                  (length cut-word)
                                                  results)))) ))

;;;
;;;    BST functions.
;;;    
(defun make-tree (value left right)
  (list value left right))

(defun value (tree)
  (first tree))

(defun left-subtree (tree)
  (second tree))

(defun right-subtree (tree)
  (third tree))

(defun grow-tree (obj tree)
  (cond ((null tree) (make-tree tree '() '()))
        ((string-equal obj (value tree)) tree)
        ((string-lessp obj (value tree))
         (make-tree (value tree)
                    (grow-tree obj (left-subtree tree))
                    (right-subtree tree)))
        (t (make-tree (value tree)
                      (left-subtree tree)
                      (grow-tree obj (right-subtree tree)))) ))

(defun search-tree (obj tree)
  (let ((val (value tree)))
    (cond ((null tree) nil)
          ((string-equal obj val) val)
          ((string-lessp obj val)
           (search-tree obj (left-subtree tree)))
          (t (search-tree obj (right-subtree tree)))) ))

(defun list->tree (list)
  (car (partial-tree list (length list))))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (truncate (- n 1) 2))
             (left-result (partial-tree elts left-size)) ;<--
             (left-tree (car left-result))
         
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))
         
             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree (cdr non-left-elts) right-size)) ;<--
             (right-tree (car right-result))
         
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;;;
;;;    Set-up code.
;;;    
(defvar *dictionary* '() "BST of recognized words (strings).")

(defun read-dictionary ()
  (with-open-file (in-stream "old/words")
    (do ((in-line (read-line in-stream nil nil)
                  (read-line in-stream nil nil))
         (dic '()))
        ((not in-line) (nreverse dic))
      (push in-line dic))))

(unless *dictionary*
  (setf *dictionary* (list->tree (read-dictionary))))

(defun fix-spelling ()
  (format t "Enter a word to test: ")
  (correct (read-line)))
;;   (let ((s (make-string-input-stream (read-line))))
;;     (do ()
;;  ((not (listen s)))
;;       (correct (read s)))) )
