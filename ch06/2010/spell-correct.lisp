;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               spell-correct.lisp
;;;;
;;;;   Started:            Wed Jul 14 01:07:56 2010
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
;;;;       Try using LOOP
;;;;       Destructively modify copies of input string
;;;;
(load "/Users/dsletten/lisp/packages/io")
;(load "/Users/dsletten/lisp/packages/lang")
(load "/Users/dsletten/lisp/packages/test")

(defpackage spell-correct (:use common-lisp io test) (:shadow append))

(in-package spell-correct)

(defvar *dictionary* (make-hash-table :test #'equal))
(defvar *dictionary-vector*)

(defun initialize ()
  (let ((words (read-file "../wordlists/words.big")))
    (dolist (word words)
      (setf (gethash word *dictionary*) t))
    (setf *dictionary-vector* (sort (coerce words 'vector) #'string-equal)) ; The file already seems to be sorted as required.
    '*yo*))

(defun binary-search (s v)
  (binary-search-aux s v 0 (1- (length v))))

(defun binary-search-aux (target vector low high)
  (if (> low high)
      (values nil nil)
      (let* ((mid (truncate (+ low high) 2))
             (mid-val (aref vector mid)))
        (cond ((string-equal target mid-val) (values mid-val mid))
              ((string-lessp target mid-val) (binary-search-aux target vector low (1- mid)))
              (t (binary-search-aux target vector (1+ mid) high)))) ))

(defun valid-word-p (word)
  (gethash word *dictionary*))

(defun valid-word-p (word)
  (binary-search word *dictionary-vector*))

;;;
;;;    DOUBLE-CHAR is pointless since INSERT-CHAR will generate the same possibilities.
;;;    
(defun spell-correct (word)
  (cond ((string= word "") nil)
        ((valid-word-p word) word)
        (t (sort (remove-duplicates (mapcan #'(lambda (f)
                                                (funcall f word))
                                            (list #'delete-char #'double-char #'transpose-char #'insert-char #'replace-char))
                                    :test #'string=)
                 #'string<))))


;;;
;;;    I. Coerce string to list, do regular recursive list processing.
;;;    
;; (defun delete-char (s)
;;   (delete-char-aux s 0 '()))

;; (defun delete-char-aux (s i results)
;;   (if (= i (length s))
;;       (remove-duplicates results :test #'string=)
;;       (let ((new-word (delete-char-from-word s i)))
;;         (if (valid-word-p new-word)
;;             (delete-char-aux s (1+ i) (cons new-word results))
;;             (delete-char-aux s (1+ i) results)))) )

;; (defun delete-char-from-word (s i)
;;   (let ((new-word (make-string (1- (length s)))))
;;     (delete-char-from-word-before s new-word i 0)))

;; (defun delete-char-from-word-before (old new i j)
;;   (cond ((= j (length new)) new)
;;         ((= i j) (delete-char-from-word-after old new i (1+ j)))
;;         (t (setf (char new j) (char old j))
;;            (delete-char-from-word-before old new i (1+ j)))) )

;; (defun delete-char-from-word-after (old new i j)
;;   (cond ((> j (length new)) new)
;;         (t (setf (char new (1- j)) (char old j))
;;            (delete-char-from-word-after old new i (1+ j)))) )

(defun delete-char (s)
  (remove-if-not #'valid-word-p
                 (mapcar #'(lambda (l)
                             (coerce l 'string))
                         (remove-char-from-list '() (coerce s 'list) '()))) )

;;;
;;;    Generate all possible lists by successively removing elt from list.
;;;    
(defun remove-char-from-list (before after result)
  (if (endp after)
      result
      (remove-char-from-list (append before (list (first after)))
                             (rest after)
                             (cons (append before (rest after)) result))))

(defun append (before after)
  (if (endp before)
      after
      (cons (first before) (append (rest before) after))))
    
;; (defun double-char (s)
;;   (double-char-aux s 0 '()))

;; (defun double-char-aux (s i results)
;;   (if (= i (length s))
;;       (remove-duplicates results :test #'string=)
;;       (let ((new-word (double-char-in-word s i)))
;;         (if (valid-word-p new-word)
;;             (double-char-aux s (1+ i) (cons new-word results))
;;             (double-char-aux s (1+ i) results)))) )

;; (defun double-char-in-word (s i)
;;   (let ((new-word (make-string (1+ (length s)))) )
;;     (double-char-in-word-before s new-word i 0)))

;; (defun double-char-in-word-before (old new i j)
;;   (cond ((= i (length old)) new)
;;         (t (setf (char new j) (char old j))
;;            (if (= i j)
;;                (double-char-in-word-after old new i (1+ j))
;;                (double-char-in-word-before old new i (1+ j)))) ))

;; (defun double-char-in-word-after (old new i j)
;;   (cond ((= i (length old)) new)
;;         (t (setf (char new j) (char old i))
;;            (double-char-in-word-after old new (1+ i) (1+ j)))) )

(defun double-char (s)
  (remove-if-not #'valid-word-p
                 (mapcar #'(lambda (l)
                             (coerce l 'string))
                         (double-char-in-list '() (coerce s 'list) '()))) )

(defun double-char-in-list (before after result)
  (if (endp after)
      result
      (double-char-in-list (append before (list (first after)))
                           (rest after)
                           (cons (append before (cons (first after) after)) result))))

;; (defun transpose-char (s)
;;   (transpose-char-aux (copy-seq s) 0 '()))

;; (defun transpose-char-aux (s i results)
;;   (if (= i (1- (length s)))
;;       (remove-duplicates results :test #'string=)
;;       (let ((new-word (copy-seq s)))
;;         (rotatef (char new-word i) (char new-word (1+ i)))
;;         (if (valid-word-p new-word)
;;             (transpose-char-aux s (1+ i) (cons new-word results))
;;             (transpose-char-aux s (1+ i) results)))) )

(defun transpose-char (s)
  (let ((char-list (coerce s 'list)))
    (remove-if-not #'valid-word-p
                   (mapcar #'(lambda (l)
                               (coerce l 'string))
                           (transpose-char-in-list '() (first char-list) (rest char-list) '()))) ))

(defun transpose-char-in-list (before current-char after result)
  (if (endp after)
      result
      (transpose-char-in-list (append before (list current-char))
                              (first after)
                              (rest after)
                              (cons (append before (cons (first after) (cons current-char (rest after)))) result))))

(defun insert-char (s)
  (remove-if-not #'valid-word-p
                 (mapcar #'(lambda (l)
                             (coerce l 'string))
                         (insert-char-in-list '() (coerce s 'list) '()))) )

(defun insert-char-in-list (before after result)
  (if (endp after)
      result
      (insert-char-in-list (append before (list (first after)))
                           (rest after)
                           (insert-char-at-position before after #\a result))))

(defun insert-char-at-position (before after char result)
  (if (char= char #\z)
      (cons (append before (cons char after)) result)
      (insert-char-at-position before after (code-char (1+ (char-code char))) (cons (append before (cons char after)) result))))

;;;
;;;    Not quite right...We just want to insert char at the position where we just removed it.
;;;    
;; (defun replace-char (s)
;;   (remove-duplicates (mapcan #'insert-char
;;                              (mapcar #'(lambda (l)
;;                                          (coerce l 'string))
;;                                      (remove-char-from-list '() (coerce s 'list) '())))
;;                      :test #'string=))

(defun replace-char (s)
  (remove-if-not #'valid-word-p
                 (mapcar #'(lambda (l)
                             (coerce l 'string))
                         (replace-char-in-list '() (coerce s 'list) '()))) )

(defun replace-char-in-list (before after result)
  (if (endp after)
      result
      (replace-char-in-list (append before (list (first after)))
                            (rest after)
                            (insert-char-at-position before (rest after) #\a result))))

;;;
;;;    II. Use any Common Lisp tool to solve the problem. In particular, LOOP, SUBSEQ, and CONCATENATE.
;;;    
(defun delete-char (s)
  (loop for i from 0 below (length s)
        for s1 = (concatenate 'string (subseq s 0 i) (subseq s (1+ i)))
        when (valid-word-p s1)
        collect s1))

(defun double-char (s)
  (loop for i from 0 below (length s)
        for s1 = (concatenate 'string (subseq s 0 (1+ i)) (subseq s i))
        when (valid-word-p s1)
        collect s1))

(defun transpose-char (s)
  (loop for i from 0 below (1- (length s))
        for s1 = (copy-seq s)
        do (rotatef (char s1 i) (char s1 (1+ i)))
        when (valid-word-p s1)
        collect s1))

(defun insert-char (s)
  (loop for i from 0 upto (length s)
        nconc (loop for ch from (char-code #\a) upto (char-code #\z)
                    for s1 = (concatenate 'string (subseq s 0 i) (string (code-char ch)) (subseq s i))
                    when (valid-word-p s1)
                    collect s1)))

(defun replace-char (s)
  (loop for i from 0 below (length s)
        nconc (loop for ch from (char-code #\a) upto (char-code #\z)
                    for s1 = (concatenate 'string (subseq s 0 i) (string (code-char ch)) (subseq s (1+ i)))
                    when (valid-word-p s1)
                    collect s1)))
                   
;;;
;;;    III. Destructively modify copies in place.
;;;
(defun delete-char (s)
  (let ((copy (subseq s 1))
        (results '()))
    (when (valid-word-p copy)
      (push (copy-seq copy) results))
    (dotimes (i (length copy) results)
      (setf (char copy i) (char s i))
      (when (valid-word-p copy)
        (push (copy-seq copy) results)))) )

;; (defun double-char (s)
;;   (let ((copy (concatenate 'string (subseq s 0 1) s))
;;         (results '()))
;;     (when (valid-word-p copy)
;;       (push (copy-seq copy) results))
;;     (dotimes (i (1- (length s)) results)
;;       (setf (char copy (1+ i)) (char s (1+ i)))
;;       (when (valid-word-p copy)
;;         (push (copy-seq copy) results)))) )

;;;
;;;    This is better after looking at 2009 version.
;;;    
(defun double-char (s)
  (let ((copy (concatenate 'string (subseq s 0 1) s))
        (results '()))
    (dotimes (i (length s) results)
      (setf (char copy i) (char s i))
      (when (valid-word-p copy)
        (push (copy-seq copy) results)))) )

(defun transpose-char (s)
  (let ((copy (copy-seq s))
        (results '()))
    (dotimes (i (1- (length s)) results)
      (rotatef (char copy i) (char copy (1+ i)))
      (when (valid-word-p copy)
        (push (copy-seq copy) results))
      (rotatef (char copy i) (char copy (1+ i)))) ))

(defun insert-char (s)
  (let ((copy (concatenate 'string (subseq s 0 1) s))
        (results '()))
    (when (valid-word-p copy)
      (push (copy-seq copy) results))
    (dotimes (i (length s) results)
      (setf (char copy i) (char s i))
      (dotimes (j 26)
        (setf (char copy (1+ i)) (code-char (+ j (char-code #\a))))
        (when (valid-word-p copy)
          (push (copy-seq copy) results)))) ))

(defun replace-char (s)
  (let ((copy (copy-seq s))
        (results '()))
    (dotimes (i (length s) results)
      (dotimes (j 26)
        (setf (char copy i) (code-char (+ j (char-code #\a))))
        (when (valid-word-p copy)
          (push (copy-seq copy) results)))
      (setf (char copy i) (char s i)))) )
        
;;;
;;;    Tests
;;;
(defun set-equal (l1 l2)
  (and (subsetp l1 l2 :test #'string=)
       (subsetp l2 l1 :test #'string=)))

(deftest test-valid-word-p ()
  (check
   (valid-word-p "sugar")
   (valid-word-p "apple")
   (valid-word-p "tooth")
   (valid-word-p "daughter")
   (not (valid-word-p "timo"))))

(deftest test-delete-char ()
  (check
   (set-equal (delete-char "tigger") '("tiger" "tiger"))
   (set-equal (delete-char "vission") '("vision" "vision"))
   (set-equal (delete-char "apple") NIL)
   (set-equal (delete-char "sudden") NIL)
   (set-equal (delete-char "grill") '("gill" "rill"))))

(deftest test-double-char ()
  (check
   (set-equal (double-char "gril") '("grill"))
   (set-equal (double-char "suden") '("sudden"))
   (set-equal (double-char "aple") '("apple"))
   (set-equal (double-char "fredom") '("freedom"))
   (set-equal (double-char "twirl") NIL)))

(deftest test-transpose-char ()
  (check
   (set-equal (transpose-char "gril") '("girl"))
   (set-equal (transpose-char "prespire") '("perspire"))
   (set-equal (transpose-char "centre") '("center"))
   (set-equal (transpose-char "teh") '("the"))
   (set-equal (transpose-char "a") NIL)))

(deftest test-insert-char ()
  (check
   (set-equal (insert-char "upst") '("upsit" "upset"))
   (set-equal (insert-char "ordr") '("order"))
   (set-equal (insert-char "rndom") '("random"))
   (set-equal (insert-char "clt") '("clot" "clit" "clat" "cult" "colt" "celt"))
   (set-equal (insert-char "lama") '("lamia" "lamba" "llama" "llama"))))

(deftest test-replace-char ()
  (check
   (set-equal (replace-char "seperate") '("separate"))
   (set-equal (replace-char "supercede") '("supersede"))
   (set-equal (replace-char "lazer") '("lazar" "layer" "laver" "later" "laser" "laker" "lager" "lader" "lacer" "razer" "mazer" "hazer" "gazer"))))
