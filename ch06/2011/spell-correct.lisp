;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               spell-correct.lisp
;;;;
;;;;   Started:            Sat Aug  6 02:00:21 2011
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
;;;;   Notes: -Generalize
;;;;          -Examine single-case functions
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/io.lisp")

(defpackage :spell-correct (:use :common-lisp :io))

(in-package :spell-correct)

(defvar *dictionary*)

(defun load-dictionary ()
  (setf *dictionary* (make-hash-table :test #'equal))
  (loop for word in (read-file "/usr/share/dict/web2")
        do (setf (gethash word *dictionary*) t)))

(defun wordp (word)
  (gethash word *dictionary*))

(defun spell-correct (word)
  (if (wordp word)
      word
      (sort (remove-duplicates (loop for test in (list #'delete-chars #'double-chars #'transpose-chars #'insert-chars #'replace-chars)
                                     nconc (remove-if-not #'wordp (funcall test word)))
                               :test #'string=)
            #'string<)))

;;;
;;;    Delete
;;;
(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (delete-char s i)))

(defun delete-char (s n)
  (loop for ch across s
        for i from 0
        with j = 0
        and result = (make-string (1- (length s)))
        unless (= i n)
        do (setf (char result j) ch)
          (incf j)
        end
        finally (return result)))

(defun delete-chars (s)
  (labels ((delete-chars-aux (before after result)
             (if (endp after)
                 result
                 (delete-chars-aux (cons (first after) before)
                                   (rest after)
                                   (cons (coerce (revappend before (rest after)) 'string) result)))) )
    (delete-chars-aux '() (coerce s 'list) '())))

(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (delete-char s i)))

;; (defun delete-chars (s)
;;   (labels ((delete-chars-aux (i result)
;;              (if (= i (length s))
;;                  result
;;                  (delete-chars-aux (1+ i) (cons (delete-char s i) result)))) )
;;     (delete-chars-aux 0 '())))

;; (defun delete-chars (s)
;;   (mapcar #'(lambda (i) (delete-char s i)) (loop for i from 0 below (length s) collect i)))

(defun delete-char (s i)
  (concatenate 'string (subseq s 0 i) (subseq s (1+ i))))

(defun delete-chars (s)
  (do ((i 0 (1+ i))
       (result '())
       (s1 (make-string (1- (length s))) (make-string (1- (length s)))) )
      ((= i (length s)) result)
    (do ((j 0 (1+ j))
         (k 0))
        ((= j (length s)))
      (unless (= j i)
        (setf (char s1 k) (char s j))
        (incf k)))
    (push s1 result)))

(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (remove (char s i) s :count 1 :start i)))

(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (delete-char s i (make-string-output-stream))))

(defun delete-char (s n stream)
  (labels ((copy-before (i)
             (if (= i n)
                 (copy-after (1+ i))
                 (progn (write-char (char s i) stream)
                        (copy-before (1+ i)))) )
           (copy-after (i)
             (if (= i (length s))
                 (get-output-stream-string stream)
                 (progn (write-char (char s i) stream)
                        (copy-after (1+ i)))) ))
    (copy-before 0)))

(defun delete-chars (s)
  (loop for i from 0 below (length s)
        with s1 = (subseq s 1)
        unless (zerop i)
        do (setf (char s1 (1- i)) (char s (1- i)))
        end
        collect (copy-seq s1)))

(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (delete-char s i)))

(defun delete-char (s n)
  (with-output-to-string (result)
    (loop for ch across s
          for i from 0
          unless (= i n)
          do (write-char ch result))))
;;;
;;;    Favorite!
;;;    
(defun delete-chars (s)
  (loop for i from 0 below (length s)
        collect (delete-char s i)))

(defun delete-char (s n)
  (with-output-to-string (result)
    (dotimes (i (length s))
      (unless (= i n)
        (write-char (char s i) result)))) )

;;;
;;;    Double
;;;    
(defun double-chars (s)
  (loop for i from 0 below (length s)
        collect (double-char s i)))

(defun double-char (s n)
  (loop for i from 0 to (length s)
        with j = 0
        and result = (make-string (1+ (length s)))
        do (setf (char result i) (char s j))
        unless (= i n)
        do (incf j)
        end
        finally (return result)))

(defun double-chars (s)
  (labels ((double-chars-aux (before after result)
             (if (endp after)
                 result
                 (double-chars-aux (cons (second after) before)
                                   (rest after)
                                   (cons (coerce (revappend before after) 'string) result)))) )
    (let ((l (coerce s 'list)))
      (double-chars-aux (list (first l)) l '()))) )

(defun double-chars (s)
  (loop for i from 0 below (length s)
        collect (double-char s i)))

;; (defun double-chars (s)
;;   (labels ((double-chars-aux (i result)
;;              (if (= i (length s))
;;                  result
;;                  (double-chars-aux (1+ i) (cons (double-char s i) result)))) )
;;     (double-chars-aux 0 '())))

;; (defun double-chars (s)
;;   (mapcar #'(lambda (i) (double-char s i)) (loop for i from 0 below (length s) collect i)))

(defun double-char (s i)
  (concatenate 'string (subseq s 0 (1+ i)) (subseq s i)))

(defun double-chars (s)
  (do ((i 0 (1+ i))
       (result '())
       (s1 (make-string (1+ (length s))) (make-string (1+ (length s)))) )
      ((= i (length s)) result)
    (do ((j 0 (1+ j))
         (k 0))
        ((= j (length s1)))
      (setf (char s1 j) (char s k))
      (unless (= j i)
        (incf k)))
    (push s1 result)))

(defun double-chars (s)
  (loop for i from 0 below (length s)
        collect (double-char s i (make-string-output-stream))))

;;;
;;;    Compare DOUBLE-CHAR-IN-WORD-BEFORE/AFTER in 2010/spell-correct.lisp
;;;    
(defun double-char (s n stream)
  (labels ((copy-before (i)
             (write-char (char s i) stream)
             (if (= i n)
                 (copy-after i)
                 (copy-before (1+ i))))
           (copy-after (i)
             (if (= i (length s))
                 (get-output-stream-string stream)
                 (progn (write-char (char s i) stream)
                        (copy-after (1+ i)))) ))
    (copy-before 0)))

(defun double-chars (s)
  (let ((s1 (make-string (1+ (length s)))))
    (setf (subseq s1 1) s)
    (loop for i from 0 below (length s)
          do (setf (char s1 i) (char s i))
          collect (copy-seq s1))))

(defun double-chars (s)
  (loop for i from 0 below (length s)
        collect (double-char s i)))

(defun double-char (s n)
  (with-output-to-string (result)
    (loop for ch across s
          for i from 0
          do (write-char ch result)
          when (= i n)
          do (write-char ch result))))
;;;
;;;    Favorite!
;;;    
(defun double-chars (s)
  (loop for i from 0 below (length s)
        collect (double-char s i)))

(defun double-char (s n)
  (with-output-to-string (result)
    (dotimes (i (length s))
      (write-char (char s i) result)
      (when (= i n)
        (write-char (char s i) result)))) )

;;;
;;;    Transpose
;;;    
(defun swap-chars (s i j)
  (coerce (loop for k from 0 below (length s)
                collect (cond ((= k i) (char s j))
                              ((= k j) (char s i))
                              (t (char s k))))
          'string))

(defun swap-chars (s i j)
  (map 'string
       #'(lambda (k)
           (cond ((= k i) (char s j))
                 ((= k j) (char s i))
                 (t (char s k))))
       (loop for i from 0 below (length s) collect i)))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i)))

(defun transpose-char (s n)
  (loop for ch across s
        for i from 0
        with result = (make-string (length s))
        do (cond ((= i n) (setf (char result (1+ i)) ch))
                 ((= i (1+ n)) (setf (char result (1- i)) ch))
                 (t (setf (char result i) ch)))
        finally (return result)))

(defun transpose-chars (s)
  (labels ((transpose-chars-aux (before after result)
             (if (endp after)
                 result
                 (transpose-chars-aux (cons (first after) before)
                                      (rest after)
                                      (cons (coerce (revappend (rest before) (list* (first after) (first before) (rest after))) 'string) result)))) )
    (let ((l (coerce s 'list)))
      (transpose-chars-aux (list (first l)) (rest l) '()))) )

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i)))

;; (defun transpose-chars (s)
;;   (labels ((transpose-chars-aux (i result)
;;              (if (= i (1- (length s)))
;;                  result
;;                  (transpose-chars-aux (1+ i) (cons (transpose-char s i) result)))) )
;;     (transpose-chars-aux 0 '())))

;; (defun transpose-chars (s)
;;   (mapcar #'(lambda (i) (transpose-char s i)) (loop for i from 0 below (1- (length s)) collect i)))

(defun transpose-char (s i)
  (concatenate 'string (subseq s 0 i) (reverse (subseq s i (+ i 2))) (subseq s (+ i 2))))

(defun transpose-chars (s)
  (do ((i 0 (1+ i))
       (result '())
       (s1 (make-string (length s)) (make-string (length s))))
      ((= i (1- (length s))) result)
    (do ((j 0 (1+ j)))
        ((= j (length s)))
      (cond ((= j i) (setf (char s1 (1+ j)) (char s j)))
            ((= j (1+ i)) (setf (char s1 (1- j)) (char s j)))
            (t (setf (char s1 j) (char s j)))) )
    (push s1 result)))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i (make-string-output-stream))))

(defun transpose-char (s n stream)
  (labels ((copy-before (i)
             (if (= i n)
                 (progn (write-char (char s (1+ i)) stream)
                        (write-char (char s i) stream)
                        (copy-after (+ i 2)))
                 (progn (write-char (char s i) stream)
                        (copy-before (1+ i)))) )
           (copy-after (i)
             (if (= i (length s))
                 (get-output-stream-string stream)
                 (progn (write-char (char s i) stream)
                        (copy-after (1+ i)))) ))
    (copy-before 0)))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        for s1 = (copy-seq s)
        do (rotatef (char s1 i) (char s1 (1+ i)))
        collect s1))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i)))

(defun transpose-char (s n)
  (with-output-to-string (result)
    (loop for i from 0 below (length s)
          do (cond ((= i n) (write-char (char s (1+ i)) result))
                   ((= i (1+ n)) (write-char (char s (1- i)) result))
                   (t (write-char (char s i) result)))) ))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i)))

(defun transpose-char (s n)
  (with-output-to-string (result)
    (dotimes (i (length s))
      (cond ((= i n) (write-char (char s (1+ i)) result))
            ((= i (1+ n)) (write-char (char s (1- i)) result))
            (t (write-char (char s i) result)))) ))

(defun transpose-chars (s)
  (loop for i from 0 below (1- (length s))
        collect (transpose-char s i)))

(defun transpose-char (s i)
  (let ((result (copy-seq s)))
    (rotatef (char result i) (char result (1+ i)))
    result))

;;;
;;;    Insert
;;;    
(defun insert-chars (s)
  (loop for i from 0 upto (length s)
        nconc (insert-char s i)))

(defun insert-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (loop for i from 0 upto (length s)
                      with j = 0
                      and result = (make-string (1+ (length s)))
                      do (cond ((= i n) (setf (char result i) (code-char ch)))
                               (t (setf (char result i) (char s j)) (incf j)))
                      finally (return result))))

;; (defun insert-chars (s)
;;   (labels ((insert-chars-aux (before after result ch)
;;              (cond ((and (endp after) (char> ch #\z)) result)
;;                    ((char> ch #\z) (insert-chars-aux (cons (first after) before) (rest after) result #\a))
;;                    (t (insert-chars-aux before
;;                                         after
;;                                         (cons (coerce (revappend before (cons ch after)) 'string)
;;                                               result)
;;                                         (code-char (1+ (char-code ch)))) ))))
;;     (insert-chars-aux '() (coerce s 'list) '() #\a)))

;;;
;;;    Modified based on 2010 version.
;;;    
(defun insert-chars (s)
  (labels ((insert-chars-aux (before after result)
             (if (endp after)
                 (insert-char before after result #\a)
                 (insert-chars-aux (cons (first after) before) (rest after) (insert-char before after result #\a))))
           (insert-char (before after result ch)
             (if (char> ch #\z)
                 result
                 (insert-char before after (cons (coerce (revappend before (cons ch after)) 'string) result) (code-char (1+ (char-code ch)))) )))
    (insert-chars-aux '() (coerce s 'list) '())))

(defun insert-chars (s)
  (loop for i from 0 to (length s)
        nconc (loop for ch from (char-code #\a) to (char-code #\z)
                    collect (insert-char s (code-char ch) i))))

;; (defun insert-chars (s)
;;   (labels ((insert-chars-aux (i ch result)
;;              (cond ((> i (length s)) result)
;;                    ((char> ch #\z) (insert-chars-aux (1+ i) #\a result))
;;                    (t (insert-chars-aux i (code-char (1+ (char-code ch))) (cons (insert-char s ch i) result)))) ))
;;     (insert-chars-aux 0 #\a '())))

;;;
;;;    The order of the results is very different here compared to other versions.
;;;    
;; (defun insert-chars (s)
;;   (let ((indices (loop for i from 0 to (length s) collect i)))
;;     (mapcan #'(lambda (ch)
;;                 (mapcar #'(lambda (i)
;;                             (insert-char s ch i))
;;                         indices))
;;             (loop for ch from (char-code #\a) to (char-code #\z) collect (code-char ch)))) )

(defun insert-char (s ch i)
  (concatenate 'string (subseq s 0 i) (string ch) (subseq s i)))

(defun insert-chars (s)
  (do ((i 0 (1+ i))
       (result '()))
      ((> i (length s)) result)
    (do ((ch #\a (code-char (1+ (char-code ch)))) )
        ((char> ch #\z))
      (do ((j 0 (1+ j))
           (k 0)
           (s1 (make-string (1+ (length s)))) )
          ((> j (length s)) (push s1 result))
        (cond ((= j i) (setf (char s1 j) ch))
              (t (setf (char s1 j) (char s k)) (incf k)))) )))

(defun insert-chars (s)
  (loop for i from 0 to (length s)
        nconc (insert-char s i)))

(defun insert-char (s n)
  (labels ((insert-before (i ch stream)
             (if (= i n)
                 (progn (write-char ch stream)
                        (insert-after i stream))
                 (progn (write-char (char s i) stream)
                        (insert-before (1+ i) ch stream))))
           (insert-after (i stream)
             (if (= i (length s))
                 (get-output-stream-string stream)
                 (progn (write-char (char s i) stream)
                        (insert-after (1+ i) stream)))) )
    (loop for ch from (char-code #\a) to (char-code #\z)
          collect (insert-before 0 (code-char ch) (make-string-output-stream)))) )

(defun insert-chars (s)
  (let ((s1 (make-string (1+ (length s)))) )
    (setf (subseq s1 1) s)
    (loop for i from 0 to (length s)
          unless (zerop i)
          do (setf (char s1 (1- i)) (char s (1- i)))
          end
          nconc (loop for ch from (char-code #\a) to (char-code #\z)
                      do (setf (char s1 i) (code-char ch))
                      collect (copy-seq s1)))) )

(defun insert-chars (s)
  (loop for i from 0 to (length s)
        nconc (insert-char s i)))

;; (defun insert-char (s n)
;;   (loop for ch from (char-code #\a) to (char-code #\z)
;;         collect (with-output-to-string (result)
;;                   (loop for ch1 across s
;;                         for i from 0
;;                         if (= i n)
;;                           do (write-char (code-char ch) result)
;;                         end
;;                         do (write-char ch1 result)
;;                         finally (when (= n (length s)) (write-char (code-char ch) result)))) ))

(defun insert-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (with-output-to-string (result)
                  (loop for i from 0 to (length s)
                        if (= i n)
                          do (write-char (code-char ch) result)
                        end
                        if (< i (length s))
                          do (write-char (char s i) result)
                        end))))

(defun insert-chars (s)
  (loop for i from 0 upto (length s)
        nconc (insert-char s i)))

(defun insert-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (with-output-to-string (result)
                  (dotimes (i (1+ (length s)))
                    (when (= i n)
                      (write-char (code-char ch) result))
                    (when (< i (length s))
                      (write-char (char s i) result)))) ))

;;;
;;;    Replace
;;;    (Replace the existing char with the same char?!)
;;;    
(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (replace-char s i)))

(defun replace-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        and result = (copy-seq s)
        do (setf (char result n) (code-char ch))
        collect result))

;; (defun replace-char (s n)
;;   (loop for ch from (char-code #\a) to (char-code #\z)
;;         collect (loop for i from 0 below (length s)
;;                       with result = (make-string (length s))
;;                       do (cond ((= i n) (setf (char result i) (code-char ch)))
;;                                (t (setf (char result i) (char s i))))
;;                       finally (return result))))

;; (defun replace-chars (s)
;;   (labels ((replace-chars-aux (before after result ch)
;;              (cond ((endp after) result)
;;                    ((char> ch #\z) (replace-chars-aux (cons (first after) before) (rest after) result #\a))
;;                    (t (replace-chars-aux before
;;                                          after
;;                                          (cons (coerce (revappend before (cons ch (rest after))) 'string) result)
;;                                          (code-char (1+ (char-code ch)))) ))))
;;     (replace-chars-aux '() (coerce s 'list) '() #\a)))
             
;;;
;;;    Modified based on 2010 version.
;;;    
(defun replace-chars (s)
  (labels ((replace-chars-aux (before after result)
             (if (endp after)
                 result
                 (replace-chars-aux (cons (first after) before) (rest after) (replace-char before after result #\a))))
           (replace-char (before after result ch)
             (if (char> ch #\z)
                 result
                 (replace-char before after (cons (coerce (revappend before (cons ch (rest after))) 'string) result) (code-char (1+ (char-code ch)))) )))
    (replace-chars-aux '() (coerce s 'list) '())))

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (loop for ch from (char-code #\a) to (char-code #\z)
                    collect (replace-char s (code-char ch) i))))

;; (defun replace-chars (s)
;;   (labels ((replace-chars-aux (i ch result)
;;              (cond ((= i (length s)) result)
;;                    ((char> ch #\z) (replace-chars-aux (1+ i) #\a result))
;;                    (t (replace-chars-aux i (code-char (1+ (char-code ch))) (cons (replace-char s ch i) result)))) ))
;;     (replace-chars-aux 0 #\a '())))

;; (defun replace-chars (s)
;;   (let ((indices (loop for i from 0 below (length s) collect i)))
;;     (mapcan #'(lambda (ch)
;;                 (mapcar #'(lambda (i)
;;                             (replace-char s ch i))
;;                         indices))
;;             (loop for ch from (char-code #\a) to (char-code #\z) collect (code-char ch)))) )

(defun replace-char (s ch i)
  (concatenate 'string (subseq s 0 i) (string ch) (subseq s (1+ i))))

(defun replace-chars (s)
  (do ((i 0 (1+ i))
       (result '()))
      ((= i (length s)) result)
    (do ((ch (char-code #\a) (1+ ch))
         (s1 (copy-seq s) (copy-seq s)))
        ((> ch (char-code #\z)))
      (setf (char s1 i) (code-char ch))
      (push s1 result))))

;; (defun replace-chars (s)
;;   (do ((i 0 (1+ i))
;;        (result '()))
;;       ((= i (length s)) result)
;;     (do ((ch #\a (code-char (1+ (char-code ch)))))
;;         ((char> ch #\z))
;;       (do ((j 0 (1+ j))
;;            (s1 (make-string (length s))))
;;           ((= j (length s)) (push s1 result))
;;         (if (= j i)
;;             (setf (char s1 j) ch)
;;             (setf (char s1 j) (char s j)))) )))

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (replace-char s i)))

(defun replace-char (s n)
  (labels ((replace-before (i ch stream)
             (if (= i n)
                 (progn (write-char ch stream)
                        (replace-after (1+ i) stream))
                 (progn (write-char (char s i) stream)
                        (replace-before (1+ i) ch stream))))
           (replace-after (i stream)
             (if (= i (length s))
                 (get-output-stream-string stream)
                 (progn (write-char (char s i) stream)
                        (replace-after (1+ i) stream)))) )
    (loop for ch from (char-code #\a) to (char-code #\z)
          collect (replace-before 0 (code-char ch) (make-string-output-stream)))) )

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        for s1 = (copy-seq s)
        nconc (loop for ch from (char-code #\a) to (char-code #\z)
                    do (setf (char s1 i) (code-char ch))
                    collect (copy-seq s1))))

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (replace-char s i)))

(defun replace-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (with-output-to-string (result)
                  (loop for i from 0 below (length s)
                        do (if (= i n)
                               (write-char (code-char ch) result)
                               (write-char (char s i) result)))) ))

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (replace-char s i)))

(defun replace-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (with-output-to-string (result)
                  (dotimes (i (length s))
                    (if (= i n)
                        (write-char (code-char ch) result)
                        (write-char (char s i) result)))) ))

(defun replace-chars (s)
  (loop for i from 0 below (length s)
        nconc (replace-char s i)))

(defun replace-char (s n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        and result = (copy-seq s)
        do (setf (char result n) (code-char ch))
        collect result))
