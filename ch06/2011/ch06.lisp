;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Wed Jul  6 23:31:46 2011
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

(defpackage :ch06
  (:use :common-lisp :test)
  (:shadow :string-equal :string-lessp :string-greaterp :string< :string> :merge))

(in-package :ch06)

;;;
;;;    6.8.2
;;;
(defun lastchar (s)
  (char s (1- (length s))))

;;
;;    No need to traverse entire string! (Random access...)
;;    
(defun lastchar (s)
  (loop for ch across s finally (return ch)))

(defun capitalize (s)
  (let ((l (coerce s 'list)))
    (coerce (cons (char-upcase (first l)) (rest l)) 'string)))

(defun capitalize (s)
  (concatenate 'string (string-upcase (subseq s 0 1)) (subseq s 1)))
;  (concatenate 'string (string-upcase (char s 0)) (subseq s 1)))   <-- OK

(defun capitalize (s)
  (if (zerop (length s))
      ""
      (with-output-to-string (result)
        (write-char (char-upcase (char s 0)) result)
        (loop for i from 1 below (length s)
              do (write-char (char s i) result)))) )

;;;
;;;    Two basic approaches to test for string equality.
;;;    1. Compare lengths up front. If lengths differ, then the strings are not equal.
;;;       Otherwise compare all pairs of corresponding characters.
;;;    2. Compare corresponding characters. If the end of one string is reached confirm
;;;       that the other is at the end too.
;;;       
(defun string-equal (s1 s2)
  (if (= (length s1) (length s2))
      (dotimes (i (length s1) t)
        (unless (char-equal (char s1 i) (char s2 i))
          (return nil)))
      nil))

(defun string-equal (s1 s2)
  (if (= (length s1) (length s2))
      (every #'char-equal s1 s2)
      nil))

(defun string-equal (s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (if (= l1 l2)
        (loop for ch1 across s1
              for ch2 across s2
              unless (char-equal ch1 ch2)
              do (return nil)
              finally (return t))
        nil)))

(defun string-equal (s1 s2)
  (do ((l1 (length s1))
       (l2 (length s2))
       (i 0 (1+ i)))
      ((or (= i l1) (= i l2)) (= l1 l2))
    (unless (char-equal (char s1 i) (char s2 i))
      (return nil))))

(defun string-equal (s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (labels ((string-equal-aux (i)
               (cond ((= i l1) (= i l2))
                     ((= i l2) nil)
                     ((char-equal (char s1 i) (char s2 i)) (string-equal-aux (1+ i)))
                     (t nil))))
      (string-equal-aux 0))))

(deftest test-string-equal ()
  (check
   (string-equal "LISP" "lisp")
   (string-equal "" "")
   (not (string-equal "" "Lisp"))
   (not (string-equal "Lisp" ""))
   (not (string-equal "Lis" "Lisp"))))

;;;
;;;    The following implementations using keyword args do not validate those args, e.g.,
;;;    (minusp start1) or (> end2 (length s2))?
;;;    
(defun string-equal (s1 s2 &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (if (= (- end1 start1) (- end2 start2))
      (dotimes (i (- end1 start1) t)
        (unless (char-equal (char s1 (+ i start1)) (char s2 (+ i start2)))
          (return nil)))
      nil))

(defun string-equal (s1 s2 &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (if (= (- end1 start1) (- end2 start2))
      (every #'char-equal (subseq s1 start1 end1) (subseq s2 start2 end2))
      nil))

(defun string-equal (s1 s2 &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (if (= (- end1 start1) (- end2 start2))
      (loop for ch1 across (subseq s1 start1 end1)
            for ch2 across (subseq s2 start2 end2)
            unless (char-equal ch1 ch2)
            do (return nil)
            finally (return t))
      nil))

(defun string-equal (s1 s2 &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (do ((l1 (- end1 start1))
       (l2 (- end2 start2))
       (i 0 (1+ i)))
      ((or (= i l1) (= i l2)) (= l1 l2))
    (unless (char-equal (char s1 (+ i start1)) (char s2 (+ i start2)))
      (return nil))))

;;;
;;;    The corresponding implementation above for this function need only retain one index
;;;    variable I. However, here I and J index S1 and S2 starting at START1 and START2, respectively.
;;;    
(defun string-equal (s1 s2 &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (labels ((string-equal-aux (i j)
             (cond ((= i end1) (= j end2))
                   ((= j end2) nil)
                   ((char-equal (char s1 i) (char s2 j)) (string-equal-aux (1+ i) (1+ j)))
                   (t nil))))
    (string-equal-aux start1 start2)))

(deftest test-string-equal ()
  (check
   (string-equal "LISP" "lisp")
   (string-equal "LISP" "Is this not lisp?" :start2 12 :end2 16)
   (string-equal "Lis" "Lisp" :end2 3)
   (string-equal "" "")
   (not (string-equal "" "Lisp"))
   (not (string-equal "Lisp" ""))
   (not (string-equal "Lis" "Lisp"))))

;; (defun string-lessp (s1 s2)
;;   (loop for ch1 across s1
;;         for ch2 across s2
;;         for i from 0
;;         unless (char-lessp ch1 ch2)
;;         do (return i)

;;;
;;;    1. If s2 is exhausted, then s1 does not precede it.
;;;    2. Otherwise if s1 is exhausted, then it does precede s2.
;;;    3. If the current character in s1 precedes the corresponding character in s2,
;;;       then s1 precedes s2.
;;;    4. If the opposite is true, then s1 does not precede s2.
;;;    5. If the corresponding characters are equal, then examine the rest of each string.
;;;    
(defun string-lessp (s1 s2)
  (do ((i 0 (1+ i))
       (l1 (length s1))
       (l2 (length s2)))
      (nil)
    (cond ((= i l2) (return nil))
          ((= i l1) (return i))
          ((char-lessp (char s1 i) (char s2 i)) (return i))
          ((char-lessp (char s2 i) (char s1 i)) (return nil)))) )

(defun string-lessp (s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (labels ((string-lessp-aux (i)
               (cond ((= i l2) nil)
                     ((= i l1) i)
                     ((char-lessp (char s1 i) (char s2 i)) i)
                     ((char-lessp (char s2 i) (char s1 i)) nil)
                     (t (string-lessp-aux (1+ i)))) ))
      (string-lessp-aux 0))))

(defun string-lessp (s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (labels ((string-lessp-aux (i)
               (cond ((= i l2) nil)
                     ((= i l1) i)
                     ((char-lessp (char s1 i) (char s2 i)) i)
                     ((char-equal (char s1 i) (char s2 i)) (string-lessp-aux (1+ i)))
                     (t nil))))
      (string-lessp-aux 0))))

(defun string-lessp (s1 s2)
  (loop for i from 0
        with l1 = (length s1)
        and l2 = (length s2)
        if (= i l2) do (return nil)
        if (= i l1) do (return i)
        if (char-lessp (char s1 i) (char s2 i)) do (return i)
        if (char-lessp (char s2 i) (char s1 i)) do (return nil)))

(defun string-lessp (s1 s2)
  (let ((i 0))
    (map nil #'(lambda (ch1 ch2)
                 (cond ((char-lessp ch1 ch2) (return-from string-lessp i))
                       ((char-lessp ch2 ch1) (return-from string-lessp nil)))
                 (incf i))
         s1
         s2)
    (if (< i (length s2))
        i
        nil)))

(defun string-lessp (s1 s2)
  (dotimes (i (min (length s1) (length s2)) (if (< i (length s2)) i nil))
    (cond ((char-lessp (char s1 i) (char s2 i)) (return i))
          ((char-lessp (char s2 i) (char s1 i)) (return nil)))) )

(defun string-compare (f-char f-len)
  #'(lambda (s1 s2 i)
      (dotimes (i (min (length s1) (length s2)) (if (funcall f-len (length s1) (length s2)) i nil))
        (cond ((funcall f-char (char s1 i) (char s2 i)) (return i))
              ((funcall f-char (char s2 i) (char s1 i)) (return nil)))) ))

(defun string-lessp (s1 s2)
  (funcall (string-compare #'char-lessp #'<) s1 s2 0))

(defun string-greaterp (s1 s2)
  (funcall (string-compare #'char-greaterp #'>) s1 s2 0))

(defun string< (s1 s2)
  (funcall (string-compare #'char< #'<) s1 s2 0))

(defun string> (s1 s2)
  (funcall (string-compare #'char> #'>) s1 s2 0))

(defun string-compare (compare-chars less)
  (if less
      (labels ((compare (s1 s2 i)
                 (cond ((= i (length s2)) nil)
                       ((= i (length s1)) i)
                       ((funcall compare-chars (char s1 i) (char s2 i)) i)
                       ((funcall compare-chars (char s2 i) (char s1 i)) nil)
                       (t (compare s1 s2 (1+ i)))) ))
        #'compare)
      (labels ((compare (s1 s2 i)
                 (cond ((= i (length s1)) nil)
                       ((= i (length s2)) i)
                       ((funcall compare-chars (char s1 i) (char s2 i)) i)
                       ((funcall compare-chars (char s2 i) (char s1 i)) nil)
                       (t (compare s1 s2 (1+ i)))) ))
        #'compare)))
  
(defun string-lessp (s1 s2)
  (funcall (string-compare #'char-lessp t) s1 s2 0))

(defun string-greaterp (s1 s2)
  (funcall (string-compare #'char-greaterp nil) s1 s2 0))

(defun string< (s1 s2)
  (funcall (string-compare #'char< t) s1 s2 0))

(defun string> (s1 s2)
  (funcall (string-compare #'char> nil) s1 s2 0))

;; (defun string-compare (f-compare f-equal result)
;;   (labels ((string-compare-aux (s1 s2 i)
;;                (cond ((= i (length s1)) (if (/= i (length s2)) (and result i) nil))
;;                      ((= i (length s2)) (and (not result) i))
;;                      ((funcall f-compare (char s1 i) (char s2 i)) i)
;;                      ((funcall f-equal (char s1 i) (char s2 i)) (string-compare-aux s1 s2 (1+ i)))
;;                      (t nil))))
;;     #'string-compare-aux))

;; (defun string-lessp (s1 s2)
;;   (funcall (string-compare #'char-lessp #'char-equal t) s1 s2 0))

;; (defun string-greaterp (s1 s2)
;;   (funcall (string-compare #'char-greaterp #'char-equal nil) s1 s2 0))

;; (defun string< (s1 s2)
;;   (funcall (string-compare #'char< #'char= t) s1 s2 0))

;; (defun string> (s1 s2)
;;   (funcall (string-compare #'char> #'char= nil) s1 s2 0))

;; (deftest test-string-lessp ()
;;   (check
;; ;;    (eql (string-lessp "abc" "pdg") (cl:string-lessp "abc" "pdg"))
;; ;;    (eql (string-lessp "abc" "apdg") (cl:string-lessp "abc" "apdg"))
;; ;;    (eql (string-lessp "abc" "abcd") (cl:string-lessp "abc" "abcd"))
;; ;;    (eql (string-lessp "abcd" "abc") (cl:string-lessp "abcd" "abc"))
;; ;;    (eql (string-lessp "alpha" "beta") (cl:string-lessp "alpha" "beta"))
;; ;;    (eql (string-lessp "beta" "alpha") (cl:string-lessp "beta" "alpha"))
;; ;;    (eql (string-lessp "alphabet" "alphabetize") (cl:string-lessp "alphabet" "alphabetize"))
;; ;;    (eql (string-lessp "alphabetize" "alphabet") (cl:string-lessp "alphabetize" "alphabet"))))

;;    (every #'(lambda (pair) (destructuring-bind (s1 s2) pair (eql (string-lessp s1 s2) (cl:string-lessp s1 s2))))
;;           '(("abc" "pdg")
;;             ("abc" "apdg")
;;             ("abc" "abcd")
;;             ("abcd" "abc")
;;             ("alpha" "beta")
;;             ("beta" "alpha")
;;             ("alphabet" "alphabetize")
;;             ("alphabetize" "alphabet")))))

(defvar *test-data* '(("abc" "pdg")
                      ("ABC" "pdg")
                      ("abc" "apdg")
                      ("ABC" "apdg")
                      ("abc" "abcd")
                      ("ABC" "abcd")
                      ("abcd" "abc")
                      ("ABCD" "abc")
                      ("alpha" "beta")
                      ("ALPHA" "beta")
                      ("beta" "alpha")
                      ("BETA" "alpha")
                      ("alphabet" "alphabetize")
                      ("ALPHABET" "alphabetize")
                      ("alphabetize" "alphabet")
                      ("ALPHABETIZE" "alphabet")))

(deftest test-string-lessp ()
  (compare-reference-function string-lessp
                              cl:string-lessp
                              *test-data*
                              :test eql))

(deftest test-string-greaterp ()
  (compare-reference-function string-greaterp
                              cl:string-greaterp
                              *test-data*
                              :test eql))

;; (deftest test-string-greaterp ()
;;   (compare-reference-function string-greaterp
;;                               cl:string-greaterp
;;                               '(("abc" "pdg")
;;                                 ("abc" "apdg")
;;                                 ("abc" "abcd")
;;                                 ("abcd" "abc")
;;                                 ("alpha" "beta")
;;                                 ("beta" "alpha")
;;                                 ("alphabet" "alphabetize")
;;                                 ("alphabetize" "alphabet"))
;;                               :test eql))

(deftest test-string< ()
  (compare-reference-function string<
                              cl:string<
                              *test-data*
                              :test eql))

;; (deftest test-string< ()
;;   (compare-reference-function string<
;;                               cl:string<
;;                               '(("abc" "pdg")
;;                                 ("abc" "apdg")
;;                                 ("abc" "abcd")
;;                                 ("abcd" "abc")
;;                                 ("alpha" "beta")
;;                                 ("beta" "alpha")
;;                                 ("alphabet" "alphabetize")
;;                                 ("alphabetize" "alphabet"))
;;                               :test eql))

(deftest test-string> ()
  (compare-reference-function string>
                              cl:string>
                              *test-data*
                              :test eql))

;; (deftest test-string> ()
;;   (compare-reference-function string>
;;                               cl:string>
;;                               '(("abc" "pdg")
;;                                 ("abc" "apdg")
;;                                 ("abc" "abcd")
;;                                 ("abcd" "abc")
;;                                 ("alpha" "beta")
;;                                 ("beta" "alpha")
;;                                 ("alphabet" "alphabetize")
;;                                 ("alphabetize" "alphabet"))
;;                               :test eql))

(defun string-lessp (s1 s2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((end1 (or end1 (length s1)))
        (end2 (or end2 (length s2))))
    (labels ((compare (i)
               (cond ((= i end2) nil)
                     ((= i end1) i)
                     ((char-lessp (char s1 (+ i start1)) (char s2 (+ i start2))) i)
                     ((char-lessp (char s2 (+ i start2)) (char s1 (+ i start1))) nil)
                     (t (compare (1+ i)))) ))
      (compare 0))))

;;;
;;;    6.8.3
;;;    
(defun merge (l1 l2 f)
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((funcall f (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2 f)))
        (t (cons (first l2) (merge l1 (rest l2) f))) ))

(deftest test-merge ()
  (check
   (equal (merge '(1 2 3) '() #'<) '(1 2 3))
   (equal (merge '() '(1 2 3) #'<) '(1 2 3))
   (equal (merge '(1 3 5) '(2 4 6) #'<) '(1 2 3 4 5 6))
   (equal (merge '(6 4 2) '(5 3 1) #'>) '(6 5 4 3 2 1))
   (equal (merge '(#\a #\c #\e) '(#\b #\d #\f) #'char<) '(#\a #\b #\c #\d #\e #\f))))

(defun merge-sort (l f)
  (cond ((null l) '())
        ((null (rest l)) l)
        (t (partition-and-merge l f '() '()))) )

(defun partition-and-merge (l f l1 l2)
  (if (endp l)
      (merge (merge-sort l1 f) (merge-sort l2 f) f)
      (partition-and-merge (rest l) f (cons (first l) l2) l1)))

(deftest test-merge-sort ()
  (check
   (equal (merge-sort '(2 1 3) #'<) '(1 2 3))
   (equal (merge-sort '(2 1 3) #'>) '(3 2 1))
   (equal (merge-sort (coerce "asdfqwer" 'list) #'char<) '(#\a #\d #\e #\f #\q #\r #\s #\w))
   (equal (merge-sort (coerce "asdfqwer" 'list) #'char>) '(#\w #\s #\r #\q #\f #\e #\d #\a))))

;;;
;;;    6.8.5
;;;    
(defun string-reverse (s)
  (labels ((string-reverse-aux (l result)
             (if (endp l)
                 (coerce result 'string)
                 (string-reverse-aux (rest l) (cons (first l) result)))) )
    (string-reverse-aux (coerce s 'list) '())))

(defun string-reverse (s)
  (let ((result '()))
    (map nil #'(lambda (ch) (push ch result)) s)
    (coerce result 'string)))

(defun string-reverse (s)
  (let ((result '()))
    (dolist (ch (coerce s 'list) (coerce result 'string))
      (push ch result))))

(defun string-reverse (s)
  (loop for ch across s
        with result = '()
        do (push ch result)
        finally (return (coerce result 'string))))

(defun string-reverse (s)
  (let ((length (length s)))
    (do ((i 0 (1+ i))
         (j (1- length) (1- j))
         (result (make-string length)))
        ((= i length) result)
      (setf (char result j) (char s i)))) )

(defun string-reverse (s)
  (let* ((length (length s))
         (result (make-string length))
         (is (loop for i from 0 below length collect i))
         (js (loop for j from length above 0 collect (1- j))))
    (map nil #'(lambda (i j) (setf (char result j) (char s i))) is js)
    result))

(defun string-reverse (s)
  (loop for i from 0 below (length s)
        for j from (1- (length s)) downto 0
        with result = (make-string (length s))
        do (setf (char result j) (char s i))
        finally (return result)))

(defun string-reverse (s)
  (with-output-to-string (result)
    (dotimes (i (length s))
      (write-char (char s (- (length s) (1+ i))) result))))

(defun string-reverse (s)
  (with-output-to-string (result)
    (loop for i from (1- (length s)) downto 0
          do (write-char (char s i) result))))

(defun string-reverse (s)
  (labels ((string-reverse-aux (result i)
             (when (< i (length s))
               (string-reverse-aux result (1+ i))
               (write-char (char s i) result))))
    (with-output-to-string (result)
      (string-reverse-aux result 0))))

;; (length (string-reverse (make-string 10000 :initial-element #\k)))
;; Error: Stack overflow (signal 1000)

(defun string-reverse (s)
  (do ((i 0 (1+ i))
       (j (1- (length s)) (1- j))
       (result (copy-seq s)))
      ((<= j i) result)
    (rotatef (char result i) (char result j))))

(deftest test-string-reverse ()
  (check
   (string= (string-reverse "Is this not pung?") "?gnup ton siht sI")
   (string= (string-reverse "") "")))
