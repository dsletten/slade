;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Tue Aug 17 02:39:01 2010
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
(load "/Users/dsletten/lisp/packages/io")
(load "/Users/dsletten/lisp/packages/time")

(defpackage ch07 (:use common-lisp test io))

(in-package ch07)

;;;
;;;    7.9.1
;;;
;;;    SBCL screws up first line?!
;;;    
;; (defun column-print (l column &optional (stream *standard-output*))
;;   (cond ((endp l) nil)
;;         (t (format stream "~VT~A~%" column (first l))
;;            (column-print (rest l) column stream))))

;;;
;;;    This works in both SBCL/Clozure
;;;    
(defun column-print (l column &optional (stream *standard-output*))
  (cond ((endp l) nil)
        (t (format stream "~V@A~A~%" column " " (first l))
           (column-print (rest l) column stream))))

;;;
;;;    So does this.
;;;    
(defun column-print (l column &optional (stream *standard-output*))
  (cond ((endp l) nil)
        (t (format stream "~V@A~%" (+ column (length (first l))) (first l))
           (column-print (rest l) column stream))))

;;;
;;;    So does this!!
;;;    
(defun column-print (l column &optional (stream *standard-output*))
  (cond ((endp l) nil)
        (t (format stream "~V@T~A~%" column (first l))
           (column-print (rest l) column stream))))

;;;
;;;    7.9.2
;;;
;; (defun sublists (l count)
;;   (sublists-aux l count (make-list count :initial-element '())))

;; (defun sublists-aux ())


;;;
;;;    Split a list into sublists of length COUNT. The final
;;;    sublist will either have COUNT elements or (mod (length l) count)
;;;
;;;    See lang:take-drop
;;;    (loop for l = list then (take-drop l count)
;;;          until (null l)
;;;          collect (take-drop l count))   .....
;;;
;;;
(defun sublists-1 (l count)
  (loop for tail on l by #'(lambda (l) (nthcdr count l))
        collect (collect-elts-1 tail count)))

(defun collect-elts-1 (l count)
  (loop for elt in l
        repeat count
        collect elt))


;;;
;;;    Complicated mutual recursion.
;;;    
(defun sublists-2 (l count)
  (cond ((endp l) '())
        (t (collect-elts-2 l count count))))

(defun collect-elts-2 (l count i)
  (cond ((or (endp l) (zerop count)) (cons '() (sublists-2 l i)))
        (t (insert-2 (first l) (collect-elts-2 (rest l) (1- count) i)))) )

(defun insert-2 (elt result)
  (cons (cons elt (first result)) (rest result)))


(defun transpose-2 (l column-height)
  (apply #'mapcar #'list (sublists-2 l column-height)))

;; (defun collect-elts (l count i)
;;   (cond ((or (endp l) (zerop count)) (cons '() (sublists l i)))
;;         (t (let ((result (collect-elts (rest l) (1- count) i)))
;;              (cons (cons (first l) (first result)) (rest result)))) ))

(defun sublists (l count length)
  (cond ((endp l) (values '() length))
        (t (collect-elts l count count length))))

(defun collect-elts (l count i length)
  (cond ((or (endp l) (zerop count))
         (multiple-value-bind (result length)
             (sublists l i length)
           (values (cons '() result) (1+ length))))
        (t (multiple-value-bind (result length)
               (collect-elts (rest l) (1- count) i length)
             (values (insert (first l) result) length)))) )

(defun transpose (l columns)
  (multiple-value-bind (result length)
      (sublists l columns 0)
    (apply #'mapcar #'list result)))


(defun make-table (vec columns)
  (let* ((rows (ceiling (length vec) columns))
         (matrix (make-array (list rows columns) :initial-element nil))
         (full-columns (- columns (- (* rows columns) (length vec)))) )
    (loop for j from 0 below columns
          with index = 0 do
          (loop for i from 0 below (if (< j full-columns) rows (1- rows))
                when (< index (length vec)) do
                (setf (aref matrix i j) (aref vec index))
                end
                do (incf index)))
    matrix))

(defun multi-column-print (vec &key (columns 1) (width (truncate 80 columns)) (indent 0) (stream *standard-output*))
  (let* ((matrix (make-table vec columns))
         (rows (array-dimension matrix 0)))
    (loop for i from 0 below rows do
          (format stream "~VA" indent "")
          (loop for j from 0 below columns
                when (aref matrix i j)
                do (format stream "~VA" width (aref matrix i j))
;                do (format stream "~V@A" width it)
                end)
          (format stream "~%"))))

;;;
;;;    Goal:
;;;    Rearrange list into new list of elts in correct order for column printing without:
;;;    -Using lists of lists as above
;;;    -Traversing original list more than once (e.g., no LENGTH)
;;;
;;;    Example: (1 2 3 4 5 6) => (1 4 2 5 3 6) for 2 columns:
;;;    1 4
;;;    2 5
;;;    3 6
;;;    
(cons 1 (catch '1a
          (cons 2 (catch '2a
                    (throw '1b (cons 3 (catch '3a
                                         (throw '2b (cons 4 (catch '1b
                                                              (throw '1a (cons 5 (catch '2b
                                                                                   (throw '2a (cons 6 '()))) )))) )))) ))))
;;;
;;;    This _kind of_ works, but we have to have advance knowledge of length to determine sequence 1a, 1b, 1c.
;;;    (i.e., (/ length columns))
;;;    
(let ((result '()))
  (tagbody
     (go 2c)
   1a (push 1 result) (go done)
   1b (push 2 result) (go 2a)
   1c (push 3 result) (go 2b)
   2a (push 4 result) (go 1a)
   2b (push 5 result) (go 1b)
   2c (push 6 result) (go 1c)
   done)
  result)

;;;
;;;    7.9.10
;;;
(defun replace-template-1 (s template replacement &key (start 0))
  (let ((match (search template s :start2 start)))
    (if match
        (concatenate 'string
                     (subseq s start match)
                     replacement
                     (replace-template-1 s template replacement :start (+ match (length template))))
        (subseq s start))))

(defun replace-template (s template replacement)
  (with-output-to-string (result)
    (let ((length (length template)))
      (loop for index = 0 then (+ match length)
            for match = (search template s) then (search template s :start2 (+ match length))
            while match
            do (write-string (subseq s index match) result)
               (write-string replacement result)
            finally (write-string (subseq s index) result)))) )

(deftest test-replace-template ()
  (check
   (equal (replace-template "*x**x* Is this *x* pung?" "*x*" "not") "notnot Is this not pung?")
   (equal (replace-template "*GIFT*? Every time we look at the *GIFT*" "*GIFT*" "keyboard duster")
          "keyboard duster? Every time we look at the keyboard duster")
   (equal (replace-template "*GIFT*? Every time we look at the *GIFT*" "*x*" "keyboard duster")
          "*GIFT*? Every time we look at the *GIFT*")))

(defun report (stream old new)
  (terpri)
  (format t "old: ~A~%" old)
  (format t "new: ~A~%" new)
  (write-line new stream))

(defun revise-file (input output template replacement)
  (with-open-file (in input :if-does-not-exist nil)
    (if in
        (with-open-file (out output :direction :output :if-exists :supersede)
          (if out
              (loop for line = (read-line in nil nil)
                    while line
                    do (report out line (replace-template line template replacement))
                    finally (format t "*END-OF-FILE-REVISION*~%"))
              (warn "Output file could not be created.~%")))
        (warn "Input file does not exist.~%"))))

;;;
;;;    7.9.11
;;;
(defun report-line (file-number line)
  (format t "~D: \"~A\"~%" file-number line))

(defun report-diff (line1 line2)
  (cond ((null line1)
         (terpri)
         (report-line 2 line2))
        ((null line2)
         (terpri)
         (report-line 1 line1))
        ((string/= line1 line2)
         (terpri)
         (report-line 1 line1)
         (report-line 2 line2))))

(defun compare-files (file1 file2)
  (with-open-file (in1 file1 :if-does-not-exist nil)
    (if in1
        (with-open-file (in2 file2 :if-does-not-exist nil)
          (if in2
              (loop for line1 = (read-line in1 nil nil)
                    for line2 = (read-line in2 nil nil)
                    while (or line1 line2)
                    do (report-diff line1 line2)
                    finally (format t "*END-OF-FILE-COMPARISON*~%"))
              (warn "File ~A does not exist.~%" file2)))
        (warn "File ~A does not exist.~%" file1))))

;;;
;;;    7.9.12
;;;
(defun extract-sort-criteria (criteria-list)
  (loop for criterion in criteria-list
        when (fourth criterion) 
        collect (list (first criterion) (second criterion) (fourth criterion)) into field-markers
        else
        collect (list (first criterion) (second criterion)) into field-markers
        end
        collect (third criterion) into tests
        finally (return (values field-markers tests))))

(defun has-key-p (field-marker)
  (not (null (key field-marker))))

(defun start (field-marker)
  (first field-marker))

(defun end (field-marker)
  (second field-marker))

(defun key (field-marker)
  (third field-marker))

(defun process-lines (lines field-markers)
  (labels ((process-line (line)
             (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
               (vector-push-extend line v) ; Capture original line
               (dolist (field-marker field-markers) ; Split line into sortable fields (KEY function may convert string)
                 (let ((field (subseq line
                                      (start field-marker)
                                      (+ (start field-marker) (end field-marker)))) )
                   (if (has-key-p field-marker)
                       (vector-push-extend (funcall (key field-marker) field) v)
                       (vector-push-extend field v))))
               v)))
    (mapcar #'process-line lines)))

;;;
;;;    This employs a Schwartzian transform MAP/SORT/MAP, but the initial
;;;    MAP(CAR) does substantial processing to split each line into relevant fields.
;;;    
(defun filesort (input output sort-criteria)
  (let ((lines (read-file input)))
    (multiple-value-bind (field-markers tests)
        (extract-sort-criteria sort-criteria)
      (let ((unsorted (process-lines lines field-markers)))
;        (print unsorted)
        (write-file output (mapcar #'(lambda (elt) (aref elt 0)) ; Extract each original line...
                                   (sort unsorted #'(lambda (a b) (evaluate-tests a b tests)))) ; ...after sorting
                    t)))) )

;;;
;;;    A and B are vectors of fields each representing a record to be sorted. The first elt of each is the original line from the file.
;;;    All of the other elts contain fields from the original line to be compared by the tests in TESTS.
;;;    If any test F is true for (f a b), then A should be sorted before B (A is "less than" B). Conversely, if
;;;    (f b a) is true, then B should be sorted before A (B is "less than" A). Otherwise, A and B are considered
;;;    "equal" in terms of that criterion, and the next test is used to further discriminate.
;;;    
(defun evaluate-tests (a b tests)
  (loop for test in tests
        for ai across (subseq a 1)
        for bi across (subseq b 1)
        when (funcall test ai bi)
        do (return t)
        end
        when (funcall test bi ai)
        do (return nil)
        end
        finally (return t))) ; Both elements are equal in every respect. Tests exhausted...

(defun evaluate-tests (a b tests)
  (labels ((evaluate-test (i tests)
             (if (endp tests)
                 t ; Both elements are equal in every respect. Tests exhausted...
                 (let ((test (first tests))
                       (ai (aref a i))
                       (bi (aref b i)))
                   (cond ((funcall test ai bi) t)
                         ((funcall test bi ai) nil)
                         (t (evaluate-test (1+ i) (rest tests)))) ))))
    (evaluate-test 1 tests)))

(defun evaluate-tests (a b tests)
  (assert (not (null tests)))
  (do* ((i 1 (1+ i))
        (criterion (first tests) (first criteria)) ; This must come before update of CRITERIA!
        (criteria (rest tests) (rest criteria))
        (ai (aref a i) (aref a i))
        (bi (aref b i) (aref b i)))
       ((endp criteria) (or (funcall criterion ai bi) (funcall criterion bi ai))) ; If final test indicates equality => NIL!
    (cond ((funcall criterion ai bi) (return t))
          ((funcall criterion bi ai) (return nil)))) )

(defun month-order (a b)
  (< (position a time:short-month-names :test #'string=)
     (position b time:short-month-names :test #'string=)))

;;;
;;;    Doesn't work for recent files (current year). Format is different:
;;;    -rwxr-xr-x  1 dsletten  dsletten  11275 Aug 23 15:34 ch07.lisp
;;;    vs.
;;;    -rw-r--r--@  1 dsletten  dsletten   2001 Oct  7  2004 ch07.lisp

;(filesort "/Users/dsletten/lisp/books/Slade/ch07/2010/ls.txt" "/Users/dsletten/lisp/books/Slade/ch07/2010/ls.out" '((49 4 < parse-integer) (41 3 month-order) (45 2 < parse-integer)))

;; (defvar *people* '(#("Jones, Mary 28" "Jones" "Mary" 28) #("Jones, Mary 19" "Jones" "Mary" 19) #("Johnson, Sue 40" "Johnson" "Sue" 4)))
;; (sort *people* #'(lambda (a b) (evaluate-tests a b (list #'string< #'string< #'>))))
;; (#("Johnson, Sue 40" "Johnson" "Sue" 4) #("Jones, Mary 28" "Jones" "Mary" 28) #("Jones, Mary 19" "Jones" "Mary" 19))
;; (setf *people* *)
;; (#("Johnson, Sue 40" "Johnson" "Sue" 4) #("Jones, Mary 28" "Jones" "Mary" 28) #("Jones, Mary 19" "Jones" "Mary" 19))
;; (sort (copy-list *people*) #'(lambda (a b) (evaluate-tests a b (list #'string> #'string> #'<))))
;; ("Jones, Mary 19" "Jones" "Mary" 19) #("Jones, Mary 28" "Jones" "Mary" 28) #("Johnson, Sue 40" "Johnson" "Sue" 4))
