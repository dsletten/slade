;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch07.lisp
;;;
;;;   STARTED:            Mon Nov 26 22:56:32 2001
;;;   MODIFICATIONS:      041015 Fixed MAKE-MATRIX et al. to return (list '())
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

;;;
;;;    7.9.1
;;;
(defun column-print (string-list column stream)
  "Output to a given stream a list of strings indented to the specified column."
  (cond ((null string-list) nil)
        (t (format stream "~VT~A~%" column (car string-list))
           (column-print (cdr string-list) column stream))) )

;;;
;;;    7.9.2
;;;
(defun multi-column-print (l &key (columns 1)
                           width
                           (stream *standard-output*)
                           (indent 0))
  (print-rows stream
              (split-list l (ceiling (length l) columns) columns)
              (or width (truncate (- 80 indent) columns))
              indent) )

;;;
;;;    Recursively print each row in a list of rows (lists).
;;;    Each row is indented by the given amount and finished with a newline.
;;;    
(defun print-rows (stream l width indent)
  (cond ((null l) nil)
        (t (format stream "~V,0T" indent)
           (print-cols stream (car l) width)
           (format stream "~%")
           (print-rows stream (cdr l) width indent))) )

;;;
;;;    Recursively print each element in a row in the proper column. The column
;;;    widths are specified by the WIDTH argument.
;;;
;;;    Slade's version prints WIDTH spaces between columns it doesn't make
;;;    columns of WIDTH size.
;;;    
(defun print-cols (stream cols width)
  (cond ((null cols) nil)
        (t (format stream "~VA" width (car cols))
;   (t (format stream "~A~V,0@T" (car cols) (1- width))
           (print-cols stream (cdr cols) width))) )

(defun print-cols (stream cols width)
  (format stream (format nil "~~{~~~DA~~}" width) cols))

(defun split-list (l n-rows n-cols)
  (transpose (make-matrix-1 l n-rows n-cols)) )

;;;
;;;    Turn a list into a matrix. If the list is stored column major we make
;;;    columns. If it's row major then we make rows. (See also matrix.lisp)
;;;    
;;;    This version is modified to accept arguments indicating both the number
;;;    of rows (M) and the number of columns (C) that the matrix should
;;;    possess.
;;;    
;;;    If the number of elements in the original list is not equal to M * C,
;;;    then we cannot form a rectangular matrix. However, the existing elements
;;;    are spread out so that the number of elements in each column differ by
;;;    one at most.
;;;
;;;    E.g., (make-matrix '(1 2 3 4 5 6 7) 3 3) => 1 4 6 rather than 1 4 7
;;;                                                2 5 7             2 5
;;;                                                3                 3 6
;;;    [Actually (make-matrix '(1 2 3 4 5 6 7) 3 3) => ((1 2 3) (4 5) (6 7))]
;;;
;;;    R represents the number of empty positions in the matrix, the difference
;;;    between the total number (* m c) and the available values (length l).
;;;    C-R is the column beyond which holes will appear in the matrix. I keeps
;;;    track of which column is currently being processed.
;;;                                                
;;;    The first version below performs horrendously! Because of the doubly
;;;    recursive call, each new element added to the list doubles the amount
;;;    work done. The runtime is a function of 2^n, where n is the number of
;;;    elements in the original list.
;;;
;;;    The second version caches the intermediate value before using it in
;;;    subsequent CONSes. As a result runtime is a function of n.
;;;    
(defun make-matrix (l m c)
  (let* ((r (- (* m c) (length l)))
         (c-r (- c r)))
    (labels ((make-matrix-aux (l m1 i)
               (cond ((null l) (list '()))
                     ((or (zerop m1) 
                          (and (> i c-r)
                               (= m1 1)))
                      (cons '() (make-matrix-aux l m (1+ i))))
                     (t (cons (cons (car l)
                                    (car
                                     (make-matrix-aux (cdr l) (1- m1) i)))
                              (cdr (make-matrix-aux (cdr l) (1- m1) i))) ))) )
      (make-matrix-aux l m 1))) )

(defun make-matrix-1 (l m c)
  (let* ((r (- (* m c) (length l)))
         (c-r (- c r)))
    (labels ((make-matrix-aux (l m1 i)
               (cond ((null l) (list '()))
                     ((or (zerop m1) 
                          (and (> i c-r)
                               (= m1 1)))
                      (cons '() (make-matrix-aux l m (1+ i))))
                     (t (let ((cdr-matrix (make-matrix-aux
                                           (cdr l) (1- m1) i)))
                          (cons (cons (car l) (car cdr-matrix))
                                (cdr cdr-matrix)))) )))
      (make-matrix-aux l m 1))) )

(defun make-matrix-2 (l m c)
  (let* ((r (- (* m c) (length l)))
         (c-r (- c r)))
    (labels ((make-matrix-aux (l m1 i)
               (cond ((null l) (list '()))
                     ((or (zerop m1) 
                          (and (> i c-r)
                               (= m1 1)))
                      (cons '() (make-matrix-aux l m (1+ i))))
                     (t (make-matrix-aux-1
                         (car l)
                         (make-matrix-aux (cdr l) (1- m1) i)))) )
             (make-matrix-aux-1 (elt l)
               (cons (cons elt (car l)) (cdr l))) )
      (make-matrix-aux l m 1))) )

;;;
;;;    Transform a list of M lists each of length N into a list of N lists
;;;    each of list M.
;;;    
(defun transpose (matrix)
  (cond ((every #'null matrix) '())
        (t (cons (get-column matrix) (transpose (remove-column matrix)))) ) )

;;;
;;;    Form a list of the CARs of each top-level list in a list of lists.
;;;    
;; (defun get-column (matrix)
;;   (cond ((null (car matrix)) '());;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compare matrix.lisp
;;  (t (cons (caar matrix) (get-column (cdr matrix)))) ) )
(defun get-column (l)
  (cond ((null l) '())
        ((null (car l)) (get-column (cdr l))) ;Handle incomplete columns.
        (t (cons (caar l) (get-column (cdr l)))) ))

;;;
;;;    Form a list of lists by removing the CAR of each top-level list in the
;;;    original list of lists.
;;;    (Complement of get-column above.)
;;;    
(defun remove-column (matrix)
  (cond ((null matrix) '())
        (t (cons (cdar matrix) (remove-column (cdr matrix)))) ) )

;;;
;;;    7.9.3
;;;
; (defun slade-split (left-text right-text &optional (stream *standard-output*))
;   (flushleft left-text stream)
;   (write-spaces (- (line-length stream)
;          (+ (hpos stream)
;             (length right-text)))
;       stream)
;   (write-string right-text stream)
;   (values) )

; (defun write-spaces (count &optional (stream *standard-output*))
;   (cond ((zerop count) nil)
;   (t (write-char #\Space stream)
;      (write-spaces (1- count) stream))) )

; ; (defun write-spaces (count &optional (stream *standard-output*)) ;;Avoids gratuitous recursion
; ;   (write (make-string count :initial-element #\Space)) )

; (defun flushleft (text &optional (stream *standard-output*))
;   (indent text 0 stream) )

; (defun indent (text indentation &optional (stream *standard-output*))
;   (terpri stream)
;   (write-spaces indentation stream)
;   (write-string text stream)
;   (values) )

; (defun line-length (&optional (stream *standard-output*))
;   (or lisp:*print-right-margin*
; ;      (stream:stream-output-width *standard-output*)
; ;      excl::*default-right-margin*
;       79) )

; (defun hpos (&optional (stream *standard-output*))
;   (stream:stream-line-column stream) )

;;;
;;;    7.9.5
;;;
(defun romans (cap n-digits &optional (i 1) result)
  (cond ((> i cap) result)
        ((= (length (decimal-to-roman i)) n-digits)
         (romans cap n-digits (1+ i) (cons (decimal-to-roman i) result)))
        (t (romans cap n-digits (1+ i) result))) )

(defun decimal-to-roman (d)             ;Cheap!!!
  (format nil "~@R" d) )

;;;
;;;    This is the hard way...
;;;    
; (defun decimal-to-roman (d)
;   (coerce (decimal-to-roman-aux d) 'string) )

; (defun decimal-to-roman-aux (d)
;   (

;;;
;;;    7.9.8
;;;
;;;    Control string can only reference a single arg. Rest of control string
;;;    should be output literally.
;;;
;;;    What about numeric args, e.g., ~3% ?
;;;    
(let ((format-functions '(("~S" prin1 t)
                          ("~A" princ t)
                          ("~%" write-line)
                          ("~&" fresh-line)
                          ("~D" print-decimal t)
                          ("~B" print-binary t)
                          ("~O" print-octal t)
                          ("~X" print-hex t)
                          ("~P" print-plural t))))
  (defun my-format (stream control arg)
    (let ((i (position #\~ control)))
      (cond ((null i) (write-string control stream))
            (t (write-string (subseq control 0 i) stream)
               (let ((ff (assoc (subseq control i (+ i 2))
                                format-functions
                                :test #'string-equal)))
                 (apply (second ff) (and (third ff) arg) (list stream)))
               (my-format stream (subseq control (+ i 2)) arg)))) ))

(defun print-binary (num stream)
  (write num :stream stream :base 2) )

(defun print-octal (num stream)
  (write num :stream stream :base 8) )

(defun print-decimal (num stream)
  (write num :stream stream :base 10) )

(defun print-hex (num stream)
  (write num :stream stream :base 16) )

(defun print-plural (num stream)
  (if (/= num 1)
      (princ "s" stream)) )

;;;
;;;    7.9.10
;;;    
(defun revise-file (input output tag text)
  (with-open-file (in input :if-does-not-exist nil)
    (cond ((null in) (format *error-output* "Input file '~A' does not exist.~%" input))
          (t (with-open-file (out output
                                  :direction :output
                                  :if-exists :supersede)
               (do ((s (read-line in nil nil)
                       (read-line in nil nil)))
                   ((null s))
                 (cond ((search tag s)
                        (let ((s1 (process-line s tag text)))
                          (format t "old: ~A~%" s)
                          (format t "new: ~A~2%" s1)
                          (format out "~A~%" s1)))
                       (t (format out "~A~%" s)))) )))) )

(defun process-line (s tag text)
  (let ((i (search tag s)))
    (cond ((null i) s)
          (t (concatenate 'string
                          (subseq s 0 i)
                          text
                          (process-line (subseq s (+ i (length tag))) tag text)))) ) )

;;;
;;;    7.9.11
;;;
;;;;;;;;;;;;;(load "/Users/dsletten/lisp/programs/utils.lisp")
(defun compare-file (file-1 file-2)
  (cond ((equal file-1 file-2) (warn "Do you take me for an idiot?~%"))
        (t (with-open-file (stream-1 file-1 :if-does-not-exist nil)
             (cond ((null stream-1) (warn "Input file '~A' does not exist.~%" file-1))
                   (t (with-open-file (stream-2 file-2 :if-does-not-exist nil)
                        (cond ((null stream-2) (warn "Input file '~A' does not exist.~%" file-2))
                              (t (do ((l1 (read-line stream-1 nil nil)
                                          (read-line stream-1 nil nil))
                                      (l2 (read-line stream-2 nil nil)
                                          (read-line stream-2 nil nil)))
                                     ((or (null l1) (null l2)))
                                   (unless (equal l1 l2)
                                     (format t "1: ~A~%" l1)
                                     (format t "2: ~A~2%" l2)))
                                 (format t "*END-OF-FILE-COMPARE*~%")))) )))) ) )

;;;
;;;    7.9.12
;;;
#|
(defun filesort (input-file output-file &rest sort-args)
  (let ((sort-1 (first (first sort-args)))
        (sort-2 (second (first sort-args))))
    (let* ((field-1-start (first sort-1))
           (field-1-end (+ field-1-start (second sort-1)))
           (test-1 (third sort-1))
           (field-2-start (first sort-2))
           (field-2-end (+ field-2-start (second sort-2)))
           (test-2 (third sort-2)))
      (with-open-file (in-stream input-file :if-does-not-exist nil)
        (cond ((null in-stream) (warn "Input file '~A' does not exist.~%" input-file))
              (t (let ((in-lines '()))
                   (do ((line (read-line in-stream nil nil)
                              (read-line in-stream nil nil)))
                       ((null line))
                     (push line in-lines))
                   (let ((sorted (sort in-lines
                                       #'(lambda (a b)
                                           (cond ((funcall test-1
                                                           (subseq a field-1-start field-1-end)
                                                           (subseq b field-1-start field-1-end)) a)
                                                 ((funcall test-1
                                                           (subseq b field-1-start field-1-end)
                                                           (subseq a field-1-start field-1-end)) b)
                                                 ((funcall test-2
                                                           (subseq a field-2-start field-2-end)
                                                           (subseq b field-2-start field-2-end)) a)
                                                 (t b)))) ))
                     (dolist (line sorted)
                       (write-line line)))) )))) ) )




;      (with-open-file (out-stream output-file
;                    :direction :output
;                    :if-exists :supersede)
;          (


(defun my-sort (l)
  (let ((test-1 'string-lessp)
        (field-1-start 4)
        (field-1-end 7))
    (sort (copy-tree l)
          #'(lambda (a b)
              (cond ((funcall test-1
                              (subseq a field-1-start field-1-end)
                              (subseq b field-1-start field-1-end)) a)
                    ;;          ((funcall test-1
                    ;;                (subseq b field-1-start field-1-end)
                    ;;                (subseq a field-1-start field-1-end)) b)
                    ;;          ((funcall test-2
                    ;;                (subseq a field-2-start field-2-end)
                    ;;                (subseq b field-2-start field-2-end)) a)
                    ;;          (t b)))) )
                    )))) )
|#

(defun filesort-1 (input-file sort-list)
  (let ((start (first sort-list))
    (end (second sort-list))
    (test (third sort-list)))
    (with-open-file (in-stream input-file
                   :if-does-not-exist nil)
      (cond ((null in-stream) (warn "File '~A' does not exist.~%" input-file))
        (t (let ((lines '()))
         (do ((line (read-line in-stream nil nil)
                (read-line in-stream nil nil)))
             ((null line))
           (push line lines))
         (let ((sorted (sort (copy-list lines)
                     #'(lambda (a b)
                     (funcall (eval test)
                          (subseq a start end)
                          (subseq b start end)))) ))
           sorted)))) )) )

(defun filesort-2 (input-file sort-list)
  (let* ((start-1 (first (first sort-list)))
     (end-1 (+ start-1 (second (first sort-list))))
     (test-1 (third (first sort-list)))
     (start-2 (first (second sort-list)))
     (end-2 (+ start-2 (second (second sort-list))))
     (test-2 (third (second sort-list))))
    (with-open-file (in-stream input-file
                   :if-does-not-exist nil)
      (cond
    ((null in-stream) (warn "File '~A' does not exist.~%" input-file))
    (t (let ((lines '()))
         (do ((line (read-line in-stream nil nil)
            (read-line in-stream nil nil)))
         ((null line))
           (push line lines))
         (let ((sorted
            (sort (copy-list lines)
              #'(lambda (a b)
                  (cond ((string= (subseq a start-1 end-1)
                          (subseq b start-1 end-1))
                     (funcall (eval test-2)
                          (subseq a start-2 end-2)
                          (subseq b start-2 end-2)))
                    (t
                     (funcall (eval test-1)
                          (subseq a start-1 end-1)
                          (subseq b start-1 end-1)))) ))))
           sorted)))) )) )
#|
* (filesort-2 "names_space.txt" '((11 5 #'string<) (0 7 #'string<)))

("Louise     Brown    782-3299    43 Oak Drive"
 "Susan      Brown    889-4321    789 Maple Street"
 "Mary       Jones    345-9090    123 Pine Street"
 "Deborah    Smith    782-1234    456 Elm Street, 12-B"
 "Jane       Smith    345-7766    1212 Grove Terrace"
 "Mary       White    889-3758    321 Avenue of Trees")
|#

(defun read-etc-passwd ()
  (let ((passwd '()))
    (with-open-file (in-stream "/etc/passwd")
      (do ((line (read-line in-stream nil nil)
         (read-line in-stream nil nil)))
      ((null line))
    (push line passwd)))
    passwd) )

(let ((numeric-fields '(2 3)))
  (defun sort-etc-passwd (field)
    (let ((sort-test (if (member field numeric-fields)
             #'(lambda (a b) (< (parse-integer a)
                        (parse-integer b)))
             #'string<)))
      (mapcar #'second (sort (mapcar #'(lambda (line)
                     (list (nth field (split line #\:))
                           line))
                     (read-etc-passwd))
                 #'(lambda (a b)
                 (funcall sort-test
                      (car a)
                      (car b)))) )) ))
