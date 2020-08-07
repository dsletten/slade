;#!/usr/local/bin/clisp

;;
;   NAME:               matrix.lisp
;
;   STARTED:            011128
;   MODIFICATIONS:      020627 Added necessary test for (zerop x) based on
;                              Graham's 'On Lisp' version.
;                       041011 Added MAKE-MATRIX-2A, GET-COLUMN-2
;
;   PURPOSE:
;       Various functions to manipulate a list into rows and columns.
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES: See ch07.lisp (Slade) on how to deal with non-rectangular (ragged)
;          matrices.
;
;   111018
;   All of the MAKE-MATRIX functions accomplish the same task. They just use
;   different strategies to do so.
;   1.  MAKE-MATRIX is the naive version, but it is very inefficient since it
;       computes the same values twice.
;   2.  MAKE-MATRIX-2 fixes MAKE-MATRIX by using an auxiliary function to further
;       manipulate the result computed once. MAKE-MATRIX-2-2011 merely fixes the
;       conceptual problem with the outer container for the entire matrix. When
;       the end of the input list is reached we need to provide a list in which
;       to store the final column as well as a list for the matrix itself.
;   2a. MAKE-MATRIX-2A simply uses a local variable to capture the result.
;   3.  MAKE-MATRIX-3 actually veers in the wrong direction, now checking whether
;       the end of a column has been reached first. This results in MAKE-MATRIX-3A
;       being unnecessarily complicated.
;   4.  This is simply MAKE-MATRIX-3 with a different auxiliary function that
;       tries to avoid all of the CONSing of the earlier version. But it looks
;       kind of weird relying on PROGN and PUSH.
;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :matrix (:use :common-lisp :test))

(in-package :matrix)

;;;
;;;    Turn a list into a matrix. If the list is stored column major we make
;;;    columns. If it's row major then we make rows.
;;;
;;;    We CDR down the list and build the matrix back up starting at the end.
;;;    At the end of the list we provide the outer list that contains the
;;;    entire structure. Then we start CONSing each row (or column!) and its
;;;    elements working our way back to the front.
;;;
;;;    In the original forward pass, each time we handle X elements we are at
;;;    the end of a row/column. We CONS in an empty list that will later hold
;;;    all of the elements for that row/column.
;;;
;;;    This works with the test (null l) before the test (zerop n) since in
;;;    Common Lisp, (car '()) is (). Otherwise the (zerop n) test would have to
;;;    come first to create an empty list for the final sublist. Compare
;;;    MAKE-MATRIX-3. (MAKE-MATRIX-3 performs like MAKE-MATRIX-2, but
;;;    consistently requires 8 bytes more!)
;;;
;;;    [The above paragraph is nonsense. By checking for the end of L first
;;;     and returning both the top-level list and the empty list for the final
;;;     sublist: (()) we can handle both rectangular and ragged matrices since
;;;     only the final sublist will potentially be short. Placing the ZEROP
;;;     test second handles all of the other sublists. See MAKE-MATRIX-2-2011
;;;     (or the identical LIST-TO-COLUMNS) below.]
;;;
;;;    MAKE-MATRIX performs horribly due to its doubly-recursive nature.
;;;    MAKE-MATRIX-2 uses an auxiliary function to cache results and thus
;;;    performs much better than MAKE-MATRIX. See the bottom of this file for
;;;    performance comparison.
;;;    
(defun make-matrix (l x)
  "Transform the one-dimensional list L into a two-dimensional list of lists each containing X elements."
  (labels ((make-matrix-aux (l n)
             (cond ((null l) '())
                   ((zerop n) (cons '() (make-matrix-aux l x)))
                   (t (cons (cons (car l)
                                  (car (make-matrix-aux (cdr l) (1- n))))
                            (cdr (make-matrix-aux (cdr l) (1- n)))) ))))
    (make-matrix-aux l x)) )

(defun make-matrix-2 (l x)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (labels ((make-matrix-aux (l n)
             (cond ((null l) '())
                   ((zerop n) (cons '() (make-matrix-aux l x)))
                   (t (make-matrix-aux-2 (car l)
                                         (make-matrix-aux (cdr l) (1- n)))) ) )
           (make-matrix-aux-2 (elt sub-result)
             (cons (cons elt (car sub-result))
                   (cdr sub-result)) ))
    (make-matrix-aux l x)) )

;;;
;;;    This does what MAKE-MATRIX-3A does but in a simpler way.
;;;    
(defun make-matrix-2-2011 (l x)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (labels ((make-matrix-aux (l n)
             (cond ((null l) (list '())) ; <---- Only difference from MAKE-MATRIX-2?!?
                   ((zerop n) (cons '() (make-matrix-aux l x)))
                   (t (make-matrix-aux-2 (car l)
                                         (make-matrix-aux (cdr l) (1- n)))) ) )
           (make-matrix-aux-2 (elt sub-result)
             (cons (cons elt (car sub-result))
                   (cdr sub-result)) ))
    (make-matrix-aux l x)) )

;;;
;;;    See Slade ch. 7 2011. This is effectively the same as MAKE-MATRIX-2-2011 above.
;;;    
(defun list-to-columns (l x)
  (labels ((make-matrix-aux (l n)
             (cond ((endp l) '(()))
                   ((zerop n) (add-new-col (make-matrix-aux l x)))
                   (t (add-elt-to-result (first l) (make-matrix-aux (rest l) (1- n)))) ))
           (add-elt-to-result (elt result)
             (cons (cons elt (first result)) (rest result)))
           (add-new-col (result)
             (cons '() result)))
    (make-matrix-aux l x)))

;;;
;;;    This accomplishes the same thing as MAKE-MATRIX-2 by caching the
;;;    intermediate result.
;;;
;;;    This also returns the correct result: (()) when the end of input list is
;;;    reached. This is the enclosing list for the whole 'matrix' as well as
;;;    the list for the contents of the final row/column.
;;;
(defun make-matrix-2a (l n)
  (labels ((make-matrix-aux (l i)
             (cond ((null l) (list '()))
                   ((zerop i) (cons '() (make-matrix-aux l n)))
                   (t (let ((result (make-matrix-aux (rest l)
                                                     (1- i))))
                        (cons (cons (first l)
                                    (first result))
                              (rest result)))) )))
    (make-matrix-aux l n)))

(defun make-matrix-3 (l x)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (if (zerop x) (error "Invalid length."))
  (labels ((make-matrix-aux (l n)
             (cond ((zerop n) (cons '() (make-matrix-aux l x)))
                   ((null l) '())
                   (t (make-matrix-aux-2 (car l)
                                         (make-matrix-aux (cdr l) (1- n)))) ) )
           (make-matrix-aux-2 (elt sub-result)
             (cons (cons elt (car sub-result))
                   (cdr sub-result)) ))
    (make-matrix-aux l x)) )

(defun make-matrix-4 (l x)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (if (zerop x) (error "Invalid length."))
  (labels ((make-matrix-aux (l n)
             (cond ((zerop n) (cons '() (make-matrix-aux l x)))
                   ((null l) '())
                   (t (make-matrix-aux-2 (car l)
                                         (make-matrix-aux (cdr l) (1- n)))) ) )
           (make-matrix-aux-2 (elt sub-result)
             (if (null (car sub-result))
                 (cons (list elt) (cdr sub-result))
                 (progn (push elt (car sub-result))
                        sub-result))) )
    (make-matrix-aux l x)) )

;;;
;;;    This variant of make-matrix-3 doesn't rely on the fact that (car '())
;;;    and (cdr '()) are both nil. This is an issue if the length of the
;;;    original list is not an exact multiple of X. In such a case, the end of
;;;    the list is reached before N counts down to 0 for the final row. As a
;;;    result, the (zerop n) test is not triggered to insert an empty sublist
;;;    for the row. The version of make-matrix-3 above still works since the
;;;    final call to make-matrix-aux-2 sends in () as the value of SUB-RESULT.
;;;    The function then CONSes ELT onto the CAR of SUB-RESULT and then CONSes
;;;    this onto the CDR. The result is exactly the same as if SUB-RESULT were
;;;    the proper (()), which is what make-matrix-3a produces.
;;;    
(defun make-matrix-3a (l x)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (if (zerop x) (error "Invalid length."))
  (labels ((make-matrix-aux (l n)
             (cond ((zerop n) (cons '() (make-matrix-aux l x)))
                   ((null l) (if (= n x) '() (list '())))
                   (t (make-matrix-aux-2 (car l)
                                         (make-matrix-aux (cdr l) (1- n)))) ) )
           (make-matrix-aux-2 (elt sub-result)
             (cons (cons elt (car sub-result))
                   (cdr sub-result)) ))
    (make-matrix-aux l x)) )

;;;
;;;    Cheap way. Doesn't quite work if last column is incomplete. MAPCAR stops
;;;    too soon.
;;;    
(defun transpose-map (l x)
  "Transform a given list into a list of columns each containing the specified number of elements."
  (apply #'mapcar #'list (make-matrix l x)) )

;;;
;;;    Purer recursive way.
;;;    This doesn't really make 'columns'. The distinction between columns and
;;;    rows is merely based on the way the original list is viewed. If the
;;;    original list is row major, then this function makes columns. If the
;;;    original list is column major, then this function makes rows. It really
;;;    just converts the list into a matrix and then transposes it. The
;;;    TRANSPOSE function is the interesting part.
;;;    
(defun make-columns (l x)
  "Transform a given list into a list of columns each containing the specified number of elements."
  (transpose (make-matrix l x)) )

;;;
;;;    Transform a list of M lists each of length N into a list of N lists
;;;    each of list M.
;;;    
(defun transpose (matrix)
  (cond ((null (car matrix)) '())       ;(every #'null matrix) ???
        (t (cons (get-column matrix) (transpose (remove-column matrix)))) ) )

;;;
;;;    Form a list of the CARs of each top-level list in a list of lists.
;;;    
(defun get-column (matrix)
  (cond ((null matrix) '())
        (t (cons (caar matrix) (get-column (cdr matrix)))) ) )

(defun get-column-2 (l)
  (cond ((null l) '())
        ((null (car l)) (get-column-2 (cdr l))) ;Handle incomplete columns.
        (t (cons (caar l) (get-column-2 (cdr l)))) ))

;;;
;;;    Form a list of lists by removing the CAR of each top-level list in the
;;;    original list of lists.
;;;    (Complement of get-column above.)
;;;    
(defun remove-column (matrix)
  (cond ((null matrix) '())
        (t (cons (cdar matrix) (remove-column (cdr matrix)))) ) )

;;;
;;;    Transpose all in one.
;;;    (This is no faster than TRANSPOSE above. In fact, it conses more!)
;;;    
(defun transpose-1 (matrix)
  (cond ((null (car matrix)) '())
        (t (let ((l (strip-matrix matrix)))
             (cons (car l) (transpose-1 (cadr l)))) )) )

(defun strip-matrix (matrix &optional caars cdars)
  (cond ((null matrix) (list (reverse caars)
                             (reverse cdars)))
        (t (strip-matrix (cdr matrix)
                         (cons (caar matrix) caars)
                         (cons (cdar matrix) cdars)))) )
			 
(defun transpose-loop (matrix)
  (if (null (first matrix))
      '()
      (loop for row in matrix
            until (null row)
            collect (first row) into firsts
            collect (rest row) into rests
            finally (return (cons firsts (transpose-loop rests)))) ))

(defun transpose-2 (matrix)
  (if (null (first matrix))
      '()
      (multiple-value-bind (firsts rests) (firsts-rests matrix)
        (cons firsts (transpose-2 rests)))) )

;(defun split-list (l n)
;  (mapcar #'reverse (split-list-aux l n n '())) )

;(defun split-list-aux (l n n1 result)
;  (cond ((null l) (reverse result))
;	((zerop n1)
;	 (push (car l) (nth (1- (length result)) result))
;	 (split-list-aux (cdr l) n (1- n) result))
;	((null (nth (1- n) result))
;	 (split-list-aux (cdr l) n (1- n1) (cons (list (car l)) result)))
;	(t
;	 (push (car l) (nth (1- n1) result))
;	 (split-list-aux (cdr l) n (1- n1) result))) )

;(defun split-list (l n)
;  (mapcar #'reverse (split-list-aux l n (make-list n))) )

;(defun split-list-aux (l n result)
;  (cond ((null l) (reverse result))
;	(t (split-list-aux (nthcdr n l) n (split-list-aux-2 l n result)))) )

;(defun split-list-aux-2 (l n result)
;  (cond ((or (null l) (zerop n))
;	 result)
;	(t (push (car l) (nth (1- n) result))
;	   (split-list-aux-2 (cdr l) (1- n) result))) )

;;;
;;;    Distribute the elements of a list across a list of lists.
;;;    (Assumes both have same length?)
(defun spread-list (l lol)
  (cond ((null lol) '())
        (t (cons (cons (car l) (car lol))
                 (spread-list (cdr l) (cdr lol)))) ) )

(defun spread-list-map (l lol)
  (mapcar #'cons l lol) )

;; (defun make-columns (l n)
;;   (let ((matrix (make-matrix-2-2011 l n)))
;;     (spread-list (get-column matrix) (spread-list (remove-column matrix)))) )

;;;
;;;    Performance comparison
;;;    
; [19]> (time (make-matrix-2 '(a b c d e f) 2))

; Real time: 3.76E-4 sec.
; Run time: 0.0 sec.
; Space: 448 Bytes
; ((A B) (C D) (E F))
; [20]> (time (make-matrix-2 '(a b c d e f g h) 2))

; Real time: 5.33E-4 sec.
; Run time: 0.0 sec.
; Space: 488 Bytes
; ((A B) (C D) (E F) (G H))
; [21]> (time (make-matrix-2 '(a b c d e f g h i j) 2))

; Real time: 6.48E-4 sec.
; Run time: 0.0 sec.
; Space: 528 Bytes
; ((A B) (C D) (E F) (G H) (I J))
; [22]> (time (make-matrix '(a b c d e f) 2))

; Real time: 0.002331 sec.
; Run time: 0.01 sec.
; Space: 1356 Bytes
; ((A B) (C D) (E F))
; [23]> (time (make-matrix '(a b c d e f g h) 2))

; Real time: 0.010469 sec.
; Run time: 0.01 sec.
; Space: 4940 Bytes
; ((A B) (C D) (E F) (G H))
; [24]> (time (make-matrix '(a b c d e f g h i j) 2))

; Real time: 0.047388 sec.
; Run time: 0.04 sec.
; Space: 19276 Bytes
; ((A B) (C D) (E F) (G H) (I J))

(deftest test-make-matrix ()
  (check
   (equal (make-matrix '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-2 ()
  (check
   (equal (make-matrix-2 '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-2 '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-2 '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-2-2011 ()
  (check
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-2-2011 '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-2-2011 '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-list-to-columns ()
  (check
   (equal (list-to-columns '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (list-to-columns '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (list-to-columns '(a b c d e) 3) '((a b c) (d e)))
   (equal (list-to-columns '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (list-to-columns '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (list-to-columns '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (list-to-columns '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (list-to-columns '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-2a ()
  (check
   (equal (make-matrix-2a '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-2a '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-2a '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-2a '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-2a '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-2a '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-2a '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-2a '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-3 ()
  (check
   (equal (make-matrix-3 '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-3 '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-3 '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-3 '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-3 '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-3 '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-3 '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-3 '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-3a ()
  (check
   (equal (make-matrix-3a '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-3a '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-3a '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-3a '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-3a '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-3a '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-3a '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-3a '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-make-matrix-4 ()
  (check
   (equal (make-matrix-4 '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (make-matrix-4 '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (make-matrix-4 '(a b c d e) 3) '((a b c) (d e)))
   (equal (make-matrix-4 '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (make-matrix-4 '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (make-matrix-4 '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (make-matrix-4 '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (make-matrix-4 '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )
