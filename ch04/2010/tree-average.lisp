;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tree-average.lisp
;;;;
;;;;   Started:            Thu Jul  1 21:42:00 2010
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
;;;;   Notes: A comparison of various solutions to Slade's exercise 4.7.15
;;;;
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2001
;;;    
;;;    This first version illustrates the basic approach taken in most of the
;;;    subsequent solutions. However, the others typically decompose the code
;;;    into more auxiliary functions. This version keeps more bundled together.
;;;
;;;    We methodically traverse the tree and accumulate both a sum of all the
;;;    elements as well as a count of the elements in a result list. A partial
;;;    result list is generated for each CAR and CDR and these must be combined
;;;    in order to produce the total result. The final result is then unpacked,
;;;    and the average is computed by the top-level function.
;;;
;;;    Unfortunately, this version is also typical in that it misunderstands
;;;    the purpose of the RESULT parameter to TREE-AVERAGE-AUX. In fact, it
;;;    serves no purpose. It is always the list '(0 0)! Obviously this is true
;;;    on the initial invocation from the top-level TREE-AVERAGE function.
;;;    However, it is also true for every recursive call which simply passes on
;;;    the current value of RESULT. The actual accumulation of results occurs
;;;    entirely in the recombination of the CAR and CDR results. And the bottom
;;;    level results at the leaves of the tree are always a count of 1 and simply
;;;    the value of the leaf itself.
;;;
;;;    Despite this flaw the code works, although it uses features of Lisp
;;;    beyond what is covered up to chapter 4 by Slade.
;;;
;;;    (Assumes all atoms are numbers.)
;;;
; (defun tree-average (tree)
;   (multiple-value-bind (count sum)
;       (tree-average-aux tree 0 0)
;     (cond ((zerop count) nil)
; 	  (t (/ sum count)))) )

(defpackage tree-average-2001 (:use common-lisp))

(in-package tree-average-2001)

(defun tree-average (tree)
  (let* ((result (tree-average-aux tree '(0 0)))
         (count  (car result))
         (sum    (cadr result)))
    (cond ((zerop count) nil)
          (t (/ sum count)))) )

(defun tree-average-aux (tree result)
  (let ((count (car result)) ; Always 0
        (sum   (cadr result))) ; Always 0
    (cond ((null tree) result)
          ((numberp tree) (list (+ count 1)
                                (+ sum tree)))
          (t (let ((car-result (tree-average-aux (car tree) result))
                   (cdr-result (tree-average-aux (cdr tree) result)))
               (list (+ (car car-result) (car cdr-result))
                     (+ (cadr car-result) (cadr cdr-result)))) ))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2004 (March)
;;;
;;;    This solution is a little different based on its reliance on COUNT and
;;;    SUM variables which are non-local to the AUX function, which uses side
;;;    effects to update them.
;;;    
(defpackage tree-average-2004-march (:use common-lisp))

(in-package tree-average-2004-march)

(defun tree-average (obj)
  (let ((count 0)
        (sum 0))
    (labels ((tree-average-aux (obj)
               (cond ((null obj) nil)
                     ((numberp obj) (incf count) (incf sum obj))
                     ((atom obj) nil)
                     (t (tree-average-aux (car obj))
                        (tree-average-aux (cdr obj)))) ))
      (tree-average-aux obj)
      (if (zerop count)
          0
          (float (/ sum count)))) ))

;;;
;;;    Based on Oz version
;;;
;;;    Not sure how this is based on an Oz version considering it uses
;;;    MULTIPLE-VALUE-BIND/VALUES?!
;;;
;;;    However, the COUNT and TOTAL parameters do serve a purpose here. Each
;;;    CAR is treated as a new tree, and its results are accumulated into the
;;;    traversal of the CDR.
;;;
(defun tree-average-oz (obj)
  (labels ((tree-average-aux (obj count total)
             (cond ((null obj) (values count total))
                   ((numberp obj) (values 1 obj))
                   ((atom obj) (values count total))
                   (t (multiple-value-bind (c1 t1)
                          (tree-average-aux (car obj) 0 0)
                        (tree-average-aux (cdr obj)
                                          (+ count c1)
                                          (+ total t1)))) )))
    (multiple-value-bind (count total)
        (tree-average-aux obj 0 0)
      (if (zerop count)
          0
          (/ total (float count)))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2004 (September)
;;;
(defpackage tree-average-2004-september (:use common-lisp))

(in-package tree-average-2004-september)

;;;
;;;    Same as first version from March 2004.
;;;    
(defun tree-average (obj)
  (let ((sum 0)
        (count 0))
    (labels ((tree-average-aux (obj)
               (cond ((null obj) t) ; <-- Value is irrelevant here. Everything is side effects!
                     ((numberp obj) (incf sum obj) (incf count))
                     ((atom obj) (error "Non-numeric atom: ~S" obj))
                     (t (tree-average-aux (car obj))
                        (tree-average-aux (cdr obj)))) ))
      (tree-average-aux obj)
      (if (zerop count)
          0
          (/ sum count)))) )

;;;
;;;    This is essentially the same as the next version, but not as many
;;;    auxiliary functions are used.
;;;    
(defun tree-average (obj)
  (labels ((tree-average-aux (obj)
             (cond ((null obj) (list 0 0))
                   ((numberp obj) (list obj 1))
                   ((atom obj) (error "Non-numeric atom: ~S" obj))
                   (t (mapcar #'+
                              (tree-average-aux (car obj))
                              (tree-average-aux (cdr obj)))) ))
           (average (l)
             (if (zerop (cadr l))
                 0
                 (/ (car l) (cadr l)))) )
    (average (tree-average-aux obj))))

;;;
;;;    This solution uses nothing beyond what's covered up to ch. 4!
;;;    
(defun tree-average (obj)
  (average (tree-average-aux obj)))

(defun tree-average-aux (obj)
  "Accumulate a sum TOTAL and COUNT of all of the numbers in a tree as a list (TOTAL COUNT)."
  (cond ((null obj) (list 0 0))
        ((numberp obj) (list obj 1))
        ((atom obj) (error "Non-numeric atom: ~S" obj))
        (t (list-add (tree-average-aux (car obj))
                     (tree-average-aux (cdr obj)))) ))

(defun list-add (l1 l2)
  "Add the corresponding elements of two lists of two numbers."
  (list (+ (first l1) (first l2))
        (+ (second l1) (second l2))))

(defun average (l)
  "Compute the average of a sum TOTAL of COUNT numbers where L is (TOTAL COUNT)." 
  (if (zerop (second l))
      0
      (/ (first l) (second l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2007
;;;
;;;    Same as 3rd version from September 2004
;;;    
(defpackage tree-average-2007 (:use common-lisp))

(in-package tree-average-2007)

(defun tree-average (tree)
  (float (average (tree-average-aux tree))))
;  (average (tree-average-aux tree)))

(defun average (l)
  (/ (first l) (second l)))

;; (defun tree-average-aux (tree sum count)
;;   (cond ((null tree) (list 0 0))
;; 	((atom tree) (list tree 1))
;; 	(t (combine-results (tree-average-aux (car tree) sum count)
;; 			    (tree-average-aux (cdr tree) sum count)))) )

(defun tree-average-aux (tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list tree 1))
        (t (combine-results (tree-average-aux (car tree))
                            (tree-average-aux (cdr tree)))) ))

(defun combine-results (l1 l2)
  (list (+ (first l1) (first l2)) (+ (second l1) (second l2))))

;; * (tree-average-aux '(1 2 3))

;; (6 3)
;; * (tree-average-aux '(1 2 (3 (4 (5) 6) (7)) 8 (9)))

;; (45 9)
;; * (tree-average-aux '(((((((1))))))))

;;;
;;;    Slade cheats (using LET from ch. 5). Plus his version is
;;;    broken:
;;;    1. Don't use ENDP when traversing a tree. The arg may be
;;;       a non-nil atom.
;;;    2. The args COUNT and TOTAL to LEAF-AVERAGE are always both 0!
;;;       The accumulation occurs in ADD-MERGE, not as a result of
;;;       (+ total l) and (+ count 1). These parameters may be
;;;       disposed of as with TREE-AVERAGE-AUX above.
;;;       
(defun tree-average-slade (num-tree)
  (let ((pair (leaf-average num-tree 0 0)))
    (/ (car pair) (cdr pair))))

(defun leaf-average (l count total)
  (cond ((null l) '(0 . 0))
;  (cond ((endp l) '(0 . 0))
        ((atom l) (cons (+ total l) (+ count 1)))
        (t (add-merge (leaf-average (car l) count total)
                      (leaf-average (cdr l) count total)))))

(defun add-merge (x y)
  (cons (+ (car x) (car y))
        (+ (cdr x) (cdr y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2009
;;;
;;;    Same as 2007 version.
;;;    
(defpackage tree-average-2009 (:use common-lisp))

(in-package tree-average-2009)

(defun average (l)
  (/ (first l) (second l)))

;; (defun tree-average (tree)
;;   (average (tree-average-aux tree 0 0)))

(defun tree-average (tree)
  (average (tree-average-aux tree)))

(defun tree-average-aux (tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list tree 1))
        (t (combine-results (tree-average-aux (car tree))
                            (tree-average-aux (cdr tree)))) ))

;;;
;;;    See 2007 why SUM/COUNT args are pointless!
;;;    
;; (defun tree-average-aux (tree sum count)
;;   (cond ((null tree) (list sum count)) ; Think about why ch04p version works w/ (list 0 0) here.
;;         ((atom tree) (list (+ sum tree) (1+ count))) ; and (list tree 1) here.
;;         (t (combine-results (tree-average-aux (car tree) sum count)
;;                             (tree-average-aux (cdr tree) sum count)))) )

(defun combine-results (result1 result2)
  (list (+ (first result1) (first result2))
        (+ (second result1) (second result2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2010
;;;
;;;    Once again (D'oh!) the SUM and COUNT parameters to TREE-AVERAGE-AUX are
;;;    pointless. They are always both 0!
;;;    
(defpackage tree-average-2010 (:use common-lisp))

(in-package tree-average-2010)

(defun tree-average (tree)
  (average (tree-average-aux 0 0 tree)))

(defun average (result)
  (/ (first result) (second result)))

(defun tree-average-aux (sum count tree)
  (cond ((null tree) (list 0 0))
        ((atom tree) (list (+ sum tree) (1+ count)))
        (t (combine-results (tree-average-aux sum count (car tree))
                            (tree-average-aux 0 0 (cdr tree)))) ))

(defun combine-results (result1 result2)
  (list (+ (first result1) (first result2))
        (+ (second result1) (second result2))))

;;;
;;;    See 040910
;;;    
(defun combine-results (result1 result2)
  (mapcar #'+ result1 result2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2019
;;;
;;;    This one is slightly different by pushing the recursive CDR traversal
;;;    to an auxiliary function.
(defpackage :tree-average-2019 (:use :common-lisp))

(in-package :tree-average-2019)

(defun compute (l)
  (/ (first l) (second l)))

(defun tree-average (tree)
  (compute (tree-average-aux tree 0 0)))

(defun tree-average-aux (tree sum count)
  (cond ((null tree) (list sum count))
        ((atom tree) (check-type tree number)
         (list (+ tree sum) (1+ count)))
        (t (process (tree-average-aux (car tree) sum count) (cdr tree)))) )

(defun process (l tree)
  (tree-average-aux tree (first l) (second l)))

