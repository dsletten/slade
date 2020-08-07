;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Mon Mar  1 02:25:30 2004
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
(load "/Users/dsletten/lisp/programs/utils.lisp")

;;;
;;;    4.7.1
;;;
(defun replicate (obj n)
  "Create a list of N copies of OBJ."
  (labels ((replicate-aux (obj n)
             (cond ((zerop n) '())
                   (t (cons obj (replicate-aux obj (1- n)))) )))
    (cond ((and (integerp n)
                (>= n 0))
           (replicate-aux obj n))
          (t nil))))

(defun fact (n)
  "Compute factorial of N: N!"
  (labels ((fact-aux (n)
             (cond ((zerop n) 1)
                   (t (* n (fact-aux (1- n)))) )))
    (cond ((and (numberp n)
                (>= n 0))
           (fact-aux n))
          (t nil))))

;;;
;;;    4.7.2
;;;    Much of this is ripped off from the 'old' version (010917). Some of it
;;;    fixes problems with that though.
;;;    See also cgi program 'roman-numerals'.
;;;
(defconstant roman-numeral-a-list '((i 1)
                                    (v 5)
                                    (x 10)
                                    (l 50)
                                    (c 100)
                                    (d 500)
                                    (m 1000)))

(defun numeral-to-decimal (numeral)
  (cadr (assoc numeral roman-numeral-a-list)))

(defun roman-to-decimal (num-list)
  (unless (roman-filter num-list) 
    (translate num-list)))

;;;
;;;    Translate a valid Roman numeral to decimal
;;;    
(defun translate (num-list)
  (cond ((null num-list) 0)
        ((null (cdr num-list))
         (numeral-to-decimal (car num-list)))
        (t (let ((this-val (numeral-to-decimal (car num-list)))
                 (next-val (numeral-to-decimal (cadr num-list))))
             (cond ((< this-val next-val)
                    (- (translate (cdr num-list)) this-val))
                   (t (+ (translate (cdr num-list)) this-val)))) )))

;;;
;;;    Is this garbage? 040913
;;;    It's broken...
;;;    
(defun validate (num-list)
  (labels ((validate-aux (num-list fail-flag)
             (cond ((null (cdr num-list)) t)
                   (t (let ((this-val (numeral-to-decimal (car num-list)))
                            (next-val (numeral-to-decimal (cadr num-list))))
                        (cond ((< this-val next-val)
                               (cond ((<= (* 10 this-val) next-val) nil)
                                     (fail-flag nil)
                                     (t (validate-aux (cdr num-list) t))))
                              (t (validate-aux (cdr num-list)
                                               fail-flag)))) ))))
    (validate-aux num-list nil)))

;; (defun symbol-test (num-list)
;;   (every #'numeral-to-decimal num-list))

(defun symbol-test (l)
  "Verify that all symbols in the list are valid roman numerals."
  (cond ((null l) t)
        ((assoc (car l) roman-numerals) (symbol-test (cdr l)))
        (t nil)) )

(let ((ok-on-left '(i x c)))
  (defun position-error (num-list)
    (cond ((null (rest num-list)) nil)
          ((and (not (member (first num-list) ok-on-left))
                (< (numeral-to-decimal (first num-list))
                   (numeral-to-decimal (second num-list)))) )
          (t (position-error (cdr num-list)))) ))


;;;
;;;    Test validity of Roman numeral. Return T to indicate it is not formed
;;;    properly.
;;;    
(defun roman-filter (num-list)
  (cond ((or (not (symbol-test num-list))
             (adjacent-scale-error num-list)
             (position-error num-list)
             (sequence-error num-list)))
        (t nil)) )

;;;
;;;    Test whether elements to be subtracted are valid.
;;;    A smaller value should not be less than one tenth of the value it
;;;    precedes.
;;;    
(defun adjacent-scale-error (num-list)
  (cond ((null (rest num-list)) nil)
        ((< (* 10 (numeral-to-decimal (first num-list)))
            (numeral-to-decimal (second num-list))) t)
        (t (adjacent-scale-error (rest num-list)))) )

;;;
;;;    Only one smaller value may appear to left of larger value.
;;;    If a value is found that is less than or equal to the value on its
;;;    right, then the auxiliary function is triggered. It continues to examine
;;;    the list. If it ever finds a larger value later in the list, then the
;;;    number is malformed.
;;;    Examples:
;;;        XIIX -> bad
;;;        XIII -> ok
;;;        IXL  -> bad
;;;    Note: There is some inefficiency in the second example as the first pair
;;;    of I's triggers the aux function and later the second pair triggers it
;;;    again.
;;;    
;; (defun sequence-error (num-list)
;;   (labels ((sequence-error-aux (max num-list)
;; 	     (cond ((null (rest num-list)) nil)
;; 		   ((< max (numeral-to-decimal (car num-list)))
;; 		    t)
;; 		   ((= max (numeral-to-decimal (car num-list)))
;; 		    (sequence-error-aux max (rest num-list)))
;; 		   (t nil))))
;;     (cond ((null (rest num-list)) nil)
;; 	  (t (let ((this-val (numeral-to-decimal (first num-list)))
;; 		   (next-val (numeral-to-decimal (second num-list))))
;; 	       (cond ((< this-val next-val)
;; 		      (or (sequence-error-aux next-val (cddr num-list))
;; 			  (sequence-error (rest num-list))))
;; 	  ((= (numeral-to-decimal (first num-list))
;; 	      (numeral-to-decimal (second num-list)))
;; 	  (t (sequence-error (rest num-list)))) ) )

;;;
;;;    Two general rules:
;;;    1. If a symbol is less than the one on its right, then every
;;;       symbol to the right of the 2nd symbol must be less than the 1st.
;;;         WRONG         RIGHT
;;;         IXL           LIX
;;;         IXX           XIX
;;;         IXV           XIV
;;;         MCMM          MMCM
;;;                       CMX
;;;       (Exceptions to this rule are listed below.)
;;;    2. If a symbol is equal to the one on its right, then every other symbol
;;;       to the right must be less than or equal to the 1st symbol.
;;;         WRONG         RIGHT
;;;         XIIX          XIX
;;;
;;;    The 'old' version doesn't seem to have the problems w/ violating rule 1!! (But
;;;    it has other problems.)
;;;         
(defun sequence-error (num-list)
                                        ;  (labels ()
  (cond ((null (cdr num-list)) nil)
        (t (let ((this-val (numeral-to-decimal (first num-list)))
                 (next-val (numeral-to-decimal (second num-list))))
             (cond ((< this-val next-val)
                    (or (notevery #'(lambda (x)
                                      (< (numeral-to-decimal x)
                                         this-val))
                                  (cddr num-list))
                        (sequence-error (cdr num-list))))
                   ((= this-val next-val)
                    (or (notevery #'(lambda (x)
                                      (<= (numeral-to-decimal x)
                                          this-val))
                                  (cddr num-list))
                        (sequence-error (cdr num-list))))
                   (t (sequence-error (cdr num-list)))) ))))

;;; Violations of SEQUENCE-ERROR (Rule 1.)
;; Ooops: ((X L I X) 49)
;; Ooops: ((X C I X) 99)
;; Ooops: ((C X L I X) 149)
;; Ooops: ((C X C I X) 199)
;; Ooops: ((C C X L I X) 249)
;; Ooops: ((C C X C I X) 299)
;; Ooops: ((C C C X L I X) 349)
;; Ooops: ((C C C X C I X) 399)
;; Ooops: ((C D X L I X) 449)
;; Ooops: ((C D X C) 490)
;; Ooops: ((C D X C I) 491)
;; Ooops: ((C D X C I I) 492)
;; Ooops: ((C D X C I I I) 493)
;; Ooops: ((C D X C I V) 494)
;; Ooops: ((C D X C V) 495)
;; Ooops: ((C D X C V I) 496)
;; Ooops: ((C D X C V I I) 497)
;; Ooops: ((C D X C V I I I) 498)
;; Ooops: ((C D X C I X) 499)
;; Ooops: ((D X L I X) 549)
;; Ooops: ((D X C I X) 599)
;; Ooops: ((D C X L I X) 649)
;; Ooops: ((D C X C I X) 699)
;; Ooops: ((D C C X L I X) 749)
;; Ooops: ((D C C X C I X) 799)
;; Ooops: ((D C C C X L I X) 849)
;; Ooops: ((D C C C X C I X) 899)
;; Ooops: ((C M X L I X) 949)
;; Ooops: ((C M X C) 990)
;; Ooops: ((C M X C I) 991)
;; Ooops: ((C M X C I I) 992)
;; Ooops: ((C M X C I I I) 993)
;; Ooops: ((C M X C I V) 994)
;; Ooops: ((C M X C V) 995)
;; Ooops: ((C M X C V I) 996)
;; Ooops: ((C M X C V I I) 997)
;; Ooops: ((C M X C V I I I) 998)
;; Ooops: ((C M X C I X) 999)
;; Ooops: ((M X L I X) 1049)
;; Ooops: ((M X C I X) 1099)
;; Ooops: ((M C X L I X) 1149)
;; Ooops: ((M C X C I X) 1199)
;; Ooops: ((M C C X L I X) 1249)
;; Ooops: ((M C C X C I X) 1299)
;; Ooops: ((M C C C X L I X) 1349)
;; Ooops: ((M C C C X C I X) 1399)
;; Ooops: ((M C D X L I X) 1449)

;;;
;;;     4.7.3
;;;
(defconstant us-currency '((100 dollar dollars)
                           (50 half-dollar half-dollars)
                           (25 quarter quarters)
                           (10 dime dimes)
                           (5 nickel nickels)
                           (1 penny pennies)))

(defun make-change (money &optional (currency-list us-currency))
  (cond ((= money 0) '())
        ((>= money (caar currency-list))
         (let ((coin-count (truncate money (caar currency-list))))
           (cons (list coin-count (if (= coin-count 1)
                                      (cadar currency-list)
                                      (caddar currency-list)))
                 (make-change (mod money (caar currency-list))
                              (cdr currency-list)))) )
        (t (make-change money (cdr currency-list)))) )


;;;
;;;    4.7.5
;;;
(defun my-reverse (l)
  (cond ((null l) '())
        (t (append (my-reverse (cdr l)) (list (car l)))) ))

(defun my-reverse2 (l)
  (labels ((reverse-aux (l result)
             (cond ((null l) result)
                   (t (reverse-aux (cdr l) (cons (car l) result)))) ))
    (reverse-aux l '())))

(defun mega-reverse (l)
  (cond ((null (cdr l)) l)
        (t
         (cons (car (mega-reverse (cdr l)))
               (mega-reverse
                (cons (car l)
                      (mega-reverse (cdr (mega-reverse (cdr l)))) )))) ))

;;;
;;;    4.7.6
;;;
(defun my-append (l1 l2)
  (cond ((null l1) l2)
        (t (cons (car l1) (my-append (cdr l1) l2)))) )

;;;
;;;    4.7.7
;;;
(defun my-nth (n l)
  (cond ((zerop n) (car l))
        ((null l) '())
        (t (my-nth (1- n) (cdr l)))) )

;;;
;;;    4.7.8
;;;
;; (defun my-last (l)
;;   (cond ((null (cdr l)) l)
;; 	 (t (my-last (cdr l)))) )
(defun my-last (l)
  (cond ((atom (cdr l)) l)
        (t (my-last (cdr l)))) )

;;;
;;;    4.7.9
;;;
(defun my-remove-duplicates (l)
  (cond ((null l) '())
        ((member (car l) (cdr l)) (my-remove-duplicates (cdr l)))
        (t (cons (car l) (my-remove-duplicates (cdr l)))) ))

;;;
;;;    4.7.10
;;;
(defun check-book (balance transactions)
  (cond ((not (numberp balance)) (cons 'error--non-numeric-balance balance))
        ((null transactions) balance)
        ((atom transactions) (cons 'error--atom-instead-of-list transactions))
        ((numberp (car transactions))
         (check-book (+ balance (car transactions)) (cdr transactions)))
        ((and (list (car transactions)) ; <-- Oops
              (= (length (car transactions)) 1)
              (numberp (caar transactions)))
         (check-book (* balance (caar transactions)) (cdr transactions)))
        (t (cons 'error--invalid-interest-rate (car transactions)))) )

;;;
;;;    4.7.11
;;;
(let ((minimum-balance 500)
      (debit-penalty -0.1))
  (defun now-account (balance transactions)
    (cond ((not (numberp balance)) (cons 'error--non-numeric-balance balance))
          ((null transactions) balance)
          ((atom transactions) (cons 'error--atom-instead-of-list
                                     transactions))
          ((numberp (car transactions))
           (if (and (< balance minimum-balance)
                    (minusp (car transactions)))
               (now-account (+ balance (car transactions) debit-penalty)
                            (cdr transactions))
               (now-account (+ balance (car transactions))
                            (cdr transactions))))
          ((and (list (car transactions))
                (= (length (car transactions)) 1)
                (numberp (caar transactions)))
           (if (> balance minimum-balance)
               (now-account (* balance (caar transactions))
                            (cdr transactions))
               (now-account balance (cdr transactions))))
          (t (cons 'error--invalid-interest-rate (car transactions)))) ))

;;;
;;;    4.7.12
;;;    The first version is correct. The others are not.      ?!?
;;;    This fails the master test suite (2010).
(defun matchp (pattern input)
  (cond	((null pattern) (null input))
        ((eq (car pattern) (car input))
         (matchp (cdr pattern) (cdr input)))
        ((eq (car pattern) '*wild*)
         (if (null input)
             (null (cdr pattern))
             (or (matchp pattern (cdr input))
                 (matchp (cdr pattern) input))))
        (t nil)))

;;
;;   This only partially works IF the recursive calls are swapped from above.
;;   Even then it fails for e.g., (matchp '(a *wild* b) '(a b c d b)). It
;;   goes into an infinite loop checking (*wild* b) and ().
;;   
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;; 	((eq (car pattern) (car input))
;; 	 (matchp (cdr pattern) (cdr input)))
;; 	((eq (car pattern) '*wild*)
;; 	 (or (and (null input) (null (cdr pattern)))
;; 	     (matchp pattern (cdr input))
;; 	     (matchp (cdr pattern) input)))
;; 	(t nil)))

;;
;;    This fails even more.
;;    
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;; 	((eq (car pattern) (car input))
;; 	 (matchp (cdr pattern) (cdr input)))
;; 	((eq (car pattern) '*wild*)
;; 	 (or (matchp (cdr pattern) input)
;; 	     (matchp pattern (cdr input))))
;; 	(t nil)))

;;Match
(test 'matchp '((((a b c) (a b c)) t)
                (((a *wild*) (a b c)) t)
                ((() ()) t)
                (((a *wild*) (a)) t)
                (((a *wild* b) (a b c d b)) t)
                (((*wild* b *wild*) (a b c d e)) t)
                (((*wild*) (a b c)) t)
                ;;Fail
                (((a b c) (a b c d)) nil)
                (((a b c d) (a b c)) nil)
                (((a b c) (a b x)) nil)
                ((() (a)) nil)
                (((a) ()) nil)
                (((a *wild* b) (a b c d e)) nil)))
;;
;;    Re-ordered based on Oz implementation (irrelevant?)
;;    No. This is not correct. It goes into infinite loop:
;;    (matchp '(a *wild* b) '(a b c d b))
;;    (The test is simply wrong, as above)
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;; 	((eq (car pattern) '*wild*)
;; 	 (or (and (null input) (null (cdr pattern))) ;<---Wrong
;; 	     (matchp pattern (cdr input))
;; 	     (matchp (cdr pattern) input)))
;; 	((eq (car pattern) (car input))
;; 	 (matchp (cdr pattern) (cdr input)))
;; 	(t nil)))
;;
;;    Although reordering the clauses of the working version above doesn't
;;    make a difference:
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;; 	((eq (car pattern) '*wild*)
;; 	 (if (null input)
;; 	     (null (cdr pattern))
;; 	     (or (matchp pattern (cdr input))
;; 		 (matchp (cdr pattern) input))))
;; 	((eq (car pattern) (car input))
;; 	 (matchp (cdr pattern) (cdr input)))
;; 	(t nil)))



;;;
;;;    4.7.13
;;;
(defun count-occurrences (elt obj)
  (cond ((null obj) 0)
        ((eql elt obj) 1)
        ((atom obj) 0)
        (t (+ (count-occurrences elt (car obj))
              (count-occurrences elt (cdr obj)))) ))

;;;
;;;    4.7.14
;;;
(defun tree-addition (n obj)
  (cond ((null obj) '())
        ((numberp obj) (+ n obj))
        ((atom obj) obj)
        (t (cons (tree-addition n (car obj))
                 (tree-addition n (cdr obj)))) ))

;;;
;;;    4.7.15
;;;
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
(defun tree-average (obj)
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

