;#!/usr/local/bin/clisp

;;
;   NAME:               ch04.lisp
;
;   STARTED:            010917
;   MODIFICATIONS:
;
;   PURPOSE:
;
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
;   NOTES:
;
;;

;;;
;;;    4.7.1
;;;    
(defun replicate (sym n)
  (cond ((<= n 0) nil)
        (t (cons sym (replicate sym (truncate (- n 1)))) )) )

(defun test-replicate ()
  (let ((test-data '((a 9 (a a a a a a a a a ))
                     ((1 2 3) 3 ((1 2 3) (1 2 3) (1 2 3)))
                     (x -2 ())
                     (pung 1.3 (pung)))) )
    (dolist (l test-data)
      (let* ((sym (first l))
             (n (second l))
             (check-val (third l))
             (value (replicate sym n)))
        (if (equal value check-val)
            (format t "~&Test passed: ~A ~A => ~A~%" sym n value)
            (format t "~&Test FAILED: ~A ~A => ~A [Should be ~A]~%"
                    sym n value check-val)))) ) )

(defun fact (n)
  (cond ((and (integerp n)
              (not (minusp n))) (fact-aux n 1))
        (t nil)) )

(defun fact-aux (n result)
  (cond ((= n 0) result)
        (t (fact-aux (- n 1) (* n result)))) )

(defun test-fact ()
  (let ((test-data '((3 6)
                     (8 40320)
                     (20 2432902008176640000)
                     (-3 ())
                     (1.3 ()))) )
    (dolist (l test-data)
      (let* ((n (first l))
             (check-val (second l))
             (fact-n (fact n)))
        (if (equal check-val fact-n)
            (format t "~&Test passed: ~A => ~A~%" n fact-n)
            (format t "~&Test FAILED: ~A => ~A [Should be ~A]~%"
                    n fact-n check-val)))) ) )

;;;
;;;    4.7.2
;;;    
(defconstant roman-numerals '((i 1)
                              (v 5)
                              (x 10)
                              (l 50)
                              (c 100)
                              (d 500)
                              (m 1000)))

;;;
;;;    Accessor function.
;;;    
(defun numeral-to-decimal (numeral)
  (second (assoc numeral roman-numerals)) )

(defun test-numeral-to-decimal ()
  (dolist (l roman-numerals)
    (let* ((n (first l))
           (check-val (second l))
           (d (numeral-to-decimal n)))
      (if (= d check-val)
          (format t "~&Test passed: ~A => ~A~%" n d)
          (format t "~&Test FAILED: ~A => ~A [Should be ~A]~%"
                  n d check-val)))) )

;;;
;;;    Main function.
;;;    
(defun roman-to-decimal (num-list)
  (cond ((roman-filter num-list) nil)
        (t (translate num-list))) )

;;;
;;;    Translate a valid Roman numeral to decimal
;;;    
(defun translate (num-list)
  (cond ((null num-list) 0)
        ((and (rest num-list)
              (< (numeral-to-decimal (first num-list))
                 (numeral-to-decimal (second num-list))))
         (- (translate (rest num-list))
            (numeral-to-decimal (first num-list))))
        (t (+ (translate (rest num-list))
              (numeral-to-decimal (first num-list)))) ) )

;;;
;;;    Test validity of Roman numeral. Return T to indicate it is not formed
;;;    properly.
;;;    
(defun roman-filter (num-list)
  (cond ((or (adjacent-scale-error num-list)
             (position-error num-list)
             (sequence-error num-list))
         (format t "~&Malformed roman numeral.~%")
         t)
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
;;;    This version doesn't appear to suffer from the problem of valid numbers violating the test
;;;    as in the 'new' version. But it passes other invalid numbers, e.g. IXX, IXV.
;;;
;;;    This version passes every valid roman numeral in roman-numeral-test.data and correctly flags
;;;    IXL and XIIX as invalid. However, it passes CIC (but this would be caught by ADJACENT-SCALE-ERROR).
;;;    
(defun sequence-error (num-list)
  (labels ((sequence-error-aux (num-list)
             (cond ((null (rest num-list)) nil)
                   ((< (numeral-to-decimal (first num-list))
                       (numeral-to-decimal (second num-list)))
                    t)
                   ((= (numeral-to-decimal (first num-list))
                       (numeral-to-decimal (second num-list)))
                    (sequence-error-aux (rest num-list)))
                   (t nil))))
    (cond ((null (rest num-list)) nil)
          ((<= (numeral-to-decimal (first num-list))
               (numeral-to-decimal (second num-list)))
           (or (sequence-error-aux (rest num-list))
               (sequence-error (rest num-list))))
          (t (sequence-error (rest num-list)))) ) )

;;;
;;;    Only I, X, C may appear to the left of a larger number.
;;;    This only checks the first elt! What about the rest of the list? 070710
;;;    
(let ((ok-on-left '(i x c)))
  (defun position-error (num-list)
    (cond ((null (rest num-list)) nil)
          ((and (not (member (first num-list) ok-on-left))
                (< (numeral-to-decimal (first num-list))
                   (numeral-to-decimal (second num-list)))) )) ) )

(defun test-roman-to-decimal ()
  (load "roman-numeral-test.data")
  (dolist (l test-roman-numerals)
    (let* ((roman (first l))
           (check-val (second l))
           (value (roman-to-decimal roman)))
      (if (eql check-val value)
          (format t "~&Test passed: ~A => ~A~%" roman value)
          (format t "~&Test FAILED: ~A => ~A [Should be ~A]~%"
                  roman value check-val))) ) )

;;;
;;;    4.7.3
;;;
;(defun make-change (money)
;  (cond ((= money 0) nil)
;	((< money 5)   (cons (list money 'penny)
;			     (make-change (mod money 1))))
;	((< money 10)  (cons (list (truncate money 5) 'nickel)
;			     (make-change (mod money 5))))
;	((< money 25)  (cons (list (truncate money 10) 'dime)
;			     (make-change (mod money 10))))
;	((< money 50)  (cons (list (truncate money 25) 'quarter)
;			     (make-change (mod money 25))))
;	((< money 100) (cons (list (truncate money 50) 'half-dollar)
;			     (make-change (mod money 50))))
;	(t             (cons (list (truncate money 100) 'dollar)
;			     (make-change (mod money 100)))) ) )

(defun make-change (money)
  (cond ((= money 0) nil)
        ((< money 5)   (cons (list money (if (> money 1)
                                             'pennies
                                             'penny))
                             (make-change (mod money 1))))
        ((< money 10)  (cons (list (truncate money 5) 'nickel)
                             (make-change (mod money 5))))
        ((< money 25)  (let ((n-dimes (truncate money 10)))
                         (cons (list n-dimes (if (> n-dimes 1)
                                                 'dimes
                                                 'dime))
                               (make-change (mod money 10)))) )
        ((< money 50)  (cons (list (truncate money 25) 'quarter)
                             (make-change (mod money 25))))
        ((< money 100) (cons (list (truncate money 50) 'half-dollar)
                             (make-change (mod money 50))))
        (t             (let ((n-dollars (truncate money 100)))
                         (cons (list n-dollars (if (> n-dollars 1)
                                                   'dollars
                                                   'dollar))
                               (make-change (mod money 100)))) )) )

(defun test-make-change ()
  )

;;;
;;;    4.7.4
;;;
(defconstant us-currency '((100 dollar      dollars)
                           (50  half-dollar half-dollars)
                           (25  quarter     quarters)
                           (10  dime        dimes)
                           (5   nickel      nickels)
                           (1   penny       pennies)))

(defun make-change (money currency-list)
  (cond ((= money 0) nil)
        ((>= money (caar currency-list))
         (let ((n-coins (truncate money (caar currency-list))))
           (cons (list n-coins (if (> n-coins 1)
                                   (caddar currency-list)
                                   (cadar currency-list)))
                 (make-change (mod money (caar currency-list))
                              (cdr currency-list)))) )
        (t (make-change money (cdr currency-list)))) )


;;;
;;;    4.7.6
;;;
;;;    Assumes two proper lists.
;;;    
(defun my-append (l1 l2)
  (cond ((null l1) l2)
        (t (my-append (butlast l1) (cons (car (last l1)) l2)))) )

;;;
;;;    Slade's solution (pg. 281)
;;;    This is not tail-recursive, but it avoids the calls to butlast
;;;    AND last!
;;;    It appears to be faster than my-append. (It's definitely
;;;    more elegant.)
;;;    
(defun slade-append (list1 list2)
  (cond ((endp list1) list2)
        (t (cons (car list1)
                 (slade-append (cdr list1) list2)))) )

(defun slade-multi-append (&rest lists)
  (cond ((endp lists) '())
        ((endp (cdr lists)) (car lists))
        ((endp (cddr lists))
         (slade-append (car lists) (cadr lists)))
        (t (slade-multi-append (car lists) (apply #'slade-multi-append (cdr lists)))) ) )

;;;
;;;    This function takes an arbitrary number of lists and appends them.
;;;    The function reduce is used to append pairs of lists two at a time.
;;;    For efficiency, the lambda function reverses its first arg and
;;;    succesive car's are consed onto the second arg.
;;;    
(defun my-append-2 (&rest lists)
  (reduce #'(lambda (l1 l2)
              (labels ((my-append-aux (a b)
                         (cond ((null a) b)
                               (t (my-append-aux (cdr a)
                                                 (cons (car a) b)))) ))
                (my-append-aux (reverse l1) l2)))
          lists) )

;;;
;;;    This is similar to the previous function, however, rather than reversing
;;;    the first argument to my-append-aux each time we reverse the second and
;;;    then reverse the results. The reason for this is that the first list
;;;    continues to grow larger and larger as reduce proceeds. We also need to
;;;    reverse the original list of arguments before we begin.
;;;    
(defun my-append-3 (&rest lists)
  (reduce #'(lambda (l1 l2)
              (labels ((my-append-aux (a b)
                         (cond ((null a) b)
                               (t (my-append-aux (cdr a)
                                                 (cons (car a) b)))) ))
                (reverse (my-append-aux l1 (reverse l2)))) )
          (reverse lists)) )
;;;
;;;    This relies heavily on the fact that (cdr nil) => nil
;;;    The first test checks whether there are 2 or fewer elements in
;;;    the list of arguments.
;;;    
(defun my-append-4 (&rest lists)
  (cond ((null (cddr lists)) (my-append-4-aux (reverse (car lists))
                                              (cadr lists)))
        (t (apply #'my-append-4 (my-append-4-aux (reverse (car lists))
                                                 (cadr lists))
                  (cddr lists)))) )

(defun my-append-4-aux (l1 l2)
  (cond ((null l1) l2)
        (t (my-append-4-aux (cdr l1) (cons (car l1) l2)))) )

(defun time-my-append ()
  (let ((test-data '(((a b c) ((d) e) (1 2 3) (f) (x (y (z))))
                     ((a b c (d e) x y z 1 2 3 4 5 6 7 8 a s s d a s d f f e d d d s 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)
                      (f g (h) i) (x o x o x o x) (5 4 3 2 1))
                     ((a b c (d e) x y z 1 2 3 4 5 6 7 8 a s s d a s d f f e d d d s 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)
                      (f g (h) i) (x o x o x o x) (5 4 3 2 1)
                      (a b c (d e) x y z 1 2 3 4 5 6 7 8 a s s d a s d f f e d d d s 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)
                      (f g (h) i) (x o x o x o x) (5 4 3 2 1))))
        (functions '(my-append-2 my-append-3 my-append-4 slade-multi-append append)))
    (dolist (test-lists test-data)
      (format t "~%~%")
      (dolist (f functions)
        (format t "~&Testing ~A:" f)
        (time (apply f test-lists)))) ) )
		

;;;
;;;    4.7.7
;;;
(defun my-nth (n l)
  (cond ((null l) nil)
        ((zerop n) (car l))
        (t (my-nth (- n 1) (cdr l)))) )

(defun test-my-nth ()
  (let ((test-data '((2 (a b c) c)
                     (0 (1 (2 3)) 1)
                     (1 ((a (b c)) (d) e f) (d))
                     (14 (x y z p d q) ()))) )
    (dolist (l test-data)
      (let* ((n (car l))
             (test-list (cadr l))
             (check-val (caddr l))
             (value (my-nth n test-list)))
        (if (equal check-val value)
            (format t "~&Test passed: ~A ~A => ~A~%" n test-list value)
            (format t "~&Test FAILED: ~A ~A => ~A [Should be ~A]~%"
                    n test-list value check-val)))) ) )

;;;
;;;    4.7.8
;;;    (Doesn't handle n = 0 (or n < 0 !!))
;;;    
(defun my-last (l &optional (n 1))
  (cond ((<= (length l) n) l)
        (t (my-last (cdr l) n))) )

(defun test-my-last ()
  (let ((test-data '((((a b c))       (c))
                     (((a b c d e) 3) (c d e))
                     (((a b . c))     (b . c))
                     (((a b c d) 9)   (a b c d)))) )
    (dolist (l test-data)
      (let* ((n (cadar l))
             (test-list (caar l))
             (check-val (cadr l))
             (value (cond ((null n) (my-last test-list))
                          (t (my-last test-list n)))) )
        (if (equal check-val value)
            (format t "~&Test passed: ~A ~A => ~A~%" test-list (or n "") value)
            (format t "~&Test FAILED: ~A ~A => ~A [Should be ~A]~%"
                    test-list (or n "") value check-val)))) ) )

;;;
;;;    4.7.9
;;;
;;;    This is idiotic!!
;;;    
; (defun my-remove-duplicates (l)
;   (cond ((null l) nil)
; 	((member (car l) (cdr (member (car l) l :test #'equal)) :test #'equal)
; 	 (my-remove-duplicates (cdr l)))
; 	(t (cons (car l) (my-remove-duplicates (cdr l)))) ) )
;;
;;    The following is a refinement of the above version,
;;    based on Slade's solution.
;;    Note: his will fail for trees--((a b) c d (a b) d e)
;;    
(defun my-remove-duplicates (l)
  (cond ((null l) nil)
        ((member (car l) (cdr l) :test #'equal)
         (my-remove-duplicates (cdr l)))
        (t (cons (car l) (my-remove-duplicates (cdr l)))) ) )

(defun test-my-remove-duplicates ()
  (let ((test-data '(((a b c a b) (c a b))
                     ((a b c d e) (a b c d e))
                     ((a (b c) d a (b c) b c) (d a (b c) b c)))) )
    (dolist (l test-data)
      (let* ((test-list (car l))
             (check-val (cadr l))
             (value (my-remove-duplicates test-list)))
        (if (equal check-val value)
            (format t "~&Test passed: ~A => ~A~%" test-list value)
            (format t "~&Test FAILED: ~A => ~A [Should be ~A]~%"
                    test-list value check-val)))) ) )

	
;;;
;;;    4.7.10
;;;
;;;    One need only test balance once to determine whether it's a number.
;;;    All recursive calls involve an argument that is the output of + or *.
;;;    As long as the transactions list is a proper list it only needs to be
;;;    checked once too. Thus it is safe to remove these tests from the
;;;    recursive function.
;;;  
(defun check-book (balance transactions)
  (cond ((not (numberp balance))
         (format t "Error--Non-numeric balance: ~A~%" balance))
        ((or (not (listp transactions))
             (not (null (last transactions 0))))
         (format t "Error--Invalid transaction list: ~A~%" transactions))
        (t (check-book-aux balance transactions))) )

;;;
;;;    To verify that an interest rate transaction is a list of one number we
;;;    use (not (null (cdr transaction)))) to check the length rather than
;;;    (/= (length transaction) 1)). This second test would not eliminate a
;;;    list such as (1.1 . 5).
;;;    
(defun check-book-aux (balance transactions)
  (let ((transaction (car transactions)))
    (cond ((null transactions) balance)
          ((numberp transaction) (check-book-aux (+ transaction balance)
                                                 (cdr transactions)))
          ((listp transaction)
           (let ((interest-rate (car transaction)))
             (cond ((or (not (numberp interest-rate))
                        (minusp interest-rate)
                        (not (null (cdr transaction))))
                    (format t "Error--Invalid interest rate: ~A~%" interest-rate))
                   (t (check-book-aux (* balance interest-rate)
                                      (cdr transactions)))) ))
          (t (format t "Error--Invalid transaction: ~A~%" transaction)))) )

;;;
;;;    4.7.11 (Derived from solution to 4.7.10)
;;;
(defun now-account (balance transactions)
  (cond ((not (numberp balance))
         (format t "Error--Non-numeric balance: ~A~%" balance))
        ((or (not (listp transactions))
             (not (null (last transactions 0))))
         (format t "Error--Invalid transaction list: ~A~%" transactions))
        (t (now-account-aux balance transactions))) )

(defconstant balance-limit 500)
(defconstant penalty 0.10)

(defun now-account-aux (balance transactions)
  (let ((transaction (car transactions)))
    (cond ((null transactions) balance)
          ((numberp transaction)
           (if (and (minusp transaction)
                    (< balance balance-limit))
               (now-account-aux (+ transaction balance (- penalty))
                                (cdr transactions))
               (now-account-aux (+ transaction balance)
                                (cdr transactions))))
          ((listp transaction)
           (let ((interest-rate (car transaction)))
             (cond ((or (not (numberp interest-rate))
                        (minusp interest-rate)
                        (not (null (cdr transaction))))
                    (format t "Error--Invalid interest rate: ~A~%" interest-rate))
                   ((< balance balance-limit)
                    (now-account-aux balance (cdr transactions)))
                   (t (now-account-aux (* balance interest-rate)
                                       (cdr transactions)))) ))
          (t (format t "Error--Invalid transaction: ~A~%" transaction)))) )

;;;
;;;    4.7.12
;;;    This fails the master test suite (2010).
(defun matchp (pattern input)
  (cond ((null pattern) (null input))
        ((equal (car pattern) (car input))
         (matchp (cdr pattern) (cdr input)))
        ((equal (car pattern) '*wild*)
         (cond ((null input) (null (cdr pattern)))
               ((equal (cadr pattern) (cadr input))
                (or (matchp (cddr pattern) (cddr input))
                    (matchp pattern (cdr input)))) ;Backtrack?     (*w* b a) vs. (a b c b a)
               (t (matchp pattern (cdr input)))) )
        (t nil)) )

;;;
;;;    This (essentially) is Slade's solution.
;;;    
(defun slade-matchp (pattern input)
  (cond ((null pattern) (null input))
        ((equal (car pattern) (car input))
         (slade-matchp (cdr pattern) (cdr input)))
        ((equal (car pattern) '*wild*)
         (cond ((null input) (null (cdr pattern)))
               ((or (slade-matchp (cdr pattern) input)
                    (slade-matchp pattern (cdr input)))) ))
        (t nil)) )

;;;
;;;    4.7.13
;;;    (Count occurrences of non-nil atom in a tree)
;;;
(defun count-occurrences (atom tree)
  (cond ((equal atom tree) 1)
        ((atom tree) 0)
        (t (+ (count-occurrences atom (car tree))
              (count-occurrences atom (cdr tree)))) ) )

;;;
;;;    4.7.14
;;;
(defun tree-addition (n tree)
  (cond ((null tree) nil)
        ((numberp tree) (+ n tree))
        ((atom tree) nil)
        (t (cons (tree-addition n (car tree))
                 (tree-addition n (cdr tree)))) ) )

;;;
;;;    4.7.15
;;;    (Assumes all atoms are numbers.)
;;;
; (defun tree-average (tree)
;   (multiple-value-bind (count sum)
;       (tree-average-aux tree 0 0)
;     (cond ((zerop count) nil)
; 	  (t (/ sum count)))) )

(defun tree-average (tree)
  (let* ((result (tree-average-aux tree '(0 0)))
         (count  (car result))
         (sum    (cadr result)))
    (cond ((zerop count) nil)
          (t (/ sum count)))) )

(defun tree-average-aux (tree result)
  (let ((count (car result))
        (sum   (cadr result)))
    (cond ((null tree) result)
          ((numberp tree) (list (+ count 1)
                                (+ sum tree)))
          (t (let ((car-result (tree-average-aux (car tree) result))
                   (cdr-result (tree-average-aux (cdr tree) result)))
               (list (+ (car car-result) (car cdr-result))
                     (+ (cadr car-result) (cadr cdr-result)))) ))) )