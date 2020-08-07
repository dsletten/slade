;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               recursion-vs-iteration.lisp
;;;;
;;;;   Started:            Sun Sep 12 22:48:32 2010
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
;;;;   Notes: DO vs. tail recursion at bottom of file
;;;;          See ch. 1 of SICP (Pascal's Triangle)
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage :recursion-vs-iteration (:use :common-lisp :test) (:shadow :length))

(in-package :recursion-vs-iteration)

(defun length-r (l)
  (if (endp l)
      0
      (1+ (length-r (rest l)))) )

(defun length-tr (l)
  (labels ((length-aux (l length)
             (if (endp l)
                 length
                 (length-aux (rest l) (1+ length)))) )
    (length-aux l 0)))

(defun length-i (l)
  (do ((l l (rest l))
       (n 0 (1+ n)))
      ((endp l) n)))

(defun length-l (l)
  (loop for elt in l
        counting elt))

(deftest test-length-r ()
  (check
   (= (length-r '(a b c d e)) (length '(a b c d e)))
   (= (length-r '()) (length '()))) )

(deftest test-length-tr ()
  (check
   (= (length-tr '(a b c d e)) (length '(a b c d e)))
   (= (length-tr '()) (length '()))) )

(deftest test-length-i ()
  (check
   (= (length-i '(a b c d e)) (length '(a b c d e)))
   (= (length-i '()) (length '()))) )

(deftest test-length-l ()
  (check
   (= (length-l '(a b c d e)) (length '(a b c d e)))
   (= (length-l '()) (length '()))) )

(defun average-r (l)
  (let ((count (length-r l))
        (sum (sum-r l)))
    (/ sum count)))

(defun sum-r (l)
  (if (endp l)
      0
      (+ (first l) (sum-r (rest l)))) )

(defun average-r2 (l)
  (let ((count (recur #'(lambda (l rec) (declare (ignore l)) (1+ rec)) l 0))
        (sum (recur #'(lambda (l rec) (+ (first l) rec)) l 0)))
    (/ sum count)))

(defun recur (f l v)
  (if (endp l)
      v
      (funcall f l (recur f (rest l) v))))

(defun average-tr (l)
  (labels ((average-aux (l sum count)
             (if (endp l)
                 (/ sum count)
                 (average-aux (rest l) (+ sum (first l)) (1+ count)))) )
    (average-aux l 0 0)))

(defun average-i (l)
  (do ((l l (rest l))
       (sum 0 (+ sum (first l)))
       (count 0 (1+ count)))
      ((endp l) (/ sum count))))

(defun average-l (l)
  (loop for elt in l
        counting elt into count
        summing elt into sum
        finally (return (/ sum count))))

(deftest test-average-r ()
  (check
   (= (average-r '(1)) 1)
   (= (average-r '(1 2 3 4 5)) 3)
   (= (average-r '(10 10 10 10)) 10)
   (= (average-r '(5.8 3.6 12.04 9.7 8.1)) 7.8479996)))

(deftest test-average-r2 ()
  (check
   (= (average-r2 '(1)) 1)
   (= (average-r2 '(1 2 3 4 5)) 3)
   (= (average-r2 '(10 10 10 10)) 10)
   (= (average-r2 '(5.8 3.6 12.04 9.7 8.1)) 7.8479996)))

(deftest test-average-tr ()
  (check
   (= (average-tr '(1)) 1)
   (= (average-tr '(1 2 3 4 5)) 3)
   (= (average-tr '(10 10 10 10)) 10)
   (= (average-tr '(5.8 3.6 12.04 9.7 8.1)) 7.8479996)))

(deftest test-average-i ()
  (check
   (= (average-i '(1)) 1)
   (= (average-i '(1 2 3 4 5)) 3)
   (= (average-i '(10 10 10 10)) 10)
   (= (average-i '(5.8 3.6 12.04 9.7 8.1)) 7.8479996)))

(deftest test-average-l ()
  (check
   (= (average-l '(1)) 1)
   (= (average-l '(1 2 3 4 5)) 3)
   (= (average-l '(10 10 10 10)) 10)
   (= (average-l '(5.8 3.6 12.04 9.7 8.1)) 7.8479996)))

(defun mega-reverse (l)
  (cond ((endp l) '())
        ((endp (rest l)) (list (first l)))
        (t (cons (first (mega-reverse (rest l)))
                 (mega-reverse (cons (first l)
                                     (mega-reverse (rest (mega-reverse (rest l)))) )))) ))

;;;
;;;    In other words, minimal number of fundamental functions...
;;;    
(defun mega-reverse (l)
  (cond ((null l) '())
        ((null (cdr l)) (cons (car l) '()))
        (t (cons (car (mega-reverse (cdr l)))
                 (mega-reverse (cons (car l)
                                     (mega-reverse (cdr (mega-reverse (cdr l)))) )))) ))

(defun reverse-tr (l)
  (labels ((reverse-aux (l result)
             (if (endp l)
                 result
                 (reverse-aux (rest l) (cons (first l) result)))) )
    (reverse-aux l '())))

(defun reverse-i (l)
  (do ((l l (rest l))
       (result '() (cons (first l) result)))
      ((endp l) result)))

(defun reverse-l (l)
  (loop for elt in l
        with result = '()
        do (push elt result)
        finally (return result)))

(defun reverse-l (l)
  (loop for elt in l
        for result = (list elt) then (cons elt result)
        finally (return result)))

(deftest test-reverse-tr ()
  (check
   (equal (reverse-tr '()) (reverse '()))
   (equal (reverse-tr '(a)) (reverse '(a)))
   (equal (reverse-tr '(a b c d e)) (reverse '(a b c d e)))
   (equal (reverse-tr '(a b c b a)) (reverse '(a b c b a)))) )

(deftest test-reverse-i ()
  (check
   (equal (reverse-i '()) (reverse '()))
   (equal (reverse-i '(a)) (reverse '(a)))
   (equal (reverse-i '(a b c d e)) (reverse '(a b c d e)))
   (equal (reverse-i '(a b c b a)) (reverse '(a b c b a)))) )

(deftest test-reverse-l ()
  (check
   (equal (reverse-l '()) (reverse '()))
   (equal (reverse-l '(a)) (reverse '(a)))
   (equal (reverse-l '(a b c d e)) (reverse '(a b c d e)))
   (equal (reverse-l '(a b c b a)) (reverse '(a b c b a)))) )

(defun fibonacci (n)
  (do* ((f0 0 f1)
        (f1 0 f2)
        (f2 1 (+ f0 f1))
        (i 0 (1+ i)))
      ((= i n) f1)))

;;;
;;;    Sedgewick ex. 1.3.13 (pg. 78)
;;;
(defun fibonacci (n)
  (do* ((f0 0 (+ f0 f1))
        (f1 1 (- f0 f1))
        (i 0 (1+ i)))
       ((= i n) f0)))

;;;
;;;    Exponential!
;;;    
(defun fibonacci-r (n)
  (case n
    (0 0)
    (1 1)
    (otherwise (+ (fibonacci (- n 2))
                  (fibonacci (- n 1)))) ))

(defun fibonacci-tr (n)
  (labels ((fibonacci-aux (f0 f1 i)
             (cond ((= i n) f0)
                   (t (fibonacci-aux f1 (+ f0 f1) (1+ i)))) ))
    (fibonacci-aux 0 1 0)))

;;;
;;;    Stu's names are better!
;;;    
(defun fibonacci-tr-stu (n)
  (labels ((fibonacci-aux (current next i)
             (cond ((= i n) current)
                   (t (fibonacci-aux next (+ current next) (1+ i)))) ))
    (fibonacci-aux 0 1 0)))

;;;
;;;    Same as FIBONACCI-SEIBEL-DO below.
;;;    
(defun fibonacci-do (n)
  (do ((f0 0 f1)
       (f1 1 (+ f0 f1))
       (i 0 (1+ i)))
      ((= i n) f0)))

(defun fibonacci-l (n)
  (loop repeat (1+ n)
        for f0 = 0 then f1
        and f1 = 1 then (+ f0 f1)
        finally (return f0)))

;;;
;;;    Figure out the different semantics of these 2!!
;;;    for vs. and
;;;    FOR/FOR, WITH/WITH are sequential (LET*/DO*)
;;;    FOR/AND, WITH/AND parallel (LET/DO)
;;;    upto vs. below
;;;    Duh...
;;;    (loop for i below 10 collect i) => (0 1 2 3 4 5 6 7 8 9)
;;;    (loop for i upto 10 collect i) => (0 1 2 3 4 5 6 7 8 9 10)
;;;    
(defun fibonacci (n)
  (loop for i upto n
        for f0 = 0 then f1
        and f1 = 1 then (+ f0 f1)
        finally (return f0)))
;; Equivalent to above????
;;
;;    Seibel pg. 88
;;    
(defun fibonacci (n)
  (loop for i below n
        and f0 = 0 then f1
        and f1 = 1 then (+ f0 f1)
        finally (return f0)))


;;;
;;;    See Slade ch. 9
;;;    
(defun remove-pairs (l)
  (cond ((endp l) '())
        ((endp (rest l)) (list (first l)))
        ((eql (first l) (second l)) (remove-pairs (rest l)))
        (t (cons (first l) (remove-pairs (rest l)))) ))

(defun remove-pairs (l)
  (cond ((endp l) '())
        (t (destructuring-bind (head . tail) l
             (cond ((endp tail) (list head))
                   ((eql head (first tail)) (remove-pairs tail))
                   (t (cons head (remove-pairs tail)))) ))))

(defun remove-pairs (l) ; A
  (labels ((remove-pairs-aux (elt l)
             (cond ((endp l) (list elt))
                   ((eql elt (first l)) (remove-pairs-aux elt (rest l)))
                   (t (cons elt (remove-pairs-aux (first l) (rest l)))) )))
    (if (endp l)
        '()
        (remove-pairs-aux (first l) (rest l)))) )

(defun remove-pairs (l)
  (loop for cons on l
        when (or (endp (rest cons))
                 (not (eql (first cons) (second cons))))
        collect (first cons)))

;;;
;;;    2004 Soundex: (Fixed for '())
;;;    
(defun remove-pairs (list)
  (if (endp list)
      '()
      (let* ((previous (first list))
             (result (list previous)))
        (dolist (current (rest list) (nreverse result))
          (unless (eql current previous)
            (push current result))
          (setf previous current)))) )
    
(defun remove-pairs (list) ; B tail-recursive version of A
  (labels ((remove-pairs-aux (previous list result)
             (if (endp list)
                 (nreverse result)
                 (let ((current (first list)))
                   (if (eql current previous)
                       (remove-pairs-aux current (rest list) result)
                       (remove-pairs-aux current (rest list) (cons current result)))) )))
    (if (endp list)
        '()
        (remove-pairs-aux (first list) (rest list) (list (first list)))) ))

;;;
;;;    Fails for final () in list
;;;    
;; (defun remove-pairs (l)
;;   (loop for cons on l
;;         when (or (endp (rest l))
;;                  (not (eql (first cons) (second cons))))
;;         collect (first cons)))


;;;
;;;    This fails for (()) => ()
;;;    
;; (defun remove-pairs (l)
;;   (loop for cons on l
;;         unless (eql (first cons) (second cons))
;;         collect (first cons)))

;;;
;;;    Fails for list (other than single-elt (())) that
;;;    ends with (): (a b ()) => (a b)
;; (defun remove-pairs (l)
;;   (cond ((endp l) '())
;;         ((endp (rest l)) (list (first l)))
;;         (t (loop for cons on l
;;                  unless (eql (first cons) (second cons))
;;                  collect (first cons)))) )

;;;    Broken
;;;    fix????
;; (defun remove-pairs (l)
;;   (do ((list l (rest list))
;;        (first (first l) (first list))
;;        (second (second l) (second list))
;;        (result '() (if (eql first second) result (cons first result))))
;;       ((endp list) (nreverse result))))

;;;
;;;    Also fails for (())
;;;    
;; (defun remove-pairs (l)
;;   (do* ((l l (rest l))
;;         (first (first l) (first l))
;;         (second (second l) (second l))
;;         (result '() (if (eql first second) result (cons first result))))
;;        ((endp l) (nreverse result))))

;;;
;;;    Fails for (a b ())
;;;    
;; (defun remove-pairs (l)
;;   (cond ((endp l) '())
;;         ((endp (rest l)) (list (first l)))
;;         (t (do* ((l l (rest l))
;;                  (first (first l) (first l))
;;                  (second (second l) (second l))
;;                  (result '() (if (eql first second) result (cons first result))))
;;                 ((endp l) (nreverse result)))) ))

(deftest test-remove-pairs ()
  (check
   (equal (remove-pairs '()) '())
   (equal (remove-pairs '(())) '(()))
   (equal (remove-pairs '(a b ())) '(a b ()))
   (equal (remove-pairs '(a a b a b b c a a b)) '(a b a b c a b))
   (equal (remove-pairs '(a a b a b b c a a b b)) '(a b a b c a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Graham pg. 101 (and Norvig Style pg. 58) discusses the connection between DO and tail recursive functions (See also pg. 89).
;;;    Clojure draws the same conclusion wrt its 'loop' special operator.
;;;    This correspondence is obvious for simple functions that have only a single exit point:
;;;
(defun fibonacci (f0 f1 n)
  (if (zerop n)
      f0
      (fibonacci f1 (+ f0 f1) (1- n))))

;(fibonacci 0 1 8)

(defun fibonacci-do (n)
  (do ((f0 0 f1)
       (f1 1 (+ f0 f1))
       (n n (1- n)))
      ((zerop n) f0)))

;;;
;;;    Seibel pg. 86
;;;    
(defun fibonacci-seibel-do (n)
  (do ((i 0 (1+ i))
       (current 0 next)
       (next 1 (+ current next)))
      ((= i n) current)))

(defun fibonacci-seibel-tr (n i current next)
  (if (= i n)
      current
      (fibonacci-seibel-tr n (1+ i) next (+ current next))))





(defun factorial (n f)
  (if (zerop n)
      f
      (factorial (1- n) (* n f))))

;(factorial 8 1)

(defun factorial-do (n)
  (do ((n n (1- n))
       (f 1 (* f n)))
      ((zerop n) f)))

(defun factorial-1 (n)
  (loop for counter from 1 upto n
        and product = 1 then (* product counter)
        finally (return product)))

(BLOCK NIL
  (LET ((COUNTER 1)
        (#:LOOP-LIMIT-7286 N)
        (PRODUCT NIL))
    (DECLARE (TYPE (AND NUMBER REAL) #:LOOP-LIMIT-7286)
             (TYPE (AND REAL NUMBER) COUNTER))
    (TAGBODY
       (SETQ PRODUCT 1)
       (WHEN (> COUNTER #:LOOP-LIMIT-7286)
         (GO SB-LOOP::END-LOOP))
     SB-LOOP::NEXT-LOOP
       (SETQ COUNTER
             (PROG1 (1+ COUNTER)
               (SETQ PRODUCT (* PRODUCT COUNTER))))
       (WHEN (> COUNTER #:LOOP-LIMIT-7286)
         (GO SB-LOOP::END-LOOP))
       (GO SB-LOOP::NEXT-LOOP)
     SB-LOOP::END-LOOP
       (RETURN PRODUCT))))

(defun factorial-2 (n)
  (loop for counter from 0 upto n
        for product = 1 then (* product counter)
        finally (return product)))

(BLOCK NIL
  (LET ((COUNTER 0)
        (#:LOOP-LIMIT-7287 N))
    (DECLARE (TYPE (AND NUMBER REAL) #:LOOP-LIMIT-7287)
             (TYPE (AND REAL NUMBER) COUNTER))
    (LET ((PRODUCT NIL))
      (TAGBODY
         (WHEN (> COUNTER #:LOOP-LIMIT-7287)
           (GO SB-LOOP::END-LOOP))
         (SETQ PRODUCT 1)
       SB-LOOP::NEXT-LOOP
         (SETQ COUNTER (1+ COUNTER))
         (WHEN (> COUNTER #:LOOP-LIMIT-7287)
           (GO SB-LOOP::END-LOOP))
         (SETQ PRODUCT (* PRODUCT COUNTER))
         (GO SB-LOOP::NEXT-LOOP)
       SB-LOOP::END-LOOP
         (RETURN PRODUCT)))) )

(defun factorial-3 (n)
  (do ((counter 1 (1+ counter))
       (product 1 (* product counter)))
      ((> counter n) product)))

(BLOCK NIL
  (LET ((COUNTER 1)
        (PRODUCT 1))
    (TAGBODY
       (GO #:G7289)
       #:G7288
       (PSETQ COUNTER (1+ COUNTER) PRODUCT (* PRODUCT COUNTER))
       #:G7289
       (UNLESS (> COUNTER N)
         (GO #:G7288))
       (RETURN-FROM NIL (PROGN PRODUCT)))))

(defun factorial-4 (n)
  (do* ((counter 0 (1+ counter))
        (product 1 (* product counter)))
       ((>= counter n) product)))

(BLOCK NIL
  (LET* ((COUNTER 0)
         (PRODUCT 1))
    (TAGBODY
       (GO #:G7291)
       #:G7290
       (SETQ COUNTER (1+ COUNTER) PRODUCT (* PRODUCT COUNTER))
       #:G7291
       (UNLESS
           (>= COUNTER N) (GO #:G7290))
       (RETURN-FROM NIL (PROGN PRODUCT)))))

(defun factorial (n)
  (loop for i from n downto 0
        for j from 1
        and factorial = 1 then (* factorial j)
        finally (return factorial)))

(deftest test-factorial ()
  (check
   (= (factorial 0) 1)
   (= (factorial 1) 1)
   (= (factorial 2) 2)
   (= (factorial 3) 6)
   (= (factorial 6) 720)
   (= (factorial 10) (* 10 (factorial 9)))) )

;;
;;    See LENGTH-TR above
;;    
(defun length (l n)
  (if (endp l)
      n
      (length (rest l) (1+ n))))

;(length '(a b c) 0)

;;
;;    LENGTH-I above
;;    
(defun length-do (l)
  (do ((l l (rest l))
       (n 0 (1+ n)))
      ((endp l) n)))

;;;
;;;    However, the translation becomes more awkward if the function may exit in multiple places.
;;;    Furthermore, DO lacks the destructuring capabilities of Clojure's 'loop'.
;;;    (See Little Lisper ch. 3)
;;;    
(defun insert-r (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons old (cons new (rest lat))))
        (t (cons (first lat) (insert-r old new (rest lat)))) ))

(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (cond ((endp lat) (nreverse result))
                   ((eql (first lat) old) (nreverse (revappend (rest lat) (cons new (cons old result)))) )
                   (t (insert (rest lat) (cons (first lat) result)))) ))
    (insert lat '())))

(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (if (endp lat)
                 (nreverse result)
                 (destructuring-bind (head . tail) lat
                   (if (eql head old)
                       (nreverse (revappend tail (cons new (cons old result))))
                       (insert tail (cons head result)))) )))
    (insert lat '())))

;;
;;    This destructuring works w/o having to change overall function structure (i.e., COND -> IF)
;;    
(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (destructuring-bind (&optional head &rest tail) lat
               (cond ((endp lat) (nreverse result))
                     ((eql head old) (nreverse (revappend tail (cons new (cons old result)))) )
                     (t (insert tail (cons head result)))) )))
    (insert lat '())))

;;
;;    Silly verbose translation
;;    
(defun insert-r (old new lat)
  (do ((l lat (rest l))
       (result '() (cons (first l) result)))
      ((or (endp l) (eql (first l) old))
       (cond ((endp l) (nreverse result))
             ((eql (first l) old) (nreverse (revappend (rest l) (list* new old result)))) ))))

;;
;;    Less verbose awkward translation
;;    
(defun insert-r (old new lat)
  (do ((l lat (rest l))
       (result '() (cons (first l) result)))
      ((if (endp l)
           (return (nreverse result))
           (eql (first l) old))
       (nreverse (revappend (rest l) (list* new old result)))) ))

;;
;;    Another alternative.
;;    
(defun insert-r (old new lat)
  (do ((l lat (rest l))
       (result '() (cons (first l) result)))
      ((endp l) (nreverse result))
    (when (eql (first l) old)
      (return (nreverse (revappend (rest l) (list* new old result)))) )))

(deftest test-insert-r ()
  (check
   (equal (insert-r 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-r 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-r 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-r 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

;;;
;;;    Fibonacci (See SICP ch. 1)
;;;
(defun fibonacci (n)
  (let* ((sqrt5 (sqrt 5d0))
         (phi (/ (1+ sqrt5) 2))
         (phin (/ (expt phi n) sqrt5)))
    (multiple-value-call #'nearer (floor phin) (ceiling phin))))

;;;
;;;    Favors Q2 if R1 = R2.
;;;    
(defun nearer (q1 r1 q2 r2)
  (if (< (abs r1) (abs r2))
      q1
      q2))

;; (defun fibonacci (n)
;;   (let* ((sqrt5 (sqrt 5d0))
;;          (phi (/ (1+ sqrt5) 2))
;;          (phin (/ (expt phi n) sqrt5)))
;;     (if (< (- phin (floor phin)) (- (ceiling phin) phin))
;;         (floor phin)
;;       (ceiling phin))))

(defun fibonacci (n)
  (labels ((fib (a b count)
             (if (zerop count)
                 b
                 (fib (+ a b) a (1- count)))) )
    (fib 1 0 n)))

(defun fibonacci (n)
  (let ((a 1)
        (b 0))
    (dotimes (i n b)
      (psetf a (+ a b) b a))))

(deftest test-fibonacci ()
  (check
   (= (fibonacci 0) 0)
   (= (fibonacci 1) 1)
   (= (fibonacci 2) 1)
   (= (fibonacci 3) 2)
   (= (fibonacci 4) 3)
   (= (fibonacci 5) 5)
   (= (fibonacci 6) 8)
   (= (fibonacci 100) 354224848179261915075)))

;;;
;;;    Same as FIBONACCI-DO above.
;;;    
(defun fibonacci (n)
  (do ((a 1 (+ a b))
       (b 0 a)
       (count n (1- count)))
      ((zerop count) b)))

(defun fibonacci (n)
  (loop for count from n downto 0
        for a = 1 then (+ a b)
        and b = 0 then a
        finally (return b)))

;;;
;;;    Another example of an awkward correlation between DO and recursion.
;;;    See _Fundamentals Of OOP_ ARRAY-ORDERED-LIST, Graham ch. 4.
;;;    
;;
;;    Quite clear
;;    
;; (defun binary-search-tr (a target test)
;;   (labels ((bsearch (low high)
;;              (let ((mid (truncate (+ high low) 2)))
;;                (cond ((< high low) (values low nil))
;;                      ((funcall test target (aref a mid)) (bsearch low (1- mid)))
;;                      ((funcall test (aref a mid) target) (bsearch (1+ mid) high))
;;                      (t (values mid t)))) ))
;;     (bsearch 0 (1- (length a)))) )

(defconstant present t)
(defconstant absent nil)
(defun binary-search-tr (a target test)
  (labels ((bsearch (low high)
             (let* ((mid (truncate (+ high low) 2))
                    (elt (aref a mid)))
               (cond ((< high low) (values low absent))
                     ((funcall test target elt) (bsearch low (1- mid)))
                     ((funcall test elt target) (bsearch (1+ mid) high))
                     (t (values mid present)))) ))
    (bsearch 0 (1- (length a)))) )

;;
;;    A little ugly
;;    
(defun binary-search (a target test)
  (do* ((low 0 (if (funcall test target (aref a mid)) low (1+ mid)))
        (high (1- (length a)) (if (funcall test target (aref a mid)) (1- mid) high))
        (mid (truncate (+ high low) 2) (truncate (+ high low) 2)))
       ((or (< high low) (not (or (funcall test target (aref a mid)) (funcall test (aref a mid) target))))
        (if (< high low)
            (values low absent)
            (values mid present)))) )

;;;
;;;    Of course, this is the conventional way to do it
;;;    
(defun binary-search (a target test)
  (do ((low 0)
       (high (1- (length a))))
       ((< high low) (values low absent))
    (let* ((mid (truncate (+ high low) 2))
           (elt (aref a mid)))
      (cond ((funcall test target elt) (setf high (1- mid)))
            ((funcall test elt target) (setf low (1+ mid)))
            (t (return (values mid present)))) )))

;;;
;;;    Another DO vs. recursion example
;;;    See _Fundamentals Of OOP_ ORDERED-LIST
;;;    
;;;    Determine whether or not OBJ is present in ORDERED-LIST L. Return the
;;;    list elt if found, NIL otherwise.
;;;
;;;    This is really the same issue as above in BINARY-SEARCH, namely, a complex termination test requires extra
;;;    logic afterwards in order to determine why the loop finished. It may be simpler to have a simple single termination
;;;    test (some obvious end state for the loop) and otherwise return or cause the necessary side effects in the
;;;    loop body.
;;;
;;;    Another similarity between these two examples is the existence of a secondary variable which depends on the state of
;;;    the termination test. Above there is no sense in calculating MID once the HIGH and LOW indexes have crossed. Likewise,
;;;    CANDIDATE below is meaningless if the node is NIL.
;;;
;;
;;    The first version works, but the logic is a little klunky.
;;    
(defmethod get ((l ordered-list) obj)
  (with-slots (contents test) l
    (do ((current contents (cdr current)))
        ((or (null current) (not (funcall test (car current) obj)))
         (if (or (null current) (funcall test obj (car current)))
             nil
             (car current)))) ))

;;
;;    The second version introduces an error when trying to use a second loop variable.
;;    
(defmethod get ((l ordered-list) target)
  (with-slots (contents) l
    (do* ((current contents (cdr current))
          (candidate (car current) (car current))) ; Fatal when (NULL CURRENT)!
         ((or (null current) (not (less-than l candidate target)))
          (if (or (null current) (greater-than l candidate target))
              nil
              candidate)))) )

;;
;;    Pretty clean as a tail-recursive helper method.
;;    
(defmethod get ((l ordered-list) target)
  (labels ((find-elt (node)
             (if (null node)
                 nil
                 (let ((candidate (car node)))
                   (cond ((less-than l candidate target) (find-elt (cdr node)))
                         ((greater-than l candidate target) nil)
                         (t candidate)))) ))
  (with-slots (contents) l
    (find-elt contents))))

;;
;;    Not too bad using LOOP.
;;    
(defmethod get ((l ordered-list) target)
  (with-slots (contents) l
    (loop for node = contents then (cdr node)
          until (null node)
          for candidate = (car node)
          if (greater-than l candidate target)
          do (return nil)
          end
          unless (less-than l candidate target)
          do (return candidate)
          end
          finally (return nil))))

;;
;;    Logic is clearer than original version.
;;    
(defmethod get ((l ordered-list) target)
  (with-slots (contents) l
    (do ((node contents (cdr node)))
        ((null node) nil)
      (let ((candidate (car node)))
        (cond ((greater-than l candidate target) (return nil))
              ((not (less-than l candidate target)) (return candidate)))) )))
