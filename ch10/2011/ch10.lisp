;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch10.lisp
;;;;
;;;;   Started:            Thu Dec 29 20:16:54 2011
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
(load "/Users/dsletten/lisp/packages/time.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch10 (:use :common-lisp :test :time) (:shadow :error :string-equal :type))

(in-package :ch10)

;;;
;;;    10.6.1
;;;
(defun error (control &rest args)
  (apply #'format *error-output* (concatenate 'string "~%** ErOrR: " control) args)
  (break ""))
;  (apply #'break (concatenate 'string "** ErOrR: " control) args))

;;;
;;;    10.6.2
;;;
(defun factorial (n)
  (check-type n (integer 0))
  (labels ((fact (n)
             (case n
               ((0 1) 1)
               (otherwise (* n (fact (1- n)))) )))
    (fact n)))

;;;
;;;    10.6.4
;;;
;;
;;    1.
;;    
(defun leap-year-p (year)
  (check-type year (integer 1582))
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(deftest test-leap-year-p ()
  (check
   (leap-year-p 2000)
   (leap-year-p 1996)
   (leap-year-p 1972)
   (not (leap-year-p 1900))
   (not (leap-year-p 2009))))

;;
;;    2., 3.
;;    
(defun make-person (name age weight sex children)
  (check-type name symbol)
  (check-type age (integer 0 120))
  (check-type weight (integer 0 1000))
  (check-type sex (member male female))
  (check-type children list)
  (pairlis '(name age weight sex children) (list name age weight sex children)))

(defun make-person (&rest args)
  (destructuring-bind (name age weight sex children) args
    (check-type name symbol)
    (check-type age (integer 0 120))
    (check-type weight (integer 0 1000))
    (check-type sex (member male female))
    (check-type children list)
    (pairlis '(name age weight sex children) (list name age weight sex children))))

(defun get-name (person) (cdr (assoc 'name person)))
(defun get-age (person) (cdr (assoc 'age person)))
(defun get-weight (person) (cdr (assoc 'weight person)))
(defun get-sex (person) (cdr (assoc 'sex person)))
(defun get-children (person) (cdr (assoc 'children person)))

(defclass person ()
  ((name :accessor name :initarg :name :type 'string)
   (age :accessor age :initarg :age :type 'number)
   (weight :accessor weight :initarg :weight :type 'number)
   (sex :accessor sex :initarg :sex :type 'sex)
   (children :accessor children :initarg :children :initform '())))

(defmethod initialize-instance :after ((p person) &rest init-args)
  (declare (ignore init-args))
  (check-type (name p) string)
  (check-type (age p) (integer 0 120))
  (check-type (weight p) (integer 0 1000))
  (check-type (sex p) sex)
  (check-type (children p) list))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~A sex: ~S age: ~A" (name p) (sex p) (age p))))

(defgeneric add-child (person child))

(defmethod add-child ((p person) (c person))
  (setf (children p) (cons c (children p))))

(defclass sex ()
  ((type :reader type :initarg :type)))

(defmethod initialize-instance :after ((s sex) &rest init-args)
  (declare (ignore init-args))
  (with-slots (type) s
    (check-type type (member male female))))

(defmethod print-object ((s sex) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~A" (type s))))

;;
;;    4.
;;    
(defvar *months* '((january 11)
                   (february 12)
                   (march 1)
                   (april 2)
                   (may 3)
                   (june 4)
                   (july 5)
                   (august 6)
                   (september 7)
                   (october 8)
                   (november 9)
                   (december 10)))

(defun daughter-of-zeller (month day year)
  (check-type year (integer 1582))
  (assert (typep (second (assoc month *months*)) '(integer 1 12)))
  (let* ((conventional-month-index (mod (+ (second (assoc month *months*)) 2) 13))
         (month-length (month-length conventional-month-index year)))
    (assert (typep day `(integer 1 ,month-length))
            (day)
            "Day should be between 1 and ~D." month-length))
  (get-day-of-week-name (son-of-zeller day (second (assoc month *months*)) year)))

;;
;;    Using ZELLER in the TIME package since the indices are off here...
;;    
(defun son-of-zeller (day month year)
  (time::zeller day month (truncate year 100) (rem year 100) (if (leap-year-p year) 1 0)))
 
(defun zeller (n m c y l)
  (mod (- (+ n
             (cl:floor (1- (* 13 m)) 5)
             y
             (cl:floor y 4)
             (cl:floor c 4))
          (* 2 c)
          (* (1+ l) (cl:floor m 11)))
       7))

(deftest test-daughter-of-zeller ()
  (check
   (equal (daughter-of-zeller 'september 1 1996) "Sunday")
   (equal (daughter-of-zeller 'september 2 1996) "Monday")
   (equal (daughter-of-zeller 'september 3 1996) "Tuesday")
   (equal (daughter-of-zeller 'september 4 1996) "Wednesday")
   (equal (daughter-of-zeller 'september 5 1996) "Thursday")
   (equal (daughter-of-zeller 'september 6 1996) "Friday")
   (equal (daughter-of-zeller 'september 7 1996) "Saturday")
   (equal (daughter-of-zeller 'september 8 1996) "Sunday")))

;;
;;    5. (Actually exercise 4.7.4)
;;
(defclass coin ()
  ((value :reader value :initarg :value)
   (name :reader name :initarg :name)
   (plural :reader plural :initarg :plural)))

(defmethod print-object ((c coin) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~A ~D" (name c) (value c))))

(defun make-coin (value name plural)
  (make-instance 'coin :value value :name name :plural plural))

(defgeneric coin-quantity (coin count))
  
(defmethod coin-quantity ((c coin) (count integer))
  (if (= count 1)
      (list count (name c))
      (list count (plural c))))

(defvar *coin-list* '((1 penny pennies)
                      (100 dollar dollars)
                      (50 half-dollar half-dollars)
                      (25 quarter quarters)
                      (5 nickel nickels)
                      (10 dime dimes)))

(defvar *us-currency* (sort (mapcar #'(lambda (entry) (apply #'make-coin entry)) *coin-list*) #'(lambda (x y) (> (value x) (value y)))) )

;;;
;;;    CURRENCY-LIST must be sorted in decreasing order.
;;;    
(defun make-change (money currency-list)
  (check-type money (integer 0))
  (labels ((make-change-aux (money currency-list)
             (cond ((zerop money) '())
                   ((endp currency-list) '())
                   ((<= (value (first currency-list)) money)
                    (multiple-value-bind (quot rem)
                        (truncate money (value (first currency-list)))
                      (cons (coin-quantity (first currency-list) quot)
                            (make-change-aux rem (rest currency-list)))) )
                   (t (make-change-aux money (rest currency-list)))) ))
    (make-change-aux money currency-list)))

(deftest test-make-change ()
  (check
   (equal (make-change 94 *us-currency*) '((1 HALF-DOLLAR) (1 QUARTER) (1 DIME) (1 NICKEL) (4 PENNIES)))
   (equal (make-change 372 *us-currency*) '((3 DOLLARS) (1 HALF-DOLLAR) (2 DIMES) (2 PENNIES)))) )

;;
;;    6.
;;
(defun check-book (balance transaction-list)
  (check-type balance real)
  (check-type transaction-list list)
  (process-transaction-list balance transaction-list))

(defun process-transaction-list (balance transaction-list)
  (if (endp transaction-list)
      balance
      (process-transaction-list (process balance (first transaction-list)) (rest transaction-list))))

(defun transactionp (obj)
  (or (numberp obj)
      (typep obj '(cons (real 0) null))))

(deftype transaction ()
  '(or real interest-rate))

(deftype interest-rate ()
  '(cons (real 0) null))

;;;
;;;    Don't need to keep rechecking BALANCE? If initially valid should always be.
;;;    
(defun process (balance transaction)
;  (assert (transactionp transaction) (transaction) "Invalid transaction: ~A" transaction)
  (check-type transaction transaction)
  (typecase transaction
    (number (+ balance transaction))
    (interest-rate (* balance (first transaction)))) )

(deftest test-check-book ()
  (check
   (= (check-book 100 '(100 50 -75)) 175)
   (= (check-book 100 '(-17.50 -1.73 -7.5)) 73.27)
   (= (check-book 100 '(100 50 -50 (1.1))) 220.0)
   (= (check-book 100 '((1.1) 100 50 -50 (1.1))) 231.0)
   (eq (type-of (handler-case (check-book 100 -17.50)
                  (error (e) (print e))))
       'simple-error)
   (eq (type-of (handler-case (check-book 'balance '(-17.50 -1.73 -7.5))
                  (error (e) (print e))))
       'simple-error)))

;;
;;    7.
;;    
(defparameter *penalty* 0.1)
(defparameter *minimum-balance* 500)

(defun now-account (balance transaction-list)
  (check-type balance real)
  (check-type transaction-list list)
  (process-now-account-transaction-list balance transaction-list))

(defun process-now-account-transaction-list (balance transaction-list)
  (if (endp transaction-list)
      balance
      (process-now-account-transaction-list (process-now-account-transaction balance (first transaction-list)) (rest transaction-list))))

(defun process-now-account-transaction (balance transaction)
  (check-type transaction transaction)
  (typecase transaction
    (number (if (and (minusp transaction)
                     (< balance *minimum-balance*))
                (+ balance transaction (- *penalty*))
                (+ balance transaction)))
    (interest-rate (if (> balance *minimum-balance*)
                       (* balance (first transaction))
                       balance))))

(deftest test-now-account ()
  (check
   (= (now-account 100 '(100 50 -75)) 174.9)
   (= (now-account 100 '(-17.50 -1.73 -7.5)) 72.97)
   (= (now-account 100 '(100 50 -50 (1.1))) 199.9)
   (= (now-account 500 '(100 50 -50 (1.1))) 660.0)
   (= (now-account 100 '((1.1) 100 50 -50 (1.1))) 199.9)
   (eq (type-of (handler-case (now-account 100 -17.50)
                  (error (e) (print e))))
       'simple-error)
   (eq (type-of (handler-case (now-account 'balance '(-17.50 -1.73 -7.5))
                  (error (e) (print e))))
       'simple-error)))

;;
;;    9. (Many other implementations in the ch. 6 file)
;;    - Error messages here cribbed from Allegro.
;;
(defun string-equal (s1 s2 &key (start1 0) end1 (start2 0) end2)
  (check-type s1 string)
  (check-type s2 string)
  (assert (typep start1 `(integer 0 (,(length s1)))) (start1) "~S specifies invalid start position ~S" 'start1 start1)
  (assert (typep end1 `(or null (integer 0 ,(length s1))) ) (end1) "~S specifies invalid end position ~S" 'end1 end1)
  (assert (typep start2 `(integer 0 (,(length s1)))) (start2) "~S specifies invalid start position ~S" 'start2 start2)
  (assert (typep end2 `(or null (integer 0 ,(length s1))) ) (end2) "~S specifies invalid end position ~S" 'end2 end2)
  (assert (or (null end1) (<= start1 end1)) (start1 end1) "~S exceeds ~S" 'start1 'end1)
  (assert (or (null end2) (<= start2 end2)) (start2 end2) "~S exceeds ~S" 'start2 'end2)
  (let ((end1 (or end1 (length s1)))
        (end2 (or end2 (length s2))))
    (if (= (- end1 start1) (- end2 start2))
        (dotimes (i (- end1 start1) t)
          (unless (char-equal (char s1 (+ i start1)) (char s2 (+ i start2)))
            (return nil)))
        nil)))

;;
;;    10.
;;
(defun roman->arabic (roman-string)
  (check-type roman-string string) ; This is all we really need to check. Invalid chars simply won't be recognized...
  (let ((roman-list (to-roman-list roman-string)))
    (if (recognize roman-list)
        (convert roman-list)
        'invalid-roman-numeral)))

(defun to-roman-list (s)
  (map 'list #'(lambda (ch) (intern (string (char-upcase ch)))) s))

(defun recognize (roman-list &optional (state (get-state 0)))
  (if (endp roman-list)
      t
      (let ((next (transition state (first roman-list))))
        (if (null next)
            nil
            (recognize (rest roman-list) next)))) )

(defun convert (roman-list)
  (cond ((endp roman-list) 0)
        ((endp (rest roman-list)) (roman-value (first roman-list)))
        (t (destructuring-bind (this next . rest) roman-list
             (if (< (roman-value this) (roman-value next))
                 (+ (- (roman-value this))
                    (convert (rest roman-list)))
                 (+ (roman-value this)
                    (convert (rest roman-list)))) ))))

(defconstant roman-numeral-alist '((i 1)
                                   (v 5)
                                   (x 10)
                                   (l 50)
                                   (c 100)
                                   (d 500)
                                   (m 1000)))

(defun roman-value (roman)
  (second (assoc roman roman-numeral-alist)))

(defconstant state-machine '((0 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m2))
                             (i1 (i i2) (v v1) (x x1))
                             (i2 (i i3))
                             (i3)
                             (i4 (i i2))
                             (v1)
                             (v2 (i i4))
                             (x1)
                             (x2 (i i1) (v v2) (x x3) (l l1) (c c1))
                             (x3 (i i1) (v v2) (x x4))
                             (x4 (i i1) (v v2))
                             (l1 (i i1) (v v2))
                             (l2 (i i1) (v v2) (x x2))
                             (c1 (i i1) (v v2))
                             (c2 (i i1) (v v2) (x x2) (l l2) (c c3) (d d1) (m m1))
                             (c3 (i i1) (v v2) (x x2) (l l2) (c c4))
                             (c4 (i i1) (v v2) (x x2) (l l2))
                             (d1 (i i1) (v v2) (x x2) (l l2))
                             (d2 (i i1) (v v2) (x x2) (l l2) (c c2))
                             (m1 (i i1) (v v2) (x x2) (l l2))
                             (m2 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m3))
                             (m3 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2) (m m4))
                             (m4 (i i1) (v v2) (x x2) (l l2) (c c2) (d d2))))

(defclass state ()
  ((name :reader name :initarg :name)
   (transition-map :accessor transition-map :initform (make-hash-table))))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "~A" (name state))))

(defun make-state (name)
  (make-instance 'state :name name))

(defgeneric add-transition (state transition))
(defmethod add-transition ((state state) (transition list))
  (setf (gethash (first transition) (transition-map state)) (second transition)))

(defgeneric transition (state input))
(defmethod transition ((state state) (input symbol))
  (get-state (gethash input (transition-map state))))

(defvar *state-map* (make-hash-table))
(defun initialize ()
  (dolist (row state-machine)
    (destructuring-bind (name . transitions) row
      (let ((state (get-state name)))
        (when (null state)
          (setf state (make-state name)
                (get-state name) state))
        (dolist (transition transitions)
          (add-transition state transition)))) ))

(defun get-state (name)
  (gethash name *state-map*))

(defun (setf get-state) (state name)
  (setf (gethash name *state-map*) state))

;;
;;    11.
;;    
(defun transpose-char (s i)
  (check-type s string)
  (assert (typep i `(integer 0 (,(1- (length s)))) ) (i) "~S is an invalid index" i)
  (let ((result (copy-seq s)))
    (rotatef (char result i) (char result (1+ i)))
    result))

;;
;;    12.
;;    
(defun insert-char (s n)
  (check-type s string)
  (assert (typep n `(integer 0 ,(length s))) (n) "~S is an invalid index" n)
  (loop for ch from (char-code #\a) to (char-code #\z)
        collect (with-output-to-string (result)
                  (dotimes (i (1+ (length s)))
                    (when (= i n)
                      (write-char (code-char ch) result))
                    (when (< i (length s))
                      (write-char (char s i) result)))) ))

;;
;;    14.
;;
(defconstant soundex-length 4)

(defun char-value (ch)
  (check-type ch character)
  (case (char-downcase ch)
    ((#\b #\f #\p #\v) 1)
    ((#\c #\g #\j #\k #\q #\s #\x #\z) 2)
    ((#\d #\t) 3)
    ((#\l) 4)
    ((#\m #\n) 5)
    ((#\r) 6)
    (t nil)))

(defun remove-pairs (l)
  (check-type l list)
  (loop for cons on l
        when (or (endp (rest cons))
                 (not (eql (first cons) (second cons))))
        collect (first cons)))

(defun compress-string (s)
  (check-type s string)
  (if (zerop (length s))
      s
      (with-output-to-string (result)
        (do ((i 0 (1+ i))
             (j 1 (1+ j)))
            ((= j (length s)) (write-char (char s i) result))
          (unless (char= (char s i) (char s j))
            (write-char (char s i) result)))) ))

(defun get-soundex-sequence (word)
  (let ((result (remove-pairs (map 'list #'char-value word))))
    (if (null (first result))
        (pack-soundex-sequence word (remove nil result))
        (pack-soundex-sequence word (rest (remove nil result)))) ))

(defun pack-soundex-sequence (word codes)
  (let ((result (make-string soundex-length :initial-element #\0)))
    (setf (char result 0) (char word 0))
    (setf (subseq result 1) (mapcar #'digit-char codes))
    result))

(defun soundex (word)
  (check-type word string)
  (get-soundex-sequence (clean word)))

(defun clean (s)
  (compress-string (remove-if #'(lambda (ch)
                                  (member ch '(#\H #\W)))
                              (string-upcase (remove-if-not #'alpha-char-p s))
                              :start 1)))
