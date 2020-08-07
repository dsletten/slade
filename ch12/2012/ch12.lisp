;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch12.lisp
;;;;
;;;;   Started:            Fri Feb  3 09:03:44 2012
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

(defpackage :ch12 (:use :common-lisp :test) (:shadow + time))

(in-package :ch12)

;;;
;;;    12.6.5
;;;
(defstruct person
  (name "":type string)
  (age 0 :type (integer 0 120))
  (weight 0 :type (integer 0 1000))
  sex
  (children ()))

(defgeneric add-child (parent child))
(defmethod add-child ((parent person) (child person))
  (unless (member child (person-children parent))
    (push child (person-children parent))))

;;;
;;;    12.6.7
;;;
(fmakunbound '+)

(defclass time ()
  ((hours   :reader hours   :initarg :hours   :initform 0 :type (integer 0 23))
   (minutes :reader minutes :initarg :minutes :initform 0 :type (integer 0 59))
;;   (seconds :reader seconds :initarg :seconds :initform 0 :type (integer 0 59))))
   (seconds :reader seconds :initarg :seconds :initform 0 :type (integer 0 *))))

(defmethod initialize-instance :after ((time time) &rest args)
  (declare (ignore args))
;;   (check-type (hours time) (integer 0 23)) ; <-- Require writer methods.
;;   (check-type (minutes time) (integer 0 59))
;;   (check-type (seconds time) (integer 0 59)))
  (check-type (hours time) (integer 0 *))
  (check-type (minutes time) (integer 0 *))
  (check-type (seconds time) (integer 0 *))
  (normalize time))

(defmethod print-object ((time time) stream)
  (print-unreadable-object (time stream :type t)
    (format stream "~2,'0D:~2,'0D:~2,'0D" (hours time) (minutes time) (seconds time))))

(defgeneric + (x y &rest more))
(defmethod + ((x number) (y number) &rest more)
  (if (null more)
      (cl:+ x y)
      (apply #'+ (+ x y) more)))

(defmethod + ((time time) (elapsed-seconds number) &rest more)
  (if (null more)
      (make-instance 'time :hours (hours time) :minutes (minutes time) :seconds (+ (seconds time) elapsed-seconds))
      (apply #'+ (+ time elapsed-seconds) more)))
(defmethod + ((elapsed-seconds number) (time time) &rest more)
  (apply #'+ time elapsed-seconds more))

;; (defmethod + ((time time) (elapsed-seconds number))
;;   (let ((new-time (make-instance 'time :hours (hours time) :minutes (minutes time) :seconds (seconds time))))
;;     (with-slots (hours minutes seconds) new-time
;;       (setf seconds (+ seconds elapsed-seconds))
;;       (when (> seconds 59)
;;         (multiple-value-bind (min sec) (truncate seconds 60)
;;           (setf seconds sec
;;                 minutes (+ minutes min))
;;           (when (> minutes 59)
;;             (multiple-value-bind (hrs min) (truncate minutes 60)
;;               (setf minutes min
;;                     hours (mod (+ hours hrs) 24)))) )))
;;     new-time))

(defmethod + ((l1 list) (l2 list) &rest more)
  (if (null more)
      (append l1 l2)
      (apply #'+ (+ l1 l2) more)))

(defmethod + ((s1 string) (s2 string) &rest more)
  (if (null more)
      (concatenate 'string s1 s2)
      (apply #'+ (+ s1 s2) more)))

(defmethod + ((s1 symbol) (s2 symbol) &rest more)
  (apply #'+ (symbol-name s1) (symbol-name s2) more))

(defmethod + ((s1 symbol) (s2 string) &rest more)
  (apply #'+ (symbol-name s1) s2 more))

(defmethod + ((s1 string) (s2 symbol) &rest more)
  (apply #'+ s1 (symbol-name s2) more))

(defmethod + ((ch1 character) (ch2 character) &rest more)
  (apply #'+ (string ch1) (string ch2) more))

(defmethod + ((ch character) (s string) &rest more)
  (apply #'+ (string ch) s more))

(defmethod + ((s string) (ch character) &rest more)
  (apply #'+ s (string ch) more))

(defgeneric normalize (time))
(defmethod normalize ((time time))
  (with-slots (hours minutes seconds) time
    (when (> seconds 59)
      (multiple-value-bind (min sec) (truncate seconds 60)
        (setf seconds sec
              minutes (+ minutes min))
        (when (> minutes 59)
          (multiple-value-bind (hrs min) (truncate minutes 60)
            (setf minutes min
                  hours (mod (+ hours hrs) 24)))) ))))
;;;
;;;    12.6.7 take II
;;;
(fmakunbound '+)

(defclass time ()
  ((hours   :reader hours   :initarg :hours   :initform 0 :type (integer 0 23))
   (minutes :reader minutes :initarg :minutes :initform 0 :type (integer 0 59))
   (seconds :reader seconds :initarg :seconds :initform 0 :type (integer 0 *))))

(defmethod initialize-instance :after ((time time) &rest args)
  (declare (ignore args))
  (check-type (hours time) (integer 0 *))
  (check-type (minutes time) (integer 0 *))
  (check-type (seconds time) (integer 0 *))
  (normalize time))

(defmethod print-object ((time time) stream)
  (print-unreadable-object (time stream :type t)
    (format stream "~2,'0D:~2,'0D:~2,'0D" (hours time) (minutes time) (seconds time))))

(defun + (x &rest more)
  (if (null more)
      x
      (apply #'+ (add x (first more)) (rest more))))

(defgeneric add (x y))
(defmethod add ((x number) (y number))
  (cl:+ x y))

(defmethod add ((time time) (elapsed-seconds number))
  (make-instance 'time :hours (hours time) :minutes (minutes time) :seconds (+ (seconds time) elapsed-seconds)))

(defmethod add ((elapsed-seconds number) (time time))
  (add time elapsed-seconds))

(defmethod add ((l1 list) (l2 list))
  (append l1 l2))

(defmethod add ((s1 string) (s2 string))
  (concatenate 'string s1 s2))

(defmethod add ((s1 symbol) (s2 symbol))
  (add (symbol-name s1) (symbol-name s2)))

(defmethod add ((s1 symbol) (s2 string))
  (add (symbol-name s1) s2))

(defmethod add ((s1 string) (s2 symbol))
  (add s1 (symbol-name s2)))

(defmethod add ((ch1 character) (ch2 character))
  (add (string ch1) (string ch2)))

(defmethod add ((ch character) (s string))
  (add (string ch) s))

(defmethod add ((s string) (ch character))
  (add s (string ch)))

(defmethod add ((ch character) (s symbol))
  (add (string ch) s))

(defmethod add ((s symbol) (ch character))
  (add s (string ch)))

(defgeneric normalize (time))
(defmethod normalize ((time time))
  (with-slots (hours minutes seconds) time
    (when (> seconds 59)
      (multiple-value-bind (min sec) (truncate seconds 60)
        (setf seconds sec
              minutes (+ minutes min))
        (when (> minutes 59)
          (multiple-value-bind (hrs min) (truncate minutes 60)
            (setf minutes min
                  hours (mod (+ hours hrs) 24)))) ))))

(deftest test-+ ()
  (check
   (equal (+ "pung" "foo" "bar") "pungfoobar")
   (equal (+ 'pung 'foo 'bar) "PUNGFOOBAR")
   (equal (+ 1 2 3 4 5) 15)
   (equal (+ 'one 'two 'three) "ONETWOTHREE")
   (equal (+ #\a #\b #\c #\d) "abcd")
   (equal (+ 'one "two" #\3) "ONEtwo3")
   (equal (+ 'one #\2 "three") "ONE2three")
   (= (minutes (+ (make-instance 'time) 50 10)) 1)
   (= (hours (+ (make-instance 'time) 50 10 3600)) 1)
   (= (hours (+ (make-instance 'time) 50 10 3600 7200)) 3)
   (= (hours (+ 3600 (make-instance 'time) 50 10)) 1)))
