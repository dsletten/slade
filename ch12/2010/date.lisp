;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               date.lisp
;;;;
;;;;   Started:            Sat Dec 18 19:28:47 2010
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
(load "/Users/dsletten/lisp/packages/time")

(defpackage date
  (:use common-lisp)
  (:export :make-date :date :date-day :date-month :date-year :date-day-of-week))

(in-package date)

(defstruct (date ;(:constructor construct-date)
                 (:print-object (lambda (date stream)
                                  (format stream "~A ~A ~2D ~4D"
                                          (time:get-short-day-of-week-name (date-day-of-week date))
                                          (time:get-short-month-name (date-month date))
                                          (date-day date)
                                          (date-year date)))) )
;;                  (:print-function (lambda (date stream depth)
;;                                     (declare (ignore depth))
;;                                     (format stream "~A ~A ~2D ~4D"
;;                                             (time:get-short-day-of-week-name (date-day-of-week date))
;;                                             (time:get-short-month-name (date-month date))
;;                                             (date-day date)
;;                                             (date-year date)))) )
  (day (time:get-day-of-month))
  (month (time:get-month))
  (year (time:get-year)))

;;;
;;;    MAKE-DATE is not a generic function...
;;;    
;; (defmethod make-date :after ((date date))
;;   (when (null (date-day date))
;;     (setf (date-day date) 8)))

;;;
;;;    Initially defined DATE structure to have a distinct DAY-OF-WEEK slot. This was
;;;    automatically computed based on DAY, MONTH, YEAR slots in MAKE-DATE, so dummy constructor
;;;    CONSTRUCT-DATE was used.
;;;
;;;    Now DAY-OF-WEEK is virtual slot. Read-only function DAY-OF-WEEK is just wrapper for ZELLER.
;;;    
;; (defun make-date (&key (day (time:get-date)) (month (time:get-month)) (year (time:get-year)))
;;   (let ((date (construct-date)))
;;     (setf (date-day date) day
;;           (date-month date) month
;;           (date-year date) year
;;           (date-day-of-week date) (zeller date))
;;     date))

(defun date-day-of-week (date)
  (zeller date))
  
(defgeneric zeller (date)
  (:documentation "Computes the day of week for given date using Zeller's Congruence."))

(defmethod zeller ((date date))
  (compute-zeller (date-day date)
                  (zeller-month (date-month date))
                  (date-year date)))

;;;
;;;    Can't do multiple arities of required params...
;;;    
;; (defmethod zeller ((day real) month year)
;;   (assert (realp month))
;;   (assert (realp year))
;;   (compute-zeller day month year))

(defun compute-zeller (day month year)
  (let ((century (truncate year 100))
        (decades (mod year 100))
        (leap (if (time:leap-year-p year) 1 0)))
    (shift (mod (- (+ day
                       (floor (1- (* 13 month)) 5)
                       decades
                       (floor decades 4)
                       (floor century 4))
                    (* 2 century)
                    (* (1+ leap) (floor month 11)))
                 7))))

(defun zeller-month (m)
  (if (= m 2)
      12
      (mod (+ m 10) 12)))

;;;
;;;    Must adjust 0-6:
;;;    Sunday-Saturday -> Monday-Sunday
;;;    
(defun shift (day-of-week)
 (mod (+ day-of-week 6) 7))