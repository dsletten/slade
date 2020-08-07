;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               baseball.lisp
;;;;
;;;;   Started:            Sat Jan 28 03:28:46 2012
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/time.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :baseball (:use :common-lisp :lang :test :time) (:shadow :merge))

(in-package :baseball)

(defclass game ()
  ((date :reader date :initarg :date)
   (home :reader home :initarg :home)
   (visitor :reader visitor :initarg :visitor)
   (score :accessor score :initarg :score)
   (next :accessor next :initarg :next)))

(defmethod print-object ((g game) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A: ~A (~D) vs. ~A (~D)" (legal-date (date g)) (name (home g)) (home (score g))
            (name (visitor g)) (visitor (score g)))))

;; (defclass date ()
;;   ((day :accessor day :initarg :day)
;;    (month :accessor month :initarg :month)
;;    (year :accessor year :initarg :year)))

(defclass score ()
  ((home :accessor home :initarg :home)
   (visitor :accessor visitor :initarg :visitor)))

(defclass team ()
  ((name :accessor name :initarg :name)
   (won :accessor won :initarg :won)
   (lost :accessor lost :initarg :lost)
   (average :accessor average)
   (behind :accessor behind :initform nil)))

(defmethod initialize-instance :after ((team team) &rest args)
  (compute-average team))

(defmethod (setf won) :after ((wins integer) (team team))
  (declare (ignore wins))
  (compute-average team))

(defmethod (setf lost) :after ((losses integer) (team team))
  (declare (ignore losses))
  (compute-average team))

(defmethod print-object ((team team) stream)
  (print-unreadable-object (team stream :type t)
    (format stream "~A ~D/~D (~F)" (name team) (won team) (lost team) (average team))))

(defgeneric compute-average (team))
(defmethod compute-average ((team team))
  (setf (average team) (normalize (/ (won team) (+ (won team) (lost team)))) ))
;  (setf (average team) (coerce (/ (won team) (+ (won team) (lost team))) 'double-float)))

(defun normalize (average)
  (/ (round (* average 1000)) 1000d0))

(defclass league ()
  ((leader :accessor leader :initarg :leader)
   (teams :accessor teams :initarg :teams)
   (games :accessor games :initform '())))

(defmethod initialize-instance :after ((l league) &rest args)
  (declare (ignore args))
  (compute-leader l))

(defmethod print-object ((l league) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~%Team ~12TWon Lost Average Games Behind~%")
    (dolist (team (teams l))
      (format stream "~A ~12T~A ~16T~A ~21T ~4,3F ~29T~A~%" (name team) (won team) (lost team) (average team) (behind team)))) )

(defgeneric compute-leader (league))
(defmethod compute-leader ((l league))
  (setf (teams l) (sort (teams l) #'> :key #'average)) ; !!!!!!!!!!!!!!
;  (setf (teams l) (sort (teams l) #'(lambda (t1 t2) (> (average t1) (average t2)))) )
  (let ((leader (first (teams l))))
    (setf (leader l) leader)
    (dolist (team (teams l))
      (setf (behind team) (games-behind team leader)))) )

(defgeneric games-behind (team1 team2))
(defmethod games-behind ((a team) (b team))
  (/ (- (- (won b) (lost b))
        (- (won a) (lost a)))
     2))
  
;; (defun round-average (avg)
;;   (format nil "~F" 

;;;
;;;    12.6.1
;;;
(defun merge (l1 l2)
  (cond ((endp l1) l2)
        ((endp l2) l1)
        (t (destructure ((h1 . t1) l1
                         (h2 . t2) l2)
             (if (inorderp h1 h2)
                 (cons h1 (merge t1 l2))
                 (cons h2 (merge l1 t2)))) )))

;; (defun merge (l1 l2)
;;   (cond ((endp l1) l2)
;;         ((endp l2) l1)
;;         (t (destructuring-bind (h1 . t1) l1
;;              (destructuring-bind (h2 . t2) l2
;;                (if (inorderp h1 h2)
;;                    (cons h1 (merge t1 l2))
;;                    (cons h2 (merge l1 t2)))) ))))

(fmakunbound 'inorderp)
(defun inorderp (obj1 obj2)
  (typecase obj1
    (number (<= obj1 obj2))
    (string (string<= obj1 obj2))
    (symbol (inorderp (symbol-name obj1) (symbol-name obj2)))
    (character (char<= obj1 obj2))
    (team (inorderp (average obj1) (average obj2)))))

(defun merge-sort (l)
  (cond ((endp l) '())
        ((endp (rest l)) l)
        (t (multiple-value-bind (l1 l2) (partition l)
             (merge (merge-sort l1) (merge-sort l2)))) ))

(defun partition (l)
  (labels ((partition-aux (l odds evens)
             (cond ((endp l) (values odds evens))
                   (t (partition-aux (rest l) (cons (first l) evens) odds)))) )
    (partition-aux l '() '())))

;(merge-sort (coerce (lang:shuffle (coerce #[1 10] 'vector)) 'list))
;(merge-sort (coerce (lang:shuffle (coerce (mapcar #'digit-char #[1 9]) 'vector)) 'list))

;;;
;;;    12.6.2
;;;
(fmakunbound 'inorderp)
(defgeneric inorderp (obj1 obj2))
(defmethod inorderp ((x number) (y number))
  (<= x y))

(defmethod inorderp ((s1 string) (s2 string))
  (string<= s1 s2))

(defmethod inorderp ((s1 symbol) (s2 symbol))
  (inorderp (symbol-name s1) (symbol-name s2)))

(defmethod inorderp ((ch1 character) (ch2 character))
  (char<= ch1 ch2))

(defmethod inorderp ((t1 team) (t2 team))
  (inorderp (average t1) (average t2)))

;; (defmethod inorderp :before (obj1 obj2)
;;   (format t "Checking order of ~A (~S) and ~A (~S)~%" obj1 (class-name (class-of obj1)) obj2 (class-name (class-of obj2))))

;;;
;;;    12.6.4
;;;
(defgeneric add-game (league game))
(defmethod add-game ((l league) (g game))
  (setf (games l) (sort (adjoin g (games l)) #'precedesp :key #'date))
  (let ((score (score g)))
    (cond ((> (home score) (visitor score)) (incf (won (home g))) (incf (lost (visitor g))))
          ((< (home score) (visitor score)) (incf (won (visitor g))) (incf (lost (home g))))
          (t tie?)))
  (compute-leader l))

