;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               dpsq.lisp
;;;;
;;;;   Started:            Fri Oct 18 23:28:30 2019
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
;;;;   Notes: Consolidated from ch11.lisp
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :dpsq (:use :common-lisp :lang :test))

(in-package :dpsq)

;;;
;;;    11.10.8
;;;
;; (defmacro dotuples (((&rest vars) l) &body body)
;;   `(loop for ,vars on ,l by #'(lambda (l) (nthcdr ,(length vars) l))
;;          do ,@body))

;(dotuples ((p q) '(1 2 3 4)) (print (list p q)) collect (vector p q))

(defun dps (symbol &rest props)
  (dotuples ((property value) props)
    (setf (get symbol property) value)))

;; (defun dps (symbol &rest props)
;;   (loop for (property value) on props by #'cddr
;;         do (setf (get symbol property) value)))

;; (defmacro dpsq (symbol &rest properties)
;;   (let ((props (mapcar #'(lambda (elt) (list 'quote elt)) properties)))
;;     `(dps ',symbol ,@props)))

;;;
;;;    Slade's version. D'oh!
;;;    
(defmacro dpsq (&rest args)
  `(apply #'dps ',args))

;;;
;;;    11.10.9
;;;
(defun load-db (db)
  (dolist (entry db)
    (apply #'dps entry)))

(defvar *db* '((jane isa programmer sex female income 60k)
               (john isa programmer sex male ingests junk-food)
               (programmer isa person income 50k)
               (person isa mammal)
               (mammal isa organism)
               (organism ingests (food air))))

(defun isa-get (entity property)
  (cond ((null entity) nil)
        ((get entity property))
        (t (isa-get (get entity 'isa) property))))

;;;
;;;    Slade's version
;;;    
(defun isa-get (node property)
  (cond ((get node property))
        ((get node 'isa) (isa-get (get node 'isa) property))
        (t nil)))

(defun isa-get (node property)
  (let ((value (get node property)))
    (if value
        value
        (let ((isa-node (get node 'isa)))
          (if isa-node
              (isa-get isa-node property)
              nil)))) )


;;;
;;;    11.10.10
;;;
(defvar *db2* '((isa +invert-onto instances)
                (instances +multiple-values t)
                (*relationship +multiple-values t)
                (relationship +save-property t)
                (children isa *relationship)
                (sibling isa *relationship +invert-value t)
                (father isa relationship)
                (spouse isa relationship +invert-value t)
                (joe-jr father joe-sr spouse mary children pat children sue)))

;;;
;;;    Pretty-print property list
;;;    
(defun ppp (symbol)
  (format t "~A~%" (symbol-name symbol))
  (dotuples ((property value) (symbol-plist symbol))
    (format t "~4@T~A~20T~A~%" property value)))

(defmacro pppq (symbol)
  `(ppp ',symbol))

(defun dps (symbol &rest props)
  (dotuples ((property value) props)
    (ddput symbol property value)))

(defun ddput (symbol property value)
  (let ((special-sub-properties (get-special-properties property)))
    (if (null special-sub-properties)
        (setf (get symbol property) value)
        (dolist (special-property special-sub-properties)
          (unless (null (get property special-property))
            (process-special-property symbol property value special-property)))) ))

;;;
;;;    +multiple-values and +save-property are mutually exclusive
;;;
;;;    Sibling inverts individual values but not other siblings. I.e., (dpsq mary sibling arthur sibling dorothy)
;;;    mary -> sibling (arthur dorothy)
;;;    arthur -> sibling mary ; Missing dorothy
;;;    dorothy -> sibling mary ; Missing arthur
;;;
;;;    +save-property only saves 1 property. E.g., change father, change spouse -> only old spouse is preserved.
;;;    Changing father does not remove individual from CHILDREN property of father.
;;;    
(defun process-special-property (symbol property value special-property)
;(print (list symbol property value special-property))
  (case special-property
;    (isa (ddput symbol (get property special-property) value)
    (isa (let ((special-sub-properties (get-special-properties (get property special-property))))
           (if (null special-sub-properties)
               (setf (get symbol property) value)
               (dolist (special-sub-property special-sub-properties)
                 (unless (null (get (get property special-property) special-sub-property)) ;?!
                   (process-special-property symbol property value special-sub-property)))) ))
    (+invert-onto (setf (get symbol property) value)
                  (ddput value (get property special-property) symbol))
    (+multiple-values (setf (get symbol property) (add-to-multi-valued-property (get symbol property) value)))
    (+save-property (let ((val (get symbol property)))
                      (if (null val)
                          (setf (get symbol property) value)
                          (let ((save-node (intern (symbol-name (gensym "SAVE")))) )
                            (setf (get save-node property) val
                                  (get symbol 'save-node) save-node
                                  (get symbol property) value)))) )
    (+invert-value (let ((saved (get symbol 'save-node)))
                     (when (get saved property)
                       (remprop (get saved property) property)))
                   (setf ;(get symbol property) value
                    (get value property) symbol))
    (+invert-property (process-special-property property value symbol '+multiple-values))
    (+lambda-property (funcall (get property '+lambda-property) symbol property value))))

(defun add-to-multi-valued-property (current new)
  (cond ((null current) new)
        ((listp current) (adjoin new current))
        ((eql current new) current)
        (t (list new current))))

(defun special-property-p (property)
  (member property '(isa +invert-onto +multiple-values +save-property +invert-value +invert-property +lambda-property)))

(defun get-special-properties (node)
  (let ((special-properties '()))
    (dotuples ((property _) (symbol-plist node))
      (when (special-property-p property)
        (push property special-properties)))
    special-properties))

(defvar *db3* '((mary sibling dorothy sibling arthur)
                (joe-jr spouse louise children jackie)))
(defvar *db4* '((job +invert-property t +multiple-values t)
                (plumber isa job)
                (carpenter isa job)
                (joe-jr job plumber job carpenter)))

(defvar *db5* `((dps 'son '+lambda-property ,#'(lambda (node prop val) (ddput node 'children val) (ddput val 'sex 'male) (ddput val 'father node)))
                (dpsq joe-jr son lester)
                (dpsq joe-sr son joe-jr)))

;;;
;;;    "Fix" Slade. Still not quite right.
;;;    
;; (defun ddput (node prop val)
;;   (print (list node prop val))
;;   (when (isa-get prop '+invert-property) ; This does strange things like (ddput 'carpenter 'joe-jr 'job)
;;     (ddput prop val node))
;;   (when (isa-get prop '+invert-value)
;;     (*put val prop node))
;;   (let ((onto (isa-get prop '+invert-onto)))
;;     (when onto
;;       (ddput val onto node)))
;;   (cond ((isa-get prop '+multiple-values) (add-property node prop val))
;;         ((and (get node prop)
;;               (isa-get prop '+save-property))
;;          (*put (or (get node 'save-node)
;;                    (ddput node 'save-node (gensym "SAVE")))
;;                prop
;;                (get node prop))
;;          (*put node prop val))
;;         (t (let ((fn (isa-get prop '+lambda-property)))
;;              (if fn
;;                  (funcall fn node prop val)
;;                  (*put node prop val)))) ))

;; (defun *put (node prop val)
;;   (when prop
;;     (setf (get node '+ddprops) (enter (get node '+ddprops) prop)
;;           (get node prop) val)))

;; (defun add-property (node prop val)
;;   (*put node prop (enter (get node prop) val)))

;; (defun enter (l value)
;;   (adjoin value (if (listp l) l (list l))))

;;;
;;;    Take 2
;;;    
(defun ddput (node property value &optional recursivep)
  (print (list node property value))
  (cond ((isa-get property '+multiple-values) (setf (get node property)
                                                    (add-to-multi-valued-property (get node property) value)))
        ((isa-get property '+save-property) (let ((current-value (get node property)))
                                              (if (null current-value)
                                                  (setf (get node property) value)
                                                  (let ((save-node (gensym "SAVE")))
                                                    (remprop current-value property)
                                                    (setf (get save-node property) current-value
                                                          (get node 'save-node) save-node
                                                          (get node property) value)))) )
        ((isa-get property '+lambda-property)
         (funcall (isa-get property '+lambda-property) node property value))
        (t (setf (get node property) value)))
  (when (isa-get property '+invert-onto)
    (ddput value (isa-get property '+invert-onto) node))
  (when (isa-get property '+invert-value)
    (when (not recursivep)
      (ddput value property node t)) ; Symmetry
    (dolist (existing-value (get-existing-values node property))
      (unless (or (eql existing-value value)
                  (member value (get-existing-values existing-value property)))
        (ddput existing-value property value)))) ; Transitivity
  (when (get property '+invert-property) ; Only execute if property directly has +invert-property
    (ddput property value node)))

(defun get-existing-values (node property)
  (let ((values (get node property)))
    (if (listp values)
        values
        (list values))))

