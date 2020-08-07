;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               dpsq-slade.lisp
;;;;
;;;;   Started:            Sat Oct 19 00:27:05 2019
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
;;;;   Notes: My reworking of Slade's version
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :dpsq-slade (:use :common-lisp :lang :test))

(in-package :dpsq-slade)

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
    (format t "~4@T~S~20T~S~%" property value)))

(defmacro pppq (symbol)
  `(ppp ',symbol))

(defun dps (symbol &rest props)
  (dotuples ((property value) props)
    (ddput symbol property value)))

(defmacro dpsq (&rest args)
  `(apply #'dps ',args))

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

;; ;;;
;; ;;;    Take 2
;; ;;;    
;; (defun ddput (node property value &optional recursivep)
;;   ;;    +MULTIPLE-VALUES and +SAVE-PROPERTY are mutually incompatible.
;;   (cond ((isa-get property '+multiple-values)
;;          (set-property node property (add-to-multi-valued-property (get node property) value)))
;;         ((isa-get property '+save-property)
;;          (let ((current-value (get node property)))
;;            (if (null current-value)
;;                (set-property node property value)
;;                (let ((save-node (gensym "SAVE")))
;;                  (remprop current-value property)
;;                  (set-property save-node property current-value)
;;                  (set-property node 'save-node save-node)
;;                  (set-property node property value)))) ))
;;   ;;    +LAMBDA-PROPERTY supersedes routine property. Logic should be in DPS not here????
;;   (let ((fn (isa-get property '+lambda-property))) ; Why ISA-GET????
;;     (if fn
;;         (funcall fn node property value)
;;         (set-property node property value)))
;;   (let ((onto (isa-get property '+invert-onto)))
;;     (when onto
;;       (ddput value onto node)))
;;   (when (isa-get property '+invert-value)
;;     (when (not recursivep)
;;       (ddput value property node t)) ; Symmetry -- Only recursive call!
;;     (dolist (existing-value (get-existing-values node property))
;;       (unless (or (eql existing-value value)
;;                   (member value (get-existing-values existing-value property)))
;;         (ddput existing-value property value)))) ; Transitivity
;;   (when (get property '+invert-property) ; Only execute if property directly has +invert-property
;;     (ddput property value node)))

;;;
;;;    Take 3
;;;    
(defun dps (node &rest plist)
  (dotuples ((property value) plist)
    (let ((fn (isa-get property '+lambda-property))) ; Why ISA-GET????
      (if fn
          (funcall fn node property value)
          (ddput node property value)))) )

;;;
;;;    Check:
;;;    1. Setting +SAVE-PROPERTY prop should erase existing relationship with old value, i.e., for +INVERT-VALUE
;;;    2. Don't update +SAVE-PROPERTY if new value is same as old
;;;    3. Trigger +INVERT-VALUE beyond immediate NODE/VALUE. E.g., new SIBLING causes existing siblings to
;;;       to get updated.
;;;
;;;    Developing realization...
;;;    Three distinct cases:
;;;    1. Set +SAVE-PROPERTY
;;;    2. Set +MULTIPLE-VALUES
;;;    3. Set basic property
;;;
;;;    Different consequences with regard to +INVERT-VALUE for cases 1. and 2...Also different check for
;;;    new PROP = old PROP
;;;    
(defun ddput (node property value &optional recursivep)
  ;;    +MULTIPLE-VALUES and +SAVE-PROPERTY are mutually incompatible.
  (cond ((isa-get property '+multiple-values)
         (set-property node property value :multi-valued-p t))
        (t (when (isa-get property '+save-property)
             (let ((current-value (get node property)))
               (when (and current-value (not (eql current-value value)))
                 (let ((save-node (gensym "SAVE")))
                   (remprop current-value property)
                   (set-property save-node property current-value)
                   (set-property node 'save-node save-node)))) )
           (set-property node property value)))
  (let ((onto (isa-get property '+invert-onto)))
    (when onto
      (ddput value onto node)))
  (when (isa-get property '+invert-value)
    (when (not recursivep)
      (ddput value property node t)) ; Symmetry -- Only recursive call!
    (dolist (existing-value (get-existing-values node property))
      (unless (or (eql existing-value value)
                  (member value (get-existing-values existing-value property)))
        (ddput existing-value property value)))) ; Transitivity
  (when (get property '+invert-property) ; Only execute if property directly has +invert-property
    (ddput property value node))) ; No list for singleton???

(defun set-property (node property value &key multi-valued-p)
  (if multi-valued-p
      (setf (get node property) (add-to-multi-valued-property node property value))
      (setf (get node property) value)))

(defun get-existing-values (node property)
  (let ((values (get node property)))
    (if (listp values)
        values
        (list values))))

(defun add-to-multi-valued-property (node property value)
  (let ((current (get node property)))
    (cond ((null current) value)
          ((listp current) (adjoin value current))
          ((eql current value) current)
          (t (list value current)))) )

