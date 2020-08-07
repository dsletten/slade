;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch11.lisp
;;;;
;;;;   Started:            Wed Jan  4 01:13:49 2012
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch11 (:use :common-lisp :lang :test) (:shadow :prog1 :repeat :while :until))

(in-package :ch11)

;;;
;;;    11.10.2
;;;
(defmacro swap (x y)
  `(rotatef ,x ,y))

;;
;;    No problem with multiple evaluation here?
;;    (SETF <PLACE> <VALUE>)
;;    X, Y only evaluated as place, value once each.
;;    
(defmacro swap (x y)
  (let ((temp (gensym)))
    `(let ((,temp ,x))
       (setf ,x ,y ,y ,temp))))

;;;
;;;    11.10.3
;;;
;; (defmacro repeat (count (value &optional initial) &body body)
;;   (let ((i (gensym)))
;;     `(do ((,i ,count (1- ,i))
;;           (,value ,initial))
;;          ((<= ,i 0) ,value)
;;        ,@body)))

;; (macroexpand-1 '(repeat 8 (x 0) (print 5) (incf x) (print 4)))
;; (DO ((#:G7304 8 (1- #:G7304)) (X 0))
;;     ((<= #:G7304 0) X)
;;   (PRINT 5)
;;   (INCF X)
;;   (PRINT 4))

(defmacro repeat (count result &body body)
  (let ((i (gensym)))
    `(do ((,i ,count (1- ,i)))
         ((<= ,i 0) ,result)
       ,@body)))

;; (macroexpand-1 '(repeat 8 (values) (write-char char stream)))
;; (DO ((#:G7326 8 (1- #:G7326)))
;;     ((<= #:G7326 0) (VALUES))
;;   (WRITE-CHAR CHAR STREAM))

;;;
;;;    11.10.4
;;;    
(defmacro repeat (count result &body body)
  `(loop repeat ,count
         do ,@body
         finally (return ,result)))

;; (macroexpand-1 '(repeat 8 (values) (write-char char stream)))
;; (LOOP REPEAT 8 DO (WRITE-CHAR CHAR STREAM) FINALLY (RETURN (VALUES)))

;;;
;;;    11.10.5
;;;
(defmacro while (test result &body body)
  `(loop (unless ,test (return ,result))
      ,@body))

;;;
;;;    11.10.6
;;;
(defmacro until (test result &body body)
  `(loop (when ,test (return ,result))
      ,@body))

(defmacro repeat (count &body body)
  (let ((i (gensym)))
    `(dotimes (,i ,count)
       ,@body)))

#|
Allegro
(macroexpand-1 '(repeat 20 (print 'foo) (bar) (baz)))
(DOTIMES (#:G7535 20) (PRINT 'FOO) (BAR) (BAZ))

(macroexpand-1 *)
(DO ((#:G7535 0 (1+ #:G7535)))
    ((>= #:G7535 20))
  (PRINT 'FOO)
  (BAR)
  (BAZ))

(macroexpand-1 *)
(BLOCK NIL
  (LET ((#:G7535 0))
    (TAGBODY
      #:|Tag607|
        (COND ((>= #:G7535 20) (RETURN-FROM NIL (PROGN))))
        (TAGBODY (PRINT 'FOO) (BAR) (BAZ))
        (PSETQ #:G7535 (1+ #:G7535))
        (GO #:|Tag607|))))
|#

;;;
;;;    11.10.7
;;;
(defmacro prog1 (first &rest rest)
  (let ((result (gensym)))
    `((lambda (,result) ,@rest ,result) ,first)))

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

(defclass organism () ())
(defclass mammal (organism)
 ((sex :accessor sex :initarg :sex)))
(defclass person (mammal) ())
(defclass programmer (person)
 ((income :accessor income :initarg :income :initform '50k)))

(defgeneric ingests (organism))
(defmethod ingests ((o organism))
 (list 'food 'air))

(defvar *jane* (make-instance 'programmer :income '60k :sex 'female))
(defvar *john* (make-instance 'programmer :sex 'male))

(defmethod ingests ((p (eql *john*)))
 'junk-food)

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
;;;    Slade's version
;;;    
;; (defun ddput (node prop val)
;;   (and (symbolp node)
;;        (symbolp prop)
;;        (progn (cond ((isa-get prop '+invert-property) (ddput prop val node)))
;;               (cond ((isa-get prop '+invert-value) (*put val prop node)))
;;               (let ((onto (isa-get prop '+invert-onto)))
;;                 (if onto
;;                     (ddput val onto node)))
;;               (cond ((isa-get prop '+multiple-values) (add-property node prop val))
;;                     ((and (get node prop)
;;                           (isa-get prop '+save-property))
;;                      (*put (or (get node 'save-node)
;;                                (ddput node 'save-node (gensym "SAVE")))
;;                            prop
;;                            (get node prop))
;;                      (*put node prop val))
;;                     (t (let ((fn (isa-get prop '+lambda-property)))
;;                          (if fn
;;                              (apply fn (list node prop val))
;;                              (*put node prop val)))) )
;;               val)))

;; (defun *put (node prop val)
;;   (cond (prop (setf (get node '+ddprops) (enter (get node '+ddprops) prop)
;;                     (get node prop) val))))

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

;;;
;;;    11.10.11
;;;
;; (defmacro msg (&rest args)
;;   (let ((stdout (gensym)))
;;     `(let ((,stdout *standard-output*))
;;        ,@(process-msg stdout args))))
;; ;  `(progn ,@(process-msg args)))

;; (defun process-msg (stdout args)
;;   (if (endp args)
;;       '()
;;       (typecase (first args)
;;         (string (cons `(write-string ,(first args)) (process-msg stdout (rest args))))
;;         ((member t) (cons '(terpri) (process-msg stdout (rest args))))
;;         (symbol (cons `(princ ,(first args)) (process-msg stdout (rest args))))
;;         ((integer 0) (cons `(write-string ,(make-string (first args) :initial-element #\space))
;;                            (process-msg stdout (rest args))))
;;         ((integer * (0)) (cons `(format t "~v%" ,(abs (first args))) (process-msg stdout (rest args))))
;;         (cons (destructuring-bind ((operator operand) . rest) args
;;                 (case operator
;;                   (hex (cons `(format t "~X" ,operand) (process-msg stdout (rest args))))
;;                   (oct (cons `(format t "~O" ,operand) (process-msg stdout (rest args))))
;;                   (bin (cons `(format t "~B" ,operand) (process-msg stdout (rest args))))
;;                   (plur (cons `(format t "~P" ,operand) (process-msg stdout (rest args))))
;;                   (to (case operand
;;                         (*standard-output* `((let ((*standard-output* ,stdout)) ,@(process-msg stdout (rest args)))) )
;;                         (otherwise `((let ((*standard-output* ,operand)) ,@(process-msg stdout (rest args)))))))))))))

(defmacro msg (&rest args)
 `(progn ,@(process-msg args)))

;; (defun process-msg (args)
;;   (maplist #'process-arg args))

;; (defun process-arg (arg)
;;   (typecase arg
;;     (string `(write-string ,arg))
;;     ((member t) '(terpri))
;;     (symbol `(princ ,arg))
;;     ((integer 0) `(write-string ,(make-string arg :initial-element #\space)))
;;     ((integer * (0)) `(format t "~v%" ,(abs arg)))
;;     (cons (destructuring-bind (operator operand) arg
;;             (case operator
;;               (hex (process-format-directive "~X" operand arg))
;;               (oct (process-format-directive "~O" operand arg))
;;               (bin (process-format-directive "~B" operand arg))
;;               (plur (process-format-directive "~P" operand arg)))))))
;; ;              (to (redirect operand '() rest))))))))

;; (defun process-msg (args)
;;   (if (endp args)
;;       '()
;;       (typecase (first args)
;;         (string (cons `(write-string ,(first args)) (process-msg (rest args))))
;;         ((member t) (cons '(terpri) (process-msg (rest args))))
;;         (symbol (cons `(princ ,(first args)) (process-msg (rest args))))
;;         ((integer 0) (cons `(write-string ,(make-string (first args) :initial-element #\space))
;;                            (process-msg (rest args))))
;;         ((integer * (0)) (cons `(format t "~v%" ,(abs (first args))) (process-msg (rest args))))
;;         (cons (destructuring-bind ((operator operand) . rest) args
;;                 (case operator
;;                   (hex (process-format-directive "~X" operand args))
;;                   (oct (process-format-directive "~O" operand args))
;;                   (bin (process-format-directive "~B" operand args))
;;                   (plur (process-format-directive "~P" operand args))
;;                   (to (redirect operand '() rest))))))))

;; (defun process-format-directive (directive operand arg)
;;  `(format t ,directive ,operand))

;; (defun process-msg (args)
;;   (if (endp args)
;;       '()
;;       (typecase (first args)
;;         (string (collect `(write-string ,(first args)) args))
;;         ((member t) (collect '(terpri) args))
;;         (symbol (collect `(princ ,(first args)) args))
;;         ((integer 0) (collect `(write-string ,(make-string (first args) :initial-element #\space)) args))
;;         ((integer * (0)) (collect `(format t "~v%" ,(abs (first args))) args))
;;         (cons (destructuring-bind ((operator operand) . rest) args
;;                 (case operator
;;                   (hex (process-format-directive "~X" operand args))
;;                   (oct (process-format-directive "~O" operand args))
;;                   (bin (process-format-directive "~B" operand args))
;;                   (plur (process-format-directive "~P" operand args))
;;                   (to (redirect operand '() rest))))))))

(defun process-msg (args)
  (if (endp args)
      '()
      (let ((arg (first args)))
        (if (redirect-arg-p arg)
            (redirect (second arg) '() (rest args))
            (cons (process arg) (process-msg (rest args)))) )))

(defun redirect-arg-p (arg)
  (typep arg '(cons (member to))))

(defgeneric process (arg))
(defmethod process ((s string))
  `(write-string ,s))
(defmethod process ((obj (eql t)))
  '(terpri))
(defmethod process ((sym symbol))
  `(princ ,sym))
(defmethod process ((n integer))
  (if (minusp n)
      `(format t "~v%" ,(abs n))
      `(write-string ,(make-string n :initial-element #\space))))
(defmethod process ((c cons))
  (destructuring-bind (operator operand) c
    (case operator
      (hex (process-format-directive "~X" operand))
      (oct (process-format-directive "~O" operand))
      (bin (process-format-directive "~B" operand))
      (plur (process-format-directive "~P" operand)))) )

(defun process-format-directive (directive operand)
  `(format t ,directive ,operand))

;; (defun collect (form args)
;;   (cons form (process-msg (rest args))))

;; (defun process-format-directive (directive operand args)
;;   (collect `(format t ,directive ,operand) args))

;;;
;;;    Rebind *STANDARD-OUTPUT* and gather expressions into LET result form until
;;;    the end of the argument list or another (to <STREAM>) expression is
;;;    encountered.
;;;
;;;    If (to *standard-output*) is encountered then stop gathering expressions.
;;;    Simply let them revert to original binding of *STANDARD-OUTPUT*.
;;;    
(defun redirect (stream result args)
  (if (endp args)
      (if (null result)
          '()
          `((let ((*standard-output* ,stream))
              ,@(nreverse result))))
      (if (redirect-arg-p (first args))
          (destructuring-bind ((operator operand) . rest) args
            `((let ((*standard-output* ,stream))
                ,@(nreverse result))
              ,@(case operand
                      (*standard-output* (process-msg (rest args)))
                      (otherwise (redirect operand '() (rest args)))) ))
        (redirect stream (cons (process (first args)) result) (rest args)))) )

#|
(msg "Hello" t)
Hello
NIL
CH11(189): (msg "Hello" t "there" t)
Hello
there
NIL
CH11(190): (let ((x 5)) (msg "John is" 1 x 1 "year" (plur x) 1 "old" t))
John is 5 years old
NIL
CH11(191): (let ((x 5)) (msg (bin x) " + " (bin x) " = " (bin (+ x x)) t))
101 + 101 = 1010
NIL
|#

;;;
;;;    11.10.12
;;;
(defun multiline-comment (stream char)
  (declare (ignore char))
  (loop (unless (read-line stream nil nil)
          (return (values)))
     (when (char= (peek-char nil stream nil #\() #\()
       (return (values)))) )

(set-macro-character #\! #'multiline-comment)

;(read-from-string (format nil "!Is this not pung?~%Oh yeah!~2% (Oh, we're not done yet!!)!~%(okay that is enough)"))

(defmacro addf (var val)
  `(setf ,var (+ ,var ,val)))

(define-modify-macro addf (&rest args) +)

(defmacro timesf (var val)
  `(setf ,var (* ,var ,val)))

(define-modify-macro timesf (&rest args) *)

(defmacro modf (var val)
  `(setf ,var (mod ,var ,val)))

(define-modify-macro modf (divisor) mod)

(defmacro reversef (l)
  `(setf ,l (reverse ,l)))

(define-modify-macro reversef () reverse)

(defmacro notf (var)
  `(setf ,var (not ,var)))

(define-modify-macro notf () not)

(defmacro negatef (var)
  `(setf ,var (- ,var)))

(define-modify-macro negatef () -)

(defmacro invertf (var)
  `(setf ,var (/ ,var)))

(define-modify-macro invertf () /)

(defmacro upcasef (var)
  `(setf ,var (string-upcase ,var)))

(defmacro upcasef () string-upcase)

(defmacro downcasef (var)
  `(setf ,var (string-downcase ,var)))

(defmacro downcasef () string-downcase)

(defmacro append1f (var val)
  `(setf ,var (append ,var (list ,val))))

(define-modify-macro append1f (item) append1)

(defmacro adjoinf (val var)
  `(setf ,var (adjoin ,val ,var)))

;;;
;;;    This doesn't work. Order of args is backwards...
;;;    
(define-modify-macro adjoinf (item) (lambda (l item) (adjoin l item)))
;; (macroexpand-1 '(adjoinf elt l))
;; (LET* ((#:G7473 ((LAMBDA # #) ELT L))) (SETQ ELT #:G7473))
;; T

;(defmacro conc1f 

(defmacro unionf (var val)
  `(setf ,var (union ,var ,val)))

(define-modify-macro unionf (set) union)
