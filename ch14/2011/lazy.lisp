;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               lazy.lisp
;;;;
;;;;   Started:            Mon Feb 14 09:53:11 2011
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;     -Using macros to explore lazy evaluation.
;;;;     -Exploring lazy arrays and sparse arrays to reduce space requirements.
;;;;      (See _Programming C# 4.0_ pg. 306)
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
;;;;   - SICP streams
;;;;   - Winston streams/delayed evaluation
;;;;   - AI Programming (Charniak, et al.) generated-lists
;;;;
;;;;

(defpackage lazy (:use common-lisp))

(in-package lazy)

(defclass delayed-state ()
  ((body :initarg :body)))

(defmethod initialize-instance :after ((ds delayed-state) &key)
  (format t "Created another DELAYED-STATE!~%"))

(defmethod print-object ((obj delayed-state) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defun delayed-state-p (obj)
  (typep obj 'delayed-state))

;; (defun make-delayed-state ()
;;   (make-instance 'delayed-state :body nil))

(defmacro delay (form)
  `(make-instance 'delayed-state :body #'(lambda () ,form)))

;; (defmacro delay (form)
;;   `(let ((ds (make-delayed-state)))
;;      (setf (body ds) #'(lambda () ,form))
;;      ds))

(defun force (delayed-state)
  (funcall (slot-value delayed-state 'body)))

(defun make-generator (&optional (start 0) (f #'1+))
  (delay (prog1 start (setf start (funcall f start)))) )

;; (defun make-generator (&optional (start 0) (f #'1+))
;;   (let ((delayed-state (delay (prog1 start (setf start (funcall f start)))) ))
;;     #'(lambda () (force delayed-state))))

(defun take (n generator)
  (loop for i from 1 to n
        collect (force generator)))

(defun drop (n generator)
  (loop for i from 1 to n
        do (force generator))
  generator)

;; (setf *g* (make-generator 1))
;; (take 20 (drop 5 *g*)) => (6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
;; (take 20 (drop 5 *g*)) => (31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)

;; (let ((s "Is this not pung?") (i 0)) (make-generator (char s 0) #'(lambda (ch) (if (< i (1- (length s))) (char s (incf i)) nil))))
;; (take 10 (drop 3 *)) => (#\t #\h #\i #\s #\  #\n #\o #\t #\  #\p)

;; (setf *g* (make-generator 0 #'(lambda (x) (mod (1+ x) 7))))
;; (take 40 *g*) => (0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4)

(defun cycle (f v)
  (let ((i 0))
    (delay (prog1 (funcall f (aref v (mod i (length v)))) (incf i)))) )

;; (setf *c* (cycle #'(lambda (ch) (char-code ch)) "asdf"))
;; (take 10 *c*) => (97 115 100 102 97 115 100 102 97 115)

(defun make-integer-stream (&optional (start 0) (f #'1+))
  (labels ((stream (n)
             (cons n (delay (stream (funcall f n)))) )) ; <-- This creates a lot of DELAYED-STATE instances!
    (stream start)))

;; (defmacro lazy-pop (stream)
;;   `(prog1
;;        (car ,stream)
;;      (setf ,stream (force (cdr ,stream)))) )

;; (defmacro lazy-pop (stream)
;;   (let ((x stream))
;;     `(prog1 (car ,x)
;;        (setf ,stream (force (cdr ,x)))) ))

;; (defmacro lazy-pop (stream)
;;   (let ((x (gensym)))
;;     `(let ((,x ,stream))
;;        (prog1 (car ,x)
;;          (setf ,stream (force (cdr ,x)))) )))

;; (defmacro lazy-pop (stream)
;;   (let ((source (gensym)))
;;     `(let ((,source ,stream))
;;        (prog1 (car ,source) (popf ,source)))) )

(define-modify-macro popf () (lambda (stream) (force (cdr stream))))
;;;
;;;    This loses the very first value of the stream...
;;;    
;; (defvar *stream2* (make-integer-stream 0 #'(lambda (x) (+ x 9))))
;; (dotimes (i 20) (print (lazy-pop *stream2*)))

;; 9 
;; 18 
;; 27 
;; 36 
;; ...
(defmacro lazy-pop (stream)
  `(car (popf ,stream)))

(defmacro lazy (body)
  (cond ((atom body) body)
        (t (case (first body)
             (cons `',(rest body)))) ))
;;              (cons (let ((b (gensym)))
;;                      `(let* ((,b ',body)
;;                              (car (second ,b))
;;                              (cdr (third ,b)))
;;                         `(,car ,cdr)))) ))))

(defmacro lazy-cons (&whole whole car cdr)
  (declare (ignore car cdr))
  `',(rest whole))

;;;;
;;;;    Lazy 2D Arrays
;;;;
;;;
;;;    There is nothing about this that requires DELAY/FORCE. The constructor could simply
;;;    store NIL for each row and then have LAZY-REF call MAKE-ARRAY when a row needed to
;;;    be instantiated.
;;;    
(defclass lazy-2d-array ()
  ((rows :accessor rows :initarg :rows :documentation "The number of rows")
   (columns :accessor columns :initarg :columns :documentation "The number of columns")
   (default :accessor default :initarg :default)
   (lazy-array :accessor lazy-array)))

(defun make-lazy-2d-array (m n &optional (default 0))
  (let ((a (make-instance 'lazy-2d-array :rows m :columns n :default default)))
    (setf (lazy-array a) (make-array m))
    (dotimes (i m a)
      (setf (aref (lazy-array a) i)
            (delay (make-array n)))) ))

(defun lazy-2d-array-p (obj)
  (typep obj 'lazy-2d-array))

(defmethod print-object ((obj lazy-2d-array) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "(~D x ~D) ~A" (rows obj) (columns obj) (lazy-array obj))))

(defun lazy-2d-ref (a i j)
  (aref (lazy-ref a i) j))

(defun (setf lazy-2d-ref) (val a i j)
  (setf (aref (lazy-ref a i) j) val))

;;;(define-modify-macro forcef

(defun lazy-ref (a i)
  (when (delayed-state-p (aref (lazy-array a) i))
    (setf (aref (lazy-array a) i)
          (force (aref (lazy-array a) i)))
    (fill (aref (lazy-array a) i) (default a)))
  (aref (lazy-array a) i))

;; (defun lazy-ref (a i)
;;   (let ((result (aref (lazy-array a) i)))
;;     (if (delayed-state-p result)
;;         (progn
;;           (setf (aref (lazy-array a) i)
;;                 (force (aref (lazy-array a) i)))
;;           (fill (aref (lazy-array a) i) (default a))
;;           (lazy-ref a i))
;;         result)))

;;;
;;;    Sparse (not lazy) vectors/arrays
;;;    
(defclass sparse-vector ()
  ((hash)
   (default :reader default :initarg :default)))

(defun sparse-vector-p (obj)
  (typep obj 'sparse-vector))

(defun make-sparse-vector (&optional (default nil))
  (make-instance 'sparse-vector :default default))

(defmethod initialize-instance :after ((v sparse-vector) &key)
  (setf (slot-value v 'hash) (make-hash-table)))

(defgeneric size (sparse-vector))
(defmethod size ((v sparse-vector))
  (hash-table-count (slot-value v 'hash)))

(defun sparse-svref (v i)
  (values (gethash i (slot-value v 'hash) (default v))))
;;   (multiple-value-bind (val present-p) (gethash i (slot-value v 'hash))

(defun (setf sparse-svref) (val v i)
  (setf (gethash i (slot-value v 'hash)) val))

;;;;
;;;;    The sparse 2D array is either adjustable or not. If adjustable, storing
;;;;    a value at a pair of indices past the existing dimensions will cause those
;;;;    dimensions to grow. If the array is not adjustable, then values may not be
;;;;    stored beyond the existing dimensions. In any case, it is an error to read
;;;;    a value beyond the existing dimensions, i.e., reading will not extend the
;;;;    array.
;;;;    
(defclass sparse-2d-array ()
  ((hash)
   (adjustable :reader adjustable :initarg :adjustable) ; Should use ADJUSTABLE-SPARSE-2D-ARRAY-P to read...
   (rows :reader rows :initarg :rows)
   (columns :reader columns :initarg :columns)
   (default :reader default :initarg :default)))

(defun sparse-2d-array-p (obj)
  (typep obj 'sparse-2d-array))

(defun make-sparse-2d-array (&key (rows 0) (columns 0) default adjustable)
;(defun make-sparse-2d-array (dimensions &optional (default nil))
  (make-instance 'sparse-2d-array :default default :adjustable adjustable :rows rows :columns columns))

(defmethod initialize-instance :after ((a sparse-2d-array) &key)
  (setf (slot-value a 'hash) (make-hash-table)))

(defun adjustable-sparse-2d-array-p (a)
  (adjustable a))

(defun sparse-aref (a i j)
  (assert (and (<= 0 i (1- (rows a)))
               (<= 0 j (1- (columns a))))
          (i j)
          "Indices out of range: (~D ~D)"
          i j)
  (unchecked-sparse-aref a i j))

(defun unchecked-sparse-aref (a i j)
  (multiple-value-bind (column present-p) (gethash i (slot-value a 'hash))
    (if present-p
        (values (gethash j column (default a)))
        (default a))))

(defun (setf sparse-aref) (val a i j)
  (adjust-sparse-array a i j)
  (multiple-value-bind (column present-p) (gethash i (slot-value a 'hash))   ; row?????????????
    (if present-p
        (setf (gethash j column) val)
        (setf (gethash i hash) (make-hash-table) ; ???????
              (sparse-aref a i j) val))))

(defun adjust-sparse-array (a i j)
  (with-slots (rows columns) a
    (when (>= i rows)
      (if (adjustable-sparse-2d-array-p a)
          (setf rows (1+ i))
          (error "Index out of bound: ~D" i)))
    (when (>= j columns)
      (if (adjustable-sparse-2d-array-p a)
          (setf columns (1+ j))
          (error "Index out of bound: ~D" j)))) )

(defgeneric iterator (sparse-array))
(defmethod iterator ((a sparse-2d-array))
  (let ((m (rows a))
        (n (columns a))
        (i 0)
        (j 0))
    #'(lambda ()
        (if (and (< i m) (< j n))
            (prog1
                (values (unchecked-sparse-aref a i j) t)
              (incf j)
              (when (>= j n)
                (setf j 0)
                (incf i)))
            (values nil nil)))) )

;; (defmethod iterator ((a sparse-2d-array))
;;   (let ((m (rows a))
;;         (n (columns a))
;;         (i 0)
;;         (j 0))
;;     #'(lambda ()
;;         (prog1
;;             (if (and (< i m) (< j n))
;;                 (values (unchecked-sparse-aref a i j) t)
;;                 (values nil nil))
;;           (incf j)
;;           (when (>= j n)
;;             (setf j 0)
;;             (incf i)))) ))

(defun print-sparse-2d-array (a)
  (let ((iterator (iterator a)))
    (dotimes (i (rows a))
      (dotimes (j (columns a))
        (format t "~A " (funcall iterator)))
      (terpri))))

        