;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               queue.lisp
;;;;
;;;;   Started:            Wed Feb 15 10:03:40 2012
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

(defpackage :queue (:use :common-lisp :test))

(in-package :queue)

(defclass queue ()
  ((head :initform nil)
   (tail :initform nil)
   (length :reader queue-length :initform 0)))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "(~D)" (queue-length q))))

(defun queuep (obj)
  (typep obj 'queue))

(defgeneric emptyp (queue))
(defmethod emptyp ((q queue))
  (null (slot-value q 'head)))

(defgeneric enqueue (queue elt))
(defmethod enqueue ((q queue) elt)
  (with-slots (head tail length) q
     (let ((l (list elt)))
       (cond ((emptyp q) (setf head l
                               tail l))
             (t (setf (cdr tail) l
                      tail l))))
     (incf length)))

(defgeneric dequeue (queue))
(defmethod dequeue ((q queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (with-slots (head tail length) q
        (decf length)
        (if (eq head tail)
            (prog1 (first head)
              (clear-queue q))
            (pop head)))) )

(defgeneric clear-queue (queue))
(defmethod clear-queue ((q queue))
  (with-slots (head tail length) q
    (setf head nil
          tail nil
          length 0)))

(defgeneric head (queue))
(defmethod head ((q queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (first (slot-value q 'head))))

#|
Performance comparison between the 2 different implementations (Allegro):
APPEND queue
QUEUE-2(1016): (time (let ((q (make-instance 'queue))) (dotimes (i 10000) (enqueue q i)) (dotimes (i 10000) (dequeue q))))
; cpu time (non-gc) 1.122024 sec user, 0.013199 sec system
; cpu time (gc)     0.068470 sec user, 0.001352 sec system
; cpu time (total)  1.190494 sec user, 0.014551 sec system
; real time  2.101347 sec
; space allocation:
;  52,745,000 cons cells, 7,606,672 other bytes, 0 static bytes

QUEUE-2(1015): (time (let ((q (make-instance 'queue))) (dotimes (i 100000) (enqueue q i)) (dotimes (i 100000) (dequeue q))))
; cpu time (non-gc) 68.050308 sec (00:01:08.50308) user, 0.871336 sec system
; cpu time (gc)     34.284512 sec user, 0.426683 sec system
; cpu time (total)  102.334820 sec (00:01:42.334820) user, 1.298019 sec system
; real time  179.158400 sec (00:02:59.158400)
; space allocation:
;  732,482,704 cons cells, 76,699,984 other bytes, 0 static bytes

TCONC queue
QUEUE(1019): (time (let ((q (make-instance 'queue))) (dotimes (i 10000) (enqueue q i)) (dotimes (i 10000) (dequeue q))))
; cpu time (non-gc) 0.805183 sec user, 0.009193 sec system
; cpu time (gc)     0.014312 sec user, 0.000081 sec system
; cpu time (total)  0.819495 sec user, 0.009274 sec system
; real time  1.175364 sec
; space allocation:
;  4,320,168 cons cells, 11,201,128 other bytes, 0 static bytes

QUEUE(1018): (time (let ((q (make-instance 'queue))) (dotimes (i 100000) (enqueue q i)) (dotimes (i 100000) (dequeue q))))
; cpu time (non-gc) 8.067252 sec user, 0.094297 sec system
; cpu time (gc)     0.126631 sec user, 0.002104 sec system
; cpu time (total)  8.193883 sec user, 0.096401 sec system
; real time  13.263021 sec
; space allocation:
;  43,200,168 cons cells, 112,007,464 other bytes, 0 static bytes
|#

#|
Is this the price of generic functions?!?
QUEUE(1020): (time (let ((q (loop for i from 1 to 10000 collect i))) (dotimes (i 10000) (pop q))))
; cpu time (non-gc) 0.000177 sec user, 0.000016 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.000177 sec user, 0.000016 sec system
; real time  0.000257 sec
; space allocation:
;  10,001 cons cells, 0 other bytes, 0 static bytes

QUEUE(1021): (time (let ((q (loop for i from 1 to 100000 collect i))) (dotimes (i 100000) (pop q))))
; cpu time (non-gc) 0.001491 sec user, 0.000012 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.001491 sec user, 0.000012 sec system
; real time  0.001637 sec
; space allocation:
;  100,001 cons cells, 0 other bytes, 0 static bytes
|#

#|
Non-generic function version
QUEUE-3(1049): (time (let ((q (make-queue))) (dotimes (i 10000) (enqueue q i)) (dotimes (i 10000) (dequeue q))))
; cpu time (non-gc) 0.317747 sec user, 0.004526 sec system
; cpu time (gc)     0.064586 sec user, 0.005381 sec system
; cpu time (total)  0.382333 sec user, 0.009907 sec system
; real time  0.742053 sec
; space allocation:
;  2,100,033 cons cells, 56,963,688 other bytes, 0 static bytes

QUEUE-3(1050): (time (let ((q (make-queue))) (dotimes (i 100000) (enqueue q i)) (dotimes (i 100000) (dequeue q))))
; cpu time (non-gc) 3.185852 sec user, 0.039048 sec system
; cpu time (gc)     0.370213 sec user, 0.005917 sec system
; cpu time (total)  3.556065 sec user, 0.044965 sec system
; real time  6.650678 sec
; space allocation:
;  21,000,038 cons cells, 569,627,872 other bytes, 0 static bytes
#|