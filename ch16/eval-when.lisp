;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               eval-when.lisp
;;;;
;;;;   Started:            Tue Feb  7 13:34:27 2006
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

;(defpackage eval-when (:use common-lisp))

;(in-package eval-when)

(eval-when (:execute)
  (format t "~&Message one.~%")
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (format t "~&Message one A.~%")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "~&Message two.~%")
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (format t "~&Message two A.~%"))
  (eval-when (:compile-toplevel)
    (format t "~&Message two B.~%"))
  (eval-when (:load-toplevel)
    (format t "~&Message two C.~%"))
  (eval-when (:execute)
    (format t "~&Message two D.~%")))

(eval-when (:compile-toplevel :load-toplevel)
  (format t "~&Message three.~%")
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (format t "~&Message three A.~%")))

(eval-when (:compile-toplevel)
  (format t "~&Message four.~%"))

(eval-when (:load-toplevel :execute)
  (format t "~&Message five.~%"))

(eval-when (:compile-toplevel :execute)
  (format t "~&Message six.~%"))

(eval-when (:load-toplevel)
  (format t "~&Message seven.~%"))

(eval-when ()
  (format t "~&Message eight.~%"))

;;
;;    This prints when LOADing FASL file!
;;    
(let ((x 3))
  (eval-when (:execute) ; Only :EXECUTE has any effect here. 
;  (eval-when (:execute :load-toplevel :compile-toplevel) ; Only :EXECUTE has any effect here. Apparently :LOAD-TOPLEVEL too!
    (format t "CLHS example: only executes when source loaded: ~D~%" x)))

