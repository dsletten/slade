;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               slade-multi-column-print.lisp
;;;
;;;   STARTED:            Sat Dec  1 15:30:05 2001
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;       This is Slade's horrible solution to ex. 7.9.2 (With a couple of glaring errors fixed!)
;;;       Among other things, he uses LOOP/RETURN/WHEN/UNLESS from chapter 9! And LET* from ch. 8.
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;
(defpackage :slade (:use :common-lisp))

(in-package :slade)

(defun multi-column-print (lst &key (columns 1) (width nil) (stream *standard-output*) (indent 0)) ;Why isn't WIDTH initialized here?!?
  (if (not width) (setf width (truncate 80 columns)))         ;This must be TRUNCATE rather than /. Can't use rational number as width!
  (let* ((num (ceiling (length lst) columns))                 ;His version: (ceiling (/ (length lst) columns))!?!
         (lists (split-list lst num))                         ;This passes SPLIT-LIST the wrong information if (not (zerop (mod (length l) columns)))
         (istring (if (zerop indent)
                      "~%~A"
                      (format nil "~~%~~~DT~~A" indent)))
         (fstring (format nil "~~,~DT~~A" width)))
    (multi-list-print lists istring fstring stream)) )

(defun multi-list-print (lists istring fstring stream)
  (cond ((null lists) (values))
        (t (format stream istring (caar lists))
           (multi-list-print2 (cdar lists) fstring stream)
           (multi-list-print (cdr lists) istring fstring stream))) )

(defun multi-list-print2 (lists fstring stream)
  (cond ((null lists))
        (t (format stream fstring (car lists))
           (multi-list-print2 (cdr lists) fstring stream))) )

(defun split-list (l n)
  (cond ((null l) nil)
        (t (mapcar #'reverse (split-list2 l n (make-list n)))) ) )

;;;
;;;    Horrible!
;;;    
(defun split-list2 (lst n result)
  (cond ((null lst) result)
        (t (let ((count 0))
             (loop
                (unless lst (return result))
                (when (= count n) (return (split-list2 lst n result)))
                (push (pop lst) (nth count result))
                (incf count)))) ) )
