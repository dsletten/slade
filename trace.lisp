;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               trace.lisp
;;;;
;;;;   Started:            Sun Jan 16 20:13:03 2022
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Slade's macro tracing mechanism. 见 notes 467 页
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

(defvar *macro-trace-list* '())

(defmacro macro-trace (&optional macro)
  (if macro
      `(pushnew ',macro *macro-trace-list*)
      '*macro-trace-list*))

(defmacro macro-untrace (&optional macro)
  (if macro
      `(setf *macro-trace-list* (delete ',macro *macro-trace-list*))
      '(setf *macro-trace-list* '())))

(defun hook (expander form env)
  (if (member (first form) *macro-trace-list*)
      (progn (format t "Trace: ~S~%" form)
             (let ((result (funcall expander form env)))
               (format t "Result: ~S~%" result)
               result))
      (funcall expander form env)))

(setf *macroexpand-hook* #'hook)
