;#!/usr/local/bin/clisp

;;
;   NAME:               format_examples.lisp
;
;   STARTED:            011021
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;

;;;
;;;    ~T
;;;    
(format t "~&01234567890123456789~%")
(format t "pung~Tfoo~%")
(format t "pung~6Tfoo~%")
(format t "pung~2Tfoo~%")
(format t "pung~2,0Tfoo~%")
(format t "pung~6,10Tfoo~%")
(format t "pung~2,10Tfoo~%")
(format t "pung~400Tfoo~%")

;;;
;;;    ~@T
;;;    
(format t "pung~3,8@Tfoo~%")
(format t "pung~4,8@Tfoo~%") ;Same as above.
