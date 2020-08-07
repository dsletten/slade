;#!/usr/local/bin/clisp

;;
;   NAME:               soundex.lisp
;
;   STARTED:            011221
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
(defun remove-pairs (s)
  (cond ((or (string= s "")
             (string= (subseq s 1) ""))
         s)
        ((string= (subseq s 0 1)
                  (subseq s 1 2))
         (remove-pairs (subseq s 1)))
        (t (concatenate 'string
                        (subseq s 0 1)
                        (remove-pairs (subseq s 1)))) ) )

(defconstant char-map '((#\B 1) (#\F 1) (#\P 1) (#\V 1)
                        (#\C 2) (#\G 2) (#\J 2) (#\K 2) (#\Q 2) (#\X 2)
                        (#\D 3) (#\T 3)
                        (#\L 4)
                        (#\M 5) (#\N 5)
                        (#\R 6)
                        (#\S 7) (#\Z 7)))

(defun soundex (word)
  (labels ((soundex-aux (word soundex-string)
             (cond ((string= word "") soundex-string)
                   ((= (length soundex-string) 4) soundex-string)
                   ((zerop (length soundex-string))
                    (soundex-aux (subseq word 1) (subseq word 0 1)))
                   (t (let ((char-val (assoc (char word 0) char-map)))
                        (if char-val
                            (soundex-aux (subseq word 1)
                                         (concatenate 'string
                                                      soundex-string
                                                      (format nil "~D"
                                                              (cadr char-val))))
                            (soundex-aux (subseq word 1) soundex-string)))) ) ))
    (soundex-aux (string-upcase (remove-pairs (string word))) "")) )
