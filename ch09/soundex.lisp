;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               soundex.lisp
;;;;
;;;;   Started:            Wed Nov 24 16:20:07 2004
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
;;;;   Notes: Slade's version is not quite right. We have to reduce adjacent
;;;;   pairs of _codes_ not _letters_! Furthermore, adjacent pairs separated
;;;;   by 'h' or 'w' are consolidated too. And the entire code is always 4
;;;;   characters in length.
;;;;   E.g., Cockburn -> 20221065 -> C216 even though 'c' and 'k' aren't the
;;;;         same letter.
;;;;
;;;;   Knuth shows Wachs -> W200 'c' separated from 's' by 'h'->consolidate.
;;;;
;;;;   Modelled after Perl's Text::Soundex (Mark Mielke) (see bottom of file).
;;;;   190918 The Perl implementation is incorrect? I don't see how it handles duplicate adjacent codes...
;;;;   
;;;;   See Knuth Vol. 3 pg. 394
;;;;   
;;;;   Cf. http://www.nara.gov/genealogy/soundex/soundex.html
;;;;   or
;;;;   http://www.archives.gov/research_room/genealogy/census/soundex.html
;;;;

(defpackage soundex (:use common-lisp))

(in-package soundex)

(defun soundex (name)
  (let ((encoded-name (map 'string
                           #'(lambda (ch)
                               (digit-char (char-encoding ch)))
                           name)))
    (subseq (concatenate 'string
                         (string-upcase (subseq name 0 1)) ; ??!
                         (remove-vowel-codes (remove-pairs encoded-name))
                         "000")
            0 4)))

;;;
;;;    Where did Slade come up with s, z -> 7?!
;;;    
(defun char-encoding (char)
  (case (char-downcase char)
    ((#\b #\f #\p #\v) 1)
    ((#\c #\g #\j #\k #\q #\x #\s #\z) 2)
;    ((#\c #\g #\j #\k #\q #\x) 2)
    ((#\d #\t) 3)
    ((#\l) 4)
    ((#\m #\n) 5)
    ((#\r) 6)
;    ((#\s #\z) 7)
    ((#\h #\w) 9)
    (otherwise 0)))

(defun remove-pairs (string)
  (coerce (remove-pairs-list (coerce string 'list)) 'string))

;; (defun remove-pairs-list (list)
;;   (let ((prev (car list))
;;         (result (list (car list))))
;;     (dolist (this (cdr list) (nreverse result))
;;       (unless (eql this prev)
;;         (push this result))
;;       (setf prev this))))

;;;
;;;    This is simply the function above with better names!
;;;    
(defun remove-pairs-list (list)
  (let* ((previous (first list))
         (result (list previous)))
    (dolist (current (rest list) (nreverse result))
      (unless (eql current previous)
        (push current result))
      (setf previous current))))

;;;
;;;    Don't need to check first(0)/last(n-1) chars for duplicates separated by
;;;    'w','h' since there can't be a char before the first or after the last
;;;    char! However, last char may not be kept if second-to-last char is w/h
;;;    and third-to-last is same as last.
;;;    
(defun remove-vowel-codes (s)
  (let ((result (make-array 1
                            :fill-pointer 0
                            :adjustable t
                            :element-type 'character)))
    (do ((len (1- (length s)))
         (i 1))
        ((>= i len)
         (when (= i len) (vector-push-extend (char s i) result)))
      (if (char= (char s i) #\9)
          (when (char= (char s (1- i)) (char s (1+ i)))
            (incf i))
          (vector-push-extend (char s i) result))
      (incf i))
    (remove-if #'(lambda (ch) (member ch '(#\0 #\9))) result)))

;;;
;;;    Trying to figure out what this does! 111128
;;;    Remove adjacent duplicates then remove the "vowel" markers: 0, 9
;;;    Since this doesn't remove h, w (i.e., 9) until after removing duplicates
;;;    it has to jump through some hoops, comparing the chars before and after
;;;    an h, or w. The 2011 version simply removes those letters at the beginning.
;;;
;;;    Actually, all this function does is gymnastics related to h, w. The other
;;;    adjacent duplicates have already been removed before SOUNDEX calls here.
;;;
(defun remove-vowel-codes (s)
  (let ((result (make-array 1
                            :fill-pointer 0
                            :adjustable t
                            :element-type 'character)))
    (do ((len (1- (length s)))
         (i 1 (1+ i)))
        ((>= i len) (when (= i len) (vector-push-extend (char s i) result)))
      (if (char= (char s i) #\9)
          (when (char= (char s (1- i)) (char s (1+ i)))
            (incf i))
          (vector-push-extend (char s i) result)))
    (remove-if #'(lambda (ch) (member ch '(#\0 #\9))) result)))

(defun remove-vowel-codes-list (s)
  (labels ((remove-aux (prev l)
             (cond ((endp l) '())
                   ((endp (rest l)) l)
                   ((char= (first l) #\9)
                    (if (char= prev (second l))
                        (remove-aux prev (cddr l))
                        (cons (second l) (remove-aux (second l) (cddr l)))) )
                   (t (cons (first l) (remove-aux (first l) (rest l)))) )))
    (let ((l (coerce s 'list)))
      (coerce (remove-if #'(lambda (ch) (member ch '(#\0 #\9)))
                         (remove-aux (first l) (rest l)))
              'string))))

;; (defun remove-vowels (string)
;;   (remove-if #'(lambda (ch)
;; 		 (member ch '(#\a #\e #\i #\o #\u #\w #\h #\y)
;; 			 :test #'char-equal))
;; 	     string))

;; (mapcar #'soundex '("euler" "gauss" "hilbert" "knuth" "lloyd" "lukasiewicz" "wachs")) => ("E460" "G200" "H416" "K530" "L300" "L222" "W200")
;; (mapcar #'soundex '("ellery" "ghosh" "heilbronn" "kant" "liddy" "lissajous" "waugh")) => ("E460" "G200" "H416" "K530" "L300" "L222" "W200")
;; (soundex "czarkowska") => "C622"
;; (soundex "cockburn") => "C216"
;; (soundex "Hu") => "H000"




;; sub soundex_nara
;; {
;;     # Implementation of NARA's soundex algorithm. If two sounds are
;;     # identical, and separated by only an H or a W... they should be
;;     # treated as one. This requires an additional "s///", as well as
;;     # the "9" character code to represent H and W. ("9" works like "0"
;;     # except it combines indentical sounds around it into one)

;;     my @results = map {
;; 	my $code = uc($_);
;;         $code =~ tr/AaEeHhIiOoUuWwYyBbFfPpVvCcGgJjKkQqSsXxZzDdTtLlMmNnRr//cd;

;; 	if (length($code)) {
;;             my $firstchar = substr($code, 0, 1);
;; 	    $code =~ tr[AaEeHhIiOoUuWwYyBbFfPpVvCcGgJjKkQqSsXxZzDdTtLlMmNnRr]
;;                        [0000990000009900111111112222222222222222333344555566]s;
;;             $code =~ s/(.)9\1/$1/g;
;; 	    ($code = substr($code, 1)) =~ tr/09//d;
;; 	    substr($firstchar . $code . '000', 0, 4);
;; 	} else {
;; 	    $nocode
;; 	}
;;     } @_;

;;     wantarray ? @results : $results[0];
;; }
