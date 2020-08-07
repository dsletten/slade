;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               soundex.lisp
;;;;
;;;;   Started:            Sun Nov 20 17:53:48 2011
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
;;;;   http://en.wikipedia.org/wiki/Soundex
;;;;
;;;;   NARA
;;;;   http://www.archives.gov/research/census/soundex.html
;;;;   This site specifies 4 rules.
;;;;
;;;;   1. Names With Double Letters
;;;;      Handled by COMPRESS-STRING in CLEAN. Doubled letters are removed before
;;;;      attempting to encode the string.
;;;;   2. Names with Letters Side-by-Side that have the Same Soundex Code Number
;;;;      Handled by REMOVE-PAIRS in GET-SOUNDEX-SEQUENCE. Doubled codes are removed
;;;;      after encoding the string. Furthermore, as in the "Pfister" example, the
;;;;      initial character is encoded although it will not be used in the final
;;;;      result (the actual first character is used rather than its code). This allows
;;;;      the code for "f" to be discarded since it is the same as the code for "P".
;;;;   3. Names with Prefixes
;;;;      Ignored (irrelevant)
;;;;   4. Consonant Separators
;;;;      There are 2 parts to this rule:
;;;;      - Vowel separators
;;;;        Vowels are encoded as NIL. The NILs are not removed until after rule 2 is
;;;;        satisfied. This allows duplicate codes separated by vowels (NILs) to persist
;;;;        in the final result.
;;;;      - "H"/"W" separating duplicate codes
;;;;        These letters are removed in CLEAN before encoding the string. Therefore they
;;;;        do not separate duplicate codes in the encoded string, and these duplicates
;;;;        are removed by rule 2.
;;;;   
;;;;   http://www.eogn.com/soundex/ (See JavaScript below)
;;;;   http://blog.eogn.com/eastmans_online_genealogy/2010/08/soundex-explained.html <-- Dead
;;;;   http://www.avotaynu.com/soundex.htm
;;;;
;;;;   http://commons.apache.org/codec/
;;;;
;;;;    Dead Links...
;;;;   http://snippets.dzone.com/posts/show/844
;;;;   http://snippets.dzone.com/posts/show/4530
;;;;   http://www.vanderharg.nl/soundex.php
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :soundex (:use :common-lisp :lang :test))

(in-package :soundex)

(defconstant soundex-length 4)

(defun char-value (ch)
  (case (char-downcase ch)
    ((#\b #\f #\p #\v) 1)
    ((#\c #\g #\j #\k #\q #\s #\x #\z) 2)
    ((#\d #\t) 3)
    ((#\l) 4)
    ((#\m #\n) 5)
    ((#\r) 6)
    (t nil)))

;;;
;;;    No other source corroborates Slade's claim about 7's...
;;;    
;; (defun char-value (ch)
;;   (case (char-downcase ch)
;;     ((#\b #\f #\p #\v) 1)
;;     ((#\c #\g #\j #\k #\q #\x) 2)
;;     ((#\d #\t) 3)
;;     ((#\l) 4)
;;     ((#\m #\n) 5)
;;     ((#\r) 6)
;;     ((#\s #\z) 7)
;;     (t nil)))

(defun remove-pairs (l)
  (loop for cons on l
        when (or (endp (rest cons))
                 (not (eql (first cons) (second cons))))
        collect (first cons)))

(defun compress-string (s)
  (if (zerop (length s))
      s
      (with-output-to-string (result)
        (do ((i 0 (1+ i))
             (j 1 (1+ j)))
            ((= j (length s)) (write-char (char s i) result))
          (unless (char= (char s i) (char s j))
            (write-char (char s i) result)))) ))

(defun get-soundex-sequence (word)
  (let ((result (remove-pairs (map 'list #'char-value word))))
    (if (null (first result))
        (pack-soundex-sequence word (remove nil result))
        (pack-soundex-sequence word (rest (remove nil result)))) ))

;; (defun get-soundex-sequence (word)
;;   (let ((codes (loop for ch across word
;;                      for code = (char-value ch)
;;                      collect code)))
;;     (let ((result (remove-pairs codes)))
;;       (if (null (first result))
;;           (pack-soundex-sequence word (remove nil result))
;;           (pack-soundex-sequence word (rest (remove nil result)))) )))

(defun pack-soundex-sequence (word codes)
  (let ((result (make-string soundex-length :initial-element #\0)))
    (setf (char result 0) (char word 0))
    (setf (subseq result 1) (mapcar #'digit-char codes))
    result))

;; (defun get-soundex-sequence (word)
;;   (do ((encoded (make-array (1- soundex-length) :initial-element #\0))
;;        (i 1 (1+ i))
;;        (j 0))
;;       ((or (= i (length word)) (= j (length encoded))) encoded)
;;     (let ((code (char-value (char word i))))
;;       (unless (or (null code)
;;                   (and (> j 0)
;;                        (char= (digit-char code) (aref encoded (1- j)))) )
;;         (setf (aref encoded j) (digit-char code))
;;         (incf j)))) )

(defun soundex (word)
  (get-soundex-sequence (clean word)))

;; (defun soundex (word)
;;   (let ((word (clean word)))
;;     (with-output-to-string (result)
;;       (write-char (char word 0) result)
;;       (dolist (code (get-soundex-sequence word))
;;         (write-char (digit-char code) result)))) )

;;;
;;;    See Apache Commons Codec
;;;    SoundexUtils
;;;
;;;    - Remove non-alphabetic characters (This handles whitespace too.)
;;;    - Convert to all uppercase
;;;    - Remove the letters 'H' and 'W' unless they start the string
;;;    - Remove adjacent pairs of characters
;;;    
(defun clean (s)
  (compress-string (remove-if #'(lambda (ch)
                                  (member ch '(#\H #\W)))
                              (string-upcase (remove-if-not #'alpha-char-p s))
                              :start 1)))

(defun similarity (s1 s2)
  (loop for ch1 across (soundex s1)
        for ch2 across (soundex s2)
        when (char= ch1 ch2)
        count ch1))

;; (similarity "Missouri" "misery")
;; 4
;; (similarity "Missouri" "Dog")
;; 2
;; (soundex "Missouri")
;; "M260"
;; (soundex "Dog")
;; "D200"

;;;
;;;    Test data from NARA site and Avotaynu site.
;;;    Also Knuth.
;;;    
(deftest test-soundex ()
  (check
   (string= (soundex "Washington") "W252")
   (string= (soundex "Lee") "L000")
   (string= (soundex "Gutierrez") "G362")
   (string= (soundex "Pfister") "P236")
   (string= (soundex "Jackson") "J250")
   (string= (soundex "Tymczak") "T522")
   (string= (soundex "VanDeusen") "V532")
   (string= (soundex "Ashcraft") "A261")
   (string= (soundex "Miller") "M460")
   (string= (soundex "Peterson") "P362")
   (string= (soundex "Peters") "P362")
   (string= (soundex "Auerbach") "A612")
   (string= (soundex "Uhrbach") "U612")
   (string= (soundex "Moskowitz") "M232")
   (string= (soundex "Moskovitz") "M213")
   (string= (soundex "Euler") "E460")
   (string= (soundex "Ellery") "E460")
   (string= (soundex "Gauss") "G200")
   (string= (soundex "Ghosh") "G200")
   (string= (soundex "Hilbert") "H416")
   (string= (soundex "Heilbronn") "H416")
   (string= (soundex "Knuth") "K530")
   (string= (soundex "Kant") "K530")
   (string= (soundex "Lloyd") "L300")
   (string= (soundex "Liddy") "L300")
   (string= (soundex "Lukasiewicz") "L222")
   (string= (soundex "Lissajous") "L222")
   (string= (soundex "Wachs") "W200")
   (string= (soundex "Waugh") "W200")
   (string= (soundex "Czarkowska") "C622")
   (string= (soundex "Cockburn") "C216")
   (string= (soundex "Hu") "H000")))

;(defun make-test (s) (apply #'format t "   (string= (soundex \"~A\") \"~A\")~%" (lang:split s)))


#|
function doSoundex() {
  var sur = document.frm.surname.value.toLowerCase();
  if( sur == "" ) { alert("Enter a surname."); return; }
  var l = sur.length;
  var v1;
  var lastn = "";
  var out = "";
  var ct = 0;
  for( i=0; i<l; i++ ) {
    var n = "0";
    var c1 = sur.charAt(i);
    if( (c1=="a")||(c1=="e")||(c1=="i")||(c1=="o")||(c1=="u")||(c1=="y") ) { n="-1"; }
    else if( (c1=="h")||(c1=="w") ) { n="-2"; }
    else if( (c1=="b")||(c1=="p")||(c1=="f")||(c1=="v") ) { n="1"; }
    else if( (c1=="c")||(c1=="s")||(c1=="k")||(c1=="g")||(c1=="j")||(c1=="q")||(c1=="x")||(c1=="z") ) { n="2"; }
    else if( (c1=="d")||(c1=="t") ) { n="3"; }
    else if( (c1=="l") ) { n="4"; }
    else if( (c1=="m")||(c1=="n") ) { n="5"; }
    else if( (c1=="r") ) { n="6"; }
    if( n==lastn ) { lastn=n; continue; }
    if( (ct==0) && (n!=0) ) { v1 = c1.toUpperCase(); ct++; lastn=n; }
    else if( n>0 ) { ct++; out=out+n; lastn=n; }
    else if( n==-1 ) { lastn=n; }
    if( ct==4 ) { break; }
  }
  if( ct>0 ) {
    var sdx = v1+"-"+out+"000";
    document.frm.soundex.value = sdx.substring(0,5);
  }
  else { 
    document.frm.soundex.value = "";
    alert("Enter a surname."); 
  }
}
function getkeycode(e) {
  var keycode;
  if (e) keycode = e.which;
  if (keycode == 13) { doSoundex(); }
}
|#