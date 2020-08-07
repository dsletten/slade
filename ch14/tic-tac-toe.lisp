;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               tic-tac-toe.lisp
;;;
;;;   STARTED:            Tue Mar 12 01:05:39 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
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
(defconstant player-symbol #\X)
(defconstant computer-symbol #\O)

(defun draw-board (board)
  (dotimes (i 3)
    (dotimes (j 3)
      (format t " ~A " (aref board i j)))
    (format t "~%")) )

(let ((board (make-array '(3 3) :initial-element nil)))
  (setf board #2A((nil nil nil)
		  (computer-symbol computer-symbol computer-symbol)
		  (player-symbol nil player-symbol)))

  (defun tic-tac-toe ()
    (draw-board board)
    (round-1)
;     (round-2)
;     (round-3)
;     (round-4) )
)
  (defun round-1 ()
    (check-rows-3 board) ))
  

;;;
;;;    Look for 3-in-a-row horizontally, vertically, or diagonally.
;;;    
(defun 3-in-row (board)
  (or (check-rows-3 board)
      (check-columns-3 board)
      (check-diagonals-3 board)) )

(defun check-rows-3 (board)
  "Check for 3-in-a-row horizontally."
  (or (check-rows-3-aux board player-symbol)
      (check-rows-3-aux board computer-symbol)) )

(defun check-rows-3-aux (board symbol)
  (do ((i 0 (1+ i)))
      ((>= i 3))
    (multiple-value-bind (a b c) (values (aref board i 0)
					 (aref board i 1)
					 (aref board i 2))
      (if (and a b c)
	  (if (char= a b c symbol)
	      (return symbol)))) ) )

;   (do ((i 0 (1+ i))
;        (a (aref board 0 0) (aref board i 0))
;        (b (aref board 0 1) (aref board i 1))
;        (c (aref board 0 2) (aref board i 2)))
;       ((>= i 3))
;     (if (and a b c)
; 	(cond ((char= a b c player-symbol) (return player-symbol))
; 	      ((char= a b c computer-symbol) (return computer-symbol)))) ) )

;   (dotimes (i 3)
;     (if (and (aref board i 0) (aref board i 1) (aref board i 2))
; 	(cond ((char= (aref board i 0) (aref board i 1) (aref board i 2) player-symbol)
; 	       (return player-symbol))
; 	      ((char= (aref board i 0) (aref board i 1) (aref board i 2) computer-symbol)
; 	       (return computer-symbol)))) ) )


(defun check-columns-3 (board)
  "Check for 3-in-a-row vertically."
  (or (check-columns-3-aux board player-symbol)
      (check-columns-3-aux board computer-symbol)) )

(defun check-columns-3-aux (board symbol)
  (do ((i 0 (1+ i)))
      ((>= i 3))
    (multiple-value-bind (a b c) (values (aref board 0 i)
					 (aref board 1 i)
					 (aref board 2 i))
      (if (and a b c)
	  (if (char= a b c symbol)
	      (return symbol)))) ) )

(defun check-diagonals-3 (board)
  "Check for 3-in-a-row diagonally."
  (or (check-diagonals-3-aux board player-symbol)
      (check-diagonals-3-aux board computer-symbol)) )

(defun check-diagonals-3-aux (board symbol)
  (or (check-diagonals-3-up board symbol)
      (check-diagonals-3-down board symbol)) )

(defun check-diagonals-3-up (board symbol)
  (check-diagonals-3-test board symbol 2 1 0) )

(defun check-diagonals-3-down (board symbol)
  (check-diagonals-3-test board symbol 0 1 2) )

(defun check-diagonals-3-test (board symbol x0 x1 x2)
  (let ((a (aref board 0 x0))
	(b (aref board 1 x1))
	(c (aref board 2 x2)))
    (if (and a b c)
	(if (char= a b c symbol)
	    symbol))) )
;;;
;;;    Check for 2-in-a-row horizontally, vertically, or diagonally.
;;;
(defun 2-in-row (board)
  (or (check-rows-2 board)
      (check-columns-2 board)
      (check-diagonals-2 board)) )

(defun check-rows-2 (board)
  "Check for 2-in-a-row horizontally."
  (or (check-rows-2-aux board player-symbol)
      (check-rows-2-aux board computer-symbol)) )

(defun check-rows-2-aux (board symbol)
  (do ((i 0 (1+ i)))
      ((>= i 3))
    (multiple-value-bind (a b c) (values (aref board i 0)
					 (aref board i 1)
					 (aref board i 2))
      (cond ((and a b (null c) (char= a b symbol))
	     (return symbol))
	    ((and a (null b) c (char= a c symbol))
	     (return symbol))
	    ((and (null a) b c (char= b c symbol))
	     (return symbol)))) ) )

(defun check-columns-2 (board)
  "Check for 2-in-a-row vertically."
  (or (check-columns-2-aux board player-symbol)
      (check-columns-2-aux board computer-symbol)) )

(defun check-columns-2-aux (board symbol)
  (do ((i 0 (1+ i)))
      ((>= i 3))
    (multiple-value-bind (a b c) (values (aref board 0 i)
					 (aref board 1 i)
					 (aref board 2 i))
      (cond ((and a b (null c) (char= a b symbol))
	     (return symbol))
	    ((and a (null b) c (char= a c symbol))
	     (return symbol))
	    ((and (null a) b c (char= b c symbol))
	     (return symbol)))) ) )

(defun check-diagonals-2 (board)
  "Check for 2-in-a-row diagonally."
  (or (check-diagonals-2-aux board player-symbol)
      (check-diagonals-2-aux board computer-symbol)) )

(defun check-diagonals-2-aux (board symbol)
  (or (check-diagonals-2-up board symbol)
      (check-diagonals-2-down board symbol)) )

(defun check-diagonals-2-up (board symbol)
  (check-diagonals-2-test board symbol 2 1 0) )

(defun check-diagonals-2-down (board symbol)
  (check-diagonals-2-test board symbol 0 1 2) )

(defun check-diagonals-2-test (board symbol x0 x1 x2)
  (let ((a (aref board 0 x0))
	(b (aref board 1 x1))
	(c (aref board 2 x2)))
    (cond ((and a b (null c) (char= a b symbol))
	   symbol)
	  ((and a (null b) c (char= a c symbol))
	   symbol)
	  ((and (null a) b c (char= b c symbol))
	   symbol))) )

;;;
;;;    Assumes opponent has made 2 moves, computer has taken center square if
;;;    it's available.
;;;
;;;    Returns coordinates of cell to capture or NIL if no sneak attack.
;;;    
(defun sneak-attack (board)
  (or (corner-sneak-attack (board))
      (edge-sneak-attack (board))
      (x-sneak-attack (board))
      (center-sneak-attack(board))) )

;;;
;;;    X |   |
;;;   -----------
;;;      | O |
;;;   -----------
;;;      | X |
;;;      
(defun corner-sneak-attack (board)
  (cond ((eql (aref board 0 0) player-symbol)
	 (or (when (eql (aref board 2 1) player-symbol) #(2 0))
	     (when (eql (aref board 1 2) player-symbol) #(0 2))))
	((eql (aref board 0 2) player-symbol)
	 (or (when (eql (aref board 2 1) player-symbol) #(2 2))
	     (when (eql (aref board 1 0) player-symbol) #(0 0))))
	((eql (aref board 2 2) player-symbol)
	 (or (when (eql (aref board 0 1) player-symbol) #(0 2))
	     (when (eql (aref board 1 0) player-symbol) #(2 0))))
	((eql (aref board 2 0) player-symbol)
	 (or (when (eql (aref board 1 2) player-symbol) #(2 2))
	     (when (eql (aref board 0 1) player-symbol) #(0 0)))) ) )

;;;
;;;      | X |
;;;   -----------
;;;    X | O |
;;;   -----------
;;;      |   |
;;;      
(defun edge-sneak-attack (board)
  )

;;;
;;;    X |   |
;;;   -----------
;;;      | O |
;;;   -----------
;;;      |   | X
;;;      
(defun x-sneak-attack (board)
  )

;;;
;;;    O |   |
;;;   -----------
;;;      | X |
;;;   -----------
;;;      |   | X
;;;      
(defun center-sneak-attack (board)
  )
