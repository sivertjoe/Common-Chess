(in-package #:chess)


(defun relative-direction (color)
  (if (eq color 1)
	1
	-1)
  )

(defun get-relative (n color)
  (* (relative-direction color) n)
  )

(defun more-or-less-2-squares (dy color)
  (or (> (abs dy) 2) (< (get-relative dy color) 0))
  )


(defun en-passant-square (start color)
  (if (eq color +white+)
	(and (>= start 32) (<= start 39))
	(and (>= start 24) (<= start 31)))
  )

(defun pawn-start (color pos)
  (+ pos (* (relative-direction color) 8))
  )

(defun pawn-stop (color pos)
  (- pos (* (relative-direction color) 8))
  )

(defun moved-two (last-move color stop)
  (let ((start-square (get-from-square last-move)) (stop-square (get-to-square last-move)))
	(and (eq start-square (pawn-start color stop)) 
		 (eq stop-square (pawn-stop color stop)))
	)
  )

(defun pawned-moved-two (logger color stop)
  (let ((last-move (car logger)))
	(unless (eq last-move nil)
	  (and (eq (get-id last-move) +pawn+) 
		   (moved-two last-move color stop)) ; if it was a pawn
	  )
	)
  )



(defun check-en-passant (color start stop logger)
	(when (and (en-passant-square start color) 
		 (pawned-moved-two logger color stop))
	  +en-passant+)
  )

; TODO en-passant, create log and check the logger I guess?
(defun one-square (color board dx start stop logger)
  (unless (> (abs dx) 1)
	(if (eq dx 0) ; only moved one square up
	  (eq (board-piece-p board stop) 0) ; Check that square is clear

	  (let ((stop-piece (board-piece-p board stop))); moved one square to the side (capture)
		(if (eq stop-piece 0)
		  (check-en-passant color start stop logger)
		  (not (eq (piece-color stop-piece) color)))
		  ))) 
  )

(defun square-underneath (color stop)
  (- stop (* 8 (relative-direction color)))
  )

(defun two-squares-clear (board first-square second-square)
  (and (eq (board-piece-p board first-square) 0) (eq (board-piece-p board second-square) 0)) 
  )

(defun pawn-start-square (color start)
  (eq 
	(case (relative-direction color)
	  (1 1)
	  (-1 6))
	(nth-value 0 (floor start 8)))
  )

(defun two-square (color board dx start stop)
  (if (eq dx 0)
	(let ((stop-minus-one (square-underneath color stop)))
	  (and (two-squares-clear board stop-minus-one stop) (pawn-start-square color start))
	))
  )


(defun legal-pawn-move(color board start stop logger)
  (multiple-value-bind (dx dy) (get-square-diff start stop)
	(unless (more-or-less-2-squares dy color)
	  (case (abs dy)
		(1 (one-square color board dx start stop logger))
		(2 (two-square color board dx start stop))
		)
	  )
	)
  )
