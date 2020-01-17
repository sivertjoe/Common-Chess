(in-package #:chess)

; Get x y coord of square
(defun get-coords (pos)
  (values (mod pos 8) 
	      (nth-value 0 (floor pos 8)))
  )


(defun board-piece-p (board square)
  (aref board square)
  )


(defun square-clear (board square)
  (eq (board-piece-p board square) 0)
  )



; Gets the x and y diff from two squares
(defun get-square-diff (start stop)
  (multiple-value-bind (start-x start-y) (get-coords start)
	(multiple-value-bind (stop-x stop-y) (get-coords stop)
	  (values (- stop-x start-x) (- stop-y start-y))
	  )
	)
  )


; meaning if there is an enemy piece, or no piece
(defun square-landable (color square board)
  (let ((piece (board-piece-p board square)))
	(if (eql piece 0)
	  T
	  (not (eql (piece-color piece) color)))
    )
  )


; Check if the (ex rook) path is clear
(defun check-path (color board start stop increment)
  (let ((next start))
	(loop 
	  (setf next (+ next increment))

	  (when (eql next stop)
		(return T))

	  (when (and ; if next != stop and !square-clear { return false; }
			  (not (eq next stop)) 
			  (not (square-clear board next)))
		(return nil))
	  )
	)
  )


; Check if any pieces attack the king square

; This function is meant to be called whenever the user 
; makes a king move, and therefor he could have walked 
; into any enemy piece, and so we need to check them all
; to see if any of them attack the new king square
(defun not-in-check (board color king-square)
  T
  )

; This function is meant to be called after the opponent
; moved a piece, we want to know if he put us in check,
; but we only need to check the piece he moved,
; and for discovered checks
(defun discovered-check (board color square)
  T
  )


; Check that the king is not walking into check
(defun castle-check-check (color board start stop increment)
  (let ((next start))
	(loop 
	  (unless (not-in-check board color next)
		(return nil))

	  (when (eql next stop)
		(return T))

	  (setf next (+ next increment))
	  )
	)
  )



