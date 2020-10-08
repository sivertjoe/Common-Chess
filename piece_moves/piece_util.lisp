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

(defun get-piece-increment (piece stop)
    (multiple-value-bind (dx dy) (get-square-diff (piece-square piece) stop)
        (cond 
          ; Pawns doesnt really increment
          ; ((eql (piece-id piece) +pawn+) 
           ;(T))

          ; ((eql (piece-id piece) +knight+) Knight doent really have an increment
           ; (T))

          ((eql (piece-id piece) +bishop+)
           (get-bishop-directions dx dy))

          ((eql (piece-id piece) +rook+)
           (get-rook-directions dx dy))

          ((eql (piece-id piece) +queen+)
           (get-queen-directions dx dy))
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


(defun is-attacking (board square turn king-square logger)
  (piece-legal-move square (flip turn) board (piece-square square) king-square logger)
)

; Check if any pieces attack the king square

; This function is meant to be called whenever the user 
; makes a king move, and therefor he could have walked 
; into any enemy piece, and so we need to check them all
; to see if any of them attack the new king square
; color is the color of the side wanting to know if they're in check
(defun in-check (board color king-square logger &optional (discovered nil))
  (for:for ((square over board)) 
		   (when (and 
                   (not (eq square 0))
                   (not (eq (piece-id square) +king+))
                   (not (eql (piece-square square) king-square))
                   (not (eql (piece-color square) color))
                   (if discovered 
                     (member (piece-id square) (list +bishop+ +queen+ +rook+))
                               T)


                   (is-attacking board square color king-square logger))
			 (return square)))
  )

; This function is meant to be called after the opponent
; moved a piece, we want to know if he put us in check,
; but we only need to check the piece he moved,
; and for discovered checks


; Check that the king is not walking into check
(defun castle-check-check (color board start stop increment logger)
  (let ((next start))
	(loop 
	  (when (in-check board color next logger)
		(return nil))

	  (when (eql next stop)
		(return T))

	  (setf next (+ next increment))
	  )
	)
  )



