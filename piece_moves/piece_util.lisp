(in-package #:chess)

; Get x y coord of square
(defun get-coords (pos)
  (values (get-board-file pos) 
	      (get-board-rank pos))
  )

(defun get-board-file (pos)
  (mod pos 8)
  )

(defun get-board-rank (pos)
    (nth-value 0 (floor pos 8)))
           

(defun board-piece-p (board square)
  (aref board square)
  )


(defun square-clear (board square)
  (eq (board-piece-p board square) 0)
  )

(defun move-captured (board square color)
  (let ((piece (board-piece-p board square)))
	(and 
      (not (eql piece 0))
	  (not (eql (piece-color piece) color)))))



; Gets the x and y diff from two squares
(defun get-square-diff (start stop)
  (multiple-value-bind (start-x start-y) (get-coords start)
	(multiple-value-bind (stop-x stop-y) (get-coords stop)
	  (values (- stop-x start-x) (- stop-y start-y))
	  )
	)
  )


; Used to get a iterator number for the piece id of the piece,
; with the iterator number we can iterate from the start square to 
; the stop square (if we want to check if the path is clear of pieces feks)
(defun get-piece-increment (piece stop)
    (multiple-value-bind (dx dy) (get-square-diff (piece-square piece) stop)
        (switch (piece-id piece) 
          (+bishop+
           (get-bishop-directions dx dy))

          (+rook+
           (get-rook-directions dx dy))

          (+queen+
           (get-queen-directions dx dy)))))


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
(defun in-check (board color king-square logger &key option)
  (for:for ((square over board)) 
		   (when (and 
                   (not (eq square 0))
                   (not (eq (piece-id square) +king+))
                   (not (eql (piece-square square) king-square))
                   (not (eql (piece-color square) color))
                   (if (eq option :discovered)
                     (member (piece-id square) (list +bishop+ +queen+ +rook+))
                               T)


                   (is-attacking board square color king-square logger))
			 (return square)))
  )

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



