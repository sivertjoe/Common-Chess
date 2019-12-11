(in-package #:chess)

(defun get-coords (pos)
  (values (mod pos 8) 
	      (nth-value 0 (floor pos 8)))
  )
(defun board-piece-p (board square)
  (aref board square)
  )


(defun no-collision (color square board)
  (let ((piece (board-piece-p board square)))
	(if (not (eq piece 0))
		(not (eq (piece-color piece) color))
		T
	  )
	)
  )

(defun square-clear (board square)
  (eq (board-piece-p board square) 0)
  )



(defun get-square-diff (start stop)
  (multiple-value-bind (start-x start-y) (get-coords start)
	(multiple-value-bind (stop-x stop-y) (get-coords stop)
	  (values (- stop-x start-x) (- stop-y start-y))
	  )
	)
  )


(defun check-path (color board start stop increment)
  (let ((next start))
	(loop 
	  (setf next (+ next increment))

	  (when (eq next stop)
		(return (no-collision color next board))
		)

	  (when (and ; if next != stop and !square-clear { return false; }
			  (not (eq next stop)) 
			  (not (square-clear board next)))
		(return nil))
	  )
	)
  )
