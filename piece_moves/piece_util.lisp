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
	  (let ((dx (- stop-x start-x))
			(dy (- stop-y start-y)))
		(values dx dy)

		)
	  )
	)
  )

