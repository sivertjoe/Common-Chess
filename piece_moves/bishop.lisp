(in-package #:chess)


(defun get-directions (dx dy)
 (values (/ dx (abs dx)) (/ dy (abs dy))) 
  )

(defun next-square (square x y)
  (+ square (* y (+ 8 (* x y))))
  )

(defun was-diagonal (dx dy)
  (eq (abs dx) (abs dy))
  )

(defun check-path (color board start stop x y)
  (let ((next start))
	(loop 
	  (setf next (next-square next x y))

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

(defun legal-bishop-move (color board start stop)
  (unless (eq start stop)
	(multiple-value-bind (dx dy) (get-square-diff start stop)
	  (when (was-diagonal dx dy)
	    (multiple-value-bind (x y) (get-directions dx dy)
		  (check-path color board start stop x y))
		)
	  )
	)
  )

