(in-package #:chess)


(defun legal-movement (dx dy)
  (or 
	(and 
	  (eq (abs dx) 2)
	  (eq (abs dy) 1))
	(and 
	  (eq (abs dx) 1)
	  (eq (abs dy) 2)))
  )


(defun legal-knight-move(color board start stop)
  (multiple-value-bind (start-x start-y) (get-coords start)
	(multiple-value-bind (stop-x stop-y) (get-coords stop)
	  (let ((dx (- stop-x start-x))
			(dy (- stop-y start-y)))
		(and (legal-movement dx dy) (no-collision color stop board))
		  )
			))
  )

