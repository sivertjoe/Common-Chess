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


(defun legal-knight-move(color board start stop &key option)
  (multiple-value-bind (dx dy) (get-square-diff start stop)
		(and (legal-movement dx dy) (or (square-landable color stop board) (eql option stop)))
    )
  )

