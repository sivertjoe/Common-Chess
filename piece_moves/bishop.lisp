(in-package #:chess)


(defun get-bishop-directions (dx dy)
  (cond
	((and (< dx 0) (< dy 0)) -9)
	((and (> dx 0) (> dy 0))  9)

	((and (< dx 0) (> dy 0))  7)
	((and (> dx 0) (< dy 0)) -7)
	)
  )

(defun was-diagonal (dx dy)
  (eq (abs dx) (abs dy))
  )

(defun legal-bishop-move (color board start stop &key option)
  (when (or (square-landable color stop board) (eql stop option))
    (multiple-value-bind (dx dy) (get-square-diff start stop)
	  (when (was-diagonal dx dy)
		(check-path color board start stop (get-bishop-directions dx dy))
		)
	  )
	)
  )

