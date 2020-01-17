(in-package #:chess)

(defun legal-queen-move (color board start stop)
  (multiple-value-bind (dx dy) (get-square-diff start stop)
	(cond
      ((was-diagonal dx dy)
	    (check-path color board start stop (get-bishop-directions dx dy)))

      ((is-straight dx dy)
	    (check-path color board start stop (get-rook-directions dx dy))))
	)
  )
