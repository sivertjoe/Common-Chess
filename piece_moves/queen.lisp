(in-package #:chess)

(defun get-queen-directions (dx dy)
  (cond
      ((was-diagonal dx dy)
	    (get-bishop-directions dx dy))

      ((is-straight dx dy)
	    (get-rook-directions dx dy)))
  )


(defun legal-queen-move (color board start stop &key option)
  (when (or (square-landable color stop board) (eql option stop))
    (multiple-value-bind (dx dy) (get-square-diff start stop)
      (cond
        ((was-diagonal dx dy)
         (check-path color board start stop (get-bishop-directions dx dy)))

        ((is-straight dx dy)
         (check-path color board start stop (get-rook-directions dx dy))))
      )
    )
  )
