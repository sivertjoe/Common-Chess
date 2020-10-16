(in-package #:chess)


(defun get-rook-directions (x y)
  (if (eq x 0)
	(* 8 (/ y (abs y)))
	(/ x (abs x)))
  )

(defun is-straight (dx dy) ; ÒwÓ
  (or
	(and 
	  (eq dx 0)
	  (not (eq dy 0)))
	(and 
	  (eq dy 0)
	  (not (eq dx 0))))
  )

(defun legal-rook-move (color board start stop &key option)
  (when (or (square-landable color stop board) (eql option stop))
	  (multiple-value-bind (dx dy) (get-square-diff start stop)
		(when (is-straight dx dy)
			(check-path color board start stop (get-rook-directions dx dy)))
		)
	  )
  )
