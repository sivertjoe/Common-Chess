(in-package #:chess)

(defun tried-to-castle (color start stop)
  (if (eq color +white+)
	(and (eq start +white-king-start+)
		 (or 
		   (eq stop +white-short+)
		   (eq stop +white-long+)))

	(and (eq start +black-king-start+)
		 (or 
		   (eq stop +black-short+)
		   (eq stop +black-long+))))
  )



(defun castle-direction (start stop)
  (if (> start stop)
	-1
	1
	)
  )


(defun get-rook (king-square direction) 
  (if (eql direction 1)
	(+ king-square 3)
	(- king-square 4)) 
  )

; if it you can castle for -> T, else nil, then !T = nil
(defun castle-right (color logger king-square direction)
  (let ((rook (get-rook king-square direction)))
   (not (for:for ((move in (reverse logger)))
			(when (or 
					(eql (get-from-square move) king-square)
					(eql (get-from-square move) rook))
			  (return T))
			 ))
   )
  )

(defun castle (color board start stop logger)
  (let ((direction (castle-direction start stop)))
	(when (and 
			(square-clear board stop)
			(check-path color board start stop direction))
	  (when (and 
		(castle-check-check color board start stop direction logger)
		(castle-right color logger start direction))
		  +castle+
		)
	  )
	)
  )


(defun moved-one-square (start stop)
    (multiple-value-bind (dx dy) (get-square-diff start stop)
	  (and 
		(< (abs dx) 2)
		(< (abs dy) 2)
		)
	  )
  )

(defun normal-move (color board start stop logger)
	(and 
	  (square-landable color stop board) 
	  (not (in-check board color stop logger :option stop))
	  (moved-one-square start stop))
  )

; if it you can castle for -> T, else nil, then !T = nil
(defun legal-king-move (color board start stop logger &key option)
  (if (tried-to-castle color start stop)
	(castle color board start stop logger)
	(normal-move color board start stop logger)
	)
  )


