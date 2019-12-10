(in-package #:chess)


(defun y-axis-index (y square-height)
  (- 7 (nth-value 0 (floor y square-height)))
  )



(defun x-axis-index (x square-width)
  (nth-value 0 (floor x square-width))
  )

(defun mouse-square(square-width square-height)
  (multiple-value-bind (x y) (sdl2:mouse-state)
	(+ (x-axis-index x square-width) 
		(* (y-axis-index y square-height) 8))	
  )
)


(defun mouse-click ()
  (sdl2:global-mouse-state-p 1)
  )


(defun mouse-piece-pos(p-width p-height)
  (multiple-value-bind (x y) (sdl2:mouse-state)
	(values (- x (/ p-width 2))
			(- y (/ p-height 2))))
	
  )
