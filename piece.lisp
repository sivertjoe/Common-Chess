(in-package #:chess)


(defun piece-create(color id x y)
  (list color id (sdl2:make-rect x y +square-width+ +square-height+)
  ))

(defun piece-rect (piece)
  (caddr piece)
  )

(defun piece-color(piece)
  (car piece)
  )

(defun piece-get-texture-index (color piece-type)
  (+ (* 2 piece-type) color)
  )

(defun piece-id (piece)
  (cadr piece)
  )


(defun piece-index (piece)
  (piece-get-texture-index (piece-color piece) 
						   (piece-id piece))
  )

(defun make-rect-from-square (square)
  (let ((x (* (mod square 8) +square-width+))
		(y (- (- +screen-height+ +square-height+) (* +square-height+ (nth-value 0 (floor square 8))))))
	  (sdl2:make-rect x y +square-width+ +square-height+)
    )
  )

(defun piece-place (piece square)
  (list (piece-color piece)
		(piece-id piece)
		(make-rect-from-square square))
  )

(defun piece-move (piece x y)
  (list (piece-color piece) 
		(piece-id piece) 
		(sdl2:make-rect x y +square-width+ +square-height+))
  )

