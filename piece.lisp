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

(defun piece-place (piece square)
  (let ((x (* (mod square 8) +square-width+))
		(y (- (- +screen-height+ +square-height+) (* +square-height+ (nth-value 0 (floor square 8))))))
	(piece-move piece x y)
    )
  )


(defun piece-move (piece x y)
  (funcall (fdefinition '(setf sdl2:rect-x)) x (piece-rect piece))
  (funcall (fdefinition '(setf sdl2:rect-y)) y (piece-rect piece))
  )

