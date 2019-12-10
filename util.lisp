(in-package #:chess)


(defun correct-color (piece turn)
  (eq (piece-color piece) turn)
  )


(defun add-piece (textures path piece index)
  (setf (aref textures index) (sdl2:load-bmp (concatenate 'string path piece)))
  )

(defun create-texture-set(path)
  (let ((textures (make-array '(12))))
	(add-piece textures path "_pawn_w.bmp" (piece-get-texture-index +white+ +pawn+ ))
	(add-piece textures path "_pawn_b.bmp" (piece-get-texture-index +black+ +pawn+ ))

	(add-piece textures path "knight_w.bmp" (piece-get-texture-index +white+ +knight+ ))
	(add-piece textures path "knight_b.bmp" (piece-get-texture-index +black+ +knight+ ))

	(add-piece textures path "bishop_b.bmp" (piece-get-texture-index +black+ +bishop+ ))
	(add-piece textures path "bishop_w.bmp" (piece-get-texture-index +white+ +bishop+ ))
	textures
	  )
 )

(defun get-texture(textures piece)
  (aref textures (piece-index piece))
  )


(defun create-board-set()
  (let ((board (make-array '(64))))
	(setf (aref board 15) (piece-create +white+ +pawn+ 700 600)) ; Create white pawn
	(setf (aref board 14) (piece-create +white+ +pawn+ 600 600)) ; Create white pawn
	(setf (aref board 13) (piece-create +white+ +pawn+ 500 600)) ; Create white pawn
	(setf (aref board 12) (piece-create +white+ +pawn+ 400 600)) ; Create white pawn
	(setf (aref board 11) (piece-create +white+ +pawn+ 300 600)) ; Create white pawn
	(setf (aref board 10) (piece-create +white+ +pawn+ 200 600)) ; Create white pawn
	(setf (aref board 9) (piece-create +white+ +pawn+ 100 600)) ; Create white pawn
	(setf (aref board 8) (piece-create +white+ +pawn+ 0 600)) ; Create white pawn

	(setf (aref board 1) (piece-create +white+ +knight+ 100 700))
	(setf (aref board 62) (piece-create +black+ +knight+ 600 0)) 

	(setf (aref board 51) (piece-create +black+ +pawn+ 300 100)) 
	(setf (aref board 52) (piece-create +black+ +pawn+ 400 100)) 

	(setf (aref board 2) (piece-create +white+ +bishop+ 200 700)) 
	(setf (aref board 58) (piece-create +black+ +bishop+ 200 0)) 
	board
	)
)


(defun flip (n)
  (if (eq n 1)
	0
	1)
  )




(defun legal-move(piece turn board start-square stop-square logger)
  (if (or (not (correct-color piece turn)) (eq start-square stop-square))
	nil
    (cond 
	  ((eq (piece-id piece) +pawn+) 
	   (legal-pawn-move turn board start-square stop-square logger))

	  ((eq (piece-id piece) +knight+)
	   (legal-knight-move turn board start-square stop-square))

	  ((eq (piece-id piece) +bishop+)
	   (legal-bishop-move turn board start-square stop-square))
	  )
	)
  )










