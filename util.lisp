(in-package #:chess)


(defun correct-color (piece turn)
  (eql (piece-color piece) turn)
  )


(defun add-piece (textures path piece index)
  (setf (aref textures index) (sdl2:load-bmp (concatenate 'string path piece)))
  )

(defun create-texture-set(path)
  (let ((textures (make-array '(12))))
	(add-piece textures path "pawn_w.bmp" (piece-get-texture-index +white+ +pawn+ ))
	(add-piece textures path "pawn_b.bmp" (piece-get-texture-index +black+ +pawn+ ))

	(add-piece textures path "knight_w.bmp" (piece-get-texture-index +white+ +knight+ ))
	(add-piece textures path "knight_b.bmp" (piece-get-texture-index +black+ +knight+ ))

	(add-piece textures path "bishop_b.bmp" (piece-get-texture-index +black+ +bishop+ ))
	(add-piece textures path "bishop_w.bmp" (piece-get-texture-index +white+ +bishop+ ))

	(add-piece textures path "rook_b.bmp" (piece-get-texture-index +black+ +rook+ ))
	(add-piece textures path "rook_w.bmp" (piece-get-texture-index +white+ +rook+ ))

	(add-piece textures path "queen_b.bmp" (piece-get-texture-index +black+ +queen+ ))
	(add-piece textures path "queen_w.bmp" (piece-get-texture-index +white+ +queen+ ))

	(add-piece textures path "king_b.bmp" (piece-get-texture-index +black+ +king+ ))
	(add-piece textures path "king_w.bmp" (piece-get-texture-index +white+ +king+ ))


	textures
	  )
 )

(defun get-texture(textures piece)
  (aref textures (piece-index piece))
  )


(defun start-square-and-y-pos (color)
  (if (eq color +white+)
	(values 8 600)
	(values 48 100))
  )

(defun create-pawns (board color)
  (let ((x 0))
    (multiple-value-bind (start y ) (start-square-and-y-pos color)
	  (loop repeat 8 do
		(setf (aref board start) (piece-create color +pawn+ x y))
		(setf start (+ start 1))
		(setf x (+ x 100))
	    )
	  )
    )
  )

(defun create-officers (board color p-type startw startb x1 x2 inc)
  (let ((start 
		  (if (eql color +white+) 
			startw	
			startb))
		(y 
		  (if (eql color +white+)
			700
			0)))
	(setf (aref board start) (piece-create color p-type x1 y)) 
	(setf (aref board (+ start inc)) (piece-create color p-type x2 y)) 
	)
  )

(defun create-rooks (board color)
  (create-officers board color +rook+ 0 56 0 700 7)
  )

(defun create-knights (board color)
  (create-officers board color +knight+ 1 57 100 600 5)
  )

(defun create-bishops (board color)
  (create-officers board color +bishop+ 2 58 200 500 3)
  )


(defun royal-square(piece-type color)
  (if (eq piece-type +queen+)
	(if (eq color +white+)
	  3
	  56)
	(if (eq color +white+)
	  4
	  57)
	)
  )


(defun create-board-set()
  (let ((board (make-array '(64))))
	(create-pawns board +white+)
	(create-pawns board +black+)

	(create-rooks board +white+)
	(create-rooks board +black+)

	(create-knights board +white+)
	(create-knights board +black+)

	(create-bishops board +white+)
	(create-bishops board +black+)

	(setf (aref board 3) (piece-create +white+ +queen+ 300 700)) 
	(setf (aref board 59) (piece-create +black+ +queen+ 300 0)) 


	(setf (aref board 4) (piece-create +white+ +king+ 400 700)) 
	(setf (aref board 60) (piece-create +black+ +king+ 400 0)) 


	board
	)
)


(defun board-place-piece (board piece square)
	(setf (aref board square) piece)  
  )


(defun flip (n)
  (if (eql n 1)
	0
	1)
  )



(defun legal-move(piece turn board start-square stop-square logger)
  (when (and (correct-color piece turn) (not (eq start-square stop-square)))
	; Can't use case here??
    (cond 
	  ((eql (piece-id piece) +pawn+) 
	   (legal-pawn-move turn board start-square stop-square logger))

	  ((eql (piece-id piece) +knight+)
	   (legal-knight-move turn board start-square stop-square))

	  ((eql (piece-id piece) +bishop+)
	   (legal-bishop-move turn board start-square stop-square))

	  ((eql (piece-id piece) +rook+)
	   (legal-rook-move turn board start-square stop-square))

	  ((eql (piece-id piece) +queen+)
	   (legal-queen-move turn board start-square stop-square))

	  ((eql (piece-id piece) +king+)
	   (legal-king-move turn board start-square stop-square logger))
	  )
	)
  )










