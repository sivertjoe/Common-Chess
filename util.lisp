(in-package #:chess)


(defun correct-color (piece turn)
  (eql (piece-color piece) turn)
  )


(defun add-piece (textures path piece index)
  (setf (aref textures index) (sdl2:load-bmp (concatenate 'string path piece)))
  )

(defun create-texture-set(path)
  (let ((textures (make-array '(12))))
	(add-piece textures path "pawn_w.bmp" (piece-get-texture-index +white+ +pawn+))
	(add-piece textures path "pawn_b.bmp" (piece-get-texture-index +black+ +pawn+))

	(add-piece textures path "knight_w.bmp" (piece-get-texture-index +white+ +knight+))
	(add-piece textures path "knight_b.bmp" (piece-get-texture-index +black+ +knight+))

	(add-piece textures path "bishop_b.bmp" (piece-get-texture-index +black+ +bishop+))
	(add-piece textures path "bishop_w.bmp" (piece-get-texture-index +white+ +bishop+))

	(add-piece textures path "rook_b.bmp" (piece-get-texture-index +black+ +rook+))
	(add-piece textures path "rook_w.bmp" (piece-get-texture-index +white+ +rook+))

	(add-piece textures path "queen_b.bmp" (piece-get-texture-index +black+ +queen+))
	(add-piece textures path "queen_w.bmp" (piece-get-texture-index +white+ +queen+))

	(add-piece textures path "king_b.bmp" (piece-get-texture-index +black+ +king+))
	(add-piece textures path "king_w.bmp" (piece-get-texture-index +white+ +king+))

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


(defun is-king-square (board color square)
  (and 
    (not (eq square 0))
    (eql (piece-id square) +king+)
    (eql (piece-color square) color)
    )
  )

(defun get-king-square (board color) 
  (for:for ((square over board)) 
		   (when (is-king-square board color square)
			 (return (piece-square square))))
  )


; For the king, knight, and pawn, they
; _Have_ to capture the enemy piece to escape check
(defun captured (piece stop)
  (eql (piece-square piece) stop)
  )



; Get the piece increment
; if you hit the stop-square along the path it means you blocked the check
; if you hit the king-square along the path you did not block the check.
(defun block-path (piece king-square stop-square)
    (let ((incr (get-piece-increment piece king-square))
          (next (piece-square piece)))
        (loop 
          (setf next (+ next incr))

          (when (eql next stop-square)
            (return T))

          (when (eql next king-square)
            (return nil))
          )
      )
  )

; if we are in this function it means, we _are_ in check, now lets just check
; if the new move un-checks us
(defun un-check (attacking-piece king-square stop-square)
  (or 
    (captured attacking-piece stop-square)
    (block-path attacking-piece king-square stop-square)
    )
   )


(defun not-king-moves (piece turn board start-square stop-square logger check-piece)
  (and 
    (piece-legal-move piece turn board start-square stop-square logger)

    (if check-piece
      (un-check check-piece (get-king-square board turn) stop-square) 
      (not (in-check board turn (get-king-square board turn) logger T))
      )
    )
  )

; piece = the piece that moved
; turn  = the color of whose move it is
; board = board
; start-square & stop-square = yes
; logger = yes
; check-piece = if I am in check, this piece is checking me
(defun legal-move(piece turn board start-square stop-square logger check-piece)
  (when (or (not (correct-color piece turn)) (eql start-square stop-square))
    (return-from legal-move nil))

  (cond 
    ((eql (piece-id piece) +king+)
     (legal-king-move turn board start-square stop-square logger))

    ((not (eql (piece-id piece) +king+))
     (not-king-moves piece turn board start-square stop-square logger check-piece))
    )
  )

(defun piece-legal-move(piece turn board start-square stop-square logger)
  (piece-legal-move-id (piece-id piece) turn board start-square stop-square logger)
  )

(defun piece-legal-move-id(id turn board start-square stop-square logger)
  (cond 
    ((eql id +pawn+) 
     (legal-pawn-move turn board start-square stop-square logger))

    ((eql id +knight+)
     (legal-knight-move turn board start-square stop-square))

    ((eql id +bishop+)
     (legal-bishop-move turn board start-square stop-square))

    ((eql id +rook+)
     (legal-rook-move turn board start-square stop-square))

    ((eql id +queen+)
     (legal-queen-move turn board start-square stop-square))

    ((eql id +king+)
     (legal-king-move turn board start-square stop-square logger))
    )
  )
