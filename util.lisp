(in-package #:chess)

(defmacro switch (var &rest rest)
  (cond
    ((null rest) nil)
    (T `(if (eql ,var ,(first (first rest)))
           (progn ,@(rest (first rest)))
           (switch ,var ,@(rest rest))))))


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
            (return nil)))
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




; This function checks if the piece moved is still on the attacking axis
; First two conditions are rook like movements
; Last two are bishop like movements
(defun get-attacking-line-from-dx-dy (dx dy start stop)
    (cond
      ((eql dy 0)
        (eql (get-board-rank start) (get-board-rank stop)))

      ((eql dx 0)
        (eql (get-board-file start) (get-board-file stop)))

      ((or
        (and (> dx 0) (> dy 0))
        (and (< dx 0) (< dy 0)))
        (eql (mod start 9) (mod stop 9)))

      ((or 
         (and (> dx 0) (< dy 0))
         (and (< dx 0) (> dy 0)))
         (eql (mod start 7) (mod stop 7)))))



(defun stil-on-pin-line (king-square start-square stop-square)
    (multiple-value-bind (dx dy) (get-square-diff start-square king-square)
      (get-attacking-line-from-dx-dy dx dy start-square stop-square)))


; if we enter this function it implies the king _is_ pinned
; and we moved the piece that was pinned to the king
(defun handle-pin-edge-case (board turn start-square stop-square king-square)
  (or 
    (move-captured board stop-square turn)
    (stil-on-pin-line king-square start-square stop-square)))

(defun not-king-move-was-legal (move turn board start-square stop-square logger check-piece)
  (and  
    move
    (if check-piece
      (un-check check-piece (get-king-square board turn) stop-square) 
      (or 
                                                                 ; Only check for discovered checks
        (not (in-check board turn (get-king-square board turn) logger :option :discovered)) 
        (handle-pin-edge-case board turn start-square stop-square (get-king-square board turn)))))

  )


(defun not-king-moves (move turn board start-square stop-square logger check-piece)
    (when (not-king-move-was-legal move turn board start-square stop-square logger check-piece)
      move))

; piece = the piece that moved
; turn  = the color of whose move it is
; board = board
; start-square & stop-square = yes
; logger = yes
; check-piece = if I am in check, this piece is checking me
(defun legal-move(piece turn board start-square stop-square logger check-piece)
  (when (or (not (correct-color piece turn)) (eql start-square stop-square))
    (return-from legal-move nil))

  (let ((move (piece-legal-move piece turn board start-square stop-square logger)))
    (if (eql (piece-id piece) +king+)
      move ; Legal king move handles check
      (not-king-moves move turn board start-square stop-square logger check-piece)))
  )

(defun piece-legal-move(piece turn board start-square stop-square logger &key option)
  (piece-legal-move-id (piece-id piece) turn board start-square stop-square logger :option option))

(defun piece-legal-move-id(id turn board start-square stop-square logger &key option)
  (switch id 
    (+pawn+
     (legal-pawn-move turn board start-square stop-square logger :option option))

    (+knight+
     (legal-knight-move turn board start-square stop-square :option option))

    (+bishop+
     (legal-bishop-move turn board start-square stop-square :option option))

    (+rook+
     (legal-rook-move turn board start-square stop-square :option option))

    (+queen+
     (legal-queen-move turn board start-square stop-square :option option))

    (+king+
     (legal-king-move turn board start-square stop-square logger :option option))))
