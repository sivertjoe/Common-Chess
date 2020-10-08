;;;; chess.lisp
(in-package #:chess)


; Global Variables
(defvar *board-image* nil)
(defvar *board* nil)
(defvar *textures* nil)
(defvar *turn* 1)


(defvar *piece* 0)
(defvar *piece-start-square* 0)
(defvar *log* nil)

(defvar *check-piece* nil)
(defvar *check-square* -1)
(defvar *red-texture* nil)




(defun draw-board(surface)
  (sdl2:blit-surface *board-image* nil surface nil)

)


(defun draw-piece(surface square)
  (unless (eq square 0)
	(sdl2:blit-surface (get-texture *textures* square) 
					   nil 
					   surface 
					   (piece-rect square)))
)


(defun draw-pieces (surface)
  (for:for ((square over *board*)) 
		   (draw-piece surface square))
  (draw-piece surface *piece*)
)

(defun rect-from-square (square)
    (let ((x (mod square 8))
          (y (- 7 (nth-value 0 (floor (/ square 8))))))
      (sdl2:make-rect (* 100 x) (* 100 y) +square-width+ +square-height+)
      )
  )

(defun draw-check-square (surface)
  (unless (eql *check-square* -1)
	(sdl2:blit-surface *red-texture*
					   nil 
					   surface 
					   (rect-from-square *check-square*))
        
    )
)


(defun draw(surface)
  (draw-board surface)  
  (draw-check-square surface)
  (draw-pieces surface)
)

(defun get-capture-id-if-capture (board square)
  (let ((piece (aref board square)))
	  (if (eq piece 0)
		+no-capture+
		(piece-id piece))
	)
  )

; p-color p-id from to cap-id promote-id en-passant
(defun create-log-entry(piece from-square to-square board)
  (let ((move (create-move  (piece-color piece) 
							(piece-id piece)
							from-square
							to-square
							(get-capture-id-if-capture board to-square)
							0
							0
							0
							)))
	move)
  )

; fixes some edge
(defun evaluate-move(move piece from-square to-square board)
  (let ((log-entry (create-log-entry piece from-square to-square board)))
	(cond
	  ((eq move +en-passant+)
	     (setf log-entry (set-en-passant log-entry 1))
		 (setf log-entry (set-capture-id log-entry +pawn+)))

	  ((eq move +castle+)
	     (setf log-entry (set-castle log-entry 1)))
	  )	  
	log-entry
	)
  )

(defun move-rook (board rook-pos stop-square)
  (let ((rook (aref board rook-pos)))
	(piece-place rook stop-square)
	rook
	)
  )

(defun fix-castle (board square)
  (cond 
	; white short castle
	((eql square 6)
	 (setf (aref board 5) (move-rook board 7 5))
	 (setf (aref board 7) 0))

	; white long castle
	((eql square 2)
	 (setf (aref board 3) (move-rook board 0 3))
	 (setf (aref board 0) 0))

	; Black short castle
	((eql square 62)
	 (setf (aref board 61) (move-rook board 63 61))
	 (setf (aref board 63) 0))

	; Black long castle
	((eql square 58)
	 (setf (aref board 59) (move-rook board 56 59))
	 (setf (aref board 56) 0))
	)
  )

(defun fix-edge-cases (move piece to-square board)
  (cond 
	((eq move +en-passant+)
	     (setf 
		   (aref board
				 (- to-square (* (relative-direction (piece-color piece)) 8))) 
		   0)
	   )

	((eq move +castle+)
	   (fix-castle board to-square))
	 )
  )


(defun touched-piece (piece)
	(and (eq piece 0) (mouse-click)) ; if no piece is held and left mouse is pressed
  )


(defun holding-piece (piece)
	(not (eq piece 0)) ; Holding a piece
  )

(defun move-piece (piece)
	   (multiple-value-bind (x-pos y-pos) (mouse-piece-pos +piece-width+ +piece-height+) ; Moving a piece around
		 (piece-move piece x-pos y-pos))
  )


(defun check-if-prev-move-made-check (board logger turn)
  (let ((prev (log-pop logger)))
    (when prev
      (piece-legal-move-id (get-id prev) (flip turn) board (get-to-square prev) (get-king-square board turn) logger)
      )
    )
  )

(defun update ()
  (cond

    ((touched-piece *piece*) ; if no piece is held and left mouse is pressed
     (let ((square (mouse-square +square-width+ +square-height+)))
       (setf *piece* (aref *board* square))
       (setf (aref *board* square) 0)
       (setq *piece-start-square* square)))


    ((holding-piece *piece*) 
     (if (mouse-click) ; Hasn't dropped the piece
       (move-piece *piece*)

    

       ; Let go of piece
       (let* ((stop-square (mouse-square +square-width+ +square-height+))
              (move (legal-move *piece* *turn* *board* *piece-start-square* stop-square *log* *check-piece*)))

         (if move ; If we got a move that was legal
           (progn 
             (piece-place *piece* stop-square)
             (board-place-piece *board* *piece* stop-square)

             ; Fix stuff like en-passant and castle
             (fix-edge-cases move *piece* stop-square *board*)

             (setf move (evaluate-move move *piece* *piece-start-square* stop-square *board*))

             (log-move *log* move)
             (setf *turn* (flip *turn*))

             (if (check-if-prev-move-made-check *board* *log* *turn*)
               (progn
                 (setf *check-square* (get-king-square *board* *turn*))
                 (setf *check-piece* *piece*))
               (progn
                 (setf *check-square* -1)
                 (setf *check-piece* nil))
               )
             )	

           (progn
             ; Place back
             (piece-place *piece* *piece-start-square*)
             (board-place-piece *board* *piece* *piece-start-square*)))


         (setf *piece* 0) ; Piece is let go
         )
       )
     )
    )
  )

(defun main-loop (window surface)
  (sdl2:with-event-loop (:method :poll)
                        (:idle ()
                               (update)
                               (draw surface)

                               (sdl2:update-window window)
                               )
                        (:quit () t))
  )


(defun init ()
  (setq *board-image* (sdl2:load-bmp "assets/board.bmp"))
  (setq *red-texture* (sdl2:load-bmp "assets/red.bmp"))
  (setf *board* (create-board-set))
  (setf *textures* (create-texture-set "assets/"))

  (setf *turn* 1)
  (setf *piece* 0)
  (setf *piece-start-square* 0)
  (setf *log* nil)
  )


(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
                   (sdl2:with-window (,window
                                       :title "Common-Chess"
                                       :w +screen-width+
                                       :h +screen-height+
                                       :flags '(:shown))
                                     (let ((,surface (sdl2:get-window-surface ,window)))
                                       ,@body)))
  )



(defun start ()
  (with-window-surface (window surface)
                       (init)
                       (main-loop window surface))
  )

