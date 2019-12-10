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

(defun draw(surface)
  (draw-board surface)  
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
							)))
	move)
  )

; fixes some edge
(defun evaluate-move(move piece from-square to-square board)
  (let ((log-entry (create-log-entry piece from-square to-square board)))
	(cond
	  ((eq move +en-passant+)
	     (setf log-entry (set-en-passant log-entry 1))
		 (setf log-entry (set-capture-id log-entry +pawn+))
	   )
	  )	  
	log-entry
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

	)
  )



(defun update ()
  (cond
	((and (eq *piece* 0) (mouse-click)) ; if no piece is held and left mouse is pressed
	 (let ((square (mouse-square +square-width+ +square-height+)))
	   (setf *piece* (aref *board* square))
	   (setf (aref *board* square) 0)
	   (setq *piece-start-square* square)))

	((not (eq *piece* 0)) ; Holding a piece
	 (if (mouse-click) 
	   (multiple-value-bind (x-pos y-pos) (mouse-piece-pos +piece-width+ +piece-height+) ; Moving a piece around
		 (setf *piece* (piece-move *piece* x-pos y-pos)))

	   (let* ((stop-square (mouse-square +square-width+ +square-height+))
			  (move (legal-move *piece* *turn* *board* *piece-start-square* stop-square *log*))) ; let go of mouse left

		 (if move
		   (progn 
			 (setf (aref *board* stop-square) (piece-place *piece* stop-square)) ; Move the piece to the square
				 
			 ; Log the move, check for stuff like en-passant and capture first
			 (push
			   (evaluate-move move *piece* *piece-start-square* stop-square *board*) 
			   *log*) 

				 
			 ; Fix stuff like en-passant and castle
			 (fix-edge-cases move *piece* stop-square *board*)
			 (setf *turn* (flip *turn*))
			 )	
			 
		   ; Place back
		   (setf (aref *board* *piece-start-square*) (piece-place *piece* *piece-start-square*)))

		 
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
  
