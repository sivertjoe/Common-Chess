(in-package #:chess)

; log is a list
(defmacro log-move (logger move)
  `(push
	 ,move
	 ,logger
	 )
  )

(defmacro log-pop (logger)
  `(car ,logger)
 ) 


(defun log-create ()
  nil
  )
