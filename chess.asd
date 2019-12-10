;;;; chess.asd

(asdf:defsystem #:chess
  :description "Describe chess here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2
  			   :for)
  :components ((:file "package")
  			   (:file "mouse")
  			   (:file "move")
  			   (:file "util")
  			   (:file "constants")
  			   (:file "piece")


  			   (:file "piece_moves/bishop")
  			   (:file "piece_moves/knight")
  			   (:file "piece_moves/pawn")
  			   (:file "piece_moves/piece_util")
               (:file "chess")))
