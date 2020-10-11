(in-package #:chess)


(defparameter +screen-width+ 800)
(defparameter +screen-height+ 800)

(defparameter +square-width+ (/ +screen-width+ 8))
(defparameter +square-height+ (/ +screen-height+ 8))

(defparameter +piece-width+ +square-width+)
(defparameter +piece-height+ +square-height+)

(defparameter +pawn+ 0)
(defparameter +knight+ 1)
(defparameter +bishop+ 2)
(defparameter +rook+ 3)
(defparameter +queen+ 4)
(defparameter +king+ 5)

(defparameter +no-capture+ 6)
(defparameter +en-passant+ 0)
(defparameter +castle+ 1)


(defparameter +white+ 1)
(defparameter +black+ 0)


(defparameter +white-king-start+ 4)
(defparameter +white-short+ 6)
(defparameter +white-long+ 2)


(defparameter +black-king-start+ 60)
(defparameter +black-short+ 62)
(defparameter +black-long+ 58)


(defparameter +red+ 0)
(defparameter +green+ 1)

