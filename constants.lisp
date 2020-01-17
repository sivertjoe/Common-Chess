(in-package #:chess)


(defvar +screen-width+ 800)
(defvar +screen-height+ 800)

(defvar +square-width+ (/ +screen-width+ 8))
(defvar +square-height+ (/ +screen-height+ 8))

(defvar +piece-width+ +square-width+)
(defvar +piece-height+ +square-height+)

(defvar +pawn+ 0)
(defvar +knight+ 1)
(defvar +bishop+ 2)
(defvar +rook+ 3)
(defvar +queen+ 4)
(defvar +king+ 5)

(defvar +no-capture+ 6)
(defvar +en-passant+ 0)
(defvar +castle+ 1)


(defvar +white+ 1)
(defvar +black+ 0)


(defvar +white-king-start+ 4)
(defvar +white-short+ 6)
(defvar +white-long+ 2)


(defvar +black-king-start+ 60)
(defvar +black-short+ 62)
(defvar +black-long+ 58)
