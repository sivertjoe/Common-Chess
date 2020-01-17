(in-package #:chess)

(defun _assert (c)
  (if c
  (format t "Success! ~c" #\linefeed) 
  (format t "Error ~c" #\linefeed))
  )



(defvar +test-color+ 1)
(defvar +test-id+ 5)
(defvar +test-capture+ 3)
(defvar +test-square+ 55)
(defvar +test-promotion+ 2)
(defvar +test-en-passant+ 1)
(defvar +test-castle-value+ 1)

(defun check-color (test)
  (_assert (eq (get-color test) +test-color+))
  )

(defun check-id (test)
  (_assert (eq (get-id test) +test-id+))
  )
(defun check-square (val)
  (_assert (eq (get-from-square val) +test-square+))
  )

(defun check-to-square (val)
  (_assert (eq (get-to-square val) +test-square+))
  )

(defun check-capture (val)
  (_assert (eq (get-capture-id val) +test-capture+))
  )

(defun check-promotion (val)
  (_assert (eq (get-promotion-id val) +test-promotion+))
  )

(defun test-en-passant (test)
  (_assert (eq (get-en-passant test) +test-en-passant+))
  )


(defun test-castle (test)
  (_assert (eq (get-castle test) +test-castle-value+))

  )


; (defun log-move (p-color p-id from to cap-id promote-id en-passant)

(defun check-log-move ()
  (let ((move (create-move +test-color+ +test-id+ +test-square+ +test-square+ +test-capture+ +test-promotion+ +test-en-passant+ +test-castle-value+)))
    (check-color move)
    (check-id move)
    (check-to-square move)
    (check-capture move)
    (check-promotion move)
    (check-square move)
    (test-en-passant move)
	(test-castle move)
    )
  )


(defun test ()
  (format t "~%")
  (let ((test (set-color +test-color+)))
    (setf test (set-id test +test-id+))
    (setf test (set-from-square test +test-square+))
    (setf test (set-to-square test +test-square+))
    (setf test (set-capture-id test +test-capture+))
    (setf test (set-promotion-id test +test-promotion+))
    (setf test (set-en-passant test +test-en-passant+))
    (setf test (set-castle test +test-castle-value+))

    (check-color test)
    (check-id test)
    (check-to-square test)
    (check-capture test)
    (check-promotion test)
    (check-square test)
    (test-en-passant test)
	(test-castle test)
    )

    (check-log-move)
)




