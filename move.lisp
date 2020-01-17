(in-package #:chess)

(defun lsh (val sh)
  (ash val sh)
 )

(defun rsh (val sh)
  (ash val (- 0 sh))
  )



(defvar +single-bit+ 1)
(defvar +id-bits+ 7)
(defvar +square-bits+ 63)


(defvar +id-len+ 3)
(defvar +square-len+ 6)


(defvar +m-en-passant+                                0)
(defvar +m-castle+     (+ +m-en-passant+ +single-bit+))
(defvar +promotion+    (+ +m-castle+  +single-bit+))
(defvar +cap-id+       (+ +promotion+   +id-len+))
(defvar +square-to+    (+ +cap-id+      +id-len+))
(defvar +square-from+  (+ +square-to+   +square-len+))
(defvar +id+           (+ +square-from+ +square-len+))
(defvar +color+        (+ +id+          +id-len+))




(defun set-value(src val)
  (lsh src val)
  )

(defun include-value (val append-value sh)
  (logior val (lsh append-value sh))
  )

(defun get-value (val n-bits sh)
  (rsh (logand val (lsh n-bits sh)) sh)
)



; Color is always used to create the number/record, therefor it's a little
; different
(defun set-color(val) (set-value val +color+))
(defun get-color(val) (get-value val 1 +color+))


(defun set-id(val id) (include-value val id +id+))
(defun get-id(val) (get-value val +id-bits+ +id+))


(defun set-from-square (val square) (include-value val square +square-from+))
(defun get-from-square (val) (get-value val +square-bits+ +square-from+))


(defun set-to-square (val square) (include-value val square +square-to+))
(defun get-to-square (val) (get-value val +square-bits+ +square-to+))


(defun set-capture-id (val id) (include-value val id +cap-id+))
(defun get-capture-id (val) (get-value val +id-bits+ +cap-id+))


(defun set-promotion-id (val id) (include-value val id +promotion+))
(defun get-promotion-id (val) (get-value val +id-bits+ +promotion+))


(defun set-en-passant (val ep) (include-value val ep +m-en-passant+))
(defun get-en-passant (val) (get-value val +single-bit+ +m-en-passant+))


(defun set-castle (val ep) (include-value val ep +m-castle+))
(defun get-castle (val) (get-value val +single-bit+ +m-castle+))




(defun create-move (p-color p-id from to cap-id promote-id en-passant castle)
  (let ((move (set-color p-color)))
    
	(set-castle 

    (set-en-passant 
      
      (set-promotion-id 
        
        (set-capture-id 
          
          (set-to-square 

            (set-from-square 
              (set-id move p-id) 

            from) 
          
          to) 

        cap-id) 
      
      promote-id) 
        
    en-passant)

	castle)
    )
  )





