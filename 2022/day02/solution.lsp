(load "/home/michael/.sbclrc")
(ql:quickload :cl-ppcre)

(defvar input)
(defvar rock (list :X 4 :Y 8 :Z 3)) ; draw win lose
(defvar paper (list :X 1 :Y 5 :Z 9)) ; lose draw win
(defvar scissors (list :X 7 :Y 2 :Z 6)) ; win lose draw
(defvar score-map (list :A rock :B paper :C scissors))

(defvar lose (list :A "Z" :B "X" :C "Y"))
(defvar draw (list :A "X" :B "Y" :C "Z"))
(defvar win (list :A "Y" :B "Z" :C "X"))
(defvar move-map (list :X lose :Y draw :Z win))

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)

(defun make-pairs (l)
  (mapcar #'(lambda (x) (cl-ppcre:split "\\s+" x)) l)
)

(defun get-last (l)
  (car (cdr l))
)

(defun get-plist-value (l key)
  (getf l (read-from-string (concatenate 'string ":" key)))
)

(defun get-score (move)
  (get-plist-value (get-plist-value score-map (car move)) (get-last move))
)

(defun get-my-move (pair)
  (get-plist-value (get-plist-value move-map (get-last pair)) (car pair))
)

(defun part-1 (input)
  (reduce #'+ (mapcar #'get-score input))
)

(defun part-2 (input)
  (part-1 (mapcar (lambda (x) (list (car x) (get-my-move x))) input))
)

(setq input (make-pairs (load-file "input.txt")))
(format t "Part 1: ~a~%" (part-1 input))
(format t "Part 2: ~a~%" (part-2 input))
