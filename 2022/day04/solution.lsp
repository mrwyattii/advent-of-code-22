(load "/home/michael/.sbclrc")
(ql:quickload :cl-ppcre)

(defvar input)

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)

(defun map-split-string (l &optional (delim "\\s+"))
  (mapcar (lambda (x) (cl-ppcre:split delim x)) l)
)

(defun map-str-to-int (l)
  (mapcar #'parse-integer l)
)

(defun count-trues (l)
  (reduce #'+ (mapcar (lambda (x) (if x 1 0)) l))
)

(defun get-sequences (input)
  (mapcar (lambda (x)
            (mapcar #'map-str-to-int (map-split-string x "-"))
          )
          (map-split-string input ",")
  )
)

(defun seq-is-subset (pair)
  (destructuring-bind ((a b) (x y)) pair
    (cond
      ((equal a x) t) ; If first values are same, must be subset
      ((< a x) (>= b y)) ; Else check if one range is inside the other
      (t (>= y b))
    )
  )
)

(defun seq-overlap (pair)
  (destructuring-bind ((a b) (x y)) pair
    (cond
      ((<= a x) (>= b x)) ; If a<=x, then we just need to check if b>=x
      (t (>= y a)) ; Otherwise we test if y>=a
    )
  )
)

(defun part-1 (input)
  (count-trues (mapcar #'seq-is-subset (get-sequences input)))
)

(defun part-2 (input)
  (count-trues (mapcar #'seq-overlap (get-sequences input)))
)

(setq input (load-file "input.txt"))
(format t "Part 1: ~a~%" (part-1 input))
(format t "Part 2: ~a~%" (part-2 input))
