(load "/home/michael/.sbclrc")
(ql:quickload :cl-ppcre)

(defvar input)
(defvar tmp)
(defvar crates)
(defvar moves)

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)

(defun split-list (l &optional (delim ""))
  (if (= 1 (length l))
    (list l)
    (let ((tmp (split-list (rest l) delim)))
      (if (string= delim (first l))
        (cons (list) tmp)
        (cons (append (list (first l)) (first tmp)) (rest tmp))
      )
    )
  )
)

(defun str-to-list (str)
  (mapcar #'string (coerce str 'list))
)

(defun remove-last (l)
  (reverse (rest (reverse l)))
)

(defun transpose (matrix)
  (apply #'mapcar #'list matrix)
)

(defun remove-empty (crate-stack)
  (if (equal (first crate-stack) " ")
    (remove-empty (rest crate-stack))
    crate-stack
  )
)

(defun extract-crate-data (crate-row)
  (loop for (a val c d) on (str-to-list crate-row) by #'cddddr collect val)
)

(defun get-crates (crate-data)
  (mapcar #'remove-empty
    (transpose
      (mapcar #'extract-crate-data
        (remove-last crate-data)
      )
    )
  )
)

(defun extract-move-data (move)
  (cl-ppcre:register-groups-bind (move-n move-from move-to)
    ("move (\\d+) from (\\d+) to (\\d+)" move :sharedp t)
    (list (parse-integer move-n)
          (- (parse-integer move-from) 1)
          (- (parse-integer move-to) 1)
    )
  )
)

(defun get-top-crates (crates pos num &optional (rev nil))
  (let ((tmp (subseq (first (subseq crates pos)) 0 num)))
    (if rev (reverse tmp) tmp)
  )
)

(defun process-move (crates move &optional (multi-move nil))
  (destructuring-bind (move-n move-from move-to) move
    (let ((crates-to-move (get-top-crates crates move-from move-n multi-move)))
      (loop for i from 0 for stack in crates collect
        (cond
          ((equal move-from i) (subseq stack move-n (length stack)))
          ((equal move-to i) (append crates-to-move stack))
          (t stack)
        )
      )
    )
  )
)

(defun part-1 (moves crates)
  (format nil "~{~A~}"
    (mapcar #'first
      (reduce #'process-move moves :initial-value crates)
    )
  )
)

(defun part-2 (moves crates)
  (format nil "~{~A~}"
    (mapcar #'first
      (reduce (lambda (x y) (process-move x y t)) moves :initial-value crates)
    )
  )
)

(setq input (split-list (load-file "input.txt") ""))
(setq crates (get-crates (first input)))
(setq moves (mapcar #'extract-move-data (first (last input))))
(format t "Part 1: ~a~%" (part-1 moves crates))
(format t "Part 2: ~a~%" (part-2 moves crates))
