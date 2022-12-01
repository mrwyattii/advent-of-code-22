(defvar input)
(defvar tmp)

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)

(defun split-list (l delim)
  (if (> 2 (length l))
    (list l)
    (if (string= delim (car l))
      (cons (list) (split-list (cdr l) delim))
      (progn
        (setq tmp (split-list (cdr l) delim))
        (cons (append (list (car l)) (car tmp)) (cdr tmp))
      )
    )
  )
)

(defun convert-to-int (l)
  (mapcar #'parse-integer l)
)

(defun sum-calories (calories)
  (reduce #'+ calories)
)

(defun part-1 (input)
  (car (sort (map 'list #'sum-calories input) '>))
)

(defun part-2 (input)
  (reduce #'+ (subseq (sort (map 'list #'sum-calories input) '>) 0 3))
)

(setq input (mapcar #'convert-to-int (split-list (load-file "input.txt") "")))
(format t "Max calories (part 1): ~a~%" (part-1 input))
(format t "Top 3 calories sum (part 2): ~a" (part-2 input))
