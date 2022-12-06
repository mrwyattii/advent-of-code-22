(load "/home/michael/.sbclrc")

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun all-unique (vals)
  (every #'identity
    (loop for i from 0 for v in vals collect
      (= (length (member v vals)) (- (length vals) i)))))

(defun find-marker (data-stream marker-len)
  (if (all-unique (subseq data-stream 0 marker-len))
    marker-len
    (1+ (find-marker (cdr data-stream) marker-len))))

(defun part-1 (data-stream)
  (find-marker data-stream 4))

(defun part-2 (data-stream)
  (find-marker data-stream 14))

(defvar *input* (coerce (car (load-file "input.txt")) 'list))
(format t "Part 1: ~a~%" (part-1 *input*))
(format t "Part 2: ~a~%" (part-2 *input*))
