(load "/home/michael/.sbclrc")

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun str->int-list (str)
  (mapcar #'digit-char-p (coerce str 'list)))

(defun any-true-lists (l1 l2)
  (mapcar (lambda (x) (some #'identity x)) (mapcar #'list l1 l2)))

(defun reduce-matrices (matrices)
  (mapcar
    (lambda (x) (reduce #'any-true-lists x))
    (apply #'mapcar #'list matrices)))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defun flip (matrix)
  (mapcar #'reverse matrix))

(defun matrix-views-rotate (matrix)
  (list matrix
        (flip matrix)
        (transpose matrix)
        (flip (transpose matrix))))

(defun unrotate-matrices (matrices)
  (destructuring-bind (a b c d) matrices
    (list a
          (flip b)
          (transpose c)
          (transpose (flip d)))))

(defun visible-trees-left->right (row &optional (min-height -1))
  ;; Checks which trees are visible from left to right in a row
  (loop for tree in row collect
    (if (> tree min-height)
        (progn
          (setq min-height tree)
          t)
        nil)))

(defun solve-visible-trees (matrix)
  (reduce-matrices
    (unrotate-matrices
      (mapcar
        (lambda (x) (mapcar #'visible-trees-left->right x))
        (matrix-views-rotate matrix)))))


;;;; Part-2
(defun see-how-many-trees (row)
  (let ((current-height (car row)))
    (min (length (cdr row))
         (1+ (reduce #'+
                     (loop for tree in (cdr row)
                        when (< tree current-height) collect 1
                        while (< tree current-height)))))))

(defun get-from-pos (matrix x y)
  (subseq (car (subseq matrix y)) x))

(defun get-scenic-score (matrix x y)
  (let ((lx (length (car matrix)))
        (ly (length matrix)))
    (destructuring-bind (a b c d) (matrix-views-rotate matrix)
      (reduce #'*
              (mapcar #'see-how-many-trees
                 (list
                   (get-from-pos a x y)
                   (get-from-pos b (1- (- lx x)) y)
                   (get-from-pos c y x)
                   (get-from-pos d (1- (- ly y)) x)))))))

(defun part-1 (matrix)
  (reduce #'+
          (mapcar (lambda (x) (count t x))
                  (solve-visible-trees matrix))))

(defun part-2 (matrix)
  (reduce #'max
          (apply #'mapcar #'max
                 (loop for x from 1 to (1- (length (cdar matrix))) collect
                   (loop for y from 1 to (1- (length (cdr matrix))) collect
                     (get-scenic-score matrix x y))))))

(defvar *input* (mapcar #'str->int-list (load-file "input.txt")))
(format t "Part 1: ~a~%" (part-1 *input*))
(format t "Part 2: ~a~%" (part-2 *input*))
