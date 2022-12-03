(defvar input)
(defvar tmp)

(defvar lower-case "abcdefghijklmnopqrstuvwxyz")
(defvar upper-case (string-upcase lower-case))

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)

(defun char-to-int (c)
  (char-code (read-from-string (concatenate 'string "\#\\" c)))
)

(defun sym-to-char (sym)
  (coerce sym 'string)
)

(defun is-upper-case (s)
  (equal (string-upcase s) s)
)

(defun diff-chars (c base &optional (offset 1))
  (+ offset (- (char-to-int c) (char-to-int base)))
)

(defun get-char-value (c)
  (if (is-upper-case c)
    (diff-chars c "A" 27)
    (diff-chars c "a" 1)
  )
)

(defun half-str-len (l)
  (/ (length l) 2)
)

(defun split-str-half (s)
  (list (subseq s 0 (half-str-len s)) (subseq s (half-str-len s) (length s)))
)

(defun str-to-list (s)
  (coerce s 'list)
)

(defun str-inter (s1 s2)
  (sym-to-char (intersection (str-to-list s1) (str-to-list s2)))
)

(defun multi-str-inter (str-list)
  (if (equal (length str-list) 2)
    (apply #'str-inter str-list)
    (str-inter (car str-list) (multi-str-inter (cdr str-list)))
  )
)

(defun get-container-value (container)
  (get-char-value (subseq (apply #'str-inter (split-str-half container)) 0 1))
)

(defun part-1 (input)
 (apply #'+ (mapcar #'(lambda (x) (get-container-value x)) input))
)

(defun part-2 (input)
  (apply #'+
    (loop for (a b c) on input by #'cdddr collect
      (get-char-value (subseq (multi-str-inter (list a b c)) 0 1))
    )
  )
)

(setq input (load-file "input.txt"))
(format t "Part 1: ~a~%" (part-1 input))
(format t "Part 2: ~a~%" (part-2 input))
