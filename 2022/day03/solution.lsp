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

(defun diff-chars (c base &optional (offset 1))
  (+ offset (- (char-to-int c) (char-to-int base)))
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

(defun str-intersection (s1 s2)
  (sym-to-char (subseq (intersection (str-to-list s1) (str-to-list s2)) 0 1))
)

(defun is-upper-case (s)
  (equal (string-upcase s) s)
)

(defun get-container-value (container)
  (progn
    (setq tmp (apply #'str-intersection (split-str-half container)))
    (if (is-upper-case tmp)
      (diff-chars tmp "A" 27)
      (diff-chars tmp "a" 1)
    )
  )
)

(defun part-1 (input)
 (apply #'+ (mapcar #'(lambda (x) (get-container-value x)) input))
)

(setq input (load-file "input.txt"))
(format t "Part 1: ~a~%" (part-1 input))
