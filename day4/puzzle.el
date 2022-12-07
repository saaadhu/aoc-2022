;;; package --- Advent of Code 2022 Day 4

;;; Commentary:
;;; Solving Day 4

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun breakout-ranges (line)
  "Breakout LINE in \"a-b,x-y\" form into (a b x y)."
  (pcase-let* ((`(, lst0 ,lst1) (split-string line ","))
                (`(,a,b) (mapcar 'string-to-number (split-string lst0 "-")))
                (`(,x,y) (mapcar 'string-to-number (split-string lst1 "-"))))
    (list  a b x y)))

(defun overlap-p (lst)
  "For ((a b) (x y)) in LST, does one range overlap another?"
  (pcase-let ((`(,a,b,x,y) (breakout-ranges lst)))
    (or (and (>= a x) (<= a y))
        (and (>= x a) (<= x b)))))
        

(defun fully-contained-p (lst)
  "For ((a b) (x y)) in LST, is one range fully contained in another?"
  (pcase-let ((`(,a,b,x,y) (breakout-ranges lst)))
    (or (and (>= a x) (<= b y))
        (and (>= x a) (<= y b)))))

(defun solve-puzzle-1 ()
  "Solve Day4 Puzzle1."
  (let* ((contents (readfile "input"))
         (lines (split-string contents "\n")))
    (seq-count 'fully-contained-p lines)))

(defun solve-puzzle-2 ()
  "Solve Day4 Puzzle2."
  (let* ((contents (readfile "input"))
         (lines (split-string contents "\n")))
    (seq-count 'overlap-p lines)))
