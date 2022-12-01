;;; package --- Advent of Code 2022 Day 1

;;; Commentary:
;;; Solving Day 1

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun get-calories (seq)
  "Sum up SEQ for a single elf."
  (let* ((calories_str (split-string seq))
    (calories (mapcar 'string-to-number calories_str)))
    (-sum calories)))

(defun solve-puzzle-1 ()
  "Solve Day1 Puzzle1."
  (let* ((contents (readfile "input"))
         (elves (split-string contents "\n\n"))
         (calories (mapcar 'get-calories elves)))
    (apply 'max calories)))

(defun solve-puzzle-2 ()
  "Solve Day1 Puzzle2."
  (let* ((contents (readfile "input"))
         (elves (split-string contents "\n\n"))
         (calories (mapcar 'get-calories elves))
         (sorted-calories (sort calories '>)))
    (+
     (nth 0 sorted-calories)
     (nth 1 sorted-calories)
     (nth 2 sorted-calories))))


