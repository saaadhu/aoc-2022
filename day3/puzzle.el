;;; package --- Advent of Code 2022 Day 3

;;; Commentary:
;;; Solving Day 3

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun elt-priority (ord)
  "Computes priority for one element with ordinal value ORD."
    (cond
     ((> ord 96) (- ord 96))
     ((+ (- ord 64) 26))))

(defun get-priority (line)
  "Compute priority for one LINE."
  (let* ((len (length line))
         (half-len (/ len 2))
         (comp-1 (append (substring line 0 half-len) '()))
         (comp-2 (append (substring line half-len len) '()))
         (elt (seq-intersection comp-1 comp-2)))
    (elt-priority (car elt))))

(defun find-badge (parts)
  "Find badge in PARTS."
  (let ((el0 (append (nth 0 parts) '()))
       (el1 (append (nth 1 parts) '()))
       (el2 (append (nth 2 parts) '())))
    (car (seq-intersection (seq-intersection el0 el1)
                      el2))))


         
(defun solve-puzzle-1 ()
  "Solve Day3 Puzzle1."
  (let* ((contents (readfile "input"))
         (lines (split-string contents "\n")))
         (apply '+ (mapcar 'get-priority lines))))

(defun solve-puzzle-2 ()
  "Solve Day3 Puzzle2."
  (let* ((contents (readfile "input"))
         (lines (split-string contents "\n"))
         (parts (seq-partition lines 3))
         (badges (mapcar 'find-badge parts)))
         (apply '+ (mapcar 'elt-priority badges))))
