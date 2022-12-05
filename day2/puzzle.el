;;; package --- Advent of Code 2022 Day 2

;;; Commentary:
;;; Solving Day 2

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun scorer (opponent mine)
  "Returns 0 if OPPONENT won, 3 if draw, and 6 if OPPONENT lost."
  (let* ((o (- (aref opponent 0) 65))
         (m (- (aref mine 0) 88))
         (diff (- o m)))
    (cond
     ((= diff 0) 3)
     ((or (= diff 1) (= diff -2)) 0)
     (6))))

(defun shapescore (choice)
  "Return score for CHOICE."
  (let ((score '(("X" . 1) ("Y" . 2) ("Z" . 3))))
    (assoc-default choice score)))

(defun score (round)
  "Computes score given opponent and my move in ROUND."
  (let* ((parts (split-string round))
         (opponent (nth 0 parts))
         (mine (nth 1 parts))
         (outcome (concat opponent mine)))
         (+ (scorer opponent mine)
            (shapescore mine))))

(defun make-choice (round)
  "Return choice that matches strategy specified in ROUND."
  (let* ((choice '(("A X" . "A Z") ("A Y" . "A X") ("A Z" . "A Y")
                   ("B X" . "B X") ("B Y" . "B Y") ("B Z" . "B Z")
                   ("C X" . "C Y") ("C Y" . "C Z") ("C Z" . "C X"))))
    (assoc-default round choice)))
                   

(defun solve-puzzle-1 ()
  "Solve Day2 Puzzle1."
  (let* (
         
         (contents (readfile "input"))
         (lines (split-string contents "\n")))
         (apply '+ (mapcar 'score lines))))

(defun solve-puzzle-2 ()
  "Solve Day2 Puzzle2."
  (let* ((contents (readfile "input"))
         (lines (split-string contents "\n"))
         (choices (mapcar 'make-choice lines)))
    (apply '+ (mapcar 'score choices))))

