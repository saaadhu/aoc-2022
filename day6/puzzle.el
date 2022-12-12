;;; package --- Advent of Code 2022 Day 6

;;; Commentary:
;;; Solving Day 6

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun is-marker-p (text)
  "Is TEXT a marker?"
  (= (length (seq-uniq text)) (length text)))

(defun find-marker-position (contents marker-len)
  "Finds position of marker given CONTENTS."
  (let ((len (length contents))
        (current 0)
        (found nil))
    (while
        (and (<= current (- len marker-len)) (not found))
      (let ((maybe-marker (substring contents current (+ current marker-len))))
        (progn
          (if (is-marker-p maybe-marker)
              (setq found t))
          (setq current  (+ current 1)))))
    (+ current (- marker-len 1))))

(defun end-marker-position (contents)
  "Find end of marker given CONTENTS."
  (find-marker-position contents 4))

(defun start-marker-position (contents)
  "Find start of marker given CONTENTS."
  (find-marker-position contents 14))

(defun solve-puzzle-1 ()
  "Solve Day 6 puzzle 1"
  (let ((contents (readfile "input")))
    (end-marker-position contents)))
         
(defun solve-puzzle-2 ()
  "Solve Day 6 puzzle 2"
  (let ((contents (readfile "input")))
    (start-marker-position contents)))
