;;; package --- Advent of Code 2022 Day 5

;;; Commentary:
;;; Solving Day 5

;;; Code:
(defun readfile (name)
  "Read file with path NAME."
  (with-temp-buffer
    (insert-file-contents name)
    (buffer-string)))

(defun place-element (stacks elt)
  "Place ELT in the form of (element id) in the right stack in STACKS."
  (pcase-let* ((`(,element,id) elt)
               (stack (gethash id stacks '())))
    (puthash id (append stack (list (substring element 1 2))) stacks)))

(defun compute-position (line)
  "Returns elements and positions given LINE."
  (let* ((parts (seq-partition line 4))
         (parts-with-index (seq-map-indexed (lambda (p i) (list (string-trim p) (+ i 1))) parts)))
    (seq-filter (lambda (e) (not (string-empty-p (car e)))) parts-with-index)))

(defun parse-instruction (line)
  "Given \"move x from y to z\" as LINE, return (x y z)."
  (pcase-let ((`(,"move",x,"from",y,"to",z) (split-string line " ")))
    (mapcar 'string-to-number (list x y z))))

(defun execute-instruction-9001 (insn stacks)
  "Execute INSN given state in STACKS."
  (pcase-let* ((`(,n,from,to) insn)
              (from-stack (gethash from stacks))
              (to-stack (gethash to stacks '()))
              (elts (seq-take from-stack n)))
    (progn
      (puthash to
               (seq-concatenate 'list elts to-stack)
               stacks)
      (puthash from (seq-drop from-stack n) stacks))))

(defun execute-instruction-9000 (insn stacks)
  "Execute INSN given state in STACKS."
  (pcase-let ((`(,n,from,to) insn))
    (dotimes (i n)
      (let ((from-stack (gethash from stacks))
            (to-stack (gethash to stacks '())))
      (progn
        (puthash to
                 (cons (car from-stack) to-stack)
                 stacks)
        (puthash from (cdr from-stack) stacks))))))

(defun solve-day5-puzzle (executor)
  "Solve Day5 puzzles."
  (pcase-let* ((contents (readfile "input"))
         (blocks (split-string contents "\n\n"))
         (`(,rawstackinfo,rawinstructions) blocks)
         (stackinfo (split-string rawstackinfo "\n"))
         (positions (mapcar 'compute-position (seq-take stackinfo (- (length stackinfo) 1))))
         (stacks (make-hash-table))
         (output '())
         (instructions (mapcar 'parse-instruction (split-string rawinstructions "\n"))))
    (progn
      (seq-do (lambda (lst) (seq-do (lambda (elt) (place-element stacks elt)) lst)) positions)
      (princ stacks)
      (seq-do (lambda (insn) (funcall executor insn stacks)) instructions)
      (maphash (lambda (key stack)
                 (setq output (cons (list key (car stack)) output))) stacks))
    (string-join
     (mapcar (lambda (lst) (nth 1 lst))
             (sort output (lambda (lst1 lst2) (< (car lst1) (car lst2))))))))
    
(defun solve-puzzle-1 ()
  "Solve Day5 puzzle 1"
  (solve-day5-puzzle 'execute-instruction-9000))

(defun solve-puzzle-2 ()
  "Solve Day5 puzzle 1"
  (solve-day5-puzzle 'execute-instruction-9001))
