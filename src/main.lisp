(defpackage sudoku
  (:use :cl :iterate))

(in-package :sudoku)

(defun sd-block (n l)
  (let ((start (nth n '(0 3 6 27 30 33 54 57 60)))) ; lookup is faster
    (iter (for b on (nthcdr start l) by #'(lambda (p) (nthcdr 9 p)))
      (for line from 1 to 3)
      (appending (subseq b 0 3)))))

(defun sd-column (n l)
  (iter (for c in (nthcdr n l) by #'(lambda (p) (nthcdr 9 p)))
        (collecting c)))

(defun sd-row (n l)
  (subseq (nthcdr (* n 9) l) 0 9))

(defun valid-p (rcb)
  (let ((digits (remove '_ rcb)))
    (equal digits (remove-duplicates digits))))

(defun column-of (i)
  (rem i 9))

(defun row-of (i)
  (floor (/ i 9)))

(defun block-of (i)
  (let ((column (column-of i))
        (row (row-of i)))
    (+ (floor column 3) (* 3 (floor row 3)))))

(defun possible-values (i p)
  (if (integerp (nth i p)) nil
      (let* ((b (sd-block (block-of i) p))
             (r (sd-row (row-of i) p))
             (c (sd-column (column-of i) p))
             (excluded (remove '_ (remove-duplicates (append b r c)))))
        (set-difference '(1 2 3 4 5 6 7 8 9) excluded))))

 ;;; find the index of the space in the puzzle with the fewest possible values
(defun best-first (p)
  (iter (for i from 0 to 80)
    (finding i minimizing (let ((l (length (possible-values i p))))
                            (if (zerop l) 10 l))))) ;; ensure filled spaces aren't returned

 ;;; Verifica se o puzzle está resolvido
(defun solved-p (p)
  (if (member '_ p) nil
      (iter (for i from 0 to 8)
        (when (not (and (valid-p (sd-block i p))
                        (valid-p (sd-column i p))
                        (valid-p (sd-row i p)))) (return nil))
        (finally (return t)))))

;;; economize a bit on consing
(defun replace-nth (list n val)
    (case n
      (0 (cons val (cdr list)))
      (t (nconc (subseq list 0 n) (list val) (nthcdr (1+ n) list)))))

(defun sudoku (p)
  (if (solved-p p) p
      (let ((best (best-first p)))
        (iter (for s in (possible-values best p))
          (let ((solution (sudoku (replace-nth p best s))))
            (when solution (return solution)))))))
            

(write-line (sudoku '(_ _ 8   _ _ _   6 _ _
          _ 4 _   9 _ 2   _ 5 _
          _ _ _   6 4 8   _ _ _

          _ 3 9   _ 2 _   1 7 _
          _ 1 _   _ _ _   _ 3 _
          _ 8 5   _ 1 _   2 6 _

          _ _ _   2 8 7   _ _ _
          _ 6 _   1 _ 4   _ 8 _
          _ _ 2   _ _ _   5 _ _)))

;(3 9 8 | 5 7 1 | 6 2 4 |
; 6 4 1 | 9 3 2 | 8 5 7 |
; 5 2 7 | 6 4 8 | 9 1 3 |

; 4 3 9 | 8 2 6 | 1 7 5 |
; 2 1 6 | 7 9 5 | 4 3 8 |
; 7 8 5 | 4 1 3 | 2 6 9 |

; 1 5 4 | 2 8 7 | 3 9 6 |
; 9 6 3 | 1 5 4 | 7 8 2 |
; 8 7 2 | 3 6 9 | 5 4 1)