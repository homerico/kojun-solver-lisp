(defpackage kojun
  (:use :cl :iterate))

(in-package :kojun)

;;; Retorna o bloco n do puzzle
(defun area-values (n l)
  (iter (for i from 0 to (1- (* (tamanho) (tamanho))))
      (if (= (area-of i) n)
          (collect (nth i l)))))

(defun same-area (i j)
  (equal (area-of i) (area-of j)))

;;; Retorna a coluna n do puzzle
(defun sd-column (i j l)
    (iter (for k from 0 to (1- (tamanho)))
        (if (and (>= i k) (same-area i k) (not (equal '_ (nth (+ (* k (tamanho)) j) l))))
              (iter (for n from (nth (+ (* k (tamanho)) j) l) to (area-length (area-of (+ (* i (tamanho)) j)) l)) (collect n))
            (when (and (< i k) (same-area i k) (not (equal '_ (nth (+ (* k (tamanho)) j) l))))
              (iter (for n from 1 to (nth (+ (* k (tamanho)) j) l)) (collect n))))))

;;; Retorna as celulas verticalmente e horizontalmente adjacentes à celula na linha i e coluna j
(defun adjacent-values (i j l)
  (iter outer (for k from (1- i) to (1+ i))
        (iter (for m from (1- j) to (1+ j))
            (when (and (>= k 0) (>= m 0) (< k (tamanho)) (< m (tamanho)) (xor (= k i) (= m j)))
                (in outer (collect (nth (+ (* k (tamanho)) m) l)))))))

;;; Verifica se o puzzle é válido
(defun valid-p (rcb)
  (let ((digits (remove '_ rcb)))
    (equal digits (remove-duplicates digits))))

;;; Retorna a coluna do espaço i
(defun column-of (i)
  (rem i (tamanho)))

;;; Retorna a linha do espaço i
(defun row-of (i)
  (floor (/ i (tamanho))))

;;; Retorna o bloco do espaço i
(defun area-of (i)
  (nth i (area)))

(defun area-length (n l)
  (length (area-values n l)))

(defun all-values-possible-without-constraints (i p)
  (iter (for var from 1 to (area-length (area-of i) p)) (collect var)))

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

;;; Retorna as possibilidades para o espaço i do puzzle
(defun possible-values (i p)
  (if (integerp (nth i p)) nil                                              ;; Se o espaço já estiver preenchido, retorna nil
      (let* ((b (area-values (area-of i) p))                                  ;; Senão, retorna a diferença entre o conjunto
             (a (adjacent-values (row-of i) (column-of i) p))
             (c (sd-column (row-of i) (column-of i) p))
             (excluded (remove '_ (remove-duplicates (append b a c)))))
        (set-difference (all-values-possible-without-constraints i p) excluded))))

;;; Verifica se o puzzle está resolvido
(defun solved-p (p)
  (if (member '_ p) nil
      (iter (for i from 0 to (1- (* (tamanho) (tamanho))))
        (when (not (and (valid-p (area-values (area-of i) p))
                        (valid-p (sd-column (row-of i) (column-of i) p))
                        (valid-p (adjacent-values (row-of i) (column-of i) p)))) (return nil))
        (finally (return t)))))

;;; find the index of the space in the puzzle with the fewest possible values
(defun best-first (p)
  (iter (for i from 0 to (1- (* (tamanho) (tamanho))))
    (finding i minimizing (let ((l (length (possible-values i p))))         ;; find the index of the space with the fewest possible values
                            (if (zerop l) (1+ (tamanho)) l)))))             ;; ensure filled spaces aren't returned

;;; Substitui o valor na posição n da lista por val
(defun replace-nth (list n val)
    (case n                                                                 ;; Se n for 0, substitui o primeiro elemento
      (0 (cons val (cdr list)))                                             ;; Se não, substitui o elemento na posição n
      (t (nconc (subseq list 0 n) (list val) (nthcdr (1+ n) list)))))       ;; e concatena com o resto da lista

;;; Resolve o puzzle
(defun kojun (p)
  (print-result-spaced p)
  (if (solved-p p) p
      (let ((best (best-first p)))                                          ;; Pega index do espaço com menos possibilidades
        (iter (for s in (possible-values best p))                           ;; Para cada possibilidade
          (let ((solution (kojun (replace-nth p best s))))                  ;; Tenta resolver o puzzle com a possibilidade
            (when solution (return solution)))))))                          ;; Se a solução for encontrada, retorna

;;; Imprime o resultado
(defun print-result-spaced (p)
  (iter (for i from 0 to (1- (* (tamanho) (tamanho))))
    (if (zerop (rem i (tamanho))) (format t "~%"))
    (format t "~a " (nth i p)))
  (format t "~%"))

;;;; Tamanho do puzzle
;(defun tamanho () 8)
;
;;;; Exemplo Nr 4
;(defun table () '(2 5 0 _   _ _ _ _
;                  _ _ 6 _   _ _ _ _
;                  _ _ 5 _   5 2 _ _
;                  _ _ _ 2   _ _ _ _
;
;                  _ _ 1 _   4 _ _ _
;                  3 _ 2 _   _ 4 _ _
;                  _ _ _ 6   _ _ _ _
;                  _ _ _ _   4 _ 3 2 ))
;
;(defun area ()    '( 1  2  2  2    2  3  4  4
;                     1  1  6  2    3  3  5  5
;                     9  8  6  7    3  3 11 11
;                     9 10  6  6    6  3 11 11
;
;                     9 10  6 12   12 12 11 14
;                    16 10 13 12   13 13 14 14
;                    16 16 13 13   13 15 15 14
;                    16 16 17 17   15 15 15 14 ))

;;; Tamanho do puzzle
(defun tamanho () 6)

;;; Exemplo Nr 2
(defun table () '(_ _ 4   _ 2 _
                  _ _ 3   _ _ _
                  1 4 _   4 _ _

                  _ 5 _   _ _ 2
                  _ _ _   _ 3 _
                  6 2 _   2 _ 5 ))

(defun area ()  '(1 2 2   2 3 4
                  1 5 2   3 3 3
                  1 1 6   3 7 7

                  8 9 6   10 10 7
                  8 9 9   11 11 7
                  9 9 9   11 11 11 ))

(kojun (table))
