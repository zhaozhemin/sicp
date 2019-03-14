#lang sicp
(#%require racket)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; ex 2.33

(define (map_ p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append_ seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length_ sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; ex 2.35

(define (fringe xs)
  (cond
    ((null? xs) '())
    ((pair? (car xs)) (append (fringe (car xs)) (fringe (cdr xs))))
    (else (cons (car xs) (fringe (cdr xs))))))

(define (count-leaves t)
  (length_ (fringe t)))

; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ex 2.37

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
    (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
          (map (lambda (x) (map (lambda (y) (dot-product x y)) cols)) m)))

; ex 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

; the procedure fold-* received should produce the same result regardless of
; the order of its arguments.

; ex 2.39

(define (reverse-right sequence)
   (accumulate (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (equal? x item)))
          sequence))

; ex 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; ex 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum-equals-s? t s)
  (= (+ (car t) (cadr t) (caddr t)) s))

(define (triple n s)
  (filter (lambda (t) (triple-sum-equals-s? t s)) (unique-triples n)))

; ex 2.42

(define (safe? board)
  (define (check? x y)
    (let ([ax (car x)]
          [dx (cadr x)]
          [ay (car y)]
          [dy (cadr y)])
      (or (= ax ay)
          (= dx dy)
          (= (abs (- ax ay)) (abs (- dx dy))))))
  (not (ormap (lambda (x)
                (ormap (lambda (y) (check? x y)) (remove x board)))
              board)))

(define (queens n)
  (define (place col boards)
    (flatmap (lambda (board)
               (map (lambda (row)
                      (cons (list row col) board))
                    (enumerate-interval 0 (- n 1))))
             boards))
  (define (queen-cols k boards)
    (cond
      [(>= k n) boards]
      [else (queen-cols (+ k 1)
                        (filter safe? (place k boards)))]))
  (queen-cols 0 '(())))
