#lang sicp

; ex 1.11

(define (f-rec n)
  (cond
    [(< n 3) n]
    [else (+ (f-rec (- n 1))
             (* 2 (f-rec (- n 2)))
             (* 3 (f-rec (- n 3))))]))

(define (f-iter n)
  (define (loop a b c n)
    (cond
      [(= n 0) c]
      [(= n 1) b]
      [(= n 2) a]
      [else (loop (+ a (* 2 b) (* c 3)) a b (dec n))]))
  (loop 2 1 0 n))

; ex 1.12

(define (pascal-triangle row col)
  (cond
    [(or (= row col) (= col 1)) 1]
    [else (+ (pascal-triangle (- row 1) (- col 1))
             (pascal-triangle (- row 1) col))]))
