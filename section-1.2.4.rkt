#lang sicp

; ex 1.16

(define (square n)
  (* n n))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond
    [(= n 0) a]
    [(even? n) (fast-expt-iter (square b) (/ n 2) a)]
    [else (fast-expt-iter b (- n 1) (* a b))]))

; ex 1.17

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-mul a b)
  (cond
    [(= b 0) 0]
    [(= b 1) a]
    [(even? b) (fast-mul (double a) (halve b))]
    [else (+ a (fast-mul a (- b 1)))]))

; ex 1.18

(define (fast-mul-iter a b)
  (define (loop a b accu)
    (cond
      [(= b 0) accu]
      [(even? b) (loop (double a) (halve b) accu)]
      [else (loop a (- b 1) (+ accu a))]))
  (loop a b 0))
