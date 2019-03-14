#lang sicp

; ex 2.17

; Can't handle empty list
(define (last-pair xs)
  (cond
    [(null? (cdr xs)) (car xs)]
    [else (last-pair (cdr xs))]))

; ex 2.18

(define (reverse xs)
  (let loop ([ys xs]
             [accu '()])
    (if (null? ys)
        accu
        (loop (cdr ys) (cons (car ys) accu)))))

; ex 2.19

(define us-coins (list 1 25 50 5 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

; Order doesn't matter.

; ex 2.20

(define (same-parity x . xs)
  (define (loop x xs)
    (cond
      ((null? xs) '())
      ((= (remainder (+ x (car xs)) 2) 0) (cons (car xs) (loop x (cdr xs))))
      (else (loop x (cdr xs)))))
  (cons x (loop x xs)))

; ex 2.21

(define (square n) (* n n))

(define (square-list-v1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-v1 (cdr items)))))

(define (square-list-v2 items)
  (map square items))

; ex 2.23

(define (for-each proc xs)
  (cond
    ((null? xs) '())
    (else (proc (car xs)) (for-each proc (cdr xs)))))
