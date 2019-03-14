#lang sicp

; ex 2.54

(define (equal? x y)
  (cond
    [(and (list? x) (not (null? x)) (list? y) (not (null? y)))
     (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))]
    [else (eq? x y)]))

; ex 2.55

; The first quote quotes the second quote.
