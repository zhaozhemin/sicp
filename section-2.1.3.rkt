#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; ex 2.4

(define (cdr z)
  (z (lambda (p q) q)))

; ex 2.5

; ex 2.6

(define (print x)
  (display "X") (newline))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (+ m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
