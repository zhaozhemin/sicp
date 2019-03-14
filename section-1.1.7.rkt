#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; ex 1.7

(define (good-enough-v2? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))

(define (sqrt-iter-v2 guess prev-guess x)
  (if (good-enough-v2? guess prev-guess)
      guess
      (sqrt-iter-v2 (improve guess x) guess x)))

(define (sqrt-v2 x)
  (sqrt-iter-v2 1.0 x x))

; ex 1.8

(define (cube x)
  (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-root-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-root-improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
