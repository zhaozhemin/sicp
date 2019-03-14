#lang sicp

(define tolerance 0.00001)

(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (square-fixed-point x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; ex 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; ex 1.36

; The number of steps is lesser with average damping.
(define (a)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2))

; (define (close-enough? x y) (< (abs (- x y)) 0.001))

; ex 1.37.a

(define (cont-frac n d k)
  (define (loop i)
    (if (< i k)
        (/ (n i) (+ (d i) (loop (+ i 1))))
        (- (n i) (d i))))
  (loop 1))

; k = 12

; ex 1.37.b

(define (cont-frac-iter n d k)
  (define (loop k accu)
    (if (< 1 k)
        (loop (- k 1) (+ (d (- k 1)) (/ (n k) accu)))
        (/ (n k) accu)))
  (loop k (d k)))

; ex 1.38

; ex 1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x)))
             (lambda (i) (- (* 2 i) 1))
             k))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; ex 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))

; ex 1.41

(define (double proc)
  (lambda (x) (proc (proc x))))

; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; ex 1.43
(define (repeated f n)
  (define (loop f n accu)
    (if (> n 1)
        (loop f (- n 1) (compose f accu))
        accu))
  (loop f n f))

; ex 1.44

(define (smooth f)
  (let ((dx 0.001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; ex 1.45

(define (nth-root x n)
  (let ([equation (lambda (y) (/ x (expt y (- n 1))))]
        [number (floor (log n 2))])
    (fixed-point ((repeated average-damp number) equation) 1.0)))

; ex 1.46

(define (iterative-improve good-enough? improve)
  (define (loop guess)
    (if (good-enough? guess)
        guess
        (loop (improve guess))))
  loop)
