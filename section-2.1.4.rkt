#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (make-interval a b) (cons a b))

; ex 2.7

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

; ex 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; ex 2.9

(define a (make-interval 1 2))
(define b (make-interval 3 7))

(= (+ (width a) (width b)) (width (add-interval a b)))  ; true
(= (* (width a) (width b)) (width (mul-interval a b)))  ; false

; ex 2.10

(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0))
      (error "Divided by 0!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; ex 2.12

(define (make-center-percent c p)
  (let ((lower-bound (- c (* c p)))
        (upper-bound (+ c (* c p))))
    (make-interval lower-bound upper-bound)))

(define (percent i)
  (let ((c (center i))
        (lo (lower-bound i))
        (hi (upper-bound i)))
    (/ (- c lo) c)))

; ex 2.13

; The percent of the product of two intervals equals the sum of the percent of
; the two intervals.
