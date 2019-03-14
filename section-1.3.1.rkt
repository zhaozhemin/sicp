#lang sicp

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

; ex 1.29

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (helper i) (+ a (* i h)))
  (define (term i)
    (cond
      [(or (= i 0) (= i n)) (f (helper i))]
      [(odd? i) (* 4 (f (helper i)))]
      [(even? i) (* 2 (f (helper i)))]))
  (* (/ h 3)
     (sum term 0 inc n)))

; ex 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; ex 1.31.a

(define (product term a next b)
  (cond
    ((or (= a 0) (= b 0)) 0)
    ((> a b) 1)
    (else (* (term a) (product term (next a) next b)))))

(define (factorial n)
  (cond
    ((= n 0) 1)
    (else (product identity 1 inc n))))

(define (approximation-pi)
  (define (term i)
    (* (/ (* 2 i) (- (* 2 i) 1))
       (/ (* 2 i) (+ (* 2 i) 1))))
  (product term 1 inc 100))

; ex 1.31.b

(define (product-iter term a next b)
  (define (product/a a accu)
    (cond
      [(or (= a 0) (= b 0)) 0]
      [(> a b) accu]
      [else (product/a (next a) (* (term a) accu))]))
  (product/a a 1))

; ex 1.32.a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate combiner
                  (combiner (term a) null-value)
                  term
                  (next a)
                  next
                  b)))

(define (sum/a term a next b)
  (accumulate + 0 term a next b))

(define (product/a term a next b)
  (accumulate * 1 term a next b))

; ex 1.32.b

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a accu)
    (if (> a b)
        accu
        (iter (next a) (combiner (term a) accu))))
  (iter a null-value))

; ex 1.33

(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (accumulate combiner
                  (if (predicate a) (combiner (term a) null-value) null-value)
                  term
                  (next a)
                  next b)))
