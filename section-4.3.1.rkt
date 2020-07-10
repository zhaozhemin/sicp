#lang sicp

; ex 4.35

(define (an-integer-between lo hi)
  (if (> lo hi)
      (amb)
      (amb lo (an-integer-between (+ lo 1) hi))))

; % ex 4.36

; It'll infinitely search for the third number (because it has no upper bound).

; I was scratching my head thinking how to interleave three streams of amb
; numbers, but not realized that I could find a potential upper bound. Below is
; one way, though not efficient.

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1)))

(define (pythagorean-triples)
  (let* ([k (an-integer-starting-from 1)]
         [i (an-integer-between 1 k)]
         [j (an-integer-between i k)])
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

; TODO ex 4.37
