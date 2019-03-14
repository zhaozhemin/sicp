#lang sicp

; ex 3.12

; '(b)
; '(b c d)

; ex 3.13

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; '(a b c a b c ...)
; Infinite loop

; ex 3.14
; It reverses the list.

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

; TODO ex 3.16

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define a '(() () ()))
(define b '((1) () ()))
(define c '((1 2) (3 4) ()))

; TODO ex 3.17

; ex 3.18

(define (cycled-list? lst0)
  (define (loop lst)
    (cond
      [(null? lst) #false]
      [(eq? lst0 (cdr lst)) #true]
      [else (loop (cdr lst))]))
  (loop lst0))

; TODO ex 3.19
