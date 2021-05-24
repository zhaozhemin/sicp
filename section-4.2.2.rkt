#lang sicp

; ex 4.25
;
; It causes infinite loop in applicative-order, as the second argument of
; unless will call factorial indefinitely. Normal-order will work.

; 4.26

; If you want to pass unless around, it has to be a procedure. But a
; proper situation yet eludes me.

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-condition exp) (cadr exp))

(define (unless-usual-value exp) (caddr exp))

(define (unless-exceptional-value exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

; ex 4.27
;
; 1 : (id 10) is not evaluated; w is a thunk
; 10 : (id 10) returns 10
; 2 : Looking up the value of w evaluates w; add another 1 to count

; ex 4.28
;
; It seems to me that the only way for eval to produce a thunk is by making
; a procedure return one of its arguments.
;
; ((if true + *) 2 3) works fine in both versions.
;
; (define (f a b) (if true a b))
; ((f + *) 2 3)
; will cause an error if you use eval to evaluate the operator.

; ex 4.29
;
; A procedure which uses its arguments in multiple places.
;
; With memoization
; 100
; 1
;
; Without memoization
; 100
; 2

; ex 4.30
;
; a
;
; Because (display x) will evaluate x.
;
; b
;
; Original:
;
; (p1 1) -> '(1 2)
; (p2 1) -> 1 (Because e in p is not evaluated)
;
; Changed:
;
; (p1 1) -> '(1 2)
; (p2 1) -> '(1 2)
;
; c
;
; If the object is other than a thunk, force-it will return the object itself.


; TODO ex 4.31
