#lang sicp

; TODO ex 4.32

; ex 4.33
;
; The definition of cons, car, and cdr should be typed into the lazy
; evaluator, not in this file.
;
; It seems that there's a slight difference between how Racket and
; Scheme handle the quoting. Scheme evals the quoting while Racket
; doesn't. For example, typing ''a into the REPL, Scheme produces 'a,
; and Racket produces ''a.

(define (make-list exp)
  (cond [(null? exp) ''()]
        [(pair? (car exp))
         (list 'cons (make-list (car exp)) (make-list (cdr exp)))]
        [else (list 'cons `(quote ,(car exp)) (make-list (cdr exp)))]))

(define (text-of-quotation exp env)
  (if (and (pair? (cadr exp)) (not (eq? (caadr exp) 'quote)))
      (eval (make-list (cadr exp)) env)
      (cadr exp)))

; TODO ex 4.34
