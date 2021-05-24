#lang sicp

; ex 4.16
;
; Install in make-procedure. Otherwisw it has to be transformed everytime it's
; called.

(define (scan-out-defines body)

  (define (separate body def expr)
    (cond ((null? body) (cons def expr))
          ((definition? (car body))
           (separate (cdr body) (append def (list (car body))) expr))
          (else (separate (cdr body) def (append expr (list (car body)))))))

  (let* ((p (separate body '() '()))
         (def (car p))
         (expr (cdr p)))
    ; It's crucial to return body as is if it contains no definition instead of
    ; transforming it (let () <body>), or it will cause infinite loop.
    (if (null? def)
        body
        (list
         (make-let
          (map (lambda (d) (list (definition-variable d) '*unassigned*)) def)
          (append
           (map
            (lambda (d)
              (make-assignment (definition-variable d) (definition-value d)))
            def)
           expr))))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ([frame (first-frame env)]
               [pair (scan var frame)])
          (if pair
              (if (eq? (cdr pair) '*unassigned*)
                  (error "Unbound variable" var)
                  (cdr pair))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

; ex 4.17
;
; The body of a lambda expression is transformed into a let expression, which
; is transformed into another lambda expression, which, when called, creates a
; new frame.
;
; Maybe treat let as a special form instead of transforming it into lambda.

; ex 4.18
;
; I don't think the implementation in the text and the one in the ex
; have much difference. They are both going to work or not to work. In
; this case, I think neither of them works because stream-map will
; evaluate y, when y is still '*unassigned*.

; TODO ex 4.19
;
; Huh, I never thought that Scheme's (also Racket's) internal
; definition is actually bound simultaneously. What are the advantages
; over sequential bingind? And what other languages have this feature?

; ex 4.20
;
; If you want to define variables that are based on other variables,
; let won't work.

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (make-let
   (map (lambda (b) (list (car b) '*unassigned*)) (let-bindings exp))
   (append
    (map (lambda (b) (make-assignment (car b) (cadr b))) (let-bindings exp))
    (let-body exp))))

; ex 4.21
;
; What the hell is this...

(define (fib m)
  ((lambda (n)
   ((lambda (f) (f f n))
    (lambda (g k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (g g (- k 1))
                     (g g (- k 2))))))))
 m))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))
