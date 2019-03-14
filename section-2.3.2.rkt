#lang racket

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation b e)
  (cond
    [(=number? e 0) 1]
    [(=number? e 1) b]
    [else (list '** b e)]))

; ex 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        [(exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (make-sum (exponent exp) '-1)))
           (deriv (base exp) var))]
        (else
          (error "unknown expression type: DERIV" exp))))

; ex 2.57

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (cond
    [(= (length (cddr s)) 0) 0]
    [(= (length (cddr s)) 1) (caddr s)]
    [(= (length (cddr s)) 2) (list '+ (caddr s) (cadddr s))]
    [else (make-sum (caddr s) (cons '+ (cdddr s)))]))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (cond
    [(= (length (cddr p)) 0) 1]
    [(= (length (cddr p)) 1) (caddr p)]
    [(= (length (cddr p)) 2) (list '* (caddr p) (cadddr p))]
    [else (make-product (caddr p) (cons '* (cdddr p)))]))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; ex 2.58.a

; (define (sum? x)
;   (and (list? x) (eq? (cadr x) '+)))

; (define (addend x)
;   (car x))

; (define (augend x)
;   (caddr x))

; (define (make-sum a b)
;   (cond ((=number? a 0) b)
;         ((=number? b 0) a)
;         ((and (number? a) (number? b))
;          (+ a b))
;         (else (list a '+ b))))
