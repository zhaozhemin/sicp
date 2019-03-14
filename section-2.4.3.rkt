#lang racket

; ex 2.73

; a

; Because numbers and varibles don't have tags.

(define ht (make-hash))

(define (put op type item)
  (hash-set! ht (list op type) item))

(define (get op type)
  (hash-ref ht (list op type) #false))

(define (attach-tag type-tag contents)
  (cond
    ((number? contents) contents)
    (else (cons type-tag contents))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

; b

(define (install-deriv-sum-package)
  (define (tag x)
    (attach-tag '+ x))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (tag (list a1 a2)))))
  (define (addend operands)
    (car operands))
  (define (augend operands)
    (cadr operands))
  (define (deriv-sum operands var)
    (make-sum
      (deriv (addend operands) var)
      (deriv (augend operands) var)))
  (put 'deriv 'make-sum make-sum)
  (put 'deriv '+ deriv-sum))

(define (install-deriv-mul-package)
  (define (tag x)
    (attach-tag '* x))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (tag (list m1 m2)))))
  (define (multiplier p)
    (car p))
  (define (multiplicand p)
    (cadr p))
  (define (deriv-product operands var)
    ((get 'deriv 'make-sum)
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))
  (put 'deriv '* deriv-product))

(install-deriv-sum-package)
(install-deriv-mul-package)

; d

; Swap the position of op and type in put.

; ex 2.74

; a

; ex 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; ex 2.76

; 
