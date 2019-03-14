#lang racket

(define ht (make-hash))

(define (put op type item)
  (hash-set! ht (list op type) item))

(define (get op type)
  (hash-ref ht (list op type) #false))

(define (put-coercion t1 t2 item)
  (hash-set! ht (list t1 t2) item))

(define (get-coercion t1 t2)
  (hash-ref ht (list t1 t2) #false))

; ex 2.78

(define (attach-tag type-tag contents)
  (cond
    [(equal? type-tag 'scheme-number) contents]
    [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond
    [(pair? datum) (car datum)]
    [(number? datum) 'scheme-number]
    [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (square n) (* n n))

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error "No method for these types: APPLY-GENERIC"
;                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

; Integer

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'negate '(scheme-number)
       (lambda (x) (- x)))
  (put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; Rational

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ; (define (make-rat n d)
  ;   (let ((g (gcd n d)))
  ;     (cons (/ n g) (/ d g))))
  ; (define (make-rat n d)
  ;   (cons n d))
  (define (make-rat n d)
    (let ([rat (reduce n d)])
      (cons (car rat) (cadr rat))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put 'zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-real (exact->inexact (/ (numer x) (denom x))))))
  (put 'project '(rational)
       (lambda (x) (round (/ (numer x) (denom x)))))
  (put 'test 'test
       (lambda (x) (test 1)))
  (define (test n) n)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer x) (apply-generic 'numer x))

(define (denom x) (apply-generic 'denom x))

; Real

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real)
       (lambda (x)
         (let ([rat (inexact->exact x)])
           (make-rational (numerator rat) (denominator rat)))))
  (put 'make 'real
       (lambda (x) (tag x))))

(define (make-real x)
  ((get 'make 'real) (exact->inexact x)))

; Complex

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  'done)

(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; ex 2.77

; magnitude -> apply-generic (strip off the 'complex tag) ->
; magnitude -> apply-generic (strip off the 'rectangular tag) ->
; magnitude in install-rectangular-package

; ex 2.79

; If you want to write a install-equ-package procedure, you have to define its
; own internal procedures for numer and denom.

(define (equ? x y)
  (apply-generic 'equ? x y))

; ex 2.80

(define (=zero? x)
  (apply-generic 'zero? x))

; ex 2.81

; a

; Infinite loop

; b

; apply-generic works correctly as is.

; c

(define (apply-generic op . args)
  (let* [(type-tags (map type-tag args))
         (proc (get op type-tags))]
    (if proc
        (apply proc (map contents args))
        ;(if (or (equal? op 'raise) (equal? op 'equ?))
        ;    (apply proc (map contents args))
        ;    (drop (apply proc (map contents args))))
        (let [(type1 (car type-tags))
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args))]
          (cond
            [(equal? type1 type2) (error "No method for these types")]
            [(= (length args) 2)
             (if (>hierarchy type1 type2)
                 (apply-generic op a1 (raise a2))
                 (apply-generic op (raise a1) a2))]
            [else (error "No method for these types" (list op type-tags))])))))

; ex 2.83

(define (raise x)
  (apply-generic 'raise x))

; ex 2.84

(define hierarchy '(complex real rational scheme-number))

; Assuming t1 and t2 are both in hierarchy.

(define (>hierarchy t1 t2)
  (let ([index-t1 (index-of hierarchy t1)]
        [index-t2 (index-of hierarchy t2)])
    (< index-t1 index-t2)))

; ex 2.85

(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (if (equal? (type-tag x) 'scheme-number)
      x
      (let [(lower-rank (project x))]
        (if (equ? (raise lower-rank) x)
            (drop lower-rank)
            x))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list)
    (car term-list))

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist? term-list)
    (null? term-list))

  (define (make-term order coeff)
    (list order coeff))

  (define (negate-term-list term-list)
    (map (lambda (t) (make-term (order t) (negate (coeff t)))) term-list))

  (define (order term)
    (car term))

  (define (coeff term)
    (cadr term))

  (define (add-terms L1 L2)
    (cond
      ((empty-termlist? L1) L2)
      ((empty-termlist? L2) L1)
      (else (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond
                ((> (order t1) (order t2))
                 (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                ((< (order t1) (order t2))
                 (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                (else (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  ; ex 2.91

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let* ([new-c (div (coeff t1) (coeff t2))]
                     [new-o (- (order t1) (order t2))]
                     [new-term (make-term new-o new-c)]
                     [rest-of-result
                       (div-terms (add-terms L1 (negate-term-list (mul-term-by-all-terms new-term L2)))
                                  L2)])
                  (list (cons new-term (car rest-of-result))
                        (cadr rest-of-result)))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (sub-poly p1 p2)  ; ex 2.88
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (negate-term-list (term-list p2))))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let* ([result (div-terms (term-list p1) (term-list p2))]
               [quotient_ (car result)]
               [remainder_ (cadr result)]
               [var (variable p1)])
          (list (make-poly var quotient_)
                (make-poly var remainder_)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (mul-terms-by-int int terms)
    (map (lambda (t) (make-term (order t) (* int (coeff t)))) terms))

  ; ex 2.96 a

  (define (pseudoremainder-terms p q)
    (let* ([t1 (first-term p)]
           [t2 (first-term q)]
           [o1 (order t1)]
           [o2 (order t2)]
           [c (coeff t2)]
           [factor (expt c (+ 1 (- o1 o2)))])
      (cadr (div-terms (mul-terms-by-int factor p) q))))

  ; ex 2.96 b

  (define (gcd-of-coeff term-list)
    (apply gcd (map coeff term-list)))

  (define (lower-coeff-terms gcd_ term-list)
    (map (lambda (t) (make-term (order t) (/ (coeff t) gcd_))) term-list))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (lower-coeff-terms (gcd-of-coeff a) a)
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
        (make-poly (variable a) (gcd-terms (term-list a) (term-list b)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))

  ; ex 2.97 a

  (define (reduce-terms n d)
    (let* ([gcd_ (gcd-terms n d)]
           [gcd-first-term (first-term gcd_)]
           [factor (expt (coeff gcd-first-term)
                         (+ 1 (- (max (order (first-term n))
                                      (order (first-term d)))
                                 (order gcd-first-term))))]
           [factor*n/gcd (car (div-terms (mul-terms-by-int factor n) gcd_))]
           [factor*d/gcd (car (div-terms (mul-terms-by-int factor d) gcd_))]
           [coeff-gcd (gcd-of-coeff (append factor*n/gcd factor*d/gcd))])
      (list (lower-coeff-terms coeff-gcd factor*n/gcd)
            (lower-coeff-terms coeff-gcd factor*d/gcd))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ([reduced-terms (reduce-terms (term-list p1) (term-list p2))])
          (list (make-poly (variable p1) (car reduced-terms))
                (make-poly (variable p2) (cadr reduced-terms))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (let ([result (div-poly p1 p2)])
                             (list (tag (car result)) (tag (cadr result))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'zero? '(polynomial)  ; ex 2.87
       (lambda (p) (andmap (lambda (t) (=zero? (coeff t))) (term-list p))))
  (put 'negate '(polynomial)
       (lambda (p) (make-polynomial (variable p)
                                    (negate-term-list (term-list p)))))
  (put 'greatest-common-divisor '(polynomial polynomial) 
       (lambda (x y) (tag (gcd-poly x y))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (let ([reduced-polys (reduce-poly p1 p2)])
           (list (tag (car reduced-polys))
                 (tag (cadr reduced-polys))))))
  (put 'div 'terms div-terms)
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (negate x)
  (apply-generic 'negate x))

(define p (make-polynomial 'x (list (list 5 (make-polynomial 'y '((2 1) (0 1))))
                                    (list 3 2)
                                    (list 1 1))))
(define q (make-polynomial 'x (list (list 5 (make-polynomial 'y '((3 1) (2 1) (0 1))))
                                    (list 4 1))))
(define a (make-polynomial 'x '((5 1) (0 -1))))
(define b (make-polynomial 'x '((2 1) (0 -1))))
(define c (make-polynomial 'x '((3 1) (2 -2) (0 -4))))
(define d (make-polynomial 'x '((1 1) (0 -3))))

; ex 2.89

(define (install-polynomial-dense-package)
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list)))

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist? term-list)
    (null? term-list))

  (define (make-term order coeff)
    (list order coeff))

  (define (negate-term-list term-list)
    (map (lambda (t) (make-term (order t) (negate (coeff t)))) term-list))

  (define (order term)
    (car term))

  (define (coeff term)
    (cadr term))

  (define (add-terms L1 L2)
    (display L1)
    (display L2)
    (cond
      ((empty-termlist? L1) L2)
      ((empty-termlist? L2) L1)
      (else (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond
                ((> (order t1) (order t2))
                 (adjoin-term
                   (coeff t1) (add-terms (rest-terms L1) L2)))
                ((< (order t1) (order t2))
                 (adjoin-term
                   (coeff t2) (add-terms L1 (rest-terms L2))))
                (else  (adjoin-term
                        (add (coeff t1) (coeff t2))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (tag x)
    (attach-tag 'polynomial-dense x))

  (put 'add '(polynomial-dense polynomial-dense)
       (lambda (x y) (tag (add-poly x y))))
  (put 'make 'polynomial-dense 
       (lambda (x y) (tag (make-poly x y))))
  'done)

; (install-polynomial-dense-package)

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial-dense) var terms))


; ex 2.94

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

; (define p3 (make-polynomial
;              'x '((4 1) (3 -1) (2 -2) (1 2))))
; (define p4 (make-polynomial 'x '((3 1) (1 -1))))

; (define p5 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
; (define p6 (make-polynomial 'x '((2 11) (0 7))))
; (define p7 (make-polynomial 'x '((1 13) (0 5))))
; (define q1 (mul p5 p6))
; (define q2 (mul p5 p7))
; (greatest-common-divisor q1 q2)

; ex 2.97 b

(define (reduce n d)
  (apply-generic 'reduce n d))

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

; TODO (add rf1 rf2) produces correct anwser, however, the sign of coeffs in
; the polynomial is flipped.
