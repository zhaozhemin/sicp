#lang sicp

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))

    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints))
        ((not (= value newval))
         (error "Contradiction" (list value newval)))
        (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant true false))
        ((eq? request 'value) value)
        ((eq? request 'constraints) constraints)
        ((eq? request 'informant) informant)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (get-constraints connector)
  (connector 'constraints))
(define (get-informant connector)
  (connector 'informant))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define a (make-connector))
(define b (make-connector))
(define s (make-connector))
; (adder a b s)
; (set-value! a 3 'user)
; (get-informant a)
; (get-informant b)
; (get-informant s)

; ex 3.33

(define (average a b c)
  (let ([u (make-connector)]
        [v (make-connector)])
    (constant 2 u)
    (multiplier u c v)
    (adder a b v))
  'ok)

; ex 3.34

; It can calculate sqaure but not sqaure root. When you set b, none of the
; conditions in process-new-value in multiplier will be met because a refers to
; the same object which doesn't have value yet.

; ex 3.35

(define (square n) (* n n))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (display "get-informant b:")
    (display (get-informant b))
    (display "\n")
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

; (define (celsius-fahrenheit-converter c f)
;   (let ((u (make-connector))
;         (v (make-connector))
;         (w (make-connector))
;         (x (make-connector))
;         (y (make-connector)))
;     (multiplier c w u)
;     (multiplier v x u)
;     (adder v y f)
;     (constant 9 w)
;     (constant 5 x)
;     (constant 32 y)
;     'ok))

; ex 3.37

(define (subtractor a1 a2 difference)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! difference
                       (- (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? difference))
           (set-value! a2
                       (- (get-value a1) (get-value difference))
                       me))
          ((and (has-value? a2) (has-value? difference))
           (set-value! a1
                       (+ (get-value difference) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! difference me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect difference me)
  me)

(define (divider m1 m2 quotient)
  (define (process-new-value)
    (cond ((and (has-value? m1) (= (get-value m1) 0))
           (set-value! quotient 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! quotient
                       (/ (get-value m1) (get-value m2))
                       me))
          ((and (has-value? quotient) (has-value? m1))
           (set-value! m2
                       (/ (get-value m1)
                          (get-value quotient))
                       me))
          ((and (has-value? quotient) (has-value? m2))
           (set-value! m1
                       (* (get-value quotient)
                          (get-value m2))
                       me))
          ((and (has-value? m2) (= (get-value m2) 0))
           (error "zero division error"))))
  (define (process-forget-value)
    (forget-value! quotient me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect quotient me)
  me)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ([z (make-connector)])
    (subtractor x y z)
    z))

(define (c* x y)
  (let ([z (make-connector)])
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ([z (make-connector)])
    (divider x y z)
    z))

(define (cv x)
  (let ([z (make-connector)])
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
