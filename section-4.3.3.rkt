#lang sicp

; ex 4.50

(define (ramb? exp) (tagged-list? exp 'ramb))

(define (pop xs)
  (define n (random (length xs)))
  (let loop ([i 0]
             [accu '()]
             [tail xs])
    (cond
      [(= i n)
       (cons (car tail) (append accu (cdr tail)))]
      [else (loop (+ i 1) (append accu (list (car tail))) (cdr tail))])))

(define (analyze-ramb exp)
 (let ([cprocs (map analyze (amb-choices exp))])
   (lambda (env succeed fail)
     (define (try-next choices)
       (if (null? choices)
           (fail)
           (let* ([p (pop choices)]
                  [choice (car p)]
                  [rest (cdr p)])
             (choice env succeed (lambda () (try-next rest))))))
     (try-next cprocs))))

; ex 4.51

; The count would have been 1 if set! had been used.

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               ; I was wondering why the following line is needed. In case of
               ; this ex, succeed is a procedure in which the interpreter calls
               ; the analyzed lambda of next line. fail2 is a procedure in
               ; which the interpreter chooses another value for y. If omitted,
               ; the program just stops.
               (succeed 'ok fail2))
             fail))))

; ex 4.52

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (val fail2) (succeed val fail2))
             (lambda () (cproc env
                               (lambda (val fail3) (succeed val fail3))
                               fail))))))

; ex 4.53

; All pairs of prime sum.

; ex 4.54

(define (require? exp) (tagged-list? exp 'require))

(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ([pproc (analyze (require-predicate exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
