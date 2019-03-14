#lang racket

; ex 3.1

(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))

; ex 3.2

(define (make-monitored f)
  (define counter 0)
  (define (dispatch m)
    (if (eq? m 'how-many-calls?)
        counter
        (begin (set! counter (+ counter 1))
               (f m))))
  dispatch)

; ex 3.3 & 3.4

(define (make-account balance password)
  (define counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) "Run for your life")
  (define (dispatch p m)
    (cond
      ((> counter 2) (call-the-cops))
      ((not (eq? p password)) (set! counter (+ counter 1))
                              (error "Incorrect password"))
      ((eq? m 'withdraw) (set! counter 0) withdraw)
      ((eq? m 'deposit) (set! counter 0) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

; ex 3.7

(define (make-joint account password new-password)
  (define counter 0)
  (define (call-the-cops) "Run for your life")
  (define (dispatch p m)
    (cond
      ((> counter 2) (call-the-cops))
      ((not (eq? p new-password)) (set! counter (+ counter 1))
                                  (error "Incorrect password"))
      (else (set! counter 0) (account password m))))
  dispatch)

; TODO ex 3.8
