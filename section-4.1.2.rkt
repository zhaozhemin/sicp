#lang sicp

;; TODO ex 4.2

;; TODO ex 4.3

;; ex 4.4

(define (and? exp) (tagged-list? exp 'and))

(define (and-conditions exp) (cdr exp))

(define (eval-and exps env)

  (define (loop exps)
    (cond ((null? exps) true)
          ((last-exp? exps) (eval (first-exp exps) env))
          ((not (true? (eval (first-exp exps) env))) false)
          (else (loop (rest-exps exps)))))

  (loop (and-conditions exps)))

(define (or? exp) (tagged-list? exp 'or))

(define (or-conditions exp) (cdr exp))

(define (eval-or exps env)

  (define (loop exps)
    (cond [(null? exps) false]
          [(last-exp? exps) (eval (first-exp exps) env)]
          [(true? (eval (first-exp exps) env)) true]
          [else (loop (rest-exps exps))]))

  (loop (or-conditions exps)))

(define (and->if exps)
  (let expand ([clauses (and-conditions exps)])
    (cond [(null? clauses) 'true]
          [(last-exp? clauses) (first-exp clauses)]
          [else (make-if (first-exp clauses)
                         (expand (rest-exps clauses))
                         'false)])))

(define (or->if exp)
  (let expand ((clauses (or-conditions exp)))
    (cond ((null? clauses) 'false)
          (else (make-if (first-exp clauses)
                         (first-exp clauses)
                         (expand (rest-exps clauses)))))))

;; ex 4.5

(define (expand-clauses clauses)
  (if (null? clauses)
      'false  ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (eq? (car (cond-actions first)) '=>)
                (make-if (cond-predicate first)
                         (list (cadr (cond-actions first))
                               (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

; ex 4.6 & ex 4.8

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (symbol? (cadr exp))
      (cdddr exp)
      (cddr exp)))

;; let is just lambda; to implement named lambda, I just assign the
;; original lambda to a variable and call that variable. And I wrap
;; this in another lambda to prevent the variable from leaking into
;; outer namespace.
(define (let->combination exp)
  (let* ((bindings (let-bindings exp))
         (lamb (make-lambda (map car bindings) (let-body exp)))
         (params (map cadr bindings)))
    (if (symbol? (cadr exp))
        (let* ((body `((define ,(cadr exp) ,lamb)
                       ,(append (list (cadr exp)) params)))
               (wrapper (make-lambda '() (list (make-begin body)))))
          (cons wrapper '()))
        (cons lamb params))))

; ex 4.7

; bindings is a list of pairs, e.g. '((a 1) (b 2))
; body is a list of list, e.g. '((+ a b))
; It's sufficient to use the derived form.
(define (make-let bindings body)
  (append (list 'let bindings) body))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (let loop ((bindings (let-bindings exp)))
    (if (null? bindings)
        (sequence->exp (let-body exp))
        (make-let (list (car bindings))
                  (list (loop (cdr bindings)))))))

; ex 4.9

(define (for? exp) (tagged-list? exp 'for))

(define (for-condition exp) (cadr exp))

(define (for-body exp) (cddr exp))

(define (for->combination exp)
  (let* ([conditions (for-condition exp)]
         [id (caar conditions)]
         [seq (cadar conditions)]
         [body (for-body exp)]
         [lamb `(lambda (,id) ,(make-begin body))])
    `(let iter ((seq ,seq))
       (cond ((null? seq) 'done)
             (else (,lamb (car seq))
                   (iter (cdr seq)))))))

; TODO ex 4.10
