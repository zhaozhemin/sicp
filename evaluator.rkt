#lang sicp

; ex 4.4

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

; ex 4.13
;
; Only make the bound variable's value in the first frame to be
; '*unassigned*.

(define (unbound? exp) (tagged-list? exp 'make-unbound!))

(define (unbound-var exp) (cadr exp))

(define (eval-make-unbound! exp env)
  (let* ([frame (first-frame env)]
         [pair (scan (unbound-var exp) frame)])
    (if pair
        (set-cdr! pair '*unassigned*)
        (error "Unbound variable: MAKE-UNBOUND!" (unbound-var exp)))))

; ex 4.14
;
; Simply put, if map is primitive, the proc of map will be called in the
; implementation language. However, the proc of map is the representation of
; procedure in the implemented language; in the implementation language, it's
; just a list, which cannot be called.
;
; Take (map (lambda (n) n) '(1 2 3)) as an example. It becomes
;
; 1. (apply map '((procedure ...) (1 2 3)) which becomes
; 2. (map '(procedure ...) '(1 2 3)) which crashes because '(procedure ...) is
;    not callable.

; TODO ex 4.15

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

; ex 4.22
;
; Just add a clause to the cond in the body of analyze.

; TODO ex 4.23

; What does it mean by 'the sequence itself has not been'? Alyssa's version
; seems to work fine.
;
; Regardless of how many expressions the sequence has, Alyssa's version always
; loops through the list and calls each lambda; if there's only one expression,
; textbook's version will just call the lambda returned by the expression.

; TODO ex 4.24

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

; Eval & Apply

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (scan-out-defines (lambda-body exp))
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((for? exp) (eval (for->combination exp) env))
        ((unbound? exp) (eval-make-unbound! exp env))
        ((application? exp)
         (meta-apply (eval (operator exp) env)
                     (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (meta-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else (error
               "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Eval expressions

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (make-assignment var expr)
  (list 'set! var expr))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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
            ; ex 4.5
            (if (eq? (car (cond-actions first)) '=>)
                (make-if (cond-predicate first)
                         (list (cadr (cond-actions first))
                               (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

; Evaluator data structures

; Boolean

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

; Procedure

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; Environment

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

; ex 4.11 & ex 4.12

(define (make-frame variable values)
  (cons '*frame* (map cons variable values)))

(define (frame-variables frame) (map car (cdr frame)))

(define (frame-values frame) (map cdr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (scan var frame)
  (assoc var (cdr frame)))

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

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let* ([frame (first-frame env)]
               [pair (scan var frame)])
          (if pair
              (set-cdr! pair val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ([frame (first-frame env)]
         [pair (scan var frame)])
    (if pair
        (set-cdr! pair val)
        (add-binding-to-frame! var val frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; Primitive procedure

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'null? null?)
        (list 'assoc assoc)
        (list 'display display)
        (list 'map map)
        (list '*unassigned* '*unassigned*)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

; Repl

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (display input)
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define environ the-global-environment)

(driver-loop)
