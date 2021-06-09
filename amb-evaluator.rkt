#lang sicp

(define apply-in-underlying-scheme apply)

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((or? exp) (analyze (or->if exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ;; ((require? exp) (analyze-require exp))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (or? exp) (tagged-list? exp 'or))

(define (or-expr exp) (cdr exp))

(define (or->if exp)
  (let expand ((exps (or-expr exp)))
    (cond ((null? exps) 'false)
          (else (make-if (first-exp exps)
                         (first-exp exps)
                         (expand (rest-exps exps)))))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (let* ((bindings (let-bindings exp))
         (lamb (make-lambda (map car bindings) (let-body exp)))
         (params (map cadr bindings)))
    (cons lamb params)))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env succeed (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

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

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () (set-variable-value! var old-value env) (fail2)))))
             fail))))

; (define (analyze-assignment exp)
;   (let ((var (assignment-variable exp))
;         (vproc (analyze (assignment-value exp))))
;     (lambda (env) (set-variable-value! var (vproc env) env) 'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) (define-variable! var val env) (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

; (define (analyze-if exp)
;   (let ((pproc (analyze (if-predicate exp)))
;         (cproc (analyze (if-consequent exp)))
;         (aproc (analyze (if-alternative exp))))
;     (lambda (env) (if (true? (pproc env)) (cproc env) (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

; (define (analyze-lambda exp)
;   (let ((vars (lambda-parameters exp))
;         (bproc (analyze-sequence (lambda-body exp))))
;     (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)

  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))

  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

; (define (analyze-sequence exps)

;   (define (sequentially proc1 proc2)
;     (lambda (env) (proc1 env) (proc2 env)))

;   (define (loop first-proc rest-procs)
;     (if (null? rest-procs)
;         first-proc
;         (loop (sequentially first-proc (car rest-procs))
;               (cdr rest-procs))))

;   (let ((procs (map analyze exps)))
;     (if (null? procs)
;         (error "Empty sequence: ANALYZE"))
;     (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs)
     env
     ;; success continuation for this aproc
     (lambda (arg fail2)
       (get-args
         (cdr aprocs)
         env
         ;; success continuation for ;; recursive call to get-args
         (lambda (args fail3)
           (succeed (cons arg args) fail3))
         fail2))
     fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

(define (make-frame variables values) (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

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
        (list 'cadr cadr)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'cons cons)
        (list 'null? null?)
        (list 'map map)
        (list 'not not)
        (list 'list list)
        (list 'modulo modulo)
        (list 'member member)
        (list 'abs abs)
        (list 'memq memq)
        (list 'length length)
        (list 'display display)
        (list 'require require)
        (list '*unassigned* '*unassigned*)
        (list 'assoc assoc)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

; Repl

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

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

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again) (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval
            input
            the-global-environment
            ;; ambeval success
            (lambda (val next-alternative)
              (announce-output output-prompt)
              (user-print val)
              (internal-loop next-alternative))
            ;; ambeval failure
            (lambda ()
              (announce-output ";;; There are no more values of")
              (user-print input)
              (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(driver-loop)

; ex 4.35

(define (an-integer-between lo hi)
  (if (> lo hi)
      (amb)
      (amb lo (an-integer-between (+ lo 1) hi))))

; % ex 4.36

; It'll infinitely search for the third number (because it has no upper bound).

; I was scratching my head thinking how to interleave three streams of amb
; numbers, but not realized that I could find a potential upper bound. Below is
; one way, though not efficient.

(define (pythagorean-triples)
  (let* ([k (an-integer-starting-from 1)]
         [i (an-integer-between 1 k)]
         [j (an-integer-between i k)])
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

; TODO ex 4.37

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; ex 4.38

; 5

; TODO ex 4.39

; ex 4.40

(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)])
    (require (not (= baker 5)))
    (let ([cooper (amb 1 2 3 4 5)])
      (require (not (= cooper 1)))
      (let ([fletcher (amb 1 2 3 4 5)])
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ([miller (amb 1 2 3 4 5)])
          (require (> miller cooper))
          (let ([smith (amb 1 2 3 4 5)])
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

; TODO ex 4.41

; ex 4.42

; Should write a xor function.

(define (liars)
  (let ([betty (amb 1 2 3 4 5)]
        [ethel (amb 1 2 3 4 5)]
        [joan (amb 1 2 3 4 5)]
        [kitty (amb 1 2 3 4 5)]
        [mary (amb 1 2 3 4 5)])
    (require (and (or (and (= kitty 2) (not (= betty 3)))
                      (and (not (= kitty 2)) (= betty 3)))
                  (or (and (= ethel 1) (not (= joan 2)))
                      (and (not (= ethel 1)) (= joan 2)))
                  (or (and (= joan 3) (not (= ethel 5)))
                      (and (not (= joan 3)) (= ethel 5)))
                  (or (and (= kitty 2) (not (= mary 4)))
                      (and (not (= kitty 2)) (= mary 4)))
                  (or (and (= mary 4) (not (= betty 1)))
                      (and (not (= mary 4)) (= betty 1)))))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; % ex 4.43
;
; What's the point of writing functions this way? The interpreter uses amb to
; choose values, yet I have to list all possible values in the penultimate
; require clause. The answer is alreay clear by then.

(define (father)
  (let ([mary 'moore]
        [melissa 'barnacle]
        [lorna (amb 'downing 'hall 'parker)]
        [rosalind (amb 'downing 'hall 'parker)]
        [gabrielle (amb 'downing 'hall 'parker)])
    (require (not (eq? lorna 'moore)))
    (require (not (eq? rosalind 'hall)))
    (require (not (eq? gabrielle 'barnacle)))
    (require (cond [(eq? gabrielle 'downing) (eq? melissa 'parker)]
                   [(eq? gabrielle 'hall) (eq? rosalind 'parker)]
                   [else false]))
    (require (distinct? (list mary melissa lorna rosalind gabrielle)))
    (list (list 'mary mary)
          (list 'melissa melissa)
          (list 'lorna lorna)
          (list 'rosalind rosalind)
          (list 'gabrielle gabrielle))))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; (define (parse-verb-phrase)
;;   (define (maybe-extend verb-phrase)
;;     (amb verb-phrase
;;          (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
;;     (maybe-extend (parse-word verbs)))

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

;; (define (parse-noun-phrase)
;;   (define (maybe-extend noun-phrase)
;;     (amb noun-phrase
;;          (maybe-extend (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))
;;   (maybe-extend (parse-simple-noun-phrase)))

(define (parse-noun-phrase)
  (amb (parse-simple-noun-phrase)
       (list 'noun-phrase
             (parse-noun-phrase)
             (parse-prepositional-phrase))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))




; TODO 4.44


; TODO 4.45
;
; '

; TODO 4.46
; TODO 4.47
; TODO 4.48
; TODO 4.49
