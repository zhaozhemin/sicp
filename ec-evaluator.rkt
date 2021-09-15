#lang sicp

(#%require "compiler.rkt")
(#%require "environment.rkt")
(#%require "reg-sim-exercise.rkt")

; Repl

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (begin (display object) (newline))))

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))

(define (assemble-exp expression)
  (assemble
   (statements (compile expression 'val 'return '()))
   eceval))

(define (compilation-exp exp)
  (cadr (cadr exp)))

(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-body procedure-body)
        (list 'extend-environment extend-environment)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'rest-operands rest-operands)
        (list 'compound-procedure? compound-procedure?)
        (list 'let? let?)
        (list 'let->combination let->combination)
        (list 'cond? cond?)
        (list 'cond-actions cond-actions)
        (list 'cond-predicate cond-predicate)
        (list 'cond-clauses cond-clauses)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'null? null?)
        (list 'no-more-exps? no-more-exps?)
        (list 'compound-procedure? compound-procedure?)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'list list)
        (list 'lexical-address-lookup lexical-address-lookup)
        (list 'lexical-address-set! lexical-address-set!)
        (list 'false? false?)
        (list 'assemble-exp assemble-exp)
        (list 'compile-and-run? compile-and-run?)
        (list 'compilation-exp compilation-exp)
        (list 'display display)
        (list '+ +)
        (list 'cons cons)))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '((assign compapp (label compound-apply))
     (branch (label external-entry))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op compile-and-run?) (reg exp))
     (branch (label compile-and-run))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ; ex 5.23
     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ; ex 5.24
     ev-cond
     (save continue)
     (assign unev (op cond-clauses) (reg exp))

     ev-cond-clauses
     (test (op null?) (reg unev))
     (branch (label ev-cond-end))
     (goto (label ev-cond-clause))

     ev-cond-clause
     (assign exp (op first-exp) (reg unev))
     (test (op cond-else-clause?) (reg exp))
     (branch (label ev-cond-else))
     (save exp)
     (save unev)
     (assign exp (op cond-predicate) (reg exp))
     (assign continue (label after-ev-cond-clause))
     (goto (label eval-dispatch))

     ev-cond-else
     (assign unev (op cond-actions) (reg exp))
     (goto (label ev-sequence))

     after-ev-cond-clause
     (restore unev)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-cond-true))
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-cond-clauses))

     ev-cond-end
     (restore continue)
     (goto (reg continue))

     ev-cond-true
     (assign unev (op cond-actions) (reg exp))
     (goto (label ev-sequence))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)             ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))    ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ; Without tail recursion opimization
     ;; ev-sequence
     ;; (test (op no-more-exps?) (reg unev))
     ;; (branch (label ev-sequence-end))
     ;; (assign exp (op first-exp) (reg unev))
     ;; (save unev)
     ;; (save env)
     ;; (assign continue (label ev-sequence-continue))
     ;; (goto (label eval-dispatch))

     ;; ev-sequence-continue
     ;; (restore env)
     ;; (restore unev)
     ;; (assign unev (op rest-exps) (reg unev))
     ;; (goto (label ev-sequence))

     ;; ev-sequence-end
     ;; (restore continue)
     ;; (goto (reg continue))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)   ; save expression (if expression itself) for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     ; evaluate the predicate:
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)   ; save variable for later
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     ; evaluate the assignment value:
     (goto (label eval-dispatch))

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)   ; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     ; evaluate the definition value:
     (goto (label eval-dispatch))

     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))

     compile-and-run
     (assign exp (op compilation-exp) (reg exp))
     (assign val (op assemble-exp) (reg exp))
     (goto (label external-entry))

     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     unknown-expression-type
     (assign
      val
      (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     ; clean up stack (from apply-dispatch):
     (restore continue)
     (assign
      val
      (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop)))))

; TODO ex 5.25

; ex 5.26

; 1. 10
; 2. 29 + 35n


; ex 5.27

;; |           | maximum depth | number of pushes  |
;; | recursive | 3 + 5n        | 16 + 32 * (n - 1) |
;; | iterative | 10            | 29 + 35n          |

; ex 5.29

; 1. 13 + 5 * (n - 2) for n >= 2
; 2. TODO

; ex 5.30

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (compile-and-go expression)
  (let ((instructions
         (assemble
          (statements (compile expression 'val 'return '()))
          eceval)))
    ;; (display instructions)
    ;; (display (statements (compile expression 'val 'return '())))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
