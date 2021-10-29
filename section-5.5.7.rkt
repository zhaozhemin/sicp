#lang sicp

;; ex 5.45

;; |             | maximum depth | number of pushes  |
;; | interpreted | 3 + 5n        | 16 + 32 * (n - 1) |
;; | compiled    | 3n - 1        | 1 + 6n            |


;; TODO ex 5.46

;; ex 5.47

(define (compile-compound-appl target linkage compile-env)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            ;; Save continue because ev-sequence-last-exp restores continue.
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? target 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc)
            all-regs
            `((assign continue (label ,proc-return))
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage,target not val: COMPILE" target))))

(define (compile-procedure-call target linkage compile-env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc)
        '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))

       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage compile-env))

        (parallel-instruction-sequences
         (append-instruction-sequences
          compound-branch
          (compile-compound-appl target compiled-linkage compile-env))

         (append-instruction-sequences
          primitive-branch
          (end-with-linkage
           linkage
           (make-instruction-sequence
            '(proc argl)
            (list target)
            `((assign
               ,target
               (op apply-primitive-procedure)
               (reg proc)
               (reg argl))))))))

       after-call))))

;; ex 5.48

;; Put these three into the machine's operation table, and test for
;; compile-and-run in eval-dispatch.

(define (assemble-exp expression)
  (assemble
   (statements (compile expression 'val 'return '()))
   eceval))

;; The second cadr extracts the expression of compile-and-run, which,
;; in the interpreter's view, is double quoted; the first cadr then
;; extracts the content of the quotation.
(define (compilation-exp exp)
  (cadr (cadr exp)))

(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

;; Lable & instructions

;; compile-and-run
;; (assign exp (op compilation-exp) (reg exp))
;; (assign val (op assemble-exp) (reg exp))
;; (goto (label external-entry))

;; TODO ex 5.49
;; TODO ex 5.50
;; TODO ex 5.51
;; TODO ex 5.52
