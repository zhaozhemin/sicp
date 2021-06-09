#lang sicp

(#%require "environment.rkt")

(define (compile exp target linkage compile-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-env))
        ((quoted? exp)
         (compile-quoted exp target linkage compile-env))
        ((variable? exp)
         (compile-variable exp target linkage compile-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-env))
        ((if? exp)
         (compile-if exp target linkage compile-env))
        ((lambda? exp)
         (compile-lambda exp target linkage compile-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage compile-env))
        ((cond? exp)
         (compile (cond->if exp) target linkage compile-env))
        ;; ((+? exp)
        ;;  (compile-plus exp target linkage))
        ((application? exp)
         (compile-application exp target linkage compile-env))
        (else (error "Unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else (make-instruction-sequence
               '()
               '()
               `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving
   '(continue)
   instruction-sequence
   (compile-linkage linkage)))

;; (define (spread-arguments operands)

;;   (define (next-target target)
;;     (cond ((eq? target 'arg1) 'arg2)
;;           ((eq? target 'arg2) 'arg1)))

;;   (define (spread ops target)
;;     (if (null? ops)
;;         (empty-instruction-sequence)
;;         (preserving
;;          `(,target env continue)
;;          (compile (car ops) target 'next)
;;          (spread (cdr ops) (next-target target)))))

;;   (spread operands 'arg1))

;; (define (spread-arguments operands)
;;   (preserving
;;    '(arg1 arg2)
;;    (compile (car operands) 'arg1 'next)
;;    (compile (cadr operands) 'arg2 'next)))

;; (define (compile-plus exp target linkage)
;;   (let ((operand-codes (spread-arguments (operands exp))))
;;     (end-with-linkage
;;      linkage
;;      (preserving
;;       '()
;;       operand-codes
;;       (make-instruction-sequence
;;        '(arg1 arg2)
;;        (list target)
;;        `((assign ,target (op +) (reg arg1) (reg arg2))))))))

(define (compile-self-evaluating exp target linkage compile-env)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage compile-env)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage compile-env)
  (let ((lexical-address (find-variable exp compile-env)))
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env)
    (list target)
    (if (eq? lexical-address 'not-found)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
    ;; ex 5.42
    `((assign
       ,target
       (op lexical-address-lookup)
       (reg env)
       (const ,lexical-address))))))))

(define (compile-assignment exp target linkage compile-env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next compile-env)))
    (let ((lexical-address (find-variable var compile-env)))
      (end-with-linkage
       linkage
       (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
         '(env val)
         (list target)
         (if (eq? lexical-address 'not-found)
             `((perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env))
               (assign ,target (const ok)))
             `((perform (op lexical-address-set!)
                        (reg env)
                        (const ,lexical-address)
                        (reg val))
               (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage compile-env)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next compile-env)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage compile-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next)
                                  after-if
                                  linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-env))
            (c-code (compile (if-consequent exp) target consequent-linkage compile-env))
            (a-code (compile (if-alternative exp) target linkage compile-env)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val)
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append
    (symbol->string name)
    (number->string (new-label-number)))))

(define (compile-sequence seq target linkage compile-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next compile-env)
                  (compile-sequence (rest-exps seq) target linkage compile-env))))

(define (compile-lambda exp target linkage compile-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next)
                              after-lambda
                              linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry compile-env))
       after-lambda))))


(define (compile-lambda-body exp proc-entry compile-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return
                       ;; ex 5.40
                       (cons formals compile-env)))))

(define (compile-application exp target linkage compile-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-env))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage compile-env)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '()
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val)
                 '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl)
           '(argl)
           '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

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

(define (compile-proc-appl target linkage compile-env)
  ;; Procedure calls except the last one.
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ;; The only time the compiler specifies a register other than
        ;; val is when targeting the evaluation of an operator to
        ;; proc, e.g., evaluating ((f 0) 1)
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc)
            all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ;; The last procedure call in the body of a procedure will be
        ;; compiled with 'return.
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage,target not val: COMPILE" target))))

(define all-regs '(env proc val argl continue))

; Instruction sequences

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
      (list-difference (registers-needed seq2) (registers-modified seq1)))
     (list-union (registers-modified seq1) (registers-modified seq2))
     (append (statements seq1) (statements seq2))))

  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))

  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg) (registers-needed seq1))
              (list-difference (registers-modified seq1) (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1) (registers-needed seq2))
   (list-union (registers-modified seq1) (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;; ex 5.33

;; The difference is how they handle the else clause. In the exercise,
;; it saves the env and goes on to compute factorial-alt. Later, the
;; env is restored to get the n to perform the multiplication. In the
;; text, it saves the argl which contains the correct n. Later, the
;; argl is restored to perform the multiplication.

;; Performance-wise the two are equal, because they have the same
;; amount of saves and restores.

;; ex 5.34

;; The essential difference is that the recursive call in recursive
;; version isn't the tail call, but the recursive call in iterative
;; version is. Therefore, the former has an extra 'save continue'.

;; ex 5.35

;; (define (f x) (+ x (g (+ x 2)))

;; TODO ex 5.36

;; The order is right-to-left; it's determined in construct-arglist. To
;; evaluate in a left-to-right order, don't reverse the arglist and use append
;; instead of cons to join the list.

;; TODO ex 5.37

;; ex 5.39

(define (lexical-address-lookup env address)
  (define (loop index vals)
    (cond ((null? vals) (error "Unbound location" address))
          ((= index 0)
           (let ((v (car vals)))
             (if (eq? v '*unassigned*)
                 (error "Unassigned variable in location" address)
                 v)))
          (else (loop (- index 1) (cdr vals)))))

  (let ((frame (loop (car address) env)))
    (loop (cdr address) (cdr frame))))

(define (lexical-address-set! env address value)
  (define (loop-env index vals)
    (cond ((null? vals) (error "Unbound location" address))
          ((= index 0) (car vals))
          (else (loop-env (- index 1) (cdr vals)))))

  (define (loop-vals index vals)
    (cond ((null? vals) (error "Unbound location" address))
          ((= index 0)  (set-car! vals value))
          (else (loop-vals (- index 1) (cdr vals)))))

  (let ((frame (loop-env (car address) env)))
    (loop-vals (cdr address) (cdr frame))))

;; ex 5.41

(define (find-variable var compile-env)
  (define (loop-env env index)
    (cond ((null? env) 'not-found)
          (else (let ((ret (loop-vars (car env) 0)))
                  (if (eq? ret 'not-found)
                      (loop-env (cdr env) (+ index 1))
                      (cons index ret))))))

  (define (loop-vars frame index)
    (cond ((null? frame) 'not-found)
          ((equal? var (car frame)) index)
          (else (loop-vars (cdr frame) (+ index 1)))))

  (loop-env compile-env 0))

(define env (list
             (list '(y z) 1 2)
             (list '(a b c d e) 1 '*unassigned* 3 4 5)
             (list '(x y) 7 2)))


(#%provide compile statements lexical-address-lookup lexical-address-set!)
