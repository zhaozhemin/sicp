#lang sicp

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

;; ex 5.40

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
                       (cons formals compile-env)))))

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

;; ex 5.42

(define (compile-variable exp target linkage compile-env)
  (let ((lexical-address (find-variable exp compile-env)))
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env)
    (list target)
    (if (eq? lexical-address 'not-found)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
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

;; ex 5.43

;; Pass (scan-out-defines (lambda-body exp)) instead of (lambda-body exp)
;; to compile-sequence.

;; TODO ex 5.44
