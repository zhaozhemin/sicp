#lang sicp

;; ex 5.9

;; This modification doesn't work with compilation in Chapter 5.5.

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (equal? (car e) 'label)
                    (error "Bad OPERANDS")
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

;; TODO ex 5.10

;; ex 5.11

;; TODO 1

;; 2

(define (make-save inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (push stack (cons name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let ((pair (pop stack)))
        (cond ((equal? name (car pair))
               (set-contents! reg (cdr pair))
               (advance-pc pc))
              (else (error "Bad RESOTRE" name)))))))

;; TODO 3

;; ex 5.12 & ex 5.13 & ex 5.15 & ex 5.16 & ex 5.19

(define (sort-insts insts)
  (define (insert x xs)
    (cond [(null? xs) (list x)]
          [(string<? (symbol->string (instruction-text x))
                     (symbol->string (instruction-text (car xs))))
           (cons x xs)]
          [else (cons (car xs) (insert x (cdr xs)))]))
  (cond [(or (null? insts) (null? (cdr insts))) insts]
        [else (insert (car insts) (sort-insts (cdr insts)))]))

(define (uniq-insts insts)
  (define (uniq x xs)
    (if (member x xs)
        xs
        (cons x xs)))
  (cond [(or (null? insts) (null? (cdr insts))) insts]
        [else (uniq (car insts) (uniq-insts (cdr insts)))]))

(define (filter proc xs)
  (cond [(null? xs) '()]
        [(proc (car xs)) (cons (car xs) (filter proc (cdr xs)))]
        [else (filter proc (cdr xs))]))


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (insts '())
        (entry-regs '())
        (saves '())
        (reg-assign '())
        ;; 5.15
        (execute-count 0)
        (trace-on false)
        (proceed false)
        (breakpoints '()))
    (let ((the-ops
           (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag))))

      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table (cons (list name (make-register name))
                                       register-table)))
        'register-allocated)

      ;; 5.13

      ; I think this is the most convenient place to make the
      ; change. Of course the caveat is that when accessing an unknown
      ; register, instead of throwing an error, now it just prints
      ; *unassigned*.
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (cadr (assoc name register-table))))))

      ; 5.19
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((and (member (instruction-label (car insts)) breakpoints)
                      (not proceed))
                 'suspend)
                ; 5.16
                (else
                 (set! proceed false)
                 (set! execute-count (+ execute-count 1))
                 (if trace-on
                     (begin
                       (display (instruction-metadata (car insts)))
                       (newline)))
                 ((instruction-execution-proc (car insts)))
                 (execute)))))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ;; 5.12
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (let* ((instructions (uniq-insts (sort-insts (map car seq))))
                        (goto-regs
                         (filter
                          (lambda (x)
                            (and (equal? 'goto (car x))
                                 (equal? 'reg (caadr x))))
                          instructions))
                        (entries (map (lambda (x) (cadadr x)) goto-regs))
                        (save-regs
                         (map (lambda (y) (cadr y))
                              (filter (lambda (x) (equal? 'save (car x)))
                                      instructions)))
                        (assigns (filter (lambda (x) (equal? 'assign (car x)))
                                         instructions)))
                   (set! the-instruction-sequence seq)
                   (set! insts instructions)
                   (set! entry-regs entries)
                   (set! saves save-regs)
                   (for-each
                    (lambda (x)
                      (let* ((var (assign-reg-name x))
                             (exps (assign-value-exp x))
                             (from (assoc var reg-assign)))
                        (if from
                            (set-cdr! from (cons exps (cdr from)))
                            (set! reg-assign (cons (cons var (list exps))
                                                   reg-assign)))))
                    assigns))))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'seq) the-instruction-sequence)
              ((eq? message 'insts) insts)
              ((eq? message 'entry-regs) entry-regs)
              ((eq? message 'saves) saves)
              ((eq? message 'reg-assign) reg-assign)
              ((eq? message 'print-stats) (stack 'print-statistics))
              ((eq? message 'print-exe-count) execute-count)
              ((eq? message 'reset-exe-count) (set! execute-count 0))
              ((eq? message 'trace-on) (set! trace-on true))
              ((eq? message 'trace-off) (set! trace-on false))
              ((eq? message 'trace-register)
               (lambda (name trace)
                 (let ((reg (lookup-register name)))
                   (reg (if trace 'trace-on 'trace-off)))))
              ((eq? message 'set-breakpoint)
               (lambda (label index)
                 (set! breakpoints (cons (cons label index) breakpoints))))
              ((eq? message 'breakpoints) breakpoints)
              ((eq? message 'proceed) (set! proceed true) (execute))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))


;; ex 5.18

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace-on
                   (begin
                     (display (list name contents value))
                     (newline)))
               (set! contents value)))
            ((eq? message 'trace-on) (set! trace-on true))
            ((eq? message 'trace-off) (set! trace-on false))
            (else (error "Unknown request: REGISTER" message))))
    dispatch))
