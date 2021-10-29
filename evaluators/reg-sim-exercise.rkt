#lang sicp

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

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    ;; (for-each (lambda (register-name)
    ;;             ((machine 'allocate-register)
    ;;              register-name))
    ;;           register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ; ex 5.18
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

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      (newline)
      (display (list 'total-pushes
                     '=
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (insts '())
        (entry-regs '())
        (saves '())
        (reg-assign '())
        ; ex 5.15
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

      ; ex 5.13

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

      ; ex 5.19
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((and (member (instruction-label (car insts)) breakpoints)
                      (not proceed))
                 'suspend)
                ; ex 5.16
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
              ; ex 5.12
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

(define (start machine)
  (machine 'start))

(define (set-breakpoint machine label index)
  ((machine 'set-breakpoint) label index))

(define (proceed-machine machine)
  (machine 'proceed))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels/alt text)
  (if (null? text)
      (cons '() '())
      (let ((result
             (extract-labels/alt (cdr text))))
        (let ((insts (car result))
              (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (cons
                 insts
                 (cons
                  (make-label-entry
                   next-inst insts)
                  labels))
                (cons
                 (cons
                  (make-instruction next-inst)
                  insts)
                 labels)))))))

(define (extract-labels text receive)
  ; ex 5.17
  (define (set-label-for-inst! inst label index)
    (set-cdr! (instruction-metadata inst) (cons label index)))

  (define (for-each-set insts label)
    (define no-label
      (filter (lambda (x) (null? (cdr (instruction-metadata x))))
              insts))
    (define (loop ys index)
      (if (null? ys)
          '()
          (begin
            (set-label-for-inst! (car ys) label index)
            (loop (cdr ys) (+ index 1)))))
    (loop no-label 1))

  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           ;; (display next-inst)
           ;; (newline)
           (if (symbol? next-inst)
               ; ex 5.8
               ; a = 3
               (begin
                 (if (assoc next-inst labels)
                     (error "Duplicate LABEL" next-inst)
                     (begin
                       (for-each-set insts next-inst)
                       (receive insts (cons (make-label-entry next-inst insts) labels)))))
               (receive (cons (make-instruction next-inst) insts) labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (make-instruction text)
  (cons (cons text '()) '()))

(define (instruction-metadata inst) (car inst))

(define (instruction-label inst) (cdr (instruction-metadata inst)))

(define (instruction-text inst) (caar inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type: ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine labels operations)
               (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()   ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda () (push stack (get-contents reg)) (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda () (set-contents! reg (pop stack)) (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))

(define (register-exp-reg exp)
  (cadr exp))

(define (constant-exp? exp)
  (tagged-list? exp 'const))

(define (constant-exp-value exp)
  (cadr exp))

(define (label-exp? exp)
  (tagged-list? exp 'label))

(define (label-exp-label exp)
  (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e) (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; ex 5.11

; 2

;; (define (make-save inst machine stack pc)
;;   (let* ((name (stack-inst-reg-name inst))
;;          (reg (get-register machine name)))
;;     (lambda ()
;;       (push stack (cons name (get-contents reg)))
;;       (advance-pc pc))))

;; (define (make-restore inst machine stack pc)
;;   (let* ((name (stack-inst-reg-name inst))
;;          (reg (get-register machine name)))
;;     (lambda ()
;;       (let ((pair (pop stack)))
;;         (cond ((equal? name (car pair))
;;                (set-contents! reg (cdr pair))
;;                (advance-pc pc))
;;               (else (error "Bad RESOTRE" name)))))))

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

;; (set-register-contents! gcd-machine 'a 206)
;; (set-register-contents! gcd-machine 'b 40)
;; (set-breakpoint gcd-machine 'test-b 4)
;; (start gcd-machine)
;; (get-register-contents gcd-machine 'a)



; TODO ex 5.6

; TODO ex 5.10


(define (fib-machine)
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n − 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n − 2)
     (assign n (reg val)) ; n now contains Fib(n − 2)
     (restore val)      ; val now contains Fib(n − 1)
     (restore continue)
     (assign val        ; Fib(n − 1) + Fib(n − 2)
             (op +)
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done)))

(define (start-fib n)
  (let ((fib (fib-machine)))
    (set-register-contents! fib 'n n)
    (start fib)
    (fib 'print-stats)
    (newline)
    (get-register-contents fib 'val)))

(#%provide make-machine set-register-contents! start assemble)
