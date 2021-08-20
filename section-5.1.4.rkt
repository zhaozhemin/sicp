#lang sicp

;; ex 5.4 & ex 5.7

(define expt-machine-rec
  (make-machine
   '(b n continue val)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign continue (label done))
     loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-loop))
     (goto (label loop))
     after-loop
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     done)))

(define expt-machine-iter
  (make-machine
   '(b n counter product)
   (list (list '= =) (list '- -) (list '* *))
   '(prep
     (assign counter (reg n))
     (assign product (const 1))
     iter-loop
     (test (op =) (reg counter) (const 0))
     (branch (label done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label iter-loop))
     done)))

;; TODO ex 5.5

;; TODO ex 5.6
