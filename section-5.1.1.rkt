#lang sicp

;; ex 5.1 & ex 5.2

(define fact-machine
  (make-machine
   '(n product counter)
   (list (list '> >) (list '+ +) (list '* *))
   '(start
     (assign product (const 1))
     (assign counter (const 1))
     iter-loop
     (test (op >) (reg counter) (reg n))
     (branch (label done))
     (assign product (op *) (reg counter) (reg product))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label iter-loop))
     done)))

;; TODO ex 5.3
