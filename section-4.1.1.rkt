#lang sicp

; ex 4.1

(define (list-of-values/left->right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env))
            (rest (list-of-values (rest-operands exps) env)))
        (cons first rest))))

(define (list-of-values/right->left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env))
            (first (eval (first-operand exps) env)))
        (cons first rest))))
