#lang sicp

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

;; ex 5.36

;; The order is right-to-left; it's determined in construct-arglist. To
;; evaluate in a left-to-right order, don't reverse the arglist and use append
;; instead of cons to join the list.

;; TODO ex 5.37

;; ex 5.38

(define (spread-arguments operands compile-env)
  (list
   (compile (car operands) 'arg1 'next compile-env)
   (if (> (length operands) 2)
       (compile (cons '+ (cdr operands)) 'arg2 'next compile-env)
       (compile (cadr operands) 'arg2 'next compile-env))))

(define (compile-plus exp target linkage compile-env)
  (let ((operand-codes (spread-arguments (operands exp) compile-env)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      (car operand-codes)
      (preserving
       '(env arg1)
       (cadr operand-codes)
       (make-instruction-sequence
        '(arg1 arg2)
        (list target)
        `((assign ,target (op +) (reg arg1) (reg arg2)))))))))
