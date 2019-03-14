#lang sicp

; ex 2.25

; cadaddr
; caar
; ?

; ex 2.26

; '(1 2 3 4 5 6)
; '((1 2 3) 4 5 6))
; '((1 2 3) (4 5 6))

; ex 2.27

(define (deep-reverse xs)
  (define (loop xs0 accu)
    (cond
      ((null? xs0) accu)
      ((list? (car xs0)) (loop (cdr xs0) (cons (deep-reverse (car xs0)) accu)))
      (else (loop (cdr xs0) (cons (car xs0) accu)))))
  (loop xs '()))

; ex 2.28

(define (fringe xs)
  (cond
    ((null? xs) '())
    ((pair? (car xs)) (append (fringe (car xs)) (fringe (cdr xs))))
    (else (cons (car xs) (fringe (cdr xs))))))

; ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-struct branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (cond
    [(not (list? (branch-struct branch))) (branch-struct branch)]
    [else (total-weight (branch-struct branch))]))

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? mobile)
  (if (not (list? mobile))
      #t
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-struct (left-branch mobile)))
           (balanced? (branch-struct (right-branch mobile))))))

; ex 2.30

(define (square-tree tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (* tree tree)]
    [else (cons (square-tree (car tree))
                (square-tree (cdr tree)))]))

(define (square-tree-map tree)
  (map (lambda (x) (if (pair? x) (square-tree-map x) (* x x)))
       tree))

; ex 2.31

(define (tree-map proc tree)
  (map (lambda (x) (if (pair? x) (tree-map proc x) (proc x)))
       tree))

; ex 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; If subsets is a procedure for computing all subsets of a set s, then the
; result of (subsets (cdr s)) is all subsets of the set s excluding the first
; element, thus the all subsets of s is (subsets (cdr s)) plus the result of
; consing the first element of s onto every list in (subsets (cdr s)).
