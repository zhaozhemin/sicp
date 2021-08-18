#lang sicp

; ex 5.26

; 1. 10
; 2. 29 + 35n

; ex 5.27

;; |           | maximum depth | number of pushes  |
;; | recursive | 3 + 5n        | 16 + 32 * (n - 1) |
;; | iterative | 10            | 29 + 35n          |

;; ex 5.28

;; Without tail recursion opimization

;; ev-sequence
;; (test (op no-more-exps?) (reg unev))
;; (branch (label ev-sequence-end))
;; (assign exp (op first-exp) (reg unev))
;; (save unev)
;; (save env)
;; (assign continue (label ev-sequence-continue))
;; (goto (label eval-dispatch))

;; ev-sequence-continue
;; (restore env)
;; (restore unev)
;; (assign unev (op rest-exps) (reg unev))
;; (goto (label ev-sequence))

;; ev-sequence-end
;; (restore continue)
;; (goto (reg continue))

; ex 5.29

; 1. 13 + 5 * (n - 2) for n >= 2
; 2. TODO

; TODO ex 5.30
