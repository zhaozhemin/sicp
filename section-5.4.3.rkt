#lang sicp

;; ex 5.23

;; (test (op let?) (reg exp))
;; (branch (label ev-let))

;; ev-let
;; (assign exp (op let->combination) (reg exp))
;; (goto (label eval-dispatch))


;; ex 5.24

;; (test (op cond?) (reg exp))
;; (branch (label ev-cond))

;; ev-cond
;; (save continue)
;; (assign unev (op cond-clauses) (reg exp))

;; ev-cond-clauses
;; (test (op null?) (reg unev))
;; (branch (label ev-cond-end))
;; (goto (label ev-cond-clause))

;; ev-cond-clause
;; (assign exp (op first-exp) (reg unev))
;; (test (op cond-else-clause?) (reg exp))
;; (branch (label ev-cond-else))
;; (save exp)
;; (save unev)
;; (assign exp (op cond-predicate) (reg exp))
;; (assign continue (label after-ev-cond-clause))
;; (goto (label eval-dispatch))

;; ev-cond-else
;; (assign unev (op cond-actions) (reg exp))
;; (goto (label ev-sequence))

;; after-ev-cond-clause
;; (restore unev)
;; (restore exp)
;; (test (op true?) (reg val))
;; (branch (label ev-cond-true))
;; (assign unev (op rest-exps) (reg unev))
;; (goto (label ev-cond-clauses))

;; ev-cond-end
;; (restore continue)
;; (goto (reg continue))

;; ev-cond-true
;; (assign unev (op cond-actions) (reg exp))
;; (goto (label ev-sequence))

;; TODO ex 5.25
