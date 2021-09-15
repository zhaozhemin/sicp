#lang sicp

;; ex 5.31

;; 1. Nothing needs to be saved.
;; 2. Same as 1.
;; 3. argl and proc need to be saved. The reason why env is not saved
;; is because construct-arglist works backwards; it first compile y,
;; and then (g 'x). So even if env is changed in evaluating (g 'x), y
;; has already got the correct value.
;; 4. Same as 3.

;; ex 5.32

;; ev-application-symbol
;; (save continue)
;; (save unev)
;; (assign continue (label ev-appl-did-operator-symbol))
;; (goto (label eval-dispatch))

;; ev-appl-did-operator-symbol
;; (restore unev)             ; the operands
;; (assign argl (op empty-arglist))
;; (assign proc (reg val))    ; the operator
;; (test (op no-operands?) (reg unev))
;; (branch (label apply-dispatch))
;; (save proc)
;; (goto (label ev-appl-operand-loop))

;; ev-application
;; (assign unev (op operands) (reg exp))
;; (assign exp (op operator) (reg exp))
;; (test (op variable?) (reg exp))
;; (branch (label ev-application-symbol))
;; (save continue)
;; (save env)
;; (save unev)
;; (assign continue (label ev-appl-did-operator))
;; (goto (label eval-dispatch))
