#lang sicp

;; ex 5.8 && ex 5.17

;; a = 3

(define (extract-labels text receive)
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
           (if (symbol? next-inst)
               (begin
                 (if (assoc next-inst labels)
                     (error "Duplicate LABEL" next-inst)
                     (begin
                       (for-each-set insts next-inst)
                       (receive
                           insts
                           (cons (make-label-entry next-inst insts)
                                 labels)))))
               (receive (cons (make-instruction next-inst) insts) labels)))))))
