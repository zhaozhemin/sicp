#lang sicp

(define (make-table)
  (list '*table*))

; ex 3.24

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

(define (make-table-proc same-key?)

  (define local-table (list '*table*))

  (define (lookup key-1 key-2)
    (let ((subtable
            (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
        (let ((record
                (assoc key-2 (cdr subtable) same-key?)))
          (if record (cdr record) false))
        false)))

  (define (insert! key-1 key-2 value)
    (let ((subtable
            (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
        (let ((record
                (assoc key-2 (cdr subtable) same-key?)))
          (if record
            (set-cdr! record value)
            (set-cdr! subtable
                      (cons (cons key-2 value)
                            (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
    'ok)

  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation: TABLE" m))))

  dispatch)

; ex 3.25
; NOTE Kinda works, but a key can't have both a value and a subtable. For
; example, '(lookup t 'a) -> 0' and '(lookup 't 'a 'b) -> 1' can't coexist.

(define (lookup table0 . keys0)
  (define (loop keys table)
    (let ([pair (assoc (car keys) (cdr table) equal?)])
      (cond
        [(not pair) #f]
        [(null? (cdr keys)) (if pair (cdr pair) #f)]
        [else (loop (cdr keys) pair)])))
  (loop keys0 table0))

(define (insert! table value . keys0)
  (define (loop keys table)
    (let ([pair (assoc (car keys) (cdr table) equal?)])
      (cond
        [(null? (cdr keys)) (if pair 
                              (set-cdr! pair value)
                              (set-cdr! table
                                        (cons (cons (car keys) value)
                                              (cdr table))))]
        [(not pair) (let ([subtable (list (car keys))])
                      (set-cdr! table (cons subtable (cdr table)))
                      (loop (cdr keys) subtable))]
        [else (loop (cdr keys) pair)])))
  (loop keys0 table))

; TODO ex 3.26

; For the sake of the simplicity, I'll just use single key table as example. First, make-table doesn't
; make much differences except the cdr of the list will be a bst. As for lookup procedure, we start at
; the root of the tree, if the given key equals to the root...

; ex 3.27

; It wouldn't work, before fib would call fib itself rather than memo-fib.
