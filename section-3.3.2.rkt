#lang sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

; (define (empty-queue? queue)
;   (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

; (define (front-queue queue)
;   (if (empty-queue? queue)
;     (error "FRONT called with an empty queue" queue)
;     (car (front-ptr queue))))

; (define (insert-queue! queue item)
;   (let ((new-pair (cons item '())))
;     (cond
;       ((empty-queue? queue)
;        (set-front-ptr! queue new-pair)
;        (set-rear-ptr! queue new-pair)
;        queue)
;       (else (set-cdr! (rear-ptr queue) new-pair)
;             (set-rear-ptr! queue new-pair)
;             queue))))

; (define (delete-queue! queue)
;   (cond 
;     ((empty-queue? queue)
;      (error "DELETE! called with an empty queue" queue))
;     (else (set-front-ptr! queue (cdr (front-ptr queue)))
;           queue)))

; ex 3.21

; The last pair of the front pointer is the same as the rear pointer.

(define (print-queue queue)
  (let loop ([front-pointer (front-ptr queue)])
    (cond
      [(null? front-pointer) (display '())]
      [else (display (car front-pointer)) (display " ")
            (loop (cdr front-pointer))])))

; ex 3.22

; (define (make-queue)
;   (let ((front-ptr '())
;         (rear-ptr '()))
;     (define (insert-queue! n)
;       (let ([p (cons n '())])
;         (cond
;           [(empty-queue?)
;            (set! front-ptr p)
;            (set! rear-ptr p)]
;           [else (set-cdr! rear-ptr p)
;                 (set! rear-ptr p)])))
;     (define (delete-queue!)
;       (if (empty-queue?)
;           (error "DELETE! called with an empty queue")
;           (set! front-ptr (cdr front-ptr))))
;     (define (front-queue)
;       (if (empty-queue?)
;           (error "FRONT! called with an empty queue")
;           (car front-ptr)))
;     (define (empty-queue?)
;       (null? front-ptr))
;     (define (dispatch m)
;       (cond
;         [(eq? m 'insert) insert-queue!]
;         [(eq? m 'delete) delete-queue!]
;         [(eq? m 'front) front-queue]
;         [(eq? m 'empty) empty-queue?]))
;     dispatch))

; (define (insert-queue! queue n)
;   ((queue 'insert) n) queue)

; (define (delete-queue! queue)
;   ((queue 'delete)) queue)

; (define (front-queue queue)
;   ((queue 'front)))

; (define (empty-queue? queue)
;   ((queue 'empty)))

; TODO ex 3.23
; How to make rear-delete-deque! O(1)?

(define make-deque make-queue)

; (define (front-insert-deque! deque n)
;   (cond
;     [(empty-queue? deque)
;      (let ([pair (cons n '())])
;        (set-front-ptr! deque pair)
;        (set-rear-ptr! deque pair)
;        deque)]
;     [else (let ([new-front-ptr (cons n (front-ptr deque))])
;             (set-front-ptr! deque new-front-ptr)
;             deque)]))
