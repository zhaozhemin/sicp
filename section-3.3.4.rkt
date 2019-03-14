#lang sicp

; Queue

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond 
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue))
    (else (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define (queue-length queue)
  (length (front-ptr queue)))

; Gates

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))

; ex 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

; TODO ex 3.29


; TODO  ex 3.31

; If the procedure wasn't immediately run, the sum would be 0. When immediately
; run, E will become 1 since C is 0. After setting A to 1, D also becomes 1.
; So S becomes 1 because both D and E is 1. On the other hand, E will never be
; 1, because C never changes its value and the inverter procedure connecting C
; and E will never run. S remains 0 since D is 1 and E is 0.

; Agenda

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)

  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

; Wire

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    ; (define (accept-action-procedure! proc)
    ;   (set! action-procedures
    ;     (cons proc action-procedures)))

    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'actions) action-procedures)
            (else (error "Unknown operation: WIRE" m))))

    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))


(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display "
                          New-value = ")
                 (display (get-signal wire)))))


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define a (make-wire))
(define b (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(define ret (make-wire))
; (inverter a b)
; (set-signal! b 1)
; (and-gate a b ret)
; (set-signal! a 1)
; (set-signal! b 1)
; (probe 'sum sum)
; (probe 'carry carry)
; (half-adder a b sum carry)
; (set-signal! a 1)
; (propagate)

; ex 3.32

; If the initial value for A and B is already 0 and 1, then the order of
; executing procedures doesn't matter. If the values are changed from 0, 0 to
; 0, 1 to 1,0, the FIFO will produce 0 (correct), LIFO will produce 1
; (wrong).

; ex 3.30
; NOTE the order of the three lists is reversed; doesn't handle the final carry
; bit well.

(define (build-list n proc)
  (if (<= n 0)
      '()
      (cons (proc n) (build-list (- n 1) proc))))

(define as (build-list 8 (lambda (x) (make-wire))))
(define bs (build-list 8 (lambda (x) (make-wire))))
(define ss (build-list 8 (lambda (x) (make-wire))))
(define c (make-wire))

(define (ripple-carry-adder as0 bs0 ss0 c)
  (define (loop as bs ss cin)
    (cond
      [(null? as) 'ok]
      [else (let ([carry (make-wire)])
              (full-adder (car as) (car bs) cin (car ss) carry)
              (loop (cdr as) (cdr bs) (cdr ss) carry))]))
  (loop as0 bs0 ss0 (make-wire)))
