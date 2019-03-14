#lang sicp
(#%require sicp-pict)

; ex 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

; ex 2.45

(define (split big small)
  (lambda (painter)
    (big painter (small painter painter))))

(define (right-split painter)
  ((split beside below) painter))

(define (up-split-v1 painter)
  ((split below beside) painter))

; ex 2.46

; (define (make-vect x y)
;   (cons x y))

; (define (xcor-vect v)
;   (car v))

; (define (ycor-vect v)
;   (cdr v))

; (define (add-vect a b)
;   (make-vect (+ (xcor-vect a) (xcor-vect b))
;              (+ (ycor-vect a) (ycor-vect b))))

; (define (sub-vect a b)
;   (make-vect (- (xcor-vect a) (xcor-vect b))
;              (- (ycor-vect a) (ycor-vect b))))

; (define (scale-vect s v)
;   (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;  ex 2.47

; (define (make-frame origin edge1 edge2)
;   (list origin edge1 edge2))

; (define (origin-frame frame)
;   (car frame))

; (define (edge1-frame frame)
;   (cadr frame))

; (define (edge2-frame frame)
;   (caddr frame))

;  ex 2.48

; (define (make-segment a b)
;   (cons a b))

; (define (start-segment seg)
;   (car seg))

; (define (end-segment seg)
;   (cadr seg))

; (define (frame-coord-map frame)
;   (lambda (v)
;     (add-vect
;      (origin-frame frame)
;      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
;                (scale-vect (ycor-vect v) (edge2-frame frame))))))

; ex 2.49

(define (draw-outline frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0.99 0))
                           (make-segment (make-vect 0 0) (make-vect 0 0.99))
                           (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0))
                           (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99)))))

(define (draw-x frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0.99 0.99))
                           (make-segment (make-vect 0 0.99) (make-vect 0.99 0)))))

(define (draw-diamond frame)
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 0.99))
                           (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
                           (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0)))))

; (define (transform-painter painter origin corner1 corner2)
;   (lambda (frame)
;     (let ((m (frame-coord-map frame)))
;       (let ((new-origin (m origin)))
;         (painter (make-frame
;                    new-origin
;                    (sub-vect (m corner1) new-origin)
;                    (sub-vect (m corner2) new-origin)))))))
