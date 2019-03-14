#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

; ex 3.5

; TODO wrong anwser

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (abs (- x2 x1)) (abs (- y2 y1)))
     (monte-carlo trials (lambda () (p (random (min x1 x2) (max x1 x2))
                                       (random (min y1 y2) (max y1 y2)))))))

(define (P x y)
  (<= (+ (* x x) (* y y)) 1.0))

; TODO ex 3.6
