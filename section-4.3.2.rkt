#lang sicp

; ex 4.38

; 5

; ex 4.39

; The order doesn't affect the answer but affect the speed. Put distinct? at
; the bottom of the requires could speed up the process. The reason is
; distinct? has to loop over the list. Some combinations could be simply ruled
; out by simple equality check.

; ex 4.40

(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)])
    (require (not (= baker 5)))
    (let ([cooper (amb 1 2 3 4 5)])
      (require (not (= cooper 1)))
      (let ([fletcher (amb 1 2 3 4 5)])
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ([miller (amb 1 2 3 4 5)])
          (require (> miller cooper))
          (let ([smith (amb 1 2 3 4 5)])
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

; ex 4.41
;
; A naive solution would be writing nested for loops.

; ex 4.42
;
; Should write a xor function.

(define (liars)
  (let ([betty (amb 1 2 3 4 5)]
        [ethel (amb 1 2 3 4 5)]
        [joan (amb 1 2 3 4 5)]
        [kitty (amb 1 2 3 4 5)]
        [mary (amb 1 2 3 4 5)])
    (require (and (or (and (= kitty 2) (not (= betty 3)))
                      (and (not (= kitty 2)) (= betty 3)))
                  (or (and (= ethel 1) (not (= joan 2)))
                      (and (not (= ethel 1)) (= joan 2)))
                  (or (and (= joan 3) (not (= ethel 5)))
                      (and (not (= joan 3)) (= ethel 5)))
                  (or (and (= kitty 2) (not (= mary 4)))
                      (and (not (= kitty 2)) (= mary 4)))
                  (or (and (= mary 4) (not (= betty 1)))
                      (and (not (= mary 4)) (= betty 1)))))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; % ex 4.43
;
; What's the point of writing functions this way? The interpreter uses amb to
; choose values, yet I have to list all possible values in the penultimate
; require clause. The answer is alreay clear by then.

(define (father)
  (let ([mary 'moore]
        [melissa 'barnacle]
        [lorna (amb 'downing 'hall 'parker)]
        [rosalind (amb 'downing 'hall 'parker)]
        [gabrielle (amb 'downing 'hall 'parker)])
    (require (not (eq? lorna 'moore)))
    (require (not (eq? rosalind 'hall)))
    (require (not (eq? gabrielle 'barnacle)))
    (require (cond [(eq? gabrielle 'downing) (eq? melissa 'parker)]
                   [(eq? gabrielle 'hall) (eq? rosalind 'parker)]
                   [else false]))
    (require (distinct? (list mary melissa lorna rosalind gabrielle)))
    (list (list 'mary mary)
          (list 'melissa melissa)
          (list 'lorna lorna)
          (list 'rosalind rosalind)
          (list 'gabrielle gabrielle))))

; 4.44
;
; It has to be runned in the amb evaluator. Racket's SICP package doesn't
; revert the assignment before choosing another branch.

(define (amb-list xs)
  (require (not (null? xs)))
  (amb (car xs) (amb-list (cdr xs))))

(define (check? x y)
  (let ([ax (car x)]
        [dx (cadr x)]
        [ay (car y)]
        [dy (cadr y)])
    (or (= ax ay)
        (= dx dy)
        (= (abs (- ax ay)) (abs (- dx dy))))))

(define (range hi)
  (define (loop lo)
    (if (>= lo hi)
        '()
        (cons lo (loop (+ lo 1)))))
  (loop 0))

(define (ormap f xs)
  (cond
    [(null? xs) false]
    [(f (car xs)) true]
    [else (ormap f (cdr xs))]))

(define (queens n)
  (define answer '())
  (define (loop col)
    (cond
      [(> col (- n 1)) (require (= (length answer) n)) answer]
      [else (let ([row (amb-list (range n))])
              (require (not (ormap (lambda (p) (check? (list row col) p)) answer)))
              (set! answer (cons (list row col) answer))
              (loop (+ col 1)))]))
  (loop 0))


; ex 4.45
;
; 1. lectures -> to the student in the class with the cat
; 2. in the class, lectures with the cat -> to the student
; 3. in the class, lectures -> to the student with the cat
; 4. in the class (that is) with the cat, lectures -> to the student
; 5. lectures with the cat -> to the student in the class

; TODO ex 4.46
; TODO ex 4.47
; TODO ex 4.48
; TODO ex 4.49
