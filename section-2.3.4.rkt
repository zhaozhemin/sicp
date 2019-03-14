#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; ex 2.67

; '(A D A B B C A)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; ex 2.68

(define (encode-symbol symbol tree)
  (define (loop symbol current-branch)
    (cond
      ((not (member symbol (symbols current-branch))) (error "Ah oh"))
      ((leaf? current-branch) '())
      ((member symbol (symbols (left-branch current-branch)))
       (cons 0 (loop symbol (left-branch current-branch))))
      (else (cons 1 (loop symbol (right-branch current-branch))))))
  (loop symbol tree))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; ex 2.69

(define (successive-merge leaves)
  (cond
    ((= (length leaves) 1) (car leaves))
    (else (let ((x (car leaves))
                (y (cadr leaves)))
            (successive-merge (adjoin-set (make-code-tree x y) (cddr leaves)))))))

; (define p '((A 4) (B 2) (C 1) (D 1)))

(define pairs '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define song-tree (generate-huffman-tree pairs))
(define lyrics (map string->symbol
                    (string-split "GET A JOB
                                   SHA NA NA NA NA NA NA NA NA
                                   GET A JOB
                                   SHA NA NA NA NA NA NA NA NA
                                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                                   SHA BOOM")))

; ex 2.70

; 84
; 108

; ex 2.71

(define pairs-5 '((A 1) (B 2) (C 4) (D 8) (E 16)))


