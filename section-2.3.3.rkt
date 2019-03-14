#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ex 2.59

(define (union-set a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(element-of-set? (car a) b) (union-set (cdr a) b)]
    [else (cons (car a) (union-set (cdr a) b))]))

; ex 2.60

; element-of-set is the same as before.

; faster
(define (adjoin-set-2.60 x s)
  (cons x s))

; faster
(define (union-set-2.60 a b)
  (append a b))

; intersection-set is the same as before, but it's slower because the lists are
; generally longer.

(define (element-of-set-ordered? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-ordered? x (cdr set)))))


(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))

; ex 2.61

(define (adjoin-set-ordered x s)
  (cond
    [(null? s) (cons x '())]
    [(< x (car s)) (cons x s)]
    [(= x (car s)) s]
    [else (cons (car s) (adjoin-set-ordered x (cdr s)))]))

; ex 2.62

(define (union-set-ordered a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(= (car a) (car b)) (union-set-ordered (cdr a) b)]
    [(< (car a) (car b)) (cons (car a) (union-set-ordered (cdr a) b))]
    [else (cons (car b) (union-set-ordered a (cdr b)))]))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

; It's in-order traversal.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                      (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

(define (tree->list-preorder tree)
  (if (null? tree)
      '()
      (append (list (entry tree))
              (tree->list-preorder (left-branch tree))
              (tree->list-preorder (right-branch tree)))))

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))

(define tree4 (make-tree 15
                         (make-tree 13
                                    (make-tree 1 '() '())
                                    (make-tree 14 '() '()))
                         (make-tree 20
                                    '()
                                    (make-tree 25
                                               (make-tree 22 '() '())
                                               (make-tree 29 '() '())))))


; ex 2.63

; a
; 1. Two procedures produce the same result.
; 2. '(1 3 5 7 9 11)

; b

; I think it is...

; ex 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

; a

(define (partial-tree-* elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (partial-tree-* elts left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n (+ left-size 1))]
             [this-entry (car non-left-elts)]
             [right-result (partial-tree-* (cdr non-left-elts) right-size)]
             [right-tree (car right-result)]
             [remaining-elts (cdr right-result)])
        (cons (make-tree this-entry
                         left-tree
                         right-tree)
              remaining-elts))))

; Maybe rewriting this procedure using let* will make it more clear.
; partial-tree makes a tree off three parts: an element in the middle of the
; (ordered) list, which is the root of the tree; a sublist containning all the
; elements smaller than the root, which is the left branch; a sublist
; containning all the elements bigger than the root, which is the right branch.
; It's recursively called on the two sublists, which in turn makes a tree off
; three parts, to make subtrees.

; ex 2.65

(define (union-set-tree a b)
  (list->tree (union-set-ordered (tree->list-1 a) (tree->list-1 b))))

(define (intersection-set-tree a b)
  (list->tree (intersection-set-ordered (tree->list-1 a) (tree->list-1 b))))

; ex 2.66

(define (lookup q db)
  (cond
    [(null? db) '()]
    [(equal? q (key (car db))) (car db)]
    [(< q (key (car db))) (lookup q (left-branch db))]
    [(> q (key (car db))) (lookup q (right-branch db))]))
