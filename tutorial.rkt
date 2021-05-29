#lang racket
(require racket/trace)

; function definitions in racket are funny
; kind of like it though
(define (double num)
  (* 2 num))
(equal? (double 2) 4)
(equal? (double -1.4) -2.8)

; lists in racket seem even funnier it seems like there are some built
; in functionalities like map which operates on lists but iterations from
; scratch require recursion - the "trace" function is really nice for seeing this
(define l (list 1 2 3 4))
(define (my-list-len l)
  (if (empty? l) 0 (+ 1 (my-list-len (rest l)))))
(trace my-list-len)
(equal? (my-list-len l) 4)

; exercise: function to find the sum of all elements in a list
(define (my-sum l)
  (if (empty? l) 0 (+ (first l) (my-sum (rest l)))))
(equal? (my-sum l) 10)

; exercise: function to find the smallest element in a list
(define l1 (list 9 2 5 12 0 -4))
(define (smallest l tiny)
  (if (empty? l) tiny
      (if (< (first l) tiny) (smallest (rest l) (first l))
          (smallest (rest l) tiny))))
(equal? (smallest l1 (first l1)) -4)

; exercise: function to find the second smallest element in a list
; NOTE: this one is pretty interesting because we're required to do
;       more than one function call within some of the conditionals.
;       Requires us to use a "cond" block
(define (smallest2 l tiny tiny2)
  (if (empty? l) tiny2
      (cond
        [(< (first l) tiny)
            (set! tiny2 tiny)
            (set! tiny (first l))
            (smallest2 (rest l) tiny tiny2)]
        [(< (first l) tiny2)
            (set! tiny2 (first l))
            (smallest2 (rest l) tiny tiny2)]
        [else (smallest2 (rest l) tiny tiny2)])))
(equal? (smallest2 l1 (first l1) (first l1)) 0)