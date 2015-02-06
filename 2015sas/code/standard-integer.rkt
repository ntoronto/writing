#lang typed/racket

(provide Value
         Maybe-Value
         Computation
         run)

;; ===================================================================================================
;; Exported semantics

(begin-for-syntax
  (provide standard-integer)

  (define standard-integer
    (make-immutable-hasheq
     (list
      ;; Expression combinators
      (cons 'comp #'comp)
      (cons 'pair #'pair)
      (cons 'ifte #'ifte)
      (cons 'const #'const)
      (cons 'id #'id)
      ;; Constant values
      (cons 'const? #'value?)
      (cons 'zero #'(const 0))
      (cons 'one #'(const 1))
      (cons 'true #'(const #t))
      (cons 'false #'(const #f))
      (cons 'null #'(const '()))
      ;; Pair and cons-lists computations
      (cons 'fst #'fst)
      (cons 'snd #'snd)
      (cons 'pair? #'pair?)
      (cons 'null? #'null?)
      ;; Comparison computations
      (cons 'lt #'lt)
      (cons 'le #'le)
      (cons 'gt #'gt)
      (cons 'ge #'ge)
      (cons 'eq #'eq)
      ;; Arithmetic computations
      (cons 'add #'add)
      (cons 'sub #'sub)
      (cons 'neg #'neg)
      (cons 'mul #'mul)
      (cons 'div #'div)
      (cons 'rcp #'rcp)
      ))))

;; ===================================================================================================
;; Types

(require "bottom.rkt")

(define-type Value (U Boolean Integer Null (Pair Value Value)))
(define-type Maybe-Value (U Bottom Value))

(define-predicate value? Value)

(define-type Computation (-> Value Maybe-Value))

(: run (-> Computation Maybe-Value))
(define (run f) (f null))

;; ===================================================================================================
;; Expression combinators

(: comp (-> Computation Computation Computation))
(define ((comp f g) a)
  (define b (g a))
  (if (bottom? b) b (f b)))

(: pair (-> Computation Computation Computation))
(define ((pair f g) a)
  (define b (f a))
  (define c (g a))
  (if (or (bottom? b) (bottom? c))
      bottom
      (cons b c)))

(: ifte (-> Computation Computation Computation Computation))
(define ((ifte c t e) a)
  (define b (c a))
  (if (or (bottom? b) (not (boolean? b)))
      bottom
      (if b (t a) (e a))))

(: const (-> Value Computation))
(define ((const x) _) x)

(: id Computation)
(define (id a) a)

;; ===================================================================================================
;; Pair and cons-lists computations

(: fst Computation)
(define (fst a) (if (pair? a) (car a) bottom))

(: snd Computation)
(define (snd a) (if (pair? a) (cdr a) bottom))

;; ===================================================================================================
;; Comparison computations

(: lt Computation)
(define (lt a)
  (match a
    [(cons (? integer? x) (? integer? y))  (< x y)]
    [_  bottom]))

(: le Computation)
(define (le a)
  (match a
    [(cons (? integer? x) (? integer? y))  (<= x y)]
    [_  bottom]))

(: gt Computation)
(define (gt a)
  (match a
    [(cons (? integer? x) (? integer? y))  (> x y)]
    [_  bottom]))

(: ge Computation)
(define (ge a)
  (match a
    [(cons (? integer? x) (? integer? y))  (>= x y)]
    [_  bottom]))

(: eq Computation)
(define (eq a)
  (match a
    [(cons (? integer? x) (? integer? y))  (= x y)]
    [_  bottom]))

;; ===================================================================================================
;; Arithmetic computations

(: add Computation)
(define (add a)
  (match a
    [(cons (? integer? x) (? integer? y))  (+ x y)]
    [_  bottom]))

(: sub Computation)
(define (sub a)
  (match a
    [(cons (? integer? x) (? integer? y))  (- x y)]
    [_  bottom]))

(: neg Computation)
(define (neg a)
  (if (integer? a) (- a) bottom))

(: mul Computation)
(define (mul a)
  (match a
    [(cons (? integer? x) (? integer? y))  (* x y)]
    [_  bottom]))

(: div Computation)
(define (div a)
  (match a
    [(cons (? integer? x) (? integer? y))
     (if (zero? y) bottom (quotient x y))]
    [_  bottom]))

(: rcp Computation)
(define (rcp a)
  (match a
    [0  bottom]
    [1  1]
    [(? integer? a)  0]
    [_  bottom]))
