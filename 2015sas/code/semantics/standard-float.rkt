#lang typed/racket

(require "bottom.rkt")

(provide (all-from-out "bottom.rkt")
         (prefix-out sf: (combine-out Value Maybe-Value Computation value? run)))

;; ===================================================================================================
;; Exported semantics

(begin-for-syntax
  (provide standard-float)
  
  (define standard-float
    (make-immutable-hasheq
     (list
      (cons 'language-name "standard float")
      (cons 'value? #'value?)
      ;; Expression combinators
      (cons 'comp #'comp)
      (cons 'pair #'pair)
      (cons 'ifte #'ifte)
      (cons 'const #'const)
      (cons 'id #'id)
      ;; Pair and list computations
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
      (cons 'neg #'neg)
      (cons 'sub #'sub)
      (cons 'mul #'mul)
      (cons 'rcp #'rcp)
      (cons 'div #'div)
      ))))

;; ===================================================================================================
;; Types

(define-type Value (U Boolean Flonum Null (Pair Value Value)))
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
    [(cons (? flonum? x) (? flonum? y))  (< x y)]
    [_  bottom]))

(: le Computation)
(define (le a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (<= x y)]
    [_  bottom]))

(: gt Computation)
(define (gt a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (> x y)]
    [_  bottom]))

(: ge Computation)
(define (ge a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (>= x y)]
    [_  bottom]))

(: eq Computation)
(define (eq a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (= x y)]
    [_  bottom]))

;; ===================================================================================================
;; Arithmetic computations

(: add Computation)
(define (add a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (+ x y)]
    [_  bottom]))

(: neg Computation)
(define (neg a)
  (if (flonum? a) (- a) bottom))

(: sub Computation)
(define (sub a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (- x y)]
    [_  bottom]))

(: mul Computation)
(define (mul a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (* x y)]
    [_  bottom]))

(: rcp Computation)
(define (rcp a)
  (if (and (flonum? a) (not (zero? a))) (/ 1.0 a) bottom))

(: div Computation)
(define (div a)
  (match a
    [(cons (? flonum? x) (? flonum? y))  (if (zero? y) bottom (/ x y))]
    [_  bottom]))
