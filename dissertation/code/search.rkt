#lang typed/racket

(require math/flonum
         math/distributions
         math/statistics)

(provide (all-defined-out))
#|
(: sample-index ((Listof Flonum) -> Index))
(define (sample-index qs)
  (sample ((inst discrete-dist Index) (build-list (length qs) (λ: ([n : Index]) n)) qs)))

(: remove-index (All (A) ((Listof A) Index -> (Listof A))))
(define (remove-index xs i)
  (cond [(= i 0)  (rest xs)]
        [else  (remove-index (rest xs) (- i 1))]))

(: normalize-probs ((Listof Flonum) -> (Listof Flonum)))
(define (normalize-probs ps)
  (define s (flsum ps))
  (map (λ: ([p : Flonum]) (/ p s)) ps))

(: list-set (All (A) ((Listof A) Index A -> (Listof A))))
(define (list-set xs i x)
  (cond [(= i 0)  (cons x (rest xs))]
        [else  (cons (first xs) (list-set (rest xs) (- i 1) x))]))
|#
;; ===================================================================================================
;; Search tree types

(struct: search-fail () #:transparent)

(struct: (X) search-succ ([value : X]) #:transparent)

(struct: (X) search-node ([left-prob : Flonum]
                          [right-prob : Flonum]
                          [left-child : (Promise (Search-Tree X))]
                          [right-child : (Promise (Search-Tree X))])
  #:transparent)

(define-type (Search-Leaf X) (U search-fail (search-succ X)))
(define-type (Search-Tree X) (U (Search-Leaf X) (search-node X)))

;; ===================================================================================================
;; Nonadaptive, independent sampling

(: sample-search-tree-once (All (X) (Flonum (Search-Tree X) -> (Values Flonum (Search-Leaf X)))))
(define (sample-search-tree-once p t)
  (match t
    [(search-node ql qr cl cr)
     (if ((random) . < . ql)
         (sample-search-tree-once (* p ql) (force cl))
         (sample-search-tree-once (* p qr) (force cr)))]
    [(and (search-succ v) t)  (values p t)]
    [(and (search-fail) t)    (values p t)]))

;; ===================================================================================================
;; Adaptive search

(: sample-search-tree (All (X) (Flonum (Search-Tree X) -> (Values Flonum
                                                                  (Search-Leaf X)
                                                                  Flonum
                                                                  (Search-Tree X)))))
(define (sample-search-tree p t)
  (match t
    [(search-node ql qr cl cr)
     (cond
       [((random) . < . ql)
        (define-values (leaf-p leaf-v child-pq new-c)
          (sample-search-tree (* p ql) (force cl)))
        (define child-q (/ child-pq p))
        (define new-t
          (cond [(zero? child-q)  (force cr)]
                [else  (define s (+ qr child-q))
                       (search-node (/ child-q s) (/ qr s) (delay new-c) cr)]))
        (values leaf-p leaf-v (* p (- 1.0 (- ql child-q))) new-t)]
       [else
        (define-values (leaf-p leaf-v child-pq new-c)
          (sample-search-tree (* p qr) (force cr)))
        (define child-q (/ child-pq p))
        (define new-t
          (cond [(zero? child-q)  (force cl)]
                [else  (define s (+ ql child-q))
                       (search-node (/ ql s) (/ child-q s) cl (delay new-c))]))
        (values leaf-p leaf-v (* p (- 1.0 (- qr child-q))) new-t)])]
    [(and (search-succ v) t)  (values p t p t)]
    [(and (search-fail) t)    (values p t 0.0 t)]))

(: sample-search-tree* (All (X) (Flonum (Search-Tree X) Integer -> (Values (Listof Flonum)
                                                                           (Listof X)
                                                                           Flonum
                                                                           (Search-Tree X)))))
;; Samples multiple leaves using `sample-search-tree'
(define (sample-search-tree* q t n)
  (cond
    [(not (index? n))  (raise-argument-error 'sample-search-tree* "Index" 1 t n)]
    [else
     (define: ps : (Listof Flonum)  empty)
     (define: ts : (Listof X)  empty)
     (let: loop ([i : Positive-Fixnum  1] [ps ps] [ts ts] [q q] [t t])
       (cond [(and (i . <= . n) (q . > . 0.0))
              (let-values ([(leaf-p leaf-t q t)  (sample-search-tree q t)])
                (let-values ([(ps ts)  (match leaf-t
                                         [(search-succ v)  (values (cons leaf-p ps)
                                                                   (cons v ts))]
                                         [_  (values ps ts)])])
                  (loop (+ i 1) ps ts q t)))]
             [else
              (values ps ts q t)]))]))

(: t (Search-Tree (U 'a 'b)))
(define t
  (search-node 0.4
               0.6
               (delay (search-node 0.1
                                   0.9
                                   (delay (search-succ 'a))
                                   (delay (search-succ 'b))))
               (delay (search-fail))))

(sample-search-tree-once 1.0 t)

(: do-sample (All (X) ((Search-Tree X) Integer -> (Discrete-Dist X))))
(define (do-sample t n)
  (define-values (ps ts q _) (sample-search-tree* 1.0 t n))
  (let-values ([(ts ps)  (count-samples ts)])
    (discrete-dist ts ps)))
