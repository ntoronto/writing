#lang typed/racket

(require math/flonum
         math/distributions
         math/statistics)

(provide (all-defined-out))

;; ===================================================================================================
;; Search tree types

(struct: (X) search-fail ([value : X]) #:transparent)
(struct: (X) search-succ ([value : X]) #:transparent)
(struct: (X) search-node ([left-prob : Flonum]
                          [left-child : (Search-Tree X)]
                          [right-prob : Flonum]
                          [right-child : (Search-Tree X)])
  #:transparent)

(define-type (Search-Tree X) (U (search-fail X) (search-succ X) (search-node X)))

;; ===================================================================================================
;; Nonadaptive, independent sampling

(: sample-search-tree-once (All (X) (Flonum (Search-Tree X) -> (Values Flonum X))))
(define (sample-search-tree-once p t)
  (match t
    [(search-node ql cl qr cr)
     (if ((random) . < . ql)
         (sample-search-tree-once (* p ql) cl)
         (sample-search-tree-once (* p qr) cr))]
    [(search-succ x)  (values p x)]
    [(search-fail x)  (values p x)]))

;; ===================================================================================================
;; Adaptive search

(: adapt-search-tree (All (X) (Flonum
                               Flonum (Search-Tree X)
                               Flonum (Search-Tree X)
                               -> (Pair Flonum (Search-Tree X)))))
(define (adapt-search-tree p q0 c0 q1 c1)
  (cond [(zero? q0)  (cons (* p q1) c1)]
        [(zero? q1)  (cons (* p q0) c0)]
        [else  (define q01 (+ q0 q1))
               (cons (* p q01) (search-node (/ q0 q01) c0 (/ q1 q01) c1))]))

(: sample-search-tree (All (X) ((Pair Flonum (Search-Tree X))
                                -> (Pair (Pair Flonum X) (Pair Flonum (Search-Tree X))))))
(define sample-search-tree
  (match-lambda
    [(cons p_t (search-node p_l* c_l* p_r* c_r*))
     (define-values (p_l c_l p_r c_r)
       (if ((random) . < . p_l*)
           (values p_l* c_l* p_r* c_r*)
           (values p_r* c_r* p_l* c_l*)))
     
     (match-define (cons px (cons new-p_t new-c_l)) (sample-search-tree (cons (* p_t p_l) c_l)))
     (cons px (adapt-search-tree p_t (/ new-p_t p_t) new-c_l p_r c_r))]
    [(cons p_t (and (search-succ x) t))  (cons (cons p_t x) (cons p_t t))]
    [(cons p_t (and (search-fail x) t))  (cons (cons p_t x) (cons 0.0 t))]))

(: sample-search-tree* (All (X) ((Pair Flonum (Search-Tree X))
                                 Integer -> (Values (Listof (Pair Flonum X))
                                                    (Pair Flonum (Search-Tree X))))))
;; Samples multiple leaves using `sample-search-tree'
(define (sample-search-tree* pt n)
  (cond
    [(not (index? n))  (raise-argument-error 'sample-search-tree* "Index" 1 pt n)]
    [else
     (let: loop ([i : Positive-Fixnum  1] [pxs : (Listof (Pair Flonum X))  empty] [pt pt])
       (cond [(and (i . <= . n) ((car pt) . > . 0.0))
              (match-let ([(cons px pt)  (sample-search-tree pt)])
                (loop (+ i 1) (cons px pxs) pt))]
             [else
              (values pxs pt)]))]))

(: do-sample (All (X) ((Search-Tree X) Integer -> (Discrete-Dist X))))
(define (do-sample t n)
  (define-values (pxs pt) (sample-search-tree* (cons 1.0 t) n))
  (printf "pt = ~v~n~n" pt)
  (let-values ([(xs ps)  (count-samples (map (inst cdr Flonum X) pxs))])
    (discrete-dist xs ps)))

(define t
  (search-node 0.4 (search-node 0.1 (search-succ 'a)
                                0.9 (search-fail 'bad))
               0.6 (search-node 0.2 (search-fail 'bad)
                                0.8 (search-succ 'c))))

(printf "pt = ~v~n~n"
        (cons 0.88 (search-node 0.5454545454545454 (search-succ 'c)
                                0.45454545454545453 (search-node 0.9 (search-succ 'b)
                                                                 0.1 (search-succ 'a)))))

(do-sample t 10000)
