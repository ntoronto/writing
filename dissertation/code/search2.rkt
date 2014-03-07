#lang typed/racket

(require math/flonum
         math/distributions
         math/statistics)

(provide (all-defined-out))

;; ===================================================================================================
;; Search tree types

(struct: (X) search-fail ([value : X]) #:transparent)
(struct: (X) search-succ ([value : X]) #:transparent)
(struct: (X) search-node ([left : (Pair Flonum (Search-Tree X))]
                          [right : (Pair Flonum (Search-Tree X))])
  #:transparent)

(define-type (Search-Tree X) (U (search-fail X) (search-succ X) (search-node X)))

;; ===================================================================================================
;; Nonadaptive, independent sampling

(: sample-search-tree-once (All (X) ((Pair Flonum (Search-Tree X)) -> (Pair Flonum X))))
(define sample-search-tree-once
  (match-lambda
    [(cons p (search-node (cons ql cl) (cons qr cr)))
     (if ((random) . < . ql)
         (sample-search-tree-once (cons (* p ql) cl))
         (sample-search-tree-once (cons (* p qr) cr)))]
    [(cons p (search-succ x))  (cons p x)]
    [(cons p (search-fail x))  (cons p x)]))

;; ===================================================================================================
;; Adaptive search

(: make-search-node (All (X) ((Pair Flonum (Search-Tree X))
                              (Pair Flonum (Search-Tree X))
                              -> (Pair Flonum (Search-Tree X)))))
(define make-search-node
  (match-lambda**
    [((cons 0.0 _) pt)  pt]
    [(pt (cons 0.0 _))  pt]
    [((cons q0 c0) (cons q1 c1))
     (define q01 (+ q0 q1))  ; Always <= 1
     (cons q01 (search-node (cons (/ q0 q01) c0) (cons (/ q1 q01) c1)))]))

(: sample-search-tree (All (X) ((Pair Flonum (Search-Tree X))
                                -> (Pair (Pair Flonum X)
                                         (Pair Flonum (Search-Tree X))))))
(define sample-search-tree
  (match-lambda
    [(cons p (search-node (cons ql cl) (cons qr cr)))
     (match-letrec
         ([(search-node (cons q0 c0) (cons q1 c1))  (if ((random) . < . ql)
                                                        (search-node (cons ql cl) (cons qr cr))
                                                        (search-node (cons qr cr) (cons ql cl)))]
          [(cons px (cons new-q0 new-c0))  (sample-search-tree (cons (* p q0) c0))])
       (cons px (make-search-node (cons (* new-q0 q0) new-c0) (cons q1 c1))))]
    [(cons p (and (search-succ x) t))  (cons (cons p x) (cons 1.0 t))]
    [(cons p (and (search-fail x) t))  (cons (cons p x) (cons 0.0 t))]))

(: sample-search-tree+ (All (X) ((Pair Flonum (Search-Tree X))
                                 -> (Pair (Pair Flonum X)
                                          (Pair Flonum (Search-Tree X))))))
(define sample-search-tree+
  (match-lambda
    [(cons p t)
     (match-let ([(cons px (cons new-p t))  (sample-search-tree (cons p t))])
       (cons px (cons (* new-p p) t)))]))

(: sample-search-tree* (All (X) ((Pair Flonum (Search-Tree X))
                                 Integer -> (Values (Listof (Pair Flonum X))
                                                    (Pair Flonum (Search-Tree X))))))
;; Samples multiple leaves using `sample-search-tree'
(define (sample-search-tree* pt n)
  (let: loop ([n n] [pxs : (Listof (Pair Flonum X))  empty] [pt pt])
    (cond [(and (n . > . 0) ((car pt) . > . 0.0))
           (match-let ([(cons px pt)  (sample-search-tree+ pt)])
             (loop (- n 1) (cons px pxs) pt))]
          [else
           (values pxs pt)])))

(: do-sample (All (X) ((Pair Flonum (Search-Tree X)) Integer -> (Discrete-Dist X))))
(define (do-sample pt n)
  (let-values ([(pxs pt)  (sample-search-tree* pt n)])
    (printf "pt = ~v~n~n" pt)
    (let-values ([(xs ps)  (count-samples (map (inst cdr Flonum X) pxs))])
      (discrete-dist xs ps))))

(define pt
  (cons 1.0 (search-node (cons 0.4 (search-node (cons 0.1 (search-fail 'a))
                                                (cons 0.9 (search-succ 'b))))
                         (cons 0.6 (search-node (cons 0.2 (search-fail 'd))
                                                (cons 0.8 (search-succ 'c)))))))

(printf "pt = ~v~n~n"
        (cons 0.88 (search-node (cons 0.5454545454545454
                                      (search-succ 'c))
                                (cons 0.45454545454545453
                                      (search-node (cons 0.9 (search-succ 'b))
                                                   (cons 0.1 (search-succ 'a)))))))

(do-sample pt 10000)
