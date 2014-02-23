#lang typed/racket

(require math/distributions
         math/flonum)

(provide (all-defined-out))

(define-syntax-rule (define-singleton-type type name pred?)
  (begin
    (define name 'name)
    (define-type type 'name)
    (define (pred? x) (eq? x name))))

;; ===================================================================================================
;; Indexes into infinite binary trees

(define-type J (Listof Boolean))

(: left (J -> J))
(define (left j) (cons #t j))

(: right (J -> J))
(define (right j) (cons #f j))

(define j0 null)

;; ===================================================================================================
;; Monomorphic set types and operations

(define-singleton-type Univ-Set univ-set univ-set?)
(define-singleton-type Empty-Set empty-set empty-set?)

(define-singleton-type Null-Set null-set null-set?)

(define-type Set (U Empty-Set Nonempty-Set))
(define-type Nonempty-Set
  (U Univ-Set Real-Set Bool-Set Pair-Set Null-Set Omega-Set))

(define-type Value
  (Rec Value (U Flonum Boolean (Pair Value Value) Null Omega-Val)))

(: set-intersect (Set Set -> Set))
(define (set-intersect A B)
  (cond [(and (real-set? A)  (real-set? B))   (real-set-intersect A B)]
        [(and (bool-set? A)  (bool-set? B))   (bool-set-intersect A B)]
        [(and (pair-set? A)  (pair-set? B))   (pair-set-intersect A B)]
        [(and (null-set? A)  (null-set? B))   null-set]
        [(and (omega-set? A) (omega-set? B))  (omega-set-intersect A B)]
        [(or  (empty-set? A) (empty-set? B))  empty-set]
        [(univ-set? A)  B]
        [(univ-set? B)  A]
        [else  empty-set]))

(: set-join (case-> (Nonempty-Set Nonempty-Set -> Nonempty-Set)
                    (Set Set -> Set)))
(define (set-join A B)
  (cond [(and (real-set? A)  (real-set? B))   (real-set-join A B)]
        [(and (bool-set? A)  (bool-set? B))   (bool-set-join A B)]
        [(and (pair-set? A)  (pair-set? B))   (pair-set-join A B)]
        [(and (null-set? A)  (null-set? B))   null-set]
        [(and (omega-set? A) (omega-set? B))  (omega-set-join A B)]
        [(or  (univ-set? A)  (univ-set? B))   univ-set]
        [(empty-set? A)  B]
        [(empty-set? B)  A]
        [else  univ-set
               #;
               (raise-argument-error 'set-join "compatible set" 1 A B)]))

(: set-member? (Value Set -> Boolean))
(define (set-member? a A)
  (cond [(and (real-set? A)  (flonum? a))     (real-set-member? a A)]
        [(and (bool-set? A)  (boolean? a))    (bool-set-member? a A)]
        [(and (pair-set? A)  (pair? a))       (pair-set-member? a A)]
        [(and (null-set? A)  (null? a))       #t]
        [(and (omega-set? A) (omega-val? a))  (omega-set-member? a A)]
        [(univ-set? A)  #t]
        [else  #f]))

(: set-singleton (Value -> Nonempty-Set))
(define (set-singleton a)
  (cond [(flonum? a)    (real-set-singleton a)]
        [(boolean? a)   (bool-set-singleton a)]
        [(pair? a)      (pair-set-singleton a)]
        [(null? a)      null-set]
        [(omega-val? a)
         (raise-argument-error 'set-singleton "Value, not Omega-Val" a)]))

(: set-proj-fst (Set -> Set))
(define (set-proj-fst A)
  (cond [(pair-set? A)  (Pair-Set-fst A)]
        [(univ-set? A)  univ-set]
        [else  empty-set]))

(: set-proj-snd (Set -> Set))
(define (set-proj-snd A)
  (cond [(pair-set? A)  (Pair-Set-snd A)]
        [(univ-set? A)  univ-set]
        [else  empty-set]))

(: set-project (J Set -> Set))
(define (set-project j A)
  (cond [(omega-set? A)  (omega-set-project j A)]
        [(univ-set? A)   (omega-set-project j univ-omega-set)]
        [else  empty-set]))

(: set-unproject (J Set Set -> Set))
(define (set-unproject j A B)
  (cond [(and (omega-set? A) (univ-set? B))  A]
        [(and (omega-set? A) (real-set? B))  (omega-set-unproject j A B)]
        [(and (univ-set? A)  (univ-set? B))  univ-omega-set]
        [(and (univ-set? A)  (real-set? B))  (omega-set-unproject j univ-omega-set B)]
        [else  empty-set]))

;; ===================================================================================================
;; Booleans

(define-singleton-type True-Set true-set true-set?)
(define-singleton-type False-Set false-set false-set?)
(define-singleton-type Bools-Set bools-set bools-set?)

(define-type Bool-Set (U True-Set False-Set Bools-Set))
(define-predicate bool-set? Bool-Set)

(: bool-set-intersect (Bool-Set Bool-Set -> (U Empty-Set Bool-Set)))
(define (bool-set-intersect A B)
  (cond [(bools-set? A)  B]
        [(bools-set? B)  A]
        [(eq? A B)  A]
        [else  empty-set]))

(: bool-set-join (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-join A B)
  (cond [(or (bools-set? A) (bools-set? B))  bools-set]
        [(eq? A B)  A]
        [else  bools-set]))

(: bool-set-member? (Boolean Bool-Set -> Boolean))
(define (bool-set-member? a A)
  (cond [(bools-set? A)   #t]
        [else  (eq? (true-set? A) a)]))

(: bool-set-singleton (Boolean -> Bool-Set))
(define (bool-set-singleton a)
  (if a true-set false-set))

;; ===================================================================================================
;; Pairs

(struct: Pair-Set ([fst : Nonempty-Set] [snd : Nonempty-Set])
  #:transparent)
(define pair-set? Pair-Set?)

(: set-prod (Set Set -> (U Empty-Set Pair-Set)))
(define (set-prod A1 A2)
  (if (or (empty-set? A1) (empty-set? A2))
      empty-set
      (Pair-Set A1 A2)))

(: pair-set-intersect (Pair-Set Pair-Set -> (U Empty-Set Pair-Set)))
(define (pair-set-intersect A B)
  (match* (A B)
    [((Pair-Set A1 A2) (Pair-Set B1 B2))
     (set-prod (set-intersect A1 B1)
               (set-intersect A2 B2))]))

(: pair-set-join (Pair-Set Pair-Set -> Pair-Set))
(define (pair-set-join A B)
  (match* (A B)
    [((Pair-Set A1 A2) (Pair-Set B1 B2))
     (Pair-Set (set-join A1 B1)
               (set-join A2 B2))]))

(: pair-set-member? ((Pair Value Value) Pair-Set -> Boolean))
(define (pair-set-member? a A)
  (match* (a A)
    [((cons a1 a2) (Pair-Set A1 A2))
     (and (set-member? a1 A1)
          (set-member? a2 A2))]))

(: pair-set-singleton ((Pair Value Value) -> Pair-Set))
(define (pair-set-singleton a)
  (Pair-Set (set-singleton (car a))
            (set-singleton (cdr a))))

;; ===================================================================================================
;; Intervals

(struct: Real-Set ([min : Flonum] [max : Flonum] [min? : Boolean] [max? : Boolean])
  #:transparent)
(define real-set? Real-Set?)

(define unit-interval (Real-Set 0.0 1.0 #t #t))

(: interval (case-> (Flonum Flonum -> (U Empty-Set Real-Set))
                    (Flonum Flonum Boolean Boolean -> (U Empty-Set Real-Set))))
(define (interval a1 a2 [a1? #t] [a2? #t])
  (cond [(not (<= -inf.0 a1 +inf.0))  (raise-argument-error 'interval "Flonum, not +nan.0" 0 a1 a2)]
        [(not (<= -inf.0 a2 +inf.0))  (raise-argument-error 'interval "Flonum, not +nan.0" 1 a1 a2)]
        [(or (> a1 a2) (and (= a1 a2) (not (and a1? a2?))))  empty-set]
        [(= a1 -inf.0)  (if (= a2 -inf.0) empty-set (Real-Set -inf.0 a2 #f (and (< a2 +inf.0) a2?)))]
        [(= a2 +inf.0)  (if (= a1 +inf.0) empty-set (Real-Set a1 +inf.0 a1? #f))]
        [else  (Real-Set a1 a2 a1? a2?)]))

(: real-set-intersect (Real-Set Real-Set -> (U Empty-Set Real-Set)))
(define (real-set-intersect A B)
  (match-define (Real-Set a1 a2 a1? a2?) A)
  (match-define (Real-Set b1 b2 b1? b2?) B)
  (define-values (c1 c1?)
    (cond [(a1 . > . b1)  (values a1 a1?)]
          [(a1 . < . b1)  (values b1 b1?)]
          [else           (values a1 (and a1? b1?))]))
  (define-values (c2 c2?)
    (cond [(a2 . > . b2)  (values b2 b2?)]
          [(a2 . < . b2)  (values a2 a2?)]
          [else           (values a2 (and a2? b2?))]))
  (interval c1 c2 c1? c2?))

(: real-set-join (Real-Set Real-Set -> Real-Set))
(define (real-set-join A B)
  (match-define (Real-Set a1 a2 a1? a2?) A)
  (match-define (Real-Set b1 b2 b1? b2?) B)
  (define-values (c1 c1?)
    (cond [(a1 . < . b1)  (values a1 a1?)]
          [(a1 . > . b1)  (values b1 b1?)]
          [else           (values a1 (or a1? b1?))]))
  (define-values (c2 c2?)
    (cond [(a2 . < . b2)  (values b2 b2?)]
          [(a2 . > . b2)  (values a2 a2?)]
          [else           (values a2 (or a2? b2?))]))
  (Real-Set c1 c2 c1? c2?))

(: real-set-member? (Real Real-Set -> Boolean))
(define (real-set-member? a A)
  (match-define (Real-Set a1 a2 a1? a2?) A)
  (cond [(not (< -inf.0 a +inf.0))
         (raise-argument-error 'real-set-member? "rational?" 0 a A)]
        [(< a1 a a2)  #t]
        [(and (= a a1) a1?)  #t]
        [(and (= a a2) a2?)  #t]
        [else  #f]))

(: real-set-singleton (Real -> Real-Set))
(define (real-set-singleton a)
  (define b (fl a))
  (cond [(not (< -inf.0 a +inf.0))
         (raise-argument-error 'real-set-singleton "rational?" a)]
        [(< b a)  (Real-Set b (flnext b) #f #f)]
        [(< a b)  (Real-Set (flprev b) b #f #f)]
        [else     (Real-Set b b #t #t)]))

(: real-set-sample (Real-Set -> Real))
(define (real-set-sample A)
  (match-define (Real-Set a1 a2 a1? a2?) A)
  (define dist (uniform-dist a1 a2))
  (let loop ()
    (define a (sample dist))
    (cond [(and (= a a1) (not a1?))  (loop)]
          [(and (= a a2) (not a2?))  (loop)]
          [else  a])))

;; ===================================================================================================
;; Sets of countable vectors with at most finitely many restricted axes

;; ---------------------------------------------------------------------------------------------------

(struct: Omega-Val ([value : (Promise Real)]
                    [left : (Promise Omega-Val)]
                    [right : (Promise Omega-Val)])
  #:transparent)

(define omega-val? Omega-Val?)

(: omega-val-pi (All (X) (J Omega-Val -> Real)))
(define (omega-val-pi j z)
  (let loop ([j (reverse j)] [z z])
    (match-define (Omega-Val a l r) z)
    (cond [(null? j)  (force a)]
          [(first j)  (loop (rest j) (force l))]
          [else       (loop (rest j) (force r))])))

;; ---------------------------------------------------------------------------------------------------

(define-singleton-type Univ-Omega-Set univ-omega-set univ-omega-set?)

(struct: Omega-Node ([axis : Real-Set] [left : Omega-Set] [right : Omega-Set])
  #:transparent)

(define-type Omega-Set (U Univ-Omega-Set Omega-Node))
(define-predicate omega-set? Omega-Set)

(: omega-set-node ((U Empty-Set Real-Set)
                   (U Empty-Set Omega-Set)
                   (U Empty-Set Omega-Set)
                   -> (U Empty-Set Omega-Set)))
(define (omega-set-node A L R)
  (if (or (empty-set? A) (empty-set? L) (empty-set? R))
      empty-set
      (Omega-Node A L R)))

(: omega-set-intersect (Omega-Set Omega-Set -> (U Empty-Set Omega-Set)))
(define (omega-set-intersect Z1 Z2)
  (match* (Z1 Z2)
    [((? univ-omega-set?) Z2)  Z2]
    [(Z1 (? univ-omega-set?))  Z1]
    [((Omega-Node A1 L1 R1) (Omega-Node A2 L2 R2))
     (omega-set-node (real-set-intersect A1 A2)
                     (omega-set-intersect L1 L2)
                     (omega-set-intersect R1 R2))]))

(: omega-set-join (Omega-Set Omega-Set -> Omega-Set))
(define (omega-set-join Z1 Z2)
  (match* (Z1 Z2)
    [((? univ-omega-set?) Z2)  univ-omega-set]
    [(Z1 (? univ-omega-set?))  univ-omega-set]
    [((Omega-Node A1 L1 R1) (Omega-Node A2 L2 R2))
     (Omega-Node (real-set-join A1 A2)
                 (omega-set-join L1 L2)
                 (omega-set-join R1 R2))]))

(: omega-set-member? (Omega-Val Omega-Set -> Boolean))
(define (omega-set-member? z Z)
  (match* (z Z)
    [(z (? univ-omega-set?))  #t]
    [((Omega-Val a l r) (Omega-Node A L R))
     (and (real-set-member? (force a) A)
          (omega-set-member? (force l) L)
          (omega-set-member? (force r) R))]))

(: omega-set-project (J Omega-Set -> Real-Set))
(define (omega-set-project j Z)
  (let loop ([j (reverse j)] [Z Z])
    (match Z
      [(? univ-omega-set?)  unit-interval]
      [(Omega-Node A L R)
       (cond [(null? j)  A]
             [(first j)  (loop (rest j) L)]
             [else       (loop (rest j) R)])])))

(define univ-omega-node
  (Omega-Node unit-interval univ-omega-set univ-omega-set))

(: omega-set-unproject (J Omega-Set Real-Set -> (U Empty-Set Omega-Set)))
(define (omega-set-unproject j Z B)
  (let loop ([j (reverse j)] [Z Z])
    (match Z
      [(? univ-omega-set?)  (loop j univ-omega-node)]
      [(Omega-Node A L R)
       (cond [(null? j)  (omega-set-node (real-set-intersect A B) L R)]
             [(first j)  (omega-set-node A (loop (rest j) L) R)]
             [else       (omega-set-node A L (loop (rest j) R))])])))

(: omega-set-sample (Omega-Set -> Omega-Val))
(define (omega-set-sample Z)
  (match Z
    [(? univ-omega-set?)
     (omega-set-sample univ-omega-node)]
    [(Omega-Node A L R)
     (Omega-Val (delay (real-set-sample A))
                (delay (omega-set-sample L))
                (delay (omega-set-sample R)))]))
