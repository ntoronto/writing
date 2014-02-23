#lang typed/racket

(require math/flonum
         "sets.rkt"
         "flops.rkt")

(define reals (Real-Set -inf.0 +inf.0 #f #f))
(define positive-reals (Real-Set 0.0 +inf.0 #f #f))
(define negative-reals (Real-Set -inf.0 0.0 #f #f))
(define nonnegative-reals (Real-Set 0.0 +inf.0 #t #f))
(define nonpositive-reals (Real-Set -inf.0 0.0 #f #t))

(: real-image (Boolean (Flonum -> Flonum) (Flonum -> Flonum) Real-Set -> Real-Set))
(define (real-image inc? g/rndd g/rndu A)
  (match-define (Real-Set a1 a2 a1? a2?) A)
  (cond [inc?  (Real-Set (g/rndd a1) (g/rndu a2) a1? a2?)]
        [else  (Real-Set (g/rndd a2) (g/rndu a1) a2? a1?)]))

(: flneg-sqrt/rndd (Flonum -> Flonum))
(define (flneg-sqrt/rndd x) (- (flsqrt/rndu x)))

(: flneg-sqrt/rndu (Flonum -> Flonum))
(define (flneg-sqrt/rndu x) (- (flsqrt/rndd x)))

;; ===================================================================================================

(struct: Bijection ([inc? : Boolean]
                    [domain : Real-Set]
                    [range : Real-Set]
                    [gb/rndd : (Flonum -> Flonum)]
                    [gb/rndu : (Flonum -> Flonum)]
                    [ga/rndd : (Flonum -> Flonum)]
                    [ga/rndu : (Flonum -> Flonum)])
  #:transparent)

(: bijection-inverse (Bijection -> Bijection))
(define (bijection-inverse g)
  (match-define (Bijection inc? X Y gb/rndd gb/rndu ga/rndd ga/rndu) g)
  (Bijection inc? Y X ga/rndd ga/rndu gb/rndd gb/rndu))

(: bijection-image (Bijection Real-Set -> (U Empty-Set Real-Set)))
(define (bijection-image g A)
  (match-define (Bijection inc? X Y gb/rndd gb/rndu _ _) g)
  (let ([A  (real-set-intersect A X)])
    (if (empty-set? A)
        empty-set
        (real-set-intersect Y (real-image inc? gb/rndd gb/rndu A)))))

(: bijection-preimage (Bijection Real-Set Real-Set -> (U Empty-Set Real-Set)))
(define (bijection-preimage g A B)
  (match-define (Bijection inc? X Y _ _ ga/rndd ga/rndu) g)
  (let ([A  (real-set-intersect A X)]
        [B  (real-set-intersect B Y)])
    (if (or (empty-set? A) (empty-set? B))
        empty-set
        (real-set-intersect A (real-image inc? ga/rndd ga/rndu B)))))

;; ===================================================================================================

(: zeros+ ((Flonum -> Flonum) -> (Flonum -> Flonum)))
(define ((zeros+ f) x)
  (f (if (= x 0.0) +0.0 x)))

(: zeros- ((Flonum -> Flonum) -> (Flonum -> Flonum)))
(define ((zeros- f) x)
  (f (if (= x 0.0) -0.0 x)))


(define exp-bij
  (Bijection #t reals positive-reals
             flexp/rndd flexp/rndu
             fllog/rndd fllog/rndu))

(define log-bij
  (bijection-inverse exp-bij))

(define flsqr (λ: ([x : Flonum]) (* x x)))

(define pos-sqr-bij
  (Bijection #t nonnegative-reals nonnegative-reals
             flsqr/rndd flsqr/rndu
             flsqrt/rndd flsqrt/rndu))

(define neg-sqr-bij
  (Bijection #f negative-reals positive-reals
             flsqr/rndd flsqr/rndu
             flneg-sqrt/rndd flneg-sqrt/rndu))

(define sqrt-bij
  (bijection-inverse pos-sqr-bij))

(bijection-preimage pos-sqr-bij (Real-Set 1.0 2.0 #t #f) (Real-Set 2.0 7.0 #t #t))

(define pos-recip-bij
  (Bijection #f positive-reals positive-reals
             ;flrecip/rndd flrecip/rndu
             ;flrecip/rndd flrecip/rndu
             (zeros+ flrecip/rndd) (zeros+ flrecip/rndu)
             (zeros+ flrecip/rndd) (zeros+ flrecip/rndu)
             ))

(define neg-recip-bij
  (Bijection #f negative-reals negative-reals
             (zeros- flrecip/rndd) (zeros- flrecip/rndu)
             (zeros- flrecip/rndd) (zeros- flrecip/rndu)))

(bijection-image pos-recip-bij (Real-Set -0.0 1.0 #t #f))
