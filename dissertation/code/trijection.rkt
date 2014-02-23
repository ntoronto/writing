#lang typed/racket

(require "sets.rkt"
         "flops.rkt")

(provide (all-defined-out))

(require/typed
 racket/bool
 [xor  (Boolean Boolean -> Boolean)])

;; ===================================================================================================
;; Image computation for uniformly strictly monotone R x R -> R functions

(: real2d-image (Boolean Boolean
                         (Flonum Flonum -> Flonum)
                         (Flonum Flonum -> Flonum)
                         Real-Set Real-Set -> Real-Set))
(define (real2d-image inc1? inc2? g/rndd g/rndu A B)
  (define-values (a1 a2 a1? a2?)
    (match-let ([(Real-Set a1 a2 a1? a2?)  A])
      (cond [inc1?  (values a1 a2 a1? a2?)]
            [else   (values a2 a1 a2? a1?)])))
  (define-values (b1 b2 b1? b2?)
    (match-let ([(Real-Set b1 b2 b1? b2?)  B])
      (cond [inc2?  (values b1 b2 b1? b2?)]
            [else   (values b2 b1 b2? b1?)])))
  (Real-Set (g/rndd a1 b1) (g/rndu a2 b2) (and a1? b1?) (and a2? b2?)))

(: first-inverse-directions (Boolean Boolean -> (Values Boolean Boolean)))
(define (first-inverse-directions inc1? inc2?)
  (values (xor inc1? inc2?) inc1?))

;; ===================================================================================================
;; Trijections (axis-invertible functions and their axial inverses)

(struct: Trijection
  ([inc1? : Boolean] [inc2? : Boolean]
   [domain1 : Real-Set] [domain2 : Real-Set] [range : Real-Set]
   [gc/rndd : (Flonum Flonum -> Flonum)] [gc/rndu : (Flonum Flonum -> Flonum)]
   [ga/rndd : (Flonum Flonum -> Flonum)] [ga/rndu : (Flonum Flonum -> Flonum)]
   [gb/rndd : (Flonum Flonum -> Flonum)] [gb/rndu : (Flonum Flonum -> Flonum)])
  #:transparent)

(: trijection-first-inverse (Trijection -> Trijection))
(define (trijection-first-inverse g)
  (match-define (Trijection gc-inc1? gc-inc2? X Y Z
                            gc/rndd gc/rndu ga/rndd ga/rndu gb/rndd gb/rndu) g)
  (define ga-inc1? (xor gc-inc1? gc-inc2?))
  (define ga-inc2? gc-inc1?)
  (Trijection ga-inc1? ga-inc2? Y Z X ga/rndd ga/rndu gb/rndd gb/rndu gc/rndd gc/rndu))

(: trijection-image (Trijection Real-Set Real-Set -> (U Empty-Set Real-Set)))
(define (trijection-image g A B)
  (match-define (Trijection inc1? inc2? X Y Z gc/rndd gc/rndu _ _ _ _) g)
  (let ([A  (real-set-intersect A X)]
        [B  (real-set-intersect B Y)])
    (if (or (empty-set? A) (empty-set? B))
        empty-set
        (real-set-intersect Z (real2d-image inc1? inc2? gc/rndd gc/rndu A B)))))

(: trijection-preimage (Trijection Real-Set Real-Set Real-Set
                         -> (Values (U Empty-Set Real-Set)
                                    (U Empty-Set Real-Set))))
(define (trijection-preimage g A B C)
  (match-define (Trijection gc-inc1? gc-inc2? X Y Z _ _ ga/rndd ga/rndu gb/rndd gb/rndu) g)
  (define ga-inc1? (xor gc-inc1? gc-inc2?))
  (define ga-inc2? gc-inc1?)
  (define gb-inc1? (xor ga-inc1? ga-inc2?))
  (define gb-inc2? ga-inc1?)
  (let ([A  (real-set-intersect A X)]
        [B  (real-set-intersect B Y)]
        [C  (real-set-intersect C Z)])
    (if (or (empty-set? A) (empty-set? B) (empty-set? C))
        (values empty-set empty-set)
        (values (real-set-intersect A (real2d-image ga-inc1? ga-inc2? ga/rndd ga/rndu B C))
                (real-set-intersect B (real2d-image gb-inc1? gb-inc2? gb/rndd gb/rndu C A))))))
#|
;; ===================================================================================================
;; Some trijections

(: zeros1+ ((Flonum Flonum -> Flonum) -> (Flonum Flonum -> Flonum)))
(define ((zeros1+ f) x y)
  (f (if (bfzero? x) 0.bf x) y))

(: zeros1- ((Flonum Flonum -> Flonum) -> (Flonum Flonum -> Flonum)))
(define ((zeros1- f) x y)
  (f (if (bfzero? x) -0.bf x) y))

(: zeros2+ ((Flonum Flonum -> Flonum) -> (Flonum Flonum -> Flonum)))
(define ((zeros2+ f) x y)
  (f x (if (bfzero? y) 0.bf y)))

(: zeros2- ((Flonum Flonum -> Flonum) -> (Flonum Flonum -> Flonum)))
(define ((zeros2- f) x y)
  (f x (if (bfzero? y) -0.bf y)))

#|
c = a + b    Addition
a = c - b    Reverse subtraction
b = c - a    Subtraction
|#

(: bfrev-/rndd (Flonum Flonum -> Flonum))
(define (bfrev-/rndd z x) (bf-/rndd x z))

(: bfrev-/rndu (Flonum Flonum -> Flonum))
(define (bfrev-/rndu z x) (bf-/rndu x z))

(define trij-add
  (Trijection #t #t reals reals reals
              bf+/rndd bf+/rndu
              bfrev-/rndd bfrev-/rndu
              bf-/rndd bf-/rndu))

(define trij-sub
  (trijection-second-inverse trij-add))

#|
c = a * b    Multiplication
a = c / b    Reverse division
b = c / a    Division
|#

(: bfrev//rndd (Flonum Flonum -> Flonum))
(define (bfrev//rndd c a) (bf//rndd a c))

(: bfrev//rndu (Flonum Flonum -> Flonum))
(define (bfrev//rndu c a) (bf//rndu a c))

(define trij-mul++
  (Trijection #t #t positive-interval positive-interval positive-interval
              bf*/rndd bf*/rndu
              (zeros1+ bfrev//rndd) (zeros1+ bfrev//rndu)
              (zeros2+ bf//rndd) (zeros2+ bf//rndu)))

(define trij-mul+-
  (Trijection #f #t positive-interval negative-interval negative-interval
              bf*/rndd bf*/rndu
              (zeros1- bfrev//rndd) (zeros1- bfrev//rndu)
              (zeros2+ bf//rndd) (zeros2+ bf//rndu)))

(define trij-mul-+
  (Trijection #t #f negative-interval positive-interval negative-interval
              bf*/rndd bf*/rndu
              (zeros1+ bfrev//rndd) (zeros1+ bfrev//rndu)
              (zeros2- bf//rndd) (zeros2- bf//rndu)))

(define trij-mul--
  (Trijection #f #f negative-interval negative-interval positive-interval
              bf*/rndd bf*/rndu
              (zeros1- bfrev//rndd) (zeros1- bfrev//rndu)
              (zeros2- bf//rndd) (zeros2- bf//rndu)))

;; These inverses may not be as you expect - unless you consider their domains

(define trij-div++
  (trijection-second-inverse trij-mul++))

(define trij-div+-
  (trijection-second-inverse trij-mul--))

(define trij-div-+
  (trijection-second-inverse trij-mul+-))

(define trij-div--
  (trijection-second-inverse trij-mul-+))

#|
TODO

#|
c = a^b
a = c^(1/b)
b = log(c)/log(a)
|#

(: bfexptinv1/rndd (Flonum Flonum -> Flonum))
(define (bfexptinv1/rndd b c)
  (bfexpt/rndd c (if (c . bf> . 1.bf) (bfrecip/rndd b) (bfrecip/rndu b))))

(: bfexptinv1/rndu (Flonum Flonum -> Flonum))
(define (bfexptinv1/rndu b c)
  (bfexpt/rndu c (if (c . bf> . 1.bf) (bfrecip/rndu b) (bfrecip/rndd b))))

(: bfexptinv2/rndd (Flonum Flonum -> Flonum))
(define (bfexptinv2/rndd c a)
  (bf//rndd (bflog/rndd c) (bflog/rndu a)))

(: bfexptinv2/rndu (Flonum Flonum -> Flonum))
(define (bfexptinv2/rndu c a)
  (bf//rndu (bflog/rndu c) (bflog/rndd a)))

(define trij-expt++
  (Trijection #t #t
              (Nonextremal-Interval 1.bf +inf.bf #f #f)
              positive-interval
              (Nonextremal-Interval 1.bf +inf.bf #f #f)
              bfexpt/rndd bfexpt/rndu
              (zeros1+ bfexptinv1/rndd) (zeros1+ bfexptinv1/rndu)
              bfexptinv2/rndd bfexptinv2/rndu))

(define trij-expt+-
  (Trijection #f #t
              (Nonextremal-Interval 1.bf +inf.bf #f #f)
              negative-interval
              (Nonextremal-Interval 0.bf 1.bf #f #f)
              bfexpt/rndd bfexpt/rndu
              bfexptinv1/rndd bfexptinv1/rndu
              bfexptinv2/rndd bfexptinv2/rndu))

(define trij-expt-+
  (Trijection #t #f
              (Nonextremal-Interval 0.bf 1.bf #f #f)
              positive-interval
              (Nonextremal-Interval 0.bf 1.bf #f #f)
              bfexpt/rndd bfexpt/rndu
              (zeros2+ (zeros1+ bfexptinv1/rndd)) (zeros2+ (zeros1+ bfexptinv1/rndu))
              bfexptinv2/rndd bfexptinv2/rndu))

(define trij-expt--
  (Trijection #f #f
              (Nonextremal-Interval 0.bf 1.bf #f #f)
              negative-interval
              (Nonextremal-Interval 1.bf +inf.bf #f #f)
              bfexpt/rndd bfexpt/rndu
              bfexptinv1/rndd bfexptinv1/rndu
              bfexptinv2/rndd bfexptinv2/rndu))
|#

;; ===================================================================================================
;; Tests
#|
(: border-function  (Boolean Boolean Flonum Flonum Flonum Flonum Flonum Flonum
                             -> (Flonum Flonum -> Flonum)))
(define ((border-function inc1? inc2? a-min a-max b-min b-max c-min c-max) a b)
  (cond [(and (a . bf> . a-min) (a . bf< . a-max)
              (b . bf> . b-min) (b . bf< . b-max))
         +nan.bf]
        [(or (a . bf< . a-min) (a . bf> . a-max)
             (b . bf< . b-min) (b . bf> . b-max))
         +nan.bf]
        [else
         (define a-min? (bf= a a-min))
         (define a-max? (bf= a a-max))
         (define b-min? (bf= b b-min))
         (define b-max? (bf= b b-max))
         (let-values ([(a-min? a-max?)  (if inc1? (values a-min? a-max?) (values a-max? a-min?))]
                      [(b-min? b-max?)  (if inc2? (values b-min? b-max?) (values b-max? b-min?))])
           (cond [a-min?  (if b-max? +nan.bf c-min)]
                 [b-min?  (if a-max? +nan.bf c-min)]
                 [a-max?  c-max]
                 [b-max?  c-max]
                 [else  +nan.bf]))]))

(: between (Flonum Flonum -> Flonum))
(define (between mn mx)
  (cond [(and (bf= mn -inf.bf) (bf= mx +inf.bf))  0.bf]
        [(bf= mn -inf.bf)
         (cond [(bfzero? mx)  -1.bf]
               [else  (bf- mx (bfabs (bf/ mx 2.bf)))])]
        [(bf= mx +inf.bf)
         (cond [(bfzero? mn)  1.bf]
               [else  (bf+ mn (bfabs (bf/ mn 2.bf)))])]
        [else  (bf/ (bf+ mn mx) 2.bf)]))

(: expand-zeros ((Listof Flonum) -> (Listof Flonum)))
(define (expand-zeros as)
  (remove-duplicates
   (append* (map (λ: ([a : Flonum]) (if (bfzero? a) (list -0.bf 0.bf) (list a))) as))
   (λ: ([a0 : Flonum] [a1 : Flonum])
     (cond [(and (bfzero? a0) (bfzero? a1))
            (equal? (Flonum-signbit a0) (Flonum-signbit a1))]
           [else
            (equal? a0 a1)]))))

(: check-border (Boolean Boolean (Flonum Flonum -> Flonum)
                         Flonum Flonum Flonum Flonum Flonum Flonum
                         -> Any))
(define (check-border inc1? inc2? f a-min a-max b-min b-max c-min c-max)
  (define g (border-function inc1? inc2? a-min a-max b-min b-max c-min c-max))
  (define as (expand-zeros (list a-min (between a-min a-max) a-max)))
  (define bs (expand-zeros (list b-min (between b-min b-max) b-max)))
  ;(printf "as = ~v~nbs = ~v~n" as bs)
  (for*: ([a  (in-list as)]
          [b  (in-list bs)])
    (define c (g a b))
    (when (not (equal? c +nan.bf))
      (check-true (equal? c (f a b))
                  (format "failed border check: ~a ~v ~v should be ~v, not ~v"
                          f a b c (f a b)))
      (void))))

(: check-trijection-border (Trijection Symbol -> Any))
(define (check-trijection-border f name)
  (printf "Testing Trijection ~a~n" name)
  (match-define (Trijection inc1? inc2? X Y Z gc/rndd gc/rndu ga/rndd ga/rndu gb/rndd gb/rndu) f)
  (define-values (a-min a-max a-min? a-max?) (interval-fields X))
  (define-values (b-min b-max b-min? b-max?) (interval-fields Y))
  (define-values (c-min c-max c-min? c-max?) (interval-fields Z))
  (define inc3? (not (eq? inc1? inc2?)))
  (printf "Testing gc~n")
  (check-border inc1? inc2? gc/rndd a-min a-max b-min b-max c-min c-max)
  (check-border inc1? inc2? gc/rndu a-min a-max b-min b-max c-min c-max)
  (printf "Testing ga~n")
  (check-border inc3? inc1? ga/rndd b-min b-max c-min c-max a-min a-max)
  (check-border inc3? inc1? ga/rndu b-min b-max c-min c-max a-min a-max)
  (printf "Testing gb~n")
  (check-border inc2? inc3? gb/rndd c-min c-max a-min a-max b-min b-max)
  (check-border inc2? inc3? gb/rndu c-min c-max a-min a-max b-min b-max))

(check-trijection-border trij-add 'trij-add)
(check-trijection-border trij-sub 'trij-sub)

(check-trijection-border trij-mul++ 'trij-mul++)
(check-trijection-border trij-mul+- 'trij-mul+-)
(check-trijection-border trij-mul-+ 'trij-mul-+)
(check-trijection-border trij-mul-- 'trij-mul--)

(check-trijection-border trij-div++ 'trij-div++)
(check-trijection-border trij-div+- 'trij-div+-)
(check-trijection-border trij-div-+ 'trij-div-+)
(check-trijection-border trij-div-- 'trij-div--)

;(check-trijection-border trij-expt++ 'trij-expt++)
;(check-trijection-border trij-expt+- 'trij-expt+-)
;; These currently fail:
;(check-trijection-border trij-expt-+ 'trij-expt-+)
;(check-trijection-border trij-expt-- 'trij-expt--)
|#
|#
