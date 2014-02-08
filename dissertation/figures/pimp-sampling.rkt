#lang typed/racket

(require math/distributions
         math/statistics
         plot/typed)

(: dist* (All (I1 O1 I2 O2)
                  ((distribution I1 O1)
                   (distribution I2 O2)
                   -> (distribution (Pair I1 I2) (Pair O1 O2)))))
(define (dist* d1 d2)
  (match-define (distribution p1 s1) d1)
  (match-define (distribution p2 s2) d2)
  
  (: new-pdf (case-> ((Pair I1 I2) -> Flonum)
                     ((Pair I1 I2) Any -> Flonum)))
  (define (new-pdf x [log? #f])
    (cond [log?  (+ (p1 (car x) #t)
                    (p2 (cdr x) #t))]
          [else  (* (p1 (car x) #f)
                    (p2 (cdr x) #f))]))
  
  (: new-sample (case-> (-> (Pair O1 O2))
                        (Integer -> (Listof (Pair O1 O2)))))
  (define new-sample
    (case-lambda
      [()  (cons (s1) (s2))]
      [(n)  (map (inst cons O1 O2) (s1 n) (s2 n))]))
  
  (distribution new-pdf new-sample))

(: dist*kernel (All (I1 O1 I2 O2)
                  ((distribution I1 O1)
                   ((U I1 O1) -> (distribution I2 O2))
                   -> (distribution (Pair I1 I2) (Pair O1 O2)))))
(define (dist*kernel d1 k2)
  (match-define (distribution p1 s1) d1)
  
  (: new-pdf (case-> ((Pair I1 I2) -> Flonum)
                     ((Pair I1 I2) Any -> Flonum)))
  (define (new-pdf x [log? #f])
    (cond [log?  (+ (p1 (car x) #t)
                    (pdf (k2 (car x)) (cdr x) #t))]
          [else  (* (p1 (car x) #f)
                    (pdf (k2 (car x)) (cdr x) #f))]))
  
  (: new-sample (case-> (-> (Pair O1 O2))
                        (Integer -> (Listof (Pair O1 O2)))))
  (define new-sample
    (case-lambda
      [()
       (let ([x1  (s1)])
         (cons x1 (sample (k2 x1))))]
      [(n)
       (map (λ: ([x1 : O1]) (cons x1 (sample (k2 x1)))) (s1 n))]))
  
  (distribution new-pdf new-sample))

;; Partitioned importance sampling

(: pimp-sample (All (X N) ((distribution X X)
                           (distribution N N)
                           (N -> (X -> Boolean))
                           (N -> (distribution X X))
                           -> (Values X Flonum))))
(define (pimp-sample P-dist N-dist s Q-kern)
  (define n (sample N-dist))
  (define Q-dist (Q-kern n))
  (define a (sample Q-dist))
  (define w (if ((s n) a)
                (/ (/ (pdf P-dist a)
                      (pdf Q-dist a))
                   (pdf N-dist n))
                0.0))
  (values a w))

(define P-dist (dist* (uniform-dist 0 1) (uniform-dist 0 1)))
(define N-dist (discrete-dist '(#t #f) '(0.4 0.6)))

(: s (Boolean -> ((Pair Flonum Flonum) -> Boolean)))
(define ((s n) xy)
  (match-define (cons x y) xy)
  (eq? n (y . > . (- (* 2 x) 0.5))))

(: Q-kern (Boolean -> (distribution (Pair Flonum Flonum) (Pair Flonum Flonum))))
(define (Q-kern n)
  (cond [n     (dist* (uniform-dist -0.125 0.875) (uniform-dist -0.125 1.125))]
        [else  (dist*kernel (normal-dist 0.8 0.25) (λ: ([x : Real]) (normal-dist (- x 0.5) 0.3)))]))

(define xws
  (for/list: : (Listof (Pair (Pair Flonum Flonum) Flonum)) ([_  (in-range 20000)])
    (define-values (x w)
      (pimp-sample P-dist N-dist s Q-kern))
    (cons x w)))

(define xys (map (inst car (Pair Flonum Flonum) Flonum) xws))
(define ws (map (inst cdr (Pair Flonum Flonum) Flonum) xws))
(define new-xys
  (sample (discrete-dist xys ws) (length xys)))

(define outlines
  (list
   (function (λ: ([x : Real]) (- (* 2 x) 0.5)) 0.25 0.75
             #:width 4 #:style 'long-dash)
   (lines '((0 0) (1 0) (1 1) (0 1) (0 0))
          #:width 4 #:style 'long-dash)))

(plot-font-size 15)
(plot-x-label #f)
(plot-y-label #f)

(plot (list (points (map (λ: ([xy : (Pair Flonum Flonum)])
                           #;(list (sample (normal-dist (car xy) 0.005))
                                 (sample (normal-dist (cdr xy) 0.005)))
                           (list (car xy) (cdr xy)))
                         xys)
                    #:sym 'fullcircle #:alpha 0.0625
                    ;#:sym 'point
                    )
            outlines)
      #:x-min -0.25 #:x-max 1.25
      #:y-min -0.25 #:y-max 1.25
      #:out-file "pimp-sampling-unweighted.pdf")

(plot (list (points (map (λ: ([xy : (Pair Flonum Flonum)])
                           #;
                           (list (sample (normal-dist (car xy) 0.005))
                                 (sample (normal-dist (cdr xy) 0.005)))
                           (list (car xy) (cdr xy)))
                         new-xys)
                    #:sym 'fullcircle #:alpha 0.03125
                    ;#:sym 'point
                    )
            outlines)
      #:x-min -0.25 #:x-max 1.25
      #:y-min -0.25 #:y-max 1.25
      #:out-file "pimp-sampling-weighted.pdf")
