#lang racket

(require plot
         (except-in plot/utils sample flsum)
         math/distributions
         math/flonum)

(printf "starting...~n")

(define px-dist (normal-dist 0 1))
(define py-dist (normal-dist 0 1))

(define (circle-cond x y)
  ((abs (- (sqrt (+ (sqr (- x 0.5)) (sqr (- y 0.5)))) 1)) . < . 0.5))

(define (random-weighted-point)
  (cond [((random) . < . 0.6)
         (define qx-dist (normal-dist -0.25 0.75))
         (define qy-dist (normal-dist -0.25 0.75))
         (define x (sample qx-dist))
         (define y (sample qy-dist))
         (define w (* (/ 1 0.6) (/ (* (pdf px-dist x) (pdf py-dist y))
                                   (* (pdf qx-dist x) (pdf qy-dist y)))))
         (list (and (circle-cond x y) ((- 1 x) . > . y)) w #t x y)]
        [else
         (define qx-dist (normal-dist 1.25 0.75))
         (define qy-dist (normal-dist 1.25 0.75))
         (define x (sample qx-dist))
         (define y (sample qy-dist))
         (define w (* (/ 1 0.4) (/ (* (pdf px-dist x) (pdf py-dist y))
                                   (* (pdf qx-dist x) (pdf qy-dist y)))))
         (list (and (circle-cond x y) ((- 1 x) . <= . y)) w #f x y)]))

(define n 1500)

(let ()
  (define xs (sample px-dist n))
  (define ys (sample py-dist n))
  (define ws (map (λ (x y) (if (circle-cond x y) 1 0)) xs ys))
  (define wxys (filter (λ (wxy) ((first wxy) . > . 0))
                       (map list ws xs ys)))
  (printf "~a~n" (fl (/ (apply + (map first (filter (λ (wxy) ((second wxy) . > . 0.5)) wxys)))
                        (apply + ws))))
  (plot (points (map rest wxys) #:alpha 1 #:sym 'fullcircle)
        #:x-min -1.1 #:x-max 2.1
        #:y-min -1.1 #:y-max 2.1))

(define vwdxys (build-list n (λ _ (random-weighted-point))))
(define wdxys (map (compose rest) (filter first vwdxys)))

(define overlay
  (list
   (function (λ (x) (- 1 x)) #:width 5 #:color 'white)
   (parametric (λ (t) (list (+ 0.5 (* 1.5 (cos t))) (+ 0.5 (* 1.5 (sin t))))) (- pi) pi
               #:color 'white #:width 6)
   (parametric (λ (t) (list (+ 0.5 (* 0.5 (cos t))) (+ 0.5 (* 0.5 (sin t))))) (- pi) pi
               #:color 'white #:width 6)
   (function (λ (x) (- 1 x)) #:width 3 #:style 'long-dash #:color 0)
   (parametric (λ (t) (list (+ 0.5 (* 1.5 (cos t))) (+ 0.5 (* 1.5 (sin t))))) (- pi) pi
               #:color 0 #:width 4)
   (parametric (λ (t) (list (+ 0.5 (* 0.5 (cos t))) (+ 0.5 (* 0.5 (sin t))))) (- pi) pi
               #:color 0 #:width 4)))

(plot (list
       (points (map (compose rest rest rest)
                    (filter third vwdxys))
               #:sym 'fullcircle
               #:color 1)
       (points (map (compose rest rest rest)
                    (filter (compose not third) vwdxys))
               #:sym 'fullsquare
               #:size 8
               #:color 3)
       overlay)
      #:x-min -1.1 #:x-max 2.1
      #:y-min -1.1 #:y-max 2.1)

(/ (apply + (map first (filter (λ (wdxy) ((third wdxy) . > . 0.5)) wdxys)))
   (apply + (map first wdxys)))

(let ()
  (define dist (discrete-dist (map rest wdxys) (map first wdxys)))
  (define dxys (discrete-dist-values dist))
  (define n (length dxys))
  (define ws (map (λ (w) (* n w)) (discrete-dist-probs dist)))
  (plot (list (for/list ([dxy  (in-list dxys)]
                         [w  (in-list ws)]
                         #:when (first dxy))
                (points (list (rest dxy))
                        #:sym 'fullcircle
                        #:size (* 6 (sqrt w))
                        #:color 1))
              (for/list ([dxy  (in-list dxys)]
                         [w  (in-list ws)]
                         #:when (not (first dxy)))
                (points (list (rest dxy))
                        #:sym 'fullsquare
                        #:size (* 8 (sqrt w))
                        #:color 3))
              overlay
              )
        #:x-min -1.1 #:x-max 2.1
        #:y-min -1.1 #:y-max 2.1))
