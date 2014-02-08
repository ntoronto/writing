#lang racket

(require plot
         math/distributions)

(define ε 0.01)
(define rx-dist (normal-dist 1 0.5))
(define ry-dist (normal-dist 2 1.0))

(define ((diff dist0 dist1) x)
  (/ (real-dist-prob dist0 (- x ε) (+ x ε))
     (real-dist-prob dist1 (- x ε) (+ x ε))))

(define (maybe-rand)
  (define wxy
    (cond [((random) . < . 0.45)
           (define x-dist (uniform-dist 2 3))
           ;; Sampling from ry-dist unconstrained means it's unused
           (define y-dist ry-dist)
           (define x (sample x-dist))
           (define y (sample y-dist))
           (and (and (x . >= . 2) (x . <= . 3))
                (list (/ ((diff rx-dist x-dist) x)
                         (* 0.25))
                      x
                      y))]
          [((random) . < . 0.65)
           (cond [((random) . < . 0.3)
                  (define x-dist (normal-dist 1.5 0.4))
                  (define y-dist (uniform-dist 1 5))
                  (define x (sample x-dist))
                  (define y (sample y-dist))
                  (and (and (y . > . (sqr x)) (x . >= . 1) (x . <= . 1.5))
                       (list (/ (* ((diff rx-dist x-dist) x) ((diff ry-dist y-dist) y))
                                (* 0.55 0.65 0.3))
                             x
                             y))]
                 [else
                  (define x-dist (uniform-dist 1 2))
                  (define y-dist (normal-dist 2 1))
                  (define x (sample x-dist))
                  (define y (sample y-dist))
                  (and (and (y . <= . (sqr x)) (y . >= . 0) (y . <= . 4))
                       (list (/ (* ((diff rx-dist x-dist) x) ((diff ry-dist y-dist) y))
                                (* 0.55 0.65 0.7))
                             x
                             y))])]
          [else
           (cond [((random) . < . 0.8)
                  (define x-dist (uniform-dist 0 1))
                  (define y-dist (uniform-dist 0 2))
                  (define x (sample x-dist))
                  (define y (sample y-dist))
                  (and (y . > . (sqr x))
                       (list (/ (* ((diff rx-dist x-dist) x) ((diff ry-dist y-dist) y))
                                (* 0.55 0.35 0.8))
                             x
                             y))]
                 [else
                  (define x-dist (triangle-dist 0 1 1))
                  (define x (sample x-dist))
                  (define y-dist (uniform-dist 0 (sqr x)))
                  (define y (sample y-dist))
                  (and (y . <= . (sqr x))
                       (list (/ (* ((diff rx-dist x-dist) x) ((diff ry-dist y-dist) y))
                                (* 0.55 0.35 0.2))
                             x
                             y))])]))
  (match wxy
    [(list w x y)  (and (y . <= . (* 2 (sqr x))) wxy)]
    [_  #f]))

(define wxys (filter values (build-list 50000 (λ _ (maybe-rand)))))
(define re-wxys (sample (discrete-dist (map rest wxys) (map first wxys)) (length wxys)))

(plot (points (map rest wxys) #:alpha 0.0125 #:sym 'fullcircle #:size 12)
      #:x-min 0 #:x-max 3
      #:y-min -5 #:y-max 5)

(plot (points re-wxys #:alpha 0.0125 #:sym 'fullcircle #:size 12)
      #:x-min 0 #:x-max 3
      #:y-min -1 #:y-max 5)
