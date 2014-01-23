#lang racket

(require math/distributions)

(define px-dist (normal-dist 0 1))
(define py-dist (normal-dist 0 1))

(define qx0-dist (normal-dist -0.25 0.75))
(define qy0-dist (normal-dist -0.25 0.75))

(define qx1-dist (normal-dist 1.25 0.75))
(define qy1-dist (normal-dist 1.25 0.75))

(define (circle-cond x y)
  ((abs (- (sqrt (+ (sqr (- x 0.5)) (sqr (- y 0.5)))) 1)) . < . 0.5))

(define (line-cond x y)
  ((- 1 x) . > . y))

(define (random-weighted-point)
  (cond [((random) . < . 0.6)
         (define x (sample qx0-dist))
         (define y (sample qy0-dist))
         (define w (if (and (circle-cond x y) (line-cond x y))
                       (* (/ 1 0.6) (/ (* (pdf px-dist x)  (pdf py-dist y))
                                       (* (pdf qx0-dist x) (pdf qy0-dist y))))
                       0))
         (list w x y)]
        [else
         (define x (sample qx1-dist))
         (define y (sample qy1-dist))
         (define w (if (and (circle-cond x y) (not (line-cond x y)))
                       (* (/ 1 0.4) (/ (* (pdf px-dist x)  (pdf py-dist y))
                                       (* (pdf qx1-dist x) (pdf qy1-dist y))))
                       0))
         (list w x y)]))
