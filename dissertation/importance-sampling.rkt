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
  (define x-dist (uniform-dist 0 2))
  (define x (sample x-dist))
  (define y-dist
    (cond [(x . > . 1.0)  ry-dist #;(uniform-dist 0 4)]
          [else  (uniform-dist 0 2)]))
  (define y (sample y-dist))
  (define wxy
    (list (* ((diff rx-dist x-dist) x) ((diff ry-dist y-dist) y)) x y))
  (match wxy
    [(list w x y)  (and (y . <= . (* 2 (sqr x))) wxy)]
    [_  #f]))

(define wxys (filter values (build-list 20000 (λ _ (maybe-rand)))))
(define re-wxys (sample (discrete-dist (map rest wxys) (map first wxys)) (length wxys)))

(plot (points (map rest wxys) #:alpha 0.0125 #:sym 'fullcircle #:size 12)
      #:x-min 0 #:x-max 3
      #:y-min -5 #:y-max 5)

(plot (points re-wxys #:alpha 0.0125 #:sym 'fullcircle #:size 12)
      #:x-min 0 #:x-max 3
      #:y-min -1 #:y-max 5)
