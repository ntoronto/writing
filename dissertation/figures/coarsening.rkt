#lang racket

(require plot plot/utils)

(plot-font-size 14)

(define (sqrprime x)
  (define n (exact-floor x))
  (/ (+ (* 3 n n) (* 3 n) 1) 3))

(plot (list (function sqr
                      #:width 4 #:style 'short-dash
                      #:label "sqr")
            (function-interval (λ (_) 0) sqr
                               #:line1-style 'transparent #:line2-style 'transparent
                               #:color (->pen-color 1)
                               #:alpha 0.25
                               #:style 2)
            (rectangles (for/list ([n  '(-3 -2 -1 0 1 2)])
                          (list (ivl n (+ n 1))
                                (ivl 0 (sqrprime n))))
                        #:color (->pen-color 3)
                        #:alpha 0.25
                        #:style 1
                        #:line-style 'long-dash)
            (function (λ (_) -1) -10 -9
                      #:color 2
                      #:width 4
                      #:label "sqr'")
            (for/list ([n  (in-range -3 4)])
              (function (λ (_) (/ (+ (* 3 n n) (* 3 n) 1) 3)) n (+ n 1)
                        #:color 2
                        #:width 4)))
      #:x-min -3 #:x-max 3
      #:y-min 0 #:y-max 9.5
      #:x-label "x" #:y-label #f
      #:out-file "coarsening-2d.pdf"
      )
