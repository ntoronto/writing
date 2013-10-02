#lang racket

(require plot math)

(define (f x y)
  (let ([x  (fl x)] [y  (fl y)])
    (* (flnormal-pdf 0.0 1.0 x #f)
       (flnormal-pdf x 1.0 y #f))))

;(plot3d (contour-intervals3d f -3 3 -3 3))

(define (prob-near ε n)
  (define xs (flvector->list (flnormal-sample 0.0 1.0 n)))
  (define y0s (flvector->list (flnormal-sample 0.0 1.0 n)))
  (define ys (map + xs y0s))
  (fl (/ (count (λ (x y) ((abs (- (sqrt (+ (* x x) (* y y))) 1.0)) . <= . ε))
                xs ys)
         n)))

(define (plot-constrained-norm-norm-density ε prob)
  (plot3d (contour-intervals3d
           (λ (x y)
             (if ((abs (- (sqrt (+ (* x x) (* y y))) 1.0)) . <= . ε)
                 (/ (f x y) prob)
                 0.0))
           -1.5 1.5 -1.5 1.5
           #:line-styles '(transparent)
           #:samples 120)))

#|
(plot-constrained-norm-norm-density 0.5 0.476)
(plot-constrained-norm-norm-density 0.25 0.251)
(plot-constrained-norm-norm-density 0.1 0.102)
(plot-constrained-norm-norm-density 0.025 0.0255)
|#

(define (norm-norm-condition-dist ω0 ω1)
  (let ([ω0  (fl ω0)]
        [ω1  (fl ω1)])
    (let* ([x  (flnormal-inv-cdf 0.0 1.0 ω0 #f #f)]
           [y  (fl+ x (flnormal-inv-cdf 0.0 1.0 ω1 #f #f))])
      (- (sqrt (+ (* x x) (* y y))) 1.0))))

(define (plot-constrained-norm-norm-source ε)
  (plot (contour-intervals norm-norm-condition-dist 0 1 0 1
                           #:levels (list (- ε) ε)
                           #:colors '(white 2 white)
                           #:styles '(transparent solid transparent)
                           #:contour-styles '(solid)
                           #:contour-colors '(2)
                           )
        #:x-label "ω0 axis"
        #:y-label "ω1 axis"))

#|
(plot-constrained-norm-norm-source 0.5)
(plot-constrained-norm-norm-source 0.25)
(plot-constrained-norm-norm-source 0.1)
(plot-constrained-norm-norm-source 0.025)
|#

#;
(plot (contour-intervals norm-norm-condition-dist 0 1 0 1
                         #:levels (list -0.1 0.1)
                         #:colors '(white 2 white)
                         #:styles '(transparent solid transparent)
                         #:contour-styles '(solid)
                         #:contour-colors '(2)
                         )
      #:x-label "ω0 axis"
      #:y-label "ω1 axis"
      #:out-file "refinement/constrained-norm-norm-source.svg")

;; ===================================================================================================

(define (plot-axial-condition eps)
  (plot (list
         (contours f -3 3 -3 3 #:levels 5)
         (rectangles (list (list (ivl -3 3) (ivl (- 2 eps) (+ 2 eps))))
                     #:color 2 #:line-color 2 #:alpha 0.75)
         (rectangles (list (list (ivl 1 3) (ivl -3 3)))
                     #:color 3 #:line-color 3 #:alpha 0.75)
         (rectangles (list (list (ivl 1 3) (ivl (- 2 eps) (+ 2 eps))))
                     #:color 1 #:line-color 1 #:alpha 0.75)
         )
        #:x-label "ω0 axis"
        #:y-label "ω1 axis"))

(plot-axial-condition 0.5)
(plot-axial-condition 0.25)
(plot-axial-condition 0.1)
(plot-axial-condition 0.025)

;; ===================================================================================================

(define (d x y)
  (- (sqrt (+ (sqr x) (sqr y))) 1))

(define (plot-circular-condition eps)
  (plot (list
         (contours f -3 3 -3 3 #:levels 5)
         (contour-intervals d -3 3 -3 3
                            #:levels (list (- eps) eps)
                            #:colors '(white 2 white)
                            #:styles '(transparent solid transparent)
                            #:alphas '(0.75)
                            #:contour-styles '(solid)
                            #:contour-colors '(2))
         (rectangles (list (list (ivl 0 2) (ivl 0 2)))
                     #:color 3 #:line-color 3 #:alpha 0.75)
         (contour-intervals d 0 2 0 2
                            #:levels (list (- eps) eps)
                            #:colors '(white 1 white)
                            #:styles '(transparent solid transparent)
                            #:alphas '(0.75)
                            #:contour-styles '(solid)
                            #:contour-colors '(1))
         )
        #:x-label "ω0 axis"
        #:y-label "ω1 axis"))

(plot-circular-condition 0.5)
(plot-circular-condition 0.25)
(plot-circular-condition 0.1)
(plot-circular-condition 0.025)

#|
;; ===================================================================================================

(define (d1 x y)
  (- (+ x y) 0.6))

(define (d2 x y)
  (* 4.0 (- (* x y) 0.075)))

(define (d3 x y)
  (define z1 (d1 x y))
  (define z2 (d2 x y))
  (if ((abs z1) . > . (abs z2)) z1 z2))

(plot (contour-intervals d1 0 1 0 1
                         #:levels (list -0.1 0.1)
                         #:colors '(white 2 white)
                         #:styles '(transparent solid transparent)
                         #:contour-styles '(solid)
                         #:contour-colors '(2)
                         )
      #:x-label "ω0 axis"
      #:y-label "ω1 axis")

(plot (list
       (contour-intervals d1 0 1 0 1
                          #:levels (list -0.1 0.1)
                          #:colors '(white 2 white)
                          #:styles '(transparent solid transparent)
                          #:contour-styles '(solid)
                          #:contour-colors '(2)
                          )
       (contour-intervals d2 0 1 0 1
                          #:levels (list -0.1 0.1)
                          #:colors '(white 3 white)
                          #:styles '(transparent solid transparent)
                          #:contour-styles '(solid)
                          #:contour-colors '(3)
                          ))
      #:x-label "ω0 axis"
      #:y-label "ω1 axis")

(plot (list
       (contour-intervals d1 0 1 0 1
                          #:levels (list -0.1 0.1)
                          #:colors '(white 2 white)
                          #:styles '(transparent solid transparent)
                          #:contour-styles '(solid)
                          #:contour-colors '(2)
                          )
       (contour-intervals d2 0 1 0 1
                          #:levels (list -0.1 0.1)
                          #:colors '(white 3 white)
                          #:styles '(transparent solid transparent)
                          #:contour-styles '(solid)
                          #:contour-colors '(3)
                          )
       (contour-intervals d3 0.081 0.615 0.081 0.615
                          #:levels (list -0.1 0.1)
                          #:colors '(white 1 white)
                          #:styles '(transparent solid transparent)
                          #:contour-styles '(solid)
                          #:contour-colors '(1)
                          #:samples 81
                          ))
      #:x-label "ω0 axis"
      #:y-label "ω1 axis"
      #:x-min 0 #:x-max 1
      #:y-min 0 #:y-max 1)
|#
