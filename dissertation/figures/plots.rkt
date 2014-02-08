#lang racket

(require plot
         (except-in plot/utils flsum)
         (except-in math/distributions sample)
         math/flonum)

(plot-font-size 12)

(plot (list
       (function (λ (x) (flnormal-pdf 0.0 1.0 (fl x) #f)) -3 3
                 #:width 4)
       (function-interval (λ (x) 0) (λ (x) (flnormal-pdf 0.0 1.0 (fl x) #f)) 0 1
                          #:line1-style 'transparent
                          #:color (->pen-color 3)
                          #:style 2
                          #:line2-width 4
                          #:line2-color (->brush-color 3)
                          #:line2-style 'long-dash))
      #:x-label "x"
      #:y-label #f
      #:out-file "density-integrate.pdf")

(plot-font-size 14)

(define (f x y)
  (let ([x  (fl x)] [y  (fl y)])
    (* (flnormal-pdf 0.0 1.0 x #f)
       (flnormal-pdf x 1.0 y #f))))

(define xs (linear-seq -2 0 32))
(define ys (linear-seq -2 -1 10))

(plot3d (list
         (contour-intervals3d f -3 3 -3 3
                              #:z-min 0 #:z-max 0.16
                              #:alphas '(0.6)
                              #:samples 31)
         (surface3d f -2 0 -2 -1
                              #:samples 31
                              #:alpha 1
                              #:color 3)
         (for/list ([x  (in-list (rest xs))])
           (lines3d (list (list x -2 0) (list x -2 (f x -2)))
                    #:color 3))
         (for/list ([y  (in-list ys)])
           (lines3d (list (list -2 y 0) (list -2 y (f -2 y)))
                    #:color 3))
         (lines3d (for/list ([x  (in-list xs)])
                    (list x -2 0))
                  #:color 3
                  #:width 2
                  #:style 'long-dash)
         (lines3d (for/list ([y  (in-list ys)])
                    (list -2 y 0))
                  #:color 3
                  #:width 2
                  #:style 'long-dash)
         (parametric3d (λ (x) (list x -2 (+ 0.001 (f x -2)))) -2 0
                       #:color 3
                       #:width 2
                       #:style 'long-dash)
         (parametric3d (λ (x) (list x -1 (+ 0.001 (f x -1)))) -2 0
                       #:color 3
                       #:width 2
                       #:style 'long-dash)
         (parametric3d (λ (y) (list -2 y (+ 0.001 (f -2 y)))) -2 -1
                       #:color 3
                       #:width 2
                       #:style 'long-dash)
         (parametric3d (λ (y) (list 0 y (+ 0.001 (f 0 y)))) -2 -1
                       #:color 3
                       #:width 2
                       #:style 'long-dash)
         )
        #:x-label "x"
        #:y-label "y"
        #:z-label #f
        #:out-file "2d-density-integrate.pdf"
        )

(plot3d (contour-intervals3d f -3 3 -3 3
                             #:z-min 0 #:z-max 0.16
                             #:samples 31)
        #:x-label "x"
        #:y-label "y"
        #:z-label #f
        #:out-file "bayes-densities-1.pdf")

(plot3d (contour-intervals3d
         (λ (x y)
           (if ((abs (- y 2)) . < . 0.125) (f x y) 0))
         -3 3 -3 3 #:z-min 0 #:z-max 0.16
         #:samples 31)
        #:x-label "x"
        #:y-label "y"
        #:out-file "bayes-densities-2.pdf")

(define (darken c)
  (second (color-seq c (->pen-color 0) 9)))

(define normal-colors
  (color-seq* (list (->brush-color 5)
                    (->brush-color 0)
                    (->brush-color 1))
              4))

(define cross-x-min 0.60269)
(define cross-x-max 1.39731)

(define cross-section
  (list (function-interval
         (λ (x) 0.05)
         (λ (x) (max 0.05 (f x 2))) cross-x-min cross-x-max
         #:line1-style 'transparent
         #:line2-color 0
         #:line2-width 2
         #:color (darken (second normal-colors)))
        (function-interval
         (λ (x) 0)
         (λ (x) (min 0.05 (f x 2))) -3 3
         #:line1-style 'transparent
         #:line2-color 0
         #:line2-width 2
         #:color (darken (first normal-colors))
         #:label "not density of X|Y=2")
        ))

(plot cross-section
      #:y-max 0.16
      #:x-label "x"
      #:y-label #f
      #:out-file "bayes-densities-3.pdf")

(plot (list
       (function-interval
        (λ (x) 0)
        (λ (x) (flnormal-pdf 1.0 (sqrt 0.5) (fl x) #f)) -3 3
        #:color (->pen-color 2)
        #:style 2
        #:line1-style 'transparent
        #:line2-color 2 #:line2-width 4
        #:label "density of X|Y=2")
       cross-section)
      #:x-label "x"
      #:y-label #f
      #:out-file "bayes-densities-4.pdf")
