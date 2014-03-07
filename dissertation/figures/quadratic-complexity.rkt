#lang racket

(require plot
         math/matrix
         math/array
         math/distributions)

(plot-font-size 14)

(define (polynomial-least-squares xs ys m)
  (define X (vandermonde-matrix xs (+ m 1)))
  (define X^T (matrix-transpose X))
  (define Y (->col-matrix ys))
  (define A (matrix-solve (matrix* X^T X) (matrix* X^T Y)))
  (printf "A = ~v~n~n" A)
  (λ (x)
    (matrix-ref (matrix* (vandermonde-matrix (list x) (+ m 1)) A) 0 0)))

(define xs (sample (normal-dist 0 1) 5))
(define ys (sample (normal-dist 0 1) 5))

(plot (list (function (polynomial-least-squares xs ys 4))
            (points (map list xs ys)))
      #:x-min (+ -1 (apply min xs))
      #:x-max (+ 1 (apply max xs))
      #:y-min (+ -1 (apply min ys))
      #:y-max (+ 1 (apply max ys))
      )

(define data
  '((0 .063)
    (1 .262)
    (2 .493)
    (3 .814)
    (4 1.222)
    (5 1.708)
    (6 2.238)
    (7 2.883)
    (8 3.680)
    (9 4.393)
    (10 5.382)
    (11 6.377)
    (12 7.517)
    (13 8.547)
    (14 9.592)
    (15 11.044)
    ))

(plot (list (function (polynomial-least-squares (map first data) (map second data) 2)
                      #:label "Quadratic Fit (OLS)"
                      #:color 0 #:style 'dot #:width 2
             )
            (points data
                    #:line-width 3
                    #:fill-color "white"
                    #:sym 'fullcircle
                    #:size 8
                    #:color 2
                    #:label "Data Points")
            )
      #:y-min -0.15
      #:x-min -0.25
      #:x-max 17
      #:x-label "Number of observations"
      #:y-label "Time (s)"
      #:out-file "quadratic-fit.pdf"
      )
