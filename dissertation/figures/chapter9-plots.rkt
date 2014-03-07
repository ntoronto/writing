#lang racket

(require plot
         plot/utils)

(plot-font-size 14)

(plot (contour-intervals
       (λ (x y)
         (max (- -0.1 (* (+ -1 (* x 2)) (+ -1 (* y 2))))
              (- (* (+ -1 (* x 2)) (+ -1 (* y 2))) 0.2)))
       0 1 0 1
       #:levels '(0)
       #:styles '(solid transparent)
       #:colors (list 2 0)
       #:contour-colors '(2)
       #:samples 41)
      #:x-label "ω0"
      #:y-label "ω1"
      #:out-file "mul-preimage.pdf")

(plot (contour-intervals
       (λ (x y)
         (max (- -0.5 (/ (+ -1 (* x 2)) (+ -1 (* y 2))))
              (- (/ (+ -1 (* x 2)) (+ -1 (* y 2))) 1)))
       0 1 0 1
       #:levels '(0)
       #:styles '(solid transparent)
       #:colors (list 2 0)
       #:contour-colors '(2)
       #:samples 40)
      #:x-label "ω0"
      #:y-label "ω1"
      #:out-file "div-preimage.svg")

(define ε1 0.1)
(define ε2 0.2)
(define ε3 0.25)

(plot3d (isosurfaces3d
         (λ (y1 y2 y3)
           (max (/ (abs (- 2 y1)) ε1) (/ (abs (- -1 y2)) ε2) (/ (abs (- 3 y3)) ε3)))
         1 3 -2 0 2 4
         #:levels '(1/4 1/2 1 2 4)
         #:styles '(1 2)
         )
        #:x-label "y1"
        #:y-label "y2"
        #:z-label "y3"
        #:angle 45 #:altitude 30
        #:out-file "nested-rectangles.pdf")
