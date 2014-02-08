#lang racket

(require plot
         plot/utils)

(plot-font-size 12)

;; Let A' ⊆ A, B' ⊆ B, and f : A → B have inverse f^{-1} : B → A. Let f := restrict f A'.
;; Then preimage f B' = A' ∩ image f^{-1} B'.
(parameterize ([plot-x-ticks  no-ticks]
               [plot-y-ticks  no-ticks])
  (plot (list
         (x-ticks (list (tick 0 #t "0") (tick 1 #t "1") (tick 2 #t "2")
                        (tick (sqrt 2) #t "√2") (tick (sqrt 7) #t "√7")))
         (y-ticks (list (tick 0 #t "0") (tick 2 #t "2") (tick 7 #t "7")))
         ;; Interval projection through sqr
         (inverse-interval (λ _ 0) sqrt 2 7
                           #:style 4 #:color (->pen-color 3) #:alpha 1 #:line1-style 'transparent)
         (function-interval (λ _ 0) sqr (sqrt 2) 2 ;(sqrt 7)
                            #:style 5  #:color (->pen-color 3) #:alpha 1 #:line1-style 'transparent)
         (function-interval (λ _ 0) sqr 2 (sqrt 7)
                            #:style 5  #:color (->pen-color 3) #:alpha 1/5 #:line1-style 'transparent)
         ;; White border around sqr
         (function sqr 0 3
                   #:width 8 #:color (->brush-color 0))
         ;; Function sqr
         (function sqr 0 3 #:label "sqr"
                   #:width 4 #:color 1 #:style 'dot)
         ;; White border around sqr and endpoints
         (points (list (list 1 (sqr 1)) (list 2 (sqr 2)))
                 #:sym 'fullcircle #:color (->brush-color 0) #:size 14)
         ;; sqr and endpoints
         (function sqr 1 2 #:label "restrict sqr [1,2)"
                   #:width 4 #:color 2)
         (points (list (list 1 (sqr 1)))
                 #:sym 'fullcircle #:color 2 #:size 10)
         (points (list (list 2 (sqr 2)))
                 #:sym 'fullcircle #:color 2 #:fill-color 0 #:line-width 3 #:size 8)
         ;; White border around intervals
         (lines (list (list 0 2) (list 0 7))
                #:width 8 #:color (->brush-color 0))
         (lines (list (list (sqrt 2) 0) (list 2 0))
                #:width 8 #:color (->brush-color 0))
         (points (list (list 0 2) (list 0 7) (list (sqrt 2) 0) (list 2 0))
                 #:sym 'fullcircle #:color (->brush-color 0) #:size 14)
         ;; Intervals
         (lines (list (list 0 2) (list 0 7))
                #:width 4 #:color 3)
         (lines (list (list (sqrt 2) 0) (list 2 0))
                #:width 4 #:color 3)
         (points (list (list 0 2) (list 0 7) (list (sqrt 2) 0))
                 #:sym 'fullcircle #:color 3 #:size 10)
         (points (list (list 2 0))
                 #:sym 'fullcircle #:color 3 #:fill-color 0 #:line-width 3 #:size 8)
         )
        #:x-label "a" #:y-label "sqr a"
        #:x-min -0.1 #:y-min -0.35
        #:out-file "preimage-by-inverse-image.pdf"))

;; Definition 4.2.1 (axial inverse).

;; Theorem 4.2.5 (preimage bounds from axial inverse images).
(parameterize ([plot-x-ticks  no-ticks]
               [plot-y-ticks  no-ticks])
  (plot (list
         (x-ticks (list (tick 0 #t "0") (tick -2 #t "-2") (tick 1/2 #t "1/2") (tick 1 #t "1")))
         (y-ticks (list (tick 0 #t "0") (tick -1 #t "-1") (tick 1/2 #t "1/2") (tick 2 #t "2")))
         (function-interval (λ (x) (- x)) (λ (x) (- 1/2 x))
                            #:color 1
                            #:line1-color 1
                            #:line2-color 1
                            #:line1-style 'short-dash
                            #:line2-style 'short-dash
                            #:line1-width 2
                            #:line2-width 2
                            ;#:style 2
                            #:label "preimage of [0,1/2]")
         (rectangles (list (list (ivl 0 1) (ivl 0 2)))
                     #:color (->pen-color 2)
                     #:line-color 2
                     #:line-style 'transparent
                     #:style 1
                     #:label "domain restriction")
         (rectangles (list (list (ivl -2 1/2) (ivl -1 1/2)))
                     #:color (->pen-color 3)
                     #:line-color 3
                     #:line-style 'transparent
                     #:style 2
                     #:label "axis inverse images")
         (rectangles (list (list (ivl 0 1/2) (ivl 0 1/2)))
                     ;#:alpha 3/5
                     #:color (->pen-color 5)
                     #:style 3
                     #:line-color 5
                     #:line-width 2
                     #:label "rectangular preimage"
                     )
         )
        #:x-label "a" #:y-label "b"
        #:x-min -2.1 #:x-max 1.1
        #:y-min -1.1 #:y-max 2.1
        #:out-file "rect-preimage-by-inverse-images.pdf"))


;; Definition 4.2.7 (uniform axis property).

;; Example 4.2.11 (mul_c is not uniformly surjective or monotone).
(plot3d (list (contour-intervals3d * -1 1 -1 1
                                   #:contour-styles '(solid transparent solid)
                                   #:samples 41)
              (isoline3d * 0
                          #:color 0
                          #:width 3
                          #:style 'short-dash
                          #:label "a·b = 0"
                          #:samples 41)
              )
        #:x-label "a" #:y-label "b" #:z-label "a·b"
        #:out-file "mul-nonuniform-properties.pdf")

