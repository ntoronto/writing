#lang racket

(require plot plot/utils)

(plot-font-size 14)

;(if ((random) . < . (random)) #t #f)


(plot (list (contour-intervals
             (λ (x y) (- x y))
             0 1 0 1
             #:levels '(-1 0 1)
             #:styles '(1 2)
             #:colors '(2 17)
             #:contour-colors '(2)
             #:contour-widths '(2))
            (point-label (list 0.075 0.8)
                         "t = [left j₀ ↦ true, * ↦ ⊥]"
                         #:point-size 0 #:size 16)
            (point-label (list 0.4 0.2)
                         "t = [left j₀ ↦ false, * ↦ ⊥]"
                         #:point-size 0 #:size 16))
       
      #:x-label "ω0"
      #:y-label "ω1"
      #:out-file "trace-induced-partition.pdf")
