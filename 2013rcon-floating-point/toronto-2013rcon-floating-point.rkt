#lang slideshow

(require slideshow/latex
         slideshow/extras
         slideshow/code
         math/flonum
         math/bigfloat
         plot)

(get-current-code-font-size (λ () (exact-floor (* 5/6 (current-font-size)))))

(define (code-literal str)
  (colorize (text str (current-code-font) ((get-current-code-font-size)))
            (current-literal-color)))

(setup-local-latex-cache)

(latex-debug? #t)

(add-preamble #<<latex
\usepackage{amsmath, amssymb}
\newcommand{\meaningof}[1]{[\![{#1}]\!]}  % use in semantic functions
\usepackage{mathtools}
\DeclarePairedDelimiter{\ceil}{\lceil}{\rceil}
\DeclarePairedDelimiter{\floor}{\lfloor}{\rfloor}
latex
              )

(background-image (bitmap "slides-background.png"))

(slide
 (scale/improve-new-text (soft-dropshadow (bt "Debugging Floating-Point")) 2)
 (scale/improve-new-text (soft-dropshadow (bt "Math in Racket")) 2)
 (hrule)
 (scale (bt "Neil Toronto") 1.5)
 (scale (bt "RacketCon 2013") 1.5)
 )

(slide
 #:title "Racket Floating-Point Support"
 (para "Math modules that provide floating-point functions:")
 (item (code math/flonum) ": additional elementary functions")
 (item (code math/special-functions) ": gamma, beta, psi, etc.")
 (item (code math/distributions) ": Gamma, Normal, etc.")
 'next
 (para "Not comprehensive (yet!) but very accurate")
 'next
 (para "Debugged using these:")
 (item (code math/bigfloat) ": arbitrary-precision flonums")
 (item (code math/flonum) ": measure error, special values,"
       "flonum functions designed to help reduce error")
 (item (code plot) " and " (code plot/typed) ": plot error over domain")
 )

(slide
 #:title "You Could Have Invented Floating-Point"
 (para "Need to represent " ($"\\pm n \\times 10^m") " or " ($"\\pm n \\times 2^m") "...")
 'next
 (para (code (struct: float ([sign : (U -1 1)]
                             [sig : Natural]
                             [exp : Integer]))))
 'next
 (para (code (: float->real (float -> Real))
             (define (float->real x)
               (match-define (float s n m) x)
               (* s n (expt 2 m)))))
 'next
 (para (code > (float->real (float -1 10 0))
             -10))
 'next
 (para (code > (float->real (float -1 10 3))
             -80))
 )

(slide
 #:title "You Could Have Invented Floating-Point Multiplication"
 (para (code (struct: float ([sign : (U -1 1)]
                             [sig : Natural]
                             [exp : Integer]))))
 'next
 (para ($"s_1 \\times n_1  \\times 2^{m_1} \\times s_2  \\times n_2 \\times 2^{m_2}"))
 (para (blank 100 0) ($"= (s_1 \\times s_2) \\times (n_1 \\times n_2) \\times 2^{m_1+m_2}"))
 'next
 (para (code (: float* (float float -> float))
             (define (float* x1 x2)
               (match-define (float s1 n1 m1) x1)
               (match-define (float s2 n2 m2) x2)
               (float (* s1 s2) (* n1 n2) (+ m1 m2)))))
 'next
 (para (code > (float->real (float* (float -1 10 0)
                                    (float -1 10 3)))
             800))
 )

(slide
 #:title "Finite Approximation"
 (item "Actual flonum fields are fixed-size, requiring")
 (subitem "Rounding least significant bit after operations")
 (subitem "Representations for overflow (i.e." (code +inf.0) "and" (code -inf.0)
          ") and underflow (i.e." (code-literal "+0.0") "and" (code -0.0) ")")
 'next
 (item "Consequence: a natural well-order over flonums")
 (para (code > (flnext 0.0)  (code:comment "from math/flonum")
             4.9406564584125e-324))
 'next
 (para (code > (list (flonum->ordinal 0.0)
                     (flonum->ordinal +max.0))
             '(0 9218868437227405311)))
 'next
 (para (code > (flonums-between 0.0 1.0)
             4607182418800017408  (code:comment "4.6 *quintillion*")))
 'next
 (item "Consequence: most flonum functions aren't exact")
)

(slide
 #:title "Correctness Means Minimizing Error"
 (item "A flonum's" (bt "unit in last place (ulp)") "is the distance between it and the next flonum")
 (para (code > (flulp #,(code-literal "#i355/113")) (code:comment "from math/flonum")
             4.440892098500626e-16))
 'next
 (item "Error is distance from the true value, in ulps")
 (para (code > #,(code-literal "#i355/113") pi
             3.1415929203539825
             3.141592653589793))
 'next
 (para (code > (flulp-error #,(code-literal "#i355/113") pi)
             600699552.0 (code:comment "600.7 million ulps")))
 'next
 (item "A flonum function is" (bt "correctly rounded*") "when its outputs' maximum error"
       "is no more than 0.5 ulps")
 'next
 (scale (para "* assuming inputs are exact; i.e. no guarantees are given for arguments with error")
        2/3)
 )

(slide
 #:title "Correctness Example: Addition"
 (para (code > (plot3d (contour-intervals3d
                        (λ (x y) (let ([x  (fl x)] [y  (fl y)])
                                   (define z* (+ (inexact->exact x)
                                                 (inexact->exact y)))
                                   (flulp-error (fl+ x y) z*)))
                        -1 1 -1 1))))
 'next
 (plot3d-pict (contour-intervals3d
               (λ (x y)
                 (let ([x  (fl x)]
                       [y  (fl y)])
                   (define z* (+ (inexact->exact x)
                                 (inexact->exact y)))
                   (flulp-error (fl+ x y) z*)))
               -1 1 -1 1))
 )

(slide
 #:title "Correctness Example: Logarithm"
 (para (code > (require math/bigfloat) (code:comment "default sig. size: 128 bits")
             > (plot (function
                      (λ (x) (let ([x  (fl x)])
                               (define z* (bigfloat->real (bflog (bf x))))
                               (flulp-error (fllog x) z*)))
                      0 10))))
 'next
 (plot-pict (function
             (λ (x) (let ([x  (fl x)])
                      (define z* (bigfloat->real (bflog (bf x))))
                      (flulp-error (fllog x) z*)))
             0 10))
 )

(slide
 #:title "Correctness is Noncompositional"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bflog (bf x))))
                        (flulp-error (fllog (fl x)) z*)))
                      0 10)))
 'next
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bflog (bf x))))
               (flulp-error (fllog (fl x)) z*))
             0 10))
 )

(slide
 #:title "Debugging: Geometric Inverse CDF"
 (item "Implement " ($"f(p,u) = \\log(u) / \\log(1-p)") " for " ($"p,u \\in [0,1]"))
 'next
 (item "First stab:")
 (para (code (define (geom p u)
               (fl/ (fllog u) (fllog (fl- 1.0 p))))))
 'next
 (item "Reference implementation:")
 (para (code (define (geom* p u)
               (let ([p  (bf p)] [u  (bf u)])
                 (bigfloat->real
                  (bf/ (bflog u) (bflog (bf- 1.bf p))))))))
 )

(define (geom0 p u)
  (fl/ (fllog u) (fllog (fl- 1.0 p))))

(define (geom* p u)
  (let ([p  (bf p)] [u  (bf u)])
    (bigfloat->real
     (bf/ (bflog u) (bflog (bf- 1.bf p))))))

(slide
 #:title "Debugging: Geometric Inverse CDF"
 'alts
 (list
  (list (item "Error plot for" (code geom) "for" (code p <= 1) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom0 p u) (geom* p u))))
                      0 1 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 0.1) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom0 p u) (geom* p u))))
                      0 0.1 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 1e-5) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom0 p u) (geom* p u))))
                      0 1e-5 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 1e-10) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom0 p u) (geom* p u))))
                      0 1e-10 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error")))
 )

(slide
 #:title "Argh!"
 (para "Q. Is this normal???")
 (para "A. Yes. Most straightforward flonum function implementations have low error most places"
       "and high error (often unbounded) in others.")
 'next
 (para "The good news:" (bt "You can usually fix them using just flonum ops."))
 'next
 (para "Q. How do I fix them???")
 (para "A. Most functions---not implementations, but functions themselves---have places where"
       "they turn low input error into high output error. Avoid these badlands.")
 'next
 (para "Let's look at the badlands for the ops in" (code geom) "...")
 )

(slide
 #:title "The Badlands: Logarithm"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bflog (bf x))))
                        (flulp-error (fllog (fl x)) z*)))
                      0 10)))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bflog (bf x))))
               (flulp-error (fllog (fl x)) z*))
             0 10))
 )

(slide
 #:title "The Badlands: Subtraction"
 (para (code > (plot3d (contour-intervals3d
                        (λ (x y)
                          (define z* (- x y))
                          (flulp-error (fl- (fl x) (fl y)) z*))
                        -1 1 -1 1))))
 (plot3d-pict (contour-intervals3d
               (λ (x y)
                 (flulp-error (fl- (fl x) (fl y))
                              (- x y)))
               -1 1 -1 1))
 )

(slide
 #:title "The Badlands: Division"
 (plot3d-pict
  (contour-intervals3d
   (λ (x y)
     (define z* (/ x y))
     (flulp-error (fl/ (fl x) (fl y)) z*))
   -1 1 -1 1))
 (item (bt "No badlands:") "except for flonum rounding error, division error doesn't depend on"
       "inputs")
 (item "Multiplication error is the same way")
 )

(slide
 #:title "Informal Error Analysis"
 (item "Recursively reason about the body of" (code geom) ":")
 (para (code (define (geom p u)
               (fl/ (fllog u) (fllog (fl- 1.0 p))))))
 'next
 (item "Can't do anything about" (code fl/) "except make sure its arguments are as accurate as"
       "possible")
 'next
 (item "If" (code u) "is exact," (code (fllog u)) "has <= 0.5 ulps error")
 'next
 (item "If" (code p) "is exact," (code (fl- 1.0 p)) "has <= 0.5 ulps error")
 'next
 (item "If" (code p) "is exact and near" (code 0.0) "...")
 'next
 (subitem "Then" (code (fl- 1.0 p)) "is inexact and near" (code 1.0) "...")
 'next
 (subitem "So" (code (fllog (fl- 1.0 p))) "may have" (bt "high error"))
 'next
 (item "Let's check" (code math/flonum) "for a" (code fllog) "alternative")
 )

(slide
 #:title "log(1+x)"
 (item "Looks interesting:" (code fllog1p))
 (plot-pict (list (function (compose fllog1p fl) -1 9)
                  (axes))
            #:x-label "x"
            #:y-label "log(1+x)")
 'next
 (item "We can use it almost directly---mathematically,")
 ($"\\log(1-p) = \\log(1+(-p)) = \\mathrm{log1p}(-p)")
 )

(define (geom1 p u)
  (fl/ (fllog u) (fllog1p (- p))))

(slide
 #:title "Debugging: Geometric Inverse CDF (Second Stab)"
 (para (code (define (geom p u)
               (fl/ (fllog u) (fllog1p (- p))))))
 'alts
 (list
  (list (item "Error plot for" (code geom) "for" (code p <= 1) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom1 p u) (geom* p u))))
                      0 1 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 0.1) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom1 p u) (geom* p u))))
                      0 0.1 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 1e-5) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom1 p u) (geom* p u))))
                      0 1e-5 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error"))
  (list (item "Error plot for" (code geom) "for" (code p <= 1e-10) ":")
        (plot3d-pict (contour-intervals3d
                      (λ (p u)
                        (let ([p  (fl p)]
                              [u  (fl u)])
                          (flulp-error (geom1 p u) (geom* p u))))
                      0 1e-10 0 1)
                     #:x-label "p"
                     #:y-label "u"
                     #:z-label "ulps error")))
 )

(slide
 #:title "Argh It Is Not Perfect!"
 (item "But < 3 ulps error is really, really good")
 'next
 (item "Does" (code geom) "have badlands?")
 'next
 (plot3d-pict (contour-intervals3d
               (λ (p u)
                 (flulp-error (geom1 (fl p) (fl u)) (geom* p u)))
               0 1 0 1)
              #:x-label "p"
              #:y-label "u"
              #:z-label "ulps error")
 'next
 (item "This is a property of the" (bt "function") ", so we can't do anything about it")
 )

(slide
 #:title "Debugging Summary"
 (item "Make direct and reference implementations")
 'next
 (item "Repeat:")
 (subitem "Plot error, identify high-error regions")
 (subitem "Find badlands inputs, replace computations")
 'next
 (item "Avoid:")
 (subitem "Subtracting nearby values")
 (subitem "Taking logs of values near" (code 1.0))
 (subitem "Other badlands (e.g. very tiny square roots, most zero crossings)")
 'next
 (item "Move multiplication and division outward")
 )

(slide
 #:title "What If I Need Moar Bits?"
 
 (item (code racket/extflonum) ": 80-bit extended flonums")
 (subitem "Requires" (code (extflonum-available?) = #t))
 (subitem "64-bit significand, 15-bit exponent")
 (code (extfl->exact (extflexp (real->extfl 1/7))))
 'next
 (item (bt "double-double") "flonums: sum of two nonoverlapping flonums represent a number")
 (subitem "Requires correctly rounded arithmetic")
 (subitem "~105-bit significand, 11-bit exponent")
 (code (let*-values ([(x2 x1)  (fl2 1/7)]
                     [(y2 y1)  (fl2exp x2 x1)])
         (fl2->real y2 y1)))
 )

(slide
 #:title "What If I Need Moar Bits?"
 (item (code math/bigfloat) ": arbitrary precision floats")
 (subitem "Requires MPFR (Racket ships with libraries)")
 (subitem "Any size significand, 32-bit exponent")
 'next
 (item "Exact rationals")
 'next
 (item (bt "AN IMPORTANT DATA POINT:"))
 'next
 (soft-dropshadow (scale (hbl-append (bt "It takes 1074-bit bigfloats to fix ") (code geom)) 3/2))
 (soft-dropshadow (scale (para #:align 'center (bt "using") (code (bflog (bf- 1.bf p)))) 3/2))
 'next
 (item "Conclusion: ``moar bits'' is not a general solution")
 )

(slide
 #:title "The Badlands: Square Oot"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfsqrt (bf x))))
                        (flulp-error (flsqrt (fl x)) z*))
                      0 #,(code-literal "1e-320")))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfsqrt (bf x))))
               (flulp-error (flsqrt (fl x)) z*))
             0 1e-320))
 )

(slide
 #:title "The Badlands: Sine"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfsin (bf x))))
                        (flulp-error (flsin (fl x)) z*))
                      0 (* 2 pi)))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfsin (bf x))))
               (flulp-error (flsin (fl x)) z*))
             0 (* 2 pi)))
 )

(slide
 #:title "The Badlands: Cosine"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfcos (bf x))))
                        (flulp-error (flcos (fl x)) z*))
                      0 (* 2 pi)))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfcos (bf x))))
               (flulp-error (flcos (fl x)) z*))
             0 (* 2 pi)))
 )

(slide
 #:title "The Badlands: Tangent"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bftan (bf x))))
                        (flulp-error (fltan (fl x)) z*))
                      0 (* 2 pi)))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bftan (bf x))))
               (flulp-error (fltan (fl x)) z*))
             0 (* 2 pi)))
 )

(slide
 #:title "The Badlands: Arcsine"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfasin (bf x))))
                        (flulp-error (flasin (fl x)) z*))
                      -1 1))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfasin (bf x))))
               (flulp-error (flasin (fl x)) z*))
             -1 1))
 )

(slide
 #:title "The Badlands: Arccosine"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfacos (bf x))))
                        (flulp-error (flacos (fl x)) z*))
                      0.8 1))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfacos (bf x))))
               (flulp-error (flacos (fl x)) z*))
             0.8 1))
 )

(slide
 #:title "The Badlands: Arctangent"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfatan (bf x))))
                        (flulp-error (flatan (fl x)) z*))
                      -10 10))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfatan (bf x))))
               (flulp-error (flatan (fl x)) z*))
             -10 10))
 )

(slide
 #:title "The Badlands: Exponential"
 (para (code > (plot (function
                      (λ (x)
                        (define z* (bigfloat->real (bfexp (bf x))))
                        (flulp-error (flexp (fl x)) z*))
                      -100 100))))
 (plot-pict (function
             (λ (x)
               (define z* (bigfloat->real (bfexp (bf x))))
               (flulp-error (flexp (fl x)) z*))
             -100 100))
 )

(slide
 #:title "The Badlands: Exponential With Base"
 (para (code > (plot3d (contour-intervals3d
                        (λ (x y)
                          (define z* (bigfloat->real
                                      (bfexpt (bf x) (bf y))))
                          (flulp-error (flexpt (fl x) (fl y)) z*))
                        0 101 -101 101))))
 (plot3d-pict (contour-intervals3d
               (λ (x y)
                 (define z* (bigfloat->real (bfexpt (bf x) (bf y))))
                 (flulp-error (flexpt (fl x) (fl y)) z*))
               0 101 -101 101))
 )

(slide
 #:title "Estimating Badlands"
 (para (code (: relative-error-fun
                ((Real -> Real) Real -> (Real -> Real)))
             (define ((relative-error-fun f e) x)
               (define z* (f x))
               (define z+ (f (* x (+ 1 e))))
               (define z- (f (* x (- 1 e))))
               (max (relative-error z+ z*) (relative-error z- z*)))))

 (para (code (: relative-error-fun-2d
                ((Real Real -> Real) Real Real 
                                     -> (Real Real -> Real)))
             (define ((relative-error-fun-2d f ex ey) x y)
               (define z* (f x y))
               (define z++ (f (* x (+ 1 ex)) (* y (+ 1 ey))))
               (define z+- (f (* x (+ 1 ex)) (* y (- 1 ey))))
               (define z-+ (f (* x (- 1 ex)) (* y (+ 1 ey))))
               (define z-- (f (* x (- 1 ex)) (* y (- 1 ey))))
               (max (relative-error z++ z*) (relative-error z+- z*)
                    (relative-error z-+ z*) (relative-error z-- z*)))))
 )
