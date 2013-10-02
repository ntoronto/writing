#lang slideshow

(require slideshow/latex
         slideshow/extras
         slideshow/code
         slideshow/step
         math/flonum
         math/bigfloat
         math/distributions
         (except-in plot shade)
         unstable/gui/pict)

(define do-expensive-plots? #t)

(current-para-width (- 1024 64 64))

;; ===================================================================================================
;; Helpers

(get-current-code-font-size (λ () (exact-floor (* 5/6 (current-font-size)))))

(define (code-literal str)
  (colorize (text str (current-code-font) ((get-current-code-font-size)))
            (current-literal-color)))

(define-syntax-rule (with-steps* steps body ...)
  (map list (with-steps steps body ...)))

(define church (bitmap "portraits/Alonzo_Church.jpg"))
(define zermelo (bitmap "portraits/Ernst_Zermelo.jpeg"))
(define fraenkel (bitmap "portraits/Abraham_Fraenkel.jpg"))
(define monster (bitmap "portraits/monster.png"))

(define half-blank (blank (* 1/2 gap-size)))

(define-syntax-rule (targetlang)
  (hbl-append (t "λ") (with-style subscript (t "ZFC"))))

;; ===================================================================================================
;; LaTeX setup

(setup-local-latex-cache)

(latex-debug? #t)

(add-preamble #<<latex
\usepackage{amsmath, amssymb}
\renewcommand{\Re}{\mathbb{R}}
\newcommand{\Nat}{\mathbb{N}}
\newcommand{\powerset}{\mathcal{P}}
\renewcommand{\Pr}{\mathrm{Pr}}
\newcommand{\given}{\,|\,}
\newcommand{\set}[1]{\lbrace {#1} \rbrace}
\newcommand{\setb}[2]{\lbrace {#1} \ \lvert\ {#2} \rbrace}
\newcommand{\pto}{\rightharpoonup}
\newcommand{\meaningof}[1]{[\![{#1}]\!]}  % use in semantic functions
\newcommand{\Set}{\mathrm{Set}}
\newcommand{\Rect}{\mathrm{Rect}}
\newcommand{\pre}{_\mathrm{pre}}
\newcommand{\arrow}{\rightsquigarrow}
\newcommand{\arrowarr}{\ensuremath{\mathrm{arr}}}
\newcommand{\arrowcomp}{\ensuremath{{>}\mspace{-6mu}{>}\mspace{-6mu}{>}}}
\newcommand{\arrowpair}{\ensuremath{\mathrm{pair}}}
\DeclareMathOperator{\preto}{\arrow_{\mspace{-24mu}\pre}}
\usepackage{mathtools}
\DeclarePairedDelimiter{\ceil}{\lceil}{\rceil}
\DeclarePairedDelimiter{\floor}{\lfloor}{\rfloor}
\DeclarePairedDelimiter{\pair}{\langle}{\rangle}
latex
              )

;; ===================================================================================================
;; Slides

(background-image (bitmap "title-background.png"))

(slide
 (scale/improve-new-text (soft-dropshadow (bt "Running Probabilistic")) 2)
 (scale/improve-new-text (soft-dropshadow (bt "Programs Backward")) 2)
 (hrule)
 (scale (bt "Neil Toronto") 1.5)
 (scale (bt "PLT @ Brigham Young University") 1.5)
 )

(background-image (bitmap "slides-background.png"))

(slide-number 1)
(slide
 #:title "Master's Research: Super-Resolution"
 (scale (bitmap "overview.png") 1/3)
 (blank 10)
 (scale
  (para "Toronto et al. Super-Resolution via Recapture and Bayesian Effect Modeling. CVPR 2009")
  2/3)
 )

(add1-slide-number)
(slide
 #:title "Master's Research: Super-Resolution"
 (item "Bayesian Edge Inference (BEI) graphical model:")
 (scale (bitmap "models-2.png") 1/3)
 'next
 (item "Objective: Obtain one sample from" ($"I' \\given I"))
 'next
 (item "Model and query: Half a page of beautiful math")
 'next
 (item "Query implementation: 600 lines of evil Python")
 )

(add1-slide-number)
(slide
 #:title "Main Results: Super-Resolution"
 'alts
 (with-steps*
  (s1 s2 s3)
  (ht-append
   (vc-append (bitmap "super-res/rs-4.png")
              (scale (bt "Resolution Synthesis") 2/3))
   (blank 20)
   ((vafter s2)
    (vc-append (bitmap "super-res/lcsr-4.png")
               (scale (bt "Local Correlation") 2/3)))
   (blank 20)
   ((vafter s3)
    (vc-append (bitmap "super-res/bei-4.png")
               (scale (bt "Bayesian Edge Inference") 2/3)))))
 'next
 (item "Also beat state-of-the-art on ``objective'' measures")
 )

(add1-slide-number)
(slide
 #:title "Pleasantly Surprising Results: Inpainting"
 (item "Dr. Morse: ``Hey, doesn't Bayesian inference handle missing data easily?''")
 'next
 (item "Me: Yep. Gimme 20 minutes...")
 'next
 'alts
 (with-steps*
  (s1 s2 s3)
  (ht-append
   (vc-append (bitmap "inpainting/orig.png")
              (scale (bt "Original Image") 2/3))
   (blank 20)
   ((vafter s2)
    (vc-append (bitmap "inpainting/defaced-33.png")
               (scale (bt "33% Defaced") 2/3)))
   (blank 20)
   ((vafter s3)
    (vc-append (bitmap "inpainting/bei.png")
               (scale (bt "Bayesian Edge Inference") 2/3)))))
 )

(add1-slide-number)
(slide
 #:title "Pleasantly Surprising Results: CCD Demosaicing"
 (item "Dr. Morse: ``You know, you can think of the result of Bayer filtering on a consumer camera as"
       "missing data.''")
 (hc-append (t "Bayer filter:") (blank 20) (scale (bitmap "ccd/Bayer_pattern_on_sensor.png") 1/3))
 'next
 (item "Me: Huh. Gimme 20 minutes...")
 'next
 'alts
 (with-steps*
  (s1 s2 s3)
  (ht-append
   (vc-append (scale (bitmap "ccd/small_orig.png") 1/2)
              (scale (bt "Original Image") 2/3))
   (blank 20)
   ((vafter s2)
    (vc-append (scale (bitmap "ccd/small_bayer.png") 1/2)
               (scale (bt "Bayer Filtered") 2/3)))
   (blank 20)
   ((vafter s3)
    (vc-append (scale (bitmap "ccd/small_bei.png") 1/2)
               (scale (bt "Bayesian Edge Inference") 2/3)))))
 )

(add1-slide-number)
(slide
 #:title "Only Mostly Satisfying"
 (para "Reason 1: Still not sure the program is right")
 'next
 (hrule)
 (para "Scene: Markov random field of overlapping facets:")
 (scale (bitmap "scene-modeling-decisions-2.png") 1/2)
 'next
 (para "Reason 2: ``To" (it "approximate") "blurring with a spatially varying point-spread function"
       "(PSF), we assign each facet a Gaussian PSF and convolve each analytically"
       (it "before combining outputs") ".''")
 )

(define (f x y)
  (let ([x  (fl x)] [y  (fl y)])
    (fl* (flnormal-pdf 0.0 1.0 x #f)
         (flnormal-pdf x 1.0 y #f))))

(add1-slide-number)
(slide
 #:title "Simple Example Theory"
 'next
 (item "Example theory: Normal-Normal")
 ($$ #<<latex
\begin{aligned}
  X &\sim \mathrm{Normal}(0,1) \\
  Y &\sim \mathrm{Normal}(X,1) \\
\end{aligned}
latex
     )
 'next
 (item "Density model" ($"f : \\Re \\times \\Re \\to [0,\\infty)") ":")
 (inset/clip (bitmap (plot3d-bitmap (contour-intervals3d f -3 3 -3 3)))
             0 -60 0 0)
 )

(define (plot-constrained-norm-norm-density ε prob)
  (inset/clip
   (bitmap
    (plot3d-bitmap
     (contour-intervals3d
      (λ (x y)
        (if ((abs (- (sqrt (+ (* x x) (* y y))) 1.0)) . <= . ε)
            (/ (f x y) prob)
            0.0))
      -1.5 1.5 -1.5 1.5
      #:line-styles '(transparent)
      #:samples 80)))
   0 -60 0 0))

(add1-slide-number)
(slide
 #:title "An Observation Problem"
 'alts
 (with-steps*
  (s1 s2)
  (cc-superimpose
   ((only s2 (λ (p) (cross-out p #:width 3 #:color "firebrick")))
    (item "Find the distribution of " ($"X,Y \\given X+Y=1")))
   ((vafter s2)
    (rotate (colorize (outline (bt "MIGHT BE TOO EASY")) "red")
            (degrees->radians 5)))))
 'next
 (item "Find the distribution of " ($"X,Y \\given \\sqrt{X^2+Y^2}=1"))
 'next
 (item "Maybe take a limit of" ($"|\\sqrt{X^2+Y^2}-1| < \\epsilon") "?")
 'next
 'alts
 (if do-expensive-plots?
     (list (list (para "Conditional density with" ($"\\epsilon = 0.5") ":")
                 (plot-constrained-norm-norm-density 0.5 0.476))
           (list (para "Conditional density with" ($"\\epsilon = 0.25") ":")
                 (plot-constrained-norm-norm-density 0.25 0.251))
           (list (para "Conditional density with" ($"\\epsilon = 0.1") ":")
                 (plot-constrained-norm-norm-density 0.1 0.102))
           (list (para "Conditional density with" ($"\\epsilon = 0.025") ":")
                 (plot-constrained-norm-norm-density 0.025 0.0255)))
     empty)
 )

;; TODO: Pictures of things densities can't model?
(add1-slide-number)
(slide
 #:title "What Can't Densities Model?"
 (item "Anything that puts nonzero probability on a zero-volume domain subset")
 'next
 (subitem "Non-axis-aligned, zero-probability conditions (can't reduce dimension)")
 'next
 (subitem "CDFs with discontinuities")
 'next
 (subitem "Discontinuous change of variable")
 'next
 (item "Distributions with variable-dimension support")
 'next
 (item "Nontrivial distributions on infinite products like" ($"[0,1]^\\Nat"))
 'next
 (para "There are tricks to get around limitations, but none are generally applicable...")
 )

(add1-slide-number)
(slide
 #:title "Measure-Theoretic Probability"
 (item "Main ideas:")
 (subitem "Don't assign numbers to" (it "changes") "in value, assign numbers to" (it "sets")
          "of values")
 'next
 (subitem "Confine assumed randomness to one place by making random variables"
          (it "deterministic") (it "functions") "that observe a random source")
 'next
 (item "Measure-theoretic model of example theory:")
 'alts
 (list
  (list
   ($$ #<<latex
\begin{aligned}
  \Omega &= \Re \times \Re \\
  P &: \mathrm{Set}(\Omega) \pto [0,1],\ P(A) = \textstyle\int_A f\ d\lambda
\end{aligned}
latex
       ))
  (list
   ($$ #<<latex
\begin{aligned}
  \Omega &= \Re \times \Re \\
  P &: \mathrm{Set}(\Omega) \pto [0,1],\ P(A) = \textstyle\int_A f\ d\lambda \\
  X &: \Omega \to \Re,\ X(\omega) = \omega_0 \\
  Y &: \Omega \to \Re,\ Y(\omega) = \omega_1
\end{aligned}
latex
       )))
 )

(add1-slide-number)
(slide
 #:title "Measure-Theoretic Queries"
 (item "Specific query:")
 ($"\\Pr[X > 1] = P(\\setb{\\omega \\in \\Omega}{X(\\omega) > 1})")
 'next
 (item "Generalized: For" ($"Z : \\Omega \\to C") "and" ($"C' \\subseteq C") ",")
 ($"\\Pr[Z \\in C'] = P(\\setb{\\omega \\in \\Omega}{Z(\\omega) \\in C'})")
 'next
 (item "Conditional query:")
 (para #:align 'center
       ($"\\Pr[e_1 \\given e_2] = \\Pr[e_1, e_2] / \\Pr[e_2]")
       (blank 10)
       "if "
       ($"\\Pr[e_2] > 0"))
 'next
 (hrule)
 (para "But" ($"\\Pr[\\sqrt{X^2+Y^2}=1] = 0") "...")
 )

(define (plot-axial-condition eps)
  (bitmap
   (plot-bitmap
    (list
     (contours f -3 3 -3 3 #:levels 5)
     (rectangles (list (list (ivl -3 3) (ivl (- 2 eps) (+ 2 eps))))
                 #:color 2 #:line-color 2 #:alpha 0.75
                 #:label "Condition set {ω ∈ Ω | |Y(ω)-2| < ε}")
     (rectangles (list (list (ivl 1 3) (ivl -3 3)))
                 #:color 3 #:line-color 3 #:alpha 0.5
                 #:label "Query set {ω ∈ Ω | X(ω) > 1}")
     (rectangles (list (list (ivl 1 3) (ivl (- 2 eps) (+ 2 eps))))
                 #:style 'transparent #:line-color 5 #:line-style 'long-dash #:line-width 2
                 ))
    #:legend-anchor 'bottom-left
    #:x-label "ω0 axis"
    #:y-label "ω1 axis")))

;; TODO: pictures of limit
(add1-slide-number)
(slide
 #:title "Zero-Probability Conditions (Axial)"
 (item "Observation query:")
 'alts
 (list (list ($$ #<<latex
\begin{aligned}
  \Pr[X > 1 \given Y = 2]\ &=\ \lim_{\epsilon \to 0} \Pr[X > 1 \given |Y-2| < \epsilon]
\end{aligned}
latex
                 ))
       (list ($$ #<<latex
\begin{aligned}
  \Pr[X > 1 \given Y = 2]\ &=\ \lim_{\epsilon \to 0} \Pr[X > 1 \given |Y-2| < \epsilon]
\\
  &=\ \lim_{\epsilon \to 0} \frac{\Pr[X > 1, |Y-2| < \epsilon]}{\Pr[|Y-2| < \epsilon]}
\end{aligned}
latex
                 )))
 'next
 'alts
 (list (list (plot-axial-condition 0.5))
       (list (plot-axial-condition 0.25))
       (list (plot-axial-condition 0.1))
       (list (plot-axial-condition 0.025)))
 )

(define (d x y)
  (- (sqrt (+ (sqr x) (sqr y))) 1))

(define (plot-circular-condition eps)
  (bitmap
   (plot-bitmap
    (list
     (contours f -3 3 -3 3 #:levels 5)
     (contour-intervals d -3 3 -3 3
                        #:levels (list (- eps) eps)
                        #:colors '(white 2 white)
                        #:styles '(transparent solid transparent)
                        #:alphas '(0.75)
                        #:contour-styles '(solid)
                        #:contour-colors '(2)
                        ;#:label "Condition set"
                        )
     (rectangles (list (list (ivl 0 2) (ivl 0 2)))
                 #:color 3 #:line-color 3 #:alpha 0.5
                 ;#:label "Query set"
                 )
     (contour-intervals d 0 2 0 2
                        #:levels (list (- eps) eps)
                        #:styles '(transparent)
                        #:contour-widths '(2)
                        #:contour-styles '(long-dash)
                        #:contour-colors '(5))
     )
    #:x-label "ω0 axis"
    #:y-label "ω1 axis")))

(add1-slide-number)
(slide
 #:title "Zero-Probability Conditions (Circular)"
 (item "Can differentiate w.r.t. any random variable:")
 ($"\\Pr[e \\given \\sqrt{X^2+Y^2} = 1]\\ =\\ \\displaystyle\\lim_{\\epsilon \\to 0}"
   "\\Pr[e \\given |\\sqrt{X^2+Y^2}-1| < \\epsilon]")
 'next
 'alts
 (list (list (plot-circular-condition 0.5))
       (list (plot-circular-condition 0.25))
       (list (plot-circular-condition 0.1))
       (list (plot-circular-condition 0.025)))
 #|
 (item "Differentiating for conditional density (if it exists):")
 ($"f(x|y) = \\displaystyle\\lim_{\\delta \\to 0}"
   "\\dfrac"
   "{\\Pr[|X - x| < \\delta \\given Y = y]}"
   "{\\lambda((x - \\delta, x + \\delta))}")
|#
 )

(add1-slide-number)
(slide
 #:title "Measure-Theoretic Models"
 (item "Random variables and" ($"\\Pr[\\cdot]") "are an abstraction boundary that hides"
       ($"\\Omega") "and" ($"P"))
 'next
 (item "Queries return the same answers for any model whose random variables' outputs have"
       "the correct joint distribution")
 'next
 (para "Let" ($"F : \\Re \\to [0,1]") "be the Normal CDF, and define a"
       (bt "uniform random source") "model:")
 'alts
 (list
  (list
   ($$ #<<latex
\begin{aligned}
  \Omega &= [0,1] \times [0,1] \\
  P &: \mathrm{Set}(\Omega) \pto [0,1],\ P(A) = \lambda(A)\ \ \text{(i.e. $A$'s area)} \\
\end{aligned}
latex
       ))
  (list
   ($$ #<<latex
\begin{aligned}
  \Omega &= [0,1] \times [0,1] \\
  P &: \mathrm{Set}(\Omega) \pto [0,1],\ P(A) = \lambda(A)\ \ \text{(i.e. $A$'s area)} \\
  X &: \Omega \to \Re,\ X(\omega) = F^{-1}(\omega_0) \\
  Y &: \Omega \to \Re,\ Y(\omega) = F^{-1}(\omega_1) + X(\omega)
\end{aligned}
latex
       )))
 )

(add1-slide-number)
(slide
 #:title "Now We Finally Get to Running Things Backward"
 (item "Generalized query:")
 'alts
 (list
  (list
   ($$ #<<latex
\begin{aligned}
  \Pr[Z \in C']
    &= P(\setb{\omega \in \Omega}{Z(\omega) \in C'})
\end{aligned}
latex
       ))
  (list
   ($$ #<<latex
\begin{aligned}
  \Pr[Z \in C']
    &= P(\setb{\omega \in \Omega}{Z(\omega) \in C'}) \\
    &= P(Z^{-1}(C'))
\end{aligned}
latex
       )))
 (para "i.e. output distributions are defined by" (bt "preimages"))
 'next
 (item "For a uniform random source model,")
 (subitem "Compute output probabilities by computing" (bt "preimage") "areas")
 'next
 (subitem "Compute conditional probabilities as quotients of" (bt "preimage") "areas")
 'next
 (subitem "Sample conditional distributions by sampling uniformly from" (bt "preimages")
          "of conditions")
 )

(add1-slide-number)
(slide
 #:title "Is Your Crazy Idea Even Feasible?"
 'next
 (item "Maybe we can find out by looking at preimages")
 'next
 (item "Normal-Normal random variables in uniform source model:")
 ($$ #<<latex
\begin{aligned}
  X &: \Omega \to \Re,\ X(\omega) = F^{-1}(\omega_0) \\
  Y &: \Omega \to \Re,\ Y(\omega) = F^{-1}(\omega_1) + X(\omega)
\end{aligned}
latex
     )
 'next
 (item "Random variable for the unit circle condition:")
 ($"Z : \\Omega \\to \\Re,\\ Z(\\omega) = |\\sqrt{X(\\omega)^2 + Y(\\omega)^2} - 1|")
 'next
 (item "What do preimages under" ($"Z") "look like?"
       "How hard would they be to represent and sample from uniformly?")
 )

(define (norm-norm-condition-dist ω0 ω1)
  (let ([ω0  (fl ω0)]
        [ω1  (fl ω1)])
    (let* ([x  (flnormal-inv-cdf 0.0 1.0 ω0 #f #f)]
           [y  (fl+ x (flnormal-inv-cdf 0.0 1.0 ω1 #f #f))])
      (- (sqrt (+ (* x x) (* y y))) 1.0))))

(define (plot-constrained-norm-norm-source ε)
  (bitmap
   (plot-bitmap
    (contour-intervals norm-norm-condition-dist 0 1 0 1
                       #:levels (list (- ε) ε)
                       #:colors '(white 2 white)
                       #:styles '(transparent solid transparent)
                       #:contour-styles '(solid)
                       #:contour-colors '(2)
                       #:samples 81)
    #:x-label "ω0 axis"
    #:y-label "ω1 axis")))

(add1-slide-number)
(slide
 #:title "Preimages Under the Unit Circle Condition"
 (item "Random variable for the unit circle condition:")
 ($"Z(\\omega) = |\\sqrt{X(\\omega)^2 + Y(\\omega)^2} - 1|")
 'alts
 (list (list (para ($"Z^{-1}([0,\\epsilon))") "," ($"\\epsilon=0.5") ":")
             (plot-constrained-norm-norm-source 0.5))
       (list (para ($"Z^{-1}([0,\\epsilon))") "," ($"\\epsilon=0.25") ":")
             (plot-constrained-norm-norm-source 0.25))
       (list (para ($"Z^{-1}([0,\\epsilon))") "," ($"\\epsilon=0.1") ":")
             (plot-constrained-norm-norm-source 0.1))
       (list (para ($"Z^{-1}([0,\\epsilon))") "," ($"\\epsilon=0.025") ":")
             (plot-constrained-norm-norm-source 0.025)))
 )

(add1-slide-number)
(slide
 #:title "Feasible If..."
 (item "Seems like we need:")
 (subitem "Standard interpretation of programs as pure functions from a random source")
 'next
 (subitem "Efficient way to compute preimage sets")
 'next
 (subitem "Efficient representation of arbitrary sets")
 'next
 (subitem "Efficient way to sample uniformly in preimages")
 'next
 (subitem "Proof of correctness w.r.t. standard interpretation")
 'next
 (item "Exact implementation is out of reach, but...")
 )

(add1-slide-number)
(slide
 #:title "What About Approximating?"
 (para "Conservative approximation with rectangles:")
 'alts
 (list (list (bitmap "refinement/constrained-norm-norm-source-00.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-01.png")))
 )

(add1-slide-number)
(slide
 #:title "What About Approximating?"
 (para "Restricting preimages to rectangular subdomains:")
 'alts
 (list (list (bitmap "refinement/constrained-norm-norm-source-02.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-03.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-04.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-05.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-06.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-07.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-08.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-09.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-10.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-11.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-12.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-13.png")))
 )

(add1-slide-number)
(slide
 #:title "What About Approximating?"
 (para "Sampling to overcome exponential explosion:")
 'alts
 (list (list (bitmap "refinement/constrained-norm-norm-source-sample.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-sample-point.png")))
 )

(add1-slide-number)
(slide
 #:title "For Conditioned Sampling, We Need..."
 (item "Standard interpretation of programs as pure functions from a random source")
 'alts
 (with-steps
   (a b c d e)
   (list
    (if (before? b)
        (item "Efficient way to compute preimage sets")
        (item "Efficient way to compute" (colorize (t "approximate") "red") "preimage"
              (colorize (t "subsets") "red")))
    (if (before? c)
        (item "Efficient representation of arbitrary sets")
        (item "Efficient representation of" (colorize (t "approximating") "red") "sets"))
    (item "Efficient way to sample uniformly in preimages")
    ((vafter d)
     (colorize (subitem "Efficient domain partition sampling") "red"))
    ((vafter e)
     (colorize
      (subitem "Efficient way to determine whether a domain sample is actually in the preimage"
               (colorize (t "(just use standard interpretation)") "black"))
      "red"))
    (item "Proof of correctness w.r.t. standard interpretation")))
 )

(add1-slide-number)
(slide
 #:title "Moar Precision: Standard Interpretation"
 (item "Grammar:")
 ($$ #<<latex
\begin{aligned}
  p &\ ::=\  x := e;\ \cdots;\ x := e;\ e \\
  e &\ ::=\  x~e \given \mathrm{if}~e~e~e \given \mathrm{let}~e~e \given \mathrm{env}~n \given
    \pair{e,e} \given \delta~e \given v \\
  x &\ ::=\  [\text{first-order function names}] \\
  \delta &\ ::=\  [\text{primitive function names}] \\
  v &\ ::=\  [\text{first-order values}]
\\
\end{aligned}
latex
     )
 'next
 (item "Semantic function" ($"\\meaningof{\\cdot} : p \\to (\\Omega \\to B)"))
 'next
 (item "Math has no general recursion, so" ($"\\meaningof{p}") "(i.e. interpretation of"
       "program" ($"p") ") is a" (it "λ-calculus") "term")
 'next
 (item "Easy implementation: " ($"\\meaningof{\\cdot}") "becomes a Racket macro")
 )

(add1-slide-number)
(slide
 #:title "Why a First-Order Language?"
 'next
 (item "Short answer: Measure theory is essentially first-order")
 'next
 (item "Longer answer: It is difficult to assign probabilities to sets of functions")
 'next
 (item "Precise answer: For uncountable Borel spaces" ($"A") "and" ($"B")
       ", there is no σ-algebra for the set of measurable functions in"
       ($"A \\to B") "for which" ($"\\mathrm{app} : (A \\to B) \\times A \\to B") "is measurable")
 'next
 (item "Short version: If you stick to standard measurable sets, the results of applying a random"
       "function to a random value can't have a sensible distribution")
 'next
 (item "It's okay, we can defunctionalize lambdas away (closures are just products)")
 )

(add1-slide-number)
(slide
 #:title "Moar Precision: Nonstandard Interpretation"
 (item "Main idea: Compute preimages compositionally")
 'next
 (item "Preimage computation type constructor:")
 ($"A \\preto B ::= \\Set(A) \\to \\pair{\\Set(B), \\Set(B) \\to \\Set(A)}")
 'next
 (item "Pair preimage combinator type:")
 ($"\\arrowpair\\pre : (A \\preto B_1) \\to (A \\preto B_2) \\to (A \\preto \\pair{B_1,B_2})")
 'next
 (para (bt "Theorem.") "If")
 (item ($"h_1 : A \\preto B_1") "computes preimages under" ($"f_1 : A \\to B_1"))
 'next
 (item ($"h_2 : A \\preto B_2") "computes preimages under" ($"f_2 : A \\to B_2"))
 'next
 (para "then" ($"\\arrowpair\\pre~h_1~h_2") "computes preimages under"
       ($"a \\mapsto \\pair{f_1(a),f_2(a)}") ".")
)

(define (d1 x y)
  (- (+ x y) 0.6))

(define (d2 x y)
  (* 4.0 (- (* x y) 0.075)))

(define (d3 x y)
  (define z1 (d1 x y))
  (define z2 (d2 x y))
  (if ((abs z1) . > . (abs z2)) z1 z2))

(define d1-renderer
  (contour-intervals d1 0 1 0 1
                     #:levels (list -0.1 0.1)
                     #:colors '(white 2 white)
                     #:styles '(transparent solid transparent)
                     #:contour-styles '(solid)
                     #:contour-colors '(2)))

(define d2-renderer
  (contour-intervals d2 0 1 0 1
                     #:levels (list -0.1 0.1)
                     #:colors '(white 3 white)
                     #:styles '(transparent solid transparent)
                     #:contour-styles '(solid)
                     #:contour-colors '(3)))

(define d3-renderer
  (contour-intervals d3 0.081 0.615 0.081 0.615
                     #:levels (list -0.1 0.1)
                     #:colors '(white 1 white)
                     #:styles '(transparent solid transparent)
                     #:contour-styles '(solid)
                     #:contour-colors '(1)
                     #:samples 81))

(define d1-plot
  (bitmap (plot-bitmap d1-renderer #:x-label "ω0 axis" #:y-label "ω1 axis")))

(define d12-plot
  (bitmap (plot-bitmap (list d1-renderer d2-renderer)
                       #:x-label "ω0 axis"
                       #:y-label "ω1 axis")))

(define d123-plot
  (bitmap
   (plot-bitmap (list d1-renderer d2-renderer d3-renderer)
                #:x-label "ω0 axis"
                #:y-label "ω1 axis"
                #:x-min 0 #:x-max 1
                #:y-min 0 #:y-max 1)))

(define d3-plot
  (bitmap
   (plot-bitmap d3-renderer
                #:x-label "ω0 axis"
                #:y-label "ω1 axis"
                #:x-min 0 #:x-max 1
                #:y-min 0 #:y-max 1)))

(add1-slide-number)
(slide
 #:title "Pair Preimages"
 (para #:align 'center
       ($"f_1(\\omega) = \\omega_0 + \\omega_1")
       (blank 40)
       ($"f_2(\\omega) = \\omega_0 \\cdot \\omega_1"))
 'next
 'alts
 (list (list (para ($"f_1^{-1}([0.5,0.7])") ":")
             d1-plot)
       (list (para ($"f_1^{-1}([0.5,0.7])") "and" ($"f_2^{-1}([0.05,0.1])") ":")
             d12-plot)
       (list (para ($"f^{-1}([0.5,0.7] \\times [0.05,0.1])") " where"
                   ($"f(\\omega) = \\pair{f_1(\\omega),f_2(\\omega)}") ":")
             d123-plot)
       (list (para ($"f^{-1}([0.5,0.7] \\times [0.05,0.1])") " where"
                   ($"f(\\omega) = \\pair{f_1(\\omega),f_2(\\omega)}") ":")
             d3-plot))
 'next
 (item ($"\\arrowpair\\pre") "does this, but for arbitrary output sets")
 )

(add1-slide-number)
(slide
 #:title "Compositional Semantics"
 (item "Semantic function"
       ($"\\meaningof{\\cdot}\\pre : p \\to (\\Omega \\preto B)"))
 'next
 (subitem "Compare" ($"\\meaningof{\\cdot} : p \\to (\\Omega \\to B)"))
 'next
 (item "Compositional definition; e.g.")
 ($"\\meaningof{\\pair{e_1,e_2}}\\pre = \\arrowpair\\pre~\\meaningof{e_1}\\pre~\\meaningof{e_2}\\pre")
 'next
 (para (bt "Corollary.") "If")
 (item ($"\\meaningof{e_1}\\pre") "computes preimages under" ($"\\meaningof{e_1}"))
 (item ($"\\meaningof{e_2}\\pre") "computes preimages under" ($"\\meaningof{e_2}"))
 (para "then" ($"\\meaningof{\\pair{e_1,e_2}}\\pre") "computes preimages under"
       ($"\\meaningof{\\pair{e_1,e_2}}") ".")
 )

(add1-slide-number)
(slide
 #:title "Nonstandard Interpretation Correctness"
 
 (para (bt "Theorem.") "For all programs" ($"p") ","
       ($"\\meaningof{p}\\pre") "computes preimages under" ($"\\meaningof{p}") ".")
 (para (it "Proof.") "By structural induction on program terms.")
 )

(add1-slide-number)
(slide
 #:title "Wait a Minute"
 (item "Q. Don't the interpretations of" ($"\\meaningof{\\cdot}\\pre") "do uncomputable things?")
 'next
 (subitem "A. Yes. Yes, they do.")
 'next
 (item "Q. Where do I get a computer that runs them?")
 'next
 (subitem "A. Nowhere, but we can approximate them.")
 'next
 (item "Q. Where did you get a λ-calculus that could operate on arbitrary, possibly infinite sets,"
       "anyway?")
 (subitem "A. Well...")
 )

(add1-slide-number)
(slide
 #:title "Lambda-ZFC"
 (para "N. Toronto and J. McCarthy. Computing in Cantor's Paradise With" (targetlang) ". FLOPS 2012")
 'next
 'alts
 (map
  list
  (with-steps (s1 s2 s3)
    (hc-append ((vafter s1)
                (vc-append church
                           (blank (* 1/2 gap-size))
                           (scale/improve-new-text (t "λ calculus") 1/2)))
               ((vafter s2)
                (hc-append
                 half-blank (t "+") half-blank
                 (vc-append (hc-append zermelo (blank (* 1/2 gap-size)) fraenkel)
                            (blank (* 1/2 gap-size))
                            (scale/improve-new-text (t "Set theory") 1/2))))
               ((vafter s3)
                (hc-append
                 half-blank (t "=") half-blank
                 (vc-append monster
                            (blank (* 1/2 gap-size))
                            (scale/improve-new-text (targetlang) 1/2)))))))
 'next
 (item "Contemporary math, but with lambdas and general recursion; or functional programming,"
       "but with infinite sets")
 'next
 (item "Can use essentially all contemporary mathematical theorems")
 'next
 (item "New theorems apply in contemporary mathematics*")
 'next
 (scale (para "* assuming the existence of an inaccessible cardinal") 2/3)
 )

(add1-slide-number)
(slide
 #:title "Everything Is Measurable"
 (item "``Measurable'' means ``can have a sensible distribution''")
 'next
 (para (bt "Theorem.") "If every primitive function (i.e. those with names in" ($"\\delta")
       ") is measurable w.r.t. standard σ-algebras, then" ($"\\meaningof{p}")
       "is measurable w.r.t. standard σ-algebras, regardless of nontermination.")
 'next
 (para (bt "Corollary.") "The halting set of every program is measurable.")
 'next
 (item "Requires only that the standard σ-algebra of" ($"\\mathrm{Bool}") "is"
       ($"\\powerset(\\mathrm{Bool})")
       "(i.e." ($"\\mathrm{true}") "and" ($"\\mathrm{false}") "are distinguishable)")
 'next
 (item "Includes uncomputable primitives like real equality and limits")
 'next
 (item "Applies to all probabilistic language work to date and in the forseeable future")
 )

(add1-slide-number)
(slide
 #:title "Back To Approximation"
 (item ($"\\meaningof{\\cdot}\\pre : p \\to (\\Omega \\preto B)") "returns uncomputable functions")
 'next
 (item "Recall:")
 ($"A \\preto B ::= \\Set(A) \\to \\pair{\\Set(B), \\Set(B) \\to \\Set(A)}")
 'next
 (item "Define:")
 ($"A \\preto' B ::= \\Rect(A) \\to \\pair{\\Rect(B), \\Rect(B) \\to \\Rect(A)}")
 'next
 (para "as well as" ($"\\Rect") "intersection, join (union-like), empty test, products, etc.")
 'next
 (item "Derive" ($"\\meaningof{\\cdot}'\\pre : p \\to (\\Omega \\preto' B)"))
 )

(add1-slide-number)
(slide
 #:title "In Theory..."
 (para (bt "Theorem (sound).") ($"\\meaningof{\\cdot}'\\pre")
       "computes overapproximations of the preimages computed by"
       ($"\\meaningof{\\cdot}\\pre") ".")
 (item "Consequence: Sampling within preimages doesn't leave anything out")
 'next
 (para (bt "Theorem (monotone).") ($"\\meaningof{\\cdot}'\\pre") "is monotone.")
 (item "Consequence: Partitioning the domain never increases approximate preimages")
 'next
 (para (bt "Theorem (decreasing).") ($"\\meaningof{\\cdot}'\\pre") "never returns preimages larger"
       "than the given subdomain.")
 (item "Consequence: Refining preimage partitions never explodes")
 )

(add1-slide-number)
(slide
 #:title "In Practice..."
 (para "Theorems prove this always works:")
 'alts
 (list (list (bitmap "refinement/constrained-norm-norm-source-01.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-02.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-03.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-04.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-05.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-06.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-07.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-08.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-09.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-10.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-11.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-12.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-13.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-sample.png"))
       (list (bitmap "refinement/constrained-norm-norm-source-sample-point.png")))
 )

(add1-slide-number)
(slide
 #:title "Algorithm 2"
 (item "Alternative to arbitrarily low-rate rejection sampling:")
 'next
 'alts
 (list (list (para "First, refine using preimage computation:")
             (bitmap "refinement/constrained-norm-norm-source-01.png"))
       (list (para "Second, randomly choose from arbitrarily fine partition:")
             (bitmap "lw-refinement/constrained-norm-norm-source-02.png"))
       (list (para "Third, refine again:")
             (bitmap "lw-refinement/constrained-norm-norm-source-03.png"))
       (list (para "Fourth, sample uniformly:")
             (bitmap "lw-refinement/constrained-norm-norm-source-04.png"))
       (list (para "Do process ``in the limit''; i.e. choose"
                   ($"[\\omega_0,\\omega_0] \\times \\Omega_1") ":")
             (bitmap "lw-refinement/constrained-norm-norm-source-05.png")))
 )

(add1-slide-number)
(slide
 #:title "Likelihood Weighting?"
 (item "Each sample must be weighted by the inverse of probability it was chosen (i.e. sampling"
       "algorithm is an importance distribution)")
 'next
 (item "Conjecture: Algorithm 2 approaches likelihood weighting (with densities for absolutely"
       "continuous distributions) as"
       ($"\\epsilon \\to 0"))
 'next
 (item "Do better than LW by combining Algorithm 1 and Algorithm 2")
 )

(add1-slide-number)
(slide
 #:title "Floating-Point Sensitivity and Accuracy"
 (item "Algorithm 2 is" (it "extremely") "sensitive to floating-point error")
 'next
 (item "Requires either perfectly outwardly rounded interval arithmetic for monotone functions:")
 (code > (ivl (exp- 1.1) (exp+ 1.1))
       (ivl 3.004166023946433 3.0041660239464334)
       > (flonums-between 3.004166023946433 3.0041660239464334)
       1)
 'next
 (item "Or a bound on its implementations' error to overapproximate upward- and downward-rounded"
       "implementations:")
 (code > (ivl (norm-inv-cdf- 0.8) (norm-inv-cdf+ 0.8))
       (ivl 0.841621233572914 0.8416212335729149)
       > (flonums-between 0.841621233572914 0.8416212335729149)
       8)
 )

(add1-slide-number)
(slide
 #:title "What About Variable-Dimension Models?"
 (item "General recursion, programs that halt with probability 1; e.g.")
 (code (define/drbayes (geometric p)
         (if (bernoulli p) 0 (+ 1 (geometric p)))))
 'next
 (item "I lied about program domain: it's" ($"\\Omega \\times T"))
 'next
 (item "Every" (bt "branch trace") ($"t \\in T") "determines which branch each" (code if)
       "expression takes in an" (it "infinite, fully inlined") "program")
 (code (if (bernoulli p)
           0
           (+ 1 (if (bernoulli p)
                    0
                    (+ 1 ...)))))
 'next
 (item "Computing preimages often decides branching; it's randomly forced otherwise")
 )

(add1-slide-number)
(slide
 #:title "Domain Values"
 (item "Values" ($"\\omega \\in \\Omega") "are infinite binary trees:")
 (scale (bitmap "representation/omega-value.png") 3/4)
 'next
 (item "Implemented using lazy trees of random values")
 'next
 (item "Each" ($"t \\in T") "is an infinite tree of branch choices")
 )

(add1-slide-number)
(slide
 #:title "Domain Rectangles"
 (item "Rectangles" ($"\\Omega' \\subseteq \\Omega") "are products of tree axes:")
 (scale (bitmap "representation/omega-rectangle.png") 3/4)
 'next
 (item "Implemented using finite trees of real sets")
 'next
 (item "Rectangles" ($"T' \\subseteq T") "are similar")
 )

(add1-slide-number)
(slide
 #:title "Solution Summary (1)"
 (item "Standard interpretation" ($"\\meaningof{\\cdot}"))
 (subitem "Directly implementable (with floating-point approximation)")
 'next
 (item "Nonstandard interpretation" ($"\\meaningof{\\cdot}\\pre")
       "for computing preimages")
 (subitem "Unimplementable (except in" (targetlang) ")")
 'next
 (subitem "Proof of correctness w.r.t. standard interpretation")
 'next
 (item "Approximating interpretation" ($"\\meaningof{\\cdot}'\\pre"))
 (subitem "Proofs of soundness w.r.t. nonstandard interpretation, and other nice properties")
 'next
 (subitem "Directly implementable, given a rectangular set library")
 )

(add1-slide-number)
(slide
 #:title "Solution Summary (2)"
 (item "Rectangular set library")
 (subitem "Sets required by semantics: rectangular subsets of" ($"\\Omega") "," ($"T") ","
          ($"\\mathrm{Bool}") ", finite cartesian products")
 'next
 (subitem "Implementation-specific rectangles: "
          ($"\\set{\\mathrm{null}}") ","
          "interval unions,"
          "tagged structures,"
          "symbol sets (planned)")
 'next
 (subitem "Values for each rectangular set type")
 'next
 (item "Two sampling algorithms (so far!)")
 (subitem "Use interpretations to sample uniformly from preimages")
 'next
 (subitem "Correct for any program, any positive-probability condition"
          "(up to floating-point error)")
 'next
 (subitem "Degrade to rejection sampling")
 )

(add1-slide-number)
(slide
 #:title "Demos"
 (item "Normal-Normal with observation")
 (item "Normal-Normal with circular condition")
 (item "Thermometer")
 (item "Normal-Normals with varying ε")
 (item "Boolean PCFG")
 (item "Ray tracing")
 )

(add1-slide-number)
(slide
 #:title "Future Work"
 (item "Optimizations")
 (subitem "O(1) random variable lookup (with proof)")
 'next
 (subitem "Supercombinators")
 'next
 (subitem "Incremental computation")
 'next
 (subitem "Static analysis")
 'next
 (item "Other samplers")
 (subitem "MCMC on partitioned program domain (countable)")
 'next
 (subitem "In general, guide sampling using previous samples")
 'next
 (item "Discover, characterize and overcome current limitations")
 )

;(start-at-recent-slide)
