#lang typed/racket

#|
Define floating-point functions that track an estimate of relative error, using the functions'
condition numbers

Limitations:
 * Doesn't know which cases are exact (i.e. dividing/multiplying by 2, adding/subtracting exact
   integers when the other is in a certain range)
 * Doesn't handle underflow and overflow
 * Only tracks relative error

Also, it would be better to track a bound on error, possibly using interval arithmetic
|#

(require plot/typed
         math/flonum
         math/base)

;; ===================================================================================================
;; Convert a function and its derivative to a function that computes condition numbers

(: condition ((Flonum -> Flonum) (Flonum -> Flonum) -> (Flonum -> Flonum)))
(define ((condition f df) x)
  (abs (/ (* x (df x)) (f x))))

(: condition2d ((Flonum Flonum -> Flonum) (Flonum Flonum -> (Values Flonum Flonum))
                                          -> (Flonum Flonum -> Flonum)))
(define ((condition2d f df) x y)
  (define-values (dx dy) (df x y))
  (define z (f x y))
  (max (abs (/ (* x dx) z))
       (abs (/ (* y dy) z))))

;; ===================================================================================================
;; Derivatives and partial derivatives of common functions

(: diff-fllog (Flonum -> Flonum))
(define (diff-fllog x)
  (/ 1.0 x))

(: diff-fllog1p (Flonum -> Flonum))
(define (diff-fllog1p x)
  (/ 1.0 (+ x 1.0)))

(: diff-flexp (Flonum -> Flonum))
(define (diff-flexp x)
  x)

(: diff-flexpm1 (Flonum -> Flonum))
(define (diff-flexpm1 x)
  x)

(: diff-fl+ (Flonum Flonum -> (Values Flonum Flonum)))
(define (diff-fl+ x y)
  (values 1.0 1.0))

(: diff-fl- (Flonum Flonum -> (Values Flonum Flonum)))
(define (diff-fl- x y)
  (values 1.0 -1.0))

(: diff-fl* (Flonum Flonum -> (Values Flonum Flonum)))
(define (diff-fl* x y)
  (values y x))

(: diff-fl/ (Flonum Flonum -> (Values Flonum Flonum)))
(define (diff-fl/ x y)
  (values (/ 1.0 y) (- (/ x (* y y)))))

;; ===================================================================================================
;; Floating-point data type that stores an estimate of its relative error

(struct: err-float ([x : Flonum] [error : Flonum]) #:transparent)

(define-syntax-rule (define-exact-unary-err-flop name flop)
  (begin
    (: name (err-float -> err-float))
    (define (name x)
      (match-let ([(err-float x ex)  x])
        (err-float (flop x) ex)))))

(define-syntax-rule (define-correct-unary-err-flop name flop diff-flop)
  (begin
    (: name (err-float -> err-float))
    (define name
      (let ([cond-flop  (condition flop diff-flop)])
        (λ (x)
          (match-let ([(err-float x ex)  x])
            (define c (cond-flop x))
            (err-float (flop x) (+ (if (fl= ex 0.0) 0.0 (fl* c ex))
                                   (* 0.5 epsilon.0)))))))))

(define-syntax-rule (define-correct-binary-err-flop name flop diff-flop)
  (begin
    (: name (err-float err-float -> err-float))
    (define name
      (let ([cond-flop  (condition2d flop diff-flop)])
        (λ (x y)
          (match-let ([(err-float x ex)  x]
                      [(err-float y ey)  y])
            (define c (cond-flop x y))
            (err-float (flop x y)
                       (+ (flmax (if (fl= ex 0.0) 0.0 (fl* c ex))
                                 (if (fl= ey 0.0) 0.0 (fl* c ey)))
                          (* 0.5 epsilon.0)))))))))

(: err-fl (Real -> err-float))
(define (err-fl r)
  (define x (fl r))
  (err-float x (fl (relative-error x r))))

(define-exact-unary-err-flop err-flabs flabs)
(define-exact-unary-err-flop err-flneg (λ: ([x : Flonum]) (- x)))

(define-correct-unary-err-flop err-fllog fllog diff-fllog)
(define-correct-unary-err-flop err-flexp flexp diff-flexp)
(define-correct-unary-err-flop err-fllog1p fllog1p diff-fllog1p)
(define-correct-unary-err-flop err-flexpm1 flexpm1 diff-flexpm1)

(define-correct-binary-err-flop err-fl+ fl+ diff-fl+)
(define-correct-binary-err-flop err-fl- fl- diff-fl-)
(define-correct-binary-err-flop err-fl* fl* diff-fl*)
(define-correct-binary-err-flop err-fl/ fl/ diff-fl/)

;; ===================================================================================================
;; Plots

(: err-flgeom (err-float err-float -> err-float))
(define (err-flgeom u p)
  (err-fl/ (err-fllog u) (err-fllog (err-fl- (err-fl 1.0) p))))

(: err-flgeom* (err-float err-float -> err-float))
(define (err-flgeom* u p)
  (err-fl/ (err-fllog u) (err-fllog1p (err-flneg p))))

(plot3d (contour-intervals3d
         (λ: ([p : Real] [u : Real])
           (let ([p  (fl p)] [u  (fl u)])
             (err-float-error (err-flgeom (err-fl u) (err-fl p)))))
         0 1 0 1))

(plot3d (list (contour-intervals3d
               (λ: ([p : Real] [u : Real])
                 (let ([p  (fl p)] [u  (fl u)])
                   (err-float-error (err-flgeom* (err-fl u) (err-fl p)))))
               0 1 0 1)
              (invisible-rect3d #f #f #f #f 0 epsilon.0)))

(plot3d (contour-intervals3d
         (λ: ([p : Real] [u : Real])
           (err-float-error (err-flgeom* (err-fl u) (err-fl p))))
         0 1 0 1))
