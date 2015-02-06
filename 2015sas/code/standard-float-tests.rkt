#lang typed/racket

(require typed/rackunit
         "bottom.rkt"
         "semantic-function.rkt"
         "standard-float.rkt")

(define-syntax-rule (valueof e)
  (run (with-meaning standard-float e)))

(define-syntax-rule (check-valueof e1 e2)
  (check-equal? (valueof e1) e2))

(check-valueof (+) 0.0)
(check-valueof (+ 4.0) 4.0)
(check-valueof (+ 4.0 5.0) 9.0)
(check-valueof (+ 4.0 5.0 6.0) 15.0)

(check-valueof (- 4.0) -4.0)
(check-valueof (- 4.0 5.0) -1.0)
(check-valueof (- 4.0 5.0 6.0) -7.0)

(check-valueof (*) 1.0)
(check-valueof (* 4.0) 4.0)
(check-valueof (* 4.0 5.0) 20.0)
(check-valueof (* 4.0 5.0 6.0) 120.0)

(check-valueof (/ 40.0) #i1/40)
(check-valueof (/ 40.0 5.0) 8.0)
(check-valueof (/ 400.0 5.0 6.0) (/ 400.0 5.0 6.0))

(check-valueof (<  4.0 5.0) #t)
(check-valueof (<= 4.0 5.0) #t)
(check-valueof (>  4.0 5.0) #f)
(check-valueof (>= 4.0 5.0) #f)
(check-valueof (=  4.0 5.0) #f)
(check-valueof (<  5.0 4.0) #f)
(check-valueof (<= 5.0 4.0) #f)
(check-valueof (>  5.0 4.0) #t)
(check-valueof (>= 5.0 4.0) #t)
(check-valueof (=  5.0 4.0) #f)
(check-valueof (<  5.0 5.0) #f)
(check-valueof (<= 5.0 5.0) #t)
(check-valueof (>  5.0 5.0) #f)
(check-valueof (>= 5.0 5.0) #t)
(check-valueof (=  5.0 5.0) #t)

(check-valueof (cons 4.0 5.0) '(4.0 . 5.0))
(check-valueof (cons (+ 4.0 5.0) (* 4.0 5.0)) '(9.0 . 20.0))

(check-valueof (car (cons 4.0 5.0)) 4.0)
(check-valueof (cdr (cons 4.0 5.0)) 5.0)
(check-valueof (car 4.0) bottom)
(check-valueof (cdr 4.0) bottom)

(check-valueof (pair? (cons 4.0 5.0)) #t)
(check-valueof (pair? null) #f)

(check-valueof (null? (cons 4.0 5.0)) #f)
(check-valueof (null? null) #t)

(check-valueof (list) '())
(check-valueof (list 4.0) '(4.0))
(check-valueof (list 4.0 5.0) '(4.0 5.0))
(check-valueof (list 4.0 5.0 6.0) '(4.0 5.0 6.0))

(check-valueof (if (<  4.0 5.0) #t #f) #t)
(check-valueof (if (<= 4.0 5.0) #t #f) #t)
(check-valueof (if (>  4.0 5.0) #t #f) #f)
(check-valueof (if (>= 4.0 5.0) #t #f) #f)
(check-valueof (if (<  5.0 4.0) #t #f) #f)
(check-valueof (if (<= 5.0 4.0) #t #f) #f)
(check-valueof (if (>  5.0 4.0) #t #f) #t)
(check-valueof (if (>= 5.0 4.0) #t #f) #t)
(check-valueof (if (<  5.0 5.0) #t #f) #f)
(check-valueof (if (<= 5.0 5.0) #t #f) #t)
(check-valueof (if (>  5.0 5.0) #t #f) #f)
(check-valueof (if (>= 5.0 5.0) #t #f) #t)

(check-valueof (cond [else 4.0]) 4.0)
(check-valueof (cond [#t 4.0] [else 5.0]) 4.0)
(check-valueof (cond [#f 4.0] [else 5.0]) 5.0)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (cond [a 4.0] [b 5.0] [c 6.0] [else 7.0])
                 (cond [a 4.0] [b 5.0] [c 6.0] [else 7.0])))

(check-valueof (and) #t)
(check-valueof (and #t) #t)
(check-valueof (and #f) #f)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (and a b c) (and a b c)))

(check-valueof (or) #f)
(check-valueof (or #t) #t)
(check-valueof (or #f) #f)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (or a b c) (or a b c)))

(check-valueof (let (+ 4.0 5.0) (+ (env 0) 6.0))
               15.0)

(check-valueof (let (+ 4.0 5.0)
                 (let (+ (env 0) 6.0)
                   (let (+ (env 0) 7.0)
                     (+ (env 0) 8.0))))
               (+ 4.0 5.0 6.0 7.0 8.0))

(check-valueof (let (+ 4.0 5.0)
                 (let (+ 6.0 7.0)
                   (let 8.0
                     (- (env 0) (env 1) (env 2)))))
               (- 8.0 (+ 6.0 7.0) (+ 4.0 5.0)))
