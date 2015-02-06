#lang typed/racket

(require typed/rackunit
         "bottom.rkt"
         "semantic-function.rkt"
         "standard-integer.rkt")

(define-syntax-rule (valueof e)
  (run (with-meaning standard-integer e)))

(define-syntax-rule (check-valueof e1 e2)
  (check-equal? (valueof e1) e2))

(check-valueof (+) 0)
(check-valueof (+ 4) 4)
(check-valueof (+ 4 5) 9)
(check-valueof (+ 4 5 6) 15)

(check-valueof (- 4) -4)
(check-valueof (- 4 5) -1)
(check-valueof (- 4 5 6) -7)

(check-valueof (*) 1)
(check-valueof (* 4) 4)
(check-valueof (* 4 5) 20)
(check-valueof (* 4 5 6) 120)

(check-valueof (/ 40) 0)
(check-valueof (/ 40 5) 8)
(check-valueof (/ 400 5 6) 13)

(check-valueof (<  4 5) #t)
(check-valueof (<= 4 5) #t)
(check-valueof (>  4 5) #f)
(check-valueof (>= 4 5) #f)
(check-valueof (=  4 5) #f)
(check-valueof (<  5 4) #f)
(check-valueof (<= 5 4) #f)
(check-valueof (>  5 4) #t)
(check-valueof (>= 5 4) #t)
(check-valueof (=  5 4) #f)
(check-valueof (<  5 5) #f)
(check-valueof (<= 5 5) #t)
(check-valueof (>  5 5) #f)
(check-valueof (>= 5 5) #t)
(check-valueof (=  5 5) #t)

(check-valueof (cons 4 5) '(4 . 5))
(check-valueof (cons (+ 4 5) (* 4 5)) '(9 . 20))

(check-valueof (car (cons 4 5)) 4)
(check-valueof (cdr (cons 4 5)) 5)
(check-valueof (car 4) bottom)
(check-valueof (cdr 4) bottom)

(check-valueof (pair? (cons 4 5)) #t)
(check-valueof (pair? null) #f)

(check-valueof (null? (cons 4 5)) #f)
(check-valueof (null? null) #t)

(check-valueof (list) '())
(check-valueof (list 4) '(4))
(check-valueof (list 4 5) '(4 5))
(check-valueof (list 4 5 6) '(4 5 6))

(check-valueof (if (<  4 5) #t #f) #t)
(check-valueof (if (<= 4 5) #t #f) #t)
(check-valueof (if (>  4 5) #t #f) #f)
(check-valueof (if (>= 4 5) #t #f) #f)
(check-valueof (if (<  5 4) #t #f) #f)
(check-valueof (if (<= 5 4) #t #f) #f)
(check-valueof (if (>  5 4) #t #f) #t)
(check-valueof (if (>= 5 4) #t #f) #t)
(check-valueof (if (<  5 5) #t #f) #f)
(check-valueof (if (<= 5 5) #t #f) #t)
(check-valueof (if (>  5 5) #t #f) #f)
(check-valueof (if (>= 5 5) #t #f) #t)

(check-valueof (cond [else 4]) 4)
(check-valueof (cond [#t 4] [else 5]) 4)
(check-valueof (cond [#f 4] [else 5]) 5)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (cond [a 4] [b 5] [c 6] [else 7])
                 (cond [a 4] [b 5] [c 6] [else 7])))

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

(check-valueof (let (+ 4 5) (+ (env 0) 6))
               15)

(check-valueof (let (+ 4 5)
                 (let (+ (env 0) 6)
                   (let (+ (env 0) 7)
                     (+ (env 0) 8))))
               (+ 4 5 6 7 8))

(check-valueof (let (+ 4 5)
                 (let (+ 6 7)
                   (let 8
                     (- (env 0) (env 1) (env 2)))))
               (- 8 (+ 6 7) (+ 4 5)))
