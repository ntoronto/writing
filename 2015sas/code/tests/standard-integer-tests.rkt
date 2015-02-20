#lang typed/racket

(require typed/rackunit
         "../semantics/standard-integer.rkt"
         "../semantic-function.rkt")

(define-syntax-rule (valueof e)
  (si:run (with-meaning standard-integer e)))

(define-syntax-rule (check-valueof e1 e2)
  (check-equal? (valueof e1) e2))

(check-valueof (+ 4 5) 9)
(check-valueof (+ 4 5 6) 15)

(check-valueof (- 4) -4)
(check-valueof (- 4 5) -1)
(check-valueof (- 4 5 6) -7)

(check-valueof (* 4 5) 20)
(check-valueof (* 4 5 6) 120)

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
  (check-valueof (cond [(const a) 4] [(const b) 5] [(const c) 6] [else 7])
                 (cond [a 4] [b 5] [c 6] [else 7])))

(check-valueof (and) #t)
(check-valueof (and #t) #t)
(check-valueof (and #f) #f)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (and (const a) (const b) (const c))
                 (and a b c)))

(check-valueof (or) #f)
(check-valueof (or #t) #t)
(check-valueof (or #f) #f)
(for* ([a  (in-list '(#t #f))]
       [b  (in-list '(#t #f))]
       [c  (in-list '(#t #f))])
  (check-valueof (or (const a) (const b) (const c))
                 (or a b c)))

(check-valueof (let ([x (+ 4 5)])
                 (+ x 6))
               15)

(check-valueof (let ([x  (+ 4 5)])
                 (let ([y  (+ x 6)])
                   (let ([z  (+ y 7)])
                     (+ z 8))))
               (+ 4 5 6 7 8))

(check-valueof (let ([x  (+ 4 5)])
                 (let ([y  (+ 6 7)])
                   (let ([z  8])
                     (- z y x))))
               (- 8 (+ 6 7) (+ 4 5)))

(check-valueof (let* ([x  (+ 4 5)]
                      [y  (+ 6 7)]
                      [z  8])
                 (- z y x))
               (- 8 (+ 6 7) (+ 4 5)))