#lang racket

(require (for-syntax racket/syntax
                     syntax/parse)
         typed/racket/base
         racket/stxparam)

(provide with-meaning
         ;; Provide the exact identifiers used by the semantic function
         let let* const ;env  ; obsoleted by named identifiers
         if cond else and or
         cons car cdr pair? null? null list
         < <= > >= =
         + - * /
         quote #%datum)

;; ===================================================================================================
;; Parameterize semantics on a collection of combinators and primitives

;; The "semantics" is a hash table from names to identifiers
(define-syntax-parameter semantics #f)

(define-syntax (with-meaning stx)
  (syntax-case stx ()
    [(_ sem expr)
     (syntax/loc stx
       (syntax-parameterize ([semantics sem])
         (meaning expr)))]))

(define-for-syntax (lookup-fail name stx)
  (raise-syntax-error 'with-meaning
                      (format "unknown language attribute ~v" name)
                      stx))

;; Looks up the identifier for a name
(define-for-syntax (lookup-language-attribute name stx [fail lookup-fail])
  (define h (syntax-parameter-value #'semantics))
  (unless h (raise-syntax-error 'with-meaning "only allowed inside of (with-meaning ...)" stx))
  (hash-ref h name (λ () (fail name stx))))

;; Returns a syntax transformer that expands into val anywhere
(define-for-syntax (head-expand stx val)
  (syntax-case stx ()
    [(_ . es)  (quasisyntax/loc stx (#,val . es))]
    [_         val]))

(define-for-syntax ((make-lookup-fail desc) _ stx)
  (raise-syntax-error 'with-meaning
                      (format "~a is not supported in language ~a"
                              desc (lookup-language-attribute 'language-name stx))
                      stx))

(define-syntax (define-language-parameters stx)
  (syntax-case stx ()
    [(_ [name text] ...)
     (with-syntax ([(_name ...)  (map (λ (n) (format-id n "_~a" (syntax->datum n)))
                                      (syntax->list #'(name ...)))])
       (syntax/loc stx
         (begin (define-syntax (_name stx)
                  (define val (lookup-language-attribute
                               'name
                               (if (symbol? (syntax-e stx)) stx (car (syntax-e stx)))
                               (make-lookup-fail text)))
                  (head-expand stx val))
                ...)))]))

(define-language-parameters
  [value? "imported constant check"]
  [comp "the composition combinator"]
  [pair "the pairing combinator"]
  [ifte "the if-then-else combinator"]
  [id "the identity combinator"]
  [const "the constant combinator"]
  [fst "car"]
  [snd "cdr"]
  [pair? "pair?"]
  [null? "null?"]
  [lt "the < operator"]
  [le "the <= operator"]
  [gt "the > operator"]
  [ge "the >= operator"]
  [eq "the = operator"]
  [add "two-argument addition"]
  [neg "negation"]
  [sub "two-argument subtraction"]
  [mul "two-argument multiplication"]
  [rcp "reciprocal"]
  [div "two-argument division"])

;; ===================================================================================================
;; The parameterized semantic function

(define-syntax-rule (define-keywords name ...)
  (begin (define-syntax (name stx)
           (raise-syntax-error 'name "only allowed inside of (with-meaning ...)" stx))
         ...))

(define-keywords env const)

(begin-for-syntax
  (define-syntax-rule (with-inner-relocations stx (id ...) . body)
    (with-syntax ([id  (syntax/loc stx id)] ...) . body)))

(define-for-syntax (maybe-const e)
  (define lang-name (lookup-language-attribute 'language-name #'e))
  (with-inner-relocations e (_value? _const)
    (quasisyntax/loc e
      (let ([x #,e])
        (if (_value? x)
            (_const x)
            (raise-syntax-error 'with-meaning
                                (format "~v is not a constant in language ~a" x #,lang-name)
                                #,e))))))

(define-for-syntax (maybe-literal orig-e)
  (define e (local-expand orig-e 'expression (list #'quote)))
  ;; Expand until literal quote
  (syntax-case e (quote)
    [(quote _)  (maybe-const orig-e)]
    [_  #f]))

(define-syntax-parameter let-depth 0)

(begin-for-syntax
  (struct local-identifier (depth)))

(define-syntax (meaning orig-stx)
  (define stx (syntax-case orig-stx () [(_ e) (syntax/loc stx e)]))
  (with-inner-relocations stx (_value?
                               _comp _pair _ifte _id _const
                               _fst _snd _pair? _null?
                               _lt _le _gt _ge _eq
                               _add _sub _neg
                               _mul _div _rcp)
    (syntax-parse stx
      #:literals (let let* env const
                   if cond else and or
                   cons car cdr pair? null? null list
                   < <= > >= =
                   + - * /)
      ;; Binding
      [(let ([x:id ex]) eb)
       (with-syntax ([d  (+ 1 (syntax-parameter-value #'let-depth))])
         (syntax/loc stx
           (_comp (let-syntax ([x  (local-identifier d)])
                    (syntax-parameterize ([let-depth  d])
                      (meaning eb)))
                  (_pair (meaning ex) _id))))]
      ;; Identifiers
      [x:id
       #:when (local-identifier? (syntax-local-value #'x (λ () #f)))
       (define d (syntax-parameter-value #'let-depth))
       (define old-d (local-identifier-depth (syntax-local-value #'x (λ () #f))))
       (with-syntax ([n  (- d old-d)])
         (syntax/loc stx (meaning (env n))))]
      [(env 0)  (syntax/loc stx _fst)]
      [(env n)  (exact-positive-integer? (syntax->datum #'n))
                (with-syntax ([n-1  (- (syntax->datum #'n) 1)])
                  (syntax/loc stx (_comp (meaning (env n-1)) _snd)))]
      ;; Sequential let
      [(let* () e)
       (syntax/loc stx
         (meaning e))]
      [(let* ([x0:id ex0] [x:id ex] ...) eb)
       (syntax/loc stx
         (meaning (let ([x0 ex0]) (let* ([x ex] ...) eb))))]
      ;; if
      [(if c t e)  (syntax/loc stx (_ifte (meaning c) (meaning t) (meaning e)))]
      ;; cond ... else
      [(cond [else e])
       (syntax/loc stx (meaning e))]
      [(cond [c0 t0] [c t] ... [else e])
       (syntax/loc stx (meaning (if c0 t0 (cond [c t] ... [else e]))))]
      ;; Conjunction
      [(and)           (syntax/loc stx (meaning #t))]
      [(and e es ...)  (syntax/loc stx (_ifte (meaning e) (meaning (and es ...)) (meaning #f)))]
      ;; Disjunction
      [(or)           (syntax/loc stx (meaning #f))]
      [(or e es ...)  (syntax/loc stx (_ifte (meaning e) (meaning #t) (meaning (or es ...))))]
      ;; Lists
      [(cons e1 e2)  (syntax/loc stx (_pair (meaning e1) (meaning e2)))]
      [(car e)       (syntax/loc stx (_comp _fst (meaning e)))]
      [(cdr e)       (syntax/loc stx (_comp _snd (meaning e)))]
      [(pair? e)     (syntax/loc stx (_comp _pair? (meaning e)))]
      [(null? e)     (syntax/loc stx (_comp _null? (meaning e)))]
      [(list)           (syntax/loc stx (meaning null))]
      [(list e es ...)  (syntax/loc stx (_pair (meaning e) (meaning (list es ...))))]
      ;; Comparison
      [(<  a b)  (syntax/loc stx (_comp _lt (_pair (meaning a) (meaning b))))]
      [(<= a b)  (syntax/loc stx (_comp _le (_pair (meaning a) (meaning b))))]
      [(>  a b)  (syntax/loc stx (_comp _gt (_pair (meaning a) (meaning b))))]
      [(>= a b)  (syntax/loc stx (_comp _ge (_pair (meaning a) (meaning b))))]
      [(=  a b)  (syntax/loc stx (_comp _eq (_pair (meaning a) (meaning b))))]
      ;; Addition
      [(+ e1 e2)         (syntax/loc stx (_comp _add (_pair (meaning e1) (meaning e2))))]
      [(+ e1 e2 es ...)  (syntax/loc stx (meaning (+ (+ e1 e2) es ...)))]
      ;; Multiplication
      [(* e1 e2)         (syntax/loc stx (_comp _mul (_pair (meaning e1) (meaning e2))))]
      [(* e1 e2 es ...)  (syntax/loc stx (meaning (* (* e1 e2) es ...)))]
      ;; Subtraction
      [(- e1)            (syntax/loc stx (_comp _neg (meaning e1)))]
      [(- e1 e2)         (syntax/loc stx (_comp _sub (_pair (meaning e1) (meaning e2))))]
      [(- e1 e2 es ...)  (syntax/loc stx (meaning (- (- e1 e2) es ...)))]
      ;; Division
      [(/ e1)            (syntax/loc stx (_comp _rcp (meaning e1)))]
      [(/ e1 e2)         (syntax/loc stx (_comp _div (_pair (meaning e1) (meaning e2))))]
      [(/ e1 e2 es ...)  (syntax/loc stx (meaning (/ (/ e1 e2) es ...)))]
      ;; Imported Racket constants and literals
      [(const e)  (maybe-const (syntax/loc stx e))]
      [null       (maybe-const stx)]
      [_
       (let ([lit-e-stx  (maybe-literal stx)])
         (if lit-e-stx
             lit-e-stx
             (raise-syntax-error 'with-meaning
                                 (format "not legal syntax in language ~a"
                                         (lookup-language-attribute 'language-name stx))
                                 stx)))])))
