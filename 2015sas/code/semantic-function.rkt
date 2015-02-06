#lang racket

(require (for-syntax racket/syntax
                     syntax/parse)
         typed/racket/base
         racket/stxparam)

(provide with-meaning env const
         let)

;; ===================================================================================================
;; Parameterize semantics on a collection of combinators and primitives

;; The "semantics": a hash table from names to identifiers
(define-syntax-parameter semantics #f)

(define-syntax-rule (with-meaning s e)
  (syntax-parameterize ([semantics s])
    (meaning e)))

;; Looks up the identifier for a name
(define-for-syntax (parameterized-lookup name stx)
  (define h (syntax-parameter-value #'semantics))
  (unless h (raise-syntax-error 'meaning "only allowed inside of (with-meaning ...)" stx))
  (hash-ref h name (λ () (raise-syntax-error 'meaning "unknown combinator" stx))))

;; Returns a syntax transformer that expands into #'val anywhere
(define-for-syntax (head-expand stx val)
  (syntax-case stx ()
    [(_ . es)  (quasisyntax/loc stx (#,val . es))]
    [_         val]))

(define-syntax (define-parameterized-combinators stx)
  (syntax-case stx ()
    [(_ name ...)
     (with-syntax ([(_name ...)  (map (λ (name) (format-id name "_~a" name))
                                      (syntax->list #'(name ...)))])
       (syntax/loc stx
         (begin (define-syntax (_name stx)
                  (head-expand stx (parameterized-lookup 'name stx)))
                ...)))]))

(define-parameterized-combinators
  comp pair ifte id const
  const? zero one true false null
  fst snd pair? null?
  lt le gt ge eq
  add sub neg
  mul div rcp)

(define-syntax-rule (_maybe-const e)
  (let ([x e])
    (if (_const? e)
        (_const e)
        (raise-syntax-error 'with-meaning "not a constant in this language" #'e))))

;; ===================================================================================================
;; The parameterized semantic function

(define-syntax-rule (define-keywords name ...)
  (begin (define-syntax (name stx)
           (raise-syntax-error 'name "only allowed inside of (with-meaning ...)" stx))
         ...))

(define-keywords env const)

(define-syntax (meaning stx)
  (syntax-parse stx
    #:literals (let env const
                 if cond else and or
                 cons car cdr pair? null? list
                 < <= > >= =
                 + - * /)
    ;; Binding
    [(_ (let ex eb))  #'(_comp (meaning eb) (_pair (meaning ex) _id))]
    [(_ (env 0))  #'_fst]
    [(_ (env n))  (exact-positive-integer? (syntax->datum #'n))
                  (with-syntax ([n-1  (- (syntax->datum #'n) 1)])
                    #'(_comp (meaning (env n-1)) _snd))]
    ;; if
    [(_ (if c t e))  #'(_ifte (meaning c) (meaning t) (meaning e))]
    ;; cond ... else
    [(_ (cond [else e]))                    #'(meaning e)]
    [(_ (cond [c0 t0] [c t] ... [else e]))  #'(meaning (if c0 t0 (cond [c t] ... [else e])))]
    ;; Conjunction
    [(_ (and))           #'_true]
    [(_ (and e es ...))  #'(_ifte (meaning e) (meaning (and es ...)) _false)]
    ;; Disjunction
    [(_ (or))           #'_false]
    [(_ (or e es ...))  #'(_ifte (meaning e) _true (meaning (or es ...)))]
    ;; Lists
    [(_ (cons e1 e2))  #'(_pair (meaning e1) (meaning e2))]
    [(_ (car e))       #'(_comp _fst (meaning e))]
    [(_ (cdr e))       #'(_comp _snd (meaning e))]
    [(_ (pair? e))     #'(_comp _pair? (meaning e))]
    [(_ (null? e))     #'(_comp _null? (meaning e))]
    [(_ (list))           #'_null]
    [(_ (list e es ...))  #'(_pair (meaning e) (meaning (list es ...)))]
    ;; Comparison
    [(_ (<  a b))  #'(_comp _lt (_pair (meaning a) (meaning b)))]
    [(_ (<= a b))  #'(_comp _le (_pair (meaning a) (meaning b)))]
    [(_ (>  a b))  #'(_comp _gt (_pair (meaning a) (meaning b)))]
    [(_ (>= a b))  #'(_comp _ge (_pair (meaning a) (meaning b)))]
    [(_ (=  a b))  #'(_comp _eq (_pair (meaning a) (meaning b)))]
    ;; Addition
    [(_ (+))               #'_zero]
    [(_ (+ e1))            #'(_comp _add (_pair (meaning e1) _zero))]
    [(_ (+ e1 e2))         #'(_comp _add (_pair (meaning e1) (meaning e2)))]
    [(_ (+ e1 e2 es ...))  #'(meaning (+ (+ e1 e2) es ...))]
    ;; Multiplication
    [(_ (*))               #'_one]
    [(_ (* e1))            #'(_comp _mul (_pair (meaning e1) _one))]
    [(_ (* e1 e2))         #'(_comp _mul (_pair (meaning e1) (meaning e2)))]
    [(_ (* e1 e2 es ...))  #'(meaning (* (* e1 e2) es ...))]
    ;; Subtraction
    [(_ (- e1))            #'(_comp _neg (meaning e1))]
    [(_ (- e1 e2))         #'(_comp _sub (_pair (meaning e1) (meaning e2)))]
    [(_ (- e1 e2 es ...))  #'(meaning (- (- e1 e2) es ...))]
    ;; Division
    [(_ (/ e1))            #'(_comp _rcp (meaning e1))]
    [(_ (/ e1 e2))         #'(_comp _div (_pair (meaning e1) (meaning e2)))]
    [(_ (/ e1 e2 es ...))  #'(meaning (/ (/ e1 e2) es ...))]
    ;; Constants
    [(_ (const e))  #'(_maybe-const e)]
    [(_ e)  #'(_maybe-const e)]
    ))
