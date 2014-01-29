#lang typed/racket

(define-syntax compile
  (syntax-rules (add choose)
    [(_ (add e1 e2))
     (for*/set ([v1  (in-set (compile e1))]
                [v2  (in-set (compile e2))])
       (+ v1 v2))]
    [(_ (choose e1 e2))
     (set-union (compile e1) (compile e2))]
    [(_ v)
     (set v)]))

(define-syntax compile/a
  (syntax-rules (add choose)
    [(_ return/a bind/a merge/a (add e1 e2))
     (bind/a (compile/a return/a bind/a merge/a e1)
             (λ (v1) (bind/a (compile/a return/a bind/a merge/a e2)
                             (λ (v2) (return/a (+ (cast v1 Natural) (cast v2 Natural)))))))]
    [(_ return/a bind/a merge/a (choose e1 e2))
     (merge/a (compile/a return/a bind/a merge/a e1)
              (compile/a return/a bind/a merge/a e2))]
    [(_ return/a bind/a merge/a v)
     (return/a (ann v Natural))]))


(: set-image (All (A B) ((A -> B) (Setof A) -> (Setof B))))
(define (set-image f A)
  (list->set (set-map A f)))

(: set-union* (All (A) ((Setof (Setof A)) -> (Setof A))))
(define (set-union* As)
  (let ([As  (set->list As)])
    (cond [(empty? As)  (set)]
          [else  (apply set-union (first As) (rest As))])))


(: return/set (All (A) (A -> (Setof A))))
(define (return/set x) (set x))

(: bind/set (All (A B) ((Setof A) (A -> (Setof B)) -> (Setof B))))
(define (bind/set m f)
  (set-union* (set-image f m)))

(: merge/set (All (A B) ((Setof A) (Setof B) -> (Setof (U A B)))))
(define (merge/set m1 m2)
  (set-union m1 m2))


(struct: (A) set+card ([set : (Setof A)] [card : Natural]) #:transparent)

(: η/len (All (A) ((Setof A) -> (set+card A))))
(define (η/len A) (set+card A (set-count A)))

#|
(η/len (return/set a))
= (η/len (set a))
= (set+card (set a) 1)
|#
(: return/len (All (A) (A -> (set+card A))))
(define (return/len a)
  (set+card (set a) 1))

#|
(η/len (liftm/len f m))
= (η/len (set-image f m))
= (set+card (set-image f m)
            (set-count (set-image f m)))
= (match-let ([(set+card m len)  (η/len m)])
    (set+card (set-image f m)
              (set-count (set-image f m))))
|#
(: liftm/len (All (A B) ((A -> B) (set+card A) -> (set+card B))))
(define (liftm/len f m)
  (match-let ([(set+card m len)  m])
    (set+card (set-image f m)
              (set-count (set-image f m)))))
#;
(define (liftm/len f m)
  (match-define (set+card A len) m)
  (set+card (set-image f A) len))

(: join/len (All (A) ((set+card (set+card A)) -> (set+card A))))
(define (join/len ms)
  (match-define (set+card bs len) ms)
  (set+card (set-union* (set-image (inst set+card-set A) bs))
            (* len (apply max (set->list (set-image (inst set+card-card A) bs))))))

#|
(η/len (bind/set m f))
= (η/len (set-union* (set-image f m)))
= (match-let ([(set+card m len)  (η/len m)])
    (set-union* (set-image f m)))
= (match-let* ([(set+card m len)  (η/len m)]
               [ns  (set-image f m)])
    (set-union* ns))
= (match-let* ([(set+card m len)  (η/len m)]
               [ns  (set-image (lift/len f) m)])
    (set+card (set-union* (set-image set+card-set ns))
              (set-count (set-union* (set-image set+card-set ns)))))
|#

(: bind/len (All (A B) ((set+card A) (A -> (set+card B)) -> (set+card B))))
(define (bind/len m f)
  (match-let* ([(set+card m len)  m]
               [ns  ((inst set-image A (set+card B)) f m)])
    (set+card (set-union* (set-image (inst set+card-set B) ns))
              (set-count
               (set-union* (set-image (inst set+card-set B) ns)))))
  #;
  (join/len (liftm/len f m)))

#|
(η/len (merge/set m1 m2))
= (η/len (set-union m1 m2))
= (set+card (set-union m1 m2)
            (set-count (set-union m1 m2)))
= (match-let ([(set+card m1 len1)  (η/len m1)]
              [(set+card m2 len2)  (η/len m2)])
    (set+card (set-union m1 m2)
              (set-count (set-union m1 m2))))
|#

(: merge/len (All (A B) ((set+card A) (set+card B) -> (set+card (U A B)))))
(define (merge/len m1 m2)
  (match-let ([(set+card m1 len1)  m1]
              [(set+card m2 len2)  m2])
    (set+card (set-union m1 m2)
              (set-count (set-union m1 m2))))
  #;
  (set+card (set-union (set+card-set m1) (set+card-set m2))
            (+ (set+card-card m1) (set+card-card m2))))

(compile/a return/set bind/set merge/set (add (choose 4 5) (choose (choose 10 11) 20)))
(compile/a return/len bind/len merge/len (add (choose 4 5) (choose (choose 10 11) 20)))

(define-type (LenArr A B) (A -> (set+card B)))

(define-type (SetArr A B) (A -> (Setof B)))

(: arr/set (All (A B) ((A -> B) -> (SetArr A B))))
(define ((arr/set f) a) (return/set (f a)))

(: >>>/set (All (A B C) ((SetArr A B) (SetArr B C) -> (SetArr A C))))
(define ((>>>/set f1 f2) a)
  (bind/set (f1 a) f2))

(: &&&/set (All (A B C) ((SetArr A B) (SetArr A C) -> (SetArr A (Pair B C)))))
(define ((&&&/set f1 f2) a)
  (bind/set (f1 a) (λ: ([b : B]) (bind/set (f2 a) (λ: ([c : C]) (return/set (cons b c)))))))

(: mergef/set (All (A B C) ((SetArr A B) (SetArr A C) -> (SetArr A (U B C)))))
(define ((mergef/set f1 f2) a)
  (set-union (f1 a) (f2 a)))


(: lift/len (All (A B) ((SetArr A B) -> (LenArr A B))))
(define ((lift/len f) a)
  (define bs (f a))
  (set+card bs (set-count bs)))

#|
(lift/len (arr/set f))
= (lift/len (λ (a) (return/set (f a))))
= (λ (a)
    (let ([bs  (return/set (f a))])
      (set+card bs (set-count bs))))
= (λ (a) (set+card (return/set (f a)) 1))
|#
(: arr/len (All (A B) ((A -> B) -> (LenArr A B))))
(define ((arr/len f) a)
  (η/len (return/set (f a)))
  #;
  (set+card (return/set (f a)) 1))

#|
((lift/len (>>>/set f1 f2)) a)
= ((lift/len (bind/set (f1 a) f2)) a)
= (let ([bs  (bind/set (f1 a) f2)])
    (set+card bs (set-count bs)))
= (let ([bs  (set-union* (set-image f2 (f1 a)))])
    (set+card bs (set-count bs)))
= (set+card (set-union* (set-image f2 (f1 a)))
            (set-count (set-union* (set-image f2 (f1 a)))))
|#

(: >>>/len (All (A B C) ((LenArr A B) (LenArr B C) -> (LenArr A C))))

(define ((>>>/len f1 f2) a)
  (match-let* ([(set+card m len)  (f1 a)]
               [ns  ((inst set-image B (set+card C)) f2 m)])
    (set+card (set-union* (set-image (inst set+card-set C) ns))
              (* len (apply max (set->list (set-image (inst set+card-card C) (set-image f2 m)))))
              #;
              (set-count
               (set-union* (set-image (inst set+card-set C) ns)))))
  #;
  (bind/len (f1 a) f2))

#;
(define ((>>>/len f1 f2) a)
  (match-define (set+card bs len) (f1 a))
  (define clens (set-image f2 bs))
  (printf "~v~n" (map f2 (set->list bs)))
  (set+card (set-union* (set-image (inst set+card-set C) clens))
            (* len (apply max (map (inst set+card-card C) (map f2 (set->list bs)))))
            #;
            (apply + (map (inst set+card-card C) (map f2 (set->list bs))))
            #;
            (set-count (set-union* (set-image (inst set+card-set C) clens)))
            #;(apply + (set->list (set-image (inst set+card-card C) clens)))))

(: &&&/len (All (A B C) ((LenArr A B) (LenArr A C) -> (LenArr A (Pair B C)))))
(define ((&&&/len f1 f2) a)
  (match-define (set+card bs len1) (f1 a))
  (match-define (set+card cs len2) (f2 a))
  (set+card
   (list->set
    (for*/list: : (Listof (Pair B C)) ([b  (in-set bs)]
                                       [c  (in-set cs)])
      (cons b c)))
   (* len1 len2)))

(: mergef/len (All (A B C) ((LenArr A B) (LenArr A C) -> (LenArr A (U B C)))))
(define ((mergef/len f1 f2) a)
  (match-define (set+card bs len1) (f1 a))
  (match-define (set+card cs len2) (f2 a))
  (set+card (set-union bs cs) (+ len1 len2)))

(define-syntax compile/arr
  (syntax-rules (add choose)
    [(_ arr/a >>>/a &&&/a mergef/a (add e1 e2))
     (>>>/a (&&&/a (compile/arr arr/a >>>/a &&&/a mergef/a e1)
                   (compile/arr arr/a >>>/a &&&/a mergef/a e2))
            (arr/a (λ: ([bc : (Pair Natural Natural)]) (+ (car bc) (cdr bc)))))]
    [(_ arr/a >>>/a &&&/a mergef/a (choose e1 e2))
     (mergef/a (compile/arr arr/a >>>/a &&&/a mergef/a e1)
               (compile/arr arr/a >>>/a &&&/a mergef/a e2))]
    [(_ arr/a >>>/a &&&/a mergef/a v)
     (arr/a (λ (_) v))]))

((compile/arr arr/set >>>/set &&&/set mergef/set (add (choose 4 5) 90)) 0)
((compile/arr arr/len >>>/len &&&/len mergef/len (add (choose 4 5) 90)) 0)

((compile/arr arr/set >>>/set &&&/set mergef/set (add (choose 4 5) (choose 4 5))) 0)

((compile/arr arr/len >>>/len &&&/len mergef/len (add (choose 4 5) (choose 4 5))) 0)

((compile/arr arr/len >>>/len &&&/len mergef/len (add (choose 4 5) (choose (choose 4 6) 5))) 0)
