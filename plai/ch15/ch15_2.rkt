#lang plai-typed


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Constraints
  [eqCon (lhs : Term) (rhs : Term)])

(define-type Term
  [tExp (e : ExprC)]
  [tVar (s : symbol)]
  [tNum]
  [tArrow (dom : Term) (rng : Term)])

(define (cg [e : ExprC]) : (listof Constraints)
  (type-case ExprC e
    [numC (_) (list (eqCon (tExp e) (tNum)))]
    [idC (s) (list (eqCon (tExp e) (tVar s)))]
    [plusC (l r) (append (cg l)
                         (append (cg r)
                                 (list (eqCon (tExp l) (tNum))
                                       (eqCon (tExp r) (tNum))
                                       (eqCon (tExp e) (tNum)))))]
    
    [multC (l r) (append (cg l)
                         (append (cg r)
                                 (list (eqCon (tExp l) (tNum))
                                       (eqCon (tExp r) (tNum))
                                       (eqCon (tExp e) (tNum)))))]
    [appC (f a) (append (cg f)
                        (append (cg a)
                                (list (eqCon (tExp f) (tArrow (tExp a) (tExp e))))))]
    [lamC (a b) (append (cg b)
                        (list (eqCon (tExp e) (tArrow (tVar a) (tExp b)))))]))

(define-type-alias Subst (listof Substitution))
(define-type Substitution
  [sub [var : Term] [is : Term]])

(define (unify [cs : (listof Constraints)]) : Subst
    (unify/Θ cs empty))

(define (unify/Θ [cs : (listof Constraints)] [Θ : Subst]) : Subst
    (cond
      [(empty? cs) Θ]
      [(cons? cs)
       (let ([l (eqCon-lhs (first cs))]
             [r (eqCon-rhs (first cs))])
         (type-case Term l
           [tVar (s) (type-case (optionof Term) (lookup l Θ)
                       [some (bound)
                             (unify/Θ (cons (eqCon bound r)
                                            (rest cs))
                                      Θ)]
                       [none ()
                             (unify/Θ (rest cs)
                                      (extend+replace l r Θ))])]
           [tExp (e) (type-case (optionof Term) (lookup l Θ)
                       [some (bound)
                             (unify/Θ (cons (eqCon bound r)
                                            (rest cs))
                                      Θ)]
                       [none ()
                             (unify/Θ (rest cs)
                                      (extend+replace l r Θ))])]
           [tNum () (type-case Term r
                      [tNum () (unify/Θ (rest cs) Θ)]
                      [else (error 'unify "number and something else")])]
           [tArrow (d r) (type-case Term r
                           [tArrow (d2 r2)
                                   (unify/Θ (cons (eqCon d d2)
                                                  (cons (eqCon r r2)
                                                        cs))
                                            Θ)]
                           [else (error 'unify "arrow and something else")])]))]))

(define (lookup [t : Term] [s : Subst]) : (optionof Term)
  (cond
    [(empty? s) (none)]
    [else (let ([sbst (first s)])
            (if (eq? t (sub-var sbst))
                (some (sub-is sbst))
                (lookup t (rest s))))]))

(define (occurs-free? [s : symbol] [tm : Term]) : boolean
  (type-case Term tm
    [tExp (_) #f]
    [tVar (t) (symbol=? s t)]
    [tNum () #f]
    [tArrow (t1 t2) (or (occurs-free? s t1)
                        (occurs-free? s t2))]))

(define (subst [s : Substitution] [t : Term]) : Term
  (type-case Substitution s
    [sub (l r)
         (if (equal? l t)
             r
             (type-case Term t
               [tArrow (t1 t2) (tArrow (subst s t1)
                                       (subst s t2))]
               [else t]))]))

(define (extend+replace [t1 : Term] [t2 : Term] [s : Subst]) : Subst
  (if (occurs? t1 t2)
      (error 'extend+replace "occurs check failed")
      (let ([sbst (sub t1 t2)])
        (cons sbst
              (map (lambda (s2)
                     (sub (subst sbst (sub-var s2))
                          (subst sbst (sub-is s2))))
                   s)))))

(define (occurs? [t1 : Term] [t2 : Term]) : boolean
  (type-case Term t1
    [tVar (s) (occurs-free? s t2)]
    [else #f]))

(define ex
  (lamC 'x (idC 'x)))

(define cs (cg ex))