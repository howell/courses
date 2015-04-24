#lang plai-typed

(define-type TyExprC
  [numC (n : number)]
  [boolC (b : boolean)]
  [idC (s : symbol)]
  [appC (fun : TyExprC) (arg : TyExprC)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [ifC (test : TyExprC) (then : TyExprC) (else : TyExprC)]
  [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)]
  [recC (fn : symbol) (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC) (use : TyExprC)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (arg : symbol) (body : TyExprC) (env : Env)])

(define-type Type
  [numT]
  [boolT]
  [funT (arg : Type) (ret : Type)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [x : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? x (bind-name (first env))) (bind-val (first env))]
            [else (lookup x (rest env))])]))

#|
Exercise
Define the types and functions associated with type environments.
|#

(define-type TyBinding
  [tybind (name : symbol) (type : Type)])

(define-type-alias TyEnv (listof TyBinding))
(define mt-tyenv empty)
(define extend-tyenv cons)

(define (lookup-ty [x : symbol] [env : TyEnv]) : Type
  (cond
    [(empty? env) (error 'lookup-ty "name not found")]
    [else (cond
            [(symbol=? x (tybind-name (first env))) (tybind-type (first env))]
            [else (lookup-ty x (rest env))])]))

(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type
  (type-case TyExprC expr
    [numC (_) (numT)]
    [boolC (_) (boolT)]
    [idC (s) (lookup-ty s tenv)]
    [plusC (l r) (let ([tl (tc l tenv)]
                       [tr (tc r tenv)])
                   (if (and (numT? tl) (numT? tr))
                       (numT)
                       (error 'plus "expected both operands to be numbers")))]
    [multC (l r) (let ([tl (tc l tenv)]
                       [tr (tc r tenv)])
                   (if (and (numT? tl) (numT? tr))
                       (numT)
                       (error 'mult "expected both operands to be numbers")))]
    [appC (f a) (let ([tf (tc f tenv)]
                      [ta (tc a tenv)])
                  (type-case Type tf
                    [funT (t1 t2) (if (equal? t1 ta)
                                      t2
                                      (error 'app "argument type mismatch"))]
                    [else (error 'app "expected function type")]))]
    [ifC (tst thn els) (let ([ttst (tc tst tenv)]
                             [tthn (tc thn tenv)]
                             [tels (tc els tenv)])
                         (type-case Type ttst
                           [boolT () (if (equal? tthn tels)
                                         tthn
                                         (error 'if "then and else branch types don't match"))]
                           [else (error 'if "expected boolean test")]))]
    [lamC (a ta tr b) (let ([tb (tc b (extend-tyenv (tybind a ta) tenv))])
                        (if (equal? tr tb)
                            (funT ta tb)
                            (error 'lam "return type mismatch")))]
    [recC (f a aT rT b u)
      (let ([extended-env
             (extend-tyenv (tybind f (funT aT rT)) tenv)])
        (cond
          [(not (equal? rT (tc b
                               (extend-tyenv
                                (tybind a aT)
                                extended-env))))
           (error 'tc "body return type not correct")]
          [else (tc u extended-env)]))]))

;; (numT -> (numT -> numT)) -> (numT -> numT)
(define list-ty
  (funT
   (funT (numT) (funT (numT) (numT))) ; cons
   (funT (numT) ; nil
         (numT))))

;; numT -> (listy-ty -> (cns-ty -> (nil-ty -> numT)))
(define consF
  (lamC 'x (numT) (funT list-ty list-ty)
        (lamC 'xs list-ty list-ty
              (lamC 'cns (funT (numT) (funT (numT) (numT)))
                    (funT (numT) (numT))
                    (lamC 'nil (numT) (numT)
                          (appC
                           (appC (idC 'cns)
                                 (idC 'x))
                           (appC
                            (appC (idC 'xs)
                                  (idC 'cns))
                            (idC 'nil))))))))

(tc consF mt-tyenv)