#lang plai-typed

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (lookup [x : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? x (bind-name (first env))) (bind-val (first env))]
            [else (lookup x (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(equal? loc (cell-location (first sto))) (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error '+ "expected numbers")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error '* "expected numbers")]))

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)])])]
    
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)])])]
    [varC (s) (v*s (fetch (lookup s env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Value v-f
                         [numV (_) (error 'appC "applied number")]
                         [closV (arg body clos-env)
                                (type-case Result (interp a env s-f)
                                  [v*s (v-a s-a)
                                       (let ([where (new-loc)])
                                         (interp body
                                                 (extend-env (bind arg where)
                                                             clos-env)
                                                 (override-store (cell where v-a) s-a)))])])])]
    [setC (var val) (type-case Result (interp val env sto)
                      [v*s (v-val s-val)
                           (let ([where (lookup var env)])
                             (v*s v-val
                                  (override-store (cell where v-val)
                                                  s-val)))])]
    [seqC (e1 e2) (type-case Result (interp e1 env sto)
                    [v*s (v1 s1)
                         (interp e2 env s1)])]))

(test
 (v*s-v 
  (interp
   (appC
    (appC
     (lamC 'f
           (lamC 'y
                 (seqC (appC (varC 'f) (varC 'y))
                       (varC 'y))))
     (lamC 'x (setC 'x (numC 3))))
    (numC 5))
   mt-env mt-store))
 (numV 5))