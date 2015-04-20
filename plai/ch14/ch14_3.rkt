#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (f : (Value (Value -> Value) -> Value))])

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

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error '+ "expected numbers")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error '* "expected numbers")]))

(define (interp/k [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value
  (type-case ExprC expr
    [numC (n) (k (numV n))]
    [idC (n) (k (lookup n env))]
    [plusC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num+ lv rv))))))]
    [multC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num* lv rv))))))]
    [lamC (a b) (k (closV (lambda (v k2)
                            (let ([new-env (extend-env (bind a v) env)])
                              (interp/k b new-env k2)))))]
    [appC (e1 e2) (interp/k e1 env
                            (lambda (fv)
                              (type-case Value fv
                                [closV (f)
                                       (interp/k e2 env
                                                 (lambda (v)
                                                   (f v k)))]
                                [else (error 'app "expected function")])))]))

(define (interp [expr : ExprC]) : Value
  (interp/k expr mt-env
            (lambda (ans)
              ans)))
