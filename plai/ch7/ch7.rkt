#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

#|
Exercise
Modify Binding and lookup, appropriately.
|#

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

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [idC (s) (lookup s env)]
    [lamC (a b) (closV a b env)]
    [appC (f a) (local ([define fd? (interp f env)])
                  (type-case Value fd?
                    [numV (_) (error 'appC "applied number")]
                    [closV (arg body clos-env) (interp body
                                                       (extend-env (bind arg (interp a env))
                                                                   clos-env))]))]))

#|
Exercise
What happens if we extend the dynamic environment instead?
Answer
We get dynamic rather than lexical scope.
|#

#|
Exercise
In what way does using an environment avoid the capture problem of substitution?
Answer
Because we associate each closure with its own environment, free variables from different closures won't be mixed up.
|#

; unbound
(test/exn (lookup 'x mt-env) "")

; lookup
(test (lookup 'x (extend-env (bind 'x (numV 5)) mt-env)) (numV 5))

; shadowing
(test (lookup 'x (extend-env (bind 'x (numV 42))
                             (extend-env (bind 'x (numV 4)) mt-env)))
      (numV 42))

; different variables
(test (lookup 'y (extend-env (bind 'x (numV 42))
                             (extend-env (bind 'y (numV 4))
                                         (extend-env (bind 'z (numV 18)) mt-env))))
      (numV 4))

; type error
(test/exn (num+ (numV 1) (closV 'b (numC 1) mt-env)) "")

; plus
(test (num+ (numV 2) (numV 3)) (numV 5))

; type error
(test/exn (num* (numV 1) (closV 'b (numC 1) mt-env)) "")

; times
(test (num* (numV 2) (numV 3)) (numV 6))

;; interp
(test (interp (appC (lamC 'const5 (plusC (numC 10) (appC (idC 'const5) (numC 10))))
                    (lamC '_ (numC 5)))
              mt-env)
      (numV 15))


(test (interp (appC (lamC 'double (plusC (numC 10) (appC (idC 'double) (plusC (numC 1) (numC 2)))))
                    (lamC 'x (plusC (idC 'x) (idC 'x))))
              mt-env)
      (numV 16))

(test (interp (appC (lamC 'quadruple (plusC (numC 10) (appC (idC 'quadruple) (plusC (numC 1) (numC 2)))))
                      (appC (lamC 'double (lamC 'x (appC (idC 'double) (appC (idC 'double) (idC 'x)))))
                            (lamC 'x (plusC (idC 'x) (idC 'x)))))
                mt-env)
        (numV 22))

#;(test/exn (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                       (numC 4)))
                        (numC 3))
                  mt-env)
          "name not found")