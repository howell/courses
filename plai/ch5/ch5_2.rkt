#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (get-fundef [f : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined fuctnion")]
    [(cons? fds) (cond
                   [(equal? f (fdC-name (first fds))) (first fds)]
                   [else (get-fundef f (rest fds))])]))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (_) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (plusC (subst what for l)
                        (subst what for r))]))

#|
Exercise
Observe that, whereas in the numC case the interpreter returned n, substitution returns in (i.e., the original expression, equivalent at that point to writing (numC n). Why?
Answer
Because substitution is creating expressions whereas interpretation is creating values.
|#

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [idC (_) (error 'interp "free variable")]))

#|
Exercise
Modify your interpreter to substitute names with answers, not expressions.
|#

(define (subst-eager [what : number] [for : symbol] [in : ExprC]) : ExprC
  (subst (numC what) for in))

(define (interp-eager [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp-eager l fds) (interp-eager r fds))]
    [multC (l r) (* (interp-eager l fds) (interp-eager r fds))]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp-eager (subst-eager (interp-eager a fds)
                                             (fdC-arg fd)
                                             (fdC-body fd))
                                fds))]
    [idC (_) (error 'interp-eager "free variable")]))