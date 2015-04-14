#lang plai-typed

#|
Exercise
An alternate implementation strategy is to have the environment map names to boxed Values. We don’t do it here because it:
(a) would be cheating, (b) wouldn’t tell us how to implement the same feature in a language without boxes,
(c) doesn’t necessarily carry over to other mutation operations, and (d) most of all, doesn’t really give us insight into
what is happening here.

It is nevertheless useful to understand, not least because you may find it a useful strategy to adopt when implementing
your own language. Therefore, alter the implementation to obey this strategy. Do you still need store-passing style?
Why or why not?

Answer
We don't need store passing style anymore, because the environment doubles as the store.
|#

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [idS (s : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [lamS (arg : symbol) (body : ExprS)]
  [boxS (arg : ExprS)]
  [unboxS (arg : ExprS)]
  [setboxS (b : ExprS) (v : ExprS)]
  [seqS (b1 : ExprS) (b2 : ExprS)]
  [letS (s : symbol) (e : ExprS) (b : ExprS)]
  [beginS (es : (listof ExprS))])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (b : (boxof Value))])

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

(define (desugar [expr : ExprS]) : ExprC
  (type-case ExprS expr
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [lamS (a b) (lamC a (desugar b))]
    [boxS (e) (boxC (desugar e))]
    [unboxS (e) (unboxC (desugar e))]
    [setboxS (b e) (setboxC (desugar b) (desugar e))]
    [seqS (e1 e2) (seqC (desugar e1) (desugar e2))]
    [letS (s e b) (appC (lamC s (desugar b)) (desugar e))]
    [beginS (es) (cond
                   [(empty? es) (error 'desugar "empty begin sequence")]
                   [(empty? (rest es)) (desugar (first es))]
                   [else (seqC (desugar (first es)) (desugar (beginS (rest es))))])]))

(define (interp [expr : ExprC] [env : Env]): Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [idC (s) (lookup s env)]
    [lamC (a b) (closV a b env)]
    [appC (f a) (let ([v-f (interp f env)])
                       (type-case Value v-f
                         [numV (_) (error 'appC "applied number")]
                         [boxV (_) (error 'appC "applied box")]
                         [closV (arg body clos-env)
                                (let ([v-a (interp a env)])
                                         (interp body
                                                 (extend-env (bind arg v-a)
                                                             clos-env)))]))]
    [boxC (e) (boxV (box (interp e env)))]
    [unboxC (e) (type-case Value (interp e env)
                  [boxV (b) (unbox b)]
                  [else (error 'unbox "expected box")])]
    [setboxC (b e) (type-case Value (interp b env)
                     [boxV (b) (let ([v (interp e env)])
                                 (begin (set-box! b v)
                                        v))]
                     [else (error 'setbox "expected box")])]
    [seqC (e1 e2) (begin (interp e1 env)
                         (interp e2 env))]))

(define (interpS [expr : ExprS]) : Value
  (interp (desugar expr) mt-env))

(test (desugar (beginS (list (numS 1) (numS 2) (numS 3))))
      (seqC (numC 1) (seqC (numC 2) (numC 3))))

(test (desugar (beginS (list (numS 0))))
      (numC 0))

(test/exn (desugar (beginS empty))
          "")

(test (interpS (letS 'b (boxS (numS 0)) (unboxS (idS 'b))))
      (numV 0))

(define incS : ExprS
  (lamS 'b (setboxS (idS 'b) (plusS (numS 1)
                                    (unboxS (idS 'b))))))

; plusC
(test (interpS
        (letS 'b (boxS (numS 0))
              (plusS (beginS (list (setboxS (idS 'b) (numS 1))
                                   (numS 0)))
                     (unboxS (idS 'b)))))
      (numV 1))

(test (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (plusS (numS 0)
                                   (beginS (list (setboxS (idS 'b) (numS 1))
                                                 (numS 0))))
                            (unboxS (idS 'b))))))
      (numV 1))

; multC (copies of plusC tests)
(test (interpS
        (letS 'b (boxS (numS 0))
              (multS (beginS (list (setboxS (idS 'b) (numS 1))
                                   (numS 2)))
                     (unboxS (idS 'b)))))
      (numV 2))

(test (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (multS (numS 0)
                                   (beginS (list (setboxS (idS 'b) (numS 1))
                                                 (numS 0))))
                            (unboxS (idS 'b))))))
      (numV 1))

; appC
(test (interpS
        (letS 'b (boxS (numS 0))
              (plusS (appS (beginS (list (setboxS (idS 'b) (numS 1))
                                         (lamS 'x (idS 'x))))
                           (beginS (list (appS incS (idS 'b))
                                         (unboxS (idS 'b)))))
                     (unboxS (idS 'b)))))
      (numV 4))

; boxC
(test (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (boxS (setboxS (idS 'b) (numS 1)))
                            (unboxS (idS 'b))))))
      (numV 1))

; unboxC
(test (interpS
        (letS 'b (boxS (numS 0))
              (unboxS (beginS (list (setboxS (idS 'b) (numS 1))
                                    (idS 'b))))))
      (numV 1))

(test (interpS
        (letS 'b (boxS (numS 0))
              (plusS (unboxS (beginS (list (setboxS (idS 'b) (numS 1))
                                           (idS 'b))))
                     (unboxS (idS 'b)))))
      (numV 2))

; setboxC
(test (interpS
        (letS 'b (boxS (numS 0))
              (letS 'b2 (boxS (numS 0))
                    (beginS (list (setboxS (beginS (list (appS incS (idS 'b2))
                                                         (idS 'b)))
                                           (appS incS (idS 'b2)))
                                  (plusS (unboxS (idS 'b2))
                                         (unboxS (idS 'b))))))))
      (numV 4))

; seqC
(test (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (appS incS (idS 'b))
                            (appS incS (idS 'b))))))
      (numV 2))
