#lang plai-typed

#|
Exercise
Define begin by desugaring into let (and hence into lambda).
Answer
(begin e1 e2 e3 e4 ...) => (let ([_ e1]) (begin e2 e3 e4 ...))
|#

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

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
  [boxV (l : Location)])

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

#|
Exercise
Implement the other version of store alteration, whereby we update an existing binding and thereby avoid multiple bindings for a location in the store.
|#

(define (override-store2 [c : Storage] [sto : Store]) : Store
  (cond
    [(empty? sto) (error 'override-store "location not found")]
    [else (cond
            [(equal? (cell-location c) (cell-location (first sto))) (cons c (rest sto))]
            [else (cons (first sto) (override-store2 c (rest sto)))])]))

#|
Exercise
When we look for a location to override the value stored at it, can the location fail to be present? If so, write a program that demonstrates this.
If not, explain what invariant of the interpreter prevents this from happening.
Answer
The location will always be present in the store. The only time we create new locations is in the boxC case, which is then immediately added to the store. Further, cells
are never removed from the store.
|#

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
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Value v-f
                         [numV (_) (error 'appC "applied number")]
                         [boxV (_) (error 'appC "applied box")]
                         [closV (arg body clos-env)
                                (type-case Result (interp a env s-f)
                                  [v*s (v-a s-a)
                                       (let ([where (new-loc)])
                                         (interp body
                                                 (extend-env (bind arg where)
                                                             clos-env)
                                                 (override-store (cell where v-a) s-a)))])])])]
    [boxC (e) (type-case Result (interp e env sto)
                [v*s (v1 s1)
                     (let ([where (new-loc)])
                       (v*s (boxV where)
                            (override-store (cell where v1) s1)))])]
    [unboxC (e) (type-case Result (interp e env sto)
                  [v*s (v1 s1)
                       (type-case Value v1
                         [boxV (l) (v*s (fetch l s1) s1)]
                         [else (error 'unbox "expected box")])])]
    [setboxC (b e) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Value v-b
                            [boxV (l) (type-case Result (interp e env s-b)
                                        [v*s (v-e s-e)
                                             (v*s v-e
                                                  (override-store (cell l v-e) s-e))])]
                            [else (error 'setbox "expected box")])])]
    [seqC (e1 e2) (type-case Result (interp e1 env sto)
                    [v*s (v1 s1)
                         (interp e2 env s1)])]))

(define (interpS [expr : ExprS]) : Result
  (interp (desugar expr) mt-env mt-store))

(test (desugar (beginS (list (numS 1) (numS 2) (numS 3))))
      (seqC (numC 1) (seqC (numC 2) (numC 3))))

(test (desugar (beginS (list (numS 0))))
      (numC 0))

(test/exn (desugar (beginS empty))
          "")

(test (v*s-v (interpS (letS 'b (boxS (numS 0)) (unboxS (idS 'b)))))
      (numV 0))

#|
Exercise
Represent this expression in ExprC:
(let ([b (box 0)])
  (begin (begin (set-box! b (+ 1 (unbox b)))
                (set-box! b (+ 1 (unbox b))))
         (unbox b)))
|#

(define exercise
  (appC (lamC 'b
              (seqC (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))
                    (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))))
        (boxC (numC 0))))

(test (v*s-v (interp exercise mt-env mt-store)) (numV 2))

#|
Exercise
Go through the interpreter; replace every reference to an updated store with a reference to one before update;
make sure your test cases catch all the introduced errors!
|#

(define incS : ExprS
  (lamS 'b (setboxS (idS 'b) (plusS (numS 1)
                                    (unboxS (idS 'b))))))

; plusC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (plusS (beginS (list (setboxS (idS 'b) (numS 1))
                                   (numS 0)))
                     (unboxS (idS 'b))))))
      (numV 1))

(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (plusS (numS 0)
                                   (beginS (list (setboxS (idS 'b) (numS 1))
                                                 (numS 0))))
                            (unboxS (idS 'b)))))))
      (numV 1))

; multC (copies of plusC tests)
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (multS (beginS (list (setboxS (idS 'b) (numS 1))
                                   (numS 2)))
                     (unboxS (idS 'b))))))
      (numV 2))

(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (multS (numS 0)
                                   (beginS (list (setboxS (idS 'b) (numS 1))
                                                 (numS 0))))
                            (unboxS (idS 'b)))))))
      (numV 1))

; appC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (plusS (appS (beginS (list (setboxS (idS 'b) (numS 1))
                                         (lamS 'x (idS 'x))))
                           (beginS (list (appS incS (idS 'b))
                                         (unboxS (idS 'b)))))
                     (unboxS (idS 'b))))))
      (numV 4))

; boxC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (boxS (setboxS (idS 'b) (numS 1)))
                            (unboxS (idS 'b)))))))
      (numV 1))

; unboxC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (unboxS (beginS (list (setboxS (idS 'b) (numS 1))
                                    (idS 'b)))))))
      (numV 1))

(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (plusS (unboxS (beginS (list (setboxS (idS 'b) (numS 1))
                                           (idS 'b))))
                     (unboxS (idS 'b))))))
      (numV 2))

; setboxC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (letS 'b2 (boxS (numS 0))
                    (beginS (list (setboxS (beginS (list (appS incS (idS 'b2))
                                                         (idS 'b)))
                                           (appS incS (idS 'b2)))
                                  (plusS (unboxS (idS 'b2))
                                         (unboxS (idS 'b)))))))))
      (numV 4))

; seqC
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (appS incS (idS 'b))
                            (appS incS (idS 'b)))))))
      (numV 2))