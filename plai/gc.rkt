#lang plai-typed

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
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type Store
  [store (capacity : number) (usage : number) (storage : (listof Storage))])

(define (mt-store capacity)
  (store capacity 0 empty))

(define (lookup [x : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? x (bind-name (first env))) (bind-val (first env))]
            [else (lookup x (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (local ([define (loop s)
            (cond
              [(empty? s) (error 'lookup "location not found")]
              [else (cond
                      [(equal? loc (cell-location (first s))) (cell-val (first s))]
                      [else (loop (rest s))])])])
    (loop (store-storage sto))))

(define-type AddResult
  [l*s (location : Location) (store : Store)])

(define (add-to-store [v : Value] [s : Store] [e : Env]) : AddResult
  (local ([define (add sto)
            (let ([l (new-loc)])
              (l*s l (store (store-capacity sto) (add1 (store-usage sto)) (cons (cell l v) (store-storage sto)))))])
    (if (< (store-usage s) (store-capacity s))
        (add s)
        (let ([s2 (gc s e)])
          (if (< (store-usage s2) (store-capacity s2))
              (add s2)
              (error 'add-to-store "out of memory!"))))))

(define (override-store [cell : Storage] [sto : Store]) : Store
  (store (store-capacity sto) (store-usage sto)
         (cons cell
               (filter
                (lambda (cell2) (not (equal? (cell-location cell2)
                                             (cell-location cell))))
                (store-storage sto)))))

(define (reachable-locations (sto : Store) (env : Env)) : (listof Location)
  (cond
    [(empty? env) empty]
    [else (append (reachable-locations-from-value sto (bind-val (first env)))
                  (reachable-locations sto (rest env)))]))

(define (reachable-locations-from-value (sto : Store) (v : Value)) : (listof Location)
  (type-case Value v
    [numV (n) empty]
    [closV (a b  e) (reachable-locations sto e)]
    [boxV (loc) (cons loc (reachable-locations-from-value sto (fetch loc sto)))]))

(define (gc (sto : Store) (env : Env)) : Store
  (let* ([reachable (reachable-locations sto env)]
         [new-storage (filter (lambda (c) (member (cell-location c) reachable))
                              (store-storage sto))])
    (store (store-capacity sto)
           (length new-storage)
           new-storage)))

;; gc doesn't affect empty stores

(test
 (gc (mt-store 0) mt-env)
 (mt-store 0))

(test
 (gc (mt-store 1) mt-env)
 (mt-store 1))

(test
 (gc (mt-store 12345) mt-env)
 (mt-store 12345))

(define a-store
  (store 10
         5
         (list (cell 0 (numV 2))
               (cell 1 (numV 4))
               (cell 4 (boxV 3))
               (cell 3 (boxV 4))
               (cell 18 (closV 'x (plusC (idC 'x) (idC 'y)) (list (bind 'y (numV 10))))))))

;; gc'ing in an empty environment collects the whole store
(test
 (gc a-store mt-env)
 (mt-store (store-capacity a-store)))

;; gc'ing preserves locations reachable from the environment

(let ([sto (store 10 1 (list (cell 0 (numV 1))))]
      [env (list (bind 'x (boxV 0)))])
  (test (gc sto env)
        sto))

;; gc'ing preserves locations transitively reachable from the environment

(let ([sto (store 10 2 (list (cell 0 (numV 1))
                             (cell 1 (boxV 0))))]
      [env (list (bind 'x (boxV 1)))])
  (test (gc sto env)
        sto))

;; gc'ing preserves locations from reachable closures' enviroments

(let ([sto (store 10 2 (list (cell 0 (closV 'x (idC 'x) (list (bind 'x (boxV 1)))))
                             (cell 1 (numV 4))))]
      [env (list (bind 'x (boxV 0)))])
  (test (gc sto env)
        sto))

;; gc'ing collects unreachable locations

(let ([sto (store 10 2 (list (cell 0 (numV 1))
                             (cell 1 (numV 2))))]
      [env (list (bind 'x (boxV 1)))])
  (test (gc sto env)
        (store (store-capacity sto)
               1
               (list (cell 1 (numV 2))))))

(define-type Result
  [v*s (v : Value) (s : Store)])

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
    [idC (s) (v*s (lookup s env) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Value v-f
                         [numV (_) (error 'appC "applied number")]
                         [boxV (_) (error 'appC "applied box")]
                         [closV (arg body clos-env)
                                (type-case Result (interp a env s-f)
                                  [v*s (v-a s-a)
                                       (interp body
                                               (extend-env (bind arg v-a)
                                                           clos-env)
                                               s-a)])])])]
    [boxC (e) (type-case Result (interp e env sto)
                [v*s (v1 s1)
                     (type-case AddResult (add-to-store v1 s1 env)
                       [l*s (where s2)
                            (v*s (boxV where) s2)])])]
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

(define (interpSC [expr : ExprS] [capacity : number]) : Result
  (interp (desugar expr) mt-env (mt-store capacity)))

(define (interpS [expr : ExprS]) : Result
  (interpSC expr 100))

(test (desugar (beginS (list (numS 1) (numS 2) (numS 3))))
      (seqC (numC 1) (seqC (numC 2) (numC 3))))

(test (desugar (beginS (list (numS 0))))
      (numC 0))

(test/exn (desugar (beginS empty))
          "")

(test (v*s-v (interpS (letS 'b (boxS (numS 0)) (unboxS (idS 'b)))))
      (numV 0))

(define exercise
  (appC (lamC 'b
              (seqC (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))
                    (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))))
        (boxC (numC 0))))

(test (v*s-v (interp exercise mt-env (mt-store 100))) (numV 2))

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

;; test garbage reclamation
(test
 (v*s-v
  (interpSC
   (letS 'f (lamS 'x (letS 'x (boxS (numS 1))
                           (letS 'y (boxS (numS 1))
                                 (idS 'x))))
         (unboxS (beginS (list (appS (idS 'f) (numS 1))
                               (appS (idS 'f) (numS 1))
                               (appS (idS 'f) (numS 1))))))
   2))
 (numV 1))