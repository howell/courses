#lang plai-typed

#|
Exercise
Augment the language with the journal features of software transactional memory journal.
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
  [seqC (b1 : ExprC) (b2 : ExprC)]
  [newjournalC]
  [commitC]
  [discardC])

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
  [beginS (es : (listof ExprS))]
  [newjournalS]
  [commitS]
  [discardS]
  [atomicS (e : ExprS)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)]
  [unitV])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type JournalEntry
  [write-entry (location : Location) (val : Value)])

(define-type Journal
  [inactiveJ]
  [activeJ (log : (listof JournalEntry))])

(define mt-journal
  (activeJ empty))

(define (write-journal [e : JournalEntry] [j : Journal]) : Journal
  (type-case Journal j
    [inactiveJ () (error 'write-journal "wrote to inactive journal")]
    [activeJ (l) (activeJ (cons e l))]))

(define-type Result
  [v*s (v : Value) (s : Store) (j : Journal)])

(define (lookup [x : symbol] [env : Env]) : Value
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

(define (commit-journal [j : Journal] [s : Store]) : Store
  (local ([define (write-entries es s)
            (cond
              [(empty? es) s]
              [else (cons (cell (write-entry-location (first es))
                                (write-entry-val (first es)))
                          (write-entries (rest es) s))])])
    (type-case Journal j
      [inactiveJ () s]
      [activeJ (l) (write-entries l s)])))

; look inside the journal before checking the store
(define (fetchJ [loc : Location] [journal : Journal] [sto : Store]) : Value
  (type-case Journal journal
    [inactiveJ () (fetch loc sto)]
    [activeJ (log) (local ([define (loop log)
                             (cond
                               [(empty? log) (fetch loc sto)]
                               [(equal? loc (write-entry-location (first log))) (write-entry-val (first log))]
                               [else (loop (rest log))])])
                     (loop log))]))

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
                   [else (seqC (desugar (first es)) (desugar (beginS (rest es))))])]
    [newjournalS () (newjournalC)]
    [commitS () (commitC)]
    [discardS () (discardC)]
    [atomicS (e) (seqC (newjournalC) (seqC (desugar e) (commitC)))]))

(define (interp [expr : ExprC] [env : Env] [sto : Store] [journal : Journal]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto journal)]
    [plusC (l r) (type-case Result (interp l env sto journal)
                   [v*s (v-l s-l j-l)
                        (type-case Result (interp r env s-l j-l)
                          [v*s (v-r s-r j-r)
                               (v*s (num+ v-l v-r) s-r j-r)])])]
    [multC (l r) (type-case Result (interp l env sto journal)
                   [v*s (v-l s-l j-l)
                        (type-case Result (interp r env s-l j-l)
                          [v*s (v-r s-r j-r)
                               (v*s (num* v-l v-r) s-r j-r)])])]
    [idC (s) (v*s (lookup s env) sto journal)]
    [lamC (a b) (v*s (closV a b env) sto journal)]
    [appC (f a) (type-case Result (interp f env sto journal)
                  [v*s (v-f s-f j-f)
                       (type-case Value v-f
                         [numV (_) (error 'appC "applied number")]
                         [boxV (_) (error 'appC "applied box")]
                         [unitV () (error 'appC "applied unit")]
                         [closV (arg body clos-env)
                                (type-case Result (interp a env s-f j-f)
                                  [v*s (v-a s-a j-a)
                                       (interp body
                                               (extend-env (bind arg v-a)
                                                           clos-env)
                                               s-a
                                               j-a)])])])]
    [boxC (e) (type-case Result (interp e env sto journal)
                [v*s (v1 s1 j1)
                     (let ([where (new-loc)])
                       (type-case Journal j1
                         [inactiveJ () (v*s (boxV where)
                                            (override-store (cell where v1) s1)
                                            j1)]
                         [activeJ (_) (v*s (boxV where)
                                           s1
                                           (write-journal (write-entry where v1) j1))]))])]
    [unboxC (e) (type-case Result (interp e env sto journal)
                  [v*s (v1 s1 j1)
                       (type-case Value v1
                         [boxV (l) (v*s (fetchJ l j1 s1) s1 j1)]
                         [else (error 'unbox "expected box")])])]
    [setboxC (b e) (type-case Result (interp b env sto journal)
                     [v*s (v-b s-b j-b)
                          (type-case Value v-b
                            [boxV (l) (type-case Result (interp e env s-b j-b)
                                        [v*s (v-e s-e j-e)
                                             (type-case Journal j-e
                                               [inactiveJ () (v*s v-e
                                                                  (override-store (cell l v-e) s-e)
                                                                  j-e)]
                                               [activeJ (_) (v*s v-e
                                                                 s-e
                                                                 (write-journal (write-entry l v-e) j-e))])])]
                            [else (error 'setbox "expected box")])])]
    [seqC (e1 e2) (type-case Result (interp e1 env sto journal)
                    [v*s (v1 s1 j1)
                         (interp e2 env s1 j1)])]
    [newjournalC () (v*s (unitV) sto mt-journal)]
    [commitC () (v*s (unitV) (commit-journal journal sto) (inactiveJ))]
    [discardC () (v*s (unitV) sto (inactiveJ))]))

(define (interpS [expr : ExprS]) : Result
  (interp (desugar expr) mt-env mt-store (inactiveJ)))

(test (desugar (beginS (list (numS 1) (numS 2) (numS 3))))
      (seqC (numC 1) (seqC (numC 2) (numC 3))))

(test (desugar (beginS (list (numS 0))))
      (numC 0))

(test/exn (desugar (beginS empty))
          "")

(test (v*s-v (interpS (letS 'b (boxS (numS 0)) (unboxS (idS 'b)))))
      (numV 0))

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

; stm
; discard loses a write
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (newjournalS)
                            (setboxS (idS 'b) (numS 1))
                            (discardS)
                            (unboxS (idS 'b)))))))
      (numV 0))

; read modification before discarding
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (newjournalS)
                            (setboxS (idS 'b) (numS 1))
                            (letS 'v (unboxS (idS 'b))
                                  (beginS (list (discardS)
                                                (idS 'v)))))))))
      (numV 1))

; read modification after commiting
(test (v*s-v
       (interpS
        (letS 'b (boxS (numS 0))
              (beginS (list (newjournalS)
                            (setboxS (idS 'b) (numS 1))
                            (commitS)
                            (unboxS (idS 'b)))))))
      (numV 1))