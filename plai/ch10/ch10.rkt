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
  [seqC (b1 : ExprC) (b2 : ExprC)]
  [consC (hd : ExprC) (tl : ExprC)]
  [nullC]
  [carC (e : ExprC)]
  [cdrC (e : ExprC)]
  [insertC (e : ExprC) (t : ExprC)]
  [leafC]
  [leftC (e : ExprC)]
  [rightC (e : ExprC)]
  [nodeC (e : ExprC)]
  [if0C (tst : ExprC) (then : ExprC) (else : ExprC)]
  [objC (ns : (listof symbol)) (es : (listof ExprC))]
  [msgC (o : ExprC) (n : symbol)])

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
  [consS (hd : ExprS) (tl : ExprS)]
  [nullS]
  [carS (e : ExprS)]
  [cdrS (e : ExprS)]
  [insertS (e : ExprS) (t : ExprS)]
  [leafS]
  [leftS (e : ExprS)]
  [rightS (e : ExprS)]
  [nodeS (e : ExprS)]
  [if0S (tst : ExprS) (then : ExprS) (else : ExprS)]
  [subS (l : ExprS) (r : ExprS)]
  [objS (ns : (listof symbol)) (es : (listof ExprS))]
  [msgS (o : ExprS) (n : symbol)]
  [methodS (o : ExprS) (n : symbol) (a : ExprS)])

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
    [consS (e1 e2) (consC (desugar e1) (desugar e2))]
    [nullS () (nullC)]
    [carS (e) (carC (desugar e))]
    [cdrS (e) (cdrC (desugar e))]
    [insertS (e1 e2) (insertC (desugar e1) (desugar e2))]
    [leafS () (leafC)]
    [leftS (e) (leftC (desugar e))]
    [rightS (e) (rightC (desugar e))]
    [nodeS (e) (nodeC (desugar e))]
    [if0S (tst t e) (if0C (desugar tst) (desugar t) (desugar e))]
    [subS (l r) (plusC (desugar l)
                       (multC (desugar r)
                              (numC -1)))]
    [objS (ns es) (objC ns (map desugar es))]
    [msgS (o n) (msgC (desugar o) n)]
    [methodS (o n a) (appC (msgC (desugar o) n) (desugar a))]))

(define-type BinaryTree
  [leaf]
  [branch (l : BinaryTree) (n : number) (r : BinaryTree)])

(define (insert [n : number] (t : BinaryTree)) : BinaryTree
  (type-case BinaryTree t
    [leaf () (branch (leaf) n (leaf))]
    [branch (l m r)
            (cond
              [(< n m) (branch (insert n l) m r)]
              [(> n m) (branch l m (insert n r))]
              [else t])]))

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)]
  [listV (v : (listof Value))]
  [treeV (v : BinaryTree)]
  [objV (ns : (listof symbol)) (vs : (listof Value))])

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

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (lookup [x : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? x (bind-name (first env))) (bind-val (first env))]
            [else (lookup x (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'lookup "location not found")]
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

(define-type Accum
  [accum (l : (listof Value)) (s : Store)])

(define (lookup-msg [n : symbol] [ns : (listof symbol)] [vs : (listof Value)]) : Value
  (cond
    [(or (empty? ns) (empty? vs)) (error 'msg "unknown label")]
    [(symbol=? n (first ns)) (first vs)]
    [else (lookup-msg n (rest ns) (rest vs))]))

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
                         [closV (arg body clos-env)
                                (type-case Result (interp a env s-f)
                                  [v*s (v-a s-a)
                                       (interp body
                                               (extend-env (bind arg v-a)
                                                           clos-env)
                                               s-a)])]
                         [else (error 'app "applied non-function value")])])]
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
                         (interp e2 env s1)])]
    [consC (e1 e2) (type-case Result (interp e1 env sto)
                     [v*s (v-e1 s-e1)
                          (type-case Result (interp e2 env s-e1)
                            [v*s (v-e2 s-e2)
                                 (type-case Value v-e2
                                   [listV (l) (v*s (listV (cons v-e1 l)) s-e2)]
                                   [else (error 'cons "cons'd on to non-list")])])])]
    [nullC () (v*s (listV empty) sto)]
    [carC (e) (type-case Result (interp e env sto)
                [v*s (v-e s-e)
                     (type-case Value v-e
                       [listV (l) (cond
                                    [(empty? l) (error 'car "empty list")]
                                    [else (v*s (first l) s-e)])]
                       [else (error 'car "non-list")])])]
    [cdrC (e) (type-case Result (interp e env sto)
                [v*s (v-e s-e)
                     (type-case Value v-e
                       [listV (l) (cond
                                    [(empty? l) (error 'cdr "empty list")]
                                    [else (v*s (listV (rest l)) s-e)])]
                       [else (error 'cdr "non-list")])])]
    [insertC (e t) (type-case Result (interp e env sto)
                     [v*s (v-e s-e)
                          (type-case Result (interp t env s-e)
                            [v*s (v-t s-t)
                                 (type-case Value v-e
                                   [numV (n) (type-case Value v-t
                                               [treeV (t) (v*s (treeV (insert n t)) s-t)]
                                               [else (error 'insert "inserted in to non-tree")])]
                                   [else (error 'insert "expected number to insert")])])])]
    [leafC () (v*s (treeV (leaf)) sto)]
    [leftC (e) (type-case Result (interp e env sto)
                 [v*s (v-e s-e)
                      (type-case Value v-e
                        [treeV (t) (type-case BinaryTree t
                                     [branch (l n r) (v*s (treeV l) s-e)]
                                     [else (error 'left "empty tree")])]
                        [else (error 'left "expected tree")])])]
    [rightC (e) (type-case Result (interp e env sto)
                  [v*s (v-e s-e)
                       (type-case Value v-e
                         [treeV (t) (type-case BinaryTree t
                                      [branch (l n r) (v*s (treeV r) s-e)]
                                      [else (error 'right "empty tree")])]
                         [else (error 'right "expected tree")])])]
    [nodeC (e) (type-case Result (interp e env sto)
                 [v*s (v-e s-e)
                      (type-case Value v-e
                        [treeV (t) (type-case BinaryTree t
                                     [branch (l n r) (v*s (numV n) s-e)]
                                     [else (error 'node "empty tree")])]
                        [else (error 'node "expected tree")])])]
    [if0C (tst t e) (type-case Result (interp tst env sto)
                      [v*s (v-tst s-tst)
                           (type-case Value v-tst
                             [numV (n)
                                   (if (zero? n)
                                       (interp t env s-tst)
                                       (interp e env s-tst))]
                             [else (error 'if0 "expected number")])])]
    [objC (ns es) (let ([acc (foldr (lambda (e acc)
                                      (type-case Result (interp e env (accum-s acc))
                                        (v*s (v-e s-e)
                                             (accum (cons v-e (accum-l acc)) s-e))))
                                    (accum empty sto)
                                    es)])
                    (v*s (objV ns (accum-l acc)) (accum-s acc)))]
    [msgC (o n) (type-case Result (interp o env sto)
                  [v*s (v-o s-o)
                       (type-case Value v-o
                         [objV (ns vs) (v*s (lookup-msg n ns vs) s-o)]
                         [else (error 'msg "expected object")])])]))

(define (interpS [expr : ExprS]) : Result
  (interp (desugar expr) mt-env mt-store))

(test
 (v*s-v
  (interpS
   (carS (consS (numS 1) (nullS)))))
 (numV 1))

(test
 (v*s-v
  (interpS
   (carS
    (cdrS
     (consS (numS 1)
            (consS (numS 2)
                   (nullS)))))))
 (numV 2))

(test/exn
 (interpS
  (carS (nullS)))
 "")

(test/exn
 (interpS
  (cdrS (nullS)))
 "")

(test
 (v*s-v
  (interpS
   (nodeS (insertS (numS 1)
                   (leafS)))))
 (numV 1))

(test
 (v*s-v
  (interpS
   (nodeS
    (leftS
     (insertS (numS 2)
              (insertS (numS 0)
                       (insertS (numS 1)
                                (leafS))))))))
 (numV 0))

(test
 (v*s-v
  (interpS
   (nodeS
    (rightS
     (insertS (numS 2)
              (insertS (numS 0)
                       (insertS (numS 1)
                                (leafS))))))))
 (numV 2))

(test
 (v*s-v
  (interpS
   (nodeS
    (insertS (numS 2)
             (insertS (numS 0)
                      (insertS (numS 1)
                               (leafS)))))))
 (numV 1))

(test/exn
 (interpS
  (nodeS (leafS)))
 "")

(test/exn
 (interpS
  (leftS (leafS)))
 "")

(test/exn
 (interpS
  (rightS (leafS)))
 "")

(test
 (v*s-v
  (interpS
   (letS 'b (boxS (numS 0))
         (beginS (list (setboxS (idS 'b) (idS 'b))
                       (letS 'b2 (unboxS (idS 'b))
                             (beginS (list (setboxS (idS 'b2) (numS 0))
                                           (setboxS (idS 'b) (numS 1))
                                           (unboxS (idS 'b2))))))))))
 (numV 1))

(define fact
  (letS 'fact (boxS (numS 0))
        (beginS (list (setboxS (idS 'fact)
                               (lamS 'n
                                     (if0S (idS 'n)
                                           (numS 1)
                                           (multS (idS 'n)
                                                  (appS (unboxS (idS 'fact))
                                                        (subS (idS 'n) (numS 1)))))))
                      (unboxS (idS 'fact))))))

(test
 (v*s-v
  (interpS
   (appS fact (numS 10))))
 (numV 3628800))

(test
 (v*s-v
  (interpS
   (objS (list 'a)
         (list (numS 0)))))
 (objV (list 'a) (list (numV 0))))

(test
 (v*s-v
  (interpS
   (objS (list 'a 'b 'c)
         (list (numS 0)
               (plusS (numS 2) (numS 3))
               (multS (numS 5) (numS 6))))))
 (objV (list 'a 'b 'c)
       (list (numV 0)
             (numV 5)
             (numV 30))))

(test
 (v*s-v
  (interpS
   (objS empty empty)))
 (objV empty empty))

(test
 (v*s-v
  (interpS
   (msgS
    (objS (list 'a)
          (list (numS 0)))
    'a)))
 (numV 0))

(test/exn
 (v*s-v
  (interpS
   (msgS
    (objS (list 'a)
          (list (numS 0)))
    'b)))
 "")

(test
 (v*s-v
  (interpS
   (msgS (objS (list 'a 'b 'c)
               (list (numS 0)
                     (plusS (numS 2) (numS 3))
                     (multS (numS 5) (numS 6))))
         'b)))
 (numV 5))

(test
 (v*s-v
  (interpS
   (msgS
    (msgS
     (objS (list 'a)
           (list (objS (list 'a)
                       (list (numS 42)))))
     'a)
    'a)))
 (numV 42))

(test
 (v*s-v
  (interpS
   (letS 'o (objS (list 'add1 'sub1)
                  (list (lamS 'x (plusS (idS 'x) (numS 1)))
                        (lamS 'x (plusS (idS 'x) (numS -1)))))
         (methodS (idS 'o) 'add1 (numS 3)))))
 (numV 4))
