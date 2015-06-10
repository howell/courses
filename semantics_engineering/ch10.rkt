#lang racket

(struct ident (sym)
  #:transparent)
(struct abs (x t m)
  #:transparent)
(struct app (f x)
  #:transparent)
(struct basic (b)
  #:transparent)
(struct prim (op args)
  #:transparent)

(struct numT ()
  #:transparent)
(struct arrowT (dom codom)
  #:transparent)

(define (get-builtin b)
  (numT))

(define (tycheck-prim-op op argTys)
  (match (cons op argTys)
    [(list '+ tl tr) (if (and (numT? tl) (numT? tr))
                         (numT)
                         (error '+ "parameter type mismatch"))]
    [(list '- tl tr) (if (and (numT? tl) (numT? tr))
                         (numT)
                         (error '- "parameter type mismatch"))]
    [(list '* tl tr) (if (and (numT? tl) (numT? tr))
                         (numT)
                         (error '* "parameter type mismatch"))]
    [(list '^ tl tr) (if (and (numT? tl) (numT? tr))
                         (numT)
                         (error '^ "parameter type mismatch"))]
    [(list 'add1 tx) (if (numT? tx)
                         (numT)
                         (error 'add1 "parameter type mismatch"))]
    [(list 'sub1 tx) (if (numT? tx)
                         (numT)
                         (error 'sub1 "parameter type mismatch"))]
    [(list 'iszero tx) (if (numT? tx)
                           (arrowT (numT) (arrowT (numT) (numT)))
                           (error 'iszero "parameter type mismatch"))]
    [else (error 'prim-op "invalid primitive operation")]))

(define (env-lookup env x)
  (env x))

(define (extend-env env x t)
  (lambda (xn)
    (if (equal? x xn)
        t
        (env xn))))

(define empty-env
  (lambda (x)
    (error 'lookup "unbound identifier")))

(define (tycheck env expr)
  (match expr
    [(ident x) (env x)]
    [(abs x tx body) (let ([bodyT (tycheck (extend-env env x tx) body)])
                       (arrowT tx bodyT))]
    [(app f x) (match (tycheck env f)
                 [(arrowT t1 t2) (let ([tx (tycheck env x)])
                                   (if (equal? t1 tx)
                                       t2
                                       (error 'app "parameter type mismatch")))])]
    [(basic b) (get-builtin b)]
    [(prim op args) (let ([argtys (map (lambda (e) (tycheck env e)) args)])
                      (tycheck-prim-op op argtys))]
    [else (error 'tycheck "illformed expression")]))

(module+ test
  (require rackunit)
  
  ;; shorthand for typechecking the the empty environment
  (define (tc expr)
    (tycheck empty-env expr))
  
  (check-equal? (tc (basic 5))
                (numT)
                "numbers typecheck")
  
  (check-equal? (tycheck (extend-env empty-env 'x (arrowT (numT) (numT)))
                         (ident 'x))
                (arrowT (numT) (numT))
                "env lookup")
  
  (check-exn exn:fail? (lambda ()
                         (tc (ident 'x)))
             "unbound ident")
  
  (check-equal? (let ([env (extend-env (extend-env empty-env 'x (numT))
                                       'x (arrowT (numT) (numT)))])
                  (tycheck env (ident 'x)))
                (arrowT (numT) (numT))
                "variable shadowing")
  
  (check-equal? (tc (abs 'x (numT) (ident 'x)))
                (arrowT (numT) (numT))
                "identity func")
  
  (check-equal? (tc (app (abs 'x (numT) (ident 'x))
                         (basic 5)))
                (numT)
                "basic application")
  
  (check-exn exn:fail? (lambda ()
                         (tc (app (basic 5) (basic 5))))
             "apply non-function")
  
  (check-exn exn:fail?
             (lambda ()
               (tc (app (abs 'x (numT) (ident 'x))
                        (abs 'x (numT) (ident 'x)))))
             "parameter type mismatch")
  
  (check-equal? (tc (prim '+ (list (basic 5) (basic 5))))
                (numT)
                "typecheck plus")
  
  (check-equal? (tc (prim 'add1 (list (basic 1))))
                (numT)
                "typecheck add1")
  
  (check-exn exn:fail?
             (lambda ()
               (tc (prim 'add1 (list (abs 'x (numT) (ident 'x))))))
             "primitive type mismatch")
  )