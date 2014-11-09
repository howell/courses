;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [new-env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([clos-env (closure-env v1)]
                      [fun (closure-fun v1)]
                      [formal (fun-formal fun)]
                      [arg-env (cons (cons formal v2) clos-env)]
                      [call-env (if (fun-nameopt fun)
                                    (cons (cons (fun-nameopt fun) v1) arg-env)
                                    arg-env)])
                 (eval-under-env (fun-body fun) call-env))
               (error "MUPL call applied to non-function")))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))

;; Problem 4

(define mupl-map
  (fun "map" "f"
       (fun "map'" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (mlet* (list (cons "hd" (fst (var "xs")))
                                  (cons "tl" (snd (var "xs"))))
                            (apair (call (var "f") (var "hd")) (call (var "map'") (var "tl"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "i") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

(define (find-free-vars e)
  (define (search bound e)
    (cond [(var? e)
           (let ([s (var-string e)])
             (if (set-member? bound s) (set) (set s)))]
          [(int? e) (set)]
          [(add? e) (set-union (search bound (add-e1 e)) (search bound (add-e2 e)))]
          [(ifgreater? e) (set-union (search bound (ifgreater-e1 e))
                                     (search bound (ifgreater-e2 e))
                                     (search bound (ifgreater-e3 e))
                                     (search bound (ifgreater-e4 e)))]
          [(fun? e)
           (let* ([name-bound (if (fun-nameopt e) (set-add bound (fun-nameopt e)) bound)]
                  [formal-bound (set-add name-bound (fun-formal e))])
             (search formal-bound (fun-body e)))]
          [(call? e) (set-union (search bound (call-funexp e)) (search bound (call-actual e)))]
          [(mlet? e)
           (let ([new-bound (set-add bound (mlet-var e))])
             (set-union (search bound (mlet-e e)) (search new-bound (mlet-body e))))]
          [(apair? e) (set-union (search bound (apair-e1 e)) (search bound (apair-e2 e)))]
          [(fst? e) (search bound (fst-e e))]
          [(snd? e) (search bound (snd-e e))]
          [(aunit? e) (set)]
          [(isaunit? e) (search bound (isaunit-e e))]
          [(closure? e) (set)]
          [(fun-challenge? e) (fun-challenge-freevars e)]
          [#t (error (format "bad MUPL expression: ~v" e))]))
  (search (set) e))

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (let ([cfv compute-free-vars])
    (cond [(var? e) e]
          [(int? e) e]
          [(add? e) (add (cfv (add-e1 e)) (cfv (add-e2 e)))]
          [(ifgreater? e) (ifgreater (cfv (ifgreater-e1 e)) (cfv (ifgreater-e2 e)) (cfv (ifgreater-e3 e)) (cfv (ifgreater-e4 e)))]
          [(fun? e) (fun-challenge (fun-nameopt e) (fun-formal e) (cfv (fun-body e)) (find-free-vars e))]
          [(call? e) (call (cfv (call-funexp e)) (cfv (call-actual e)))]
          [(mlet? e) (mlet (mlet-var e) (cfv (mlet-e e)) (cfv (mlet-body e)))]
          [(apair? e) (apair (cfv (apair-e1 e)) (cfv (apair-e2 e)))]
          [(fst? e) (fst (cfv (fst-e e)))]
          [(snd? e) (snd (cfv (snd-e e)))]
          [(aunit? e) e]
          [(isaunit? e) (isaunit (cfv (isaunit-e e)))]
          [(closure? e) e]
          [(fun-challenge? e) e]
          [#t (error (format "bad MUPL expression: ~v" e))])))
        

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([v (eval-under-env-c (mlet-e e) env)]
                [new-env (cons (cons (mlet-var e) v) env)])
           (eval-under-env-c (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fun? e) (error "fun's should be replaced with fun-challenge")]
        [(fun-challenge? e)
         (let ([clos-env (filter (lambda (x)
                                   (set-member? (fun-challenge-freevars e) (car x)))
                                 env)])
           (closure clos-env e))]
        [(closure? e) e]
        [(call? e)
         (let ([v1 (eval-under-env-c (call-funexp e) env)]
               [v2 (eval-under-env-c (call-actual e) env)])
           (if (closure? v1)
               (let* ([clos-env (closure-env v1)]
                      [fun (closure-fun v1)]
                      [formal (fun-challenge-formal fun)]
                      [arg-env (cons (cons formal v2) clos-env)]
                      [call-env (if (fun-challenge-nameopt fun)
                                    (cons (cons (fun-challenge-nameopt fun) v1) arg-env)
                                    arg-env)])
                 (eval-under-env-c (fun-challenge-body fun) call-env))
               (error "MUPL call applied to non-function")))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
