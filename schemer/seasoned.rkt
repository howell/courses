#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (two-in-a-row? lat)))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceding (car lat))
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond
      ((null? tup) null)
      (else (cons (+ sum (car tup))
                  (sum-of-prefixes-b (+ sum (car tup)) (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) null)
      (else (cons (pick (car tup) (cons (car tup) rev-pre))
                  (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup null)))

(define multirember
  (lambda (a lat)
    (letrec
        ([mr (lambda (lat)
               (cond
                 ((null? lat) null)
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat) (mr (cdr lat))))))])
      (mr lat))))

(define rember-f
  (lambda (match?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((match? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f match?) a (cdr l))))))))

(define rember-eq?
  (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (letrec
        ([m-f (lambda (a l)
                (cond
                  ((null? l) null)
                  ((test? a (car l)) (m-f a (cdr l)))
                  (else (cons (car l) (m-f a (cdr l))))))])
      m-f)))

(define member?
  (lambda (a lat)
    (letrec
        ([loop (lambda (l)
                 (cond
                   ((null? l) #f)
                   (else (or (eq? a (car l))
                             (loop (cdr l))))))])
      (loop lat))))


#;(define union
    (lambda (s1 s2)
      (cond
        ((null? s1) s2)
        ((member? (car s1) s2) (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2))))))

(define union
  (lambda (s1 s2)
    (letrec ([loop (lambda (s1)
                     (cond
                       ((null? s1) s2)
                       ((member? (car s1) s2) (loop (cdr s2)))
                       (else (cons (car s1) (loop (cdr s1))))))])
      (loop s1))))

(define intersect
  (lambda (set1 set2)
    (letrec ([loop (lambda (s1)
                     (cond
                       ((null? s1) null)
                       ((member? (car s1) set2) (cons (car s1)
                                                      (loop (cdr s1) set2)))
                       (else (loop (cdr s1) set2))))])
      (cond
        ((null? set2) null)
        (else (loop set1))))))

(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec ([A (lambda (lset)
                    (cond
                      ((null? (car lset)) (hop null))
                      ((null? (cdr lset)) (car lset))
                      (else (I (car lset) (A (cdr lset))))))]
               [I (lambda (set1 set2)
                    (letrec ([loop (lambda (s1)
                                     (cond
                                       ((null? s1) null)
                                       ((member? (car s1) set2) (cons (car s1)
                                                                      (loop (cdr s1) set2)))
                                       (else (loop (cdr s1) set2))))])
                      (cond
                        ((null? set2) null)
                        (else (loop set1)))))])
        (cond
          ((null? lset) null)
          (else (A lset)))))))

(define rember
  (lambda (a lat)
    (letrec ([loop (lambda (lat)
                     (cond
                       ((null? lat) lat)
                       ((eq? a (car lat)) (cdr lat))
                       (else (cons (car lat) (loop (cdr lat))))))])
      (loop lat))))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec ([R (lambda (lat)
                    (cond
                      ((null? lat) null)
                      ((eq? a (car lat)) (skip (R (cdr lat))))
                      (else (cons (car lat) (R (cdr lat))))))])
        (R lat)))))

#;(define leftmost
    (lambda (l)
      (cond
        ((null? l) null)
        ((atom? (car l)) (car l))
        (else (let ([lmost (leftmost (car l))])
                (cond
                  ((atom? lmost) lmost)
                  (else (leftmost (cdr l)))))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o< n m) #f)
      ((o> n m) #f)
      (else #t))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (cond
                    ((null? l2) #t)
                    ((atom? (car l2)) #f)
                    (else #f)))
      ((atom? (car l1)) (cond
                          ((null? l2) #f)
                          ((atom? (car l2)) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                          (else #f)))
      (else (cond
              ((null? l2) #f)
              ((atom? (car l2)) #f)
              (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))

(define rember1*
  (lambda (a l)
    (letrec
        ([loop (lambda (l)
                 (cond
                   ((null? l) null)
                   ((atom? (car l)) (cond
                                      ((eq? a (car l)) (cdr l))
                                      (else (cons (car l) (loop (cdr l))))))
                   (else (let ([av (loop (car l))])
                           (cond
                             ((eqlist? av (car l)) (cons (car l) (loop (cdr l))))
                             (else (cons av (cdr l))))))))])
      (loop l))))

#;(define depth*
    (lambda (l)
      (cond
        ((null? l) 1)
        ((atom? (car l) (depth* (cdr l))))
        (else (let ([car-depth (add1 (depth* (car l)))]
                    [cdr-depth (depth* (cdr l))])
                (cond
                  ((o> cdr-depth car-depth) cdr-depth)
                  (else car-depth)))))))

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec ([lm (lambda (l)
                     (cond
                       ((null? l) null)
                       ((atom? (car l)) (skip (car l)))
                       (else (begin
                               (lm (car l))
                               (lm (cdr l))))))])
        (lm l)))))

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l)) (if (eq? a (car l))
                           (cdr l)
                           (cons (car l) (rm a (cdr l) oh))))
      (else (if (atom?
                 (let/cc oh
                   (rm a (car l) oh)))
                (cons (car l) (rm a (cdr l) oh))
                (cons (rm a (car l) 0) (cdr l)))))))

(define x null)

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons x
                null))))

(define omnivore
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  null)))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons food
                (cons 'more
                      (cons food
                            null))))))

(define chez-nous
  (lambda ()
    (let ([y x])
      (set! x food)
      (set! food y))))

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake
                null))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (sweet-tooth food)))

(define ingredients null)

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (sweet-tooth food)))

#;(define deep
    (lambda (m)
      (cond
        ((zero? m) 'pizza)
        (else (cons (deep (sub1 m)) null)))))

#;(define Rs null)
#;(define Ns null)

#;(define deepR
    (lambda (n)
      (let ([res (deep n)])
        (set! Rs (cons res Rs))
        (set! Ns (cons n Ns))
        res)))

(define find
  (lambda (n Ns Rs)
    (letrec ([loop (lambda (Ns Rs)
                     (cond
                       ((null? Ns) #f)
                       ((null? Rs) #f)
                       ((o= n (car Ns)) (car Rs))
                       (else (loop (cdr Ns) (cdr Rs)))))])
      (loop Ns Rs))))

#;(define deepM
    (let ([Ns null]
          [Rs null])
      (lambda (n)
        (let ([r (find n Ns Rs)])
          (if (list? r)
              r
              (let ([res (deep n)])
                (set! Rs (cons res Rs))
                (set! Ns (cons n Ns))
                res))))))

#;(define deep
    (lambda (m)
      (cond
        ((zero? m) 'pizza)
        (else (cons (deepM (sub1 m)) null)))))

#;(define length
    (let ([h (lambda (l) 0)])
      (set! h (lambda (l)
                (cond
                  ((null? l) 0)
                  (else (add1 (h (cdr l)))))))
      h))

(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

(define Y!
  (lambda (L)
    (let ([h (lambda (l) 0)])
      (set! h (L (lambda (a) (h a))))
      (L h))))

(define Y-bang
  (lambda (f)
    (letrec ([h (f (lambda (arg) (h arg)))])
      h)))

(define length
  (Y! L))

(define D
  (lambda (depth*)
    (lambda (l)
      (cond
        ((null? l) 1)
        ((atom? (car l)) (depth* (cdr l)))
        (else (let ([car-depth (add1 (depth* (car l)))]
                    [cdr-depth (depth* (cdr l))])
                (cond
                  ((o> cdr-depth car-depth) cdr-depth)
                  (else car-depth))))))))

(define depth*
  (Y! D))

(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define biz
  (let ([x 0])
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (o= a x)
            0
            (f a))))))

#;(define deepM
    (let ([Ns null]
          [Rs null]
          [D (lambda (m)
               (if (zero? m)
                   'pizza
                   (cons (deepM (sub1 m)) null)))])
      (lambda (n)
        (let ([r (find n Ns Rs)])
          (if (list? r)
              r
              (let ([res (D n)])
                (set! Rs (cons res Rs))
                (set! Ns (cons n Ns))
                res))))))

(define counter #f)

(define set-counter #f)

(define consC
  (let ([N 0])
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (consC (deep (sub1 m)) null)))))

(define supercounter
  (lambda (f)
    (letrec
        ([S (lambda (n)
              (if (zero? n)
                  (f n)
                  (begin
                    (f n)
                    (S (sub1 n)))))])
      (begin
        (S 1000)
        (counter)))))

(define deepM
  (let ([Ns null]
        [Rs null]
        [D (lambda (m)
             (if (zero? m)
                 'pizza
                 (consC (deepM (sub1 m)) null)))])
    (lambda (n)
      (let ([r (find n Ns Rs)])
        (if (list? r)
            r
            (let ([res (D n)])
              (set! Rs (cons res Rs))
              (set! Ns (cons n Ns))
              res))))))

#;(define kons
    (lambda (kar kdr)
      (lambda (selector)
        (selector kar kdr))))

#;(define kar
    (lambda (l)
      (l (lambda (kar kdr) kar))))

#;(define kdr
    (lambda (l)
      (l (lambda (kar kdr) kdr))))

(define bons
  (lambda (kar)
    (let ([kdr null])
      (lambda (selector)
        (selector (lambda (x)
                    (set! kdr x))
                  kar
                  kdr)))))

(define kar
  (lambda (l)
    (l (lambda (set-kdr kar kdr) kar))))

(define kdr
  (lambda (l)
    (l (lambda (set-kdr kar kdr) kdr))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (set-kdr kar kdr) set-kdr)) x)))

(define kons
  (lambda (a d)
    (let ([l (bons a)])
      (set-kdr l d)
      l)))

(define lots
  (lambda (m)
    (cond
      ((zero? m) null)
      (else (kons 'egg (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (kdr l)))))))

(define eklist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else (and (eq? (kar ls1) (kar ls2))
                 (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (c1 c2)
    (let ([t1 (kdr c1)]
          [t2 (kdr c2)])
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ([v (o= (kdr c1) (kdr c2))])
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(define add-at-end
  (lambda (l)
    (cond
      ((null? (kdr l)) (kons (kar l) (kons 'egg null)))
      (else (kons (kar l) (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ([A (lambda (ls)
              (cond
                ((null? (kdr ls)) (set-kdr ls (kons 'egg null)))
                (else (A (kdr ls)))))])
      (A l)
      l)))

(define dozen
  (lots 12))

(define bakers-dozen
  (add-at-end dozen))

(define bakers-dozen-too
  (add-at-end-too dozen))

(define bakers-dozen-again
  (add-at-end dozen))

(define last-kons
  (lambda (ls)
    (cond
      ((null? (kdr ls)) ls)
      (else (last-kons (kdr ls))))))

(define long
  (lots 12))

(define finite-lenkth
  (lambda (p)
    (let/cc infinite
      (letrec ([C (lambda (p q)
                    (cond
                      ((same? p q) (infinite #f))
                      ((null? q) 0)
                      ((null? (kdr q)) 1)
                      (else (o+ (C (sl p) (qk q)) 2))))]
               [qk (lambda (x) (kdr (kdr x)))]
               [sl (lambda (x) (kdr x))])
        (cond
          ((null? p) 0)
          (else (add1 (C p (kdr p)))))))))

(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p null)
       null)
      null)
     null)))

(define toppings #f)

(define deepB
  (lambda (m)
    (cond
      ((zero? m) (let/cc jump
                   (set! toppings jump)
                   'pizza))
      (else (cons (deepB (sub1 m)) null)))))

(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k 'pizza))
      (else (deep&co (sub1 m)
                     (lambda (x) (k (cons x null))))))))

(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m) (begin
                   (set! toppings k)
                   (k 'pizza)))
      (else (deep&coB (sub1 m) (lambda (x) (k (cons x null))))))))

(define leave #f)

(define walk
  (lambda (l)
    (cond
      ((null? l) null)
      ((atom? (car l)) (leave (car l)))
      (else (begin
              (walk (car l))
              (walk (cdr l)))))))

(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))

(define fill #f)

(define waddle
  (lambda (l)
    (cond
      ((null? l) null)
      ((atom? (car l)) (begin
                         (let/cc rest
                           (set! fill rest)
                           (leave (car l)))
                         (waddle (cdr l))))
      (else (begin
              (waddle (car l))
              (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill 'go))))

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave null))))

(define two-in-a-row*?
  (lambda (l)
    (let ([fst (get-first l)])
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ([next (get-next 'go)])
      (cond
        ((null? next) #f)
        ((eq? a next) #t)
        (else (two-in-a-row-b*? next))))))

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (if (eq? name1 name2)
          value
          (table name2)))))

(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e)) (eq? (car e) 'define))
      (else #f))))

(define global-table #f)

(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new) (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (val set-f) (set-f new)))))

(define unbox
  (lambda (box)
    (box (lambda (val set-f) val))))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es)) (meaning (car es) table))
      (else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))))))

(define box-all
  (lambda (as)
    (cond
      ((null? as) null)
      (else (cons (box (car as))
                  (box-all (cdr as)))))))

(define multi-extend
  (lambda (formals boxed-args table)
    (if (null? formals)
        table
        (extend (car formals)
                (car boxed-args)
                (multi-extend (cdr formals)
                              (cdr boxed-args)
                              table)))))

(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (odd? (sub1 n))))))

(define odd?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (even? (sub1 n))))))

(define *application
  (lambda (e table)
    ((meaning (function-of e) table) (evlis (arguments-of e) table))))

(define evlis
  (lambda (es table)
    (cond
      ((null? es) null)
      (else ((lambda (val)
               (cons val (evlis (cdr es) table)))
             (meaning (car es) table))))))

(define :car
  (lambda (arg-list)
    (car (car arg-list))))

(define a-prim
  (lambda (p)
    (lambda (arg-list)
      (p (car arg-list)))))

(define b-prim
  (lambda (p)
    (lambda (arg-list)
      (p (car arg-list) (car (cdr arg-list))))))

(define *const
  ((lambda (:cons :car :cdr :eq? :atom? :null? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond
         ((number? e) e)
         ((eq? e #t) #t)
         ((eq? e #f) #f)
         ((eq? e 'cons) :cons)
         ((eq? e 'car) :car)
         ((eq? e 'cdr) :cdr)
         ((eq? e 'eq?) :eq?)
         ((eq? e 'atom?) :atom?)
         ((eq? e 'null?) :null?)
         ((eq? e 'zero?) :zero?)
         ((eq? e 'add1) :add1)
         ((eq? e 'sub1) :sub1)
         ((eq? e 'number?) :number?))))
   (b-prim cons) (a-prim car) (a-prim cdr) (b-prim eq?) (a-prim atom?) (a-prim null?)
   (a-prim zero?) (a-prim add1) (a-prim sub1) (a-prim number?)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccbody-of e)
              (extend (name-of e) (box (a-prim skip)) table)))))

(define abort #f)

(define value
  (lambda (e)
    (let/cc the-end
      (set! abort the-end)
      (cond
        ((define? e) (*define e))
        (else (the-meaning e))))))

(define the-empty-table
  (lambda (name)
    (abort (cons 'no-answer
                 (cons name null)))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e)) (cond
                         ((eq? (car e) 'quote) *quote)
                         ((eq? (car e) 'lambda) *lambda)
                         ((eq? (car e) 'letcc) *letcc)
                         ((eq? (car e) 'set!) *set)
                         ((eq? (car e) 'cond) *cond)
                         (else *application)))
      (else *application))))

(define text-of
  (lambda (x)
    (car (cdr x))))

(define formals-of
  (lambda (x)
    (car (cdr x))))

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond
      ((null? (cdr (cdr x))) 0)
      (else (car (cdr (cdr x)))))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (car (cdr x))))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))

