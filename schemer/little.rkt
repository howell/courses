#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define rember
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((equal? a (car l)) (cdr l))
      (else (cons (car l) (rember a (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((equal? old (car lat)) (cons old (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((equal? old (car lat)) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((equal? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((equal? o1 (car lat)) (cons new (cdr lat)))
              ((equal? o2 (car lat)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      (else (cond
              ((equal? a (car lat)) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else (cond
              ((equal? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
              (else (cons old (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else (cond
              ((equal? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons old (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      (else (cond
              ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

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

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (t1 t2)
    (cond
      ((and (null? t1) (null? t2)) (quote ()))
      (else (cons (o+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

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

(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (o^ n (sub1 m)))))))

(define o%
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o% (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      ((null? lat) (quote ()))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((equal? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      ((null? lat) (quote ()))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) (0))
      ((atom? (car l)) (cond
                         ((equal? a (car l)) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((equal? old (car l)) (cons new (subst* new old (cdr l))))
                         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((equal? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((equal? a (car l)) #t)
                         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

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

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))

(define rember2
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr (aexp))))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) (quote +)) (o+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x)) (o* (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^)) (o^ (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

(define value-prefix
  (lambda (pexp)
    (cond
      ((atom? pexp) pexp)
      ((eq? (car pexp) (quote +)) (o+ (value-prefix (car (cdr pexp))) (value-prefix (car (cdr (cdr pexp))))))
      ((eq? (car pexp) (quote x)) (o* (value-prefix (car (cdr pexp))) (value-prefix (car (cdr (cdr pexp))))))
      ((eq? (car pexp) (quote ^)) (o^ (value-prefix (car (cdr pexp))) (value-prefix (car (cdr (cdr pexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +)) (o+ (value2 (1st-sub-exp nexp))
                                           (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x)) (o* (value2 (1st-sub-exp nexp))
                                           (value2 (2nd-sub-exp nexp))))
      (else (o^ (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp)))))))

(define 1st-sub-exp2
  (lambda (aexp)
    (car aexp)))

(define operator2
  (lambda (aexp)
    (car (cdr aexp))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define o+2
  (lambda (n m)
    (cond
      ((sero? n) m)
      (else (edd1 (o+2 (sub1 n) m))))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (equal? a (car l)) (member? a (cdr l)))))))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
              (else (cons (car lat) (makeset (cdr lat)))))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else (and (member? (car s1) s2)
                 (subset? (cdr s1) s2))))))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      (else (or (member? (car s1) s2)
                (intersect? (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) (quote ()))
      ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((cons (car s1) (union (cdr s1) (multirember (car s1) s2)))))))

(define set-difference
  (lambda (s1 s2)
    (cond
      ((null? s1) (quote ()))
      ((member? (car s1) s2) (set-difference (cdr s1) s2))
      (else (cons (car s1) (set-difference (cdr s1) s2))))))

(define intersectall
  (lambda (sets)
    (cond
      ((null? (cdr sets)) (car sets))
      (else (intersect (car sets) (intersectall (cdr sets)))))))

(define a-pair?
  (lambda (s)
    (cond
      ((atom? s) #f)
      ((null? s) #f)
      ((null? (cdr s)) #f)
      ((null? (cdr (cdr s))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define third
  (lambda (t)
    (car (cdr (cdr t)))))

(define firsts
  (lambda (ps)
    (cond
      ((null? ps) (quote ()))
      (else (cons (first (car ps)) (firsts (cdr ps)))))))

(define fun?
  (lambda (ps)
    (set? (firsts ps))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revel2
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define fullfun?
  (lambda (rel)
    (fun? (revrel rel))))

(define rember-f
  (lambda (match? a l)
    (cond
      ((null? l) (quote ()))
      ((match? a (car l)) (cdr l))
      (else (cons (car l) (rember-f match? a (cdr l)))))))

(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
  (eq-c? 'salad))

(define rember-fc
  (lambda (match?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((match? a (car l)) (cdr l))
        (else (cons (car l) ((rember-fc match?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (old new lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((test? old (car lat)) (cons new lat))
                (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((test? old (car lat)) (cons old (cons new (cdr lat))))
                (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((eq? old (car lat)) (seq new old (cdr lat)))
        (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insertL-g
  (insert-g seqL))

(define insertR-g
  (insert-g seqR))

(define insertL-g2
  (insert-g (lambda (new old l) (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-g
  (insert-g seqS))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a (quote +)) o+)
      ((eq? a (quote x)) o*)
      (else o^))))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) ((multirember-f test?) a (cdr l)))
        (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define multirember-eq
  (multirember-f eq?))

(define multiremberT
  (lambda (test? l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l)) ((multiremberT test? (cdr l))))
      (else (cons (car l) ((multiremberT test? (cdr l))))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote ())))
      ((eq? a (car lat)) (multirember&co a (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat) seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat ls rs)
                                                (col (cons new (cons oldL newlat)) (add1 ls) rs))))
      ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat ls rs)
                                                (col (cons oldR (cons new newlat)) ls (add1 rs)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat ls rs)
                                (col (cons (car lat) newlat) ls rs)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l)) (cond
                         ((even? (car l)) (evens-only*&co (cdr l)
                                                          (lambda (newl prod sum)
                                                            (col (cons (car l) newl) (o* (car l) prod) sum))))
                         (else (evens-only*&co (cdr l)
                                               (lambda (newl prod sum)
                                                 (col newl prod (o+ (car l) sum)))))))
      (else (evens-only*&co (car l)
                            (lambda (newl1 prod1 sum1)
                              (evens-only*&co (cdr l)
                                              (lambda (newl2 prod2 sum2)
                                                (col (cons newl1 newl2) (o* prod1 prod2) (o+ sum1 sum2))))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a x lat)
    (cond
      ((number? x) (keep-looking a (pick x lat) lat))
      (else (equal? a x)))))

(define shift
  (lambda (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora)) (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (o* 2 (weight* (first pora)))
                (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(define eternity
  (lambda (x)
    (eternity x)))

(define length<=1
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity)))

(define mk-length
  (lambda (almost-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (almost-length (cdr l))))))))

(define length<=3
  ((lambda (mk-length)
    (mk-length (mk-length (mk-length (mk-length eternity)))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))))

(define z
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (lambda (x) ((mk-length mk-length) x))))))

(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      (else (cond
              ((equal? name (car names)) (car values))
              (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                                  (lambda (n)
                                    (lookup-in-table name (cdr table) table-f)))))))

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
                         ((eq? (car e) 'cond) *cond)
                         (else *application)))
      (else *application))))

(define value4
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table (lambda (n) (car '())))))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (e)
    (cond
      ((atom? e) (eq? e 'else))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cdr e) table)))

(define evlis
  (lambda (l table)
    (cond
      ((null? l) l)
      (else (cons (meaning (car l) table) (evlis (cdr l) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (e)
    (eq? (car e) 'primitive)))

(define non-primitive?
  (lambda (e)
    (eq? (car e) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))