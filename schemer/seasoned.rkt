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
      