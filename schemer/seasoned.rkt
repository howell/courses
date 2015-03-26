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

(define depth*
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