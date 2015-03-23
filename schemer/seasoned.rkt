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
                                 