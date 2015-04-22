#lang racket

(define-for-syntax (intersperse e l)
  (letrec ([loop (lambda (l)
                   (cond
                     [(null? l) null]
                     [(null? (cdr l)) l]
                     [else (cons (car l) (cons e (loop (cdr l))))]))])
    (loop l)))

(define (intersperse e l)
  (letrec [(loop (lambda (l)
                   (if (null? l)
                       null
                       (cons (car l) (cons e (loop (cdr l)))))))]
    (loop l)))

(define (intersperse-syntax2 e)
  (syntax-case e ()
    [(_ (stx) l ...)
     (datum->syntax e (cons 'list (intersperse #'stx (syntax->list #'(l ...)))))]))

(intersperse-syntax2 1 2 3)