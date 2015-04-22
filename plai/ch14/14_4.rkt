#lang racket

(define-syntax let/cc
  (syntax-rules ()
    [(let/cc k b)
     (call/cc (lambda (k) b))]))

(define-syntax (generator e)
  (syntax-case e ()
    [(generator (yield) (v) b)
     #'(let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
         (letrec ([resumer (lambda (v)
                             (begin b
                                    (error 'generator "fell through")))]
                  [yield (lambda (v)
                           (let/cc gen-k
                             (begin
                               (set! resumer gen-k)
                               (where-to-go v))))])
           (lambda (v)
             (let/cc dyn-k
               (begin
                 (set! where-to-go dyn-k)
                 (resumer v))))))]))

#|
Exercise
What happens if we move the let/ccs and mutation to be the first statement inside the begins instead?
Answer
We will go in to an infinite loop, since after calling the continuation we will evaluate the other part of the begin, which jumps back to the other continuation.
|#

(define (gen-ex)
  (let ([g (generator (yield) (from)
                      (letrec ([f (lambda (n)
                                    (yield n)
                                    (f (+ n 1)))])
                        (f from)))])
    (begin
      (display (g 12))
      (newline)
      (display (g 0))
      (newline)
      (display (g 0))
      (newline)
      (display (g 0)))))

(define (scheduler-loop-0 threads)
  (cond
    [(empty? threads) 'done]
    [(cons? threads)
     (begin
       (let/cc after-thread ((first threads) after-thread))
       (scheduler-loop-0 (append (rest threads)
                                 (list (first threads)))))]))


(define-syntax thread-0
  (syntax-rules ()
    [(thread (yielder) b ...)
     (letrec ([thread-resumer (lambda (_)
                                (begin b ...))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! yielder
                 (lambda ()
                   (let/cc thread-k
                     (begin
                       (set! thread-resumer thread-k)
                       (sched-k 'dummy)))))
           (thread-resumer 'tres))))]))

(define d display) ;; a useful shorthand in what follows

(define (sched-0-ex)
  (scheduler-loop-0
   (list
    (thread-0 (y) (d "t1-1  ") (y) (d "t1-2  ") (y) (d "t1-3 "))
    (thread-0 (y) (d "t2-1  ") (y) (d "t2-2  ") (y) (d "t2-3 "))
    (thread-0 (y) (d "t3-1  ") (y) (d "t3-2  ") (y) (d "t3-3 ")))))

(define (scheduler-loop-1 threads)
  (cond
    [(empty? threads) 'done]
    [(cons? threads)
     (if (let/cc after-thread ((first threads) after-thread))
         (scheduler-loop-1 (append (rest threads)
                                   (list (first threads))))
         (scheduler-loop-1 (rest threads)))]))

(define-syntax thread-1
  (syntax-rules ()
    [(thread (yielder) b ...)
     (letrec ([thread-resumer (lambda (_)
                                (begin b ...
                                       (finisher)))]
              [finisher (lambda () (error 'finisher "nothing here"))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! finisher
                 (lambda ()
                   (let/cc thread-k
                     (sched-k #f))))
           (set! yielder
                 (lambda ()
                   (let/cc thread-k
                     (begin
                       (set! thread-resumer thread-k)
                       (sched-k #t)))))
           (thread-resumer 'tres))))]))

(define (sched-1-ex)
  (scheduler-loop-1
   (list
    (thread-1 (y) (d "t1-1  ") (y) (d "t1-2  ") (y) (d "t1-3 "))
    (thread-1 (y) (d "t2-1  ") (y) (d "t2-2  ") (y) (d "t2-3 "))
    (thread-1 (y) (d "t3-1  ") (y) (d "t3-2  ") (y) (d "t3-3 ")))))

#|
Exercise
After we are done building cooperative multitasking, implement preemptive multitasking. What changes?
|#

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

; (intersperse e1 e2 e3 ...) -> (e1 'x e2 'x e3 'x ...)
(define-for-syntax (intersperse-syntax e)
  (syntax-case e ()
    [(_ (stx) l ...)
     (datum->syntax e (intersperse #'stx (syntax->list #'(l ...))))]))

(define (intersperse-syntax2 e)
  (syntax-case e ()
    [(_ (stx) l ...)
     (datum->syntax e (cons 'list (intersperse #'stx (syntax->list #'(l ...)))))]))

(define-for-syntax (append-syntax e)
  (syntax-case e ()
    [(_ (stx) l ...)
     (datum->syntax e (syntax->list #'(l ... stx)))]))

(define-syntax (preemptive-thread e)
  (syntax-case e ()
    [(_ (finisher) (yielder) b ...)
     (datum->syntax e
                    (cons 'begin
                          (syntax->list (append-syntax (#'(finisher))
                                                       (intersperse-syntax (#'(yielder)) #'(b ...))))))]))

(define-syntax thread-p
  (syntax-rules ()
    [(thread-p b ...)
     (letrec ([thread-resumer (lambda (_)
                                (preemptive-thread (finisher) (yielder) #'(b ...)))]
              [finisher (lambda () (error 'finisher "nothing here"))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! finisher
                 (lambda ()
                   (let/cc thread-k
                     (sched-k #f))))
           (set! yielder
                 (lambda ()
                   (let/cc thread-k
                     (begin
                       (set! thread-resumer thread-k)
                       (sched-k #t)))))
           (thread-resumer 'tres))))]))

#;(define foo
    (let ([yielder (lambda () empty)]
          [finisher (lambda () empty)])
      (begin (preemptive-thread finisher yielder (+ 1 2) (+ 3 4)))))

#;(define bar
    (let ([yielder (lambda () empty)] [finisher (lambda () empty)])
      (begin ((+ 1 2) (yielder) ((+ 3 4) (finisher))))))

#;(define stx #'(1 'x (2 'x:19 (3 'x 'x))))

(intersperse-syntax2 (0) 1 2 3)

#;(let ([f (lambda () 0)])
  (intersperse-syntax2 '((f)) 1 2 3))

#;(append-syntax (0) 1 2 3 4)

#;(intersperse 'x (list 1 2 3 4 5))

#;(define (preemptive-ex)
  (scheduler-loop-1
   (list
    (thread-p (d "t1-1  ") (d "t1-2  ") (d "t1-3 "))
    (thread-p (d "t2-1  ") (d "t2-2  ") (d "t2-3 "))
    (thread-p (d "t3-1  ") (d "t3-2  ") (d "t3-3 ")))))