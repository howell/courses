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

(define gen-ex
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
