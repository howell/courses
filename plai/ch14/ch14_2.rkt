#lang racket

(define-syntax (cps e)
  (syntax-case e (with rec lam cnd seq set quote display read-number generator let/cc)
    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                  (seq (set v f)
                       b)))]
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
              ((cps b) dyn-k))))]
    [(_ (cnd tst thn els))
     #'(lambda (k)
         ((cps tst) (lambda (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    [(_ (display output))
     #'(lambda (k)
         ((cps output) (lambda (ov)
                         (k (display ov)))))]
    [(_ (read-number prompt))
     #'(lambda (k)
         ((cps prompt) (lambda (pv)
                         (read-number/suspend pv k))))]
    [(_ (seq e1 e2))
     #'(lambda (k)
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]
    [(_ (set v e))
     #'(lambda (k)
         ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
    [(_ (let/cc kont b))
     (identifier? #'kont)
     #'(lambda (k)
         (let ([kont (lambda (v dyn-k)
                       (k v))])
           ((cps b) k)))]
    [(_ (generator (yield) (v) b))
     (and (identifier? #'v) (identifier? #'yield))
     #'(lambda (k)
         (k (let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
              (letrec ([resumer (lambda (v)
                                  ((cps b) (lambda (k) (error 'generator "fell through"))))]
                       [yield (lambda (v gen-k)
                                (begin
                                  (set! resumer gen-k)
                                  (where-to-go v)))])
                (lambda (v dyn-k)
                  (begin
                    (set! where-to-go dyn-k)
                    (resumer v)))))))]
    [(_ (coroutine (yield) (v) b))
     (and (identifier? #'yield) (identifier? #'v))
     #'(lambda (k)
         (k (let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
              (letrec ([resumer (lambda (v)
                                  ((cps b) (lambda (k) (error 'coroutine "fell through"))))]
                       [yield (lambda (cr co-k)
                                (lambda (v co-k2)
                                  (begin
                                    (set! resumer co-k)
                                    #;(display v)
                                    (cr v co-k2))))])
                (lambda (v dyn-k)
                  (begin
                    (set! where-to-go dyn-k)
                    (resumer v)))))))]
    [(_ 'e)
     #'(lambda (k)
         (k 'e))]
    [(_ (f a))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    [(_ (f a b))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               ((cps b) (lambda (bv)
                                          (k (fv av bv)))))))))]
    [(_ (f a b c))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               ((cps b) (lambda (bv)
                                          ((cps c) (lambda (cv)
                                                     (k (fv av bv cv)))))))))))]
    [(_ atomic)
     #'(lambda (k)
         (k atomic))]))


(define (run c) (c identity))

(define bar
  (cps (lam (z) 5)))

(define foo
  (cps ((lam (z) 5) '_)))


(define gen-ex
  (cps (with (g (generator (yield) (from)
                           (rec (f (lam (n)
                                        (seq
                                         (yield n)
                                         (f (+ n 1)))))
                             (f from))))
             (seq
              (display (g 12))
              (seq (display (g 0))
                   (seq (display (g 0))
                        (display (g 0))))))))

#|
Exercise
How do generators differ from coroutines and threads? Implement coroutines and threads using a similar strategy.
|#

#;(define co-ex
    (cps (couroutine (yield) (b)
                     (seq (display 0)
                          (yield b)))))

(define co-ex
  (cps (rec (a (coroutine (yield) (b)
                          (seq
                           (display 0)
                           (seq ((yield b) 0)
                                (seq (display 0)
                                     ((yield b) 0))))))
         (with (b (coroutine (yield) (n)
                             (seq
                              (display n)
                              (seq ((yield a) 0)
                                   (seq (display (+ n 1))
                                        (seq (yield a 0)
                                             (seq (display (+ n 2))
                                                  (yield a 0))))))))
               (a b)))))

(run co-ex)