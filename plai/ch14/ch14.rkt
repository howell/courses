#lang plai-typed

(define (read-number [prompt : string]) : number
  (begin
    (display prompt)
    (let ([v (read)])
      (if (s-exp-number? v)
          (s-exp->number v)
          (read-number prompt)))))

(define (add-server)
  (display
   (+ (read-number "First number")
      (read-number "Second number"))))

(define-type-alias label number)

#|
Exercise
Define new-label. You might use new-loc for inspiration.
|#

(define new-label
  (let ([next-label (box 0)])
    (lambda ()
      (let ([label (unbox next-label)])
        (begin (set-box! next-label (+ 1 label))
               label)))))

(define table (make-hash empty))

(define (read-number/suspend [prompt : string] rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g))))

(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

(define (suspend-server)
  (read-number/suspend "First number"
                       (lambda (v1)
                         (read-number/suspend "Second number"
                                              (lambda (v2)
                                                (display
                                                 (+ v1 v2)))))))

(define cookie '-100)

(define (state-server)
  (read-number/suspend "First number"
                       (lambda (v1)
                         (begin
                           (set! cookie v1)
                           (read-number/suspend "Second number"
                                                (lambda (v2)
                                                  (display
                                                   (+ cookie v2))))))))
#|
Exercise
What do we expect for the same sequence as before?
Answer
We use the value of cookie instead of the closed over value, so the result will be different.
|#

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
    [(_ atomic)
     #'(lambda (k)
         (k atomic))]))

(define (run c) (c identity))

(define bar
  (cps (lam (z) 5)))

(define foo
  (cps ((lam (z) 5) '_)))


(test (run (cps 3))
      3)
(test (run (cps ((lam (z) 5) '_)))
      5)
(test (run (cps ((lam (x) (* x x)) 5)))
      25)
(test (run (cps (+ 5 ((lam (x) (* x x)) 5))))
      30)

(define (cps-server)
  (run (cps (display (+ (read-number "First")
                        (read-number "Second"))))))

(define cps-server-expanded
  (lambda (k)
    (read-number/suspend "First" (lambda (lv)
                                   (read-number/suspend "Second" (lambda (rv)
                                                                   (k (display (+ lv rv)))))))))

(test (run (cps (let/cc esc 3)))
      3)

(test (run (cps (let/cc esc (esc 3))))
      3)

(test (run (cps (+ 1 (let/cc esc (esc 3)))))
      4)

(test (run (cps (let/cc esc (+ 2 (esc 3)))))
      3)

(test (run (cps (+ 1 (let/cc esc (+ 2 (esc 3))))))
      4)

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
Using let/cc and macros, create a throw/catch mechanism.
|#
