#lang plai

(define-syntax my-let-1
  (syntax-rules ()
    [(my-let-1 (var val) body)
     ((lambda (var) body) val)]))

(define-syntax my-let-2
  (syntax-rules ()
    [(my-let-2 ([var val] ...) body)
     ((lambda (var ...) body) val ...)]))

#|
Exercise
Write one or more macros to confirm that the output of a macro is expanded further.
Answer
(huh 5) diverges!
|#

(define-syntax wat
  (syntax-rules ()
    [(wat x)
     (huh x)]))

(define-syntax huh
  (syntax-rules ()
    [(huh x)
     (wat x)]))


(define-syntax (my-let-3 x)
  (syntax-case x ()
    [(my-let-3 (var val) body)
     #'((lambda (var) body) val)]))

#|
Exercise
syntax-rules can actually be expressed as a macro over syntax-case. Define it.
|#

#;(define-syntax (my-syntax-rules x)
    (syntax-case x ()
      [(my-syntax-rules () l)
       #'(syntax-case l ()
           [([p1 p2] ...)
            #'[p1 #'p2] ...])]))

#;(define-syntax my-let-4
    (my-syntax-rules ()
                     [(my-let-1 (var val) body)
                      ((lambda (var) body) val)]))

(define-syntax (my-let-guard x)
  (syntax-case x ()
    [(my-let-guard (var val) body)
     (identifier? #'var)
     #'((lambda (var) body) val)]))

(define-syntax (my-or-1 x)
  (syntax-case x ()
    [(my-or-1 e0 e1 ...)
     #'(if e0
           e0
           (my-or-1 e1 ...))]))

#|
Exercise
Why is #f the right default? (for zero or arguments)
Answer
Because, definitonally, x OR F = x. We can never change the result of an OR calculation by OR'ing with false.
|#

(define-syntax (my-or-2 x)
  (syntax-case x ()
    [(my-or-2)
     #'#f]
    [(my-or-2 e0 e1 ...)
     #'(if e0
           e0
           (my-or-2 e1 ...))]))

(define-syntax (my-or-3 x)
  (syntax-case x ()
    [(my-or-3)
     #'#f]
    [(my-or-3 e)
     #'e]
    [(my-or-3 e0 e1 ...)
     #'(if e0
           e0
           (my-or-3 e1 ...))]))

(define-syntax (my-or-4 x)
  (syntax-case x ()
    [(my-or-4)
     #'#f]
    [(my-or-4 e)
     #'e]
    [(my-or-4 e0 e1 ...)
     #'(let ([v e0])
         (if v
             v
             (my-or-4 e1 ...)))]))

(let ([v #t]) (my-or-4 #f v))

(define-syntax object/self-1
  (syntax-rules ()
    [(object [mtd-name (var) val] ...)
     (let ([self (lambda (msg-name)
                   (lambda (v) (error 'object "nothing here")))])
       (begin
         (set! self
               (lambda (msg)
                 (case msg
                   [(mtd-name) (lambda (var) val)]
                 ...)))
         self))]))

(define (msg s n a)
  ((s n) a))

#;(define os-1
  (object/self-1
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))

(define-syntax object/self-2
  (syntax-rules ()
    [(object self [mtd-name (var) val] ...)
     (let ([self (lambda (msg-name)
                   (lambda (v) (error 'object "nothing here")))])
       (begin
         (set! self
               (lambda (msg)
                 (case msg
                   [(mtd-name) (lambda (var) val)]
                 ...)))
         self))]))

(define os-2
  (object/self-2 self
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))

(define-syntax (object/self-3 x)
  (syntax-case x ()
    [(object [mtd-name (var) val] ...)
     (with-syntax ([self (datum->syntax x 'self)])
       #'(let ([self (lambda (msg-name)
                       (lambda (v) (error 'object "nothing here")))])
           (begin
             (set! self
                   (lambda (msg-name)
                     (case msg-name
                       [(mtd-name) (lambda (var) val)]
                       ...)))
             self)))]))

(define os-3
  (object/self-3
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))