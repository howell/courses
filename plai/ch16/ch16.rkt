#lang plai

(define (make-contract pred?)
  (lambda (val)
    (if (pred? val) val (blame "violation"))))

(define (blame s) (error 'contract "~a" s))

(define non-neg?-contract
  (make-contract
   (lambda (n) (and (number? n)
                    (>= n 0)))))

(define (real-sqrt-1 x)
  (sqrt (non-neg?-contract x)))

(define (real-sqrt-2 x)
  (begin
    (non-neg?-contract x)
    (sqrt x)))

(define d/dx
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x 0.001))
            (f x))
         0.001))))

(define (even-list? l)
  (cond
    [(empty? l) #t]
    [else (and (even? (first l))
               (even-list? (rest l)))]))

(define even-list?-contract
  (make-contract
   (lambda (l) (and (list? l)
                    (andmap number? l)
                    (even-list? l)))))

(define (immediate pred?)
  (lambda (val)
    (if (pred? val) val (blame val))))

(define (guard ctc val) (ctc val))

(define (function dom rng)
  (lambda (val)
    (if (procedure? val)
        (lambda (x) (rng (val (dom x))))
        (blame val))))

(define-syntax (define/contract stx)
  (syntax-case stx (::)
    [(_ (f (id :: c) ...) b)
     (with-syntax ([(new-id ...) (generate-temporaries #'(id ...))])
       #'(define f
           (lambda (new-id ...)
             (let ([id (guard c new-id)]
                   ...)
               b))))]))

(define num?-con (immediate number?))

(define a1 (guard (function (immediate number?)
                            (immediate number?))
                  add1))
