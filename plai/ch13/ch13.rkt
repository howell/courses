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

(define-syntax (my-syntax-rules x)
  (syntax-case x ()
    [(my-syntax-rules () l)
     #'(syntax-case l ()
         [([p1 p2] ...)
          #'[p1 #'p2] ...])]))

(define-syntax my-let-4
  (my-syntax-rules ()
    [(my-let-1 (var val) body)
     ((lambda (var) body) val)]))