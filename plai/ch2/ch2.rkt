#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; > (parse '(+ (* 1 2) (+ 2 3)))
; - ArithC
; (plusC
;  (multC (numC 1) (numC 2))
;  (plusC (numC 2) (numC 3)))
; What happens if you forget to quote the argument to the parser? Why?
; A: We get a type error; the host language determines that the type of (+ (* 1 2) (+ 2 3))
; is number, which is incompatible with the type expected by parse, s-expression.