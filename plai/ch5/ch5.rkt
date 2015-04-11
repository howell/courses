#lang plai-typed

#|
Exercise
Add conditionals to your language. You can either add boolean datatypes or, if you want to do something quicker, add a conditional that treats 0 as false and everything else
as true.

What are the important test cases you should write?
|#

(define-type BoolC
  [trueC]
  [falseC])

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [ifteC (c : BoolC) (t : ArithC) (e : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifteC (c t e)
           (type-case BoolC c
             [trueC () (interp t)]
             [falseC () (interp e)])]))
             
(test (interp (ifteC (trueC) (numC 1) (numC 2))) 1)

(test (interp (ifteC (falseC) (numC 1) (numC 2))) 2)

#|
Exercise
When a function has multiple arguments, what simple but important criterion governs the names of those arguments?
Answer
The parameter names are distinct.
|#

