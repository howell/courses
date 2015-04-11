#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (numC 42)) 42)

(test (interp (plusC (numC 1) (plusC (numC 2) (numC 3))))
      (interp (plusC (plusC (numC 1) (numC 2)) (numC 3))))

(test (interp (multC (numC 13) (plusC (numC 4) (numC 3)))) 91)

(test (interp (plusC (multC (numC 2) (numC 9)) (numC 4))) 22)

#|
Exercise: What all would you have to change so that the number has signed-32-bit arithmetic?
Answer: We'd have to implement signed-32-bit arithmetic in the host language and then implement
interp in terms of that implementation- use signed 32 bit numbers for numC's and the corresponding
operations for plusC and multC.
|#