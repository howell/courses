#lang plai

(define (guard ctc val pos neg) ((ctc pos neg) val))

(define (blame s) (error 'contract s))

(define (immediate pred?)
  (lambda (pos neg)
    (lambda (val)
      (if (pred? val) val (blame pos)))))

(define (function dom rng)
  (lambda (pos neg)
    (let ([dom-c (dom neg pos)]
          [rng-c (rng pos neg)])
      (lambda (val)
        (if (procedure? val)
            (lambda (x) (rng-c (val (dom-c x))))
            (blame pos))))))


(define a1 (guard (function (immediate number?)
                            (immediate number?))
                  add1
                  "add1 body"
                  "add1 input"))

(define d/dx
  (guard (function (function (immediate number?) (immediate number?))
                   (function (immediate number?) (immediate number?)))
         (lambda (f)
           (lambda (x)
             (/ (- (f (+ x 0.001))
                   (f x))
                0.001)))
         "d/dx body"
         "d/dx input"))