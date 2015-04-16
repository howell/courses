#lang plai

#|
Alert: All the code that follows will be in #lang plai, not in the typed language.

Exercise
Why #lang plai? What problems do you encounter when you try to type the following code? Are some of them amenable to easy
fixes, such as introducing a new datatype and applying it consistently? How about if we make simplifications for the purposes
of modeling, such as assuming methods have only one argument? Or are some of them less tractable?
Answer
???
|#

(define o-1
  (lambda (m)
    (case m
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))

(test ((o-1 'add1) 5) 6)

(define (msg o m . a)
  (apply (o m) a))

(test (msg o-1 'add1 5) 6)

(define (o-constr-1 x)
  (lambda (m)
    (case m
      [(addX) (lambda (y) (+ x y))])))

(test (msg (o-constr-1 5) 'addX 3) 8)
(test (msg (o-constr-1 2) 'addX 3) 5)

(define (o-state-1 count)
  (lambda (m)
    (case m
      [(inc) (lambda () (set! count (+ count 1)))]
      [(dec) (lambda () (set! count (- count 1)))]
      [(get) (lambda () count)])))

(test (let ([o (o-state-1 5)])
        (begin (msg o 'inc)
               (msg o 'dec)
               (msg o 'get)))
      5)

(test (let ([o1 (o-state-1 3)]
            [o2 (o-state-1 3)])
        (begin (msg o1 'inc)
               (msg o1 'inc)
               (+ (msg o1 'get)
                  (msg o2 'get))))
      (+ 5 3))

(define (o-state-2 init)
  (let ([count init])
    (lambda (m)
      (case m
        [(inc) (lambda () (set! count (+ count 1)))]
        [(dec) (lambda () (set! count (- count 1)))]
        [(get) (lambda () count)]))))

(define o-static-1
  (let ([counter 0])
    (lambda (amount)
      (begin
        (set! counter (+ 1 counter))
        (lambda (m)
          (case m
            [(inc) (lambda (n) (set! amount (+ amount n)))]
            [(dec) (lambda (n) (set! amount (- amount n)))]
            [(get) (lambda () amount)]
            [(count) (lambda () counter)]))))))

(test (let ([o (o-static-1 1000)])
        (msg o 'count))
      1)

(test (let ([o (o-static-1 0)])
        (msg o 'count))
      2)

(define o-self!
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(first) (lambda (x) (msg self 'second (+ x 1)))]
                [(second) (lambda (x) (+ x 1))])))
      self)))

(test (msg o-self! 'first 5) 7)

(define (msg/self o m . a)
  (apply (o m) o a))

(define o-self-no!
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg/self self 'second (+ x 1)))]
      [(second) (lambda (self x) (+ x 1))])))

(define (mt)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(add) (lambda () 0)])))
      self)))

(define (node v l r)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(add) (lambda () (+ v
                                     (msg l 'add)
                                     (msg r 'add)))])))
      self)))

(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

(test (msg a-tree 'add) (+ 10 5 15 6))

#|
Exercise
Observe that the application (parent-object m) is like “half a msg”, just like an l-value was “half a value lookup” [REF].
Is there any connection?
Answer
In both cases we are looking for what a certain name/symbol means in a certain context. In the variable case, the context
is the environment; in the object case, it is the inheritance hierarchy.
We are also looking for *where* something is defined, but don't care about what to do with it.
|#

#|
Exercise
Rewrite this block of code using self-application instead of mutation.
|#

(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)])
    (lambda (m)
      (case m
        [(size) (lambda (self) (+ 1
                                  (msg/self l 'size)
                                  (msg/self r 'size)))]
        [else (parent-object m)]))))

(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)])
    (lambda (m)
      (case m
        [(size) (lambda (self) 0)]
        [else (parent-object m)]))))

(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))

(test (msg a-tree/size 'add) (+ 10 5 15 6))
(test (msg/self a-tree/size 'size) 4)

#|
Exercise
The code above is fundamentally broken. The self reference is to the same syntactic object, whereas it needs to refer to the most-refined object: this is known as open
recursion.
Modify the object representations so that self always refers to the most refined version of the object. Hint: You will find the self-application method
(Self-Reference Without Mutation) of recursion handy.
|#

(define (Y f)
  ((lambda (g) (f (lambda (x) ((g g) x)))) (lambda (g) (f (lambda (x) ((g g) x))))))

#|
Exercise
Modify the inheritance pattern above to implement a Self-like, prototype-based language, instead of a class-based language. Because classes provide each object with
distinct copies of their parent objects, a prototype-language might provide a clone operation to simplify creation of the operation that simulates classes atop prototypes.
|#

(define (node/size/proto parent v l r)
  (lambda (m)
    (case m
      [(size) (lambda (self) (+ 1
                                (msg/self l 'size)
                                (msg/self r 'size)))]
      [else (parent m)])))

(define (mt/size/proto parent)
  (lambda (m)
    (case m
      [(size) (lambda (self) 0)]
      [else (parent m)])))

(define (mk-node/size/proto v l r)
  (node/size/proto (node v l r)
                   v l r))

(define a-tree/size/proto
  (mk-node/size/proto
   10
   (mk-node/size/proto 5 (mt/size/proto (mt)) (mt/size/proto (mt)))
   (mk-node/size/proto 15
                       (mk-node/size/proto 6 (mt/size/proto (mt)) (mt/size/proto (mt)))
                       (mt/size/proto (mt)))))

(test (msg a-tree/size/proto 'add) (+ 10 5 15 6))
(test (msg/self a-tree/size/proto 'size) 4)