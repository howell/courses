
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s)
        (string-append s suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (= n 0) null (let* ([pr (s)]
                          [x (car pr)]
                          [sn (cdr pr)])
                     (cons x (stream-for-n-steps sn (- n 1))))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (n)
                (cons (if (= 0 (remainder n 5)) (- 0 n) n) (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (p) (cons (if p "dan.jpg" "dog.jpg") (lambda () (f (not p)))))])
    (lambda () (f #t))))

(define (stream-map f s)
  (lambda () (let* ([pr (s)]
                    [x (car pr)]
                    [sn (cdr pr)])
               (cons (f x) (stream-map f sn)))))

(define (stream-add-zero s)
  (stream-map (lambda (x) (cons 0 x)) s))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons 
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (define limit (vector-length vec))
  (define (search i)
    (if (= i limit)
        #f
        (let ([x (vector-ref vec i)])
          (if (pair? x) (if (equal? (car x) v) x (search (+ i 1))) (search (+ i 1))))))
  (search 0))

(define (cached-assoc xs n)
  (letrec ([insert-position 0]
           [table (make-vector n #f)]
           [f (lambda (v)
                (let ([ans (vector-assoc v table)])
                  (if ans
                      (cdr ans)
                      (let ([ans (assoc v xs)])
                        (begin
                          (vector-set! table insert-position (cons v ans))
                          (set! insert-position (remainder (+ insert-position 1) n))
                          ans)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less lim do body)
     (letrec ([x lim]
              [loop (lambda () (if (< body lim)
                                   (loop)
                                   #t))])
       (loop))]))
