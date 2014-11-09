#lang racket
;; Programming Languages Homework5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw5.rkt")

(require rackunit)

(define fib
  (fun "fib" "n"
       (ifgreater (int 2) (var "n")
                  (int 1)
                  (add (call (var "fib") (add (var "n") (int -1))) (call (var "fib") (add (var "n") (int -2)))))))

(define add-ten
  (mlet "ten" (int 10)
        (fun "add-ten" "x"
             (add (var "x") (var "ten")))))

(define ten
  (mlet "x" (int 7)
        (mlet "y" (int 3)
              (fun "ten" "unused" (add (var "x") (var "y"))))))

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 3) (int 3) (int 2))) (int 2))
   (check-equal? (eval-exp (ifgreater (int 3) (int 2) (int 3) (int 2))) (int 3))
   (check-exn exn:fail? (lambda () (eval-exp (ifgreater (apair (int 1) (int 2)) (int 0) (int 1) (int 2)))))
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-exn exn:fail? (lambda () (eval-exp (mlet "x" (int 1) (add (int 5) (var "y"))))))
   (check-equal? (eval-exp (mlet "x" (int 1) (mlet "x" (int 2) (add (int 5) (var "x"))))) (int 7) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (mlet "y" (int 2) (add (var "y") (var "x"))))) (int 3) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call fib (int 0))) (int 1))
   (check-equal? (eval-exp (call fib (int 1))) (int 1))
   (check-equal? (eval-exp (call fib (int 2))) (int 2))
   (check-equal? (eval-exp (call fib (int 3))) (int 3))
   (check-equal? (eval-exp (call fib (int 4))) (int 5))
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-exn exn:fail? (lambda () (eval-exp (snd (int 0)))))
   
   ;; fst test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   (check-exn exn:fail? (lambda () (eval-exp (fst fib))))
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (add (var "x") (int 1)))) (add (var "x") (var "y")))) (int 21) "mlet* test")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 2) (int 3) (int 4))) (int 3) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (aunit))) 
                 (aunit) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ;; challenge problem tests
   (check-equal? (find-free-vars fib) (set))
   (check-equal? (find-free-vars (mlet-body add-ten)) (set "ten"))
   (check-equal? (find-free-vars (mlet-body (mlet-body ten))) (set "x" "y"))
   (check-equal? (find-free-vars mupl-map) (set))
   (check-equal? (find-free-vars mupl-mapAddN) (set))
   
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 3) (int 3) (int 2))) (int 2))
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 2) (int 3) (int 2))) (int 3))
   (check-exn exn:fail? (lambda () (eval-exp-c (ifgreater (apair (int 1) (int 2)) (int 0) (int 1) (int 2)))))
   
   ;; mlet test
   (check-equal? (eval-exp-c (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-exn exn:fail? (lambda () (eval-exp-c (mlet "x" (int 1) (add (int 5) (var "y"))))))
   (check-equal? (eval-exp-c (mlet "x" (int 1) (mlet "x" (int 2) (add (int 5) (var "x"))))) (int 7) "mlet test")
   (check-equal? (eval-exp-c (mlet "x" (int 1) (mlet "y" (int 2) (add (var "y") (var "x"))))) (int 3) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp-c (call (closure '() (fun-challenge #f "x" (add (var "x") (int 7)) (set))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp-c (call fib (int 0))) (int 1))
   (check-equal? (eval-exp-c (call fib (int 1))) (int 1))
   (check-equal? (eval-exp-c (call fib (int 2))) (int 2))
   (check-equal? (eval-exp-c (call fib (int 3))) (int 3))
   (check-equal? (eval-exp-c (call fib (int 4))) (int 5))
   
   ;;snd test
   (check-equal? (eval-exp-c (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-exn exn:fail? (lambda () (eval-exp-c (snd (int 0)))))
   
   ;; fst test
   (check-equal? (eval-exp-c (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   (check-exn exn:fail? (lambda () (eval-exp-c (fst fib))))
   
   ;; isaunit test
   (check-equal? (eval-exp-c (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp-c (isaunit (aunit))) (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp-c (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 10)) (cons "y" (add (var "x") (int 1)))) (add (var "x") (var "y")))) (int 21) "mlet* test")
   
   ;; ifeq test
   (check-equal? (eval-exp-c (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp-c (ifeq (int 2) (int 2) (int 3) (int 4))) (int 3) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (aunit))) 
                 (aunit) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp-c (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
