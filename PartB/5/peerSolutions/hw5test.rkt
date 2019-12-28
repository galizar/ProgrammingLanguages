#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5_2.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"

   ;; -- P1 --
   ;; -- a --

   ;; test reverse-apair
;   (check-equal? (reverse-apair (aunit)) (aunit))
;   (check-equal? (reverse-apair (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))
;                 (apair (int 3) (apair (int 2) (apair (int 1) (aunit)))))
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist empty) (aunit))
   (check-equal? (racketlist->mupllist null) (aunit))
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (aunit) 5 (cons 1 2))) (apair (aunit) (apair (int 5) (apair (apair (int 1) (int 2)) (aunit)))))
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (aunit) (apair (int 3) (aunit)))) (list (aunit) (int 3)))

   ;; -- Problem 2 --
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
;   (check-equal? (eval-exp (ifgreater (int 2) (aunit) (int 1) (int 4))) (int 4) "ifgreater test one exp does not eval to (int)")
   ;; mlet & add test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet & add test")
   ;; var test
   (check-equal? (eval-under-env (var "foo") (list (cons "foo" (int 3)))) (int 3) "var test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (closure (list (cons "n" (int 8))) (fun "add8" "x" (add (var "x") (var "n"))))
                                 (int 2)))
                 (int 10) "call test using closure environment")
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "foo" (int 7))
                                        (cons "bar" (int 6))
                                        (cons "lorem" (add (var "foo") (var "bar"))))
                                  (add (int 8) (var "lorem"))))
                 (int 21))
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (aunit) (aunit) (int 55))) (int 55))
   (check-equal? (eval-exp (ifeq (int 2) (int 2) (add (int 2) (int 2)) (aunit))) (int 4))
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-mapAddN (int 7))
                                  (racketlist->mupllist 
                                   (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   

))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
