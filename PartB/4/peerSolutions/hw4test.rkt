#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4_2.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test 1")
   (check-equal? (sequence 0 3 2) (list 0 2) "seqtest 2")
   (check-equal? (sequence 0 1 3) (list 0) "seqtest 3")
   (check-equal? (sequence 0 6 2) (list 0 2 4 6) "seqtest4")
   
   ; string-append-map test
   (check-equal? (string-append-map empty "lel") empty "sam test 1")
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   (check-equal? (list-nth-mod (list "h" "e" "l" "l" "o") 0) "h" "list-nth-mod test 1")
   (check-equal? (list-nth-mod (list 0 1 2) 3) 0 "list-nth-mod test 2")
   (check-equal? (list-nth-mod (list 0 1 2) 5) 2 "list-nth-mod test 3")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 0) empty "stream-for-n-steps test 0")
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test 1")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 2)
                 (list (cons 0 "dan.jpg") (cons 0 "dog.jpg"))
                 "stream-add-zero test 1")

   ;; -- P8 --
   ; mobius-list tests
;   (check-equal? (stream-for-n-steps (λ () (mobius-list (list 1 2 3))) 4)
;                 (list 1 2 3 1))
;   (check-equal? (stream-for-n-steps (λ () (mobius-list (list 1 2 3))) 6)
;                 (list 1 2 3 1 2 3))
;
;   ; zip-stream-pair tests
;   (check-equal? (stream-for-n-steps (zip-stream-pair ones ones) 3)
;                 (list (cons 1 1) (cons 1 1) (cons 1 1)))
;
;   (check-equal? (stream-for-n-steps (zip-stream-pair
;                                      (λ () (mobius-list (list 1 2 3)))
;                                      (λ () (mobius-list (list 3 2 1))))
;                                     6)
;                 (list (cons 1 3) (cons 2 2) (cons 3 1) (cons 1 3) (cons 2 2) (cons 3 1)))
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists  (list 1 2 3) (list "a" "b" "c")) 6)
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "c") (cons 1 "a") (cons 2 "b") (cons 3 "c")))

;; -- P9 --
   ; vector-assoc test
   (check-equal? (vector-assoc "hello" (vector (cons 1 2) (cons "lorem" "ipsum") 5)) false)
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc empty 5) 3) false)
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons "h" 1) (cons "l" 3) (cons "lel" "foo")) 5) 3) false)
   
;   ; while-less test
;   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
