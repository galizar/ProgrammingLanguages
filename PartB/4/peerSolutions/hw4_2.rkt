#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([x (s)])
        (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (n)
                (cons (if (= (remainder n 5) 0) (- n) n)
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (flag)
                (cons
                 (if flag "dan.jpg" "dog.jpg")
                 (lambda () (f (not flag)))))])
    (lambda () (f #t))))

(define (stream-add-zero s)
  (lambda ()
    (let ([x (s)])
      (cons (cons 0 (car x)) (stream-add-zero (cdr x))))))
               
(define (cycle-lists xs ys)
  (define (f n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

(define (vector-assoc v vec)
  (define (loop n)
    (cond [(>= n (vector-length vec)) #f]
          [(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v))
           (vector-ref vec n)]
          [#t (loop (+ n 1))]))
  (loop 0))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [idx 0]
           [f (lambda (v)
                (let ([cache-result (vector-assoc v cache)])
                  (if cache-result
                      cache-result
                      (let ([result (assoc v xs)])
                        (begin
                          (if result
                              (begin
                                (vector-set! cache idx result)
                                (set! idx (if (= idx (- n 1)) 0 (+ idx 1))))
                              #f)
                          result)))))])
    f))


