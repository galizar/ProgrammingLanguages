
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0)
         (error "list-nth-mod: negative number")]
        [(null? xs)
         (error "list-nth-mod: empty list")]
        [#t
         (car (list-tail xs (remainder n (length xs))))]))

;problem 4
(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([next (s)]) (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

;problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0)
                          (- x)
                          x)
                      (lambda () (f (+ x 1)))))])
   (lambda () (f 1))))

;problem 6
(define (dan-then-dog)
  (cons "dan.jpg"
        (lambda ()
          (cons "dog.jpg"
                dan-then-dog))))

;problem 7
(define (stream-add-zero s) (lambda ()
                              (let ([next (s)])
                              (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))
                             

;problem 8
(define (cycle-lists xs ys)
    (letrec ([f (lambda (n)
                  (cons (cons (list-nth-mod xs n)
                              (list-nth-mod ys n))
                        (lambda () (f (+ n 1)))))])
      (lambda () (f 0))))


;problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond
                  [(= n (vector-length vec)) #f]
                  [(and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n))))
                   (vector-ref vec n)]
                  [#t (f (+ n 1))]))])
    (f 0)))

;problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [k 0])
    (lambda (v)
        (or (vector-assoc v cache)
            (let ([new-ans (assoc v xs)])
              (and new-ans
                  (begin (vector-set! cache k new-ans)
                         (set! k (if (= k (- n 1))
                                    0
                                    (+ k 1)))
                         new-ans)))))))
             