
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([fn-for-int (λ (curr acc)
                         (if (> curr high)
                             (reverse acc)
                             (fn-for-int (+ curr stride) (cons curr acc))))])
    (fn-for-int low empty)))

(define (string-append-map xs suffix)
  (map (λ (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (if (> 0 n)
      (error "list-nth-mod: negative number")
      (letrec ([fn-for-lst (λ (lst curr-idx)
                             (cond [(empty? lst) (error "list-nth-mod: empty list")]
                                   [else
                                    (let ([objective-idx (modulo n (length xs))])
                                      (if (= curr-idx objective-idx)
                                          (first lst)
                                          (fn-for-lst (rest lst) (add1 curr-idx))))]))])
        (fn-for-lst xs 0))))

(define (stream-for-n-steps s n)
  (letrec ([fn-for-stream (λ (stream acc steps-taken)
                            (let ([next (stream)])
                              (if (= n steps-taken)
                                  (reverse acc)
                                  (fn-for-stream (cdr next) (cons (car next) acc) (add1 steps-taken)))))])
    (fn-for-stream s empty 0)))

(define (funny-number-stream)
  (letrec ([f (λ (x)
                (let ([next (λ () (f (add1 x)))])
                  (if (= 0 (modulo x 5))
                      (cons (- x) next)
                      (cons     x next))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([f (λ (b)
                (let ([next (λ () (f (not b)))])
                  (if b
                      (cons "dan.jpg" next)
                      (cons "dog.jpg" next))))])
    (f true)))

(define (stream-add-zero s)
  (letrec ([f (λ (stream)
                (let* ([eval-strm (stream)]
                       [next (λ () (f (cdr eval-strm)))])
                  (λ () (cons (cons 0 (car eval-strm))
                              (next)))))])
    (f s)))

(define (cycle-lists xs ys) (cons "test" (λ () "test")))