
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
                (let* ([eval-strm (stream)])
                  (λ () (cons (cons 0 (car eval-strm))
                              (f (cdr eval-strm))))))])
    (f s)))

;; -- P8 -- 

;; IMPERATIVE: l is a non-empty list
(define (mobius-list l)
  (letrec ([fn-for-list (λ (alist)
                          (cond [(empty? alist) (fn-for-list l)]
                                [else
                                 (cons (first alist)
                                       (λ () (fn-for-list (rest alist))))]))])
    (fn-for-list l)))

(define (zip-stream-pair s1 s2)
  (letrec ([fn-for-strmpair (λ (strm1 strm2)
                              (let ([strm-eval1 (strm1)]
                                    [strm-eval2 (strm2)])
                                (λ () (cons (cons (car strm-eval1) (car strm-eval2))
                                            (fn-for-strmpair (cdr strm-eval1) (cdr strm-eval2))))))])
    (fn-for-strmpair s1 s2)))

;; IMPERATIVE: xs and ys are non-empty lists
(define (cycle-lists xs ys)
  (zip-stream-pair
   (λ () (mobius-list xs))
   (λ () (mobius-list ys))))

;; -- P9 --
(define (vector-assoc v vec)
  (letrec ([iterator (λ (idx-seq)
                       (cond [(empty? idx-seq) false]
                             [else
                              (let ([curr (vector-ref vec (first idx-seq))])
                                (if (and (pair? curr) (equal? (car curr) v))
                                    curr
                                    (iterator (rest idx-seq))))]))])
    (iterator (sequence 0 (sub1 (vector-length vec)) 1))))

;; -- P10 --
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n false)]
           [fill-pos 0]
           [cache-and-return (λ (pr)
                               (begin (vector-set! cache fill-pos pr)
                                      (set! fill-pos (if (= fill-pos (sub1 n)) 0 (add1 fill-pos)))
                                      pr))]
           [cassoc (λ (v)
                     (let ([cached (vector-assoc v cache)])
                       (cond [(not (false? cached)) cached]
                             [else
                              (let ([item (assoc v xs)])
                                (cond [(not (false? item)) (cache-and-return item)]
                                      [else false]))])))])
    cassoc))