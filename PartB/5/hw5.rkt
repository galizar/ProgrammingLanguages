;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; Receives a list with Racket values and returns a list with the equivalent values in MUPL in the same order
;; The passed list may contain MUPL values already

(define (reverse-apair apr)
  (letrec ([aux (λ (le-pair result)
                  (cond [(aunit? le-pair) result]
                        [else (aux (apair-e2 le-pair) (apair (apair-e1 le-pair) result))]))])
    (aux apr (aunit))))

(define (racketlist->mupllist l)
  (letrec ([to-mupl-val (λ (val)
                          (cond [(null? val) (aunit)]
                                [(integer? val) (int val)]
                                [(pair? val)
                                 (cond [(list? val) (aux val (aunit))]
                                       [else (apair (to-mupl-val (car val)) (to-mupl-val (cdr val)))])]
                                [else val]))]
           [aux (λ (alist acc)
                  (cond [(null? alist) (reverse-apair acc)]
                        [else (aux (rest alist) (apair (to-mupl-val (first alist)) acc))]))])
    (aux l (aunit))))

(define (mupllist->racketlist mupl-pair)
  (letrec ([aux (λ (pr acc)
                  (cond [(aunit? pr) (reverse acc)]
                        [else (aux (apair-e2 pr) (cons (apair-e1 pr) acc))]))])
    (aux mupl-pair empty)))

;; Problem 2

; lookup a variable in an environment
; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;;; Do NOT change the two cases given to you.  
;;; DO add more cases for other kinds of MUPL expressions.
;;; We will test eval-under-env by calling it directly even though
;;; "in real life" it would be a helper function of eval-exp.
;; e: MUPL exp | env: (Racket list of Racket pairs)
(define (eval-under-env e env)
  (cond [(or (int? e) (closure? e) (aunit? e))
         e]
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([e1-evaluated (eval-under-env (ifgreater-e1 e) env)]
               [e2-evaluated (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? e1-evaluated)
                    (int? e2-evaluated)
                    (> (int-num e1-evaluated) (int-num e2-evaluated)))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]
        [(mlet? e)
         (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e)
         (let ([evaluated-funexp (eval-under-env (call-funexp e) env)])
           (if (closure? evaluated-funexp)
               (let ([func (closure-fun evaluated-funexp)])
                 (eval-under-env (fun-body func)
                                 (cons (cons (fun-nameopt func) evaluated-funexp)     ;; associates function name to closure
                                       (cons (cons (fun-formal func) (eval-under-env (call-actual e) env)) ;; associates call argument to function argument name
                                             (closure-env evaluated-funexp)))))               
               (error (format "bad MUPL expression: ~v" e))))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(and (fst? e) (apair? (eval-under-env (fst-e e) env)))
         (apair-e1 (eval-under-env (fst-e e) env))]
        [(and (snd? e) (apair? (eval-under-env (snd-e e) env)))
         (apair-e2 (eval-under-env (snd-e e) env))]
        [(isaunit? e)
         (let ([res (eval-under-env (isaunit-e e) env)])
           (if (aunit? res) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2)
  (letrec ([fn-for-lst (λ (alist acc-env)
                         (cond [(empty? alist) (eval-under-env e2 acc-env)]
                               [else (fn-for-lst (rest alist)
                                                 (cons (cons (car (first alist))
                                                             (eval-under-env (cdr (first alist)) acc-env))
                                                       acc-env))]))])
    (fn-for-lst lstlst empty)))

(define (ifeq e1 e2 e3 e4)
  (if (and (int? e1) (int? e2) (= (int-num e1) (int-num e2)))
      e3
      e4))

;; Problem 4

(define mupl-map
  (fun #f "func" (fun "mmap" "lst" (ifgreater (isaunit (var "lst")) (int 0)
                                             (aunit)
                                             (apair (call (var "func") (fst (var "lst")))
                                                    (call (var "mmap") (snd (var "lst"))))))))
         
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n" (fun #f "alst" (call (call (var "map") (fun #f "x" (add (var "x") (var "n")))) (var "alst"))))))

;;; Challenge Problem
;
;(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function
;
;;; We will test this function directly, so it must do
;;; as described in the assignment
;(define (compute-free-vars e) "CHANGE")
;
;;; Do NOT share code with eval-under-env because that will make
;;; auto-grading and peer assessment more difficult, so
;;; copy most of your interpreter here and make minor changes
;(define (eval-under-env-c e env) "CHANGE")
;
;;; Do NOT change this
;(define (eval-exp-c e)
;  (eval-under-env-c (compute-free-vars e) null))
