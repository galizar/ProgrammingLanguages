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

(define (racketlist->mupllist xs)
  (if (null? xs) (aunit) (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (cond [(or (null? xs) (aunit? xs)) null]
        [(list? xs) (cons (car xs) (mupllist->racketlist (cdr xs)))]
        [(apair? xs) (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) (aunit)]
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1)
                                   (int? v2))
                              (if (> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
                              (error "MUPL Greater Than applied to non-number")))]

        [(mlet? e) (let ([lenv (cons (mlet-var e) (eval-under-env (mlet-e e) env))]) (eval-under-env (mlet-body e) (cons lenv env)))]
        [(fun? e) (closure env e)]
        [(call? e) (let* ([funexp (eval-under-env (call-funexp e) env)]
                          [actual (eval-under-env (call-actual e) env)]
                          [closure-env (closure-env funexp)]
                          [closure-fun (closure-fun funexp)]
                          [fun-name (cons (fun-nameopt closure-fun) funexp)]
                          [parameter (cons (fun-formal closure-fun) actual)]
                          [fun-body (fun-body closure-fun)]
                          [fun-env (cons parameter (if (fun-nameopt closure-fun)
                                                       (cons fun-name closure-env)
                                                       closure-env))])
                     (eval-under-env fun-body fun-env))]
        [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                      (apair v1 v2))]
        [(fst? e) (let ([ex (eval-under-env (fst-e e) env)])
                    (if (apair? ex)
                        (apair-e1 ex)
                        (error "Expression not a pair")))]
        [(snd? e) (let ([ex (eval-under-env (snd-e e) env)])
                    (if (apair? ex)
                        (apair-e2 ex)
                        (error "Expression not a pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) (letrec ([mlet-env (lambda (vars env)
                                               (if (null? vars)
                                                   env
                                                   (mlet-env (cdr vars) (cons (cons (car (car vars)) (eval-under-env (cdr (car vars)) env)) env))))])
                            (let ([mlenv (mlet-env lstlst null)]) (eval-under-env e2 mlenv))))


(define (ifeq e1 e2 e3 e4) (let ([_x (eval-exp e1)]
                                 [_y (eval-exp e2)])
                             (if (eq? _x _y) (eval-exp e3) (eval-exp e4) )))

;; Problem 4

 (define mupl-map (fun #f "f" (fun "aux" "xs"
                                  (ifaunit (var "xs")
                                           (aunit)
                                           (apair (call (var "f") (fst (var "xs")))
                                                  (call (var "aux") (snd (var "xs"))))))))


(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "num" (call (var "map") (call (fun #f "x" (fun #f "y" (add (var "x") (var "y")))) (var "num"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
