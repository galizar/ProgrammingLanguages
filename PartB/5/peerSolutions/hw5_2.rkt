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

(define (racketlist->mupllist exps)
  (if (null? exps)
      (aunit)
      (apair (car exps) (racketlist->mupllist (cdr exps)))))

(define (mupllist->racketlist exps)
  (if (aunit? exps)
      null
      (cons (apair-e1 exps) (mupllist->racketlist (apair-e2 exps)))))

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
        
        ;; cases implemented by me

        [(int? e) e]
        
        [(closure? e) e]

        [(fun? e) (closure env e)]
        
        [(aunit? e) e]

        [(apair? e)
         (let [(v1 (eval-under-env (apair-e1 e) env))
               (v2 (eval-under-env (apair-e2 e) env))]
           (apair v1 v2))]

        [(isaunit? e)
         (let [(v (eval-under-env (isaunit-e e) env))]
           (if (aunit? v)
               (int 1)
               (int 0)))]
        
        [(fst? e)
         (let [(v (eval-under-env (fst-e e) env))]
           (if (apair? v)
               (apair-e1 v)
               (error "fst applied to non-apair")))]
        
        [(snd? e)
         (let [(v (eval-under-env (snd-e e) env))]
           (if (apair? v)
               (apair-e2 v)
               (error "snd applied to non-apair")))]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "ifgreater applied to non-number")))]

        [(mlet? e)
         (let* ([val (eval-under-env (mlet-e e) env)]
                [env (cons (cons (mlet-var e) val) env)])
           (eval-under-env (mlet-body e) env))]

        [(call? e)
         (let ([the-closure (eval-under-env (call-funexp e) env)])
           (if (closure? the-closure)
               (let* ([val (eval-under-env (call-actual e) env)]
                      [f (closure-fun the-closure)]
                      [fname (fun-nameopt f)]
                      [env (cons (cons (fun-formal f) val)
                                 (if fname
                                     (cons (cons fname the-closure) (closure-env the-closure))
                                     (closure-env the-closure)))])
                 (eval-under-env (fun-body f) env))
               (error "calling not a function")))]
                
        ;; end of cases implemented by me
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([bind (car lstlst)])
        (mlet (car bind) (cdr bind) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "loop" "es"
            (ifaunit (var "es")
                     (aunit)
                     (apair (call (var "f") (fst (var "es")))
                            (call (var "loop") (snd (var "es"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "nums"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "nums"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment

(define (compute-free-vars e) ; returns converted MUPL exp (not a list of free vars itself)
                              ; as it is said in Homework Assignment 5
  
  (letrec ([loop (λ (e)       ; returns a cons of:
                              ;   - the same e (except for "fun" it returns "fun-challenge") and
                              ;   - a set of its free variables:
                              ;       - fun exp     -> (fun-body's "vars") - ("fun-formal" + "nameopt")
                              ;       - mlet exp    -> (mlet-e's "vars") + [(mlet-body's "vars") - "mlet-var"]
                              ;       - any other e -> set of its "vars"
                   
                   (cond [(var? e)
                          (cons e (set (var-string e)))]
                         
                         [(fst? e)
                          (let ([ans (loop (fst-e e))])
                            (cons (fst (car ans))
                                  (cdr ans)))]
                         
                         [(snd? e)
                          (let ([ans (loop (snd-e e))])
                            (cons (snd (car ans))
                                  (cdr ans)))]
                         
                         [(isaunit? e)
                          (let ([ans (loop (isaunit-e e))])
                            (cons (isaunit (car ans))
                                  (cdr ans)))]

                         [(apair? e)
                          (let ([ans1 (loop (apair-e1 e))]
                                [ans2 (loop (apair-e2 e))])
                            (cons (apair (car ans1) (car ans2))
                                  (set-union (cdr ans1) (cdr ans2))))]
                         
                         [(add? e)
                          (let ([ans1 (loop (add-e1 e))]
                                [ans2 (loop (add-e2 e))])
                            (cons (add (car ans1) (car ans2))
                                  (set-union (cdr ans1) (cdr ans2))))]
                         
                         [(ifgreater? e)
                          (let ([ans1 (loop (ifgreater-e1 e))]
                                [ans2 (loop (ifgreater-e2 e))]
                                [ans3 (loop (ifgreater-e3 e))]
                                [ans4 (loop (ifgreater-e4 e))])
                            (cons (ifgreater (car ans1) (car ans2) (car ans3) (car ans4))
                                  (set-union (cdr ans1) (cdr ans2) (cdr ans3) (cdr ans4))))]

                         [(call? e)
                          (let ([ans1 (loop (call-funexp e))]
                                [ans2 (loop (call-actual e))])
                            (cons (call (car ans1) (car ans2))
                                  (set-union (cdr ans1) (cdr ans2))))]

                         [(mlet? e)
                          (let ([var (mlet-var e)]
                                [ans1 (loop (mlet-e e))]
                                [ans2 (loop (mlet-body e))])
                            (cons (mlet var (car ans1) (car ans2))
                                  (set-union (cdr ans1) (set-remove (cdr ans2) var))))]

                         [(fun? e)
                          (let* ([nameopt (fun-nameopt e)]
                                 [formal (fun-formal e)]
                                 [body (fun-body e)]
                                 [ans (loop body)]
                                 [freevars (set-subtract (cdr ans) (set nameopt formal))])
                            (cons (fun-challenge nameopt formal (car ans) (set->list freevars))
                                  freevars))]
                         
                         [#t (cons e (set))]))])
    (car (loop e)))) 

;; Some of my test cases:
;
;  ********************************************************************************************************
;    Test case 1: (compute-free-vars mupl-map)
;  ********************************************************************************************************
;
;   (check-equal? (compute-free-vars mupl-map)
;                 (fun-challenge #f "f"
;                                (fun-challenge "loop" "es"
;                                               (ifgreater (isaunit (var "es")) (int 0)
;                                                          (aunit)
;                                                          (apair (call (var "f") (fst (var "es")))
;                                                                 (call (var "loop") (snd (var "es")))))
;                                               '("f"))
;                                '())
;                 "challenge problem test 1")
;
;  ********************************************************************************************************
;    Test case 1: (compute-free-vars mupl-mapAddN)
;  ********************************************************************************************************
;   
;   (check-equal? (compute-free-vars mupl-mapAddN)
;                 (mlet "map"
;                       (fun-challenge #f "f"
;                                      (fun-challenge "loop" "es"
;                                                     (ifgreater (isaunit (var "es")) (int 0)
;                                                                (aunit)
;                                                                (apair (call (var "f") (fst (var "es")))
;                                                                       (call (var "loop") (snd (var "es")))))
;                                                     '("f"))
;                                      '())
;                       (fun-challenge #f "i"
;                                      (fun-challenge #f "nums"
;                                                     (call (call (var "map") (fun-challenge #f "x"
;                                                                                            (add (var "x") (var "i"))
;                                                                                            '("i")))
;                                                           (var "nums"))
;                                                     '("i" "map"))
;                                      '("map")))
;                 "challenge problem test 2")
;
;  ********************************************************************************************************
;    Test case 3
;  ********************************************************************************************************
;   
;   (check-equal? (let ([mm (compute-free-vars mupl-map)]
;                       [f (compute-free-vars (fun #f "x" (add (var "x") (int 1))))]
;                       [lst (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))])
;                   (eval-exp-c (call (call mm f) lst)))
;                 (apair (int 2) (apair (int 3) (apair (int 4) (aunit))))
;                 "challenge problem test 3")
;
;; End of my test cases

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes

(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        ;; the most interesting part is here:
        [(fun-challenge? e)
         (let* ([vars (fun-challenge-freevars e)]
                [env (map (λ (s) (cons s (envlookup env s))) vars)])
           (closure env e))]
        ;; end of the most interesting part
        [(aunit? e) e]
        [(apair? e)
         (let [(v1 (eval-under-env-c (apair-e1 e) env))
               (v2 (eval-under-env-c (apair-e2 e) env))]
           (apair v1 v2))]
        [(isaunit? e)
         (let [(v (eval-under-env-c (isaunit-e e) env))]
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fst? e)
         (let [(v (eval-under-env-c (fst-e e) env))]
           (if (apair? v)
               (apair-e1 v)
               (error "fst applied to non-apair")))]
        [(snd? e)
         (let [(v (eval-under-env-c (snd-e e) env))]
           (if (apair? v)
               (apair-e2 v)
               (error "snd applied to non-apair")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([val (eval-under-env-c (mlet-e e) env)]
                [env (cons (cons (mlet-var e) val) env)])
           (eval-under-env-c (mlet-body e) env))]        
        [(call? e)
         (let ([the-closure (eval-under-env-c (call-funexp e) env)])
           (if (closure? the-closure)
               (let* ([val (eval-under-env-c (call-actual e) env)]
                      [f (closure-fun the-closure)]
                      [fname (fun-challenge-nameopt f)]
                      [env (cons (cons (fun-challenge-formal f) val)
                                 (if fname
                                     (cons (cons fname the-closure) (closure-env the-closure))
                                     (closure-env the-closure)))])
                 (eval-under-env-c (fun-challenge-body f) env))
               (error "calling not a function")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
