;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require xrepl) ; for ctrl + up 
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

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
; a
(define (racketlist->mupllist es) 
  (cond [(null? es) (aunit)]
        [#t (apair (car es) (racketlist->mupllist (cdr es)))]))
; b
(define (mupllist->racketlist es) 
  (cond [(aunit? es) null]
        [#t (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))]))

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
  (cond 
;        [(var? e) 
;         (begin 
;           (fprintf (current-output-port) "Env in var eval: ~s ~n" env)
;           (envlookup env (var-string e)))]
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
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(ifgreater? e) 
         (let ([temp-e1 (eval-under-env (ifgreater-e1 e) env)]
               [temp-e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? temp-e1) (int? temp-e2))
               (if (> (int-num temp-e1) (int-num temp-e2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "ifgreater has to take ints")))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(mlet? e) 
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [temp-env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) temp-env))]  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(apair? e)
         ( let([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(fst? e)
         (let([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "fst has to have a pair as an input.")))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(snd? e)
         (let([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "snd has to have a pair as an input.")))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(fun? e) (closure env e)]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(call? e)
        (letrec ([temp-closure (eval-under-env (call-funexp e) env)]
                 [temp-actual (eval-under-env (call-actual e) env)])
          (if (closure? temp-closure)
              (letrec ([temp-fun (closure-fun temp-closure)]
                       [temp-env (cons (cons (fun-formal temp-fun) temp-actual) (closure-env temp-closure))])
                (if (fun-nameopt temp-fun) 
                    (eval-under-env (fun-body temp-fun) (cons (cons (fun-nameopt temp-fun) temp-closure) temp-env))
                    (eval-under-env (fun-body temp-fun) temp-env)
                                                             ))
              (error "first argument must be a closure")))]
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) 
             (int 1)
             (int 0))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
        [(closure? e) e]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
        [#t (error "bad MUPL expression")]))
 
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
  (letrec ([f (lambda (lst env)
                (if (null? lst)
                    (eval-under-env e2 env)
                    (letrec ([first (car lst)]
                             [var2b (car first)]
                             [muple (cdr first)])
                      (f (cdr lst) (cons (cons var2b (eval-under-env muple env)) env))))
                )])
    (f lstlst null)))

(define (ifeq e1 e2 e3 e4)
  (letrec ([v1 (eval-exp e1)]
           [v2 (eval-exp e2)])
  (ifgreater v1 v2 e4 (ifgreater v2 v1 e4 e3))))

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

