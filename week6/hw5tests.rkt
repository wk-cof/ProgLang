#lang racket

(require "hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
;(define test1
;  (mupllist->racketlist
;   (eval-exp (call (call mupl-mapAddN (int 7))
;                   (racketlist->mupllist 
;                    (list (int 3) (int 4) (int 9)))))))


"******************"
"TESTS FOR PROBLEM 1"
"(apair 2 (apair 3 (apair 4 (aunit)))) : Expected Output"
(racketlist->mupllist '(2 3 4))

(aunit? (racketlist->mupllist '()))

(eq? 'air-bag
 (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
   (racketlist->mupllist 2)))
    
"'(2 3 4) : Expected Output"
(mupllist->racketlist (racketlist->mupllist (list 2 3 4)))

(eq? (mupllist->racketlist (aunit)) '())

(eq? 'air-bag
 (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
   (mupllist->racketlist (list 2 3 4))))


"******************"
"TESTS FOR PROBLEM 2"
(define test2a (int 7))
(eq? (eval-exp test2a) test2a) 

(define test2b (aunit))
(eq? (eval-exp test2b) test2b) 

(eq?
 (eval-under-env (var "X") (list (cons "A" (int 2)) (cons "X" test2a) (cons "B" (int 22)) (cons "X" test2b)))
 test2a)

(define (int-eq? A B)
  (eq? (int-num A) (int-num B)))
(int-eq? test2a (eval-exp(add (int 1) (add (int 3) (int 3)))))

(int-eq? test2a
         (eval-exp (ifgreater (int 7) (int 6) test2a test2b)))

(int-eq?
 (eval-under-env (mlet "X" (int 7) (var "X") )
                 (list (cons "A" 2) (cons "B" 22) (cons "X" test2b)))
 test2a)
  
(int-eq? (eval-exp (fst (apair (int 2) (add (int 3) (int 2)))))  (int 2))
(int-eq? (eval-exp (snd (apair (int 2) (add (int 3) (int 2)))))  (int 5))

(int-eq? (int 1) (eval-exp (isaunit (aunit))))
(int-eq? (int 0) (eval-exp (isaunit test2a)))

(define my-closure 
  (closure
   (list (cons "Y" (int 23)))
   (fun #f "X" (add (var "X") (add (var "Y") (int 1))))))
(int-eq? (int 33)
         (eval-exp (call my-closure (int 9))))

(define roll-sum (fun "roll-sum" "X" (ifgreater (int 1)  (var "X") (int 0) (add (var "X") (call (var "roll-sum") (add (var "X") (int -1)))))))

"Roll SUM"
(define x 10)
(int-eq? (int (/ (* x (+ 1 x)) 2))
         (eval-exp (call (closure null roll-sum) (int x))))


"******************"
"TESTS FOR PROBLEM 3"
(int-eq? (int 2)
 (eval-exp (ifaunit (aunit) (int 2) (int 4))))
(int-eq? (int 4)
 (eval-exp (ifaunit test2a (int 2) (int 4))))


(int-eq? (int 5)
 (mlet* (list (cons "X" (int 2)) (cons "Y" (int 3))) (add (var "X") (var "Y"))))
(int-eq? (int 205)
 (mlet* (list (cons "X" (int 2)) (cons "Y" (add (int 3) (var "X")))  (cons "X" (int 200))) (add (var "X") (var "Y"))))

(int-eq?
 (eval-exp (ifeq (add (int 2) (int 4)) (add (int 3) (int 3))
                 (add (int 1) (int 1)) (add (int 9) (int 9))))
 (int 2))

(int-eq?
 (eval-exp (ifeq (add (int 2) (int 4)) (add (int 1) (int 3))
                 (add (int 1) (int 1)) (add (int 9) (int 9))))
 (int 18))


