
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (if (> low high)
       null 
       (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix) 
   (map (lambda(x) (string-append x suffix)) xs))
  
(define (list-nth-mod xs n) 
  ( cond [(< n 0) "list-nth-mod: negative number"]
         [(null? xs) "list-nth-mod: empty list"]
         [#t (let* ([len (length xs)] 
                  [real-len (remainder n len)])
               (car (list-tail xs real-len)))]))
        
(define (stream-for-n-steps s n)
  (cond [(= n 0) null ]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream 
  (letrec ([f (lambda(x) (cons 
                          (if (= (remainder x 5) 0)
                              (- 0 x)
                              x)
                          (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec (
   )