#lang racket

;; *Exercise 1.11:* A function f is defined by the rule that f(n) = n
;; if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative
;; process.

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-iter_ a b c n)
    (if (= n 0)
        c
        (f-iter_ (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (f-iter_ 2 1 0 n)
  )

;; 0
;; 1
;; 2
;; 2 + 2*1
;; (2 + 2*1) + 2*2 + 3*1
;; ((2 + 2*1) + 2*2 + 3*1) + 2*(2 + 2*1) + 3*2
;; (((2 + 2*1) + 2*2 + 3*1) + 2*(2 + 2*1) + 3*2) + 2*((2 + 2*1) + 2*2 + 3*1) + 3*(2 + 2*1)

;; 0
;; 1
;; 2
;; 4
;; 11
;; 25
;; 59
;; 142
;; 335
;; 796
