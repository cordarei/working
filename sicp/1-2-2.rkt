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


;; *Exercise 1.12:* The following pattern of numbers is called "Pascal's
;;      triangle".

;;                  1
;;                1   1
;;              1   2   1
;;            1   3   3   1
;;          1   4   6   4   1

;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it.(4)
;; Write a procedure that computes elements of Pascal's triangle by
;; means of a recursive process.

(define (pascal i j)
  (cond
    [(= 1 j) 1]
    [(= i j) 1]
    [else
     (+ (pascal (- i 1) (- j 1))
        (pascal (- i 1) j))]))

;; ﻿> (pascal 1 1)
;; 1
;; ﻿> (pascal 7 3)
;; 15
;; ﻿> (pascal 7 4)
;; 20
;; ﻿> (pascal 5 3)
;; 6
;; ﻿> (for* ([i (in-range 1 10)]
;;          [j (in-range 1 (+ 1 i))])
;;     (display (pascal i j)) (display (if (= i j) "\n" " ")))
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; 1 5 10 10 5 1
;; 1 6 15 20 15 6 1
;; 1 7 21 35 35 21 7 1
;; 1 8 28 56 70 56 28 8 1
