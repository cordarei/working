#lang racket

;; *Exercise 1.1:* Below is a sequence of expressions.  What is the
;; result printed by the interpreter in response to each expression?
;; Assume that the sequence is to be evaluated in the order in which
;; it is presented.

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

#|
Answer:
﻿> 10
﻿> 12
﻿> 8
﻿> 3
﻿> 6
﻿> 19
﻿> #f
﻿> 4
﻿> 16
﻿> 6
﻿> 16
|#


#|
*Exercise 1.2:* Translate the following expression into prefix
form.

5 + 4 + (2 - (3 - (6 + 4/5)))
-----------------------------
3(6 - 2)(2 - 7)
|#

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; ﻿> -37/150


#|
*Exercise 1.3:* Define a procedure that takes three numbers as
arguments and returns the sum of the squares of the two larger
numbers.
|#

(define (foo a b c)
  (let ((a2 (* a a))
        (b2 (* b b))
        (c2 (* c c)))
    (- (+ a2 b2 c2)
       (min a2 b2 c2))))

(define (foo2 a b c)
  (define x
    (if (< a b) a b))
  (define y
    (if (> a b) a b))
  (define z
    (if (> x c) x c))
  (+ (* z z) (* y y)))

#|
﻿> (foo 1 2 3)
13
﻿> (foo 42 5 3)
1789
﻿> (foo 6 8 10)
164
﻿> (foo2 1 2 3)
13
﻿> (foo2 42 5 3)
1789
﻿> (foo2 6 8 10)
164
|#
