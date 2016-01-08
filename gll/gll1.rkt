#lang racket
;; https://github.com/epsil/gll/blob/master/README.md

;; macro for simulating REPL checks
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ exp)
     #'(displayln
        (format "\n>>> ~s\n~s"
                (quote exp)
                exp))]))


(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define (succeed val)
  (lambda (str)
    (success val str)))


(check (succeed '()))
(check ((succeed '()) "foo"))


(define (string match)
  (lambda (str)
    (let* ([len (min (string-length str) (string-length match))]
           [head (substring str 0 len)]
           [tail (substring str len)])
      (if (equal? match head)
          (success head tail)
          (failure str)))))


(check ((string "foo") "foobar"))
(check ((string "foo") "bar"))


(define (alt a b)
  (lambda (str)
    (let ([result (a str)])
      (match result
        [(success val rest) result]
        [failure (b str)]))))


(check ((alt (string "foo")
             (string "bar"))
        "foobar"))
(check ((alt (string "foo")
             (string "bar"))
        "barfoo"))


(define (seq a b)
  (lambda (str)
    (match (a str)
       [(success val1 rest1)
        (match (b rest1)
          [(success val2 rest2)
           (success (list val1 val2) rest2)]
          [failure failure])]
       [failure failure])))


(check ((seq (string "foo")
             (string "bar"))
        "foobar"))
(check ((seq (string "foo")
             (string "bar"))
        "barfoo"))
(check ((seq (string "foo")
             (string "bar"))
        "foobaz"))

