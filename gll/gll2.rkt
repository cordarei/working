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


(define (bind p fn)
  (lambda (str)
    (match (p str)
      [(success val rest)
       ((fn val) rest)]
      [failure failure])))

(define (seq a b)
  (bind a (lambda (x)
            (bind b (lambda (y)
                      (succeed (list x y)))))))


(check ((seq (string "foo")
             (string "bar"))
        "foobar"))
(check ((seq (string "foo")
             (string "bar"))
        "barfoo"))
(check ((seq (string "foo")
             (string "bar"))
        "foobaz"))

(define article
  (alt (string "the ")
       (string "a ")))

(define noun
  (alt (string "student ")
       (string "professor ")))

(define verb
  (alt (string "studies ")
       (string "lectures ")))

(define noun-phrase
  (seq article noun))

(define verb-phrase
  (seq verb noun-phrase))

(define sentence
  (seq noun-phrase verb-phrase))


(check (noun-phrase "the student "))
(check (sentence "the professor lectures the student "))
(check (sentence "the professor lectures the student a lot"))
(check (sentence "not a sentence "))