#lang racket
;; https://github.com/epsil/gll/blob/master/README.md

(require racket/mpair)

;; macro for simulating REPL checks
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ exp)
     #'(displayln
        (format "\n>>> ~s\n~s"
                (quote exp)
                exp))]))


(define (memo fn)
  (let ([alist (mlist)])
    (λ args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ([result (apply fn args)]
                  [entry (mcons args result)])
             (set! alist (mcons entry alist))
             result)]))))

(define-syntax-rule (delay-parser parser)
  (λ args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (delay-parser body)))

(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define (succeed val)
  (λ (str)
    (success val str)))


(check (succeed '()))
(check ((succeed '()) "foo"))


(define (string match)
  (λ (str)
    (let* ([len (min (string-length str) (string-length match))]
           [head (substring str 0 len)]
           [tail (substring str len)])
      (if (equal? match head)
          (success head tail)
          (failure str)))))


(check ((string "foo") "foobar"))
(check ((string "foo") "bar"))


(define alt
  (memo
   (λ (a b)
     (memo
      (λ (str)
        (let ([result (a str)])
          (match result
            [(success val rest) result]
            [failure (b str)])))))))


(check ((alt (string "foo")
             (string "bar"))
        "foobar"))
(check ((alt (string "foo")
             (string "bar"))
        "barfoo"))


(define bind
  (memo
   (λ (p fn)
     (memo
      (λ (str)
        (match (p str)
          [(success val rest)
           ((fn val) rest)]
          [failure failure]))))))

(define (seq a b)
  (bind a (λ (x)
            (bind b (λ (y)
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


;; self-referential parsers:

#;(define r
  (alt (seq (string "a") r)
       (string "a")))

#;(define r
  (λ (arg)
    ((alt (seq (string "a") r)
          (string "a"))
     arg)))

(define-parser r
  (alt (seq (string "a") r)
       (string "a")))

(check (r "aaaaa"))
(check (r "a"))
(check (r "aa"))
(check (r ""))
(check (r "b"))