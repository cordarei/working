#lang racket
;; https://github.com/epsil/gll/blob/master/README.md

(require racket/mpair)

;; macro for simulating REPL checks
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ exp)
     #'(begin
         (displayln (format "\n>>> ~s" (quote exp)))
         (displayln (format "~s" exp)))]))


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
  (λ (str cont)
    (cont (success val str))))


(check (succeed '()))
(check ((succeed '()) "foo" displayln))


(define (string match)
  (λ (str cont)
    (let* ([len (min (string-length str) (string-length match))]
           [head (substring str 0 len)]
           [tail (substring str len)])
      (if (equal? match head)
          (cont (success head tail))
          (cont (failure str))))))


(check ((string "foo") "foobar" displayln))
(check ((string "foo") "bar" displayln))


(define bind
  (memo
   (λ (p fn)
     (memo
      (λ (str cont)
        (p str (λ (result)
                 (match result
                   [(success val rest)
                    ((fn val) rest cont)]
                   [failure
                    (cont failure)]))))))))

(define (seq a b)
  (bind a (λ (x)
            (bind b (λ (y)
                      (succeed (list x y)))))))


(check ((seq (string "foo")
             (string "bar"))
        "foobar"
        displayln))
(check ((seq (string "foo")
             (string "bar"))
        "barfoo"
        displayln))
(check ((seq (string "foo")
             (string "bar"))
        "foobaz"
        displayln))


(define alt
  (memo
   (λ (a b)
     (memo
      (λ (str cont)
        (a str cont)
        (b str cont))))))


(check ((alt (string "foo")
             (string "bar"))
        "foobar"
        displayln))
(check ((alt (string "foo")
             (string "bar"))
        "barfoo"
        displayln))


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


(check (noun-phrase "the student " displayln))
(check (sentence "the professor lectures the student " displayln))
(check (sentence "the professor lectures the student a lot" displayln))
(check (sentence "not a sentence " displayln))


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

(check (r "aaaaa" displayln))
(check (r "a" displayln))
(check (r "aa" displayln))
(check (r "" displayln))
(check (r "b" displayln))