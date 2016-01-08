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

(define-syntax (check-parser-single stx)
  (syntax-case stx ()
    [(_ parser input)
     #'(begin
         (newline)
         (display ">>> ")
         (displayln (~s (quote (parser input println))))
         (parser input println))]))

(define-syntax (check-parser stx)
  (syntax-case stx ()
    [(_ parser inputs ...)
     #'(begin
         (check-parser-single parser inputs) ...)]))

(define (memo fn)
  (let ([alist (mlist)])
    (λ args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ([result (apply fn args)]
                  [entry (mcons args result)])
             (set! alist (mcons entry alist))
             result)]))))

(define (memo-cps fn)
  (let ([table (mlist)])
    (define entry-continuations mcar)
    (define entry-results mcdr)
    (define (push-continuation! entry cont)
      (set-mcar! entry (mcons cont (entry-continuations entry))))
    (define (push-result! entry result)
      (set-mcdr! entry (mcons result (entry-results entry))))
    (define (result-subsumed? entry result)
      (mmember result (entry-results entry)))
    (define (make-entry)
      (mcons (mlist) (mlist)))
    (define (table-ref str)
      (match (massoc str table)
        [(mcons str entry) entry]
        [_ (let ([entry (make-entry)])
             (set! table (mcons (mcons str entry) table))
             entry)]))
    (λ (str cont)
      (let ([entry (table-ref str)])
        (match entry
          [(mcons (mlist) (mlist))
           (push-continuation! entry cont)
           (fn str (λ (result)
                     (unless (result-subsumed? entry result)
                       (push-result! entry result)
                       (for ([cont (entry-continuations entry)])
                            (cont result)))))]
          [(mcons _ _)
           (push-continuation! entry cont)
           (for ([result (entry-results entry)])
                (cont result))]
          [_ (error entry)])))))

(define-syntax-rule (delay-parser parser)
  (λ args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (delay-parser body)))


(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define succeed
  (memo
   (λ (val)
     (memo-cps
      (λ (str cont)
        (cont (success val str)))))))


(check (succeed '()))
(check-parser (succeed '()) "foo" "bar")


(define string
  (memo
   (λ (match)
     (memo-cps
      (λ (str cont)
        (let* ([len (min (string-length str) (string-length match))]
               [head (substring str 0 len)]
               [tail (substring str len)])
          (if (equal? match head)
              (cont (success head tail))
              (cont (failure str)))))))))


(check-parser (string "foo") "foobar" "bar")


(define (bind p fn)
  (λ (str cont)
        (p str (λ (result)
                 (match result
                   [(success val rest)
                    ((fn val) rest cont)]
                   [failure
                    (cont failure)])))))

(define seq
  (memo
   (λ (a b)
     (memo-cps
      (bind a
            (λ (x)
              (bind b
                    (λ (y)
                      (succeed (list x y))))))))))


(check-parser (seq (string "foo")
                   (string "bar"))
              "foobar"
              "barfoo"
              "foobaz")


(define alt
  (memo
   (λ (a b)
     (memo-cps
      (λ (str cont)
        (a str cont)
        (b str cont))))))


(check-parser (alt (string "foo")
                   (string "bar"))
              "foobar"
              "barfoo")


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


(check (noun-phrase "the student " println))
(check-parser sentence
              "the professor lectures the student "
              "the professor lectures the student a lot"
              "not a sentence ")


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

(check-parser r
             "aaaaa"
             "a"
             "aa"
             ""
             "bbb")


;; left-recursive grammar:

(define-parser s
  (alt (seq s (string "a"))
       (string "a")))

(check-parser s "aaa")