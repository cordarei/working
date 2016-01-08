#lang racket
;; https://github.com/epsil/gll/blob/master/README.md

(require racket/mpair)


(define print-stream-max-length (make-parameter 3))
(define (print-stream stream)
  (let rec ([stream stream]
            [string ""]
            [n (print-stream-max-length)])
    (cond
      [(= (stream-length stream) 0)
       (display string)]
      [(= n 0)
       (display string)
       (display "...")]
      [else
       (rec (stream-rest stream)
            (string-append string " " (~s (stream-first stream)))
            (- n 1))])))



(define (memo fn)
  (let ([alist (mlist)])
    (λ args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ([result (apply fn args)]
                  [entry (mcons args result)])
             (set! alist (mcons entry alist))
             result)]))))

;; trampoline

(define trampoline%
  (class object%
    (super-new)
    (define stack (mlist))
    (define table (mlist))

    (define/public (get-stack) stack)
    (define/public (get-table) table)

    (define/public (has-next?) (not (empty? stack)))

    (define/public (push-stack fn . args)
      (let ([call (mcons fn args)])
        (set! stack (mcons call stack))))

    (define/public (step)
      (match stack
        [(mcons (mcons fn args) tail)
         (set! stack tail)
         (apply fn args)]
        [_ (void)]))
    
    (define/public (run)
      (do () ((empty? stack))
        (step)))

    (define/public (push fn str cont)
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
      (define (table-ref fn str)
        (let ([pair (massoc fn table)])
          (match pair
            [(mcons fn memo)
             (match (massoc str memo)
               [(mcons str entry) entry]
               [_ (let ([entry (make-entry)])
                    (set-mcdr! pair (mcons (mcons str entry) memo))
                    entry)])]
            [_ (let* ([entry (make-entry)]
                      [memo (mlist (mcons str entry))])
                 (set! table (mcons (mcons fn memo) table))
                 entry)])))
      (let ([entry (table-ref fn str)])
        (match entry
          [(mcons (mlist) (mlist))
           (push-continuation! entry cont)
           (push-stack fn str this
                       (λ (result)
                         (unless (result-subsumed? entry result)
                           (push-result! entry result)
                           (for ([cont (entry-continuations entry)])
                                (cont result)))))]
          [_
           (push-continuation! entry cont)
           (for ([result (entry-results entry)])
                (cont result))])))
    ))



(define-syntax-rule (make-stream body ...)
  (stream-rest
   (stream-cons '() (begin body ...))))

(define (run-parser parser str)
  (let ([tramp (new trampoline%)]
        [results '()])
    (define (compute)
      (when (send tramp has-next?)
        (do () ((not (and (empty? results)
                          (send tramp has-next?))))
          (send tramp step)))
      (stream))
    (define (stream)
      (let ([result (sequence->stream results)])
        (set! results '())
        (if (send tramp has-next?)
            (stream-append result (make-stream (compute)))
            result)))
    (make-stream
     (parser str tramp
             (λ (result)
               (match result
                 [(success val "")
                  (set! results (cons result results))]
                 [failure failure])))
     (compute))))

(define (make-parser parser)
  (λ (str (tramp #f) (cont #f))
    (if (and cont tramp)
        (parser str tramp cont)
        (run-parser parser str))))


(define-syntax-rule (delay-parser parser)
  (λ args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (make-parser (delay-parser body))))


(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define succeed
  (memo
   (λ (val)
     (λ (str tramp cont)
        (cont (success val str))))))


;(check (succeed '()))
;(check-parser (succeed '()) "foo" "bar")


(define string
  (memo
   (λ (match)
     (λ (str tramp cont)
        (let* ([len (min (string-length str) (string-length match))]
               [head (substring str 0 len)]
               [tail (substring str len)])
          (if (equal? match head)
              (cont (success head tail))
              (cont (failure str))))))))


;(check-parser (string "foo") "foobar" "bar")


(define regexp
  (memo
   (λ (pattern)
     (λ (str tramp cont)
       (match (regexp-match-positions (string-append "^" pattern) str)
         [(cons (cons beg end) _)
          (let* ([len (string-length str)]
                 [head (substring str beg end)]
                 [tail (substring str end len)])
            (cont (success head tail)))]
         [_ (cont (failure str))])))))


(define (bind p fn)
  (λ (str tramp cont)
    (p str tramp
       (λ (result)
         (match result
           [(success val rest)
            ((fn val) rest tramp cont)]
           [failure
            (cont failure)])))))

(define seq
  (memo
   (λ parsers
     (define (seq2 b a)
       (bind a
            (λ (x)
              (bind b
                    (λ (y)
                      (succeed (append x (list y))))))))
     (foldl seq2 (succeed '()) parsers)
     )))


;(check-parser (seq (string "foo")
;                   (string "bar"))
;              "foobar"
;              "barfoo"
;              "foobaz")


(define alt
  (memo
   (λ parsers
     (λ (str tramp cont)
        (for ([fn parsers])
             (send tramp push fn str cont))))))


;(check-parser (alt (string "foo")
;                   (string "bar"))
;              "foobar"
;              "barfoo")


(define red
  (memo
   (λ (p fn)
     (bind p
           (λ (val)
             (match val
               [(list val ...)
                (succeed (apply fn val))]
               [_ (succeed (fn val))]))))))


(define article
  (alt (string "the ")
       (string "a ")))

(define noun
  (alt (string "student ")
       (string "professor ")))

(define verb
  (alt (string "studies ")
       (string "lectures ")))

(define-parser noun-phrase
  (seq article noun))

(define verb-phrase
  (seq verb noun-phrase))

(define sentence
  (seq noun-phrase verb-phrase))


;(check (noun-phrase "the student " println))
;(check-parser sentence
;              "the professor lectures the student "
;              "the professor lectures the student a lot"
;              "not a sentence ")


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

;(check-parser r
;             "aaaaa"
;             "a"
;             "aa"
;             ""
;             "bbb")


;; left-recursive grammar:

(define-parser s
  (alt (seq s (string "a"))
       (string "a")))

;(check-parser s "aaa")


(define (op o) (λ (x _ y) (o x y)))
(define-parser expr
  (alt (red (seq expr (string "+") term)
            (op +))
       (red (seq expr (string "-") term)
            (op -))
       term))

(define-parser term
  (alt (red (seq term (string "*") factor)
            (op *))
       (red (seq term (string "/") factor)
            (op /))
       factor))

(define-parser factor
  (alt (red (seq (string "(") expr (string ")"))
            (λ (_ x __) x))
       num))

(define-parser num
  (red (regexp "[0-9]+")
       string->number))

(define (do-parse parser str) (for ([res (in-stream (run-parser parser str))])
                                   (println res)))