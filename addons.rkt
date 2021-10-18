#lang racket/base

(provide empty-doc
         space
         lparen
         rparen

         alt
         j-concat
         j-append
         h-concat
         h-append
         hs-concat
         hs-append
         v-concat
         v-append
         sep
         sep/flat
         flow
         flow*
         two-cols

         flush-if
         indent-next

         flat
         flat*)
(require racket/match
         "core.rkt"
         "process.rkt"
         "memoize.rkt")

(define empty-doc (text ""))

(define space (text " "))
(define lparen (text "("))
(define rparen (text ")"))

(define (alt . xs)
  (for/fold ([current fail]) ([x (in-list xs)])
    (alternatives current x)))

(define (fold-doc f xs)
  (match xs
    ['() empty-doc]
    [(cons x xs) (for/fold ([current x]) ([x (in-list xs)])
                   (f current x))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (j-concat xs)
  (fold-doc concat xs))

(define (j-append . xs)
  (j-concat xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (h-append/bin a b)
  (j-append a (align b)))

(define (h-concat xs)
  (fold-doc h-append/bin xs))

(define (h-append . xs)
  (h-concat xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hs-append/bin x y)
  (h-append x space y))

(define (hs-concat xs)
  (fold-doc hs-append/bin xs))

(define (hs-append . xs)
  (hs-concat xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (v-append/bin x y)
  (j-append (flush x) y))

(define (v-concat xs)
  (fold-doc v-append/bin xs))

(define (v-append . xs)
  (v-concat xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sep xs)
  (match xs
    ['() empty-doc]
    [xs (alt (hs-concat xs) (v-concat xs))]))

(define (sep/flat xs)
  (match xs
    ['() empty-doc]
    [xs (alt (flat (hs-concat xs)) (v-concat xs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flow xs
              #:allow-first-newline? [allow-first-newline? #t]
              #:sep [sep space])
  (match xs
    ['() empty-doc]
    [_
     (define flowed
       (let loop ([xs xs])
         (match xs
           [(list x) x]
           [(cons x xs)
            (define flowed (loop xs))
            (alt (v-append x flowed)
                 (j-append x sep flowed))])))
     (cond
       [allow-first-newline?
        (alt flowed (v-append empty-doc flowed))]
       [else flowed])]))

(define (flow* xs
               #:allow-first-newline? [allow-first-newline? #t]
               #:sep [sep space])
  (match xs
    ['() empty-doc]
    [(cons result xs)
     (define flowed
       (let loop ([xs xs] [result result])
         (match xs
           ['() result]
           [(cons x xs)
            (loop xs
                  (alt (v-append result x)
                       (h-append result sep x)))])))
     (cond
       [allow-first-newline?
        (alt flowed (v-append empty-doc flowed))]
       [else flowed])]))

(define (two-cols rows #:sep [sep space])
  (align
   (context
    (memoize*
     (λ (width-limit first-limit full?)
       (apply alt
              (for/list ([offset (in-inclusive-range 0 first-limit)])
                (cost
                 (v-concat
                  (for/list ([row (in-list rows)])
                    (match row
                      [(list a b)
                       (h-append a
                                 (alt
                                  (context
                                   (memoize*
                                    (λ (width-limit* first-limit* full?*)
                                      (define width (- first-limit first-limit*))
                                      (cond
                                        [(< offset width) fail]
                                        [else (text (make-string (- offset width) #\space))]))))
                                  (cost empty-doc (add1 first-limit)))
                                 sep
                                 b)])))
                 offset))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flat d)
  (define loop
    (memoize
     (λ (d)
       (match d
         [(:flush _) fail]
         ;; we can actually prune the annotation since the whole thing is tagged
         ;; as flat anyway, but leaving things in-place to preserve reference
         ;; is more worthwhile
         #;[(:annotate _ 'flat) d]
         [_ (doc-process loop d)]))))
  #;(annotate (loop d) 'flat)
  (loop d))

(define (flat? m)
  (zero? (measure-height m)))

(define (flat* d)
  (select d flat?))

(define (flush-if b d)
  (if b
      (flush d)
      d))

(define space-caches (make-hash))

(define (indent-next n d)
  (v-append d (hash-ref! space-caches n (λ () (text (make-string n #\space))))))
