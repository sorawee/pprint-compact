#lang racket/base

(provide empty-doc
         space
         lparen
         rparen

         alt
         h-concat
         h-append
         hs-concat
         hs-append
         v-concat
         v-append
         sep

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

(define (h-concat xs)
  (fold-doc concat xs))

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
  (h-append (flush x) y))

(define (v-concat xs)
  (fold-doc v-append/bin xs))

(define (v-append . xs)
  (v-concat xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sep xs)
  (match xs
    ['() empty-doc]
    [xs (alt (hs-concat xs) (v-concat xs))]))

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
         [(:annotate _ 'flat) d]
         [_ (doc-process loop d)]))))
  (annotate (loop d) 'flat))

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
