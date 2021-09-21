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

         flat)
(require racket/match
         "core.rkt"
         "memoize.rkt")

(define empty-doc (text ""))

(define space (text " "))
(define lparen (text "("))
(define rparen (text ")"))

(define (alt x . xs)
  (for/fold ([current x]) ([x (in-list xs)])
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

(define (flat d)
  (define loop
    (memoize
     (λ (d)
       (match d
         [(:flush _) fail]
         [(:fail) d]
         [(:text _) d]
         [(:annotate _ 'flat) d]
         [(:annotate doc a)
          (define doc* (loop doc))
          (cond
            [(eq? doc* doc) d]
            [else (annotate doc* a)])]
         [(:concat a b)
          (define a* (loop a))
          (define b* (loop b))
          (cond
            [(and (eq? a* a) (eq? b* b)) d]
            [else (concat a* b*)])]
         [(:alternatives a b)
          (define a* (loop a))
          (define b* (loop b))
          (cond
            [(and (eq? a* a) (eq? b* b)) d]
            [else (alternatives a* b*)])]))))
  (annotate (loop d) 'flat))
