#lang racket/base

(provide memoize)

(define (memoize f)
  (define table (make-hasheq))
  (λ (x) (hash-ref! table x (λ () (f x)))))
