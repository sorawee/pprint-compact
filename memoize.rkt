#lang racket/base

(provide memoize)

(define (memoize f)
  (define table (make-hasheq))
  (λ args (hash-ref! table args (λ () (apply f args)))))
