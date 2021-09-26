#lang racket/base

(provide memoize)

(define (memoize f #:backend [backend make-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))
