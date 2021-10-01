#lang racket/base

(provide memoize)

(define (memoize f #:backend [backend make-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))

(module+ private
  (provide memoize memoize2)
  (define (memoize2 n f)
    (define table (build-vector (add1 n) (λ (i) (make-hasheq))))
    (λ (x y)
      (hash-ref! (vector-ref table y) x (λ () (f x y))))))
