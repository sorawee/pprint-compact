#lang racket/base

(provide memoize memoize2)

(define (memoize f #:backend [backend make-weak-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))

(define (memoize2 f #:backend [backend make-weak-hasheq])
  (define table (backend))
  (λ (x y) (hash-ref! (hash-ref! table x backend) y (λ () (f x y)))))
