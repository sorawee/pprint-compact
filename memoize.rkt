#lang racket/base

(provide memoize
         memoize2)

(define (memoize f #:backend [backend make-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))

(define (memoize2 f)
  (define table (make-hash))
  (define ref (make-hasheq))
  (λ (x y)
    (define a (hash-ref! ref x gensym))
    (define b (hash-ref! ref y gensym))
    (hash-ref! table (list a b) (λ () (f x y)))))
