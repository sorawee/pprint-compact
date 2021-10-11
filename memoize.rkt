#lang racket/base

(provide memoize memoize*)

(define (memoize f #:backend [backend make-weak-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))

(define (memoize* f #:backend [backend make-weak-hasheq])
  (define table-full (backend))
  (define table-not-full (backend))
  (λ (d width-limit first-limit full?)
    (hash-ref! (hash-ref! (hash-ref! (if full? table-full table-not-full)
                                     d backend)
                          width-limit backend)
               first-limit (λ () (f d width-limit first-limit full?)))))
