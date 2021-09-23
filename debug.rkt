#lang racket/base

(provide dag-size
         tree-size)

(require "process.rkt"
         "memoize.rkt")

(define (dag-size d)
  (define cnt 0)
  (define loop
    (memoize
     (λ (d)
       (set! cnt (add1 cnt))
       (doc-process loop d))))
  (loop d)
  cnt)

(define (tree-size d)
  (define cnt 0)
  (define loop
    (λ (d)
      (set! cnt (add1 cnt))
      (doc-process loop d)))
  (loop d)
  cnt)
