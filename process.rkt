#lang racket/base

(provide doc-process)
(require racket/match
         "core.rkt")

(define (doc-process f doc)
  (match doc
    [(:flush d)
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (flush d*)])]
    [(:fail) fail]
    [(:text _) doc]
    [(:annotate d a)
     (define d* (f d))
     (cond
       [(eq? d* d) doc]
       [else (annotate d* a)])]
    [(:concat a b)
     (define a* (f a))
     (define b* (f b))
     (cond
       [(and (eq? a* a) (eq? b* b)) doc]
       [else (concat a* b*)])]
    [(:alternatives a b)
     (define a* (f a))
     (define b* (f b))
     (cond
       [(and (eq? a* a) (eq? b* b)) doc]
       [else (alternatives a* b*)])]))
