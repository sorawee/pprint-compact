#lang racket/base

(provide compute-frontier)
(require racket/match
         racket/list
         "measure.rkt")

(define (measure< a b)
  (cond
    [(= (measure-last-width a) (measure-last-width b))
     (cond
       [(= (measure-badness a) (measure-badness b))
        (cond
          [(= (measure-height a) (measure-height b))
           (< (measure-cost a) (measure-cost b))]
          [else (< (measure-height a) (measure-height b))])]
       [else (< (measure-badness a) (measure-badness b))])]
    [else (< (measure-last-width a) (measure-last-width b))]))

(define (measure-projected< a b)
  (cond
    [(= (measure-badness a) (measure-badness b))
     (cond
       [(= (measure-height a) (measure-height b))
        (< (measure-cost a) (measure-cost b))]
       [else (< (measure-height a) (measure-height b))])]
    [else (< (measure-badness a) (measure-badness b))]))

(define (compute-frontier candidates)
  (match candidates
    ['() '()]
    [(list x) candidates]
    [_
     (define xs (sort candidates measure<))
     (let loop ([xs (rest xs)] [frontier (list (first xs))])
       (match xs
         ['() frontier]
         [(cons x xs)
          (define lowest (first frontier))
          (cond
            [(measure-projected< x lowest) (loop xs (cons x frontier))]
            [else (loop xs frontier)])]))]))
