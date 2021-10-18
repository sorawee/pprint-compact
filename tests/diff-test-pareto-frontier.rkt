#lang racket

(require rackunit
         "../measure.rkt"
         "../pareto-frontier.rkt")

(define (dominated? a b)
  (and (>= (measure-last-width a) (measure-last-width b))
       (cond
         [(= (measure-badness a) (measure-badness b))
          (cond
            [(= (measure-height a) (measure-height b))
             (>= (measure-cost a) (measure-cost b))]
            [else
             (> (measure-height a) (measure-height b))])]
         [else (> (measure-badness a) (measure-badness b))])))

(define (naive xs)
  (let loop ([xs xs] [frontier '()])
    (match xs
      ['() frontier]
      [(cons x xs)
       (cond
         [(ormap (λ (front) (dominated? x front)) frontier) (loop xs frontier)]
         [else (loop xs (cons x (filter-not (λ (front) (dominated? front x)) frontier)))])])))

(define (gen)
  (for/list ([i (random 100)])
    (measure (random 100) (random 100) (random 100) (random 100) #f)))

(for ([i (in-range 10000)])
  (define orig (gen))
  (define xs (compute-frontier orig))
  (define ys (naive orig))

  (printf "checking ~a\n" i)
  (unless (and (equal? (length xs) (length ys))
               (equal? (list->set xs) (list->set ys)))
    (printf "orig: ~a\n" orig)
    (printf "len: ~ax\n" (length orig))
    (check-equal? (length xs) (length ys))
    (check-equal? (list->set xs) (list->set ys))))
