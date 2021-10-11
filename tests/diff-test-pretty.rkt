#lang racket

(require data/enumerate/lib)

(define tree/e
  (delay/e
   (or/e
    (single/e 'text)

    (list/e (single/e 'concat) tree/e tree/e)

    (list/e (single/e 'align) tree/e)

    (list/e (single/e 'alt) tree/e tree/e)

    (single/e 'nl)

    (single/e 'eol)

    (single/e 'fail)

    (list/e (single/e 'flat) tree/e))))


(define char-code-a (char->integer #\a))

(define (transform t)
  (match t
    ['text
     `(text ,(list->string
              (for/list ([i (random 20)])
                (integer->char (+ char-code-a (random 5))))))]
    [`(,op ,args ...)
     `(,op ,@(map transform args))]

    [_ t]))

(module+ main
  (for ([i #;(in-naturals) (in-range 10)])
    (define rng (make-pseudo-random-generator))
    (printf "current rng: ~a\n" (pseudo-random-generator->vector rng))
    (current-pseudo-random-generator rng)

    (define t (from-nat tree/e (random-index tree/e)))
    (writeln (transform t))))
