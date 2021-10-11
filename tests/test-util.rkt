#lang racket

(provide get-dim out)

(define (get-dim s)
  (define ss (string-split s "\n"))
  (cons (length ss) (apply max (map string-length ss))))

(define out string-append)
