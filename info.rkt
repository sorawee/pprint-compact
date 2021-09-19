#lang info
(define collection "pprint-compact")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pprint-compact.scrbl" ())))
(define pkg-desc "An implementation of 'A Pretty But Not Greedy Printer'")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
