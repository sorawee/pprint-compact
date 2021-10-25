#lang racket/base

(provide (struct-out measure))

(struct measure (badness width last-width height cost r) #:transparent)
