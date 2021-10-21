#lang racket/base

(provide (struct-out measure))

(struct measure (badness last-width height cost r) #:transparent)
