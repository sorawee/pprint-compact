#lang racket/base

(require "core.rkt"
         "addons.rkt")

(provide pretty-print
         pretty-format
         current-page-width
         current-page-indent
         (all-from-out "addons.rkt")
         (except-out (all-from-out "core.rkt")
                     ; we have j-append already
                     concat
                     ; we have alt already
                     alternatives
                     ; we have pretty-format already
                     render))

(define current-page-width (make-parameter 80))
(define current-page-indent (make-parameter 0))

(define (pretty-print d
                      #:out [out (current-output-port)]
                      #:width [width (current-page-width)]
                      #:indent [indent (current-page-indent)])
  (display (pretty-format d #:width width #:indent indent) out))

(define (pretty-format d
                       #:width [width (current-page-width)]
                       #:indent [indent (current-page-indent)])
  (render d width indent))
