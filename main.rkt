#lang racket/base

(require "core.rkt"
         "addons.rkt")

(provide pretty-print
         pretty-format
         current-page-width
         current-page-indent
         (all-from-out "addons.rkt")
         (except-out (all-from-out "core.rkt")
                     ; we have h-append already
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

(module+ test
  (require racket/match
           rackunit)
  (define (pretty d)
    (match d
      [(list) (text "()")]
      [(list f args ...)
       (define fp (pretty f))
       (define argsp (map pretty args))
       (alt (h-append lparen
                      (v-concat (cons fp argsp))
                      rparen)
            (flat
             (h-append lparen
                       (hs-concat (cons fp argsp))
                       rparen))
            (h-append lparen
                      fp
                      space
                      (v-concat argsp)
                      rparen))]
      [_ (text d)]))

  (define (pretty* d)
    (match d
      [(list) (text "()")]
      [(list f args ...)
       (define fp (pretty* f))
       (define argsp (map pretty* args))
       (alt (h-append lparen
                      (v-concat (cons fp argsp))
                      rparen)
            (h-append lparen
                      (hs-concat (cons fp argsp))
                      rparen)
            (h-append lparen
                      fp
                      space
                      (v-concat argsp)
                      rparen))]
      [_ (text d)]))

  (check-equal?
   (pretty-format (pretty '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))
                  #:width 31)
   #<<EOF
(+ (foo 1 2)
   (bar 2 3)
   (baz 3 4))
EOF
   )

  (check-equal?
   (pretty-format (pretty* '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))
                  #:width 31)
   #<<EOF
(+ (foo 1
        2) (bar 2 3) (baz 3 4))
EOF
   )

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:width 15)
   "(+ 123 456 789)")

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:width 14)
   #<<EOF
(+ 123
   456
   789)
EOF
   )

  (check-equal?
   (pretty-format (pretty '("+" "123" "456" "789")) #:width 5)
   #<<EOF
(+
 123
 456
 789)
EOF
   )

  (define abcd '("a" "b" "c" "d"))
  (define abcd4 (list abcd abcd abcd abcd))

  (define p (open-output-string))
  (define prefix "hello: ")
  (display prefix p)
  (pretty-print (pretty (list (list "abcde" abcd4)
                              (list "abcdefgh" abcd4)))
                #:out p
                #:width 20
                #:indent (string-length prefix))
  (check-equal? (get-output-string p)
                #<<EOF
hello: ((abcde ((a b c d)
                (a b c d)
                (a b c d)
                (a b c d)))
        (abcdefgh
         ((a b c d)
          (a b c d)
          (a b c d)
          (a b c d))))
EOF
                ))
