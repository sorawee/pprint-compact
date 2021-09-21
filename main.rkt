#lang racket/base

(require "core.rkt"
         "addons.rkt")

(provide (all-from-out "addons.rkt")
         (except-out (all-from-out "core.rkt")
                     ; we have h-append already
                     concat
                     ; we have alt already
                     alternatives))

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
       (alt (h-append (text "(")
                      (v-concat (cons fp argsp))
                      (text ")"))
            (h-append (text "(")
                      (hs-concat (cons fp argsp))
                      (text ")"))
            (h-append (text "(")
                      fp
                      (text " ")
                      (v-concat argsp)
                      (text ")")))]
      [_ (text d)]))

  (check-equal?
   (parameterize ([current-max-width 31])
     (render (pretty '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))))
   #<<EOF
(+ (foo 1 2)
   (bar 2 3)
   (baz 3 4))
EOF
   )

  (check-equal?
   (parameterize ([current-max-width 31])
     (render (pretty* '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))))
   #<<EOF
(+ (foo 1
        2) (bar 2 3) (baz 3 4))
EOF
   )

  (check-equal?
   (parameterize ([current-max-width 15])
     (render (pretty '("+" "123" "456" "789"))))
   "(+ 123 456 789)")

  (check-equal?
   (parameterize ([current-max-width 14])
     (render (pretty '("+" "123" "456" "789"))))
   #<<EOF
(+ 123
   456
   789)
EOF
   )

  (check-equal?
   (parameterize ([current-max-width 5])
     (render (pretty '("+" "123" "456" "789"))))
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
  (display (parameterize ([current-max-width 20]
                          [current-indent (string-length prefix)])
             (render (pretty (list (list "abcde" abcd4)
                                   (list "abcdefgh" abcd4)))))
           p)
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
