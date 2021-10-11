#lang at-exp racket

(require racket/match
         racket/string
         (except-in rackunit fail)
         pprint-compact
         "test-util.rkt")

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
 @out{(+ (foo 1 2)
         (bar 2 3)
         (baz 3 4))})


(check-equal?
 (get-dim
  (pretty-format (pretty* '("+" ("foo" "1" "2") ("bar" "2" "3") ("baz" "3" "4")))
                 #:width 31))
 (get-dim
  @out{(+ (foo 1
               2) (bar 2 3) (baz 3 4))}))

(check-equal?
 (pretty-format (pretty '("+" "123" "456" "789")) #:width 15)
 "(+ 123 456 789)")

(check-equal?
 (pretty-format (pretty '("+" "123" "456" "789")) #:width 14)
 @out{(+ 123
         456
         789)})

(check-equal?
 (pretty-format (pretty '("+" "123" "456" "789")) #:width 5)
 @out{(+
       123
       456
       789)})

(define abcd '("a" "b" "c" "d"))
(define abcd4 (list abcd abcd abcd abcd))

(define p (open-output-string))
(define prefix "hello: ")
(display prefix p)
(pretty-print (pretty (list (list "abcde" abcd4)
                            (list "abcdefgh" abcd4)))
              #:out p
              #:width (+ 20 (string-length prefix))
              #:indent (string-length prefix))
(check-equal? (get-output-string p)
              @out{hello: ((abcde ((a b c d)
                                   (a b c d)
                                   (a b c d)
                                   (a b c d)))
                           (abcdefgh
                            ((a b c d)
                             (a b c d)
                             (a b c d)
                             (a b c d))))})
