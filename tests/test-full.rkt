#lang racket

(require (except-in rackunit fail)
         pprint-compact)

(check-equal?
 (pretty-format (h-append (full (text "abc"))
                          (v-append (text "")
                                    (text "def"))))
 #<<EOF
abc
   def
EOF
 )

(check-equal?
 (pretty-format (j-append (full (text "abc"))
                          (v-append (text "")
                                    (text "def"))))
 #<<EOF
abc
def
EOF
 )

(check-equal?
 (pretty-format (h-append (full (text "abc")) (h-append empty-doc empty-doc)))
 "abc")

(check-exn
 exn:fail?
 (λ () (pretty-format (h-append (full (text "abc")) (h-append empty-doc (text "a"))))))
