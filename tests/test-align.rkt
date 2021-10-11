#lang racket

(require (except-in rackunit fail)
         pprint-compact)

(check-equal?
 (pretty-format (h-append (j-append (text "abcdef")
                                    (v-append (text "")
                                              (text "ghi")))
                          (v-append (text "1")
                                    (text "2")
                                    (text "3"))))
 #<<EOF
abcdef
ghi1
   2
   3
EOF
 )
