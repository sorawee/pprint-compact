#lang racket

(require (except-in rackunit fail)
         pprint-compact)

(check-equal?
 (pretty-format (h-append (text "ab")
                          (h-append (text "cd")
                                    (v-append (text "1")
                                              (text "12")
                                              (text "123"))))
                #:width 0)
 #<<EOF
abcd1
    12
    123
EOF
 )

(check-equal?
 (pretty-format (h-append (text "ab")
                          (h-append (text "cd")
                                    (v-append (text "1")
                                              (text "12")
                                              (text "123"))))
                #:width 2
                #:indent 5)
 #<<EOF
abcd1
         12
         123
EOF
 )
