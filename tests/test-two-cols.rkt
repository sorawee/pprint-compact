#lang racket

(require (except-in rackunit fail)
         pprint-compact)

(define (format-bindings)
  (two-cols
   (list (list (v-append (h-append (text "[")
                                   (sep (list (text "mutable") (text "empty")))))
               (text "Doc]"))
         (list (text "[nest") (text "Int -> Doc -> Doc]"))
         (list (text "[linebreak") (text "Doc]")))))

(define doc
  (h-append (text "test: ")
            (h-append (text "(")
                      (v-append (h-append (text "let (")
                                          (format-bindings)
                                          (text ")"))
                                (h-append (text " ")
                                          (text "(void)")))
                      (text ")"))))

(check-equal?
 (pretty-format doc #:width 45)
 #<<EOF
test: (let ([mutable empty Doc]
            [nest          Int -> Doc -> Doc]
            [linebreak     Doc])
        (void))
EOF
 )

(check-equal?
 (pretty-format doc #:width 44)
 #<<EOF
test: (let ([mutable empty Doc]
            [nest      Int -> Doc -> Doc]
            [linebreak Doc])
        (void))
EOF
 )



(check-equal?
 (pretty-format doc #:width 31)
 #<<EOF
test: (let ([mutable empty Doc]
            [nest Int -> Doc -> Doc]
            [linebreak Doc])
        (void))
EOF
 )

(check-equal?
 (pretty-format doc #:width 30)
 #<<EOF
test: (let ([mutable
             empty     Doc]
            [nest Int -> Doc -> Doc]
            [linebreak Doc])
        (void))
EOF
 )

(check-equal?
 (pretty-format doc #:width 26)
 #<<EOF
test: (let ([mutable
             empty Doc]
            [nest Int -> Doc -> Doc]
            [linebreak Doc])
        (void))
EOF
 )
