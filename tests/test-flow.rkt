#lang racket

(require (except-in rackunit fail)
         pprint-compact)

(define game-list
  (list (text "Splatoon,")
        (text "Breath")
        (text "of")
        (text "the")
        (text "Wild,")
        (text "Octo")
        (text "Expansion,")
        (text "and")
        (text "Hades")))

(define (generate-flow allow?)
  (h-append (flow game-list #:allow-first-newline? allow?)
            (text " are great!")))

(check-equal?
 (pretty-format (h-append (text "Games:") (generate-flow #f)) #:width 20)
 #<<EOF
Games:Splatoon,
      Breath of
      the Wild, Octo
      Expansion, and
      Hades are great!
EOF
 )

(check-equal?
 (pretty-format (h-append (text "Games:") (h-append (flow* game-list)
                                                    (text " are great!"))) #:width 20)
 #<<EOF
Games:Splatoon,
      Breath of the
      Wild, Octo
      Expansion, and
      Hades are great!
EOF
 )

(check-equal?
 (pretty-format (j-append (text "Games:") (generate-flow #f)) #:width 14)
 #<<EOF
Games:Splatoon,
Breath of
the Wild, Octo
Expansion, and
Hades are great!
EOF
 )

(check-equal?
 (pretty-format (j-append (text "Games:") (generate-flow #t)) #:width 14)
 #<<EOF
Games:
Splatoon,
Breath of
the Wild, Octo
Expansion, and
Hades are great!
EOF
 )
