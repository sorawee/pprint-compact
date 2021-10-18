#lang at-exp racket

(require (except-in rackunit fail)
         pprint-compact)

(define f (j-append (h-append (text "function(")
                              (v-append (text "a,")
                                        (text "b,")
                                        (text "c)")))
                    (v-append (text "{")
                              (h-append (text "  ") (v-append (text "print();")
                                                              (text "print();")))
                              (text "}"))))

(check-equal? (pretty-format (j-append (text "function()") (v-append (text "{")
                                                                      (h-append (text "  ") (v-append f
                                                                                                      f))
                                                                      (text "}"))))
              #<<EOF
function(){
  function(a,
           b,
           c){
    print();
    print();
  }
  function(a,
           b,
           c){
    print();
    print();
  }
}
EOF
              )

(check-equal?
 (pretty-format (j-append (text "a")
                          (j-append (text "b")
                                    (j-append (align (v-append (text "x") (text "y")))
                                              (align (v-append (text "p") (text "q")))))))
 #<<EOF
abx
  yp
   q
EOF
 )
