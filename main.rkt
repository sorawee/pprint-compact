#lang racket/base

(provide render

         ;; parameters
         current-max-width
         current-indent

         ;; constructs
         text
         flush

         alt

         h-append
         hs-append
         v-append

         h-concat
         hs-concat
         v-concat

         empty-doc

         sep)

(module+ test
  (require rackunit
           racket/set))

(require racket/match
         racket/list
         racket/string)

(struct text (s) #:transparent)
(struct alternatives (a b) #:transparent)
(struct flush (d) #:transparent)
(struct concat (a b) #:transparent)

(struct measure (width last-width height r) #:transparent)

(define current-max-width (make-parameter 80))
(define current-indent (make-parameter 0))

(define (min-by x y #:key [key values])
  (cond
    [(<= (key x) (key y)) x]
    [else y]))

(define (valid? candidate)
  (<= (measure-width candidate) (current-max-width)))

;; x is dominated by y?
(define ((dominated? objectives) x y)
  (andmap (λ (objective) (>= (objective x) (objective y))) objectives))

(define (pareto xs objectives)
  (define domed? (dominated? objectives))
  (for/fold ([frontier '()]) ([current (in-list xs)])
    (cond
      [(ormap (λ (front) (domed? current front)) frontier) frontier]
      [else
       (cons current
             (filter-not (λ (front) (domed? front current)) frontier))])))

(module+ test
  (check-equal? (list->set (pareto (list (list 1 5 6)
                                         (list 1 5 7)
                                         (list 1 6 5)
                                         (list 2 4 7))
                                   (list first second third)))
                (set (list 1 5 6)
                     (list 1 6 5)
                     (list 2 4 7))))

(define (memoize f)
  (define table (make-hash))
  (λ args (hash-ref! table args (λ () (apply f args)))))

;; invariant: 0 <= last-width <= width
(define render*
  (memoize
   (λ (d)
     (match d
       [(text s)
        (define len (string-length s))
        (list (measure len len 0 (λ (indent xs) (cons s xs))))]
       [(flush d)
        (for/list ([m (in-list (render* d))])
          (match-define (measure width _ height r) m)
          (measure width
                   0
                   (add1 height)
                   (λ (indent xs)
                     (r indent (list* "\n" (make-string indent #\space) xs)))))]
       [(concat a b)
        (define candidates
          (for*/list ([m-a (in-list (render* a))] [m-b (in-list (render* b))])
            (match-define (measure width-a last-width-a height-a r-a) m-a)
            (match-define (measure width-b last-width-b height-b r-b) m-b)
            (measure (max width-a (+ last-width-a width-b))
                     (+ last-width-a last-width-b)
                     (+ height-a height-b)
                     (λ (indent xs)
                       (r-a indent (r-b (+ indent last-width-a) xs))))))
        (match (filter valid? candidates)
          ['() (list (for/fold ([best-candidate (first candidates)])
                               ([current (in-list (rest candidates))])
                       (min-by best-candidate current #:key measure-width)))]
          [candidates
           (pareto candidates
                   (list measure-width measure-last-width measure-height))])]
       [(alternatives a b) (append (render* a) (render* b))]))))

(define (find-optimal-layout d)
  (define candidates (render* d))
  (for/fold ([best (first candidates)]) ([current (rest candidates)])
    (min-by best current #:key measure-height)))

(define (render d)
  (string-append* ((measure-r (find-optimal-layout d)) (current-indent) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derivative constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alt x . xs)
  (for/fold ([current x]) ([x (in-list xs)])
    (alternatives current x)))

(define empty-doc (text ""))

(define (fold-doc f xs)
  (match xs
    ['() empty-doc]
    [(list x) x]
    [(cons x xs) (f x (fold-doc f xs))]))

(define (h-concat xs)
  (fold-doc concat xs))

(define (h-append . xs)
  (h-concat xs))

(define (hs-append/bin x y)
  (h-append x (text " ") y))

(define (v-append/bin x y)
  (h-append (flush x) y))

(define (v-concat xs)
  (fold-doc v-append/bin xs))

(define (hs-concat xs)
  (fold-doc hs-append/bin xs))

(define (hs-append . xs)
  (hs-concat xs))

(define (v-append . xs)
  (v-concat xs))

(define (sep xs)
  (match xs
    ['() empty-doc]
    [xs (alt (hs-concat xs) (v-concat xs))]))

(module+ test
  (define (pretty d)
    (match d
      [(list xs ...)
       (h-append (text "(")
                 (sep (map pretty xs))
                 (text ")"))]
      [_ (text d)]))

  (define abcd '("a" "b" "c" "d"))
  (define abcd4 (list abcd abcd abcd abcd))

  (define prettied
    (pretty (list (list "abcde" abcd4)
                  (list "abcdefgh" abcd4))))

  (define rendered
    (parameterize ([current-max-width 20])
      (find-optimal-layout prettied)))

  (check-equal? (measure-width rendered) 20)
  (check-equal? (measure-last-width rendered) 15)
  (check-equal? (measure-height rendered) 8)

  (define p (open-output-string))
  (define prefix "hello: ")
  (display prefix p)
  (display (parameterize ([current-max-width 20]
                          [current-indent (string-length prefix)])
             (render prettied))
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
