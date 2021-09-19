#lang racket/base

(provide render

         ;; primitives
         text
         choice
         flush
         h-append

         ;; derivatives
         hs-append
         v-append
         v-concat
         hs-concat
         sep)

(module+ test
  (require rackunit
           racket/set))

(require racket/match
         racket/list)

(struct text (s) #:transparent)
(struct choice (a b) #:transparent)
(struct flush (d) #:transparent)
(struct h-append (a b) #:transparent)

(struct measure (width last-width height d) #:transparent)

(define current-max-width (make-parameter 80))

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
  (for/fold ([frontier '()])
            ([current (in-list xs)])
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

;; invariant: 0 <= last-width <= width
(define (render* d)
  (match d
    [(text s)
     (define len (string-length s))
     (list (measure len len 0 d))]
    [(flush d)
     (for/list ([m (in-list (render* d))])
       (match-define (measure width _ height d) m)
       (measure width 0 (add1 height) (flush d)))]
    [(h-append a b)
     (define candidates
       (for*/list ([m-a (in-list (render* a))] [m-b (in-list (render* b))])
         (match-define (measure width-a last-width-a height-a d-a) m-a)
         (match-define (measure width-b last-width-b height-b d-b) m-b)
         (measure (max width-a (+ last-width-a width-b))
                  (+ last-width-a last-width-b)
                  (+ height-a height-b)
                  (h-append d-a d-b))))
     (match (filter valid? candidates)
       ['() (list (for/fold ([best-candidate (first candidates)])
                            ([current (in-list (rest candidates))])
                    (min-by best-candidate current #:key measure-width)))]
       [candidates
        (pareto candidates
                (list measure-width measure-last-width measure-height))])]
    [(choice a b) (append (render* a) (render* b))]))

(define (render d)
  (define candidates (render* d))
  (for/fold ([best (first candidates)]) ([current (rest candidates)])
    (min-by best current #:key measure-height)))

(define empty-doc (text ""))

(define (fold-doc f xs)
  (match xs
    ['() empty-doc]
    [(list x) x]
    [(cons x xs) (f x (fold-doc f xs))]))

(define (hs-append x y)
  (h-append x (h-append (text " ") y)))

(define (v-append x y)
  (h-append (flush x) y))

(define (v-concat xs)
  (fold-doc v-append xs))

(define (hs-concat xs)
  (fold-doc hs-append xs))

(define (sep xs)
  (match xs
    ['() empty-doc]
    [xs (choice (hs-concat xs) (v-concat xs))]))

(module+ test
  (define (pretty d)
    (match d
      [(list xs ...)
       (h-append (text "(")
                 (h-append (sep (map pretty xs))
                           (text ")")))]
      [_ (text d)]))

  (define abcd '("a" "b" "c" "d"))
  (define abcd4 (list abcd abcd abcd abcd))

  (define rendered
    (parameterize ([current-max-width 20])
      (render* (pretty (list (list "abcde" abcd4)
                             (list "abcdefgh" abcd4))))))

  (check-equal? (measure-width rendered) 20)
  (check-equal? (measure-last-width rendered) 15)
  (check-equal? (measure-height rendered) 8))
