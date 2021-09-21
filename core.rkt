#lang racket/base

(provide render

         (struct-out measure)

         ;; parameters
         current-max-width
         current-indent

         ;; primitives
         :text
         :flush
         :concat
         :alternatives
         :fail
         :annotate

         ;; constructs
         text
         flush
         concat
         alternatives
         annotate

         fail)

(require racket/match
         racket/list
         racket/string
         "memoize.rkt")

(module+ test
  (require rackunit
           racket/set))

(struct :text (s) #:transparent)
(struct :alternatives (a b) #:transparent)
(struct :flush (d) #:transparent)
(struct :concat (a b) #:transparent)
(struct :fail () #:transparent)
(struct :annotate (d a) #:transparent)
#;(struct :select (d p) #:transparent)

(struct measure (width last-width height r) #:transparent)

(define current-max-width (make-parameter 80))
(define current-indent (make-parameter 0))

(define (min-by x y #:key [key values])
  (cond
    [(<= (key x) (key y)) x]
    [else y]))

(define (valid? candidate)
  (<= (measure-width candidate) (current-max-width)))

;; is x dominated by y?
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
                     (list 2 4 7)))

  (check-equal? (length (pareto (list (list 1 5 6)
                                      (list 1 5 7)
                                      (list 1 6 5)
                                      (list 1 6 5)
                                      (list 2 4 7))
                                (list first second third)))
                3))

(define (manage-candidates candidates)
  (match (filter valid? candidates)
    ['()
     (match candidates
       ['() '()]
       [(cons x xs)
        (list (for/fold ([best-candidate x])
                        ([current (in-list xs)])
                (min-by best-candidate current #:key measure-width)))])]
    [candidates
     (pareto candidates
             (list measure-width measure-last-width measure-height))]))

(define (find-optimal-layout d)
  (define render
    (memoize
     (λ (d)
       (match d
         [(:text s)
          (define len (string-length s))
          (list (measure len len 0 (λ (indent xs) (cons s xs))))]
         [(:flush d)
          (manage-candidates
           (for/list ([m (in-list (render d))])
             (match-define (measure width _ height r) m)
             (measure
              width
              0
              (add1 height)
              (λ (indent xs)
                (r indent (list* "\n" (make-string indent #\space) xs))))))]
         [(:concat a b)
          (manage-candidates
           (for*/list ([m-a (in-list (render a))] [m-b (in-list (render b))])
             (match-define (measure width-a last-width-a height-a r-a) m-a)
             (match-define (measure width-b last-width-b height-b r-b) m-b)
             (measure (max width-a (+ last-width-a width-b))
                      (+ last-width-a last-width-b)
                      (+ height-a height-b)
                      (λ (indent xs)
                        (r-a indent (r-b (+ indent last-width-a) xs))))))]
         [(:alternatives a b) (manage-candidates (append (render a) (render b)))]
         [(:annotate d _) (render d)]
         [(:fail) '()]))))
  (match (render d)
    ['() (raise-arguments-error 'render "the document fails to render")]
    [(cons x xs)
     (for/fold ([best x]) ([current (in-list xs)])
       (min-by best current #:key measure-height))]))

(define (render d)
  (string-append* ((measure-r (find-optimal-layout d)) (current-indent) '())))

(define fail (:fail))

(define text :text)

(define (flush d)
  (match d
    [(:fail) fail]
    [_ (:flush d)]))

(define (concat a b)
  (match* (a b)
    [((:fail) _) fail]
    [(_ (:fail)) fail]
    [(_ _) (:concat a b)]))

(define (alternatives a b)
  (match* (a b)
    [((:fail) _) b]
    [(_ (:fail)) a]
    [(_ _)
     (cond
       [(eq? a b) a]
       [else (:alternatives a b)])]))

(define (annotate d a)
  (match d
    [(:fail) fail]
    [_ (:annotate d a)]))
