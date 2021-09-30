#lang racket/base

(provide render

         (struct-out measure)

         ;; predicate
         doc?

         ;; pattern expanders
         :text
         :flush
         :concat
         :alternatives
         :full
         :fail
         :annotate
         :select

         ;; primitive constructors
         text
         flush
         concat
         alternatives
         full
         annotate
         select

         fail)

(require racket/match
         racket/list
         racket/string
         "memoize.rkt")

(module+ test
  (require rackunit
           racket/set))

(struct doc () #:transparent)
(struct :text doc (s) #:transparent #:constructor-name text)
(struct :alternatives doc (a b) #:transparent #:constructor-name make-alternatives)
(struct :flush doc (d) #:transparent #:constructor-name make-flush)
(struct :concat doc (a b) #:transparent #:constructor-name make-concat)
(struct :full doc (d) #:transparent #:constructor-name make-full)
(struct :fail doc () #:transparent #:constructor-name make-fail)
(struct :annotate doc (d a) #:transparent #:constructor-name make-annotate)
(struct :select doc (d p) #:transparent #:constructor-name make-select)

(struct measure (width badness last-width height r) #:transparent)

(define (min-by x y #:key [key values])
  (cond
    [(<= (key x) (key y)) x]
    [else y]))

(define (mins-by xs #:key [key values])
  (for/fold ([best (list (first xs))]) ([x (in-list (rest xs))])
    (cond
      [(= (key (first best)) (key x)) (cons x best)]
      [(< (key (first best)) (key x)) best]
      [else (list x)])))

;; is x dominated by y?
(define ((dominated? objectives) x y)
  (andmap (λ (objective) (>= (objective x) (objective y))) objectives))

(define ((lexicographic-order< objectives) x y)
  (let loop ([objectives objectives])
    (match objectives
      [(cons objective objectives)
       (cond
         [(= (objective x) (objective y)) (loop objectives)]
         [else (< (objective x) (objective y))])]
      [_ #f])))

(define (pareto xs objectives)
  (define domed? (dominated? objectives))
  (define xs* (sort xs (lexicographic-order< objectives)))
  (for/fold ([frontier '()]) ([current (in-list xs*)])
    (cond
      [(ormap (λ (front) (domed? current front)) frontier) frontier]
      [else (cons current frontier)])))

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

;; precondition: all badness are equal
(define (compute-pareto candidates)
  (pareto candidates
          (list measure-width measure-last-width measure-height)))

(define (find-optimal-layout d max-width)
  (define ((valid? best-candidate) candidate)
    (= (measure-badness best-candidate) (measure-badness candidate)))

  (define (manage-candidates candidates)
    (match candidates
      ['() '()]
      [_ (compute-pareto (mins-by candidates #:key measure-badness))]))

  (define render
    (memoize2
     (λ (d width-limit)
       (match d
         [(:text s)
          (define len (string-length s))
          (cons (list (measure len (max 0 (- len width-limit)) len 0 (λ (indent xs) (cons s xs))))
                '())]
         [(:full d)
          (match-define (cons as bs) (render d width-limit))
          (cons '() (manage-candidates (append as bs)))]
         [(:flush d)
          (match-define (cons as bs) (render d width-limit))
          (cons
           (manage-candidates
            (for/list ([m (in-sequences (in-list as) (in-list bs))])
              (match-define (measure width badness _ height r) m)
              (measure
               width
               badness
               0
               (add1 height)
               (λ (indent xs)
                 (r indent (list* "\n" (make-string indent #\space) xs))))))
           '())]
         [(:concat a b)
          (match-define (cons a/no-req _) (render a width-limit))
          (define zs
            (for/list ([m-a (in-list a/no-req)])
              (match-define (measure width-a badness-a last-width-a height-a r-a) m-a)
              (define (proceed bs)
                (for/list ([m-b (in-list bs)])
                  (match-define (measure width-b badness-b last-width-b height-b r-b) m-b)
                  (measure (max width-a (+ last-width-a width-b))
                           (+ badness-a badness-b)
                           (+ last-width-a last-width-b)
                           (+ height-a height-b)
                           (λ (indent xs)
                             (r-a indent (r-b (+ indent last-width-a) xs))))))
              (match-define (cons b/no-req b/req) (render b (- width-limit last-width-a)))
              (cons (proceed b/no-req) (proceed b/req))))
          (cons
           (manage-candidates (append* (map car zs)))
           (manage-candidates (append* (map cdr zs))))]

         [(:alternatives a b)
          (match-define (cons a/no-req a/req) (render a width-limit))
          (match-define (cons b/no-req b/req) (render b width-limit))
          (cons (manage-candidates (append a/no-req b/no-req))
                (manage-candidates (append a/req b/req)))]
         [(:annotate d _) (render d width-limit)]
         [(:select d p)
          (match-define (cons as bs) (render d width-limit))
          (cons (filter p as) (filter p bs))]
         [(:fail) (cons '() '())]))))
  (match-define (cons as bs) (render d max-width))
  (match (manage-candidates (append as bs))
    ['() (raise-arguments-error 'render "the document fails to render")]
    [(cons x xs)
     (for/fold ([best x]) ([current (in-list xs)])
       (min-by best current #:key measure-height))]))

(define (render d width indent)
  (string-append* ((measure-r (find-optimal-layout d width)) indent '())))

;; perform partial evaluation on the "constructor"s

(define fail (make-fail))

(define (flush d)
  (match d
    [(:fail) fail]
    [(:full d) (flush d)]
    [_ (make-flush d)]))

(define (concat a b)
  (match* (a b)
    [((:full _) _) fail]
    [(a (:full b)) (full (concat a b))]
    [((:fail) _) fail]
    [(_ (:fail)) fail]
    [((:text "") d) d]
    [(d (:text "")) d]
    [((:text sa) (:text sb)) (text (string-append sa sb))]
    [(_ _) (make-concat a b)]))

(define (alternatives a b)
  (match* (a b)
    [((:fail) _) b]
    [(_ (:fail)) a]
    [(_ _)
     (cond
       [(eq? a b) a]
       [else (make-alternatives a b)])]))

(define (annotate d a)
  (match d
    [(:fail) fail]
    [(:annotate d a*)
     (cond
       [(equal? a a*) d]
       [(make-annotate d a)])]
    [_ (make-annotate d a)]))

(define (select d p)
  (match d
    [(:fail) fail]
    [_ (make-select d p)]))

(define (full d)
  (match d
    [(:full _) d]
    [(:fail) fail]
    [_ (make-full d)]))
