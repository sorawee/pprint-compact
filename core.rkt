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
         :worsen

         ;; primitive constructors
         text
         flush
         concat
         alternatives
         full
         annotate
         select
         worsen

         fail)

(require racket/match
         racket/list
         racket/string
         "measure.rkt"
         "pareto-frontier.rkt"
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
(struct :worsen doc (d n) #:transparent #:constructor-name make-worsen)

(define (mins-by xs #:key [key values])
  (for/fold ([best (list (first xs))]) ([x (in-list (rest xs))])
    (cond
      [(= (key (first best)) (key x)) (cons x best)]
      [(< (key (first best)) (key x)) best]
      [else (list x)])))

(define (find-optimal-layout d max-width)
  (define render
    (memoize2
     (λ (d width-limit)
       (match d
         [(:text s)
          (define len (string-length s))
          (cons (list (measure (max 0 (- len width-limit)) len 0 (λ (indent xs) (cons s xs))))
                '())]
         [(:full d)
          (match-define (cons as bs) (render d width-limit))
          (cons '() (compute-frontier (append as bs)))]
         [(:flush d)
          (match-define (cons as bs) (render d width-limit))
          (cons
           (compute-frontier
            (for/list ([m (in-sequences (in-list as) (in-list bs))])
              (match-define (measure badness _ height r) m)
              (measure
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
              (match-define (measure badness-a last-width-a height-a r-a) m-a)
              (define penalty-multiplier (max 0 (- last-width-a width-limit)))
              (define remaining-width (max 0 (- width-limit last-width-a)))
              (define (proceed bs)
                (for/list ([m-b (in-list bs)])
                  (match-define (measure badness-b last-width-b height-b r-b) m-b)
                  (measure (+ badness-a badness-b (* (add1 height-b) penalty-multiplier))
                           (+ last-width-a last-width-b)
                           (+ height-a height-b)
                           (λ (indent xs)
                             (r-a indent (r-b (+ indent last-width-a) xs))))))
              (match-define (cons b/no-req b/req) (render b remaining-width))
              (cons (proceed b/no-req) (proceed b/req))))
          (cons
           (compute-frontier (append* (map car zs)))
           (compute-frontier (append* (map cdr zs))))]

         [(:alternatives a b)
          (match-define (cons a/no-req a/req) (render a width-limit))
          (match-define (cons b/no-req b/req) (render b width-limit))
          (cons (compute-frontier (append a/no-req b/no-req))
                (compute-frontier (append a/req b/req)))]
         [(:annotate d _) (render d width-limit)]
         [(:select d p)
          (match-define (cons as bs) (render d width-limit))
          (cons (filter p as) (filter p bs))]
         [(:worsen d n)
          (match-define (cons as bs) (render d width-limit))
          (cons (for/list ([a (in-list as)])
                  (struct-copy measure a [height (+ n (measure-height a))]))
                (for/list ([b (in-list bs)])
                  (struct-copy measure b [height (+ n (measure-height b))])))]
         [(:fail) (cons '() '())]))))
  (match-define (cons as bs) (render d max-width))
  (match (compute-frontier (append as bs))
    ['() (raise-arguments-error 'render "the document fails to render")]
    [xs (first (mins-by (mins-by xs #:key measure-badness) #:key measure-height))]))

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

(define (worsen d n)
  (match d
    [(:fail) fail]
    [_ (make-worsen d n)]))
