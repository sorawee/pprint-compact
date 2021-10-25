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
         :cost

         ;; primitive constructors
         text
         flush
         concat
         alternatives
         full
         annotate
         select
         cost

         fail)

(require racket/match
         racket/list
         racket/string
         syntax/parse/define
         "measure.rkt"
         "pareto-frontier.rkt"
         "memoize.rkt"
         (for-syntax racket/base))

(define-for-syntax current-debug? #f)

(define-syntax-parse-rule (cond-dbg
                           [#:dbg e ...+]
                           [#:prod e2 ...+])
  #:with out (if current-debug?
                 #'(let () e ...)
                 #'(let () e2 ...))
  out)

(cond-dbg
 [#:dbg
  (displayln "==========")
  (displayln "debug mode")
  (displayln "==========")]
 [#:prod (void)])

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
(struct :cost doc (d n) #:transparent #:constructor-name make-cost)

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
          (cons (list (measure (max 0 (- len width-limit)) len len 0 0 (λ (indent xs) (cons s xs))))
                '())]
         [(:full d)
          (match-define (cons as bs) (render d width-limit))
          (cons '() (compute-frontier (append as bs)))]
         [(:flush d)
          (match-define (cons as bs) (render d width-limit))
          (cons
           (compute-frontier
            (for/list ([m (in-sequences (in-list as) (in-list bs))])
              (match-define (measure badness width _ height cost r) m)
              (measure
               badness
               width
               0
               (add1 height)
               cost
               (λ (indent xs)
                 (r indent (list* "\n" (make-string indent #\space) xs))))))
           '())]
         [(:concat a b)
          (match-define (cons a/no-req _) (render a width-limit))
          (define zs
            (for/list ([m-a (in-list a/no-req)])
              (match-define (measure badness-a width-a last-width-a height-a cost-a r-a) m-a)
              (define penalty-multiplier (max 0 (- last-width-a width-limit)))
              (define remaining-width (max 0 (- width-limit last-width-a)))
              ;; make +inf.0 reference canonical
              (define remaining-width* (if (= +inf.0 remaining-width) +inf.0 remaining-width))
              (define (proceed bs)
                (for/list ([m-b (in-list bs)])
                  (match-define (measure badness-b width-b last-width-b height-b cost-b r-b) m-b)
                  (measure (+ badness-a badness-b (* height-b penalty-multiplier))
                           (max width-a (+ width-b last-width-a))
                           (+ last-width-a last-width-b)
                           (+ height-a height-b)
                           (+ cost-a cost-b)
                           (λ (indent xs)
                             (r-a indent (r-b (+ indent last-width-a) xs))))))
              (match-define (cons b/no-req b/req) (render b remaining-width*))
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
         [(:cost d n)
          (match-define (cons as bs) (render d width-limit))
          (cons (for/list ([a (in-list as)])
                  (struct-copy measure a [cost (+ n (measure-cost a))]))
                (for/list ([b (in-list bs)])
                  (struct-copy measure b [cost (+ n (measure-cost b))])))]
         [(:fail) (cons '() '())]))))
  (match-define (cons as bs) (render d max-width))
  (match (compute-frontier (append as bs))
    ['() (raise-arguments-error 'render "the document fails to render")]
    [xs (first (mins-by (mins-by (mins-by xs #:key measure-badness)
                                 #:key measure-height)
                        #:key measure-cost))]))

(define (render d width indent)
  (string-append* ((measure-r (find-optimal-layout d width)) indent '())))

;; perform partial evaluation on the "constructor"s

(define fail (make-fail))

(define (flush d)
  (cond-dbg
   [#:dbg (make-flush d)]
   [#:prod (match d
             [(:fail) fail]
             [(:full d) (flush d)]
             [_ (make-flush d)])]))

(define (concat a b)
  (cond-dbg
   [#:dbg (make-concat a b)]
   [#:prod (match* (a b)
             [((:full _) _) fail]
             [(a (:full b)) (full (concat a b))]
             [((:fail) _) fail]
             [(_ (:fail)) fail]
             [((:text "") d) d]
             [(d (:text "")) d]
             [((:text sa) (:text sb)) (text (string-append sa sb))]
             [(_ _) (make-concat a b)])]))

(define (alternatives a b)
  (cond-dbg
   [#:dbg (make-alternatives a b)]
   [#:prod (match* (a b)
             [((:fail) _) b]
             [(_ (:fail)) a]
             [(_ _)
              (cond
                [(eq? a b) a]
                [else (make-alternatives a b)])])]))

(define (annotate d a)
  (cond-dbg
   [#:dbg (make-annotate d a)]
   [#:prod (match d
             [(:fail) fail]
             [(:annotate d a*)
              (cond
                [(equal? a a*) d]
                [(make-annotate d a)])]
             [_ (make-annotate d a)])]))

(define (select d p)
  (cond-dbg
   [#:dbg (make-select d p)]
   [#:prod (match d
             [(:fail) fail]
             [_ (make-select d p)])]))

(define (full d)
  (cond-dbg
   [#:dbg (make-full d)]
   [#:prod (match d
             [(:full _) d]
             [(:fail) fail]
             [_ (make-full d)])]))

(define (cost d n)
  (cond-dbg
   [#:dbg (make-cost d n)]
   [#:prod (match d
             [(:fail) fail]
             [_ (make-cost d n)])]))
