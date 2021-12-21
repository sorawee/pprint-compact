#lang racket/base

(provide render

         (struct-out measure)

         ;; predicate
         doc?

         ;; pattern expanders
         :text
         :flush
         :concat
         :align
         :alternatives
         :full
         :fail
         :annotate
         :select
         :cost
         :context

         ;; primitive constructors
         text
         flush
         concat
         align
         alternatives
         full
         annotate
         select
         cost
         context

         fail)

(require racket/match
         racket/list
         racket/string
         racket/pretty
         syntax/parse/define
         "measure.rkt"
         "pareto-frontier.rkt"
         "memoize.rkt"
         (for-syntax racket/base))

(define-for-syntax current-debug? #f)

(define-simple-macro (cond-dbg
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

(struct doc () #:transparent)
(struct :text doc (s) #:transparent #:constructor-name text)
(struct :alternatives doc (a b) #:transparent #:constructor-name make-alternatives)
(struct :flush doc (d) #:transparent #:constructor-name make-flush)
(struct :align doc (d) #:transparent #:constructor-name make-align)
(struct :concat doc (a b) #:transparent #:constructor-name make-concat)
(struct :full doc (d) #:transparent #:constructor-name make-full)
(struct :fail doc () #:transparent #:constructor-name make-fail)
(struct :annotate doc (d a) #:transparent #:constructor-name make-annotate)
(struct :select doc (d p) #:transparent #:constructor-name make-select)
(struct :cost doc (d n) #:transparent #:constructor-name make-cost)
(struct :context doc (f) #:transparent #:constructor-name context)

(define (mins-by xs #:key [key values])
  (for/fold ([best (list (first xs))]) ([x (in-list (rest xs))])
    (cond
      [(= (key (first best)) (key x)) (cons x best)]
      [(< (key (first best)) (key x)) best]
      [else (list x)])))

(define (return d width-limit first-limit full? out)
  #;(cond-dbg
   [#:dbg
    (displayln "--------------------")
    (printf "d: ~e\n" d)
    (printf "width-limit: ~a\n" width-limit)
    (printf "first-limit: ~a\n" first-limit)
    (printf "full?: ~a\n" full?)
    (printf "out: ~a\n" out)]
   [#:prod (void)])
  out)

(define (find-optimal-layout d max-width max-first)
  ;; render :: doc?, nat?, int?, bool? -> (listof measure?)
  (define render
    (memoize-render
     (λ (d width-limit first-limit full?)
       (return
        d width-limit first-limit full?
        (match d
          [(:text s)
           (define len (string-length s))
           (cond
             [full?
              (if (positive? len)
                  (cons '() '())
                  (cons '() (list (measure 0 0 0 0 (λ (indent-s indent-l xs) xs)))))]
             [else (cons (list (measure (max 0 (- len (max 0 first-limit)))
                                        len
                                        0
                                        0
                                        (λ (current-column current-align-pos xs) (cons s xs))))
                         '())])]
          [(:full d)
           (match-define (cons as bs) (render d width-limit first-limit full?))
           (cons '() (compute-frontier (append as bs)))]
          [(:flush d)
           (match-define (cons as bs) (render d width-limit first-limit full?))
           (cons
            (compute-frontier
             (for/list ([m (in-sequences (in-list as) (in-list bs))])
               (match-define (measure badness _ height cost r) m)
               (measure
                badness
                0
                (add1 height)
                cost
                (λ (current-column current-align-pos xs)
                  (r current-column
                     current-align-pos
                     (list* "\n" (make-string current-align-pos #\space) xs))))))
            '())]

          [(:align d)
           (match-define (cons d/no-req d/req)
             (render d (max 0 first-limit) (max 0 first-limit) full?))

           ;; proceed :: listof measure? -> listof measure?
           (define (proceed ms)
             (for/list ([m (in-list ms)])
               (match-define (measure badness last-width height cost r) m)
               (measure (+ badness (* height (max 0 (- first-limit))))
                        (cond
                          [(zero? height) last-width]
                          [else (+ last-width width-limit (- first-limit))])
                        height
                        cost
                        (λ (current-column current-align-pos xs)
                          (r current-column current-column xs)))))

           (cons (proceed d/no-req) (proceed d/req))]

          [(:concat a b)
           (match-define (cons a/no-req a/req) (render a width-limit first-limit full?))

           ;; we don't need to care about full? anymore
           ;; 1. if a is more than one line, then the effect of full is already discarded
           ;; 2. if a is one line empty, then a/no-req is empty and a/req is not
           ;; 3. if a is one line non empty, then both a/no-req and a/req are empty.
           ;; therefore, we need to care only whether we are using a/no-req or a/req

           ;; get-zs :: listof measure? boolean? -> (listof (pairof (listof measure?) (listof measure?)))
           (define (get-zs as full?)
             (for/list ([m-a (in-list as)])
               (match-define (measure badness-a last-width-a height-a cost-a r-a) m-a)
               (define remaining-first-width
                 (cond
                   [(zero? height-a) (- first-limit last-width-a)]
                   [else (- width-limit last-width-a)]))

               (define (proceed bs)
                 (for/list ([m-b (in-list bs)])
                   (match-define (measure badness-b last-width-b height-b cost-b r-b) m-b)
                   (measure (+ badness-a badness-b)
                            (cond
                              [(zero? height-b) (+ last-width-a last-width-b)]
                              [else last-width-b])
                            (+ height-a height-b)
                            (+ cost-a cost-b)
                            (λ (current-column current-align-pos xs)
                              (r-a current-column
                                   current-align-pos
                                   (r-b (+ last-width-a
                                           (if (zero? height-a)
                                               current-column
                                               current-align-pos))
                                        current-align-pos
                                        xs))))))

               (match-define (cons b/no-req b/req)
                 (render b width-limit remaining-first-width full?))
               (cons (proceed b/no-req) (proceed b/req))))

           (define zs/no-req (get-zs a/no-req #f))
           (define zs/req (get-zs a/req #t))

           (cons
            (compute-frontier (append (append* (map car zs/no-req)) (append* (map car zs/req))))
            (compute-frontier (append (append* (map cdr zs/no-req)) (append* (map cdr zs/req)))))]

          [(:alternatives a b)
           (match-define (cons a/no-req a/req) (render a width-limit first-limit full?))
           (match-define (cons b/no-req b/req) (render b width-limit first-limit full?))
           (cons (compute-frontier (append a/no-req b/no-req))
                 (compute-frontier (append a/req b/req)))]
          [(:annotate d _) (render d width-limit first-limit full?)]
          [(:select d p)
           (match-define (cons as bs) (render d width-limit first-limit full?))
           (cons (filter p as) (filter p bs))]
          [(:cost d n)
           (match-define (cons as bs) (render d width-limit first-limit full?))
           (cons (for/list ([a (in-list as)])
                   (struct-copy measure a [cost (+ n (measure-cost a))]))
                 (for/list ([b (in-list bs)])
                   (struct-copy measure b [cost (+ n (measure-cost b))])))]
          [(:context f) (render (f width-limit first-limit full?) width-limit first-limit full?)]
          [(:fail) (cons '() '())])))))
  (match-define (cons as bs) (render d max-width max-first #f))
  (match (compute-frontier (append as bs))
    ['() (raise-arguments-error 'render "the document fails to render")]
    [xs (first (mins-by (mins-by xs #:key measure-badness) #:key measure-height))]))

(define (render d width indent)
  (match-define (measure badness _ _ _ renderer)
    (find-optimal-layout d width (- width indent)))
  (define out (string-append* (renderer indent indent '())))
  (cond-dbg
   [#:dbg
    (define expected-badness (+ badness (max 0 (- indent width))))
    (define out* (string-append (make-string indent #\space) out))
    (define actual-badness
     (for/sum ([line (string-split out* "\n")])
       (max 0 (- (string-length line) width))))
    (unless (= expected-badness actual-badness)
      (printf "expected badness: ~a\n" expected-badness)
      (printf "actual badness: ~a\n" actual-badness)
      (printf "out:\n~a\n" out*)
      (error 'unexpected-badness))]
   [#:prod (void)])
  out)

;; perform partial evaluation on the "constructor"s

(define fail (make-fail))

(define (flush d)
  (cond-dbg
   [#:dbg (make-flush d)]
   [#:prod (match d
             [(:fail) fail]
             [(:full d) (flush d)]
             [_ (make-flush d)])]))

(define (align d)
  (cond-dbg
   [#:dbg (make-align d)]
   [#:prod (match d
             [(:align _) d]
             [(:text _) d]
             [(:fail) fail]
             [_ (make-align d)])]))

(define (concat a b)
  (cond-dbg
   [#:dbg (make-concat a b)]
   [#:prod (match* (a b)
             [((:text "") d) d]
             [(d (:text "")) d]
             [((:text sa) (:text sb)) (text (string-append sa sb))]
             [((:full _) (:text _)) fail] ; the text is non-empty
             #;[(a (:concat b c))
              (concat (concat a b) c)]
             #;[(a (:full b)) (full (concat a b))]
             [((:fail) _) fail]
             [(_ (:fail)) fail]
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
             [(:annotate _ a*)
              (cond
                [(equal? a a*) d]
                [else (make-annotate d a)])]
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
