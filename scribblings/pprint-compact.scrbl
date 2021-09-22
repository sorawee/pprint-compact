#lang scribble/manual
@require[scribble/example
         scriblib/autobib
         @for-label[pprint-compact
                    pprint-compact/memoize
                    pprint-compact/process
                    racket/base
                    racket/contract
                    racket/match]]

@(define evaluator (make-base-eval))
@(evaluator '(require racket/match pprint-compact))
@(define-cite ~cite citet generate-bibliography)

@(define Ber17
   (make-bib #:title "A Pretty But Not Greedy Printer"
             #:url "https://jyp.github.io/pdf/Prettiest.pdf"
             #:author "Jean-Philippe Bernady"
             #:date "2017"))

@(define Pod14
   (make-bib #:title "Polynomial-Time Optimal Pretty-Printing Combinators with Choice"
             #:url "https://oops.math.spbu.ru/papers/printer-combinators.pdf"
             #:author (authors "Anton Podkopaev" "Dmitri Boulytchev")
             #:date "2014"))

@title{pprint-compact: a non-greedy pretty printer}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[pprint-compact]

This library implements an optimal pretty printer as described in @citet[Ber17],
which generates the most optimal textual document from a tree structure.

The library is similar to another pretty printer library @racketmodname[pprint #:indirect],
but @racketmodname[pprint #:indirect] implements a greedy printer and doesn't support the choice operator (@racket[alt]). In practice, this means @racketmodname[pprint #:indirect] will be more efficient, but it lacks expressivity that this library provides, and could produce a non-optimal layout.

Unlike @link["https://github.com/jyp/prettiest/"]{the canonical Haskell implementation} which removes the choice operator to improve performance, we implement all constructs described in @citet[Ber17], with additional optimizations and constructs.

A part of this documentation and its structure are shamelessly copied/adapted from the PPrint library.

@table-of-contents[]

@section{Getting Started}

Here's a simple example of pretty-printing a fragment of code.

@examples[#:eval evaluator
  (define doc
    (v-append
      (text "while (true) {")
      (h-append (text "    ")
                (v-append (text "f();")
                          (alt (hs-append (text "if (done())")
                                          (text "exit();"))
                               (v-append (text "if (done())")
                                         (h-append (text "    ")
                                                   (text "exit();"))))))
      (text "}")))
  (pretty-print doc)
]

With a different page width limit, the document could be printed differently:

@examples[#:eval evaluator
  (pretty-print doc #:width 20)
]

@section{Documents}

Formatting text involves creating an ``abstract document'' or @deftech{doc},
which encapsulates formatting information for the pretty printer.
The library functions (see @secref{Constructing_Documents})
build and combine docs, which can then be rendered for pretty printing
(see @secref{Rendering_Documents}).

@defproc[(doc? [x any/c]) boolean?]{
  Determines whether @racket[x] is a member of the @tech{doc} datatype.
}

@section{Best Practice for Document Construction}

The arguments to @racket[alt] should roughly have the same content, albeit with different formats.
This means that the @deftech{tree size} of a @tech{doc} containing @racket[alt] tends to blow up exponentially.
The time complexity of the algorithm used by this library however depends on the @deftech{DAG size} of the @tech{doc},
so provided that sub-documents are sufficiently @emph{shared}, the @tech{DAG size} should be small enough to allow efficient pretty printing.

As an example, say we want to pretty print an S-expression with two possible layouts for each ``list'': horizontal and vertical. That is,

@racketblock[
  (1 2 3)
]

could be rendered as itself or

@racketblock[
  (1
   2
   3)
]

We can construct a function to convert an S-expression to a @tech{doc}:

@examples[#:eval evaluator #:label #f
  (define (pretty s)
    (match s
      [(list xs ...)
       (code:comment @#,elem{Calculate all subdocuments first to @emph{share} their references})
       (define xs* (map pretty xs))

       (alt (h-append lparen (hs-concat xs*) rparen)
            (h-append lparen (v-concat xs*) rparen))]
      [_ (text s)]))
]

And then pretty print it:

@examples[#:eval evaluator #:label #f
  (define doc (pretty '("1" "2" "3")))
  (pretty-print doc)
  (pretty-print doc #:width 3)
]

The important point is that we @emph{reuse} @racket[xs*] across branches of @racket[alt].
Had we call @racket[(map pretty xs)] twice in branches of @racket[alt],
both @tech{doc} construction and @racket[pretty-print] would be inefficient.

@section{Library Documentation}

@subsection{Rendering Documents}

@defproc[(pretty-print [d doc?]
                       [#:out out output-port? (current-output-port)]
                       [#:width width (or/c +inf.0 natural-number/c) (current-page-width)]
                       [#:indent indent natural-number/c (current-page-indent)])
         void?]{
  Pretty prints the @tech{doc} @racket[d] to the output port @racket[out] with a maximum page width of @racket[width] and an initial indentation level for subsequent lines of @racket[indent].

@examples[#:eval evaluator
  (define prefix-s "value is: ")
  (begin
    (display prefix-s)
    (pretty-print (v-append (text "a") (text "b") (text "c"))
                  #:indent (string-length prefix-s)))]}

@defproc[(pretty-format [d doc?]
                        [#:width width (or/c +inf.0 natural-number/c) (current-page-width)]
                        [#:indent indent natural-number/c (current-page-indent)])
         string?]{
  Pretty prints the @tech{doc} @racket[d] as a string with a maximum page width of @racket[width].
}

@subsection{Constructing Documents}

@defproc[(text [s string?]) doc?]{
  Constructs a @tech{doc} containing the fixed string @racket[s].
  @racket[s] must @bold{not} contain a newline character.
}

@defproc[(flush [d doc?]) doc?]{
  Constructs a @tech{doc} which is like @racket[d] but with a newline at the end.
}

@defproc[(alt [x doc?] ...) doc?]{
  Constructs a @tech{doc} which is rendered to one of @racket[x]s, whichever resulting in the prettiest layout for the whole document. If given no arguments, the resulting doc is @racket[fail].

  See @secref["Best_Practice_for_Document_Construction" #:doc '(lib "pprint-compact/scribblings/pprint-compact.scrbl")]
  for caveats of this construct.
}

@defproc[(h-append [x doc?] ...) doc?]{
  Concatenates @tech{doc} @racket[x]s horizontally.
}

@defproc[(h-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] horizontally.
}

@defproc[(v-append [x doc?] ...) doc?]{
  Concatenates @tech{doc} @racket[x]s vertically.
}

@defproc[(v-concat [xs (listof doc?)]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] vertically.
}

@defproc[(hs-append [x doc?] ...) doc?]{
  Concatenates @tech{doc} @racket[x]s horizontally with successive pairs separated by @racket[space].
}

@defproc[(hs-concat [x doc?] ...) doc?]{
  Concatenates @tech{doc}s in @racket[xs] horizontally with successive pairs separated by @racket[space].
}

@defproc[(sep [xs doc?]) doc?]{
  Concatenates @tech{doc}s in @racket[xs] either (1) vertically or (2)
  horizontally with successive pairs separated by @racket[space].
}

@defthing[fail doc?]{
  Constructs a @tech{doc} that fails to render.
  This doc interacts with @racket[alt]: only non-failing branches are considered for rendering.
}

@defproc[(annotate [d doc?] [a any/c]) doc?]{
  Constructs a @tech{doc} which is like @racket[d] but with an @tech{annotation} @racket[a].
}

@defproc[(flat [x doc?]) doc?]{
  Constrains @tech{doc} @racket[x] to fit in one line.
  If @racket[x] can't fit in one line, it fails to render.
}

@subsection{Useful Constants}


@defthing[empty-doc doc?]{
  Same as @racket[(text "")]
}

@defthing[lparen doc?]{
  Same as @racket[(text "(")]
}

@defthing[rparen doc?]{
  Same as @racket[(text ")")]
}

@defthing[space doc?]{
  Same as @racket[(text " ")]
}

@subsection{Parameters}

@defparam[current-page-width page-width (or/c natural-number/c +inf.0) #:value 80]{
  A parameter that determines the page width for pretty printing.
}

@defparam[current-page-indent indent natural-number/c #:value 0]{
  A parameter that determines the initial indentation level for subsequent lines.
}

@subsection{Match Expanders}

Internally, a @tech{doc} is either a @racket[:text], @racket[:flush], @racket[:concat], @racket[:alternatives], @racket[:annotate], or @racket[:fail].
We provide these @seclink["Extending_match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match expander}s to allow @tech{doc} processing (see @secref{Processing_Documents}.
The match expanders are illegal outside of the pattern position of the @racket[match] form.
Keep in mind that this list is unstable and could change across versions of the library.

@defform[(:text s)]{
  A match expander that recognizes text @racket[s] of type @racket[string?].
}

@defform[(:flush d)]{
  A match expander that recognizes a @tech{doc} @racket[d] with a newline at the end.
}

@defform[(:concat da db)]{
  A match expander that recognizes a horizontal concatenation of @tech{doc}s @racket[da] and @racket[db].
}

@defform[(:alternatives da db)]{
  A match expander that recognizes two choices: @tech{doc}s @racket[da] and @racket[db].
}

@defform[(:annotate d a)]{
  A match expander that recognizes a @tech{doc} @racket[d] with an annotation value @racket[a].
}

@defform[(:fail)]{
  A match expander that recognizes a failing @tech{doc}.
}

@section{Annotation}

An @deftech{annotation} can be attached to a @tech{doc} via @racket[annotate].
An annotated doc is rendered like the corresponding unannotated doc,
but various constructs can inspect an annotation for optimization or
to change the rendering semantics.
Unless you wish to write a function to process a @tech{doc} (see @secref{Processing_Documents}),
you can pretend that this feature doesn't exist.

@section{Processing Documents}

If you wish to process a @tech{doc}, it is highly recommended that your function that recurs over the @tech{doc} structure is @emph{memoizing}. This can be done by using @racket[memoize] function in @racketmodname[pprint-compact/memoize].

@subsection{Memoization Library}

@defmodule[pprint-compact/memoize]

@defproc[(memoize [f (-> any/c any/c)]) procedure?]{
  Memoizes the function @racket[f] based on @racket[eq?] of the input.
}

@subsection{Document Processing Library}

@defmodule[pprint-compact/process]

@defproc[(doc-process [f procedure?] [d doc?]) doc?]{
  Calls @racket[f] on the immediate subdocuments of @racket[d] and reassembles the results back.
  The function attempts to avoid creating new objects as best as it can. Note that @racket[f] should be memoized.

  Prefer this function over manual @racket[match]ing against all match expanders, since the list of match expanders could change across versions of this library, making the code brittle to changes. Using this function on the other hand makes doc processing stable across versions.
}

@subsection{Tutorial: Implementing flat}

This section will demonstrate how to perform document processing by implementing @racket[flat].

The goal of @racket[flat] is to constrain a document to have only one line.
This can be done by replacing every @racket[:flush] with @racket[fail].

@racketblock[
  (define (my-flat d)
    (match d
      [(:flush) fail]
      [(:text s) d]
      [(:fail) d]
      [(:concat a b) (h-append (my-flat a) (my-flat b))]
      [(:alternatives a b) (alt (my-flat a) (my-flat b))]
      [(:annotate d a) (annotate (my-flat d) a)]))
]

While this code is functional, it is inefficient: the time complexity is linear to the @tech{tree size} of the document, even though the @tech{DAG size} could be much smaller.

We can improve the function by using memoization:

@racketblock[
  (define (my-flat d)
    (define loop
      (memoize
        (λ (d)
          (match d
            [(:flush) fail]
            [(:text s) d]
            [(:fail) d]
            [(:concat a b) (h-append (loop a) (loop b))]
            [(:alternatives a b) (alt (loop a) (loop b))]
            [(:annotate d a) (annotate (loop d) a)]))))
    (loop d))]

However, this function is still subtly inefficient because when it recurs, it unconditionally constructs new objects even though there is no change. Moreover, the code is brittle as the list of match expanders could change across versions of this library.

We can solve both problems at once by using @racket[doc-process].

@racketblock[
  (define (my-flat d)
    (define loop
      (memoize
        (λ (d)
          (match d
            [(:flush) fail]
            [_ (doc-process loop d)]))))
    (loop d))]

We can further optimize the function by noticing that if a doc fragment is already flat, then there is no need to recur further. Therefore, we add an @tech{annotation} that the output doc is flat and avoid computation if the doc is already flat:

@racketblock[
  (define (my-flat d)
    (define loop
      (memoize
        (λ (d)
          (match d
            [(:flush) fail]
            [(:annotate _ 'flat) d]
            [_ (doc-process loop d)]))))
    (annotate (loop d) 'flat))]

@section{Design Notes}

For the history of pretty printer in general, see @seclink["history" #:doc '(lib "pprint/pprint.scrbl") #:indirect? #t]{History} in the @racketmodname[pprint #:indirect] library.

This library implements a more recent algorithm described in @citet[Ber17] and @citet[Pod14], which are more expressive and optimal, at the cost of being less efficient. The time complexity of the original algorithm is in the worst case @math{O(n W^4)}, where @math{n} is the @tech{tree size} and @math{W} is the page width.
This library uses memoization to improve the time complexity to @math{O(n W^4)} where @math{n} is the @tech{DAG size}.
It also adds features like @racket[fail] and @racket[annotate], which are basis to implement constructs like @racket[flat] efficiently.

@section{Acknowledgment}

I would like to give special thanks to @link["https://justinpombrio.net/"]{Justin Pombrio} for pointing me to @citet[Ber17] and countless advice.

@(generate-bibliography)
