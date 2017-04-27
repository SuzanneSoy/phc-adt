#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require scribble-enhanced/doc
          scribble-math
          racket/require
          hyper-literate
          (for-label (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                     (lib "phc-adt/tagged.hl.rkt")
                     extensible-parser-specifications
                     racket/format
                     phc-toolkit
                     phc-toolkit/untyped-only
                     remember
                     syntax/parse
                     syntax/parse/experimental/template
                     (subtract-in typed/racket/base type-expander)
                     type-expander
                     type-expander/expander
                     multi-id))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "tagged-supertype"
       #:tag-prefix "phc-adt/tagged-supertype"
       ]{Supertypes of tagged structures}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/tagged-supertype"))


@section{type-expander}

@chunk[<tagged-supertype>
       (define-multi-id tagged-supertype
         #:type-expander <tagged-supertype-type-expander>
         #:match-expander <tagged-supertype-match-expander>)]

As a type, @racket[tagged-supertype] accepts two syntaxes. With the first
one, the type of each field is specified, and the second returns a parametric
structure:

@chunk[<tagged-supertype-type-expander-signature-types>
       (_ name:id [field:id (~optional :colon) type:expr] …)]

@chunk[<tagged-supertype-type-expander-signature-infer>
       (_ name (~either [field:id] field:id) …)]

The type uses the @racket[structure] type-expander, and
expands to the union of all structures which contain a
superset of the given set of fields. It uses the specified
type for the given fields, and defaults to @racket[Any] for the
other extra fields.

@chunk[<tagged-supertype-type-expander-impl-types>
       (has-fields/type #'([field type] …))]

The second syntax builds upon the first, and  produces a
parametric type, with a @racket[∀] type argument for each
specified field (other fields still falling back to 
@racket[Any]).

@CHUNK[<tagged-supertype-type-expander-impl-infer>
       (define-temp-ids "~a/τ" (field …))
       #`(∀ (field/τ …)
            #,(has-fields/type #'([field field/τ] …)))]

The type-expander finally calls either case depending on the
syntax used.

@chunk[<tagged-supertype-type-expander>
       (λ (stx)
         (syntax-parse stx
           [<tagged-supertype-type-expander-signature-types>
            <tagged-supertype-type-expander-impl-types>]
           [<tagged-supertype-type-expander-signature-infer>
            <tagged-supertype-type-expander-impl-infer>]))]

@section{Match}

The match-expander for tagged-supertype accepts all
structures which contain a superset of the given set of fields:

@chunk[<tagged-supertype-match-expander>
       (λ/syntax-parse (_ . :tagged-match-args-syntax-class)
         (define/with-syntax ([common . (all-field …)] …)
           (has-fields/common #'(fieldᵢ …)))
         (define/with-syntax ((maybe-fieldᵢ …) …)
           (if (attribute no-implicit)
               (map (const #'()) #'(fieldᵢ …))
               #'((fieldᵢ) …)))
         (define/with-syntax ((maybe-pats …) …)
           (quasitemplate ((<maybe-pat…> …) …)))
         #`(or (tagged name #:no-implicit-bind [all-field . maybe-pats] …) …))]

@chunk[<tagged-anytag-match>
       (define-match-expander tagged-anytag-match
         (λ/syntax-case ([fieldᵢ patᵢⱼ …] …) ()
           (tagged-anytag-match! #'([fieldᵢ (and patᵢⱼ …)] …))))]

Each field that was passed to @racket[tagged-supertype]
additionally matches against the given @racket[pat …], and
other fields do not use any extra pattern.

@chunk[<maybe-pat…>
       (!cdr-assoc #:default []
                   all-field
                   [fieldᵢ . [maybe-fieldᵢ … patᵢⱼ …]]
                   …)]

@section{Nested supertype}

The @racket[(tagged-supertype* f₁ f₂ … fₙ T)] type describes any structure
containing a field @racket[f₁], whose type is any structure containing a field
@racket[f₂] etc. The last field's type is given by @racket[T].

@chunk[<tagged-supertype*>
       (define-multi-id tagged-supertype*
         #:type-expander
         (λ (stx)
           (error (string-append "tagged-supertype* is currently broken (needs"
                                 " to ignore the tag name, since it doe not"
                                 " have a tag at each step."))
           (syntax-parse stx
             [(_ T:expr)
              #`T]
             [(_ T:expr field:id other-fields:id …)
              #`(tagged-supertype
                 [field (tagged-supertype* T other-fields …)])]))
         (code:comment
          "#:match-expander <tagged-supertype-match-expander> ; TODO"))]

@section{Conclusion}

@chunk[<*>
       (require (for-syntax racket/base
                            racket/function
                            racket/syntax
                            syntax/parse
                            syntax/parse/experimental/template
                            phc-toolkit/untyped
                            type-expander/expander)
                phc-toolkit
                multi-id
                type-expander
                "tagged-structure-low-level.hl.rkt"
                "tagged.hl.rkt")

       (provide tagged-supertype
                tagged-supertype*)

       <tagged-anytag-match>
       <tagged-supertype>
       <tagged-supertype*>]