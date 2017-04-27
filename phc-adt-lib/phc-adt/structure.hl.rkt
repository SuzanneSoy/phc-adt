#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require racket/require
          scribble-math
          scribble-enhanced/doc
          (for-label phc-toolkit
                     (lib "phc-adt/tagged.hl.rkt")
                     multi-id
                     racket/base
                     phc-toolkit/untyped-only))
@doc-lib-setup

@(unless-preexpanding
  (require (for-label (submod ".."))))

@title[#:style (with-html5 manual-doc-style)
       #:tag "structure"
       #:tag-prefix "phc-adt/structure"
       ]{User API for untagged structures}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/structure"))

@(table-of-contents)

@section{Introduction}

Untagged structures are implemented exactly like @racket[tagged] structures,
except that they always use the @racket[untagged] tag name.

@chunk[<structure>
       (define-multi-id structure
         #:type-expander  <expand-to-tagged>
         #:match-expander <expand-to-tagged>
         #:call           <expand-to-tagged>)]

All three cases simply expand to
@racket[(tagged untagged . _original-arguments)].

@chunk[<expand-to-tagged>
       (λ/syntax-case (_ . _original-arguments) ()
         (syntax/top-loc stx
           (tagged untagged . _original-arguments)))]

The @racket[structure?] predicate is implemented in the same way:

@chunk[<structure?>
       (define-syntax structure?
         (λ/syntax-case (_ . _original-arguments) ()
           (syntax/top-loc stx
             (tagged? untagged . _original-arguments))))]

@section{Defining untagged structures with @racket[define-structure]}

The @racket[define-structure] expands to the
@racket[(define-tagged #:tag untagged . _original-arguments)], which uses
@racket[define-tagged] but forces the tag name to be @racket[untagged].

@chunk[<define-structure>
       (define-syntax/case (define-structure . _original-arguments) ()
         (syntax/top-loc stx
           (define-tagged #:tag untagged . _original-arguments)))]

@section{Implementation of @racket[StructureTop] and @racket[StructureTop?]}

The @racket[StructureTop?] predicate is defined in terms of
@racket[tagged-any-fields-predicate]:

@CHUNK[<StructureTop?>
       (define-syntax StructureTop?
         (make-id+call-transformer-delayed
          (λ () (tagged-any-fields-predicate #'untagged))))]

Similarly, the @racket[StructureTop] type is defined using
@racket[tagged-any-fields-type]:

@CHUNK[<StructureTop>
       (define-type-expander (StructureTop stx)
         (syntax-case stx ()
           [id
            (identifier? #'id)
            (tagged-any-fields-type #'untagged)]))]

@section{Supertypes for structures}

Like the @racket[structure] and @racket[structure?] identifiers,
@racket[structure-supertype] is defined in terms of its tagged structure
counterpart, @racket[tagged-supertype]:

@chunk[<structure-supertype>
       (define-multi-id structure-supertype
         #:type-expander <expand-to-tagged-supertype>
         #:match-expander <expand-to-tagged-supertype>)]

@chunk[<expand-to-tagged-supertype>
       (λ/syntax-case (_ . _original-arguments) ()
           (syntax/top-loc stx
             (tagged-supertype untagged . _original-arguments)))]

@section{Putting it all together}

@chunk[<*>
       (require phc-toolkit
                "tagged.hl.rkt"
                "tagged-structure-low-level.hl.rkt"
                "tagged-supertype.hl.rkt"
                multi-id
                type-expander
                (for-syntax racket/base
                            phc-toolkit/untyped))

       (provide structure
                structure?
                define-structure
                StructureTop
                StructureTop?
                structure-supertype)
       
       <structure>
       <structure?>
       <define-structure>
       <StructureTop>
       <StructureTop?>
       <structure-supertype>]