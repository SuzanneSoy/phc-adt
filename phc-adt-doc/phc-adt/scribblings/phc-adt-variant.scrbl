#lang scribble/manual

@(require racket/require
          (for-label (subtract-in typed/racket/base type-expander)
                     type-expander
                     phc-adt
                     (lib "phc-adt/tagged.hl.rkt")
                     (lib "phc-adt/structure.hl.rkt")
                     (lib "phc-adt/constructor.hl.rkt")
                     (lib "phc-adt/variant.hl.rkt")
                     (lib "phc-adt/tagged-supertype.hl.rkt"))
          scribble-enhanced/doc
          scribble-math
          (subtract-in scribble/struct scribble-enhanced/doc)
          scribble/decode)
@doc-lib-setup

@title{Variants}

@deftech{Variants} behave like @racketmodname[typed/racket]'s union types
(expressed via @racket[U]). They benefit however of special support by the
graph library (not published yet), which needs to rewrite data structures
in-depth during the construction of the graph, and cannot handle arbitrary data
structures.

@defform[#:kind "type-expander"
         #:literals (constructor tagged)
         (variant maybe-check-overlap
                  constructor-or-taggedᵢ ...
                  maybe-one-other-type)
         #:grammar
         [(maybe-check-overlap (code:line)
                               (code:line #:check-overlap))
          (constructor-or-tagged (tagged      tag-nameᵢ . tagged-args)
                                 (structure             . structure-args)
                                 (constructor tag-nameᵢ . constructor-args))
          (constructor tag-name maybe-∀ τᵢ ... #:rest τ-rest)
          (tag-nameᵢ Identifier)
          (maybe-one-other-type (code:line)
                                (code:line Type))]]{
 The current implementation does not completely match this documentation, but
 the implementation should shortly be updated.

 Expands to the union type of all the given @tech{tagged structures}, @tech{
  untagged structures} and @tech{constructors}.

 When another type which is not a tagged structure or similar is specified,
 it is included in the union. Only one such type is allowed.

 The types passed to @racket[variant] should not overlap. When the keyword
 @racket[#:check-overlap] is specified, @racket[variant] uses a hack to throw
 an error when two types overlap. The error message can however be unclear and
 misleading, so this feature is disabled by default.

 The elements of the grammar for @racket[define-tagged] may appear in any
 order, as long as the @racket[constructor-or-taggedᵢ ... maybe-one-other-type]
 form a contiguous sequence (the @racket[maybe-one-other-type] may occur at any
 position within the sequence, but must be included at most once).}

@defidform[#:kind "type-expander"
           V]{
 An alias for the @racket[variant] type expander.
}

@defform[#:kind "syntax"
         #:literals (constructor tagged)
         (variant? maybe-check-overlap
                   constructor-or-taggedᵢ ...
                   maybe-one-other-type)
         #:grammar
         [(maybe-check-overlap (code:line)
                               (code:line #:check-overlap))
          (constructor-or-tagged (tagged      tag-nameᵢ . tagged-args)
                                 (structure             . structure-args)
                                 (constructor tag-nameᵢ . constructor-args))
          (constructor tag-name maybe-∀ τᵢ ... #:rest τ-rest)
          (tag-nameᵢ Identifier)
          (maybe-one-other-type (code:line)
                                (code:line Type))]]{
 The current implementation does not completely match this documentation, but
 the implementation should shortly be updated.

 Expands to a predicate for the type:
 
 @racketblock[(variant maybe-check-overlap
                       constructor-or-taggedᵢ ...
                       maybe-one-other-type)]

 The elements of the grammar for @racket[variant] may appear in any order, as
 long as the @racket[constructor-or-taggedᵢ ... maybe-one-other-type] form a
 contiguous sequence (the @racket[maybe-one-other-type] may occur at any
 position within the sequence, but must be included at most once).}

@defform[#:kind "syntax"
         #:literals (:)
         (define-variant name maybe-predicate? maybe-check-overlap cases)
         #:grammar
         [(name Identifier)
          (maybe-predicate? (code:line)
                            (code:line #:? predicate-name?))
          (predicate-name? Identifier)
          (maybe-check-overlap (code:line)
                               (code:line #:check-overlap))
          (cases (code:line constructor-or-taggedᵢ ... maybe-one-other-type))
          (constructor-or-tagged (tagged      tag-nameᵢ . tagged-args)
                                 (structure             . structure-args)
                                 (constructor tag-nameᵢ . constructor-args))
          (constructor tag-name maybe-∀ τᵢ ... #:rest τ-rest)
          (tag-nameᵢ Identifier)
          (maybe-one-other-type (code:line)
                                (code:line Type))]]{
 The current implementation does not completely match this documentation, but
 the implementation should shortly be updated.

 Defines @racket[name] as a shorthand for the type expander and predicate for
 a variant with the given cases.

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:kind "type expander"
                 #:link-target? #f
                 _name]{
  Expands to the same type as @racket[(variant maybe-check-overlap cases)]
  would.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:link-target? #f _predicate?]{
                                                
  Expands to the same predicate as @racket[(variant? maybe-check-overlap cases)]
  would.}))))

 The elements of the grammar for @racket[define-variant] may appear in any
 order, as long as the @racket[constructor-or-taggedᵢ ... maybe-one-other-type]
 form a contiguous sequence (the @racket[maybe-one-other-type] may occur at any
 position within the sequence, but must be included at most once).}