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
          scribble-math)
@doc-lib-setup

@title{Untagged structures}

@deftech{Untagged structures} are implemented as tagged structures, using a
special tag name: @racket[untagged]. The @racket[structure] identifier and its
derivatives therefore simply call @racket[tagged], filling in the tag name
with @racket[untagged].

@defform[#:kind "type expander"
         #:literals (:)
         (structure fields-maybe-types)
         #:grammar
         [(fields-maybe-types (code:line just-fieldᵢ ...)
                              (code:line maybe-∀ field+type-descriptorᵢ ...))
          (maybe-∀ (code:line)
                   (code:line #:∀ (tvarⱼ ...)))
          (just-fieldᵢ fieldᵢ
                       [fieldᵢ])
          (field+type-descriptor [fieldᵢ typeᵢ]
                                 [fieldᵢ : typeᵢ])
          (fieldᵢ Identifier)
          (typeᵢ Type)
          (tvarⱼ Identifier)]]{
 Expands to the same type as @racket[(tagged untagged fields-maybe-types)]
 would.

 The elements may appear in any order, as long as the field descriptors form a
 contiguous sequence.}

@defform*[#:kind "syntax"
          #:link-target? #f
          #:literals (:)
          ((structure maybe-instance maybe-∀ fields-maybe-types)
           (structure maybe-builder  maybe-∀ fields+values-maybe-types))
          #:grammar
          [(maybe-instance (code:line)
                           #:instance)
           (maybe-builder (code:line)
                          #:builder)
           (tag-name Identifier)
           (maybe-∀ (code:line)
                    (code:line #:∀ (tvarⱼ ...)))
           (fields-maybe-types (code:line just-fieldᵢ ...)
                               (code:line [fieldᵢ : typeᵢ] ...))
           (just-fieldᵢ fieldᵢ
                        [fieldᵢ])
           (field+value-maybe-type (code:line [fieldᵢ valueᵢ] ...)
                                   (code:line field+value+typeᵢ ...))
           (field+value+typeᵢ [fieldᵢ : typeᵢ valueᵢ]
                              [fieldᵢ valueᵢ : typeᵢ])
           (fieldᵢ Identifier)
           (typeᵢ Type)
           (valueᵢ Expression)]]{

 The first syntax expands to the same instance as
 @racket[(tagged untagged maybe-instance maybe-∀ fields-maybe-types)]
 would.

 The second syntax expands to the same builder function as
 @racket[(tagged untagged maybe-builder maybe-∀ fields+values-maybe-types)]
 would.
 
 The elements may appear in any order, as long as the field descriptors form a
 contiguous sequence.}

@defform[#:kind "match expander"
         #:link-target? #f
         #:literals (:)
         (structure maybe-no-implicit field-maybe-patsᵢ ...)
         #:grammar
         [(maybe-no-implicit (code:line)
                             (code:line #:no-implicit-bind))
          (field-maybe-patsᵢ fieldᵢ
                             [fieldᵢ patᵢⱼ ...])
          (fieldᵢ Identifier)
          (patᵢⱼ #,"match pattern")]]{
 Expands to the same match pattern as
 @racket[(tagged untagged maybe-no-implicit field-maybe-patsᵢ ...)] would.
 
 The elements may appear in any order, as long as the
 @racket[field-maybe-patsᵢ] form a contiguous sequence.}

@defform*[#:kind "syntax"
          #:literals (:)
          [(structure? fieldᵢ ...)
           (structure? [fieldᵢ : typeᵢ] ...)
           (structure? [fieldᵢ predᵢ] ...)]
          #:contracts ([tag-name Identifier]
                       [fieldᵢ Identifier]
                       [typeᵢ Type/Make-Predicate]
                       [predᵢ (ExpressionOf (→ Any Any : typeᵢ))])]{

 The first syntax expands to the same predicate as
 @racket[(tagged untagged fieldᵢ ...)] would.

 The second syntax expands to the same predicate as
 @racket[(tagged untagged [fieldᵢ : typeᵢ] ...)] would.

 The third syntax expands to the same predicate as
 @racket[(tagged untagged [fieldᵢ predᵢ] ...)] would.
 
 The elements may appear in any order, as long as the field descriptors form a
 contiguous sequence.}

@defform[#:kind "syntax"
         #:literals (:)
         (define-structure maybe-predicate? name fields-maybe-types)
         #:grammar
         [(maybe-predicate? (code:line)
                            (code:line #:? predicate-name?))
          (tag-name Identifier)
          (fields-maybe-types (code:line just-fieldᵢ ...)
                              (code:line maybe-∀ field+type-descriptorᵢ ...))
          (maybe-∀ (code:line)
                   (code:line #:∀ (tvarⱼ ...)))
          (just-fieldᵢ fieldᵢ
                       [fieldᵢ])
          (field+type-descriptor [fieldᵢ typeᵢ]
                                 [fieldᵢ : typeᵢ])
          (fieldᵢ Identifier)
          (typeᵢ Type)
          (tvarⱼ Identifier)]]{

 Defines @racket[name] and @racket[predicate?] in the same way as
 @racket[
 (define-tagged #:tag untagged maybe-predicate? name fields-maybe-types)] would.

 The elements of the grammar for @racket[define-structure] may appear in any
 order, as long as the field descriptors form a contiguous sequence.}

@defform[#:kind "type expander"
         #:literals (:)
         (structure-supertype field+typeᵢ ...)
         #:grammar
         [(field+type [fieldᵢ typeᵢ]
                      [fieldᵢ : typeᵢ])]]{
 Expands to the same union type as
 @racket[(tagged-supertype untagged field+typeᵢ ...)] would.}

@defform[#:kind "match expander"
         #:link-target? #f
         #:literals (:)
         (structure-supertype maybe-no-implicit field-maybe-patsᵢ ...)
         #:grammar
         [(maybe-no-implicit (code:line)
                             (code:line #:no-implicit-bind))
          (field-maybe-patsᵢ fieldᵢ
                             [fieldᵢ patᵢⱼ ...])
          (fieldᵢ Identifier)
          (patᵢⱼ #,"match pattern")]]{
 Expands to the same match pattern as
 @racket[(tagged-supertype untagged maybe-no-implicit field-maybe-patsᵢ ...)]
 would.

The elements may appear in any order, as long as the tag name appears before
 any field descriptor, and as long as the field descriptors form a contiguous
 sequence.}

@defform[(structure-supertype* …)]{
 Currently not implemented. Will be equivalent to nesting
 @racket[structure-supertype].}

@defidform[#:kind "type"
           StructureTop]{
                         
 The supertype of all @tech{untagged structures}, including @tech{tagged
  structures}, @tech{constructors} and @tech{nodes} using the tag name
 @racket[untagged]. It does not include tagged structures, constructors or
 nodes with other tag names than @racket[untagged].}

@defproc[(StructureTop? [v Any]) Boolean]{
                                          
 A predicate for @racket[StructureTop]. It accepts all @tech{untagged
  structures}, including @tech{tagged structures}, @tech{constructors} and
 @tech{nodes} using the tag name @racket[untagged], and rejects any other
 value.}
