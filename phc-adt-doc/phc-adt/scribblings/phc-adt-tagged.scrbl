#lang scribble/manual

@(require racket/require
          (for-label (subtract-in typed/racket/base type-expander)
                     type-expander
                     phc-adt
                     racket/shared
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

@title{Tagged structures}

@deftech{Tagged structures} behave like Racket's plain @racket[struct]s, but
do not need to be declared before they are used. They are similar to
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{prefab} structs in that
aspect, but prefab structs lack field names, i.e. they behave more like
vectors tagged with the prefab struct's name.

A tagged structure is identified by its tag name, and its set of field names.
The type of a tagged structure can be expressed without having declared the
tagged structure in advance. It is also possible to create instances of tagged
structures without declaring them beforehand, and this applies to match
patterns for tagged structures too. Fields can be accessed without knowing the
structure's tag name, using @racket[(uniform-get instance field-name)].

These features make tagged structures particularly suited for writing
compilers and other programs which transform large and complex data structures
in small steps. This library is designed to work hand in hand with the
@elem[#:style 'tt "phc-graph"] library (not available yet, but will be soon),
which adds to tagged structures some support for safe cyclic data structures,
and easy manipulation of those via higher-order operations. The regular tagged
structures should normally not be used to form cyclic data structures@note{It
 is possible in theory to build cyclic data structures using @racket[shared],
 for example, but this use case is not supported by this library, and is
 unlikely to play well with Typed/Racket in any case.}. Thus, the graph library
uses @deftech{nodes} instead, which can contain cycles, as long as the cycles
are safely created via the graph library. Nodes also have a tag name and a set
of fields, and each node type is a subtype of the corresponding tagged
structure with the same name and fields.

@defform[#:kind "type expander"
         #:literals (:)
         (tagged tag-name fields-maybe-types)
         #:grammar
         [(tag-name Identifier)
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
                               
 Expands to the type for a tagged structure with the given tag name and
 fields. If the types for the fields are not given, then the tagged structure
 is polymorphic, with one type variable per field.

 If @racket[#:∀ (tvarⱼ ...)] is specified, a polymorphic tagged structure is
 polymorphic, with the given type variables.

 The elements may appear in any order, as long as the tag name appears before
 any field descriptor, and as long as the field descriptors form a contiguous
 sequence.}

@defform*[#:kind "syntax"
          #:link-target? #f
          #:literals (:)
          ((tagged maybe-instance maybe-∀ tag-name fields-maybe-types)
           (tagged maybe-builder  maybe-∀ tag-name fields+values-maybe-types))
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
 When using the @racket[fields-maybe-types] syntax, this form expands to a
 lambda function which can be used to build an instance of the tagged
 structure, by passing as many values as there are fields.
 
 When using the @racket[fields+values-maybe-types] syntax, this form directly
 returns an instance of the tagged structure, with the given values.
 
 It is mandatory to disambiguate with either @racket[#:instance] or
 @racket[#:builder] when using @racket[tagged] with an empty list of fields
 (i.e. a structure with no fields) as it cannot be guessed from the syntax
 otherwise, so it is best to always include one or the other when writing a
 macro which expands to uses of @racket[tagged].

 When types are specified, they are used to annotate the values when producing
 an instance, otherwise they are used as the argument types for the builder
 function.

 When @racket[#:∀ (tvarⱼ ...)] is specified for a builder, a polymorphic
 builder is produced, with the given @racket[tvarⱼ ...] type variables.

 When @racket[#:∀ (tvarⱼ ...)] is specified for an instance, the type of
 values annotated with @racket[tvarⱼ] is inferred, and an instance of a
 polymorphic tagged structure is produced. A @racket[tvarⱼ] can be used within
 a more complex type, in which case only that part of the type is inferred.
 
 The elements may appear in any order, as long as the tag name appears before
 any field descriptor, and as long as the field descriptors form a contiguous
 sequence.}

@defform[#:kind "match expander"
         #:link-target? #f
         #:literals (:)
         (tagged tag-name maybe-no-implicit field-maybe-patsᵢ ...)
         #:grammar
         [(tag-name Identifier)
          (maybe-no-implicit (code:line)
                             (code:line #:no-implicit-bind))
          (field-maybe-patsᵢ fieldᵢ
                             [fieldᵢ patᵢⱼ ...])
          (fieldᵢ Identifier)
          (patᵢⱼ #,"match pattern")]]{
 Expands to a match pattern for a tagged structure with the given name and
 fields. The value of each @racket[fieldᵢ] is matched against all of the
 corresponding @racket[patᵢⱼ ...]. When there are not @racket[patᵢⱼ] for a
 @racket[fieldᵢ], the brackets around the field name may be omitted.

 Unless @racket[#:no-implicit-bind] is specified, every @racket[fieldᵢ] is
 bound by the match pattern to the field's value.

 The elements may appear in any order, as long as the tag name appears before
 any @racket[field-maybe-patsᵢ], and as long as the @racket[field-maybe-patsᵢ]
 form a contiguous sequence.}

@defform*[#:kind "syntax"
          #:literals (:)
          [(tagged? tag-name fieldᵢ ...)
           (tagged? tag-name [fieldᵢ : typeᵢ] ...)
           (tagged? tag-name [fieldᵢ predᵢ] ...)]
          #:contracts ([tag-name Identifier]
                       [fieldᵢ Identifier]
                       [typeᵢ Type/Make-Predicate]
                       [predᵢ (ExpressionOf (→ Any Any : typeᵢ))])]{
 Expands to a predicate for tagged structures with the given @racket[tag] and
 @racket[field]s. If types are specified, each @racket[typeᵢ] is passed to
 @racket[make-predicate], and the resulting predicate is checked against the
 value of the corresponding @racket[fieldᵢ].

 Each @racket[typeᵢ] must therefore be a valid type for which
 @racket[make-predicate] can generate a predicate (@racket[make-predicate]
 cannot create a predicate for some types, like function types, or any type
 which translates to a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperone}
 contract.

 The last form allows the use of arbitrary predicates @racket[predᵢ] which are
 checked against the value of the corresponding @racket[fieldᵢ]. When the type
 of a given @racket[predᵢ] includes a filter asserting that it returns true if
 and only if the value is of type @racket[typeᵢ], then the predicate produced by
 @racket[tagged-predicate!] will also have that filter on the corresponding
 field. By default, any function of type @racket[(→ Any Any)] will
 implicitly have the @racket[Any] filter, which does not bring any extra
 information.

 The elements may appear in any order, as long as the tag name appears before
 any field descriptor, and as long as the field descriptors form a contiguous
 sequence.}

@defform[#:kind "syntax"
         #:literals (:)
         (define-tagged name maybe-tag-name maybe-predicate? fields-maybe-types)
         #:grammar
         [(name Identifier)
          (maybe-tag-name (code:line)
                          (code:line #:tag tag-name))
          (tag-name Identifier)
          (maybe-predicate? (code:line)
                            (code:line #:? predicate-name?))
          (predicate-name? Identifier)
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
                               
 Defines @racket[name] as a shorthand for the type expander, match expander,
 builder function and predicate for a tagged structure with the given
 @racket[tag-name] and fields.

 When @racket[#:tag tag-name] is omitted, it defaults to @racket[name].

 The predicate is bound to @racket[predicate-name?]; When
 @racket[#:? predicate-name?] is omitted, it defaults to @racket[_name?], which
 is an identifier with the same lexical context as @racket[name], with a
 @racket["?"] appended at the end.

 The @racket[_name] and @racket[_predicate?] identifiers behave as follows:

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:kind "type expander"
                 #:link-target? #f
                 _name]{
  Expands to the same type as @racket[(tagged tag-name fields-maybe-types)]
  would.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:link-target? #f _name]{
  Expands to the same builder function as
  @racket[(tagged #:builder tag-name fields-maybe-types)] would.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defform[#:kind "match expander"
               #:link-target? #f
               (_name patᵢ ...)]{
  Expands to the same match pattern as
  @racket[(tagged tag-name [fieldᵢ patᵢ] ...)] would.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:link-target? #f _predicate?]{
  Expands to the same predicate as @racket[(tagged? just-fieldᵢ)] would. Note
  that it does not attempt to check the field values, as doing so would mean
  that all of the @racket[typeᵢ], if specified, would have to be suitable
  arguments for @racket[make-predicate].}))))

 The elements of the grammar for @racket[define-tagged] may appear in any
 order, as long as the tag name appears before any field descriptor, and as
 long as the field descriptors form a contiguous sequence.}

@defform[#:kind "type expander"
         #:literals (:)
         (tagged-supertype tag-name field+typeᵢ ...)
         #:grammar
         [(tag-name Identifier)
          (field+type [fieldᵢ typeᵢ]
                      [fieldᵢ : typeᵢ])]]{
 Expands to the union type of all tagged structures with the given name and a
 superset of the given fields, where each given @racket[fieldᵢ] must have the
 corresponding type @racket[typeᵢ], and the other fields have the type
 @racket[Any].}

@defform[#:kind "match expander"
         #:link-target? #f
         #:literals (:)
         (tagged-supertype tag-name maybe-no-implicit field-maybe-patsᵢ ...)
         #:grammar
         [(tag-name Identifier)
          (maybe-no-implicit (code:line)
                             (code:line #:no-implicit-bind))
          (field-maybe-patsᵢ fieldᵢ
                             [fieldᵢ patᵢⱼ ...])
          (fieldᵢ Identifier)
          (patᵢⱼ #,"match pattern")]]{
 Expands to a match pattern accepting any tagged structures with the given
 name and a superset of the given fields, where each given @racket[fieldᵢ] must
 match all the corresponding @racket[patᵢⱼ], and the other fields are matched
 against @racket[_] (i.e. they can contain any value).

 Unless @racket[#:no-implicit-bind] is specified, every @racket[fieldᵢ] is
 bound by the match pattern to the field's value, but the other extra fields
 are not bound to any variable.
 
 The elements may appear in any order, as long as the tag name appears before
 any field descriptor, and as long as the field descriptors form a contiguous
 sequence.}

@defform[(tagged-supertype* …)]{
 Currently not implemented. Will be equivalent to nesting
 @racket[tagged-supertype].}

@defidform[#:kind "type"
           TaggedTop]{
                      
 The supertype of all @tech{tagged structures}, including @tech{untagged
  structures}, @tech{nodes} and @tech{constructors}.}

@defproc[(TaggedTop? [v Any]) Boolean]{
 A predicate for @racket[TaggedTop]. It accepts all @tech{tagged structures},
 including @tech{untagged structures}, @tech{nodes} and @tech{constructors},
 and rejects any other value.}

@defform[(uniform-get v f)
         #:grammar
         ([v Expression]
          [f Identifier])]{
 Returns the value contained within the @racket[f] field of the tagged structure
 instance @racket[v].}