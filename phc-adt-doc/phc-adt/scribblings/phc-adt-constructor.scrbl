#lang scribble/manual

@(require racket/require
          (for-label (except-in (subtract-in typed/racket/base type-expander)
                                values)
                     type-expander
                     phc-adt
                     xlist
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

@title{Constructors}

@deftech{Constructors} are implemented as tagged structures, using a
single special field: @racket[values]. The @racket[constructor] identifier and
its derivatives therefore call @racket[tagged], using that single field. The
identifiers described within this section provide some syntactic sugar,
allowing constructors to contain more than one value. These values are wrapped
in a (possibly improper) list which is stored within the tagged structure's
@racket[values] field.


@defform*[#:kind "type-expander"
          [(constructor tag-name maybe-∀ τᵢ ...)
           (constructor tag-name maybe-∀ τᵢ ... . dotted-τ-rest)
           (constructor tag-name maybe-∀ τᵢ ... #:rest τ-rest)]
          #:grammar
          [(tag-name Identifier)
           (maybe-∀ (code:line)
                    (code:line #:∀ (tvarⱼ ...)))
           (τᵢ xlist-type-or-repeated-type)
           (τ-rest xlist-type-or-repeated-type)
           (dotted-τ-rest #,"like τ-rest, but must not be a syntax pair")]]{

 Expands to the type for a constructor with the given tag name and type of
 contents. The @racket[(τᵢ ...)], @racket[(τᵢ ... . dotted-τ-rest)] or
 @racket[(τᵢ ... #:rest τ-rest)] sequence is passed unmodified to
 @racket[xlist]. Therefore, depending on the syntax used, the expanded type is
 equivalent to one of the following types:

 @racketblock[
 (tagged tag-name maybe-∀ [values (xlist τᵢ ...)])
 (tagged tag-name maybe-∀ [values (xlist τᵢ ... . dotted-τ-rest)])
 (tagged tag-name maybe-∀ [values (xlist τᵢ ... #:rest τ-rest)])]

 The elements may appear in any order, as long as the tag name appears before
 any element type, and as long as the element types form a contiguous
 sequence.}

@defform*[#:kind "syntax"
          #:link-target? #f
          #:literals (* : ! ::)
          [(constructor maybe-∀ tag-name *)
           (constructor maybe-∀ tag-name : typeᵢ ...)
           (constructor maybe-∀ tag-name ! . xlist-types)
           (constructor maybe-∀ tag-name :: . xlist-types)]
          #:grammar
          [(maybe-∀ (code:line)
                    (code:line #:∀ (tvarⱼ ...)))
           (tag-name Identifier)
           (xlist-types (τᵢ ...)
                        (τᵢ ... . dotted-τ-rest)
                        (τᵢ ... #:rest τ-rest))
           (τᵢ xlist-type-or-repeated-type)
           (typeᵢ Type)]]{
 Expands to a builder function for a constructor with the given tag name and
 type of contents.

 The first syntax, using @racket[*] and no types, produces a polymorphic
 builder function which accepts any number of arguments, infers their types,
 and uses the whole list of arguments as the constructor's value.

 In the following three cases, when @racket[#:∀ (tvarⱼ ...)] is specified, a
 polymorphic builder with the @racket[tvarⱼ] type variables is produced.

 The second syntax, using @racket[:] followed by a sequence of regular types,
 produces a builder function with one argument per type. The builder function
 aggregates all its arguments in a list, and uses that list as the
 constructor's value.

 The second syntax, using @racket[!] followed by a sequence of types valid for
 @racket[xlist], produces a builder function which accepts a variable number of
 arguments. The builder function @racket[cast]s the whole list of arguments to
 the type @racket[(xlist . xlist-types)], which must therefore be a suitable
 argument to @racket[make-predicate]. The cast list is used as the
 constructor's value.

 The third syntax, using @racket[::] followed by a sequence of types valid for
 @racket[xlist], produces a builder function which accepts a single value of
 type @racket[(xlist . xlist-typed)], and uses that value as the constructor's
 value.

 Usually, the value stored within a constructor will be a list (i.e. a tuple
 in other languages), but it is possible to store a single value using
 @racket[xlist]'s rest syntax:

 @racketblock[
 ((constructor #:∀ (A) tag-name :: . A) 123)
 ((constructor tag-name :: . Number) 123)
 ((constructor tag-name :: #:rest (Vector Number String)) #(123 "abc"))]

 The elements may appear in any order, as long as the tag name appears before
 any element type, and as long as the element types form a contiguous
 sequence.}

@defform*[#:kind "syntax"
          #:link-target? #f
          #:literals (:)
          [(constructor maybe-∀ tag-name value-maybe-typeᵢ)
           (constructor maybe-∀ tag-name value-maybe-typeᵢ . dotted-rest)
           (constructor maybe-∀ tag-name value-maybe-typeᵢ #:rest rest)]
          #:grammar
          [(maybe-∀ (code:line)
                    (code:line #:∀ (tvarⱼ ...)))
           (tag-name Identifier)
           (value-maybe-typeᵢ valueᵢ
                              [valueᵢ : typeᵢ]
                              [: typeᵢ valueᵢ])
           (rest value-maybe-typeᵢ)
           (dotted-rest #,"like rest, but must not be a syntax pair")]]{

 Expands to an instance of a constructor containing the given values, grouped
 inside a list.

 When a @racket[typeᵢ] is specified, it is used to annotate the value, and is
 used as the type for that element in the resulting constructor type.

 When @racket[#:∀ (tvarⱼ ...)] is specified, the type of values annotated with
 @racket[tvarⱼ] is inferred, and an instance of a polymorphic constructor is
 produced. A @racket[tvarⱼ] can be used within a more complex type, in which
 case only that part of the type is inferred.

 The elements may appear in any order, as long as the tag name appears before
 any value, and as long as the values form a contiguous sequence, including the
 @racket[#:rest rest] which must appear immediately after the sequence of
 values, if specified. The @racket[dotted-rest], on the other hand, can be
 separated from the other values, so
 @racket[(constructor foo 1 [2 : A] 3 #:∀ (A) . 4)] is a valid (but awkward)
 use of @racket[constructor].

 The type of the @racket[dotted-rest] can still be specified using
 @racket[typed/racket]'s reader abbreviation for @racket[ann], namely
 @racket[#{dotted-rest :: type}].}

@defform*[#:kind "match expander"
          #:link-target? #f
          #:literals (* : ! ::)
          [(constructor tag-name . xlist-pats)]
          #:grammar
          [(tag-name Identifier)
           (xlist-pats (patᵢ ...)
                       (patᵢ ... . dotted-pat-rest)
                       (patᵢ ... #:rest pat-rest))
           (patᵢ XList-Match-Pattern)]]{
                                        
 Expands to a match pattern which checks whether the value is a constructor
 with the given tag name, and then matches the constructor's value against the
 match pattern @racket[(xlist . xlist-pats)]. The @racket[xlist] match expander
 in turn matches each element of a (possibly improper) list against the given
 patterns, and supports various means of specifying fixed-length, bounded and
 unbounded repetitions like "must appear between three and five times". See the
 documentation for the @racket[xlist] match expander for more details.}

@defform[#:kind "syntax"
         #:literals (* : ! ::)
         (constructor? tag-name . xlist-types)
         #:grammar
         [(tag-name Identifier)
          (xlist-types (τᵢ ...)
                       (τᵢ ... . dotted-τ-rest)
                       (τᵢ ... #:rest τ-rest))
          (τᵢ xlist-type-or-repeated-type)
          (τ-rest xlist-type-or-repeated-type)
          (dotted-τ-rest #,"like τ-rest, but must not be a syntax pair")]]{
 Expands to a predicate which returns true if and only if the following
 conditions are met:
 @itemlist[
 @item{The value is a constructor with the given tag name (i.e. a tagged
   structure with the given tag name and a single field named @racket[values],
   so nodes and untagged structures with a single field named @racket[values]
   are accepted too)}
 @item{The constructor's value (i.e. the contents of its @racket[values]
   field) is accepted by @racket[(make-predicate (xList . xlist-types))]}]}

@defform[(define-constructor name maybe-tag maybe-pred? maybe-∀ . type-spec)
         #:grammar
         [(name Identifier)
          (maybe-∀ (code:line)
                   (code:line #:∀ (tvarⱼ ...)))
          (maybe-tag (code:line)
                          (code:line #:tag tag-name))
          (tag-name Identifier)
          (maybe-pred? (code:line)
                            (code:line #:? predicate-name?))
          (predicate-name? Identifier)
          (types-spec (: typeᵢ ...)
                      (! . xlist-types)
                      (:: . xlist-types))
          (xlist-types (τᵢ ...)
                       (τᵢ ... . dotted-τ-rest)
                       (τᵢ ... #:rest τ-rest))
          (τᵢ xlist-type-or-repeated-type)
          (τ-rest xlist-type-or-repeated-type)
          (dotted-τ-rest #,"like τ-rest, but must not be a syntax pair")
          (typeᵢ Type)]]{
 Defines @racket[name] as a shorthand for the type expander, match expander,
 builder function and predicate for a constructor with given
 @racket[tag-name] and content types.

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
  Expands to the same type as @racket[(constructor tag-name typeᵢ ...)] or
  @racket[(constructor tag-name . xlist-types)] would.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:link-target? #f _name]{
  Expands to the same builder function as
  @racket[(constructor tag-name types-spec)] would. The use of @racket[:],
  @racket[!] or @racket[::] before the sequence of types therefore specifies
  whether the builder function accepts a simple fixed number of arguments, a
  variable number of arguments (performing a cast), or a single argument used as
  the whole value for the constructor.}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defform[#:kind "match expander"
               #:link-target? #f
               (_name patᵢ ...)]{
                                 
  When using the @racket[: typeᵢ ...] form of @racket[define-constructor], the
  defined match expander expects one pattern @racket[patᵢ] per type. The
  resulting match pattern verifies that the value is a constructor with the
  given @racket[tag-name] containing a list with the correct number of elements,
  and matches each element against the corresponding @racket[patᵢ].

  When using the @racket[(! . xlist-types)] or @racket[(:: . xlist-types)]
  forms of @racket[define-constructor], the defined match expander expects one
  pattern per (possibly repeated) xlist type. The resulting match pattern
  verifies that the value is a constructor with the given @racket[tag-name]
  containing a value accepted by
  @racket[(make-predicate (xlist . xlist-types))]. It then uses the
  @racket[split-xlist] match expander, which splits the list into one sublist
  per repeated xlist type (and a single item for each non-repeated xlist type),
  and matches each sublist or single item against the corresponding
  @racket[patᵢ]. See the documentation for @racket[split-xlist] for more details
  about this process. The resulting match pattern is therefore equivalent to:

  @racketblock[(and (tagged? tag-name values)
                    (? (make-predicate (xlist . xlist-types)))
                    (split-xlist [patᵢ ...] . xlist-types))]}))))

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defidform[#:link-target? #f _predicate?]{
  Expands to the same predicate as
  
  @racketblock[(constructor? tag-name (xlist τᵢ … . τ-rest))]

  would, where all occurrences of @racket[tvarⱼ] type variables are replaced
  with @racket[Any].}))))

 The elements of the grammar for @racket[define-tagged] may appear in any
 order, as long as the tag name appears before any field descriptor, and as
 long as the field descriptors form a contiguous sequence.}

@defidform[#:kind "type"
           ConstructorTop]{ The supertype of all @tech{constructors},
 including @tech{tagged structures}, @tech{untagged structures} and @tech{
  nodes} which only contain a single @racket[values] field.}

@defproc[(ConstructorTop? [v Any]) Boolean]{
 A predicate for @racket[ConstructorTop]. It accepts all @tech{constructors},
 including @tech{tagged structures}, @tech{untagged structures} and @tech{
  nodes} which contain a single @racket[values] field, and rejects any other
 value.}

@defproc[(constructor-values [v ConstructorTop]) T]{
 Returns the value stored within the constructor.}