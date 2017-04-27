#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require racket/require
          scribble-enhanced/doc
          scribble-math
          hyper-literate
          (for-label (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                     (lib "phc-adt/node-low-level.hl.rkt")
                     extensible-parser-specifications
                     racket/format
                     phc-toolkit
                     phc-toolkit/untyped-only
                     remember
                     syntax/parse
                     (subtract-in typed/racket/base type-expander)
                     type-expander
                     multi-id))

@(unless-preexpanding
  (require (for-label (submod ".."))))

@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "tagged"
       #:tag-prefix "phc-adt/tagged"]{User API for tagged structures}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/tagged"))

@(table-of-contents)

@section{Overview of the implementation of structures}

Tagged structures are represented using regular Racket @tc[struct]s,
see @secref["choices" #:tag-prefixes '("phc-adt/choices")]
for a somewhat outdated discussion of alternative possibilities.

The ADT type system implemented by this library needs to know about all declared
structures across the program, so that fields can be accessed anonymously, e.g.
@tc[(get _instance _f)] instead of the traditional @tc[(_s-f _instance)] (where
@tc[_s] is the struct's identifier, and @tc[f] is the field name). This allows
the accessor @racket[get] to work on all structures which contain a field with
the given name (and therefore plays well with
@seclink["Union_Types" #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]{
 unions} of structures which share some fields). It also avoids the need to
specify the struct which declared a field in order to accessing it. A separate
library adds a coat of syntactic sugar to enable the notation
@racket[instance.f1.f2.f3], following the well-known convention often used by
object-oriented languages.

The section @secref["tagged-low-level"
                    #:tag-prefixes '("phc-adt/tagged-low-level")]
describes the low-level implementation of tagged structures, including how all
declared structures are remembered across compilations, and the implementation
of the basic operations on them: constructor, predicate, accessor, type,
match expander, and row polymorphism (also known as static duck typing). The
implementation of row polymorphism works by querying the list of all known
tagged structures and returns those containing a given set of fields.

@section{A polyvalent identifier: type, match, constructor and instance}

The @tc[tagged] identifier can be used to describe a tagged structure type, a
match pattern, a tagged structure builder function, or to directly create an
instance. The last two cases are handled by the @tc[<call-expander>].

@chunk[<tagged>
       (define-multi-id tagged
         #:type-expander  tagged-type-expander
         #:match-expander tagged-match-expander
         #:call           tagged-call-expander)]

@section{The @racket[tagged] call expander (builder and instance)}

@chunk[#:save-as parse-field-instance… |<call [fieldᵢ] …+>|
       (~seq {~either [fieldᵢ:id] fieldᵢ:id} …+
             {~global-or builder?}
             {~global-or no-types?}
             {~post-fail <no-values-error> #:when (attribute instance?)})]

@chunk[#:save-as parse-field… |<[fieldᵢ] …+>|
       (~seq {~either [fieldᵢ:id] fieldᵢ:id} …+
             {~global-or no-types?})]

@chunk[#:save-as parse-field-colon-type… |<call [fieldᵢ : τᵢ] …+>|
       (~seq [fieldᵢ:id C:colon τᵢ:expr] …+
             {~global-or builder?}
             {~global-or types?}
             {~post-fail <no-values-error> #:when (attribute instance?)})]

@chunk[#:save-as parse-field-type… |<[fieldᵢ τᵢ] …+>|
       (~seq [fieldᵢ:id {~optional C:colon} τᵢ:expr] …+
             {~global-or types?})]

@chunk[#:save-as parse-field-pats… |<[fieldᵢ patᵢⱼ …] …+>|
       (~seq (~either {~and fieldᵢ:id {~bind [(patᵢⱼ 1) (list)]}}
                      [fieldᵢ:id patᵢⱼ:expr …])
             …+)]

@chunk[#:save-as parse-field-value… |<call [fieldᵢ valueᵢ] …+>|
       (~seq [fieldᵢ:id valueᵢ:expr] …+
             {~global-or instance?}
             {~global-or no-types?}
             {~post-fail <values-error> #:when (attribute builder?)})]

@chunk[#:save-as parse-field-value-type… |<call [fieldᵢ valueᵢ : τᵢ] …+>|
       (~seq (~either [fieldᵢ:id valueᵢ:expr C:colon τᵢ:expr]
                      [fieldᵢ:id C:colon τᵢ:expr valueᵢ:expr])
             …+
             {~global-or instance?}
             {~global-or types?}
             {~post-fail <values-error> #:when (attribute builder?)})]

@chunk[#:save-as no-values-error <no-values-error>
       (~a "The #:instance keyword implies the use of [field value],"
           " [field : type value] or [field value : type].")]

@chunk[#:save-as values-error <values-error>
       (~a "The #:builder keyword implies the use of [field], field"
           " or [field : type].")]

@chunk[#:save-as empty-error <empty-err>
       (~a "If no fields are specified, then either #:builder or #:instance"
           " must be present")]

When called like a macro, @tc[tagged] accepts several syntaxes:
@itemlist[
 @item{@racket[(tagged name [fieldᵢ] …+)] or @racket[(tagged name fieldᵢ …+)],
  which return a builder function which has a type argument @racket[τᵢ]
  corresponding to each @racket[fieldᵢ]'s type.
  
  @(parse-field-instance…)

  This clause implies the creation of a builder function, so if 
  @racket[#:instance] is specified, the following error is raised:
  
  @(no-values-error)}
 @item{@racket[(tagged name [fieldᵢ valueᵢ] …+)], which returns an instance,
  inferring the type of the fields
  
  @(parse-field-colon-type…)

  This clause implies the creation of an instance, so if @racket[#:builder] is
  specified, the following error is raised:
  
  @(values-error)}
 @item{@racket[(tagged name [fieldᵢ : τᵢ] …+)], which returns a constructor with
  the given types
  
  @(parse-field-value…)

  This clause implies the creation of a builder function, so if
  @racket[#:instance] is specified, the following error is raised:
  
  @(no-values-error)}
 @item{@racket[(tagged name [fieldᵢ valueᵢ : τᵢ] …+)], which returns an instance
  using the given types
  
  @(parse-field-value-type…)

  This clause implies the creation of an instance, so if @racket[#:builder] is
  specified, the following error is raised:
  
  @(values-error)}]

@subsection{Call to @racket[tagged] with no fields:
 instance or constructor?}

A call to @tc[(tagged)] with no field is ambiguous: it could return a
constructor function for the structure with no fields, or an instance of that
structure.

@racketblock[(tagged)]

We tried to make a hybrid object using @tc[define-struct/exec] which would be an
instance of the structure with no fields, but could also be called as a function
(which would return itself). Unfortunately, this use case is not yet fully
supported by Typed/Racket: the instance cannot be annotated as a function type,
or passed as an argument to a higher-order function (Typed/Racket issue
@hyperlink["https://github.com/racket/typed-racket/issues/430"]{#430}). This can
be circumvented by using unsafe operations to give the instance its proper type
@tc[(Rec R (∩ (→ R) struct-type))], but Typed/Racket does not consider this type
applicable, and an annotation is required each time the instance is used as a
builder function (Typed/Racket issue
@hyperlink["https://github.com/racket/typed-racket/issues/431"]{#431}.

We therefore added two optional keywords, @tc[#:instance] and @tc[#:builder],
which can be used to disambiguate. They can also be used when fields
respectively with or without values are specified, so that macros don't need to
handle the empty structure as a special case.

@subsection{Signature for the @racket[tagged] call expander}

@chunk[#:save-as disambiguate-mixin <tagged-call-instance-or-builder-mixin>
       (define-eh-alternative-mixin tagged-call-instance-or-builder-mixin
         (pattern
          (~optional (~and instance-or-builder
                           (~or {~global-or instance? #:instance}
                                {~global-or builder? #:builder}))
                     #:name "either #:instance or #:builder")))]

@chunk[#:save-as fields-mixin <tagged-call-fields-mixin>
       (define-eh-alternative-mixin tagged-call-fields-mixin
         (pattern
          (~optional/else
           (~try-after name-order-point <name-after-field-error>
                       (~or |<call [fieldᵢ] …+>|
                            |<call [fieldᵢ : τᵢ] …+>|
                            |<call [fieldᵢ valueᵢ] …+>|
                            |<call [fieldᵢ valueᵢ : τᵢ] …+>|))
           #:defaults ([(fieldᵢ 1) (list)]
                       [(valueᵢ 1) (list)]
                       [(τᵢ 1) (list)])
           #:else-post-fail <empty-err> #:unless (or (attribute builder?)
                                                     (attribute instance?))
           #:name (~a "field or [field] or [field : type] for #:builder,"
                      " [field value] or [field : type value]"
                      " or [field value : type] for #:instance"))))]

@chunk[#:save-as ∀-mixin <∀-mixin>
       (define-eh-alternative-mixin ∀-mixin
         (pattern {~optional (~seq #:∀ ({~named-seq tvarᵢ :id …})
                                   (~global-or tvars?))}))]

@chunk[#:save-as name-id-mixin <name-id-mixin>
       (define-eh-alternative-mixin name-id-mixin
         (pattern
          (~once (~order-point name-order-point name:id))))]

@chunk[#:save-as name-after-field-error <name-after-field-error>
       "the name must appear before any field"]

When called as a macro, @tc[tagged] expects:

@itemlist[
 @item{The tagged structure's tag name:

  @(name-id-mixin)}
 @item{An optional list of type variables, preceded by @racket[#:∀]:

  @(∀-mixin)}
 @item{Either of the @racket[#:builder] or @racket[#:instance] options, or none:

  @(disambiguate-mixin)}
 @item{An optional list of fields, possibly annotated with a type, and possibly
  associated to a value:

  @(fields-mixin)

  When no fields are specified, the following error is raised unless either
  @racket[#:builder] or @racket[#:instance] is specified.

  @(empty-error)}]

The four elements can appear in any order, with one constraint: the name must
appear before the first field descriptor. Not only does it make more sense
semantically, but it also avoids ambiguities when the list of field names is
just a list of plain identifiers (without any type or value).

@(name-after-field-error)

We can now combine all four mixins.

@chunk[<tagged-call-args-mixin>
       (define-eh-alternative-mixin tagged-call-args-mixin
         #:define-splicing-syntax-class tagged-call-args-syntax-class
         (pattern {~mixin name-id-mixin})
         (pattern {~mixin tagged-call-instance-or-builder-mixin})
         (pattern {~mixin tagged-call-fields-mixin})
         (pattern {~mixin ∀-mixin}))]

@subsection{Implementation}

The call expander uses the low-level functions @tc[tagged-builder!],
@tc[tagged-∀-builder!] and @tc[tagged-infer-builder!] implemented in
@secref["Creating_instances_of_a_tagged_structure"
        #:tag-prefixes '("phc-adt/tagged-low-level")].
The first returns the syntax for a builder function for the given tagged
structure. The second returns the syntax for a polymorphic builder function for
the given tagged structure, using the given type variables which are bound
inside the field type declarations. The last returns the syntax for a
polymorphic builder function for the given tagged structure, with one type
parameter per field, which allows the type of each field to be inferred.

@chunk[<call-expander>
       (define/syntax-parse+simple
           (tagged-call-expander :tagged-call-args-syntax-class)
         <call-expander-cases>)]

If type variables are present, then @tc[tagged-∀-builder!] is used. Otherwise,
if types are specified, then @tc[tagged-builder!] is used, otherwise
@tc[tagged-infer-builder!] is used.

@chunk[<call-expander-cases>
       (define/with-syntax f
         (if (attribute tvars?)
             (tagged-∀-builder! #'((tvarᵢ …) name [fieldᵢ : τᵢ] …))
             (if (attribute types?)
                 (tagged-builder! #'(name [fieldᵢ τᵢ] …))
                 (tagged-infer-builder! #'(name fieldᵢ …)))))]

If the @tc[#:instance] keyword was specified, or if values are specified for
each field, the builder function is immediately called with those values, in
order to produce an instance of the tagged structure. Otherwise, the builder
function itself is produced.

@chunk[<call-expander-cases>
       (if (attribute instance?)
           #'(f valueᵢ …)
           #'f)]

@section{Type expander}

@chunk[#:save-as type-fields-mixin <tagged-type-fields-mixin>
       (define-eh-alternative-mixin tagged-type-fields-mixin
         (pattern
          (~optional
           (~try-after name-order-point <name-after-field-error>
                       (~named-seq field-declarations
                         (~or |<[fieldᵢ] …+>|
                              |<[fieldᵢ τᵢ] …+>|)))
           #:defaults ([(fieldᵢ 1) (list)]
                       [(τᵢ 1) (list)])
           #:name "field or [field] or [field type] or [field : type]")))]

When used as a type expander, @tc[tagged] expects:

@itemlist[
 @item{The tagged structure's tag name, as defined for the call expander in
  @racket[<name-id-mixin>]}
 @item{An optional list of type variables, as defined for the call expander in
  @racket[<∀-mixin>]}
 @item{An optional list of fields, possibly annotated with a type:

  @(type-fields-mixin)

  The main difference with the allowed field specifications for the call
  expander are that values are not allowed. Furthermore, the @racket[:] between
  a field and its type is optional:

  @(parse-field-type…)

  Furthermore, the @racket[instance?] and @racket[builder?] attributes are not
  meaningful for the type expander.

  @(parse-field…)}]

The three elements can appear in any order, with the same constraint as for the
call expander: the name must appear before the first field descriptor.

@(name-after-field-error)

@chunk[<tagged-type-args-mixin>
       (define-eh-alternative-mixin tagged-type-args-mixin
         #:define-splicing-syntax-class tagged-type-args-syntax-class
         (pattern {~mixin name-id-mixin})
         (pattern {~mixin tagged-type-fields-mixin})
         (pattern {~mixin ∀-mixin}))]

The type expander uses the low-level functions @tc[tagged-type!],
@tc[tagged-∀-type!] and @tc[tagged-infer-type!] implemented in
@secref["Type_of_a_tagged_structure"
        #:tag-prefixes '("phc-adt/tagged-low-level")]. The first
returns the syntax for the type for the given tagged structure. The second
returns the syntax for a polymorphic type for the given tagged structure, using
the given type variables which are bound inside the field type declarations. The
last returns the syntax for a polymorphic type for the given tagged structure,
with one type parameter per field, which allows the type of each field to be
inferred or filled in later.

@chunk[<type-expander>
       (define/syntax-parse+simple
           (tagged-type-expander :tagged-type-args-syntax-class)
         <type-expander-cases>)]

If type variables are present, then @tc[tagged-∀-type!] is used. Otherwise,
if types are specified, then @tc[tagged-type!] is used, otherwise
@tc[tagged-infer-type!] is used.

@chunk[<type-expander-cases>
       (if (attribute tvars?)
           (tagged-∀-type! #'((tvarᵢ …) name [fieldᵢ : τᵢ] …))
           (if (attribute types?)
               (tagged-type! #'(name [fieldᵢ τᵢ] …))
               (tagged-infer-type! #'(name fieldᵢ …))))]

@subsection{The @racket[TaggedTop] type}

The @tc[TaggedTop] type is extracted from the low-level @tc[TaggedTop-struct]
identifier (which is a struct identifier). The @tc[TaggedTop] type includes not
only tagged structures, but also nodes.

@chunk[<TaggedTop>
       (define-type TaggedTop TaggedTop-struct)]

Additionally, the @racket[TaggedTop?] predicate is simply aliased from the
low-level @racket[TaggedTop-struct?].

@; Do not use rename-out, as it confuses scribble (two documentations for one
@; identifier: the user-level documentation of the TaggedTop? function, and
@; the low-level documentation of the TaggedTop-struct struct.
@chunk[<TaggedTop>
       (define TaggedTop? TaggedTop-struct?)]

@section{Match expander}

@chunk[#:save-as match-fields-mixin <tagged-match-fields-mixin>
       (define-eh-alternative-mixin tagged-match-fields-mixin
         (pattern
          (~maybe/empty
           (~try-after name-order-point <name-after-field-error>
                       |<[fieldᵢ patᵢⱼ …] …+>|)
           #:name (~a "field or [field pat …]"))))]

@chunk[#:save-as no-implicit-mixin <tagged-match-no-implicit-bind-mixin>
       (define-eh-alternative-mixin tagged-match-no-implicit-bind-mixin
         (pattern (~optional (~global-or no-implicit #:no-implicit-bind))))]

When used as a match expander, @tc[tagged] expects:

@itemlist[
 @item{The tagged structure's tag name, as defined for the call expander in
  @racket[<name-id-mixin>]}
 @item{The @racket[#:no-implicit-bind], which specified that the field name
  should not automatically be bound by the match pattern to the field's
  contents:

  @(no-implicit-mixin)}
 @item{A (possibly empty) list of fields, each associated with zero or more
  patterns:

  @(match-fields-mixin)

  The main differences with the allowed field specifications for the call
  expander are that values and types are not allowed, but instead the field name
  may be followed by match patterns:

  @(parse-field-pats…)}]

The three elements can appear in any order, with the same constraint as for the
call expander: the name must appear before the first field descriptor.

@(name-after-field-error)

@chunk[<tagged-match-args-mixin>
       (define-eh-alternative-mixin tagged-match-args-mixin
         #:define-syntax-class tagged-match-args-syntax-class
         (pattern {~mixin name-id-mixin})
         (pattern {~mixin tagged-match-no-implicit-bind-mixin})
         (pattern {~mixin tagged-match-fields-mixin}))]

The match expander uses the low-level function @tc[tagged-match!] implemented in
@secref["Type_of_a_tagged_structure"
        #:tag-prefixes '("phc-adt/tagged-low-level")]. It returns
the syntax for a match pattern for the given tagged structure. The resulting
match pattern checks that the value is an instance of a tagged structure with
the given name and fields, and matches the value of each field against the
corresponding pattern.

@chunk[<match-expander>
       (define/syntax-parse+simple
           (tagged-match-expander . :tagged-match-args-syntax-class)
         <match-expander-body>)]

Unless @racket[#:no-implicit-bind] is specified, we include the field name as
part of the pattern, so that field names are bound to the field's contents.

@chunk[<match-expander-body>
       (if (attribute no-implicit)
           (tagged-match! #'(name [fieldᵢ (and patᵢⱼ …)] …))
           (tagged-match! #'(name [fieldᵢ (and fieldᵢ patᵢⱼ …)] …)))]

@section{Predicates for tagged structures}

@chunk[<tagged?>
       (define-syntax tagged?
         (syntax-parser
           [(_ name fieldᵢ:id …)
            (tagged-any-predicate! #'(name fieldᵢ …))]
           [(_ name [fieldᵢ:id :colon τᵢ:type] …)
            (tagged-predicate! #'(name [fieldᵢ τᵢ] …))]
           [(_ name [fieldᵢ:id predᵢ:type] …)
            (tagged-pred-predicate! #'(name [fieldᵢ predᵢ] …))]))]

@subsection{The @racket[TaggedTop?] predicate}

The @tc[TaggedTop?] predicate is simply re-provided. It is initially defined in
@secref["Common_ancestor_to_all_tagged_structures__TaggedTop-struct"
        #:tag-prefixes '("phc-adt/tagged-low-level")].

@chunk[|<provide TaggedTop?>|
       (provide (rename-out [TaggedTop-struct? TaggedTop?]))]

@section{Defining shorthands with @racket[define-tagged]}

The @tc[define-tagged] macro can be used to bind to an
identifier the type expander, match expander, predicate and
constructor function for a given tagged structure.

@chunk[#:save-as tag-kw-mixin <tag-kw-mixin>
       (define-eh-alternative-mixin tag-kw-mixin
         (pattern {~optional {~seq #:tag explicit-tag <default-tag-name>}}))]

@chunk[#:save-as tag-kw-mixin-default <default-tag-name>
       {~post-check
        {~bind [tag-name (or (attribute explicit-tag)
                             #'name)]}}]

@chunk[#:save-as predicate?-mixin <predicate?-mixin>
       (define-eh-alternative-mixin predicate?-mixin
         (pattern {~optional {~seq #:? predicate? <default-name?>}}))]

@chunk[#:save-as predicate?-mixin-default <default-name?>
       {~post-check
        {~bind [name? (or (attribute predicate?)
                          (format-id/record #'name "~a?" #'name))]}}]

The @tc[define-tagged] macro expects:

@itemlist[
 @item{The tagged structure's tag name, as defined for the call expander in
  @racket[<name-id-mixin>]}
 @item{An optional list of type variables, as defined for the call expander in
  @racket[<∀-mixin>]}
 @item{A possibly empty list of fields, possibly annotated with a type, as
  defined for the type expander in @racket[<tagged-type-fields-mixin>]}
 @item{Optionally, the tag name to be used, specified with
  @racket[#:tag tag-name]:

  @(tag-kw-mixin)

  The tag name defaults to @racket[_name], i.e. the identifier currently being
  defined.

  @(tag-kw-mixin-default)}
 @item{Optionally, a name for the predicate, specified with
  @racket[#:? predicate-name?]:
  
  @(predicate?-mixin)

  The predicate name defaults to @racket[_name?], where @racket[_name] is the
  identifier currently being defined.

  @(predicate?-mixin-default)}]

The five elements can appear in any order, with the same constraint as for the
call expander: the name must appear before the first field descriptor.

@chunk[<define-tagged-args-mixin>
       (define-eh-alternative-mixin define-tagged-args-mixin
         #:define-splicing-syntax-class define-tagged-args-syntax-class
         (pattern (~or {~mixin name-id-mixin}
                       {~mixin tag-kw-mixin}
                       {~mixin tagged-type-fields-mixin}
                       {~mixin predicate?-mixin}
                       {~mixin ∀-mixin})))]

The @tc[define-tagged] macro is then implemented using @racket[define-multi-id]:

@CHUNK[<define-tagged>
       (define-syntax/parse+simple
           (define-tagged :define-tagged-args-syntax-class)
         (define-temp-ids "~a/pat" (fieldᵢ …))
         (quasisyntax/top-loc stx
           (begin
             (define-multi-id name
               #:type-expander    (make-id+call-transformer
                                   #'<type-expander/define>)
               #:match-expander   <match-expander/define>
               #:else             <else-expander/define>)
             (define name?        <predicate/define>))))]

The type expander handles the same three cases as for @tc[tagged]: with type
variables, with a type for each field, or inferred.

@CHUNK[<type-expander/define>
       #,(if (attribute tvars?)
             (tagged-∀-type! #'((tvarᵢ …) tag-name [fieldᵢ τᵢ] …))
             (if (attribute types?)
                 (tagged-type! #'(tag-name [fieldᵢ τᵢ] …))
                 (tagged-infer-type! #'(tag-name fieldᵢ …))))]

The match expander is a short form of the one implemented for @tc[tagged], as it
takes only one positional pattern per field.

@chunk[<match-expander/define>
       (λ (stx2)
         (syntax-case stx2 ()
           [(_ fieldᵢ/pat …)
            (tagged-match! #'(tag-name [fieldᵢ fieldᵢ/pat] …))]
           (code:comment "Todo: implement a \"rest\" pattern")))]

Otherwise, when @racket[_name] is called as a function, or used as an identifier
on its own, we produce a builder function. When @racket[_name] is called as a
function, the builder function is applied immediately to the arguments,
otherwise the builder function itself is used. The same three cases as for
@tc[tagged] are handled: with type variables, with a type for each field, or
inferred.

@CHUNK[<else-expander/define>
       #'#,(if (attribute tvars?)
               (tagged-∀-builder!
                #'((tvarᵢ …) tag-name [fieldᵢ τᵢ] …))
               (if (attribute types?)
                   (tagged-builder! #'(tag-name [fieldᵢ τᵢ] …))
                   (tagged-infer-builder! #'(tag-name fieldᵢ …))))]

Finally, we define the predicate @racket[name?]. Contrarily to @racket[tagged?],
it does not take into account the field types, as we have no guarantee that
Typed/Racket's @racket[make-predicate] works for those. Instead, @racket[name?]
recognises any instance of a tagged structure with the given tag name and
fields. If a more accurate predicate is desired, it can easily be implemented
using @racket[tagged?].

@CHUNK[<predicate/define>
       #,(tagged-any-predicate! #'(tag-name fieldᵢ …))]

@section{Implementation of @racket[uniform-get]}

@racket[uniform-get] operates on tagged structures. It retrieves the desired
field from the structure, and forces it to obtain the actual value.

It is implemented as @racket[tagged-get-field] in
@secref["Accessing_fields_of_tagged_structures"
        #:tag-prefixes '("phc-adt/tagged-low-level")], and is
simply re-provided here.

@section{Putting it all together}

@chunk[<*>
       (require (for-syntax racket/base
                            racket/syntax
                            syntax/parse
                            phc-toolkit/untyped
                            syntax/strip-context
                            racket/function
                            extensible-parser-specifications
                            racket/format
                            type-expander/expander)
                phc-toolkit
                multi-id
                type-expander
                racket/promise
                "tagged-structure-low-level.hl.rkt"
                racket/format)

       @; Do not use rename-out, as it confuses scribble (two documentations for
       @; one identifier: the user-level documentation of uniform-get, and the
       @; low-level documentation of tagged-get-field.
       (define-syntax uniform-get
         (make-rename-transformer #'tagged-get-field))
       (define-syntax λuniform-get
         (make-rename-transformer #'λ-tagged-get-field))
       (provide uniform-get
                λuniform-get
                tagged
                tagged?
                define-tagged
                TaggedTop
                TaggedTop?
                
                (for-syntax tagged-call-args-syntax-class
                            tagged-call-expander-forward-attributes
                            tagged-call-expander
                  
                            tagged-type-args-syntax-class
                            tagged-type-expander-forward-attributes
                            tagged-type-expander
                  
                            tagged-match-args-syntax-class
                            tagged-match-expander-forward-attributes
                            tagged-match-expander

                            define-tagged-args-syntax-class
                            define-tagged-forward-attributes))
       
       (begin-for-syntax
         <∀-mixin>
         <name-id-mixin>
         <tagged-call-instance-or-builder-mixin>
         <tagged-call-fields-mixin>
         <tagged-call-args-mixin>
         <tagged-type-fields-mixin>
         <tagged-type-args-mixin>
         <tagged-match-fields-mixin>
         <tagged-match-no-implicit-bind-mixin>
         <tagged-match-args-mixin>

         <predicate?-mixin>
         <tag-kw-mixin>
         <define-tagged-args-mixin>)

       (begin-for-syntax
         <call-expander>
         <type-expander>
         <match-expander>)
       <tagged>
       <tagged?>
       <TaggedTop>
       <define-tagged>]

@;tagged-call-instance-or-builder-mixin
@;tagged-call-fields-mixin
@;tagged-call-args-mixin

@;tagged-type-fields-mixin
@;tagged-type-args-mixin

@;tagged-match-fields-mixin
@;tagged-match-no-implicit-bind-mixin
@;tagged-match-args-mixin

@;tag-kw-mixin
@;predicate?-mixin
@;define-tagged-args-mixin

@;name-id-mixin
@;∀-mixin
