#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require scribble-enhanced/doc
          racket/require
          hyper-literate
          (for-label (except-in phc-toolkit ?)
                     phc-toolkit/untyped-only
                     racket/list
                     racket/format
                     racket/promise
                     racket/string
                     racket/require
                     racket/set
                     remember
                     syntax/parse
                     typed-struct-props
                     typed/racket/unsafe
                     (subtract-in racket/contract typed/racket/base)
                     (subtract-in racket/syntax phc-toolkit)
                     (subtract-in syntax/stx phc-toolkit)
                     (except-in (subtract-in typed/racket/base
                                             racket/set)
                                values)
                     (only-in racket/base values)
                     "node-low-level.hl.rkt"))
@(unless-preexpanding
  (require (for-label (submod ".." sorting-and-identifiers)))
  (require (for-label (submod ".." pre-declare)))
  (require (for-label (submod ".."))))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "tagged-low-level"
       #:tag-prefix "phc-adt/tagged-low-level"
       ]{Low-level implementation of tagged structures}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/tagged-low-level"))

@(declare-exporting (lib "phc-adt/tagged-structure-low-level.hl.rkt"))

@(table-of-contents)

@section{Overview}

A tagged structure is a data structure associating fields with their value.
Two tagged structure types with the same set of fields can be distinguished by
their tag. Compared to the traditional algebraic data types, a tagged
structure acts like (traditional) structure wrapped in a (traditional)
constructor.

Tagged structures are the central data type of this library.
@itemlist[
 @item{Tagged structures can be used as-is.}
 @item{Constructors which tag multiple values can be created by aggregating
  those values, and storing them within a tagged structure containing a single
  field named ``@racketid[values]''.}
 @item{Untagged structures can be created by implicitly supplying a default tag,
  which is the same for all untagged structures. In our case, the default tag is
  named @racket[untagged].}
 @item{Nodes are implemented exactly like tagged structures, except that the
  contents of their fields are wrapped in promises. The promises allow creating
  data structures that contain cycles in appearance, despite being built
  exclusively using purely immutable primitives.}]

In order to implement field access in a way that works for tagged structures
and nodes alike, it is desirable that their implementation has the same shape.
We therefore also wrap the contents of tagged structure fields with promises.
While the promises present within nodes do perform some kind of computation
each time they are forced, the promises present within tagged structures
simply return an already-known value.

@section{Implementation using Racket structs}

A tagged structure is implemented as a Racket struct, in which every field has a
distinct polymorphic type.

@chunk[<define-tagged>
       (struct/props (fieldᵢ/τ …) tagged-struct common-struct ()
                     #:property prop:custom-write
                     (make-writer common-struct name fieldᵢ …)
               
                     #:property prop:equal+hash
                     (make-comparer common-struct tagged-struct name
                                    fieldᵢ …))]

Tagged structures with different tag names but the same set of fields are
implemented as descendant @racket[struct]s of a common one. The common
@racket[struct] contains all the fields, and the descendants only serve to
distinguish between the different tag names.

@chunk[<define-common>
       (struct/props (fieldᵢ/τ …) common-struct TaggedTop-struct
                     ([fieldᵢ : (Promise fieldᵢ/τ)] …))]

It is desirable that all data structures (tagged structures and nodes) have
the same shape. This makes it easier to access the value of a given field,
without having two different field access operators (one for tagged structure
and one for nodes). Since nodes need to have the contents of each field
wrapped within a @racket[Promise], we will also impose this on tagged
structures and their derivatives (untagged structures and constructors).
Although the promises used in nodes will actually perform some work, the
promises in other data structures will simply wrap an already-computed value.
The operator accessing a field's contents will therefore access the desired
field, and force the promise contained within, in order to obtain the real
value.

@subsection{Nodes as subtypes of their corresponding tagged struct type}

Nodes are implemented as subtypes of their corresponding tagged struct type.

@chunk[<define-node>
       (struct/props (fieldᵢ/τ … raw-D/τ raw-I/τ)
                     node-struct
                     tagged-struct
                     ([raw : (raw-node raw-D/τ raw-I/τ)])
                     #:property prop:custom-write
                     (make-node-writer common-struct
                                       name
                                       fieldᵢ …)
                     #:property prop:equal+hash
                     (make-node-comparer common-struct
                                         node-struct
                                         name
                                         fieldᵢ …))]

They contain an extra @racket[raw] field, which contains a raw representation of
the node consisting of a tuple of two elements: the graph's database of nodes,
and an index into that database).

@racketblock[
 (struct (Database) raw-node ([database : Database] [index : Index]))]

@section{Common ancestor to all tagged structures: @racket[TaggedTop-struct]}

@chunk[#:save-as taggedtop-decl <TaggedTop>
       (struct TaggedTop-struct () #:transparent)]

@defstruct[TaggedTop-struct ()]{
 We define the @racket[TaggedTop-struct] struct as the parent of every
 ``common'' struct.
 
 @(taggedtop-decl)

 The hierarchy is therefore as follows:

 @itemlist[
 @item{The @racket[struct] for a node is a subtype of the @racket[struct] for
   the tagged structure with the same name and fields.}
 @item{The @racket[struct] for a tagged structure is a subtype of the ``common''
   @racket[struct] which has the same set of fields. All tagged structures with
   the same fields but distinct tag names are implemented as subtypes of their
   ``common'' @racket[struct].}
 @item{@racket[TaggedTop-struct] is the direct supertype of all ``common''
   @racket[struct]. Transitively, @racket[TaggedTop-struct] is therefore also a
   supertype of the @racket[struct]s corresponding to every tagged structure and
   node.}]}

@section{Printing and comparing structures and nodes}

The data types defined in this library have a custom printed representation, and
have a custom implementation of equality.

The following sections present how tagged structures are printed and compared.
Nodes are described in a separate section,
@secref["node-low-level" #:tag-prefixes '("phc-adt/node-low-level")]. Their
behaviour differs slightly from how tagged structures are printed and
compared, as they need to take into account the presence of logical cycles in
the data structure. Node printing is explained in the section
@secref["Printing_nodes" #:tag-prefixes '("phc-adt/node-low-level")], and
node equality is explained in the section
@secref["Comparing_and_hashing_nodes"
        #:tag-prefixes '("phc-adt/node-low-level")].

@subsection{Printing tagged structures}

Tagged structures are printed in different ways depending on their fields:

@itemlist[
 @item{If the tagged structure only contains a single field whose name is
  ``@racketid[values]'', then it is printed as
  @racket[(constructor name value …)].}

 @item{Otherwise, if the tagged structure's tag name is @racket[untagged],
  it is printed as @racket[(structure name [field value] …)].}

 @item{Finally, it the tagged structure does not fall in the above two cases,
  it is printed as @racket[(tagged name [field value] …)].}]

@CHUNK[<custom-write>
       (define-syntax/parse (make-writer pid name fieldᵢ …)
         (define fields (map syntax-e (syntax->list #'(fieldᵢ …))))
         (define has-values-field? (member 'values fields))
         (define has-other-fields? (not (null? (remove 'values fields))))
         (define untagged? (eq? (syntax-e #'name) 'untagged))

         (define/with-syntax e
           (cond
             [untagged?
              #'(format "(structure ~a)"
                        (string-join (list <format-field> …) " "))]
             [(and has-values-field? (not has-other-fields?))
              #'`(constructor name
                              . ,(force ((struct-accessor pid values) self)))]
             [else
              #'(format "(tagged ~a ~a)"
                        'name
                        (string-join (list <format-field> …) " "))]))

         #'(λ (self out mode)
             (display e out)))]

Each field is formatted as @tc[[fieldᵢ valueᵢ]]. The whole printed form is
built so that copy-pasting it yields a value which is @racket[equal?] to the
original.

@chunk[<format-field>
       (format "[~a ~a]" 'fieldᵢ (force ((struct-accessor pid fieldᵢ) self)))]

@section{Comparing tagged structures}

Tagged structures are compared by recursively applying @racket[equal?] to their
fields, after forcing the promise wrapping each field. Forcing these promises is
safe, as the result of these promises is already known when creating the tagged
structure. The promises are present only to ensure that tagged structures and
nodes have the same shape, but cannot by themselves create logical cycles.

@CHUNK[<equal+hash>
       (define-syntax/parse (make-comparer pid id name fieldᵢ …)
         #'(list (λ (a b rec-equal?)
                   (and ((struct-predicate id) a)
                        ((struct-predicate id) b)
                        (rec-equal? (force ((struct-accessor pid fieldᵢ) a))
                                    (force ((struct-accessor pid fieldᵢ) b)))
                        …
                        #t))
                 (λ (a rec-hash)
                   (fxxor (rec-hash 'id)
                          (rec-hash (force ((struct-accessor pid fieldᵢ) a)))
                          …))
                 (λ (a rec-hash)
                   (fxxor (rec-hash 'id)
                          (rec-hash (force ((struct-accessor pid fieldᵢ) a)))
                          …))))]

@section{Pre-declaring structs}

@subsection{Why pre-declare the structs?}

We wish to pre-declare a Racket @tc[struct] type for all tagged structures used
in the program. This requirement is needed to achieve several goals:

@itemlist[
 @item{To allow on-the-fly declaration. Otherwise, it would be necessary to be
  in a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{module-begin
   context} to be able to declare a @racket[struct].@note{It is possible in
   untyped Racket to declare a struct within an 
   @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
    internal-definition context}, however it is not possible in Typed Racket due
   to
   @hyperlink["https://github.com/racket/typed-racket/issues/192"]{bug #192}.
   Furthermore, the declaration would not be visible outside the @racket[let].}
  This means that, within an expression, it would be impossible to create an
  instance of a structure which was not previously declared.}
 @item{To enable "interned" tagged structures, i.e. two tagged structures with
  the same name and fields used in two different files are compatible, just as
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{prefab} structs.}
 @item{If we use @code{(get-field s b)} in module @code{A}, and define a 
  @racket[struct] type with a field @code{b} in module @code{B}, then the module
  @code{A} would have to @racket[require] @code{B}, in order to have access to
  the struct metadata, and this could easily create cyclic dependencies.
  
  Moving the @racket[struct] definition to a third file solves that problem.}]

We do not however wish to remember the type of each field. Indeed, the type may
contain type identifiers which are not exported by the module using the tagged
structure. Instead, we declare parametric structs, using a distinct type
argument for each field. The struct can then be instantiated with the correct
types where needed.

@CHUNK[<pre-declare-all-tagged-structure-structs>
       (define-syntax (pre-declare-all-tagged-structure-structs stx)
         (define/with-parse (([name₁:id fieldᵢ:id …] [nameⱼ:id . _] …) …)
           (group-by (∘ list->set cdr)
                     <all-remembered-tagged-structures>
                     set=?))
         #`(begin
             (require (submod (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                              pre-declare)
                      phc-toolkit)
             (pre-declare-group [name₁ nameⱼ …] [fieldᵢ …])
             …))]

@CHUNK[<pre-declare-all-tagged-structure-structs>
       (define-syntax/parse (pre-declare-group [name:id …] [fieldᵢ:id …])
         
         (define/with-syntax common-struct
           (make-struct-identifier-common #f #'(fieldᵢ …)))
         
         (define-temp-ids "~a/τ" (fieldᵢ …))

         #'(begin
             <define-common>
             (provide (struct-out common-struct))
             
             (pre-declare-tagged-and-node common-struct name [fieldᵢ …])
             …))]

@CHUNK[<pre-declare-all-tagged-structure-structs>
       (define-syntax/case
           (pre-declare-tagged-and-node common-struct name (fieldᵢ …)) ()
         
         (define-temp-ids "~a/τ" (fieldᵢ …))
         (define-temp-ids "~a/pred" (fieldᵢ …))
         (define/with-syntax ([_ . Anyᵢ] …) #'([fieldᵢ . Any] …))
         (define/with-syntax tagged-struct
           (make-struct-identifier-tagged #f #'(name fieldᵢ …)))
         (define/with-syntax tagged-pred
           (make-struct-identifier-tagged-pred #f #'(name fieldᵢ …)))
         (define/with-syntax node-struct
           (make-struct-identifier-node #f #'(name fieldᵢ …)))

         (template (begin
                     <define-tagged>
                     <define-tagged-pred>
                     <define-node>
                     (provide tagged-pred
                              (struct-out tagged-struct)
                              (struct-out node-struct)))))]

@subsection{Remembering tagged structures across compilations}

In order to know which @tc[struct]s to pre-declare, we need
to remember them across compilations. We use the 
@tc[remember] library for that purpose.

@chunk[<all-remembered-tagged-structures>
       (set->list (begin (check-adt-context)
                         (get-remembered 'tagged-structure)))]

@chunk[<remember-structure!>
       (remember-write! 'tagged-structure
                        `(,(syntax-e #'name) . ,sorted-field-symbols))]

@deftogether[
 ([defform #:kind "for-syntax function"
   (check-remembered-common! #'(name fieldᵢ …))]
  [defform #:kind "for-syntax function"
   (check-remembered-tagged! #'(name fieldᵢ …))]
  [defform #:kind "for-syntax function"
   (check-remembered-node! #'(name fieldᵢ …))])]{
 These for-syntax functions check whether a tagged structure with the given name
 and fields has already been remembered, and return the common, tagged or node
 @racket[struct] identifier for that tagged structure. If the tagged structure
 has not yet been remembered, or if it was remembered for the first time during
 the current compilation, a delayed error is raised, and the function returns
 the @racket[struct] identifier for the @racket[not-remembered] tagged
 structure as a fallback, so that the current compilation may proceed as far as
 possible before the delayed error is triggered. The @racket[not-remembered]
 tagged structure has no fields, and is always available.

 The delayed error asks the user to re-compile the file, as new items have been
 remembered. The delayed error will be displayed after the file is expanded, but
 before it is type checked. If another compilation error happens while compiling
 the rest of the file, then the delayed error will not be displayed.}

@defform*[#:kind "for-syntax function"
          [(check-remembered-?! #'(name fieldᵢ …))]]{
 This for-syntax function checks whether a tagged structure with the given name
 and fields has already been remembered, and returns @racket[#t] in that case.
 If the tagged structure has not yet been remembered, or if it was remembered
 for the first time during the current compilation, a delayed error is raised
 and the function returns @racket[#f].}

If the name and set of fields were already remembered, all is fine and
we simply generate the corresponding @tc[struct] identifiers:

@chunk[<check-remembered!>
       (define-for-syntax/case-args (check-remembered! (name fieldᵢ …))
         (let* ([sorted-fields (sort-fields #'(fieldᵢ …))]
                [sorted-field-symbols (map syntax-e sorted-fields)])
           (when (check-duplicates sorted-field-symbols)
             (raise-syntax-error 'tagged-structure
                                 "Duplicate fields in structure descriptor"
                                 #f
                                 #f
                                 sorted-fields))
           (check-adt-context)
           (if (remembered? 'tagged-structure `(,(syntax-e #'name)
                                                . ,sorted-field-symbols))
               (values
                #t
                (make-struct-identifier-common #t sorted-fields)
                (make-struct-identifier-tagged #t `(,#'name . ,sorted-fields))
                (make-struct-identifier-node #t `(,#'name . ,sorted-fields)))
               <not-remembered>)))]
       
@chunk[<check-remembered!>
       (define-for-syntax (check-remembered-common! descriptor)
         (let-values ([(? common tagged node) (check-remembered! descriptor)])
           common))
       (define-for-syntax (check-remembered-tagged! descriptor)
         (let-values ([(? common tagged node) (check-remembered! descriptor)])
           tagged))
       (define-for-syntax (check-remembered-node! descriptor)
         (let-values ([(? common tagged node) (check-remembered! descriptor)])
           node))
       (define-for-syntax (check-remembered-?! descriptor)
         (let-values ([(? common tagged node) (check-remembered! descriptor)])
           ?))]

The @tc[struct] identifiers are generated as shown below.
Since their identifier is of the form 
@tc["(structure field₀ field₁ …)"], it contains the unusual
characters @tc["("] and @tc[")"]. This reduces the risk of
conflicts between @racket[struct] identifiers produced by
this library and user-declared identifiers (the structs
declared by this library normally have a fresh scope, but
due to bug #399 this is currently not possible).

@CHUNK[<make-struct-identifier-from-list>
       (define/contract? (make-struct-identifier-from-list ctx-introduce? lst)
         (-> boolean?
             (listof symbol?)
             identifier?)

         ((if ctx-introduce? ctx-introduce syntax-local-introduce)
          #`#,(string->symbol
               (~a lst))))]

@CHUNK[<make-struct-identifier-common>
       (define/contract? (make-struct-identifier-common ctx-introduce? fields)
         (-> boolean?
             (stx-list/c (listof identifier?))
             identifier?)
         
         (make-struct-identifier-from-list
          ctx-introduce?
          `(common . ,(map syntax-e (sort-fields fields)))))]
       
@CHUNK[<make-struct-identifier-tagged>
       (define/contract? (make-struct-identifier-tagged ctx-introduce?
                                                        name+fields)
         (-> boolean?
             (stx-list/c (cons/c identifier? (listof identifier?)))
             identifier?)
         
         (make-struct-identifier-from-list
          ctx-introduce?
          `(tagged ,(syntax-e (stx-car name+fields))
                   . ,(map syntax-e
                           (sort-fields (stx-cdr name+fields))))))]

@CHUNK[<make-struct-identifier-node>
       (define/contract? (make-struct-identifier-node ctx-introduce?
                                                      name+fields)
         (-> boolean?
             (stx-list/c (cons/c identifier? (listof identifier?)))
             identifier?)

         (make-struct-identifier-from-list
          ctx-introduce?
          `(node ,(syntax-e (stx-car name+fields))
                 . ,(map syntax-e
                         (sort-fields (stx-cdr name+fields))))))]

@CHUNK[<make-struct-identifier-tagged-pred>
       (define/contract?
           (make-struct-identifier-tagged-pred ctx-introduce?
                                               name+fields)
         (-> boolean?
             (stx-list/c (cons/c identifier? (listof identifier?)))
             identifier?)
         
         (make-struct-identifier-from-list
          ctx-introduce?
          `(tagged-cast-predicate
            ,(syntax-e (stx-car name+fields))
            . ,(map syntax-e
                    (sort-fields (stx-cdr name+fields))))))]

@subsection{Sorting the set of fields}

Some operations will need to obtain the Racket @tc[struct]
for a given set of fields. The fields are first sorted, in
order to obtain a canonical specification for the structure.

@chunk[<sort-fields>
       (define/contract? (sort-fields fields)
         (-> (stx-list/c (listof identifier?))
             (listof identifier?))
         
         (when (check-duplicates (stx->list fields) #:key syntax-e)
           (raise-syntax-error 'tagged-structure
                               "Duplicate fields in structure descriptor"
                               fields))
         (sort (stx->list fields)
               symbol<?
               #:key syntax-e))]

The @tc[sort-fields-alist] function will sort an associative
list where the keys are field identifiers. This allows us
later to sort a list of fields associated with their type,
for example.

@chunk[<sort-fields-alist>
       (define/contract? (sort-fields-alist fields-alist)
         (-> (stx-list/c (listof (stx-car/c identifier?)))
             (listof (stx-e/c (cons/c identifier? any/c))))
         
         (when (check-duplicates (map (λ~> stx-car stx-e)
                                      (stx->list fields-alist)))
           (raise-syntax-error 'structure
                               "Duplicate fields in structure description"
                               (stx-map stx-car fields-alist)))
         (sort (stx->list fields-alist)
               symbol<?
               #:key (λ~> stx-car stx-e)))]

@subsection{Not-yet-remembered structs should cause an error}

If the set of fields given to @tc[check-remember-structure!] is not already
known, it is remembered (i.e. written to a file by the
@racketmodname[remember] library), so that it will be known during the next
compilation. A delayed error is then set up, and a dummy @tc[struct]
identifier is returned (the struct identifier associated with the tagged
structure @racket[not-remembered], which does not have any field).

@chunk[<not-remembered>
       (begin <remember-structure!>
              (remembered-error! 'tagged-structure
                                 #'(name fieldᵢ …)
                                 (syntax->list #'(name fieldᵢ …)))
              (values
               #f
               (make-struct-identifier-common #t '())
               (make-struct-identifier-tagged #t `(,#'not-remembered))
               (make-struct-identifier-node #t `(,#'not-remembered))))]

The structure with no fields is pre-remembered so that it
is always available and can be returned in place of the
actual @tc[struct] when the requested set of fields has not
been remembered yet:

@chunk[<remember-empty-tagged-structure>
       (remembered! tagged-structure (not-remembered))]

Our goal is to let the file be macro-expanded as much as
possible before an error is triggered. That way, if the file
contains multiple structures which have not yet been
remembered, they can all be remembered in one compilation
pass, instead of stumbling on each one in turn.

We use the @racket[not-remembered] tagged structure as a fallback when a
structure is not already remembered. This is semantically incorrect, and
obviously would not typecheck, as the user code would expect a different type.
However, the delayed error is triggered @emph{before} the type checker has a
chance to run: the type checker runs on the fully-expanded program, and the
error is triggered while the program is still being macro-expanded.

The compilation may however fail earlier. For example, if a
reflective operation attempts to obtain a @tc[struct]'s
accessor for a given field, but that @tc[struct] corresponds
to a structure which was not yet remembered, then this
operation will fail at compile-time. All the primitive
operations implemented in this file should however work even
if the structure wasn't remembered, giving results which
will not typecheck but can still be expanded.

We additionally always declare a tagged structure with only the
``@racketid[values]'' field, as it is the base type for all constructors.

@chunk[<remember-one-constructor>
       (remembered! tagged-structure (always-remembered values))]

@section{Creating instances of a tagged structure}

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-builder! #'(name [fieldᵢ τᵢ] …))
         #:grammar ([name Identifier]
                    [tvarᵢ Identifier]
                    [fieldᵢ Identifier]
                    [τᵢ Type])]{
 This for-syntax function returns the syntax for a builder function for the
 given tagged structure. The builder function expects one parameter of type
 @racket[τᵢ] for each @racket[fieldᵢ].

 The builder function has the following type:

 @racketblock[(→ τᵢ … (tagged name [fieldᵢ τᵢ] …))]

 where @racket[(tagged name [fieldᵢ τᵢ] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ τᵢ] …))]

 This function also checks that a tag with the given name and fields has already
 been remembered, using @racket[check-remembered-tagged!]}

@CHUNK[<tagged-builder!>
       (define-for-syntax tagged-builder!
         (λ/syntax-case (name [fieldᵢ τᵢ] …) ()
           (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
           (define/with-syntax ([sorted-fieldⱼ . sorted-τⱼ] …)
             (sort-fields-alist #'([fieldᵢ . τᵢ] …)))
           (cond
             (code:comment "Can't use (inst st …) on a non-polymorphic type.")
             [(stx-null? #'(fieldᵢ …))
              #'st]
             (code:comment "Otherwise, re-order")
             [else
              #`(λ ([fieldᵢ : τᵢ] …)
                  ((inst st sorted-τⱼ …) (delay sorted-fieldⱼ) …))])))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-∀-builder! #'((tvarᵢ ...) name [fieldᵢ τᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier]
                    [tvarᵢ Identifier]
                    [τᵢ Type])]{
 This for-syntax function returns the syntax for a polymorphic builder function
 for the given tagged structure. The polymorphic builder function has the given
 @racket[tvarᵢ] type variables. The polymorphic builder function expects one
 parameter of type @racket[τᵢ] for each @racket[fieldᵢ], where @racket[τᵢ] can
 be a regular type or one of the @racket[tvarᵢ] type variables.

 The builder function has the following type:

 @RACKETBLOCK[(∀ (tvarᵢ …) (→ τᵢ … (tagged name [fieldᵢ τᵢ] …)))]

 where @racket[(tagged name [fieldᵢ τᵢ] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ τᵢ] …))]
 
 This function also checks that a tag with the given name and fields has already
 been remembered, using @racket[check-remembered-tagged!]}

@CHUNK[<tagged-∀-builder!>
       (define-for-syntax tagged-∀-builder!
         (λ/syntax-case ((tvarᵢ …) name [fieldᵢ τᵢ] …) ()
           (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
           (define/with-syntax ([sorted-fieldⱼ . sorted-τⱼ] …)
             (sort-fields-alist #'([fieldᵢ . τᵢ] …)))
           (cond
             [(stx-null? #'(tvarᵢ …))
              (tagged-builder! #'(name [fieldᵢ τᵢ] …))]
             (code:comment "Can't use (inst st …) on a non-polymorphic type.")
             [(stx-null? #'(fieldᵢ …))
              #`(λ #:∀ (tvarᵢ …) () (st))]
             (code:comment "Otherwise, re-order")
             [else
              #`(λ #:∀ (tvarᵢ …) ([fieldᵢ : τᵢ] …)
                  ((inst st sorted-τⱼ …) (delay sorted-fieldⱼ) …))])))]

@defform[#:kind "for-syntax function"
         (tagged-infer-builder! #'(name fieldᵢ …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier])]{
 This for-syntax function returns the syntax for a polymorphic builder function
 for the given tagged structure. The polymorphic builder function has one type
 variable for each field. The polymorphic builder function expects one parameter
 for each @racket[fieldᵢ], and infers the type of that field.

 The builder function has the following type:

 @RACKETBLOCK[(∀ (τᵢ …) (→ τᵢ … (tagged name [fieldᵢ τᵢ] …)))]

 where @racket[(tagged name [fieldᵢ τᵢ] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ τᵢ] …))]

 with a fresh @racket[τᵢ] identifier is introduced for each @racket[fieldᵢ].
 
 This function also checks that a tag with the given name and fields has already
 been remembered, using @racket[check-remembered-tagged!]}

@CHUNK[<tagged-infer-builder!>
       (define-for-syntax tagged-infer-builder!
         (λ/syntax-case (name fieldᵢ …) ()
           (define-temp-ids "~a/τ" (fieldᵢ …))
           (tagged-∀-builder! #'((fieldᵢ/τ …) name [fieldᵢ fieldᵢ/τ] …))))]

@section{Predicate for a tagged structure}

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-any-predicate! #'(name fieldᵢ …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier])]{
 This for-syntax function returns the syntax for a predicate for the given
 tagged structure. No check is performed on the contents of the structure's
 fields, i.e. the predicate has the following type:

 @RACKETBLOCK[(→ Any Boolean : (tagged name [fieldᵢ Any] …))]

 where @racket[(tagged name [fieldᵢ Any] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ Any] …))]

 In other words, it is a function accepting any value, and returning
 @racket[#t] if and only if the value is an instance of a structure with the
 given tag name and fields, regardless of the contents of those fields.
 Otherwise, @racket[#f] is returned.
 
 This function also checks that a tag with the given name and
 fields has already been remembered, using @racket[check-remembered-tagged!]}

@chunk[<tagged-any-predicate!>
       (define-for-syntax/case-args (tagged-any-predicate! (name fieldᵢ …))
         (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
         (define/with-syntax ([_ . Anyᵢ] …) #'([fieldᵢ . Any] …))
         #'(make-predicate (maybe-apply-type st Anyᵢ …)))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-any-fields-predicate #'name)
         #:grammar ([name Identifier])]{
 This for-syntax function returns the syntax for a predicate for any tagged
 structure with the given name. No check is performed on the structure's
 fields.}

@chunk[<tagged-any-fields>
       (define-for-syntax tagged-any-fields
         (λ/syntax-parse tag-name:id
           (map (λ (name+fields)
                  (with-syntax ([(name fieldᵢ …) name+fields])
                    (cons (check-remembered-tagged! #'(name fieldᵢ …))
                          name+fields)))
                (filter (λ (name+fields) (equal? (car name+fields)
                                                 (syntax-e #'tag-name)))
                        <all-remembered-tagged-structures>))))]

@CHUNK[<tagged-any-fields-predicate>
       (define-for-syntax tagged-any-fields-predicate
         (λ/syntax-parse tag-name:id
           #`(make-predicate #,(tagged-any-fields-type #'tag-name))))]

@subsection{A predicate over the contents of the fields}

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-predicate! #'(name [fieldᵢ τᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier]
                    [τᵢ Type])]{
 This for-syntax function returns the syntax for a predicate for the given
 tagged structure. The predicate also checks that each  @racket[fieldᵢ] is a
 value of the corresponding @racket[τᵢ] type. Each given @racket[τᵢ] must be
 a suitable argument for Typed Racket's  @racket[make-predicate].

 The predicate has the following type:

 @RACKETBLOCK[(→ Any Boolean : (tagged name [fieldᵢ τᵢ] …))]

 where @racket[(tagged name [fieldᵢ τᵢ] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ τᵢ] …))]

 In other words, it is a function accepting any value, and returning @racket[#t]
 if and only if the value is an instance of a structure with the given tag and
 fields, and each @racket[fieldᵢ] contains a value of the type @racket[τᵢ].
 Otherwise, @racket[#f] is returned. Note that the actual values contained
 within the fields are checked, instead of their static type (supplied or
 inferred when building the tagged structure instance).
 
 This function also checks that a tag with the given name and
 fields has already been remembered, using @racket[check-remembered-tagged!].}

Typed Racket's @racket[make-predicate] cannot operate on promises, because its
automatic contract generation would need to force the promise. This is a
potentially side-effectful operation that a predicate should not perform
automatically. In our case, we know that by construction the promises are side
effect-free. We therefore manually define a predicate builder. The returned
predicate forces the promises contained within each @racket[fieldᵢ], and
checks whether the resulting value is of the corresponding type @racket[τᵢ]:

@chunk[<tagged-pred-lambda>
       (λ (fieldᵢ/pred …)
         (λ ([v : Any])
           (and ((struct-predicate tagged-struct) v)
                (fieldᵢ/pred (force ((struct-accessor common-struct fieldᵢ) v)))
                …)))]

Unfortunately, Typed Racket's inference is not strong enough to properly
express the type of the predicate we build above; as of the time of writing
this library, it infers that when the predicate returns @racket[#true],
@racket[v] has the @racket[(tagged-struct Anyᵢ …)] type, and that its fields
have the respective @racket[fieldᵢ/τ] type. It also infers that when the
predicate returns false, one of these propositions must be false@note{These
 negative propositions cannot be written with the syntax currently supported by
 Typed Racket, but they are still shown by Typed Racket for debugging purposes
 in error messages, for example when trying to annotate the function with an
 incorrect proposition.}. However, it is not currently capable of combining
these pieces of information into a single proposition asserting that the type
of @racket[v] is @racket[(tagged-struct fieldᵢ/τ …)] if and only if the
predicate returns true. To circumvent this precision problem, we annotate the
predicate builder defined above with the most precise type that can be
expressed and automatically validated by Typed Racket:

@chunk[<tagged-pred-simple-type>
       (∀ (fieldᵢ/τ …)
          (→ (→ Any Boolean : fieldᵢ/τ)
             …
             (→ Any Boolean : #:+ (!maybe-apply tagged-struct Anyᵢ …))))]

We then use @racket[unsafe-cast]@note{It would be tempting to use the safe
 @racket[cast], but @racket[cast] enforces the type with a contract, which, in
 this case, cannot be generated by the current version of Typed Racket.} to
give the predicate the more precise type:

@chunk[<tagged-pred-correct-type>
       (∀ (fieldᵢ/τ …)
          (→ (→ Any Any : fieldᵢ/τ)
             …
             (→ Any Boolean : (!maybe-apply tagged-struct fieldᵢ/τ …))))]

@chunk[<define-tagged-pred>
       (define tagged-pred
         (unsafe-cast/no-expand (ann <tagged-pred-lambda>
                                     <tagged-pred-simple-type>)
                                <tagged-pred-correct-type>))]

Finally, we can define the @racket[tagged-predicate!] for-syntax function
described earlier in terms of this specialised predicate builder.

@; TODO: use a special make-predicate that recognizes other tagged
@; structure, so that a predicate for a tagged structure can reference
@; other tagged structures. Take care of cycles for nodes.
@chunk[<tagged-predicate!>
       (define-for-syntax/case-args (tagged-predicate! (name [fieldᵢ τᵢ] …))
         (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
         (define/with-syntax ([sorted-fieldⱼ . sorted-τⱼ] …)
           (sort-fields-alist #'([fieldᵢ . τᵢ] …)))
         (define/with-syntax st-make-predicate
           (make-struct-identifier-tagged-pred #t #'(name fieldᵢ …)))
         #'(st-make-predicate (make-predicate sorted-τⱼ) …))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-pred-predicate! #'(name [fieldᵢ predᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier]
                    [predᵢ (ExpressionOf (→ Any Any : τᵢ))])]{
 This for-syntax function returns the syntax for a predicate for the given
 tagged structure. The predicate also checks that each @racket[fieldᵢ] is
 accepted by the corresponding predicate @racket[predᵢ].

 When the type of a given @racket[predᵢ] includes a filter @racket[: τᵢ]
 asserting that it returns true if and only if the value is of type
 @racket[τᵢ], then the predicate produced by @racket[tagged-predicate!] will
 also have that filter on the corresponding field. By default, a function of
 type @racket[(→ Any Any)] will implicitly have the @racket[Any] filter, which
 does not bring any extra information. In other words, the @racket[(→ Any Any)]
 type in which no filter is specified is equivalent to the
 @racket[(→ Any Any : Any)] type, where @racket[: Any] indicates the filter.

 The generated predicate has therefore the following type:

 @RACKETBLOCK[(→ Any Boolean : (tagged name [fieldᵢ τᵢ] …))]

 where @racket[(tagged name [fieldᵢ τᵢ] …)] is the type produced by:

 @racketblock[(tagged-type! #'(name [fieldᵢ τᵢ] …))]

 In other words, it is a function accepting any value, and returning @racket[#t]
 if and only if the value is an instance of a structure with the given tag and
 fields, and each @racket[fieldᵢ] contains a value of the type @racket[τᵢ].
 Otherwise, @racket[#f] is returned. Note that the actual values contained
 within the fields are checked, instead of their static type (supplied or
 inferred when building the tagged structure instance).
 
 This function also checks that a tag with the given name and
 fields has already been remembered, using @racket[check-remembered-tagged!].}

@chunk[<tagged-pred-predicate!>
       (define-for-syntax/case-args
           (tagged-pred-predicate! (name [fieldᵢ predᵢ] …))
         (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
         (define/with-syntax ([sorted-fieldⱼ . sorted-predⱼ] …)
           (sort-fields-alist #'([fieldᵢ . predᵢ] …)))
         (define/with-syntax st-make-predicate
           (make-struct-identifier-tagged-pred #t #'(name fieldᵢ …)))
         #'(st-make-predicate sorted-predⱼ …))]

@section{Matching against tagged structures}

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-match! #'(name [fieldᵢ patᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier]
                    [patᵢ Match-Pattern])]{
 This for-syntax function returns the syntax for a match pattern for the given
 tagged structure. The pattern matches each @racket[fieldᵢ] against the
 corresponding @racket[patᵢ]. It also checks that a tag with the given name and
 fields has already been remembered, using @racket[check-remembered-tagged!]}

@chunk[<tagged-match!>
       (define-for-syntax/case-args (tagged-match! (name [fieldᵢ patᵢ] …))
         (define-values (was-remembered common-struct tagged-struct node-struct)
           (check-remembered! #'(name fieldᵢ …)))
         (define/with-syntax st tagged-struct)
         (define/with-syntax ([sorted-fieldⱼ . sorted-patⱼ] …)
           (sort-fields-alist #'([fieldᵢ . patᵢ] …)))
         (if was-remembered
             #'(struct st ((app force sorted-patⱼ) …))
             <match-not-remembered>))]

The match pattern @tc[(struct st (pat …))] fails to compile when the struct
@tc[st] is not declared, and when it does not have the right number of fields.
To avoid a confusing error message when the tagged structure was not
remembered yet, we insert a dummy pattern but still process the nested
patterns. This way, the nested patterns can themselves raise not-remembered
errors and cause new tagged structures to be remembered.

@chunk[<match-not-remembered>
       #'(app (λ (v) 'not-remembered) (and sorted-patⱼ …))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-anytag-match! #'([fieldᵢ patᵢ] …))
         #:grammar ([fieldᵢ Identifier]
                    [patᵢ Match-Pattern])]{
                                           
 This for-syntax function returns the syntax for a match pattern for any
 tagged structure with the given fields, regardless of the tagged structure's
 tag. The pattern matches each @racket[fieldᵢ] against the corresponding
 @racket[patᵢ]. It also checks that a tag with a dummy name (@racket[any-tag])
 and the given fields has already been remembered, using
 @racket[check-remembered-tagged!]}

@; TODO: get rid of the any-tag

@chunk[<tagged-anytag-match!>
       (define-for-syntax/case-args (tagged-anytag-match! ([fieldᵢ patᵢ] …))
         (define-values (was-remembered common-struct tagged-struct node-struct)
           (check-remembered-tagged! #'(any-tag fieldᵢ …)))
         (define/with-syntax st common-struct)
         (define/with-syntax ([sorted-fieldⱼ . sorted-patⱼ] …)
           (sort-fields-alist #'([fieldᵢ . patᵢ] …)))
         (if was-remembered
             #'(struct st ((app force sorted-patⱼ) …))
             <match-not-remembered>))]

@section{Type of a tagged structure}

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-type! #'(name [fieldᵢ τᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier])]{
 This for-syntax function returns the syntax for the type of tagged structures
 with the given name and field types. It also checks that a tag with the given
 name and fields has already been remembered, using
 @racket[check-remembered-tagged!]}

@chunk[<tagged-type!>
       (define-for-syntax tagged-type!
         (λ/syntax-case (name [fieldᵢ τᵢ] …) ()
           (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
           (define/with-syntax ([sorted-fieldⱼ . sorted-τⱼ] …)
             (sort-fields-alist #'([fieldᵢ . τᵢ] …)))
           (code:comment "Can't instantiate a non-polymorphic type.")
           (if (stx-null? #'(fieldᵢ …))
               #'st
               #'(st sorted-τⱼ …))))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-∀-type! #'((tvarᵢ …) name [fieldᵢ τᵢ] …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier])]{
 This for-syntax function returns the syntax for a polymorphic type for the
 given tagged structure, using the given type variables @racket[tvarᵢ…]. It also
 checks that a tag with the given name and fields has already been remembered,
 using @racket[check-remembered-tagged!]}

@CHUNK[<tagged-∀-type!>
       (define-for-syntax tagged-∀-type!
         (λ/syntax-case ((tvarᵢ …) name [fieldᵢ τᵢ] …) ()
           (define/with-syntax st (check-remembered-tagged! #'(name fieldᵢ …)))
           (define/with-syntax ([sorted-fieldⱼ . sorted-τⱼ] …)
             (sort-fields-alist #'([fieldᵢ . τᵢ] …)))
           (cond
             [(stx-null? #'(tvarᵢ …))
              (tagged-type! #'(name [fieldᵢ τᵢ] …))]
             (code:comment "Can't instantiate a non-polymorphic type.")
             [(stx-null? #'(fieldᵢ …))
              #`(∀ (tvarᵢ …) st)]
             (code:comment "Otherwise, re-order")
             [else
              #`(∀ (tvarᵢ …) (st sorted-τⱼ …))])))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-infer-type! #'(name fieldᵢ …))
         #:grammar ([name Identifier]
                    [fieldᵢ Identifier])]{
 This for-syntax function returns the syntax for a polymorphic type for the
 given tagged structure, with one automatically-generated type variable per
 field. It also checks that a tag with the given name and fields has already
 been remembered, using @racket[check-remembered-tagged!]}

@chunk[<tagged-infer-type!>
       (define-for-syntax tagged-infer-type!
         (λ/syntax-case (name fieldᵢ …) ()
           (define-temp-ids "~a/τ" (fieldᵢ …))
           (tagged-∀-type! #'((fieldᵢ/τ …) name [fieldᵢ fieldᵢ/τ] …))))]

@defform[#:kind "for-syntax function"
         #:literals (:)
         (tagged-any-fields-type #'name)
         #:grammar ([name Identifier])]{
 This for-syntax function returns the syntax for the union type of all tagged
 structures with the given name. The type of each field is @racket[Any].}

@CHUNK[<tagged-any-fields-type>
       (define-for-syntax tagged-any-fields-type
         (λ/syntax-parse tag-name:id
           (define/with-syntax ([sᵢ nameᵢ fieldᵢⱼ …] …)
             (tagged-any-fields #'tag-name))
           (define/with-syntax ([[_ Anyᵢⱼ] …] …)
             #'([[fieldᵢⱼ Any] …] …))
           #`(U . #,(stx-map (λ (sᵢ Anyᵢⱼ*) (if (stx-null? Anyᵢⱼ*)
                                                sᵢ
                                                #`(#,sᵢ . #,Anyᵢⱼ*)))
                             #'(sᵢ …)
                             #'([Anyᵢⱼ …] …)))))]

@section{Accessing fields of tagged structures}

@defform[(tagged-get-field v f)]{
 Returns the value contained within the @racket[f] field of the tagged
 structure instance @racket[v]. }

@CHUNK[<tagged-get-field>
       (define-syntax (tagged-get-field stx)
         (syntax-case stx ()
           [(_ v f . else-expr)
            (identifier? #'f)
            (let ()
              (define/with-syntax else-expr-or-error
                (syntax-case #'else-expr ()
                  [() (if (identifier? #'v)
                          #`(typecheck-fail #,stx #:covered-id v)
                          #`(typecheck-fail #,stx))]
                  [(e) #'e]))
              (define/with-syntax ([sⱼ all-fieldⱼₖ …] …)
                (has-fields/common #'(f)))
              #'(let ([v-cache v])
                  (cond
                    [((struct-predicate sⱼ) v-cache)
                     (force ((struct-accessor sⱼ f) v))]
                    …
                    [else else-expr-or-error])))]))]

@defform[(λ-tagged-get-field f)]{
 Returns an accessor for the @racket[f] field of any tagged structure instance.
 The instance must contain a field named @racket[f], otherwise a type error is
 raised at compile-time, when using the accessor on an inappropriate value.
}

@CHUNK[<λ-tagged-get-field>
       (define-syntax/parse (λ-tagged-get-field f:id)
         (define/with-syntax ([sⱼ all-fieldⱼₖ …] …)
           (has-fields/common #'(f)))
         #`(λ #:∀ (τ) ([v : #,(has-fields/type #'([f τ]))])
             (cond [((struct-predicate sⱼ) v)
                    (force ((struct-accessor sⱼ f) v))]
                   …)))]

@section{Row polymorphism}

Row polymorphism, also known as "static duck typing" is a type system feature
which allows a single type variable to be used as a place holder for several
omitted fields, along with their types. The @racketmodname[phc-adt] library
supports a limited form of row polymorphism: for most operations, a set of
tuples of omitted field names must be specified, thereby indicating a bound on
the row type variable.

This is both an limitation of our implementation (to reduce the combinatorial
explosion of possible input and output types), as well as a desirable feature.
Indeed, this library is intended to be used to write compilers, and a compiler
pass should have a precise knowledge of the intermediate representation it
manipulates. It is possible that a compiler pass may operate on several
similar intermediate representations (for example a full-blown representation
for actual compilation and a minimal representation for testing purposes),
which makes row polymorphism desirable. It is however risky to allow as an
input to a compiler pass any data structure containing at least the minimum
set of required fields: changes in the intermediate representation may add new
fields which should, semantically, be handled by the compiler pass. A
catch-all row type variable would simply ignore the extra fields, without
raising an error. Thanks to the bound which specifies the possible tuples of
omitted field names, changes to the the input type will raise a type error,
bringing the programmer's attention to the issue. If the new type is legit,
and does not warrant a modification of the pass, the fix is easy to implement:
simply adding a new tuple of possibly omitted fields to the bound (or
replacing an existing tuple) will allow the pass to work with the new type.
If, on the other hand, the pass needs to be modified, the type system will
have successfully caught a potential issue.

This section presents the implementation of the features which allow a limited
form of row polymorphism, as well as structural subtyping.

@subsection{Type for any tagged structure containing a given set of fields}

@defproc[#:kind "for-syntax function"
         (has-fields [stx-fields (syntax/c (listof identifier?))])
         (listof (cons/c identifier?
                         (cons/c identifier?
                                 (listof identifier?))))]{
                                    
 Returns a list of tagged structures which have all of the given fields. Each
 tagged structure list with the low-level struct's id as the first element, the
 tag name as the second element, followed by the whole list of fields which
 belong to that tagged structure.}

@chunk[<has-fields>
       (define-for-syntax has-fields
         (λ/syntax-case (fieldᵢ …) ()
           (map (λ (t+fields)
                  (with-syntax ([(tag fieldᵢ …) t+fields])
                    (list* (make-struct-identifier-common #t #'(fieldᵢ …))
                           #'tag
                           (sort-fields #'(fieldᵢ …)))))
                (filter (λ (s)
                          (andmap (λ (f) (member f (cdr s)))
                                  (syntax->datum #'(fieldᵢ …))))
                        <all-remembered-tagged-structures>))))]

@defform[#:kind "for-syntax function"
         (has-fields/common #'(fieldᵢ …))]{
 Returns a list of ``common'' structs which have all of the given fields. Each
 ``common'' struct is represented as a pair of the struct's id and the whole
 list of fields which belong to that tagged structure.}

@chunk[<has-fields>
       (define-for-syntax (has-fields/common stx-fields)
         (remove-duplicates (map (λ (s) (cons (car s) (cddr s)))
                                 (has-fields stx-fields))
                            free-identifier=?
                            #:key car))]

@defform[#:kind "for-syntax function"
         (has-fields/type #'([fieldᵢ τᵢ] …))]{                                  
 Returns the syntax for the union type of several ``common'' structs. Each
 tagged structure has all of the given fields, and each @racket[fieldᵢ] is of
 the corresponding type @racket[τᵢ]. The other extra fields which are not part
 of the @racket[#'([fieldᵢ τᵢ] …)] specification have the @racket[Any] type.}

@chunk[<has-fields/type>
       (define-for-syntax has-fields/type
         (λ/syntax-case ([fieldᵢ τᵢ] …) ()
           (define/with-syntax ((sⱼ all-fieldⱼₖ …) …)
             (has-fields/common #'(fieldᵢ …)))
           (define/with-syntax ((all-field-τⱼₖ …) …)
             (template
              ([(!cdr-assoc #:default Any all-fieldⱼₖ [fieldᵢ . τᵢ] …) …] …)))
           #'(U (maybe-apply-type sⱼ all-field-τⱼₖ …) …)))]

@subsection{Changing the tag of a tagged structure}

@defform[(change-tag instance [(tagᵢ fieldᵢⱼ …) new-tagᵢ] …)]{ The
 @racket[change-tag] macro takes an instance of a tagged structure, and
 produces a new tagged structure instance with a different tag name. The
 @racket[instance]'s type must be one of @racket[(tagged tagᵢ fieldᵢⱼ …) …].
 The new instance will contain the same fields as the original, but its tag
 name will be the @racket[new-tagᵢ] corresponding to the input's type.}

@CHUNK[<change-tag>
       (define-syntax/case (change-tag [(tagᵢ fieldᵢⱼ …) new-tagᵢ] …)
         <change-tag-factored-out>
         #`(cond #,(stx-map <change-tag-case>
                            #'([tagᵢ (fieldᵢⱼ …) new-tagᵢ]))))]

@chunk[<change-tag-factored-out>
       (define old-s (check-remembered-tagged! #'(tag fieldⱼₛ)))]

@chunk[<change-tag-case>
       (λ/syntax-case (tag (fieldⱼ …) new-tag) ()
         (define/with-syntax (fieldⱼₛ …) (sort-fields #'(fieldⱼ …)))
         (define new-s (check-remembered-tagged! #'(new-tag fieldⱼₛ)))
         #'[((struct-predicate old-s) instance)
            ((struct-constructor new-s)
             ((struct-accessor new-s fieldⱼₛ) instance) …)])]

@subsection{Splitting a tagged structure}

@defform[#:literals (: U)
         (split instance : (U (tagᵢ fieldᵢⱼ …) …) requestedₖ …)]{
 The @racket[split] macro splits a tagged structure into two tagged
 structures. The first contains the @racket[requestedₖ …] fields, while the
 second contains all other fields. The two new tagged structures have the same
 tag as the original instance. This can however be altered later on using
 @racket[change-tag].

 The expression generated by @racket[split] produces two values, one for each
 new tagged structure.

 Since the type of the @racket[_instance] is not known at compile-time, this
 form requires that the user specify a union of possible tagged structure
 types. In theory, it would be possible to use the list of all tagged
 structures, but this would result in a @racket[cond] testing over a large
 number of impossible cases.

 The @racketmodname[trivial] library could help by tracking the type of
 expressions in simple cases. That information could then be used to infer the
 list of possible tagged structures. The explicit annotation would then become
 mandatory only when the type could not be inferred.}

@; TODO: should split be allowed for nodes ?

The @racket[split] macro generates a @racket[cond] form, with one clause for
each possible instance type. In each @racket[cond] clause, the
@racket[requestedₖ …] and the other fields are separated into two different
tagged structures, the first .

@CHUNK[<split>
       (define-syntax split
         (syntax-parser
           #:literals (U)
           [(_ instance :colon (U (~and τᵢ (tagᵢ fieldᵢⱼ …)) …) requestedₖ …)
            <split-check>
            <split-compute-extra-fields>
            <split-case-factored-out>
            #`(cond
                #,@(stx-map <split-case> #'([tagᵢ (extra-fieldᵢₗ …)] …)))]))]

The @racket[split] macro first computes the set of
extra fields for each possible input type:

@chunk[<split-compute-extra-fields>
       (define/with-syntax ((extra-fieldᵢₗ …) …)
         (stx-map (λ (x)
                    (free-id-set->list
                     (free-id-set-subtract x requested-id-set)))
                  instance-id-sets))]

It then generates a cond clause for each possible input type, which tests
whether the instance belongs to that type. If it is the case, then the body of
the clause

@chunk[<split-case-factored-out>
       (define/with-syntax (requestedₖₛ …) (sort-fields #'(requestedₖ …)))]

@chunk[<split-case>
       (λ/syntax-case (tag (extraₗ …)) ()
         (define/with-syntax (extraₗₛ …) (sort-fields #'(extraₗ …)))
         (define/with-syntax s-requested (check-remembered-tagged! #'(tag requestedₖ …)))
         (define/with-syntax s (check-remembered-tagged! #'(tag requestedₖ … extraₗ …)))
         (define/with-syntax c (check-remembered-common! #'(tag requestedₖ … extraₗ …)))
         (define/with-syntax s-extra (check-remembered-tagged! #'(tag extraₗ …)))
         (code:comment "the generated cond clause:")
         #'[((struct-predicate s) instance)
            (values ((struct-constructor s-requested)
                     ((struct-accessor c requestedₖₛ) instance) …)
                    ((struct-constructor s-extra)
                     ((struct-accessor c extraₗₛ) instance) …))])]

The argument-verification code for @racket[split] is given below. It uses
@racket[immutable-free-id-set]s to quickly compute the set of identifiers
present within @racket[requestedₖ …] but missing from one of the
@racket[fieldᵢⱼ …] tuples.

@chunk[<split-check>
       (define instance-id-sets
         (stx-map (∘ immutable-free-id-set syntax->list) #'((fieldᵢⱼ …) …)))
       
       (define requested-id-set
         (immutable-free-id-set (syntax->list #'(requestedₖ …))))
       
       (for ([τ (in-syntax #'(τᵢ …))]
             [instance-id-set instance-id-sets])
         (let ([missing (free-id-set-subtract requested-id-set
                                              instance-id-set)])
           (unless (free-id-set-empty? missing)
             <split-error>)))]

If there are such missing identifiers, the macro raises an error, otherwise
the computation proceeds normally:

@chunk[<split-error>
       (raise-syntax-error
        'split
        (format "The requested fields ~a are missing from the instance type ~a"
                (free-id-set->list missing)
                τ)
        this-syntax
        τ
        (free-id-set->list missing))]

@defform[(split/type #'((U (tagᵢ [fieldᵢⱼ τᵢⱼ] …) …) requestedₖ …))]{
 We also define a @racket[split/type] for-syntax function, which returns the
 syntax for the union type of the extra fields of a @racket[split] operation,
 i.e. the type of the second value produced by @racket[split].}

@CHUNK[<split>
       (define-for-syntax split/type
         (syntax-parser
           #:literals (U)
           [((U {~and τᵢ (tagᵢ [fieldᵢⱼ τᵢⱼ] …)} …) requestedₖ …)
            <split-check>
            (define/with-syntax (([extra-fieldᵢₗ . extra-τᵢₗ] …) …)
              (for/list ([field+τⱼ… (in-syntax #'(([fieldᵢⱼ . τᵢⱼ] …) …))])
                (~for/list ([($stx [field . τ]) (in-syntax field+τⱼ…)]
                            #:unless (free-id-set-member? requested-id-set
                                                          #'field))
                           #'[field . τ])))
            #`(U #,@(stx-map tagged-type! #'([tagᵢ (extra-fieldᵢₗ …)] …)))]))]

@subsection{Merging two tagged structures}

@defform[#:literals (U :)
         (merge instance-a instance-b
                : (U [(tag-aᵢ field-aᵢⱼ …) (tag-bₖ field-bₖₗ …)] …))]{
 The @racket[merge] macro merges two tagged structures into a single one. The
 resulting structure will contain all the fields
 @racket[field1ᵢⱼ … field2ₖₗ …], and will have the same tag as
 @racket[instance1] (although the tag can be changed later on using
 @racket[change-tag]).

 Since the type of @racket[_instance1] and @racket[_instance2] is not known at
 compile-time, this form requires that the user specify a union of possible
 tagged structure types for both instances. In theory, it would be possible to
 use the list of all tagged structures, but the resulting @racket[cond] would
 test for each possible pair of tagged structure types. In other words, the
 number of pairs of types to account for would be the Cartesian product of all
 tagged structures used in the program. Clearly, this is not a viable solution.

 The @racketmodname[trivial] library could help by tracking the type of
 expressions in simple cases. That information could then be used to infer the
 list of possible tagged structures. The explicit annotation would then become
 mandatory only when the type could not be inferred.

 If the @racketmodname[trivial] library were to be used, node types should be
 excluded. Indeed, the node types rely on the fact that they cannot be
 constructed outside of a graph to provide useful guarantees (e.g. the
 possibility to map over all nodes of a given type contained within a graph).}

@CHUNK[<merge>
       (define-syntax merge
         (syntax-parser
           #:literals (U)
           [(_ instance-a instance-b
               :colon [U [(~and τ-a (tag-aᵢ field-aᵢⱼ …))
                          (~and τ-b (tag-bₖ field-bₖₗ …))] …])
            #`(cond
                #,@(stx-map <merge-case> #'([(τ-a tag-aᵢ field-aᵢⱼ …)
                                             (τ-b tag-bₖ field-bₖₗ …)]
                                            …)))]))]

@; TODO: refactor to avoid the `and` within the cond clauses, as TR might not
@; handle it well. Instead, use nested conds, and group by (tag-aᵢ field-aᵢⱼ …)

@CHUNK[<merge-case>
       (λ/syntax-case [(τ-a tag-a field-aⱼ …) (τ-b tag-b field-bₗ …)] ()
         <merge-check>
         (define/with-syntax s-a (check-remembered-tagged! #'(tag-a field-aⱼ …)))
         (define/with-syntax c-a (check-remembered-common! #'(tag-a field-aⱼ …)))
         (define/with-syntax s-b (check-remembered-tagged! #'(tag-b field-bₗ …)))
         (define/with-syntax c-b (check-remembered-common! #'(tag-b field-bₗ …)))
         (define/with-syntax (field-aⱼₛ …) (sort-fields #'(field-aⱼ …)))
         (define/with-syntax (field-bₗₛ …) (sort-fields #'(field-bₗ …)))
         (define s-new (check-remembered-tagged!
                        #'(tag-a field-aⱼₛ … field-bₗₛ …)))
         #`[(and ((struct-predicate s-a) instance-a)
                 ((struct-predicate s-b) instance-b))
            (#,(tagged-infer-builder! #'(tag-a field-aⱼₛ … field-bₗₛ …))
             (force ((struct-accessor c-a field-aⱼₛ) instance-a))
             …
             (force ((struct-accessor c-b field-bₗₛ) instance-b))
             …)])]

@chunk[<merge-check>
       (define fields-a-id-set
         (immutable-free-id-set (syntax->list #'(field-aⱼ …))))
       (define fields-b-id-set
         (immutable-free-id-set (syntax->list #'(field-bₗ …))))
       (let ([intersection (free-id-set-intersect fields-a-id-set
                                                  fields-b-id-set)])
         (unless (free-id-set-empty? intersection)
           <merge-error>))]

@chunk[<merge-error>
       (raise-syntax-error
        'merge
        (format "The fields ~a are present in both tagged structures ~a and ~a"
                (free-id-set->list intersection)
                #'τ-a
                #'τ-b)
        this-syntax
        #'τ-a
        (free-id-set->list intersection))]

@defform[(merge/type #'(U [(tag-aᵢ [field-aᵢⱼ τ-aᵢⱼ] …)
                           (tag-bᵢ [field-bᵢⱼ τ-bᵢⱼ] …)] …))]{
 We also define a @racket[merge/type] for-syntax function, which returns the
 syntax for the union type of the extra fields of a @racket[split] operation,
 i.e. the type of the second value produced by @racket[split].}

@CHUNK[<merge>
       (define-for-syntax merge/type
         (syntax-parser
           #:literals (U)
           [(U [(~and τ-a (tag-aᵢ field-aᵢⱼ …))
                (~and τ-b (tag-bₖ field-bₖₗ …))] …)
            #`(U #,@(stx-map <merge-type-case>
                             #'([tag-aᵢ field-aᵢⱼ … field-bₖₗ …] …)))]))]

@CHUNK[<merge-type-case>
       (λ/syntax-case [(τ-a tag-a field-aⱼ …) (τ-b tag-b field-bₗ …)] ()
         <merge-check>
         (tagged-type! #'[tag-a field-aⱼ … field-bₗ …]))]

@subsection{Updating a tagged structure}

@defform[#:literals (U :)
         (with+ instance : (U (tagᵢ fieldᵢⱼ …) …)
               [new-field value] …)]{
 The @racket[with+] macro produces a tagged structure instance containing the
 same fields as @racket[instance], extended with the given @racket[new-field]s.
 None of the @racket[new-field …] must be present in the original
 @racket[instance]. 

 Since the type of the @racket[_instance] is not known at compile-time, this
 form requires that the user specify a union of possible tagged structure types
 for the instance. In theory, it would be possible to use the list of all
 tagged structures, but the resulting @racket[cond] would test for a large
 number of impossible cases.

 The @racketmodname[trivial] library could help by tracking the type of
 expressions in simple cases. That information could then be used to infer the
 list of possible tagged structures. The explicit annotation would then become
 mandatory only when the type could not be inferred.

 If the @racketmodname[trivial] library were to be used, node types should be
 excluded. Indeed, the node types rely on the fact that they cannot be
 constructed outside of a graph to provide useful guarantees (e.g. the
 possibility to map over all nodes of a given type contained within a graph).
 Instead, the normal tagged structure with the same name and fields can be
 returned.}


@CHUNK[<with+>
       (define-syntax/parse (with+ instance
                                  :colon (U {~and τᵢ (tagᵢ fieldᵢⱼ …)} …)
                                  [new-fieldₖ valueₖ] …)
         <with+-check>
         #'(with! instance : (U (tagᵢ fieldᵢⱼ …) …) [new-fieldₖ valueₖ] …))]

@chunk[<with+-check>
       (define instance-id-sets
         (stx-map (∘ immutable-free-id-set syntax->list) #'([fieldᵢⱼ …] …)))
       (define new-fields-id-set
         (immutable-free-id-set (syntax->list #'(new-fieldₖ …))))
       (for ([τ (in-syntax #'(τᵢ …))]
             [instance-id-set instance-id-sets])
         (let ([intersection (free-id-set-intersect new-fields-id-set
                                                    instance-id-set)])
           (unless (free-id-set-empty? intersection)
             <with+-error>)))]

@chunk[<with+-error>
       (raise-syntax-error
        'with+
        (format "The new fields ~a are already present in the instance type ~a"
                (map syntax->datum (free-id-set->list intersection))
                (syntax->datum τ))
        this-syntax
        τ
        (free-id-set->list intersection))]

@defform[#:literals (U :)
         (with! instance : (U (tagᵢ fieldᵢⱼ …) …)
                [updated-field value] …)]{
 Like @racket[with+], but this version allows overwriting fields, i.e. the
 @racket[updated-field]s may already be present in the @racket[instance].
 Although the @racket[!] is traditionally used in Racket to indicate operations
 which mutate data structures, in this case it merely indicates that the given
 fields may exist in the original instance. Since a fresh updated copy of the
 original instance is created, this operation is still pure.

 The same restrictions concerning nodes apply.}


@CHUNK[<with!>
       (define-syntax with!
         (syntax-parser
           #:literals (U)
           [(_ instance :colon (U (tagᵢ fieldᵢⱼ …) …) [updated-fieldₖ valueₖ] …)
            #`(cond
                #,@(stx-map <with!-case> #'([tagᵢ fieldᵢⱼ …] …)))]))]

@CHUNK[<with!-case>
       (λ/syntax-case (tag fieldⱼ …) ()
         (define/with-syntax old-s (check-remembered-tagged! #'(tag fieldⱼ …)))
         (define/with-syntax old-c (check-remembered-common! #'(tag fieldⱼ …)))
         (define field→value
           (make-free-id-table
            (stx-map syntax-e <with!-fieldⱼ-assoc>)))
         <with!-fieldⱼ-overwritten>
         (define/with-syntax ([fieldₗ . maybe-overwrittenₗ] …)
           (free-id-table-map field→value cons))
         #`[((struct-predicate old-s) instance)
            (#,(tagged-infer-builder! #'(tag fieldₗ …)) maybe-overwrittenₗ …)])]

The implementation works by initially mapping every @racket[fieldⱼ] identifier
to its value in the original instance:

@chunk[<with!-fieldⱼ-assoc>
       #'([fieldⱼ . (force ((struct-accessor old-c fieldⱼ) instance))] …)]

The entries corresponding to an @racket[updated-fieldₖ] are then overwritten
in the table:

@chunk[<with!-fieldⱼ-overwritten>
       (for ([updated-field (in-syntax #'(updated-fieldₖ …))]
             [value (in-syntax #'(valueₖ …))])
         (free-id-table-set! field→value updated-field value))]

@defform[#:literals (U :)
         (with!! instance : (U (tagᵢ fieldᵢⱼ …) …)
                 [updated-field value] …)]{
 Like @racket[with!], but checks that all the given fields are already present
 in the original instance. In other words, it does not change the type of the
 instance, and merely performs a functional update of the given fields. This
 version works on a much smaller set of types (namely those containing all the
 given fields), so the annotation is optional.

 The same restrictions concerning nodes apply.}

@; no update allowed for nodes
@; include only the types which have all of the given fields
@chunk[<with!!>
       (define-syntax with!!
         (syntax-parser
           (code:comment "Auto-detect the set of tagged structures containing")
           (code:comment "all the updated fields.")
           [(_ instance
               [updated-fieldₖ valueₖ] …)
            #:with ([sᵢ tagᵢ fieldᵢⱼ …] …) (has-fields #'(updated-fieldₖ …))
            #'(with! instance : (U (tagᵢ fieldᵢⱼ …) …)
                     [updated-fieldₖ valueₖ] …)]
           (code:comment "Use an explicit list of tagged structures containing")
           (code:comment "all the updated fields.")
           [(_ instance :colon (U {~and τᵢ (tagᵢ fieldᵢⱼ …)} …)
               [updated-fieldₖ valueₖ] …)
            <with!!-check>
            #'(with! instance : (U (tagᵢ fieldᵢⱼ …) …)
                     [updated-fieldₖ valueₖ] …)]))]

@chunk[<with!!-check>
       (define instance-id-sets
         (stx-map (∘ immutable-free-id-set syntax->list) #'([fieldᵢⱼ …] …)))
       (define updated-id-set
         (immutable-free-id-set (syntax->list #'(updated-fieldₖ …))))
       (for ([instance-id-set instance-id-sets]
             [τ (in-syntax #'(τᵢ …))])
         (let ([missing (free-id-set-subtract updated-id-set
                                              instance-id-set)])
           (unless (free-id-set-empty? missing)
             <with!!-error>)))]

@chunk[<with!!-error>
       (raise-syntax-error
        'with!!
        (format "The updated fields ~a are not present in the instance type ~a"
                (map syntax->datum (free-id-set->list missing))
                (syntax->datum τ))
        this-syntax
        τ
        (free-id-set->list missing))]

@defproc[#:kind "for-syntax function"
         (tagged-struct-id? [id any/c])
         (or/c #f
               (cons/c (or/c 'tagged 'node)
                       (cons/c identifier
                               (listof identifier))))]{
 The @racket[tagged-struct-id?] expects an identifier. When the @racket[id] is
 an identifier which refers to a @racket[struct] definition corresponding to a
 tagged structure, @racket[tagged-struct-id?] returns a list containing the
 tagged structure's tag name and fields, prefixed with either @racket['tagged]
 or @racket['node], depending on whether the given struct id corresponds to a
 tagged structure's struct, or to a node's struct. Otherwise,
 @racket[tagged-struct-id?] returns @racket[#false].
 
 This can be used to recognise occurrences of tagged structures within
 fully-expanded types.}

@CHUNK[<tagged-struct-id?>
       (define-for-syntax tagged-struct-ids-cache #f)
       (define-for-syntax (tagged-struct-id? id)
         <tagged-struct-ids-init-cache>
         (and (identifier? id)
              (free-id-table-ref tagged-struct-ids-cache id #f)))]

The @racket[tagged-struct-id] function uses a free-identifier table which
associates struct identifiers to their corresponding tag name and fields
(prefixed with @racket['tagged] or @racket['node]). The table is initialised
when @racket[tagged-struct-id?] is called for the first time. It could not be
initialised beforehand, as the @racket[adt-init] macro needs to be called by the
user code first.

@chunk[<tagged-struct-ids-init-cache>
       (unless tagged-struct-ids-cache
         (set! tagged-struct-ids-cache
               (make-immutable-free-id-table
                (append-map (λ (s)
                              (list (list* (make-struct-identifier-tagged #t s)
                                           'tagged
                                           s)
                                    (list* (make-struct-identifier-node #t s)
                                           'node
                                           s)))
                            <all-remembered-tagged-structures>))))]

@section{Putting it all together}

The low-level implementation of algebraic data types is split into three
modules: @tc[sorting-and-identifiers], @tc[pre-declare] and the main module.
Furthermore, the section
@secref["node-low-level" #:tag-prefixes '("phc-adt/node-low-level")],
implemented as a separate file, contains the implementation details for printing
and comparing nodes.

@chunk[<*>
       <module-sorting-and-identifiers>
       <module-pre-declare>
       <main-module>]

The @tc[sorting-and-identifiers] module contains the utility functions related
to sorting fields (to obtain a canonical representation of the tagged structure
descriptor), and the functions which derive the @tc[struct] identifiers for
tagged structures, nodes and the ``common'' supertype of all tagged structures
which share the same set of fields. These @tc[struct] identifiers are derived
from the list of field names and the tag name.

@chunk[<module-sorting-and-identifiers>
       (module sorting-and-identifiers racket/base
         (require racket/list
                  racket/format
                  racket/contract
                  phc-toolkit/untyped
                  (for-template "ctx.hl.rkt"))

         (provide make-struct-identifier-common
                  make-struct-identifier-tagged
                  make-struct-identifier-node
                  make-struct-identifier-tagged-pred
                  sort-fields
                  sort-fields-alist)

         <sort-fields>
         <sort-fields-alist>
         <make-struct-identifier-from-list>
         <make-struct-identifier-common>
         <make-struct-identifier-tagged>
         <make-struct-identifier-node>
         <make-struct-identifier-tagged-pred>)]

The @tc[pre-declare] submodule contains everything which concerns the
pre-declaration of structs. It also uses the printer and comparer for nodes from
@secref["node-low-level" #:tag-prefixes '("phc-adt/node-low-level")].

@CHUNK[<module-pre-declare>
       (module pre-declare typed/racket/base
         (require racket/promise
                  racket/string
                  racket/require
                  phc-toolkit
                  remember
                  typed-struct-props
                  "node-low-level.hl.rkt"
                  "ctx.hl.rkt"
                  (only-in type-expander unsafe-cast/no-expand)
                  (for-syntax racket/base
                              racket/syntax
                              racket/list
                              racket/set
                              racket/function
                              (subtract-in syntax/stx phc-toolkit/untyped)
                              syntax/parse
                              syntax/parse/experimental/template
                              syntax/strip-context
                              phc-toolkit/untyped))
         (require (for-syntax (submod ".." sorting-and-identifiers)))

         (provide (struct-out TaggedTop-struct)
                  pre-declare-all-tagged-structure-structs
                  pre-declare-group)

         (begin-for-syntax
           (define-template-metafunction !maybe-apply
             (λ (stx)
               (syntax-case stx ()
                 [(_ t) #'t]
                 [(_ t . args) #'(t . args)]))))
         
         <remember-empty-tagged-structure>
         <remember-one-constructor>
         <TaggedTop>
         <custom-write>
         <equal+hash>
         <pre-declare-all-tagged-structure-structs>)]

The main module contains all the code related to remembering the tagged
structures across compilations. It also contains many for-syntax functions
which, given the tag name and fields of a tagged structure, produce syntax for
that tagged structure's builder function, type, predicate and match pattern.

@chunk[<main-module>
       (require phc-toolkit
                remember
                racket/promise
                (submod "." pre-declare)
                type-expander
                "ctx.hl.rkt"
                (for-syntax racket/base
                            racket/syntax
                            racket/list
                            racket/set
                            racket/function
                            phc-toolkit/untyped
                            syntax/parse
                            syntax/parse/experimental/template
                            syntax/id-set
                            syntax/id-table
                            generic-bind
                            (submod "." sorting-and-identifiers)))

       (provide (for-syntax tagged-builder!
                            tagged-∀-builder!
                            tagged-infer-builder!
                            tagged-type!
                            tagged-∀-type!
                            tagged-infer-type!
                            tagged-predicate!
                            tagged-pred-predicate!
                            tagged-any-predicate!
                            tagged-match!
                            tagged-anytag-match!
                            check-remembered-common!
                            check-remembered-tagged!
                            check-remembered-node!
                            check-remembered-?!
                            has-fields
                            has-fields/common
                            has-fields/type
                            tagged-any-fields-type
                            tagged-any-fields-predicate
                            split/type
                            merge/type
                            tagged-struct-id?)
                tagged-get-field
                λ-tagged-get-field
                split
                merge
                with+
                with!
                with!!)

       (provide (all-from-out (submod "." pre-declare)))

       <check-remembered!>
       <tagged-builder!>
       <tagged-∀-builder!>
       <tagged-infer-builder!>
       <tagged-any-fields>
       <tagged-type!>
       <tagged-∀-type!>
       <tagged-infer-type!>
       <tagged-any-fields-type>
       <tagged-predicate!>
       <tagged-pred-predicate!>
       <tagged-any-predicate!>
       <tagged-any-fields-predicate>
       <tagged-match!>
       <tagged-anytag-match!>
       <has-fields>
       <has-fields/type>
       <tagged-get-field>
       <λ-tagged-get-field>
       <split>
       <merge>
       <with+>
       <with!>
       <with!!>
       <tagged-struct-id?>]
