#lang hyper-literate typed/racket/base
@(require scribble-enhanced/doc
          scribble-math
          racket/sandbox
          scribble/example
          (for-label typed/racket/base
                     phc-toolkit
                     phc-toolkit/untyped-only
                     datatype
                     "ctx.hl.rkt"
                     "tagged.hl.rkt"
                     "tagged-supertype.hl.rkt"
                     "structure.hl.rkt"
                     "constructor.hl.rkt"
                     "variant.hl.rkt"
                     "adt-init.rkt"))
@doc-lib-setup

@title[#:style (with-html5 manual-doc-style)
       #:tag "adt"
       #:tag-prefix "phc-adt/adt"]{Algebraic Data Types}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/adt"))

@(table-of-contents)

@section{A note on polysemy}

The name ``constructor'' usually designates two things:
@itemlist[
 @item{A tagged value, like the ones created or accessed using the
  @racket[constructor] macro defined in
  @secref["constructor" #:tag-prefixes '("phc-adt/constructor")]}
 @item{A constructor function or macro for some kind of data structure, which
  is the function or macro used to create instances of that data structure.}]

Since this could lead to ambiguities, we clarify by saying ``constructor'' in
the former case, and ``builder'' or ``builder function'' in the latter case.

@section[#:tag "ADT|introduction"]{Introduction}

We define variants (tagged unions), with the following constraints:

@; TODO: put a short usage example here

@itemlist[
 @item{A constructor is described by a tag name, followed by zero or more
  values. Likewise, a tagged structure is described by a tag name, followed by
  zero or more field names, each field name being mapped to a value.}
 @item{Two different variants can contain the same constructor or tagged
  structure, and it is not possible to create an instance of that constructor or
  tagged structure that would belong to one variant but not the other.}
 @item{Constructors and tagged structures are "anonymous": it is not necessary
  to declare a constructor or tagged structure before creating instances of it,
  expressing its type, or using it as a match pattern.}
 @item{Constructors types and tagged structures types are "interned": two
  constructors with the same tag name have the same type, even if they are used
  in different files. The same applies to two tagged structures with the same
  tag name and field names: even if they are used in different files, they have
  the same type.}]

The @racketmodname[datatype] package by Andrew Kent also
implements Algebraic Data Types. The main differences are that unlike our
library, data structures have to be declared before they are used (they are
not "anonymous"), and a given constructor name cannot be shared by multiple
unions, as can be seen in the example below where the second
@tc[define-datatype] throws an error:

@(define tr-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'typed/racket)))
@examples[#:eval tr-evaluator
          (require datatype)
          
          (define-datatype Expr
            [Var (Symbol)]
            [Lambda (Symbol Expr)]
            [App (Expr Expr)])
          
          ;; define-datatype: variant type #<syntax:11:3 Var> already bound
          ;; in: Simple-Expr
          (eval:error
           (define-datatype Simple-Expr
             [Var (Symbol)]
             [Lambda (Symbol Expr)]))]

@section{Remembered data types and pre-declarations}

This library works by remembering all the constructors and all the tagged
structures across compilations. More precisely, each constructor's tag name is
written to a file named @filepath{adt-pre-declarations.rkt} in the same
directory as the user code. The tag name and list of fields of each tagged
structure is also written in the same file.

The generated @racket{adt-pre-declarations.rkt} file declares a
@racket[struct] for each tagged structure and constructor, so that all user
files which @racket[require] the same @racket{adt-pre-declarations.rkt} will
share the same @racket[struct] definitions.

User files which make use of the @racketmodname[phc-adt] should include a call
to @racket[adt-init] before using anything else. The @racket[adt-init] macro
@racket[require]s the @filepath{adt-pre-declarations.rkt} file, and records
the lexical context of that @racket[require], so that the other macros
implemented by this library can fetch the pre-declared @racket[struct] types
from the correct lexical scope. The @filepath{ctx.hl.rkt} file takes care of
recording that lexical scope, while @filepath{adt-init.rkt} performs the
initialisation sequence (creating the @filepath{adt-pre-declarations.rkt} file
if it does not exist, loading the pre-declared @racket[struct] types from
@filepath{adt-pre-declarations.rkt}, and using a utility from
@filepath{ctx.hl.rkt} to record the lexical context).

@chunk[<require-modules>
       (require "ctx.hl.rkt")]

@section{The initialisation process}

The initialisation process can be somewhat complex: the directives
@racket[(remember-output-file "adt-pre-declarations.rkt")],
@racket[(set-adt-context)] and @racket[(require "adt-pre-declarations.rkt")]
have to be inserted in the right order, and the file
@filepath{adt-pre-declarations.rkt} has to be initialised with the appropriate
contents when it does not exist. The @racket[adt-init] macro defined in
@filepath{"adt-init.rkt"} takes care of these steps.

@chunk[<require-modules>
       (require "adt-init.rkt")]

The generated @filepath{adt-pre-declarations.rkt} file will call the
@racket[pre-declare-all-tagged-structure-structs] macro defined in
@filepath{tagged-structure-low-level.hl.rkt}.

@section{Tagged structures, untagged structures, constructors, and variants}

We first define a low-level interface for tagged structures in the
@filepath{tagged-structure-low-level.hl.rkt} file. This low-level interface
includes for-syntax functions for expressing the type of tagged structures,
creating builder functions for them, as well as match patterns. It also includes
means to access the value of a given field on any tagged structure which
contains that field. The @filepath{tagged.hl.rkt} file provides syntactic sugar
for this low-level interface, and defines the @racket[tagged] identifier, which
acts as a type expander, match expander and macro. The macro can be used to
create builder functions which return instances of tagged structures, or to
directly create such instances.

@chunk[<require-modules>
       (require "tagged.hl.rkt")]

The @filepath{"tagged-supertype.hl.rkt"} file defines a few operations
implementing some form of "static duck typing": As a type expander,
@racket[(tagged-supertype fieldᵢ …)] expands to the union type of all tagged
structures containing a superset of the given set of fields. As a match
expander, @racket[(tagged-supertype [fieldᵢ patᵢⱼ …] …)] expands to a match
pattern which accepts any tagged structure with a superset of the given set of
fields, as long as the value of each @racket[fieldᵢ] matches against all of the
corresponding @racket[patᵢⱼ …].

@chunk[<require-modules>
       (require "tagged-supertype.hl.rkt")]

We then define untagged structures, which are tagged structures with the
@racket[untagged] tag name. Untagged structures can be used conveniently when
the tag name is not important and the goal is simply to map a set of field names
to values. The @filepath{structure.hl.rkt} file defines the @tc[structure]
type expander, match expander and macro. The @tc[structure] identifier acts as
a simple wrapper around @racket[tagged] which supplies @racket[untagged] as the
tag name.

@chunk[<require-modules>
       (require "structure.hl.rkt")]

Constructors are defined as tagged structures containing a single field, called
@racket[values]. The @racket[constructor] macro, defined in
@filepath{"constructor.hl.rkt"} accepts a rich syntax for creating constructor
instances containing multiple values, associated with the tag name. The values
are aggregated in a list, which is stored within the @racket[values] field of
the tagged structure used to implement the constructor. The @racket[constructor]
identifier is therefore nothing more than syntactic sugar for @racket[tagged].
It relies on the @racketmodname[xlist] library, which provides a rich syntax for
expressing the complex list types, as well as the corresponding match pattern.

@chunk[<require-modules>
       (require "constructor.hl.rkt")]

For convenience, we write a @tc[variant] form, which is a thin wrapper against
the union type of several constructors and tagged structures,
@tc[(U constructor-or-tagged …)].

@chunk[<require-modules>
       (require "variant.hl.rkt")]

Finally, we directly include the row polymorphism features from
@filepath{tagged-structure-low-level.hl.rkt}:

@chunk[<require-modules>
       (require "tagged-structure-low-level.hl.rkt")]

@;{Finally, we define a @tc[uniform-get] form, which can
operate on @tc[tagged] structures. We also wrap the plain 
@tc[structure] form so that it instead returns a tagged
structure, using a common tag for all plain structures. This
allows us to rely on the invariant that @tc[uniform-get]
always operates on data with the same shape (a constructor
whose single value is a promise for a structure)@note{This
 avoids the risk of combinatorial explosion for the input
 type of @racket[uniform-get], when accessing a deeply nested
 field: allowing
 @racket[(U structure
            (constructor structure)
            (constructor (Promise structure)))]
 would result in a type of size @${n⁴}, with @${n} the depth
 of the accessed field.}}

@chunk[<*>
       (begin <require-modules>)
       (provide adt-init
                tagged
                tagged?
                define-tagged
                TaggedTop
                TaggedTop?
                tagged-supertype
                tagged-supertype*
                
                structure
                structure?
                define-structure
                StructureTop
                StructureTop?
                structure-supertype
                
                constructor
                constructor?
                define-constructor
                ConstructorTop
                ConstructorTop?
                
                variant
                define-variant

                constructor-values
                uniform-get

                split
                #;(for-syntax split/type)
                merge
                #;(for-syntax merge/type)
                with+
                with!
                with!!
                #;(for-syntax tagged-struct-id?))]