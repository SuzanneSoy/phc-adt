#lang scribble/manual

@(require racket/require
          (for-label typed/racket/base
                     phc-adt
                     (lib "phc-adt/tagged.hl.rkt")
                     (lib "phc-adt/structure.hl.rkt")
                     (lib "phc-adt/constructor.hl.rkt")
                     (lib "phc-adt/variant.hl.rkt")
                     (lib "phc-adt/tagged-supertype.hl.rkt")
                     (lib "phc-adt/adt-init.rkt"))
          scribble-enhanced/doc
          scribble-math
          (subtract-in scribble/struct scribble-enhanced/doc)
          scribble/decode)
@doc-lib-setup

@title{Algebraic Data Types for compilers}

@defmodule[phc-adt
           #:use-sources
           [(lib "phc-adt/tagged.hl.rkt")
            (lib "phc-adt/structure.hl.rkt")
            (lib "phc-adt/constructor.hl.rkt")
            (lib "phc-adt/variant.hl.rkt")
            (lib "phc-adt/tagged-supertype.hl.rkt")
            (lib "phc-adt/adt-init.rkt")]]

This library is implmented using literate programming. The
implementation details are presented in 
@other-doc['(lib "phc-adt/scribblings/phc-adt-implementation.scrbl")].

This library defines @tech{tagged structures}, @tech{untagged structures},
@tech{constructors} and @tech{variants}. While uses of Racket's
@racket[struct] need the struct to be declared before they are used, the
structures implemented by this library can be used anonymously, without
declaring them in advance. All uses of a tagged structure with the same tag
name and set of fields are equivalent, even if they are declared in separate
files. It is also possible to access a field on a tagged structure instance
without specifying the tagged structure's tag name. This is used in a separate
library (not published yet) to implement the dotted notation commonly used in
object-oriented languages for field accesses, @racket[instance.field].

This library works by saving across compilations the list of all tagged
structures used across the program. The tag name and list of field names for
each tagged structure is written to a file. That file is, by default, located
in the same directory as the source file, and it is called
@filepath{adt-pre-declarations.rkt}, but this can be changed using the optional
argument to @racket[adt-init]. These "remembered" tagged structure definitions
are used to pre-declare the @racket[struct]s corresponding to each tagged
structure, so that all the code using the same
@filepath{adt-pre-declarations.rkt} file sees the same structure definitions.

@defform*[[(adt-init)
           (adt-init pre-declarations-file)]
          #:contracts
          [(pre-declarations-file string?)]]{
 The @racket[(adt-init)] macro has to be called in a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{module context},
 after the @racket[(require phc-adt)] but before using any of the identifiers
 provided by this library.

 The @racket[pre-declarations-file] (which defaults to @filepath{
  adt-pre-declarations.rkt}) should be the string representation of a path,
 relative to the directory containing the file which uses @racket[adt-init].
 
 The @racket[adt-init] macro creates the @racket[pre-declarations-file] if it
 does not exist, and @racket[require]s it. The @racket[pre-declarations-file]
 contains a list of remembered tagged structures (specifically, the tag name
 followed by a list of field names). It uses the @hash-lang[]
 @racketmodname[phc-adt/declarations] which pre-declares the @racket[struct]s
 corresponding to each remembered tagged structure.

 It then makes all the identifiers defined by this library available (the
 identifiers cannot otherwise be used before calling @racket[adt-init]).}

@defmodulelang[phc-adt/declarations #:no-declare]{
 This @hash-lang[] is used by the @racket[_pre-declarations-file] (see
 @racket[adt-init]), and is not intended to be used in other situations.}


@(table-of-contents)

@include-section{phc-adt-tagged.scrbl}
@include-section{phc-adt-structure.scrbl}
@include-section{phc-adt-constructor.scrbl}
@include-section{phc-adt-variant.scrbl}