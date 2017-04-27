#lang scribble/manual

@title{Algebraic Data Types for compilers: Implementation}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

This library is implemented using literate programming. The implementation
details are presented in the following sections. The user documentation is in
the @other-doc['(lib "phc-adt/scribblings/phc-adt.scrbl")] document.

@(table-of-contents)

@include-section[(submod (lib "phc-adt/adt.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/tagged-structure-low-level.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/node-low-level.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/ctx.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/tagged.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/tagged-supertype.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/structure.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/constructor.hl.rkt") doc)]
@include-section[(submod (lib "phc-adt/variant.hl.rkt") doc)]
@include-section[(submod "phc-adt-choices.scrbl" doc)]