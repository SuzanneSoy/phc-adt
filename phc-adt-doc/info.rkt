#lang info
(define collection 'multi)
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "hyper-literate"
                     "phc-adt-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "typed-racket-lib"
                     "scribble-enhanced"
		     "scribble-math"
                     "type-expander"
                     "xlist"
                     "alexis-util"
                     "extensible-parser-specifications"
                     "multi-id"
                     "phc-toolkit"
                     "remember"
                     "threading"
                     "trivial"
                     "typed-struct-props"
                     "datatype"))
(define pkg-desc "Algebraic Datatypes tailored for writing compilers (documentation only)")
(define version "1.1")
(define pkg-authors '("Suzanne Soy"))
