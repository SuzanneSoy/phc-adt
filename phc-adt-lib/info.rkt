#lang info
(define collection 'multi)
(define deps '("base"
               "typed-racket-lib"
               "hyper-literate"
               "multi-id"
               "phc-toolkit"
               "remember"
               "type-expander"
               "extensible-parser-specifications"
               "alexis-util"
               "typed-struct-props"
               "match-string"
               "xlist"
               "compatibility-lib"
               "generic-bind"
               "datatype"))
(define build-deps '("at-exp-lib"
                     "sandbox-lib"
                     "scribble-enhanced"
                     "scribble-lib"
                     "scribble-math"))
(define pkg-desc "Algebraic Datatypes tailored for writing compilers (tests are in phc-adt-test)")
(define version "1.1")
(define pkg-authors '("Suzanne Soy"))
