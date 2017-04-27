#lang info
(define collection 'multi)
(define deps '("base"
               "phc-adt-lib"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "multi-id"
               "phc-toolkit"
               "type-expander"))
(define build-deps '())
(define pkg-desc "Tests for the phc-adt library")
(define version "1.1")
(define pkg-authors '("Georges Dupéron"))
