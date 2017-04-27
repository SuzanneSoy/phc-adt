#lang typed/racket

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (only-in (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                  λ-tagged-get-field))
(adt-init)

(define i (structure [a 1 : Number] [b "b" : String]))
(define c (structure [a : Number] [b : String]))
(define i2 (c 1 "b"))

(check-equal?: (uniform-get i a) : Number 1)
(check-equal?: (uniform-get i b) : String "b")
(check-equal?: (uniform-get i2 a) : Number 1)
(check-equal?: (uniform-get i2 b) : String "b")