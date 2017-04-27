#lang typed/racket

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (only-in (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                  λ-tagged-get-field))
(adt-init)

(define-tagged st2 [b String] [a Number])

((tagged t a b c) 1 'b "c")
((tagged t a [b] c) 1 'b "c")
((tagged t [a] [b] [c]) 1 'b "c")
((tagged t [a : Number] [b : Symbol] [c : String]) 1 'b "c")
(tagged t [a : Number 1] [b : Symbol 'b] [c : String "c"])
(tagged t [a 1] [b 'b] [c "c"])

(tagged t [a 1] [b 'b] [c "c"])

(define-tagged tabc [a 1] [b 'b] [c "c"])
