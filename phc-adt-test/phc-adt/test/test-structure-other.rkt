#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit)
(adt-init)

(provide i-other t-other c-other)

(define i-other (structure [test-fa "a"] [test-fb (ann 'b 'b)]))
(define-type t-other (structure [test-fa Number] [test-fc 'c]))
(define c-other (structure [test-fa : Number] [test-fd : 'd]))