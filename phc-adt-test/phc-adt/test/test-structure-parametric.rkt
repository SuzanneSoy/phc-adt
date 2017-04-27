#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit "ck.rkt")
(adt-init)

(define-structure #:∀ (T) a [a : T] [b : T])
(check-print-type (a 3 "b") "((tagged untagged a b) (U Positive-Byte String) (U Positive-Byte String))")

(define-structure b #:∀ (T) [a : T] [b : T])
(define-structure c [a : T] [b : T] #:∀ (T))

(define printed-type
  "((tagged untagged a b) (U Positive-Byte String) (U Positive-Byte String))")

(check-print-type (a 3 "b") printed-type)
(check-ann (a 3 4) (structure [a Positive-Byte] [b Positive-Byte]))

(check-print-type (b 3 "b") printed-type)
(check-ann (b 3 4) (structure [a Positive-Byte] [b Positive-Byte]))

(check-print-type (c 3 "b") printed-type)
(check-ann (c 3 4) (structure [a Positive-Byte] [b Positive-Byte]))
