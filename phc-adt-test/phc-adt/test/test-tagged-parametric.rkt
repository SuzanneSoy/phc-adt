#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit "ck.rkt")
(adt-init)

(define-tagged #:∀ (T) a [a : T] [b : T])
(define-tagged b #:∀ (T) [a : T] [b : T])
(define-tagged c [a : T] [b : T] #:∀ (T))

(define (printed-type [t : String])
  (string-append "((tagged " t " a b)"
                 " (U Positive-Byte String) (U Positive-Byte String))"))

(check-print-type (a 3 "b") (printed-type "a"))
(check-ann (a 3 4) (tagged a [a Positive-Byte] [b Positive-Byte]))

(check-print-type (b 3 "b") (printed-type "b"))
(check-ann (b 3 4) (tagged b [a Positive-Byte] [b Positive-Byte]))

(check-print-type (c 3 "b") (printed-type "c"))
(check-ann (c 3 4) (tagged c [a Positive-Byte] [b Positive-Byte]))
