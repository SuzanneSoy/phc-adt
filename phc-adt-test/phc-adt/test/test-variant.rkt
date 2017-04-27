#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit)
(adt-init)
  
(define-variant v1 [x Number String] [y String Number] [z Number String])
(check-equal?: (ann (constructor x 1 "a")
                    (U [constructor w Number String]
                       [constructor x Number String]
                       [constructor y String Number]))
               (constructor x 1 "a"))
(check-equal?: (constructor x 1 "a")
               (constructor x 1 "a"))
(check-equal?: (ann (constructor x 1 "a") v1)
               (constructor x 1 "a"))
(check-equal?: (ann (constructor x 1 "a") v1)
               (ann (constructor x 1 "a") v1))
(check-not-equal?: (ann (constructor x 2 "b") v1)
                   (ann (constructor y "b" 2) v1))
(check-not-equal?: (ann (constructor x 3 "c") v1)
                   (ann (constructor z 3 "c") v1))