#lang typed/racket

(require phc-adt phc-toolkit type-expander "ck.rkt")
(adt-init)

(module m-abc typed/racket
  (require phc-adt phc-toolkit type-expander "ck.rkt")
  (adt-init)
  
  (define-tagged t0 tagged-fc tagged-fb tagged-fa)
  (define c0 t0)
  (provide (rename-out [t0 t0-cba]
                       [c0 c0-cba])))

(module m-xyz typed/racket
  (require phc-adt phc-toolkit type-expander "ck.rkt")
  (adt-init)
  
  (define-tagged t0 tagged-fx tagged-fy tagged-fz)
  (define c0 t0)
  (provide (rename-out [t0 t0-xyz]
                       [c0 c0-xyz])))

(require 'm-abc
         'm-xyz)

(define-tagged t0 tagged-fa tagged-fb tagged-fc)
(define c0 t0)

(ck (t0 'a "b" 3) (t0 Symbol String Number))
(ck (c0 'a "b" 3) (t0 Symbol String Number))
(ck-not (t0 'a "b" 3) (t0 Number Number Number))
(ck-not (c0 'a "b" 3) (t0 Number Number Number))

(ck (t0 'a "b" 3) (t0-cba Number String Symbol))
(ck (c0 'a "b" 3) (t0-cba Number String Symbol))
(ck-not (t0 'a "b" 3) (t0-cba Number Number Number))
(ck-not (c0 'a "b" 3) (t0-cba Number Number Number))

(ck (t0-cba 'a "b" 3) (t0 Number String Symbol))
(ck (c0-cba 'a "b" 3) (t0 Number String Symbol))
(ck-not (t0-cba 'a "b" 3) (t0 Number Number Number))
(ck-not (c0-cba 'a "b" 3) (t0 Number Number Number))

(ck (t0-cba 'a "b" 3) (t0-cba Symbol String Number))
(ck (c0-cba 'a "b" 3) (t0-cba Symbol String Number))
(ck-not (t0-cba 'a "b" 3) (t0-cba Number Number Number))
(ck-not (c0-cba 'a "b" 3) (t0-cba Number Number Number))

(ck (t0-xyz 'a "b" 3) (t0-xyz Symbol String Number))
(ck (c0-xyz 'a "b" 3) (t0-xyz Symbol String Number))
(ck-not (t0-xyz 'a "b" 3) (t0 Number Number Number))
(ck-not (c0-xyz 'a "b" 3) (t0 Number Number Number))
(ck-not (t0 'a "b" 3) (t0-xyz Symbol String Number))
(ck-not (c0 'a "b" 3) (t0-xyz Symbol String Number))

(check-equal?-classes:
 [#:name "abc"
  (t0 'a "b" 3)
  (c0 'a "b" 3)
  (t0-cba 3 "b" 'a)
  (c0-cba 3 "b" 'a)]
 [#:name "cba"
  (t0 3 "b" 'a)
  (c0 3 "b" 'a)
  (t0-cba 'a "b" 3)
  (c0-cba 'a "b" 3)])