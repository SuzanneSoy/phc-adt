#lang typed/racket

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (only-in (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                  λ-tagged-get-field))
(adt-init)

;; define-structure
(define-structure named1 test-fa test-fb)
(define-structure named2 test-fb test-fa)
(define-structure named3 test-fa test-fc)

(check-equal?-classes:
 [#:name "named1 ∪ named2"
  : (named1 'a 'b)
  (ann (named1 (ann 'a 'a) (ann 'b 'b)) (named1 'a 'b))
  (ann (named1 (ann 'a 'a) (ann 'b 'b)) (structure [test-fa 'a] [test-fb 'b]))
  (ann (named1 (ann 'a 'a) (ann 'b 'b)) ((structure [test-fa] test-fb) 'a 'b))
  (ann (named2 (ann 'b 'b) (ann 'a 'a)) (named2 'b 'a))
  (ann (named2 (ann 'b 'b) (ann 'a 'a)) (structure [test-fa 'a] [test-fb 'b]))
  (ann (named2 (ann 'b 'b) (ann 'a 'a)) ((structure [test-fa] test-fb) 'a 'b))]
 [#:name "named3"
  : (named3 'a 'b)
  (ann (named3 (ann 'a 'a) (ann 'b 'b)) (named3 'a 'b))
  (ann (named3 (ann 'a 'a) (ann 'b 'b)) (structure [test-fa 'a] [test-fc 'b]))
  (ann (named3 (ann 'a 'a) (ann 'b 'b)) ((structure [test-fa] test-fc) 'a 'b))])

(check-equal?: (match (named1 (ann 'a 'a) (ann 'b 'b))
                 [(named1 fa fb) (list fb fa)])
               : (List 'b 'a)
               '(b a))

;; Types
(define-type t0 (structure [test-fa Number] [test-fb String]))
(define-type t1 (structure))

(define i2 (structure [test-fa 1] [test-fb "a"]))
(define i3 (structure #:instance))

(check-not-exn (λ () (ann i2 t0)))
(check-not-exn (λ () (ann i3 t1)))

(check-not-exn (λ () (ann i2 StructureTop)))
(check-not-exn (λ () (ann i3 StructureTop)))

(check-true (StructureTop? i2))
(check-true (StructureTop? i3))

;; Instance and make-instance
(define c4 (structure test-fa test-fb))
(define c5 (structure [test-fa : Number] [test-fb : String]))
(define-type test-fa+test-fb (structure [test-fa Number] [test-fb String]))
(check-equal?-classes:
 [#:name "test-fa+test-fb"
  : test-fa+test-fb
  ((structure test-fa test-fb) 2 "b")
  ((structure [test-fa] test-fb) 2 "b")
  ((structure test-fa [test-fb]) 2 "b")
  ((structure [test-fa] [test-fb]) 2 "b")
             
  (structure [test-fa 2] [test-fb "b"])

  ((structure [test-fa : Number] [test-fb : String]) 2 "b")

  (structure [test-fa 2 : Number] [test-fb "b" : String])
  
  ((structure test-fb test-fa) "b" 2)
  ((structure test-fb [test-fa]) "b" 2)
  ((structure [test-fb] test-fa) "b" 2)
  ((structure [test-fb] [test-fa]) "b" 2)
             
  (structure [test-fb "b"] [test-fa 2])

  ((structure [test-fb : String] [test-fa : Number]) "b" 2)

  (structure [test-fb "b" : String] [test-fa 2 : Number])])

;; Accessor
(check-equal?: ((λ-tagged-get-field test-fb) (c4 7 "ee"))
               : String
               "ee")
(check-equal?: ((λ-tagged-get-field test-fb) (c5 7 "ee"))
               : String
               "ee")

;; Match
((inst check-equal?-classes (List String Number))
 (cons
  "match"
  (list
   ;; Simple
   (match (c4 7 "ee") [(structure [test-fa fa] [test-fb fb]) (list fb fa)])
   ;; Change order in the struct definition
   (match (c4 7 "ee") [(structure [test-fb fb] [test-fa fa]) (list fb fa)])
   ;; No patterns
   (match (c4 7 "ee") [(structure [test-fb] [test-fa]) (list test-fb test-fa)])
   (match (c4 7 "ee") [(structure test-fb test-fa) (list test-fb test-fa)]))))

;; supertypes:

(define fn1 (ann (λ (x) x)
                 (→ (structure-supertype [test-fa Number])
                    (structure-supertype [test-fa Number]))))
(check-not-exn
 (λ ()
   (ann fn1
        (→ (U (structure-supertype [test-fa Number] [test-fb Any])
              (structure-supertype [test-fa Number] [test-fc Any])
              (structure-supertype [test-fa Number] [test-fd Any]))
           (U (structure-supertype [test-fa Number] [test-fb Any])
              (structure-supertype [test-fa Number] [test-fc Any])
              (structure-supertype [test-fa Number] [test-fd Any]))))))

(define fn2 (ann (λ (x) x)
                 (→ ((structure-supertype test-fa) Number)
                    ((structure-supertype test-fa) Number))))
(check-not-exn
 (λ ()
   (ann fn2
        (→ (U (structure-supertype [test-fa Number] [test-fb Any])
              (structure-supertype [test-fa Number] [test-fc Any])
              (structure-supertype [test-fa Number] [test-fd Any]))
           (U (structure-supertype [test-fa Number] [test-fb Any])
              (structure-supertype [test-fa Number] [test-fc Any])
              (structure-supertype [test-fa Number] [test-fd Any]))))))

(check-not-exn (λ () (ann (structure [test-fa 7] [test-fb 'x])
                          (structure-supertype [test-fa Number]))))

(check-not-exn (λ () (ann (structure [test-fa 8] [test-fc 42])
                          ((structure-supertype [test-fa]) Number))))

(check-not-exn (λ () (ann (structure [test-fa 8] [test-fd "blob"])
                          ((structure-supertype test-fa) Number))))

(check-equal?: (match (structure [test-fa 8] [test-fc 'y])
                 [(structure-supertype [test-fa x]) (+ x 1)])
               : Number
               9)

;; Exchange structures across files (values, types …)
(require "test-structure-other.rkt")

(check-equal? (ann i-other (structure [test-fa String] [test-fb 'b]))
              (structure [test-fa "a"] [test-fb (ann 'b 'b)]))

(check-not-exn (λ () (ann (structure [test-fa 1] [test-fc 'c]) t-other)))

(check-equal? (c-other 7 'd)
              (structure [test-fa 7 : Number] [test-fd 'd : 'd]))
