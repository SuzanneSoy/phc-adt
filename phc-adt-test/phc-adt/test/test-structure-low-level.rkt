#lang typed/racket

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (lib "phc-adt/tagged-structure-low-level.hl.rkt")
         (for-syntax phc-toolkit/untyped))
(adt-init)

;; TODO: test all these with unsorted fields too.

;; Inferred type
(define-syntax (test-structure-infer-type stx)
  (syntax-case stx ()
    [(_ name . fields)
     (quasisyntax/top-loc stx
       (define-type name #,(tagged-infer-type! #'(untagged . fields))))]))

(test-structure-infer-type test0 test-fa test-fb)
(test-structure-infer-type test1)

;; Explicit type
(define-syntax (test-structure-type stx)
  (syntax-case stx ()
    [(_ name [field _ type] …)
     (quasisyntax/top-loc stx
       (define-type name #,(tagged-type! #'(untagged [field type] …))))]))

(test-structure-type test2 [test-fa : Number] [test-fb : String])

;; Builders. Assigning them to a variable can fail at compile-time if TR does
;; not see the type properly because it has the wrong scopes).
(define c3 (structure #:builder))
(define c4 (structure #:builder [test-fa : Number] [test-fb : String]))
(define c6 (structure #:builder [test-fa : Number] [test-fc : Number]))
(define c5 (structure #:builder test-fa test-fb))

;; Call constructors, and check the return type
(check-not-exn (λ () (ann (c3) test1)))
(let ([i4 (c4 7 "ee")]
      [i5 (c5 8 "ff")])
  (check-not-exn (λ () (ann i4 test2)))
  (check-not-exn (λ () (ann i5 test2)))
  (check-not-exn (λ () (ann i4 (test0 Number String))))
  (check-not-exn (λ () (ann i5 (test0 Number String)))))

;; TODO: bug report because using directly (ann v #t) does not work, but
;; wrapping it with a no-op if does work.
(define-syntax-rule (check-true-type v)
  (check-equal?: (if (ann v Boolean) #t #f)
                 : #t
                 #t))

(define-syntax-rule (check-false-type v)
  (check-false (ann (if (ann v Boolean) #t #f)
                    #f)))

(let ([i4 (c4 7 "ee")]
      [i5 (c5 8 "ff")])
  (check-true-type ((structure? test-fa test-fb) i4))
  (check-true-type ((structure? test-fa test-fb) i5))
  (check-false-type ((structure?) i4))
  (check-true-type ((structure?) (c3)))
  (check-false-type ((structure? test-fa test-fb) (c3))))

;; Predicate

(check-equal?: (tagged-get-field (c4 7 "ee") test-fa 'else)
               : Number
               7)
(check-equal?: (tagged-get-field (c5 7 "ee") test-fb 'else)
               : String
               "ee")
(check-equal?: ((λ-tagged-get-field test-fa) (c4 7 "ee"))
               : Number
               7)
(check-equal?: ((λ-tagged-get-field test-fb) (c5 7 "ee"))
               : String
               "ee")

;; Match-expander
(define-match-expander test-structure-match
  (λ/syntax-case (_ [field pat …] …) ()
    (quasisyntax/loc stx
      #,(tagged-match! #'(untagged [field (and pat …)] …)))))

(check-equal?: (match (c5 7 "ee")
                 [(test-structure-match [test-fa x] [test-fb y])
                  (list y x)])
               : (List String Number)
               '("ee" 7))

;; Supertypes
(define-syntax (test-supertypes stx)
  (syntax-case stx ()
    [(_ . fields)
     #`'#,(map cdr (has-fields #'fields))]))

(check-true (set=?
             (list->set (test-supertypes test-fa))
             (set '(untagged test-fa test-fb)
                  '(untagged test-fa test-fc)
                  '(untagged test-fa test-fd))))
