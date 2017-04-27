#lang typed/racket/base

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (for-syntax racket/base
                     syntax/stx
                     racket/list))
(adt-init)

(define-syntax (repeat-stx stx)
  (syntax-case stx ()
    [(_ n expr)
     (number? (syntax-e #'n))
     #`(begin . #,(stx-map (λ _ #'expr)
                           (range (syntax-e #'n))))]))

(repeat-stx 1 (check-not-exn (λ () (structure [test-fa 2 : Number]
                                              [test-fb "b" : String]))))

;(repeat-stx 1024 (check-not-exn (λ () (structure [test-fa 2 : Number]
;                                                 [test-fb "b" : String]))))