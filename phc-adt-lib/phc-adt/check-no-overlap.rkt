#lang typed/racket
(struct (A) st ([x : A]))
(require phc-toolkit
         racket/format)

(struct variant-no-overlap ())
(define-syntax/parse (check-no-overlap τ₁ τ₂)
  #`(let ()
      (λ ([x : (U (∩ τ₁ τ₂) variant-no-overlap)])
        (cond
          [(variant-no-overlap? x) #t]
          [(typecheck-fail τ₁
                           #,(format "The types ~a and ~a seem to overlap"
                                     (syntax->datum #'τ₁)
                                     (syntax->datum #'τ₂)))]))
      (void)))


(check-no-overlap (st Number) (st String))
(check-no-overlap (st Negative-Integer) (st Byte))
(check-not-tc
 #:message-regexp
 #rx"The types \\(st Nonpositive-Integer\\) and \\(st Byte\\) seem to overlap"
 (check-no-overlap (st Nonpositive-Integer) (st Byte)))