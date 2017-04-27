#lang typed/racket/base

(require phc-toolkit
         typed/rackunit
         type-expander
         racket/string
         (for-syntax racket/base
                     phc-toolkit/untyped)
         (lib "phc-adt/ctx.hl.rkt"))

(provide ck
         ck-not
         check-print-type)

(define-syntax (ck stx)
  (syntax-case stx ()
    [(_ v t)
     (quasisyntax/top-loc stx
       (check-tc
        (require (only-in (lib "phc-adt/ctx.hl.rkt") set-adt-context-macro))
        (set-adt-context-macro #,(datum->syntax #'t 'there))
        (ann v t)))]))

(define-syntax (ck-not stx)
  (syntax-case stx ()
    [(_ v t)
     (quasisyntax/top-loc stx
       (check-not-tc
        (require (only-in (lib "phc-adt/ctx.hl.rkt") set-adt-context-macro))
        (set-adt-context-macro #,(datum->syntax #'t 'there))
        (ann v t)))]))

(: clean-type-str (→ String String))
(define (clean-type-str type-str)
  (string-trim
   (regexp-replace* #px"(?-s:[ \n]+)"
                    (regexp-replace #px"^- :" type-str "")
                    " ")))

(define-syntax/case (check-print-type e str) ()
  (eval-tc
   (λ (f)
     (quasisyntax/top-loc stx
       (check-equal?: (clean-type-str (#,f)) str)))
   (quasisyntax/top-loc stx
     (begin (current-print (λ _ (void)))
            (require (only-in (lib "phc-adt/ctx.hl.rkt") set-adt-context-macro))
            (set-adt-context-macro #,(datum->syntax #'t 'there))
            e))))
