#lang typed/racket

(require phc-adt
         syntax/macro-testing
         typed/rackunit
         type-expander)

(define compile-time-exception-regexp
  #px"phc-adt: adt-init must be called before using the features in phc-adt")
(check-exn compile-time-exception-regexp
           (λ () (convert-compile-time-error
                  (tagged tg [x "x"] [y 2]))))
(check-exn compile-time-exception-regexp
           (λ () (convert-compile-time-error
                  (match '() [(tagged tg x y) 'a] [_ 'b]))))
(check-exn compile-time-exception-regexp
           (λ () (convert-compile-time-error
                  (let () (define-type foo (tagged tg x y)) 1))))
