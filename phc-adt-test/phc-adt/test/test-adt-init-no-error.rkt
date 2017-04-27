#lang typed/racket

(require phc-adt
         syntax/macro-testing
         typed/rackunit
         type-expander)

(adt-init)
(check-not-exn (λ () (tagged t1 [x "x"] [y "2"])))
(check-not-exn (λ () (match '() [(tagged tg x y) 'a] [_ 'b])))
(check-not-exn (λ () (let () (define-type foo (tagged tg x y)) 1)))