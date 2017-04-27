#lang type-expander

(require phc-adt phc-toolkit)
(adt-init)

(define-tagged foo [a : 'a1] [b : 'b1] [c : 'c1])
(define-tagged bar [a : 'a2] [b : 'b2] [c : 'c2])
(define-tagged baz [a : 'a3] [b : 'b3])
(define-tagged qux           [b : 'b4] [d : 'd4])

(let-values ([(x y) (split (foo 'a1 'b1 'c1) : (U (foo a b c)) b)])
  (check-equal?: x : (tagged foo [b 'b1]) (tagged foo [b 'b1]))
  (check-equal?: y : (tagged foo [a 'a1] [c 'c1]) (tagged foo [a 'a1] [c 'c1])))

(let-values ([(x y) (split (bar 'a2 'b2 'c2) : (U (foo a b c) (bar a b c)) b)])
  (check-equal?: x : (tagged bar [b 'b2]) (tagged bar [b 'b2]))
  (check-equal?: y : (tagged bar [a 'a2] [c 'c2]) (tagged bar [a 'a2] [c 'c2])))

(let-values ([(x y) (split (baz 'a3 'b3) : (U (foo a b c) (baz a b)) b)])
  (check-equal?: x : (tagged baz [b 'b3]) (tagged baz [b 'b3]))
  (check-equal?: y : (tagged baz [a 'a3]) (tagged baz [a 'a3])))

(let-values ([(x y) (split (qux 'b4 'd4) : (U (foo a b c) (qux b d)) b)])
  (check-equal?: x : (tagged qux [b 'b4]) (tagged qux [b 'b4]))
  (check-equal?: y : (tagged qux [d 'd4]) (tagged qux [d 'd4])))
