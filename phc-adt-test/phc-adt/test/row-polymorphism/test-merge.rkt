#lang type-expander

(require phc-adt phc-toolkit)
(adt-init)

(check-equal?:
 (merge (tagged foo [b 'b1]) (tagged foo [a 'a1] [c 'c1])
        : (U [(foo b) (foo a c)]))
 : (tagged foo [a 'a1] [b 'b1] [c 'c1])
 (tagged foo [a 'a1] [b 'b1] [c 'c1]))

(check-equal?:
 (merge (tagged bar [b 'b2]) (tagged bar [a 'a2] [c 'c2])
        : (U [(bar b) (bar a c)] [(foo b) (foo a c)]))
 : (tagged bar [a 'a2] [b 'b2] [c 'c2])
 (tagged bar [a 'a2] [b 'b2] [c 'c2]))

(check-equal?:
 (merge (tagged baz [b 'b3]) (tagged baz [a 'a3])
        : (U [(baz b) (baz a)] [(foo b) (foo a c)]))
 : (tagged baz [a 'a3] [b 'b3])
 (tagged baz [a 'a3] [b 'b3]))

(check-equal?:
 (merge (tagged qux [b 'b4]) (tagged qux [d 'd4])
        : (U [(qux b) (qux d)] [(foo b) (foo a c)]))
 : (tagged qux [b 'b4] [d 'd4])
 (tagged qux [b 'b4] [d 'd4]))

;; Different tags
(check-equal?:
 (merge (tagged qux [b 'b4]) (tagged foo [d 'd4])
        : (U [(qux b) (foo d)] [(foo b) (foo a c)]))
 : (tagged qux [b 'b4] [d 'd4])
 (tagged qux [b 'b4] [d 'd4]))
(check-equal?:
 (merge (tagged qux [b 'b4]) (tagged foo [d 'd4])
        : (U [(qux b) (foo d)] [(foo b) (foo a)]))
 : (tagged qux [b 'b4] [d 'd4])
 (tagged qux [b 'b4] [d 'd4]))
