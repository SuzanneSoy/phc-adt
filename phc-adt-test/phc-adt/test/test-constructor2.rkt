#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit)
(adt-init)
  
(check-equal?: (constructor-values
                (ann (constructor a 1 "x")
                     ;; TODO: Make a (Constructor-AnyTag …) type expander.
                     (tagged a [values (List Number String)])))
               (list 1 "x"))
(check-equal?: (constructor-values
                (ann (constructor a 1 "x")
                     (tagged a [values Any])))
               (list 1 "x"))
(check-equal?: (constructor-values
                (ann (constructor a 1 "x")
                     (constructor a Number String)))
               (list 1 "x"))
(check-equal?: (constructor-values
                (ann (constructor b)
                     (constructor b)))
               (list))
(check-equal?: (constructor-values
                (ann (constructor c 'd)
                     (constructor c Symbol)))
               '(d))
(check-equal?: (ann (constructor c 2 "y")
                    (constructor c Number String))
               (constructor c 2 "y"))
(check-not-equal?: (constructor d 2 "y")
                   (constructor d 2 "y" 'z))
(check-not-equal?: (constructor e 2 "y")
                   (constructor F 2 "y"))