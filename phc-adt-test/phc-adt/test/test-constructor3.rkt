#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit)
(adt-init)

;; define-constructor
(define-constructor tag0 :)
(define-constructor tag1 : Number)
(define-constructor tag2 : Number String)
(define-constructor tag3 : Number String 'c)

;; Type expander
(check-equal?: (constructor-values (ann (constructor tag0) tag0))
               '())
(check-equal?: (constructor-values (ann (constructor tag1 1) tag1))
               '(1))
(check-equal?: (constructor-values (ann (constructor tag2 1 "b") tag2))
               '(1 "b"))
(check-equal?: (constructor-values
                (ann (constructor tag3 1 "b" (ann 'c 'c)) tag3))
               '(1 "b" c))

;; Call
(check-equal?: (constructor-values (ann (tag0) (constructor tag0)))
               '())
(check-equal?: (constructor-values (ann (tag1 1) (constructor tag1 Number)))
               '(1))
(check-equal?: (constructor-values
                (ann (tag2 1 "b") (constructor tag2 Number String)))
               '(1 "b"))
(check-equal?: (constructor-values
                (ann (tag3 1 "b" 'c) (constructor tag3 Number String 'c)))
               '(1 "b" c))

;; Id
(check-not-exn (λ () (ann tag0 (→ (constructor tag0)))))
(check-not-exn (λ () (ann tag1 (→ Number (constructor tag1 Number)))))
(check-not-exn
 (λ () (ann tag2 (→ Number String (constructor tag2 Number String)))))
(check-not-exn
 (λ () (ann tag3 (→ Number String 'c (constructor tag3 Number String 'c)))))

;; Match expander
(check-equal?: (ann (match (constructor tag0) [(tag0) #t]) #t)
               #t)
(check-equal?: (ann (match (constructor tag1 1) [(tag1 x) (list x)])
                    (List Number))
               '(1))
(check-equal?: (ann (match (constructor tag2 1 "b") [(tag2 x y) (list y x)])
                    (List String Number))
               '("b" 1))
(check-equal?: (ann (match (constructor tag3 1 "b" (ann 'c 'c))
                      [(tag3 x y z) (list z y x)])
                    (List 'c String Number))
               '(c "b" 1))

;; Match expander which single pattern
(check-equal?: (ann (match (constructor tag0) [(tag0 #:rest whole) whole]) Null)
               '())
(check-equal?: (ann (match (constructor tag1 1) [(tag1 x) x])
                    Number)
               '1)
(check-equal?: (ann (match (constructor tag2 1 "b") [(tag2 x y) (list x y)])
                    (List Number String))
               '(1 "b"))
(check-equal?: (ann (match (constructor tag3 1 "b" (ann 'c 'c))
                      [(tag3 x y z) (list x y z)])
                    (List Number String 'c))
               '(1 "b" c))
