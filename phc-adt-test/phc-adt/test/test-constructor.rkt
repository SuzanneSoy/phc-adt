#lang typed/racket

(require phc-adt phc-toolkit type-expander typed/rackunit)
(adt-init)

; (_ name:id *)
(check-not-exn
 (λ ()
   (ann (constructor tag1 *)
        (∀ (A ...) (→ A ... A (tagged tag1 [values (List A ... A)]))))))

(check-equal?: (constructor-values
                (ann ((constructor tag1 *))
                     (constructor tag1)))
               '())
(check-equal?: (constructor-values
                (ann ((constructor tag1 *) 1)
                     (constructor tag1 One)))
               '(1))
(check-equal?: (constructor-values
                (ann ((constructor tag1 *) 1 "b")
                     (constructor tag1 Number String)))
               '(1 "b"))
(check-equal?: (constructor-values
                (ann ((constructor tag1 *) 1 "b" 'c)
                     (constructor tag1 Number String 'c)))
               '(1 "b" c))

; (_ name:id :colon)
(check-not-exn (λ () (ann (constructor tag1 :)
                          (→ (constructor tag1)))))

; (_ name:id :colon T₀:expr)
(check-not-exn (λ () (ann (constructor tag1 : Number)
                          (→ Number (constructor tag1 Number)))))

; (_ name:id :colon Tᵢ:expr …+)
(check-not-exn
 (λ () (ann (constructor tag1 : Number String)
            (→ Number String (constructor tag1 Number String)))))
(check-not-exn
 (λ () (ann (constructor tag1 : Number String 'c)
            (→ Number String 'c (constructor tag1 Number String 'c)))))

; Call (_ name:id :colon)
(check-equal?: (constructor-values (ann ((constructor tag1 :))
                                        (constructor tag1)))
               '())

; Call (_ name:id :colon T₀:expr)
(check-equal?: (constructor-values (ann ((constructor tag1 : Number) 1)
                                        (constructor tag1 Number)))
               '(1))

; Call (_ name:id :colon Tᵢ:expr …+)
(check-equal?: (constructor-values
                (ann ((constructor tag1 : Number String) 1 "b")
                     (constructor tag1 Number String)))
               '(1 "b"))
(check-equal?: (constructor-values
                (ann ((constructor tag1 : Number String 'c) 1 "b" 'c)
                     (constructor tag1 Number String 'c)))
               '(1 "b" c))

; (_ name:id [val₀:expr :colon T₀:expr])
(check-equal?: (constructor-values
                (ann (constructor tag1 [1 : One])
                     (constructor tag1 One)))
               '(1))

; (_ name:id [valᵢ:expr :colon Tᵢ:expr] …)
(check-equal?: (constructor-values
                (ann (constructor tag1 [1 : One] ["b" : (U "b" "B")])
                     (constructor tag1 One (U "b" "B"))))
               '(1 "b"))

; (_ name:id val₀:expr)
(check-equal?: (constructor-values
                (ann (constructor tag1 "a")
                     (constructor tag1 String)))
               '("a"))

; (_ name:id valᵢ:expr …)
(check-equal?: (constructor-values
                (ann (constructor tag1 "a" "b")
                     (constructor tag1 String String)))
               '("a" "b"))
(check-equal?: (constructor-values
                (ann (constructor tag1 "a" "b" 'c)
                     (constructor tag1 String String Symbol)))
               '("a" "b" c))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t . "a"))
               : (List* "a" (Vector Char 3) #t "a")
               '("a" #(#\b 3) #t . "a"))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t . #\b))
               : (List* "a" (Vector Char 3) #t Char)
               '("a" #(#\b 3) #t . #\b))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t . 3))
               : (List* "a" (Vector Char 3) #t 3)
               '("a" #(#\b 3) #t . 3))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t . #t))
               : (List* "a" (Vector Char 3) #t #t)
               '("a" #(#\b 3) #t . #t))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t . #("x" #\y 26 #f)))
               : (List* "a" (Vector Char 3) #t (Vector "x" Char 26 #f))
               '("a" #(#\b 3) #t . #("x" #\y 26 #f)))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t #("x" #\y 26 #f) . 123))
               : (List* "a" (Vector Char 3) #t (Vector "x" Char 26 #f) 123)
               '("a" #(#\b 3) #t #("x" #\y 26 #f) . 123))

(check-equal?: (constructor-values
                (constructor tag1 "a" #(#\b 3) #t #("x" #\y 26 #f)))
               : (List "a" (Vector Char 3) #t (Vector "x" Char 26 #f))
               '("a" #(#\b 3) #t #("x" #\y 26 #f)))