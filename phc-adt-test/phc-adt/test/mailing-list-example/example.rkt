#lang typed/racket
;; phc-adt sets registers some information in its support files when it
;; encounters new structure types, so this file has to be compiled twice
;; in DrRacket (not on the command-line). The first compilation will fail,
;; it is normal and expected (but it should soon give a better error message)
(require type-expander multi-id phc-adt typed/rackunit)
(adt-init)

;; This internally does
;; (define-multi-id s1
;;   #:type-expander  (λ (stx) …)
;;   #:match-expander (λ (stx) …)
;;   #:call           (λ (stx) …)
;;   #:id             (λ (stx) …))
(define-structure s1 [a : Number] [b : String])

;; The "structure" identifier is also a multi-id, for on-the-fly usage of a
;; structure as a type, match pattern, constructor function or instance creation

(: foo (→ (U
           ;; type-expander: s1
           s1
           ;; type-expander: (structure [field : type] …)
           (structure [a : Number] [c : Symbol]))
          Number))
(define (foo s)
  (match s
    ;; match-expander: (s1 pat ...)
    [(s1 (? number? the-a) the-b) (+ the-a (string-length the-b))]
    ;; match-expander: (structure field-name …)
    [(structure a c) (+ a (string-length (symbol->string c)))]))

(define instances
  (append
   ;; identifier macro: s1, to pretend it's a function
   (map s1
        (range 5)
        '("x" "xx" "xxx" "xxxx" "xxxxx"))
   ;; macro: (s1 args …), to pretend it's a function call
   (list (s1 42 "Why does six times nine equal forty two?"))
   ;; macro: (structure [field : type] …), produces a bulder function
   (map (structure [a : Number] [c : Symbol])
        (reverse (range 5))
        '(x xx xxx xxxx xxxxx))
   ;; macro: (structure [field value] …) or (structure [field : type value]),
   ;; produces an instance
   (list (structure [a pi] [c 'three-fourteen]))))

(check-equal? (map foo instances)
              (append '(1 3 5 7 9)
                      '(82)
                      '(5 5 5 5 5)
                      '(17.141592653589793)))
