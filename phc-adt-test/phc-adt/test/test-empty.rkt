#lang typed/racket

(require phc-adt
         phc-toolkit
         type-expander
         typed/rackunit
         (only-in (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                  λ-tagged-get-field))
(adt-init)

(define-structure empty-st)
(define-tagged empty-tg)
(define-constructor empty-ct :)
(define-constructor empty-ct-t2 #:tag empty-ct ::)
(define-constructor empty-ct-t3 #:tag empty-ct !)
(define-constructor empty-ct-2 ::)
(define-constructor empty-ct-3 !)

;(ann empty-ct (→ (constructor empty-ct)))
;(ann (empty-ct) (constructor empty-ct))
(check-equal?: (if (match (constructor empty-ct)
                     [(empty-ct) #t]
                     [_ #f])
                   #t
                   #f)
               : #t
               #t)
;(ann (constructor empty-ct) empty-ct)
(check-ann empty-ct?
           (→ Any Boolean : (constructor empty-ct)))
(check-equal?: (if (empty-ct? (constructor empty-ct)) #t #f)
               : #t
               #t)

(check-ann (structure #:builder)
           (→ (structure)))
(check-ann (tagged empty-tg #:builder)
           (→ (tagged empty-tg)))
(check-ann (constructor empty-ct :)
           (→ (constructor empty-ct)))
(check-ann (constructor empty-ct ::)
           (→ Null (constructor empty-ct)))
(check-ann (constructor empty-ct !)
           (→ Any * (constructor empty-ct)))

(check-true: (match ((constructor empty-ct :))
               [(tagged empty-ct [values '()]) #t]
               [_ #f]))

(check-true: (match ((constructor empty-ct ::) null)
               [(tagged empty-ct [values '()]) #t]
               [_ #f]))

(check-true: (match ((constructor empty-ct !))
               [(tagged empty-ct [values '()]) #t]
               [_ #f]))

(check-true: (match (constructor empty-ct)
               [(tagged empty-ct [values '()]) #t]
               [_ #f]))

(check-ann (let ([v ((constructor empty-ct ::) null)])
             (if ((|(tagged-cast-predicate empty-ct values)|
                   (make-predicate Null))
                  v)
                 v
                 #f))
           (constructor empty-ct))

;; result type should be (constructor empty-ct . Number)
(check-ann (constructor empty-ct . #{1 : Number})
           (constructor empty-ct . Number))
(check-ann (constructor empty-ct #:rest 1 : Number)
           (constructor empty-ct . Number))

(check-ann (constructor empty-ct . 1)
           (constructor empty-ct . 1))
(check-ann (constructor empty-ct #:rest 1)
           (constructor empty-ct . 1))
(check-ann (constructor empty-ct #:rest (list 1 2))
           (constructor empty-ct Number Number))

(check-ann (constructor empty-ct 1)
           (constructor empty-ct 1))
(check-ann (constructor empty-ct [1 : Number])
           (constructor empty-ct Number))
(check-ann (constructor empty-ct)
           (constructor empty-ct))
(check-ann (constructor empty-ct)
           (tagged empty-ct [values Null]))

(check-equal?-classes:
 [(constructor empty-ct . #{1 : Number})
  (constructor empty-ct #:rest 1 : Number)
  (constructor empty-ct . 1)
  (constructor empty-ct #:rest 1)]
 [(constructor empty-ct #:rest (list 1 2))]
 [(constructor empty-ct 1)
  (constructor empty-ct [1 : Number])]
 [(constructor empty-ct)
  (constructor empty-ct . #{null : Null})
  (constructor empty-ct #:rest null : Null)
  (constructor empty-ct . null)
  (constructor empty-ct #:rest null)])
