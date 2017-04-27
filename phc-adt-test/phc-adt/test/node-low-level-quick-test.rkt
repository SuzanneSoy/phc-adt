#lang typed/racket
(require (lib "phc-adt/node-low-level.hl.rkt")
         typed/rackunit
         phc-toolkit
         phc-adt)
(adt-init "./mailing-list-example/adt-pre-declarations.rkt")

(define-constructor foo-ct #:tag foo :)

(check-not-exn
 (λ ()
   (equal-hash-code
    (|(node foo values)| (delay (list 'a 'b 'c))
                         (raw-node (vector (list 'a 'b 'c)) 0)))))

(check-not-exn
 (λ ()
   (equal-secondary-hash-code
    (|(node foo values)| (delay (list 'a 'b 'c))
                         (raw-node (vector (list 'a 'b 'c)) 0)))))

(check-true: (equal?
              (|(node foo values)| (delay (list 'a 'b 'c))
                                   (raw-node (vector (list 'a 'b 'c)) 0))
              (|(node foo values)| (delay (list 'a 'b'c))
                                   (raw-node (vector (list 'a 'b 'c)) 1))))
(check-false: (equal?
               (|(node foo values)| (delay (list 'a 'b 'c))
                                    (raw-node (vector (list 'a 'b 'c)) 0))
               (|(node foo values)| (delay (list 'a 'x 'c))
                                    (raw-node (vector (list 'a 'x 'c)) 1))))
