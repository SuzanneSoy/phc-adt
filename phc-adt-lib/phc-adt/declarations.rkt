#lang racket

(require (prefix-in tr: typed/racket)
         remember
         (submod "tagged-structure-low-level.hl.rkt" pre-declare)
         "ctx.hl.rkt")
(provide remembered!
         (rename-out [new-#%module-begin #%module-begin]))

(define-syntax-rule (new-#%module-begin . body)
  (tr:#%module-begin
   (tr:begin . body)
   (set-adt-context-macro here)
   (pre-declare-all-tagged-structure-structs)))