#lang racket/base

(require (for-template (lib "phc-adt/tagged.hl.rkt"))
         rackunit
         syntax/parse)

(check-equal?
 (syntax-parse #'(nam #:instance)
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (syntax->datum #'(name [fieldᵢ ...])))]
   [_ 'wrong])
 '(#t nam []))
         
(check-equal?
 (syntax-parse #'(nam [field0 value0])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute no-types?)
           (syntax->datum #'(name [fieldᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0] [value0]))

(check-equal?
 (syntax-parse #'(nam [field0 value0] [field1 value1])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute no-types?)
           (syntax->datum #'(name [fieldᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0 field1] [value0 value1]))

(check-equal?
 (syntax-parse #'(nam [field0 : type0 value0])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute types?)
           (syntax->datum #'(name [fieldᵢ ...] [τᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0] [type0] [value0]))

(check-equal?
 (syntax-parse #'(nam [field0 : type0 value0] [field1 : type1 value1])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute types?)
           (syntax->datum #'(name [fieldᵢ ...] [τᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0 field1] [type0 type1] [value0 value1]))

(check-equal?
 (syntax-parse #'(nam #:instance)
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?)
           (syntax->datum #'(name)))]
   [_ 'wrong])
 '(#t nam))

(check-equal?
 (syntax-parse #'(nam [field0 value0])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute no-types?)
           (syntax->datum #'(name [fieldᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0] [value0]))

(check-equal?
 (syntax-parse #'(nam [field0 : type0 value0])
   [(:tagged-call-args-syntax-class)
    (list* (attribute instance?) (attribute types?)
           (syntax->datum #'(name [fieldᵢ ...] [τᵢ ...] [valueᵢ ...])))]
   [_ 'wrong])
 '(#t #t nam [field0] [type0] [value0]))

(check-equal?
 (syntax-parse #'(nam [field0] field1)
   [(:tagged-call-args-syntax-class)
    (list* (attribute builder?)
           (syntax->datum #'(name [fieldᵢ ...])))]
   [_ 'wrong])
 '(#t nam [field0 field1]))

(check-equal?
 (syntax-parse #'(nam [field0] field1)
   [(:tagged-call-args-syntax-class)
    (list* (attribute builder?)
           (syntax->datum #'(name [fieldᵢ ...])))]
   [_ 'wrong])
 '(#t nam [field0 field1]))

(check-equal?
 (syntax-parse #'(nam [field0] field1)
   [(:tagged-call-args-syntax-class)
    (list* (attribute builder?)
           (syntax->datum #'(name [fieldᵢ ...])))]
   [_ 'wrong])
 '(#t nam [field0 field1]))

(check-equal?
 (syntax-parse #'(nam)
   [(:tagged-call-args-syntax-class) 'wrong]
   [_ 'parse-failed])
 'parse-failed)

(check-equal?
 (syntax-parse #'(#:instance)
   [(:tagged-call-args-syntax-class) 'wrong]
   [_ 'parse-failed])
 'parse-failed)

(check-equal?
 (syntax-parse #'()
   [(:tagged-call-args-syntax-class) 'wrong]
   [_ 'parse-failed])
 'parse-failed)
