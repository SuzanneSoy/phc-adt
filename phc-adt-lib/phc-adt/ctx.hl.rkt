#lang hyper-literate typed/racket/base #:no-require-lang
@(require scribble-enhanced/doc
          racket/require
          hyper-literate
          (for-label racket/format
                     racket/list
                     (subtract-in racket/contract typed/racket/base)
                     phc-toolkit
                     phc-toolkit/untyped-only
                     remember
                     (subtract-in typed/racket/base type-expander)
                     type-expander))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "ctx"
       #:tag-prefix "phc-adt/ctx"
       ]{Implementation of ADTs: syntax scopes}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/ctx"))


Due to TR bug #399, structs declared by a macro do not work
if the macro itself is declared in a separate module. This
seems to be due to the extra scope added as pieces of syntax
cross the module boundary. There is unfortunately no
equivalent to @racket[syntax-local-introduce] that could be
used to flip this module scope.

We therefore require the user to call 
@racket[(set-adt-context)] at the beginning of the file.
This macro stores the scopes present where it was called in
a mutable for-syntax variable:

@chunk[<adt-context>
       (define-for-syntax mutable-adt-context (box #f))]

These scopes are later used as the context for struct
identifiers:

@chunk[<ctx-introduce>
       (define-for-syntax (ctx-introduce id)
         (unless (unbox mutable-adt-context)
           (raise-syntax-error 'adt
                               (~a "(adt-init) must be called in the"
                                   " file (or REPL). ") id))
         (struct-identifier-fresh-introducer
          (replace-context (syntax-local-introduce
                            (unbox mutable-adt-context))
                           id)))]

The @racket[(set-adt-context)] macro should be called at
the beginning of the file or typed in the REPL before using
structures. It simply stores the syntax used to call it in 
@racket[mutable-adt-context].

@chunk[<adt-context>
       (define-for-syntax (set-adt-context ctx)
         (set-box! mutable-adt-context ctx))

       (define-syntax (set-adt-context-macro stx)
         (syntax-case stx ()
           [(_ ctx)
            (begin (set-box! mutable-adt-context #'ctx)
                   #'(void))]))]

For debugging purposes, we provide a macro and a for-syntax
function which show the current ADT context (i.e. the list of
scopes).

@chunk[<adt-context>
       (define-for-syntax (debug-show-adt-context)
         (displayln
          (hash-ref (syntax-debug-info (unbox mutable-adt-context))
                    'context)))
       (define-syntax (debug-show-adt-context-macro stx)
         (debug-show-adt-context)
         #'(define dummy (void)))]

The @tc[struct] identifiers are introduced in a fresh scope
@note{Due to TR bug #399, this feature is temporarily
 disabled, until the bug is fixed.}, so that they do not
conflict with any other user value.

@chunk[<fresh-introducer>
       (define-for-syntax struct-identifier-fresh-introducer
         (λ (x) x) #;(make-syntax-introducer))]

We provide two ways of checking whether @racket[set-adt-context] was called:
@racket[(adt-context?)] returns a boolean, while @racket[(check-adt-context)]
raises an error when @racket[set-adt-context] has not been called.

@chunk[<adt-context?>
       (define-for-syntax (adt-context?)
         (true? (unbox mutable-adt-context)))]

@chunk[<check-adt-context>
       (define-for-syntax (check-adt-context)
         (unless (adt-context?)
           (raise-syntax-error 'phc-adt
                               (string-append
                                "adt-init must be called before"
                                " using the features in phc-adt"))))]

@section{Putting it all together}

@chunk[<*>
       (begin
         (require (for-syntax racket/base
                              racket/syntax
                              racket/set
                              racket/list
                              racket/format
                              phc-toolkit/untyped
                              syntax/strip-context)
                  racket/require-syntax
                  type-expander
                  phc-toolkit
                  remember))

       (provide (for-syntax set-adt-context)
                set-adt-context-macro
                debug-show-adt-context-macro)

       (begin-for-syntax
         (provide debug-show-adt-context
                  adt-context?
                  check-adt-context
                  ctx-introduce))
       
       <adt-context>
       <fresh-introducer>
       <ctx-introduce>
       <adt-context?>
       <check-adt-context>]