#lang at-exp typed/racket
(provide adt-init)
(require remember
         "ctx.hl.rkt"
         phc-toolkit
         (for-syntax (only-in '#%kernel [#%app #%plain-app])
                     syntax/parse
                     syntax/parse/experimental/template
                     phc-toolkit/untyped
                     racket/port
                     mzlib/etc))

(define-for-syntax ((the-trampoline srcdir pre-declarations-filename) stx2)
  (syntax-case stx2 ()
    [(_ self2)
     #`(adt-init-2 self2
                   #,srcdir
                   #,pre-declarations-filename)]))

(define-syntax adt-init
  (syntax-parser
    [(self (~optional pre-declarations-filename
                      #:defaults ([pre-declarations-filename
                                   #'"adt-pre-declarations.rkt"])))
     #'(begin (define-syntaxes (trampoline)
                (#%plain-app the-trampoline
                             (this-expression-source-directory self)
                             'pre-declarations-filename))
              (begin-for-syntax (set-adt-context #'self))
              (trampoline self))]))

(define-syntax/parse (adt-init-2 ctx pre-declarations-dir pre-declarations-file)
  (define pre-declarations-path
    (build-path (syntax-e #'pre-declarations-dir)
                (syntax-e #'pre-declarations-file)))
  (define pre-declarations-path-string
    (path->string pre-declarations-path))

  ;; Initialize the pre-declarations file if it is empty:
  (init-file pre-declarations-path
             "#lang s-exp phc-adt/declarations\n")

  (remember-output-file-parameter pre-declarations-path-string)
  ;(set-adt-context #'ctx)
  #`(require #,(datum->syntax #'ctx (syntax-e #'pre-declarations-file))))

(define-for-syntax (init-file path string-contents)
  (unless (file-exists? path)
    (with-handlers ([exn:fail:filesystem (λ (exn) (void))])
      (with-output-file [port path] #:exists 'error
        (display string-contents port)))))