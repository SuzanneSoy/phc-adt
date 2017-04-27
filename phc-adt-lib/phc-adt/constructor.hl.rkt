#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require racket/require
          scribble-math
          scribble-enhanced/doc
          (subtract-in scribble/core scribble-enhanced/doc)
          xlist/scribble-enhanced
          (for-label (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                     (lib "phc-adt/node-low-level.hl.rkt")
                     (lib "phc-adt/tagged.hl.rkt")
                     xlist
                     racket/list
                     (subtract-in racket/set type-expander)
                     syntax/parse
                     syntax/parse/experimental/template
                     (subtract-in racket/syntax phc-toolkit)
                     phc-toolkit/untyped-only
                     (except-in (subtract-in typed/racket/base type-expander)
                                values)
                     (except-in phc-toolkit ?)
                     multi-id
                     type-expander
                     type-expander/expander))
@doc-lib-setup

@(unless-preexpanding
  (require (for-label (submod ".."))))

@title[#:style (with-html5 manual-doc-style)
       #:tag "constructor"
       #:tag-prefix "phc-adt/constructor"]{User API for constructors}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/constructor"))

@(table-of-contents)

@section{Introduction}

This file defines @tc[constructor], a form which allows tagging values, so that
two otherwise identical values can be distinguished by the constructors used to
wrap them. Coupled with the variants defined by this library, it implements a
slight variation on the constructors and variants commonly found in other
languages. The @tc[constructor] form is effectively a wrapper around @tc[tagged]
structures, which stores all values within a single field named @tc[values].

The constructors defined in this library are "interned", meaning that two
constructors in different files will be the same if they use same tag name. In
other words, the tag of a constructor works in the same way as a symbol in
Racket: unless otherwise specified, the same string of characters will always
produce the same symbol, even across modules. The same goes for constructors:
the same constructor name will always refer to the same type.

@section{The polyvalent identifier @racket[constructor]:
 type, match, builder and instance}

We define the @tc[constructor] macro which acts as a type, a match expander, and
a constructor function (which can be called to create a tagged value, i.e. a
constructor instance). It can also be directly given a value to directly produce
a tagged value, i.e. a constructor instance.

@chunk[<constructor>
       (define-multi-id constructor
         #:type-expander  (make-rest-transformer <type-expander>)
         #:match-expander (make-rest-transformer <match-expander>)
         #:call           (make-rest-transformer <call-expander>))]

The @tc[constructor?] macro returns a predicate for the
given constructor name, or checks if a value is an instance
of the given constructor name. This form is implemented in 
@racket[<predicate>] below.

@chunk[<constructor?>
       (define-syntax constructor? (make-rest-transformer <predicate>))]

@section{Type-expander}

@chunk[#:save-as constructor-type-types-mixin <constructor-type-types-mixin>
       (define-eh-alternative-mixin types-mixin
         (pattern
          (~maybe/empty (~after name-order-point <name-after-field-error>
                                τᵢ:type … {~lift-rest τ-rest}))))]


@chunk[#:save-as name-after-field-error <name-after-field-error>
       "The name must appear before any value or type"]

@chunk[#:save-as name-id-mixin <name-id-mixin>
       (define-eh-alternative-mixin name-id-mixin
         (pattern
          (~once (~order-point name-order-point name:id))))]

@chunk[#:save-as ∀-mixin <∀-mixin>
       (define-eh-alternative-mixin ∀-mixin
         (pattern {~optional (~seq #:∀ ({~named-seq tvarᵢ :id …})
                                   (~global-or tvars?)
                                   #;(~global-or [no-types? #f])
                                   #;<∀-fail-no-types>)}))]

@; TODO: this depends on the order in which mixins are included, because
@; no-types? may be declared by a mixin included later on.
@chunk[#:save-as ∀-fail-no-types <∀-fail-no-types>
       #|
       {~post-fail (string-append "Expected [field:id type:expr] … or"
       " [field:id : type:expr] … because #:∀ is"
       " used")
       #:when (attribute no-types?)}
       |#]

The type-expander for @tc[constructor] expects:

@(require scribble/decode)

@itemlist[
 @item{The constructor's tag name, as defined for the tagged call expander in
       @(make-link-element
         #f
         (racket <name-id-mixin>)
         `(elem (prefixable "(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
                            "phc-adt/tagged"
                            "chunk:<name-id-mixin>:1:1"))):

  @(name-id-mixin)}
 @item{An optional list of type variables, as defined for the tagged call
  expander in
  @(make-link-element
         #f
         (racket <∀-mixin>)
         `(elem (prefixable "(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
                            "phc-adt/tagged"
                            "chunk:<∀-mixin>:1:1"))):

  @(∀-mixin)}
 @item{An optional list of types:

  @(constructor-type-types-mixin)}]

The three elements can appear in any order, with one constraint: the name must
appear before the first type. Not only does it make more sense semantically,
but it also avoids ambiguities when some of the types are plain type
identifiers.

@(name-after-field-error)

@chunk[<constructor-type-args-mixin>
       (define-eh-alternative-mixin constructor-type-seq-args-mixin
         #:define-syntax-class constructor-type-seq-args-syntax-class
         (pattern {~mixin name-id-mixin})
         (pattern {~mixin types-mixin})
         (pattern {~mixin ∀-mixin}))]

The type expander handles two cases: when type variables are present, it uses
the low-level function @racket[tagged-∀-type!], otherwise it uses the low-level
function @racket[tagged-type!]. The constructor contains a (possibly improper)
list of values. The type of that list is expressed using the syntax of the
@racketmodname[xlist] library.

@chunk[<type-expander>
       (λ/syntax-parse :constructor-type-seq-args-syntax-class
         (if (attribute tvars?)
             (tagged-∀-type! #'((tvarᵢ …) name [values (xlist τᵢ … . τ-rest)]))
             (tagged-type! #'(name [values (xlist τᵢ … . τ-rest)]))))]

@section{Match-expander}

@CHUNK[<match-expander>
       (syntax-parser
         [(name:id . pats)
          (tagged-match! #'(name [values (xlist . pats)]))])]

The match expander simply matches the given patterns against the constructor's
single field, @racket[values]. The patterns will usually match one value each,
but the @racket[xlist] pattern expander allows a more flexible syntax than the
regular @racket[list] match pattern.

@section{Predicate}

The @racket[constructor?] macro expands to a predicate and accepts the same
syntax as for the type expander, without polymorphic variables. Additionally the
resulting type as expanded by @racket[xlist] must be a suitable argument to
@racket[make-predicate].

@CHUNK[<predicate>
       (λ/syntax-parse (name:id . types)
         (tagged-predicate! #'(name [values (xList . types)])))]

@section{Instance creation}

The @racket[constructor] macro can return a builder function or an instance. It
accepts the following syntaxes:

@chunk[#:save-as value-maybe-type <value-maybe-type>
       (define-syntax-class value-maybe-type
         (pattern [vᵢ :colon τᵢ:type] #:with aᵢ #'τᵢ #:with (tvarₖ …) #'())
         (pattern [:colon τᵢ:type vᵢ] #:with aᵢ #'τᵢ #:with (tvarₖ …) #'())
         (pattern vᵢ:literal-value
                  #:with τᵢ #'vᵢ.type
                  #:with aᵢ #'vᵢ.type
                  #:with (tvarₖ …) #'())
         (pattern (~and vᵢ (~not #:rest))
                  #:with τᵢ (gensym 'τ)
                  #:attr aᵢ #f
                  #:with (tvarₖ …) #'(τᵢ)))]

@CHUNK[#:save-as literal-value <literal-value>
       (define-syntax-class literal-value
         (pattern n:number             #:with type #'n)
         (pattern s:str                #:with type #'s)
         (pattern b:boolean            #:with type #'b)
         (pattern c:char               #:with type #'Char)
         (pattern ((~literal quote) v) #:with type (replace-chars #'v))
         (pattern v
                  #:when (vector? (syntax-e #'v))
                  #:with type (replace-chars #'v)))]

@chunk[#:save-as replace-chars <replace-chars>
       ;https://github.com/racket/typed-racket/issues/434
       (define (replace-chars t)
         (cond [(syntax? t)  (datum->syntax t
                                            (replace-chars (syntax-e t))
                                            t
                                            t)]
               [(pair? t)    (list 'Pairof
                                   (replace-chars (car t))
                                   (replace-chars (cdr t)))]
               [(char? t)    'Char]
               [(vector? t)  (cons 'Vector (map replace-chars
                                                (vector->list t)))]
               [(null? t)    'Null]
               [(number? t)  t]
               [(string? t)  t]
               [(boolean? t) t]
               (code:comment "Hope for the best.")
               (code:comment "We really should use a ∀ tvar instead.")
               [else         (list 'quote t)]))]
               
@chunk[#:save-as infer-pat <infer-pat>
       (~after name-order-point <name-after-field-error>
               {~literal *})]

@CHUNK[#:save-as call-expander-infer-case <call-expander-cases>
       [(~no-order {~mixin ∀-mixin} {~mixin name-id-mixin} {~once <infer-pat>})
        #`(… (λ #:∀ (A ...) [l : A ... A]
               (#,(tagged-builder! #'(… (name [values (List A ... A)])))
                l)))]]

@chunk[#:save-as colon-pat <colon-pat>
       (~after name-order-point <name-after-field-error>
               :colon τᵢ …
               {~lift-rest {~and τ-rest ()}})]

@CHUNK[#:save-as call-expander-:-case <call-expander-cases>
       [(~no-order {~mixin ∀-mixin} {~mixin name-id-mixin} {~once <colon-pat>})
        (define-temp-ids "~a/arg" (τᵢ …))
        #`(λ #,@(when-attr tvars? #'(#:∀ (tvarᵢ …))) ([τᵢ/arg : τᵢ] …)
            (#,(tagged-builder! #'(name [values (List τᵢ …)]))
             (list τᵢ/arg …)))]]

@chunk[#:save-as !-pat <!-pat>
       (~after name-order-point <name-after-field-error>
               {~datum !} τᵢ … {~lift-rest τ-rest})]

@CHUNK[#:save-as call-expander-!-case <call-expander-cases>
       [(~no-order {~mixin ∀-mixin} {~mixin name-id-mixin} {~once <!-pat>})
        #`(λ [l : Any *]
            (#,(tagged-builder! #'(name [values (xList τᵢ … . τ-rest)]))
             (cast l (xlist τᵢ … . τ-rest))))]]

@chunk[#:save-as dcolon-pat <dcolon-pat>
       (~after name-order-point <name-after-field-error>
               {~datum ::} τᵢ … {~lift-rest τ-rest})]

@CHUNK[#:save-as call-expander-::-case <call-expander-cases>
       [(~no-order {~mixin ∀-mixin} {~mixin name-id-mixin} {~once <dcolon-pat>})
        (if (attribute tvars?)
            (tagged-builder!   #'(name
                                  [values (xlist τᵢ … . τ-rest)]))
            (tagged-∀-builder! #'((tvarᵢ …)
                                  name
                                  [values (xList τᵢ … . τ-rest)])))]]

@CHUNK[#:save-as call-expander-values-case <call-expander-cases>
       [(~no-order {~mixin ∀-mixin}
                   {~mixin name-id-mixin}
                   (~maybe/empty
                    (~after name-order-point <name-after-field-error>
                            :value-maybe-type …
                            <call-expander-rest>)))
        (define-temp-ids "~a/arg" (τᵢ …))
        (quasitemplate
         (#,(tagged-∀-builder! #'((tvarᵢ … tvarₖ … … tvar-rest …)
                                  name
                                  [values (xlist τᵢ … #:rest x-τ-rest)]))
          (list* {?? (ann vᵢ aᵢ) vᵢ}
                 …
                 {?? (ann v-rest a-rest) v-rest})))]]

@CHUNK[#:save-as call-expander-rest <call-expander-rest>
       (~either <call-expander-rest-keyword>
                <call-expander-empty-rest>
                <call-expander-dotted-rest>)]

@(define comment1 "pattern for the value, infers type for literals")
@(define-for-syntax comment1 "pattern for the value, infers type for literals")
@chunk[#:save-as call-expander-rest-keyword <call-expander-rest-keyword>
       (~as-rest #:rest
                 (code:comment #,comment1)
                 (~either (~and v-rest:literal-value
                                {~with a-rest #'v-rest.type})
                          (~and v-rest
                                {~attr a-rest #f}))
                 (~either (~and {~seq}
                                {~with x-τ-rest      (gensym 'x-τ-rest)}
                                {~with (tvar-rest …) #'(x-τ-rest)})
                          (~and (~seq :colon x-τ-rest)
                                {~with (tvar-rest …) #'()})))]

@CHUNK[#:save-as call-expander-empty-rest <call-expander-empty-rest>
       (~seq
        (~lift-rest
         (~and ()
               {~with v-rest        #'null}
               {~with a-rest        #'Null}
               {~with x-τ-rest      #'Null}
               {~with (tvar-rest …) #'()})))]

@CHUNK[#:save-as call-expander-dotted-rest <call-expander-dotted-rest>
       (~seq
        (~lift-rest
         (~either (~and v-rest:type-label
                        (~with x-τ-rest      #'v-rest.type)
                        {~with a-rest        #'v-rest.type}
                        (~with (tvar-rest …) #'()))
                  (~and v-rest:literal-value
                        (~with x-τ-rest      #'v-rest.type)
                        {~with a-rest        #'v-rest.type}
                        (~with (tvar-rest …) #'()))
                  (~and v-rest
                        (~with x-τ-rest      (gensym 'x-τ-rest))
                        {~attr a-rest        #f}
                        (~with (tvar-rest …) #'(x-τ-rest))))))]

@CHUNK[#:save-as type-label-syntax-class <type-label-syntax-class>
       (define-syntax-class type-label
         #:attributes (type raw-type)
         (pattern v
                  #:attr raw-type (syntax-property #'v-rest 'type-label)
                  #:when (attribute raw-type)
                  #:attr type     (datum->syntax #'v-rest
                                                 (attribute raw-type)
                                                 #'v-rest)))]

@itemlist[
 @item{@racket[(constructor name *)], which returns a polymorphic builder
  function that infers the type of its arguments. All arguments are aggregated
  into a list with the inferred type for each element, and that list is used as
  the constructor's value.

  @(infer-pat)
  
  @(call-expander-infer-case)}
 @item{@racket[(constructor : τᵢ …)], which returns a builder function. This
  does not support the extended @racket[xlist] syntax, as Typed/Racket's
  function types are not expressive enough to support it.

  @(colon-pat)
  
  @(call-expander-:-case)}
 @item{@racket[(constructor ! . _xlist-type)], which returns a builder function
  expecting the values as a rest argument, and casts the list at runtime. The
  @racket[_xlist-type] must be a valid sequence of types for the type form of
  @racket[xlist], and the result must be a suitable argument to
  @racket[make-predicate].

  @(!-pat)
  
  @(call-expander-!-case)}
 @item{@racket[(constructor :: . _xlist-type)], which returns a builder function
  expecting the whole list of values as a single argument, and returns the
  constructor instance containing that list. The @racket[_xlist-type] must be a
  valid sequence of types for the type form of @racket[xlist].

  @(dcolon-pat)

  @(call-expander-::-case)}
 @item{@racket[(constructor _value-maybe-typeᵢ … . rest)], which returns an
  instance containing a (possibly improper) list with the given values and
  @racket[rest] as the tail of the list. If @racket[rest] is @racket[()], then
  the result is a proper list.
  
  @;@(constructor-value-mixin)
  @(call-expander-values-case)
  
  Each @racket[_value-maybe-typeᵢ] may be one of:
  @itemlist[
 @item{@racket[[valᵢ : τᵢ]]}
 @item{@racket[[: τᵢ valᵢ]]}
 @item{@racket[valᵢ]}]

  @(value-maybe-type)

  Literals are specially recognised so that their type is preserved with as much
  precision as possible:

  @(literal-value)

  As noted in Typed/Racket bug
  @hyperlink["https://github.com/racket/typed-racket/issues/434"]{#434}, literal
  characters are not currently recognised as belonging to their own singleton
  type. We therefore rewrite the type for quoted data to turn literal characters
  into the @racket[Char] type:

  @(replace-chars)

  Optionally, a rest element may be specified using the following syntax:
  @(call-expander-rest)

  @(call-expander-rest-keyword)
  @(call-expander-empty-rest)
  @(call-expander-dotted-rest)

  The last case depends on the @racket[type-label?] syntax class to recognise
  uses of the @elem[#:style 'tt "#{val : type}"] type annotation syntax from
  @racketmodname[typed/racket]. Typed/Racket enables that reader extension,
  which embeds the type into the value as a syntax property for later use by the
  type checker

  @(type-label-syntax-class)}]

All four forms accept a @racket[#:∀ (tvarᵢ …)] specification, and the fourth
injects a @racket[tvarᵢ] type variable for values for which no type is given.

@CHUNK[<call-expander>
       (syntax-parser
         <call-expander-cases>)]

@section{Defining shorthands for constructors with @racket[define-constructor]}

The @racket[define-constructor] macro binds an identifier to a type-expander,
match-expander and call-expander for the constructor with the same name. It
also defines a predicate for that constructor type.

@;; Copied over from tagged.hl.rkt without any change.

@chunk[#:save-as tag-kw-mixin <tag-kw-mixin>
       (define-eh-alternative-mixin tag-kw-mixin
         (pattern {~optional {~seq #:tag explicit-tag <default-tag-name>}}))]

@chunk[#:save-as tag-kw-mixin-default <default-tag-name>
       {~post-check
        {~bind [tag-name (or (attribute explicit-tag)
                             #'name)]}}]

@chunk[#:save-as predicate?-mixin <predicate?-mixin>
       (define-eh-alternative-mixin predicate?-mixin
         (pattern {~optional {~seq #:? predicate? <default-name?>}}))]

@chunk[#:save-as predicate?-mixin-default <default-name?>
       {~post-check
        {~bind [name? (or (attribute predicate?)
                          (format-id/record #'name "~a?" #'name))]}}]

Like @tc[define-tagged], the @tc[constructor] macro expects:

@itemlist[
 @item{The tagged structure's tag name, as defined for the call expander in
  @racket[<name-id-mixin>]}
 @item{An optional list of type variables, as defined for the call expander in
  @racket[<∀-mixin>]}
 @item{Optionally, the tag name to be used, specified with
  @racket[#:tag tag-name] as for @racket[define-tagged] in
  @secref["Defining_shorthands_with_define-tagged"
          #:tag-prefixes '("phc-adt/tagged")]:

  @(tag-kw-mixin)

  The tag name defaults to @racket[_name], i.e. the identifier currently being
  defined.

  @(tag-kw-mixin-default)}
 @item{Optionally, a name for the predicate, specified with
  @racket[#:? predicate-name?] as for @racket[define-tagged] in
  @secref["Defining_shorthands_with_define-tagged"
          #:tag-prefixes '("phc-adt/tagged")]:
  
  @(predicate?-mixin)

  The predicate name defaults to @racket[_name?], where @racket[_name] is the
  identifier currently being defined.

  @(predicate?-mixin-default)}]

Unlike @tc[define-tagged], which also expects a list of field names possibly
annotated with a type, the @tc[constructor] macro instead expects a
description of the list of values it contains. Three syntaxes are accepted:

@itemlist[
 @item{@(colon-pat)}
 @item{@(!-pat)}
 @item{@(dcolon-pat)}]

These syntaxes control how the call expander for the defined @racket[_name]
works, and have the same meaning as in the call expander for
@racket[constructor] (@racket[xlist], @racket[cast] and single-argument
@racket[xlist]).
 
@chunk[<define-constructor>
       (define-syntax define-constructor
         (syntax-parser-with-arrows
          [(_ . (~no-order {~mixin name-id-mixin}
                           {~mixin ∀-mixin}
                           {~mixin tag-kw-mixin}
                           {~mixin predicate?-mixin}
                           (~once
                            (~and (~seq type-decls …)
                                  (~either <colon-pat>
                                           <!-pat>
                                           <dcolon-pat>)))))
           #:with tvarᵢ→Any (stx-map (const #'Any) #'(tvarᵢ …))
           <normalize-type/define>
           (quasisyntax/top-loc stx
             (begin
               <multi-id/define>
               <predicate/define>))]))]

@chunk[<multi-id/define>
       (define-multi-id name
         #:type-expander  (make-id+call-transformer <type-expander/define>)
         #:match-expander (make-rest-transformer    <match-expander/define>)
         #:else                                     <call-expander/define>)]

@; exact copy-paste from the type expander: TODO: factor it out.
@CHUNK[<type-expander/define> 
       #'(constructor tag-name
                      #,@(when-attr tvars? #'(#:∀ (tvarᵢ …)))
                      τᵢ … . τ-rest)]

@CHUNK[<call-expander/define>
       #'(constructor tag-name
                      #,@(when-attr tvars? #'(#:∀ (tvarᵢ …)))
                      type-decls …)]

In order to attach patterns to the @racket[xlist] type, pre-process the types
using @racket[normalize-xlist-type].

@chunk[<normalize-type/define>
       #:with <with-normalize> (normalize-xlist-type #'(τᵢ … . τ-rest) stx)]

Once normalized, the types for the @racket[xlist] are all of the form
@racket[τᵢ ^ {repeat …}], except for the rest type, which is always present
including when it is @racket[Null], and is specified using
@racket[#:rest rest-type].

@chunk[<with-normalize>
       ({~seq normalized-τᵢ {~literal ^} (normalized-repeat …)} …
        #:rest normalized-rest)]

We then define an argument for the pattern expander corresponding to each type
within the normalized sequence:

@chunk[<normalize-type/define>
       (define-temp-ids "~a/pat" (normalized-τᵢ …))]

The match expander expects these patterns and a rest pattern:

@CHUNK[<match-expander/define>
       (syntax-parser
         [({~var normalized-τᵢ/pat} … . {~either <match-rest-signature/define>})
          #'#,(tagged-match! #'(name [values <match-xlist/define>]))])]

The rest pattern can be specified either using a dotted notation if it is a
single term, using @racket[#:rest pat-rest], or can be omitted in which case
it defaults to matching @racket[null]. The following syntaxes are therefore
accepted:

@chunk[<match-rest-signature/define>
       (#:rest pat-rest)
       (~and () {~bind [pat-rest #'(? null?)]})
       pat-rest:not-stx-pair]

The match expander produces an @racket[xlist] pattern using the given patterns
and the optional rest pattern. The given patterns are repeated as within the
type specification.

@chunk[<match-xlist/define>
       (and (? (make-predicate (xlist τᵢ … . τ-rest)))
            (split-xlist (list normalized-τᵢ/pat … pat-rest)
                         τᵢ … . τ-rest))]

@CHUNK[<predicate/define>
       (define name? 
         #,(if (attribute tvars?)
               (tagged-predicate!
                #'(tag-name [values ((xlist τᵢ … . τ-rest) tvarᵢ→Any)]))
               (tagged-predicate!
                #'(tag-name [values (xlist τᵢ … . τ-rest)]))))]

@; TODO: add a #:predicate-type option.

@section{Miscellanea}

@chunk[<constructor-values>
       (define-syntax constructor-values
         (make-id+call-transformer-delayed
          (λ () #'(λ-tagged-get-field values))))]

@CHUNK[<ConstructorTop?>
       (define-syntax ConstructorTop?
         (make-id+call-transformer-delayed
          (λ ()
            #`(struct-predicate
               #,(check-remembered-common!
                  #'(always-remembered values))))))]

@CHUNK[<ConstructorTop>
       (define-type-expander (ConstructorTop stx)
         (syntax-case stx ()
           [id
            (identifier? #'id)
            #'((check-remembered-common!
                #'(always-remembered values))
               Any)]))]

@section{Putting it all together}

@chunk[<*>
       (require phc-toolkit
                "tagged.hl.rkt"
                "tagged-structure-low-level.hl.rkt"
                (only-in match-string [append match-append])
                type-expander
                xlist
                multi-id
                (for-syntax racket/base
                            syntax/parse
                            syntax/parse/experimental/template
                            racket/contract
                            racket/syntax
                            racket/string
                            racket/function
                            racket/list
                            type-expander/expander
                            phc-toolkit/untyped
                            extensible-parser-specifications))

       (provide constructor-values
                constructor
                constructor?
                ConstructorTop
                ConstructorTop?
                define-constructor
                (for-syntax constructor-type-seq-args-syntax-class))

       (begin-for-syntax
         (define-syntax-class not-stx-pair
           (pattern {~not (_ . _)}))
         <type-label-syntax-class>
         <name-id-mixin>
         <∀-mixin>
         <constructor-type-types-mixin>
         <constructor-type-args-mixin>
         <tag-kw-mixin>
         <predicate?-mixin>
         <replace-chars>
         <literal-value>
         <value-maybe-type>)
       
       <constructor>
       <constructor?>
       <ConstructorTop>
       <ConstructorTop?>
       <define-constructor>
       <constructor-values>]