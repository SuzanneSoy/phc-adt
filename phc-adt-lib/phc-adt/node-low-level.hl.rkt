#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require scribble-enhanced/doc
          racket/require
          hyper-literate
          (for-label racket/format
                     racket/promise
                     racket/list
                     type-expander
                     (except-in (subtract-in typed/racket/base type-expander)
                                values)
                     (only-in racket/base values)
                     (subtract-in racket/contract typed/racket/base)
                     phc-toolkit
                     phc-toolkit/untyped-only
                     remember))
@(unless-preexpanding
  (require (for-label (submod ".."))))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "node-low-level"
       #:tag-prefix "phc-adt/node-low-level"
       ]{Implementation of nodes: printing and equality}

@(chunks-toc-prefix
  '("(lib phc-adt/scribblings/phc-adt-implementation.scrbl)"
    "phc-adt/node-low-level"))

This section discusses the implementation of @tc[prop:custom-write] and
@tc[prop:equal+hash] for nodes.

@(table-of-contents)

@section{Printing nodes}

To avoid printing large and confusing swathes of data when a node is displayed,
we only print its constituents up to a certain depth. The
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{parameter}
@tc[write-node-depth] controls the depth for printing nested nodes.

@CHUNK[<write-node-depth>
       (define write-node-depth (make-parameter 1))]

The @tc[make-node-writer] macro expands to a procedure which prints a node with
the given name and fields. If the @racket[write-node-depth] is @racket[0], then
the contents of the node are elided, and only its name is printed, so that the
resulting printed representation is @racket["(node name …)"] with an actual
ellipsis character.

@CHUNK[<node-custom-write>
       (define-syntax/parse (make-node-writer pid name fieldᵢ …)
         #'(λ (self out mode)
             (if (> (write-node-depth) 0)
                 (parameterize ([write-node-depth (sub1 (write-node-depth))])
                   (fprintf out
                            "(node ~a ~a)"
                            'name
                            (string-join (list <format-field> …) " ")))
                 (fprintf out "(node ~a …)" 'name))))]

Each field is formatted as @tc[[fieldᵢ valueᵢ]]. Copy-pasting the whole printed
form will not form a valid expression which would be @tc[equal?] to the
original. This limitation is deliberate: a node will often refer to many other
nodes, and a stand-alone representation of such a node would result in a very
large printed form. Instead, the user should call the @tc[serialize-graph]
macro, which will produce a complete, canonical @note{The representation is
 canonical so long as unordered sets or hash tables are not used as part of the
 node's contents. In that case, the printed form is canonical modulo the order
 of elements within the set or hash table. Once executed, it will nevertheless
 produce a node which is @racket[equal?] to the original.} and self-contained
representation of the node.

@chunk[<format-field>
       (format "[~a ~a]" 'fieldᵢ (force ((struct-accessor pid fieldᵢ) self)))]

@section{Comparing and hashing nodes}

Nodes are represented like tagged structures, but contain an extra @tc[raw]
field. The @tc[raw] field contains a low-level representation of the node, which
is used to implement node equality. The low-level representation uses the
@tc[raw-node] Racket @racket[struct]. It contains two fields, @tc[database] and
@tc[index]. The first is the database of nodes, as created by the graph
construction macro. It contains one vector of nodes per node type. The second is
a logical pointer into that database, consisting of the node's type's name,
represented as a symbol, and an offset within the corresponding vector,
represented as an @tc[Index].

@chunk[<raw-node>
       (struct/props (D I) raw-node ([database : D] [index : I]) #:transparent
                     <raw-node-equality>)]

A regular with-promises node can have several in-memory representations which
are not pointer-equal. This is due to the fact that the contents of node fields
are wrapped with promises, and accessing the node via two distinct paths will
yield two copies, each with fresh promises. We therefore use the @tc[raw-node]
as a proxy for pointer equality: we know for sure that two nodes are exactly the
same if the @tc[database] and @tc[index] is the same for both nodes.

@chunk[<raw-node-equality>
       #:property prop:equal+hash
       (list (λ (a b r)
               (and (raw-node? a)
                    (raw-node? b)
                    (eq? (raw-node-database a) (raw-node-database b))
                    (equal? (raw-node-index a) (raw-node-index b))))
             (λ (a r)
               (fxxor (eq-hash-code (raw-node-database a))
                      (r (raw-node-index a))))
             (λ (a r)
               (fxxor (eq-hash-code (raw-node-database a))
                      (r (raw-node-index a)))))]

The following function can then be used to test if two nodes are the same, based
on the contents of their @tc[raw] field:

@chunk[<same-node?>
       (define (same-node? a b)
         (and ((struct-predicate node-id) a)
              ((struct-predicate node-id) b)
              (equal? ((struct-accessor node-id raw) a)
                      ((struct-accessor node-id raw) b))))]

To detect cycles within the graph while implementing node equality, we use a
global hash table tracking which nodes have already been visited.

@chunk[<seen-hash-table>
       (define seen-nodes
         : (Parameterof (U #f (HashTable (raw-node Any Any) Any)))
         (make-parameter #f))]

The current implementation uses a mutable hash table. It is only initialised
when @tc[equal?] starts comparing two nodes, so that references to nodes are not
kept once @tc[equal?] finished running. However, in theory, an immutable hash
table could be threaded through all the recursive calls to @tc[equal?].
Unfortunately, the recursive equality function supplied by Racket when
implementing @tc[prop:equal+hash] does not accept an extra parameter to thread
state throughout the recursion. It would therefore be necessary to re-implement
the algorithm used by Racket's @tc[equal?] as described by
@cite[adams2008scheme-equality] tailored to the comparison of data structures
with high-level logical cycles. To be correct, such a re-implementation would
however need to access the @tc[prop:equal+hash] property of other structs, but
Racket provides no public predicate or accessor for that property. Therefore,
although it would, in theory, be possible to implement node equality without
mutable state, Racket's library does not offer the primitives needed to build
it. We therefore settle on using a global, mutable hash table, which will exist
only during the execution of @tc[equal?].

@chunk[<node-equal+hash>
       (define-syntax/parse
           (make-node-comparer common-id node-id name fieldᵢ …)
         (define-temp-ids "~a/τ" (fieldᵢ …))
         #'(let ()
             <same-node?>
             <find-in-table>
             <node-hash>
             (list <node-equal?>
                   <node-equal-hash-code>
                   <node-equal-secondary-hash-code>)))]

@subsection{Hashing nodes}

@tc[equal-hash-code] and @tc[equal-secondary-hash-code] are implemented via
a single function @tc[node-hash], the only difference being the function used to
recursively compute the hash of sub-elements.

@chunk[<node-equal-hash-code>
       (λ (a rec-equal-hash-code)
         (node-hash a rec-equal-hash-code))]

@chunk[<node-equal-secondary-hash-code>
       (λ (a rec-equal-secondary-hash-code)
         (node-hash a rec-equal-secondary-hash-code))]

It would be desirable to implement hashing in the following way: if the current
node is already present in a hash table of seen nodes, but is not @tc[eq?] to
that copy, then the racket hash function is called on the already-seen node.
Otherwise, if the node has never been seen, or if it is @tc[eq?] to the seen
node, the hash code is computed.

The problem with this approach is that it introduces an intermediate recursive
call to Racket's hashing function. When Racket's hashing function is applied to
a structure with the @tc[prop:equal+hash] property, it does @emph{not}
return the result of the struct's hash implementation unmodified.

For example, the code below implements a struct @tc[s] with no fields, which
computes its hash code by recursively calling Racket's hashing function on other
(unique) instances of @tc[s], and returns the constant @tc[1] at a certain
depth. The custom hashing function does not alter in any way the result returned
by Racket's hashing function, however we can see that the hash for the same
instance of @tc[s] depends on the number of recursive calls to Racket's hashing
function @tc[r]. This simple experiment seems to suggest that Racket adds
@tc[50] at each step, but this is not something that can be relied upon.

@(require scribble/eval)
@defs+int[
 {(define recursion-depth (make-parameter #f))
  (struct s (x) #:transparent
    #:property prop:equal+hash
    (list (λ (a b r) (error "Not implemented"))
          (λ (a r)
            (if (= 0 (recursion-depth))
                1
                (parameterize ([recursion-depth (sub1 (recursion-depth))])
                  (r (s (gensym))))))
          (λ (a r) (error "Not implemented"))))
  (define s-instance (s 'x))}
 (parameterize ([recursion-depth 0])
   (equal-hash-code s-instance))
 (parameterize ([recursion-depth 1])
   (equal-hash-code s-instance))
 (parameterize ([recursion-depth 2])
   (equal-hash-code s-instance))]

Since the order of traversal of the nodes is not fixed in the presence of sets
and hash tables, we need to make sure that the recursion depth at which a node's
hash is computed is constant. We achieve this by @emph{always} calling Racket's
hash function on the already-seen node from the hash table, even if was inserted
just now. To distinguish between the current node and the already-seen node from
the hash table, we remove the contents of the node's @tc[raw] field, and replace
them with a special marker.

@chunk[<node-hash>
       (: node-hash (∀ (fieldᵢ/τ …)
                       (→ (node-id fieldᵢ/τ … Any Any) (→ Any Fixnum) Fixnum)))
       (define (node-hash nd racket-recur-hash)
         (if (eq? (raw-node-database ((struct-accessor node-id raw) nd))
                  'unique-copy)
             <compute-hash>
             <hash-init-table-and-recur>))]

When the node's @tc[raw] field does not indicate @tc['unique-copy], we first
initialise the hash table if needed, then recursively call
@tc[racket-recur-hash] on the unique copy of the node:

@chunk[<hash-init-table-and-recur>
       (let ([seen-table (or (seen-nodes)
                             ((inst make-hash (raw-node Any Any) Any)))])
         (parameterize ([seen-nodes seen-table])
           (racket-recur-hash (find-in-table seen-table nd))))]

To obtain the unique copy of the node, we look it up in the hash table, creating
it and adding it to the hash table if the current node is not already present
there:

@chunk[<find-in-table>
       (: find-in-table (∀ (fieldᵢ/τ …)
                           (→ (HashTable (raw-node Any Any) Any)
                              (node-id fieldᵢ/τ … Any Any)
                              Any)))
       (define (find-in-table seen-table nd)
         (hash-ref! seen-table
                    ((struct-accessor node-id raw) nd)
                    (λ () <make-unique-copy-node>)))]

To create a unique copy of the node, we create a new instance of the node's
struct, and copy over all the fields except for the @tc[raw] field, whose value
becomes @tc['unique-copy].

@chunk[<make-unique-copy-node>
       ((struct-constructor node-id) ((struct-accessor common-id fieldᵢ) nd)
                                     …
                                     (raw-node 'unique-copy 'unique-copy))]

The hash code is finally computed by combining the hash code for each field's
contents (after forcing it). The node's tag name is also hashed, and added to
the mix.

@chunk[<compute-hash>
       (combine-hash-codes
        (racket-recur-hash 'name)
        (racket-recur-hash (force ((struct-accessor common-id fieldᵢ) nd)))
        …)]

To combine hash codes, we simply compute their @elem[#:style 'tt]{xor}. Later
versions of this library may use more sophisticated mechanisms.

@chunk[<combine-hash-codes>
       (: combine-hash-codes (→ Fixnum * Fixnum))
       (define (combine-hash-codes . fixnums)
         (apply fxxor fixnums))]

@subsection{Caching node equality}

We provide a mechanism at run-time to cache the result of equality tests
within a limited dynamic scope. The graph generation procedure can coalesce
nodes which are @racket[equal?], which means that it needs to perform a
significant number of equality comparisons, and can therefore benefit from
caching the result of inner equality tests during the execution of the
coalescing operation.

@chunk[<equality-cache>
       (define equality-cache
         : (Parameterof (U #f (HashTable (Pairof (raw-node Any Any)
                                                 (raw-node Any Any))
                                         Boolean)))
         (make-parameter #f))]

The @racket[with-node-equality-cache] form executes its body while enabling
caching of the result of direct and recursive calls to @racket[equal?] on
nodes.

@chunk[<with-node-equality-cache>
       (define-syntax-rule (with-node-equality-cache . body)
         (parameterize ([equality-cache (or (equality-cache)
                                            <make-equality-cache>)])
           . body))]

If necessary, a new equality cache is created, unless
@racket[with-node-equality-cache] is used within the dynamic extent of another
use of itself.

@chunk[<make-equality-cache>
       ((inst make-hash (Pairof (raw-node Any Any) (raw-node Any Any)) Any))]

When comparing two nodes, we first check whether an equality cache is
installed. If so, we attempt to query the cache, and memoize the result of the
comparison when the pair of values is not already in the cache.

@chunk[<memoize-equality>
       (λ (result-thunk)
         (let ([e-cache (equality-cache)])
           (if e-cache
               (cond
                 [(hash-has-key? e-cache (cons a-raw b-raw))
                  (hash-ref e-cache (cons a-raw b-raw))]
                 [(hash-has-key? e-cache (cons b-raw a-raw))
                  (hash-ref e-cache (cons b-raw a-raw))]
                 [else
                  (let ([result (result-thunk)])
                    (hash-set! e-cache (cons a-raw b-raw) result)
                    result)])
               (result-thunk))))]

@subsection{Comparing nodes for equality}

We implement equality following the same architecture as for hash codes, but
check that both nodes are already unique copies. In addition, the implementation
of @tc[equal?] checks that both values are of the node's type.

@chunk[<node-equal?>
       (λ (a b racket-recur-equal?)
         (and ((struct-predicate node-id) a)
              ((struct-predicate node-id) b)
              (let ([a-raw ((struct-accessor node-id raw) a)]
                    [b-raw ((struct-accessor node-id raw) b)])
                (if (and (eq? (raw-node-database a-raw) 'unique-copy)
                         (eq? (raw-node-database b-raw) 'unique-copy))
                    <compare>
                    (or (same-node? a b)
                        (<memoize-equality>
                         (λ () <equality-init-table-and-recur>)))))))]

When either or both of the node's @tc[raw] field do not indicate
@tc['unique-copy], we first initialise the hash table if needed, then
recursively call @tc[racket-recur-hash] on the unique copy of each node:

@chunk[<equality-init-table-and-recur>
       (let ([seen-table (or (seen-nodes)
                             ((inst make-hash (raw-node Any Any) Any)))])
         (parameterize ([seen-nodes seen-table])
           (racket-recur-equal? (find-in-table seen-table a)
                                (find-in-table seen-table b))))]

The nodes are compared pointwise, checking each pair of fields for equality,
after forcing both:

@chunk[<compare>
       (and (racket-recur-equal? (force ((struct-accessor common-id fieldᵢ) a))
                                 (force ((struct-accessor common-id fieldᵢ) b)))
            …)]

@chunk[<*>
       (require racket/promise
                racket/string
                racket/require
                phc-toolkit
                remember
                typed-struct-props
                (for-syntax racket/base
                            racket/syntax
                            racket/list
                            racket/set
                            racket/format
                            (subtract-in syntax/stx phc-toolkit/untyped)
                            syntax/parse
                            phc-toolkit/untyped))
       
       (provide make-node-comparer
                make-node-writer
                raw-node
                write-node-depth
                with-node-equality-cache)

       <equality-cache>
       <with-node-equality-cache>
       <seen-hash-table>
       <write-node-depth>
       <node-custom-write>
       <raw-node>
       <combine-hash-codes>
       <node-equal+hash>]

@define[adams2008scheme-equality
        (string-append "Efficient nondestructive equality checking for trees"
                       " and graphs, Adams and Dybvig, 2008")]
@bibliography[
 @bib-entry[#:key adams2008scheme-equality
            #:title @list{Efficient nondestructive equality checking for trees
              and graphs in @emph{ACM Sigplan Notices} (Vol. 43, No. 9)
              pp. 179–188}
            #:date "2008"
            #:author "Michael D. Adams and R. Kent Dybvig"
            #:url "http://www.cs.indiana.edu/~dyb/pubs/equal.pdf"]]