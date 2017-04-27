[![Build Status,](https://img.shields.io/travis/jsmaniac/phc-adt/master.svg)](https://travis-ci.org/jsmaniac/phc-adt)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/phc-adt/master.svg)](https://codecov.io/gh/jsmaniac/phc-adt)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/phc-adt)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/phc-adt/)
[![Maintained as of 2017,](https://img.shields.io/maintenance/yes/2017.svg)](https://github.com/jsmaniac/phc-adt/issues)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

phc-adt
=======

This library provides Algebraic Datatypes (structures and variants), with the
features described below. It is designed to work hand-in-hand with our graph
library (https://github.com/jsmaniac/phc), but can also be used on its own.

Structures
----------

We define structures as an extension of Racket's syntax. Structures are
anonymous products of values, where each value is annotated by a field
name. The syntax used for constructing an instance of a structure is:

(structure [field₁ : value₁] … [fieldₙ : valueₙ])

The type of a structure is described using a similar syntax:

(structure [field₁ : type₁] … [fieldₙ : typeₙ])

Accessing the field `f` of an instance `i` is done using the dot operator:
      
    i.f

The main difference with Racket's built-in `struct` is that the type does not
need to be pre-declared to create instances of a structure, making them
effectively anonymous. Racket also requires knowing the name of the struct
type in order to access a field of an instance, using `(s-f i)` where `s` is
the struct's name, `f` is the field name and `i` is the instance. When very
similar structures are used, specifying the structure's name each time becomes
cumbersome.
      
Unions
------

Typed/Racket has built-in untagged union types, which can be constructed using
the following notation:

    (U type₁ … typeₙ)

These cannot be used as-is inside node types for our graph library
(https://github.com/jsmaniac/phc), because there is no way at run-time to
distinguish between the various cases of the union in the general case. For
example, `(U (→ Integer) (→ String))` denotes the type of an union of thunks,
one returning an `Integer` and the other a `String`, and it is not
possible to know to which case an instance belongs without calling the thunks,
which could have undesired side-effects.

The graph library's implementation relies on being able to rewrite arbitrary
data structures, changing the type of some parts in the process. We therefore
allow only a limited form of unions, namely those where all cases but one are
pairs whose first element is a symbol. The first element of the pair can then
be used to distinguish between the cases. The remaining case is used as a
fall-back when the symbol comparison for all others have failed, and should
not overlap with the other cases.

Variants
--------

We define variants (tagged unions) as an alternative to Racket's untagged
unions. In our implementation, variants are anonymous unions of
constructors. Each constructor is a tuple of values annotated with a
tag. Variants are usually not anonymous in other languages: the same
constructor cannot belong to two different variants. In our implementation,
this is not the case: two different variants can contain the same exact
constructor. The reason for this feature is that the input and output types of
graph transformations are very similar, and are likely to contain
nearly-identical variants.  Name collisions would be frequent if a constructor
name was bound to a single variant. We use the following syntax to denote a
variant type:

    (V constructor₁ … constructorₙ)

Constructors are the product of a tag name and a payload, which is itself a
product of zero or more types. Constructors types are declared as follows:

    (constructor tag payload₁ … payloadₙ)

Instances of a constructor can be created using the following syntax:

    (constructor tag v₁ … vₙ)

All constructors within a variant should have a distinct tag, but the same tag
name can be used in the several variants, with an identical or different
payload.

To improve encapsulation, we further define *private* constructors. These have
a unique tag which is uncomparable to all other tags, even those using the
same string of characters for the name. We also define *uninterned*
constructors, which are strict subtypes of the regular constructor with the
same name (i.e. using the same string of characters for the name). Another way
to see these is that a *private* constructor protects both its creation
mechanism and its matching mechanism, whereas the *uninterned* constructors
protect only the creation mechanism, but no special permission is needed to
match on an *uninterned* constructor.

When using Racket's `match` form, an instance of a variant only matches with
its tag (or an uninterned subtype) and the same number of fields.

---

Nodes
-----

Below follows a short description of graph nodes, which are provided by a
separate library (https://github.com/jsmaniac/phc-graph, code is currently
being refactored and migrated from https://github.com/jsmaniac/phc).

Graph nodes behave like tagged structures in most aspects. They are similar to
a constructor containing a structure as its single field. An *incomplete* node
can only be created within a *mapping* of the same graph, using a notation
similar to the one used for structures:
      
 (node-name [field₁ : value₁] … [fieldₙ : valueₙ])

The type of a node can be expressed similarly:

(node-name [field₁ : type₁] [fieldₙ : typeₙ])

It is not possible to access an *incomplete* node's fields. However, once the
graph is fully constructed, the field `f` of the *promise* node `p` can be
accessed using the same dot operator:
      
    p.f

On the other hand, it is not possible to directly create a promise node
without using a graph constructor.

**TODO:** nodes are actually rather similar to uninterned constructors
containing a single value, which is a structure.