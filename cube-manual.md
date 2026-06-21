<a id="x-28MGL-CUBE-3A-40CUBE-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-MANUAL%20MGL-PAX:SECTION"></a>

# Cube Manual

## Table of Contents

- [1 Links][3912]
- [2 Introduction][43a2]
- [3 Basics][f36f]
- [4 Synchronization][a5a6]
- [5 Facets][0ef1]
- [6 Facet Extension API][1eda]
- [7 The Default Implementation of `call-with-facet*`][d720]
- [8 Lifetime][a810]
    - [8.1 Facet Barriers][0fc8]

###### \[in package MGL-CUBE\]
<a id="x-28MGL-CUBE-3A-40CUBE-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-LINKS%20MGL-PAX:SECTION"></a>

## 1 Links

Here is the [official
repository](https://github.com/melisgl/mgl-mat) and the [HTML
documentation](http://melisgl.github.io/mgl-mat/cube-manual.html)
for the latest version.

<a id="x-28MGL-CUBE-3A-40CUBE-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-INTRODUCTION%20MGL-PAX:SECTION"></a>

## 2 Introduction

This is the library on which MGL-MAT (see [MAT Manual][f470])
is built. The idea of automatically translating between various
representations may be useful for other applications, so this got
its own package and all ties to MGL-MAT has been severed.

This package defines [`cube`][1659], an abstract base class that provides a
framework for automatic conversion between various representations
of the same data. To define a cube, `cube` needs to be subclassed and
the [Facet Extension API][1eda] be implemented.

If you are only interested in how to use cubes in general, read
[Basics][f36f], [Lifetime][a810] and [Facet Barriers][0fc8].

If you want to implement a new cube datatype, then see [Facets][0ef1],
[Facet Extension API][1eda], and [The Default Implementation of `call-with-facet*`][d720].

<a id="x-28MGL-CUBE-3A-40CUBE-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-BASICS%20MGL-PAX:SECTION"></a>

## 3 Basics

Here we learn what a [`cube`][1659] is and how to access the data in it with
[`with-facet`][0706].

<a id="x-28MGL-CUBE-3ACUBE-20CLASS-29"></a>
<a id="MGL-CUBE:CUBE%20CLASS"></a>

- [class] **cube**

    A datacube that has various representations of the
    same stuff. These representations go by the name \`facet'. Clients
    must use [`with-facet`][0706] to acquire a dynamic extent reference to a
    facet. With the information provided in the `direction` argument of
    `with-facet`, the cube keeps track of which facets are up-to-date and
    copies data between them as necessary.
    
    The cube is an abstract class, it does not provide useful behavior
    in itself. One must subclass it and implement the
    [Facet Extension API][1eda].
    
    Also see [Lifetime][a810] and [Facet Barriers][0fc8].

<a id="x-28MGL-CUBE-3AWITH-FACET-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CUBE:WITH-FACET%20MGL-PAX:MACRO"></a>

- [macro] **with-facet** *(var (cube facet-name &key (direction :io) type)) &body body*

    Find or create the facet with `facet-name` in `cube` and bind `var` to
    the representation of `cube`'s data provided by that facet. This
    representation is called the facet's *value*. The value is to be
    treated as dynamic extent: it is not allowed to keep a reference to
    it. For the description of the `direction` parameter, see the type
    [`direction`][69de].
    
    If `type` is specified, then `var` is declared to be of that type.

<a id="x-28MGL-CUBE-3ADIRECTION-20TYPE-29"></a>
<a id="MGL-CUBE:DIRECTION%20TYPE"></a>

- [type] **direction**

    Used by [`with-facet`][0706], `direction` can be `:input`, `:output` or `:io`.
    
    - `:input` promises that the facet will only be read and never
      written. Other up-to-date facets of the same cube remain
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      [`select-copy-source-for-facet*`][7a49]).
    
    - `:output` promises that *all* data will be overwritten without
      reading any data. All up-to-date facets become non-up-to-date,
      while this facet is marked as up-to-date. No copying of data takes
      place.
    
    - `:io` promises nothing about the type of access. All up-to-date
      facets become non-up-to-date, while this facet is marked as
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      `select-copy-source-for-facet*`).
    
    Any number of `with-facet`s with direction `:input` may be active at
    the same time, but `:io` and `:output` cannot coexists with another
    `with-facet` regardless of the direction. The exception for this rule
    is that an inner `with-facet` does not conflict with an enclosing
    `with-facet` if they are for the same facet (but inner `with-facet`s
    for another facet or for the same facet from another thread do).
    
    See [`check-no-writers`][ea8f] and [`check-no-watchers`][face] called by
    [The Default Implementation of `call-with-facet*`][d720].

<a id="x-28MGL-CUBE-3AWITH-FACETS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CUBE:WITH-FACETS%20MGL-PAX:MACRO"></a>

- [macro] **with-facets** *(&rest facet-binding-specs) &body body*

    A shorthand for writing nested [`with-facet`][0706] calls.
    
    ```
    (with-facet (f1 (c1 'name1 :direction :input))
      (with-facet (f2 (c2 'name2 :direction :output))
        ...))
    ```
    
    is equivalent to:
    
    ```
    (with-facets ((f1 (c1 'name1 :direction :input))
                  (f2 (c2 'name2 :direction :output)))
      ...)
    ```

<a id="x-28MGL-CUBE-3A-40CUBE-SYNCHRONIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-SYNCHRONIZATION%20MGL-PAX:SECTION"></a>

## 4 Synchronization

Cubes keep track of which facets are used, which are up-to-date to
be able to perform automatic translation between facets. [`with-facet`][0706]
and other operations access and make changes to this metadata so
thread safety is a concern. In this section, we detail how to relax
the default thread safety guarantees.

A related concern is async signal safety which arises most often
when C-c'ing or killing a thread or when the extremely nasty
WITH-TIMEOUT macro is used. In a nutshell, changes to cube metadata
are always made with interrupts disabled so things should be async
signal safe.

<a id="x-28MGL-CUBE-3ASYNCHRONIZATION-20-28MGL-PAX-3AACCESSOR-20MGL-CUBE-3ACUBE-29-29"></a>
<a id="MGL-CUBE:SYNCHRONIZATION%20%28MGL-PAX:ACCESSOR%20MGL-CUBE:CUBE%29"></a>

- [accessor] **synchronization** *[cube][1659] (:synchronization = \*default-synchronization\*)*

    By default, setup and teardown of facets by
    [`with-facet`][0706] is performed in a thread safe way. Corrupting internal
    data structures of cubes is not fun, but in the name of
    performance, synchronization can be turned off either dynamically
    or on a per instance basis.
    
    If `t`, then access to cube metadata is always synchronized. If `nil`,
    then never. If `:maybe`, then whether access is synchronized is
    determined by [`*maybe-synchronize-cube*`][8324] that's true by default.
    
    The default is the value of [`*default-synchronization*`][e2c4]
    that's `:maybe` by default.
    
    Note that the body of a `with-facet` is never synchronized with
    anyone, apart from the implicit reader/writer conflict (see
    [`direction`][69de]).

<a id="x-28MGL-CUBE-3A-2ADEFAULT-SYNCHRONIZATION-2A-20VARIABLE-29"></a>
<a id="MGL-CUBE:*DEFAULT-SYNCHRONIZATION*%20VARIABLE"></a>

- [variable] **\*default-synchronization\*** *:maybe*

    The default value for [`synchronization`][7bcf] of new cubes.

<a id="x-28MGL-CUBE-3A-2AMAYBE-SYNCHRONIZE-CUBE-2A-20VARIABLE-29"></a>
<a id="MGL-CUBE:*MAYBE-SYNCHRONIZE-CUBE*%20VARIABLE"></a>

- [variable] **\*maybe-synchronize-cube\*** *t*

    Determines whether access the cube metadata is synchronized for
    cubes with [`synchronization`][7bcf] `:maybe`.

<a id="x-28MGL-CUBE-3A-40CUBE-FACETS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-FACETS%20MGL-PAX:SECTION"></a>

## 5 Facets

The basic currency for implementing new cube types is the [`facet`][36d6].
Simply using a cube only involves facet names and values, never
facets themselves.

<a id="x-28MGL-CUBE-3AFACETS-20FUNCTION-29"></a>
<a id="MGL-CUBE:FACETS%20FUNCTION"></a>

- [function] **facets** *cube*

    Return the facets of `cube`.

<a id="x-28MGL-CUBE-3AFIND-FACET-20FUNCTION-29"></a>
<a id="MGL-CUBE:FIND-FACET%20FUNCTION"></a>

- [function] **find-facet** *cube facet-name*

    Return the facet of `cube` for the facet with `facet-name` or `nil` if no
    such facet exists.

<a id="x-28MGL-CUBE-3AFACET-20STRUCTURE-29"></a>
<a id="MGL-CUBE:FACET%20STRUCTURE"></a>

- [structure] **facet**

    A cube has facets, as we discussed in [Basics][f36f]. Facets holds
    the data in a particular representation, this is called the *value*
    of the facet. A facet holds one such value and some metadata
    pertaining to it: its [`facet-name`][8ce7], whether it's
    up-to-date ([`facet-up-to-date-p`][a34e]), etc. `facet` objects are never seen
    when simply using a cube, they are for implementing the
    [Facet Extension API][1eda].

<a id="x-28MGL-CUBE-3AFACET-NAME-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-NAME%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-name** *facet*

    A symbol that uniquely identifies the facet within a cube.

<a id="x-28MGL-CUBE-3AFACET-VALUE-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-VALUE%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-value** *facet*

    This is what's normally exposed by [`with-facet`][0706].

<a id="x-28MGL-CUBE-3AFACET-DESCRIPTION-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-DESCRIPTION%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-description** *facet*

    Returned by [`make-facet*`][6afc] as its second value, this is an
    arbitrary object in which additional information can be
    stored.

<a id="x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-UP-TO-DATE-P%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-up-to-date-p** *facet*

    Whether the cube has changed since this facet has been last
    updated. See [`facet-up-to-date-p*`][696b].

<a id="x-28MGL-CUBE-3AFACET-N-WATCHERS-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-N-WATCHERS%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-n-watchers** *facet*

    The number of active [`with-facet`][0706]s. Updated by [`watch-facet`][f791] and
    [`unwatch-facet`][2ce8].

<a id="x-28MGL-CUBE-3AFACET-WATCHER-THREADS-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-WATCHER-THREADS%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-watcher-threads** *facet*

    The threads (one for each watcher) that have active
    [`with-facet`][0706]s.

<a id="x-28MGL-CUBE-3AFACET-DIRECTION-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-CUBE-3AFACET-29-29"></a>
<a id="MGL-CUBE:FACET-DIRECTION%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29"></a>

- [structure-accessor] **facet-direction** *facet*

    The direction of the last [`with-facet`][0706] on this facet.

<a id="x-28MGL-CUBE-3A-40CUBE-FACET-EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-FACET-EXTENSION-API%20MGL-PAX:SECTION"></a>

## 6 Facet Extension API

Many of the generic functions in this section take [`facet`][36d6] arguments.
`facet` is a structure and is not intended to be subclassed. To be
able to add specialized methods, the name of the
facet ([`facet-name`][8ce7]) is also passed as the
argument right in front of the corresponding facet argument.

In summary, define `eql`([`0`][db03] [`1`][5fd4]) specializers on facet name arguments, and use
[`facet-description`][65d4] to associate arbitrary information with facets.

<a id="x-28MGL-CUBE-3AMAKE-FACET-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:MAKE-FACET*%20GENERIC-FUNCTION"></a>

- [generic-function] **make-facet\*** *cube facet-name*

    Called by [`with-facet`][0706] (or more directly [`watch-facet`][f791])
    when there is no facet with `facet-name`. As the first value, return a
    new object capable of storing `cube`'s data in the facet with
    `facet-name`. As the second value, return a facet description which
    will be available as [`facet-description`][65d4]. As the third value, return a
    generalized boolean indicating whether this facet must be explicitly
    destroyed (in which case a finalizer will be added to `cube`).

<a id="x-28MGL-CUBE-3ADESTROY-FACET-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:DESTROY-FACET*%20GENERIC-FUNCTION"></a>

- [generic-function] **destroy-facet\*** *facet-name facet*

    Free the resources associated with `facet` with
    `facet-name`. The cube this facet belongs to is not among the
    parameters because this method can be called from a finalizer on the
    cube (so we can't have a reference to the cube portably) which also
    means that it may run in an unpredictable thread.

<a id="x-28MGL-CUBE-3ACOPY-FACET-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:COPY-FACET*%20GENERIC-FUNCTION"></a>

- [generic-function] **copy-facet\*** *cube from-facet-name from-facet to-facet-name to-facet*

    Copy the `cube`'s data from `from-facet` with
    `from-facet-name` to `to-facet` with `to-facet-name`. Called by
    [`with-facet`][0706] (or more directly [`watch-facet`][f791]) when necessary. `from-facet`
    is what [`select-copy-source-for-facet*`][7a49] returned.

<a id="x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:CALL-WITH-FACET*%20GENERIC-FUNCTION"></a>

- [generic-function] **call-with-facet\*** *cube facet-name direction fn*

    Call `fn` with an up-to-date [`facet-value`][deec] that belongs
    to `facet-name` of `cube`. [`with-facet`][0706] is directly implemented in terms
    of this function. See [The Default Implementation of `call-with-facet*`][d720] for the gory
    details.
    
    Specializations will most likely want to call the default
    implementation (with [`call-next-method`][6832]) but with a lambda that
    transforms `facet-value` before passing it on to `fn`.

<a id="x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:FACET-UP-TO-DATE-P*%20GENERIC-FUNCTION"></a>

- [generic-function] **facet-up-to-date-p\*** *cube facet-name facet*

    Check if `facet` with `facet-name` has been updated
    since the latest change to `cube` (that is, since the access to other
    facets with [`direction`][69de] of `:io` or `:output`). The default method simply
    calls [`facet-up-to-date-p`][a34e] on `facet`.
    
    One reason to specialize this is when some facets actually share
    common storage, so updating one make the other up-to-date as well.

<a id="x-28MGL-CUBE-3ASELECT-COPY-SOURCE-FOR-FACET-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:SELECT-COPY-SOURCE-FOR-FACET*%20GENERIC-FUNCTION"></a>

- [generic-function] **select-copy-source-for-facet\*** *cube to-name to-facet*

    Called when `to-facet` with `to-name` is about to be
    updated by copying data from an up-to-date facet. Return the
    facet (or its name) from which data shall be copied. Note that if
    the returned facet is not [`facet-up-to-date-p`][a34e]*, then it will be
    updated first and another SELECT-COPY-SOURCE-FOR-FACET* will take
    place, so be careful not to get into endless recursion. The default
    method simply returns the first up-to-date facet.

PAX integration follows, don't worry about it if you don't use PAX,
but you really should (see [PAX Manual][2415]).

<a id="x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-CUBE:FACET-NAME%20MGL-PAX:LOCATIVE"></a>

- [locative] **facet-name**

    The [`facet-name`][799d] locative is to refer to stuff defined with
    [`define-facet-name`][0e24].

<a id="x-28MGL-CUBE-3ADEFINE-FACET-NAME-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CUBE:DEFINE-FACET-NAME%20MGL-PAX:MACRO"></a>

- [macro] **define-facet-name** *symbol lambda-list &body docstring*

    Just a macro to document that `symbol` refers to a facet name (as in
    the [`facet-name`][799d]). This is totally confusing, so here is
    an example of how MGL-MAT (see [MAT Manual][f470]) documents the
    [`mgl-mat:backing-array`][1fda] facet:
    
    ```commonlisp
    (define-facet-name backing-array ()
      "The corresponding facet is a one dimensional lisp array.")
    ```
    
    Which makes it possible to refer to this definition (refer as in
    link and `m-.` to) [`mgl-mat:backing-array`][1fda] facet-name. See
    [PAX Manual][2415] for more.

Also see [The Default Implementation of `call-with-facet*`][d720].

<a id="x-28MGL-CUBE-3A-40CUBE-DEFAULT-CALL-WITH-FACET-2A-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-DEFAULT-CALL-WITH-FACET*%20MGL-PAX:SECTION"></a>

## 7 The Default Implementation of `call-with-facet*`

<a id="x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20-28METHOD-20-28MGL-CUBE-3ACUBE-20T-20T-20T-29-29-29"></a>
<a id="MGL-CUBE:CALL-WITH-FACET*%20%28METHOD%20%28MGL-CUBE:CUBE%20T%20T%20T%29%29"></a>

- [method] **call-with-facet\*** *(cube cube) facet-name direction fn*

    The default implementation of `call-with-facet*` is defined in terms
    of the [`watch-facet`][f791] and the [`unwatch-facet`][2ce8] generic functions. These
    can be considered part of the [Facet Extension API][1eda].

<a id="x-28MGL-CUBE-3AWATCH-FACET-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:WATCH-FACET%20GENERIC-FUNCTION"></a>

- [generic-function] **watch-facet** *cube facet-name direction*

    This is what the default [`call-with-facet*`][f1d8] method,
    in terms of which [`with-facet`][0706] is implemented, calls first. The
    default method takes care of creating facets, copying and tracking
    up-to-dateness.
    
    Calls [`check-no-writers`][ea8f] (unless [`*let-input-through-p*`][6fe2]) and
    [`check-no-watchers`][face] (unless [`*let-output-through-p*`][bf71]) depending on
    `direction` to detect situations with a writer being concurrent to
    readers/writers because that would screw up the tracking of
    up-to-dateness.
    
    The default implementation should suffice most of the time.
    MGL-MAT specializes it to override the `direction` arg, if
    it's `:output` but not all elements are visible due to reshaping, so
    that invisible elements are still copied over.

<a id="x-28MGL-CUBE-3AUNWATCH-FACET-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CUBE:UNWATCH-FACET%20GENERIC-FUNCTION"></a>

- [generic-function] **unwatch-facet** *cube facet-name*

    This is what the default [`call-with-facet*`][f1d8] method,
    in terms of which [`with-facet`][0706] is implemented, calls last. The default
    method takes care of taking down facets. External resource managers
    may want to hook into this to handle unused facets.

<a id="x-28MGL-CUBE-3A-2ALET-INPUT-THROUGH-P-2A-20VARIABLE-29"></a>
<a id="MGL-CUBE:*LET-INPUT-THROUGH-P*%20VARIABLE"></a>

- [variable] **\*let-input-through-p\*** *nil*

    If true, [`with-facets`][ddd4] (more precisely, the default implementation of
    [`call-with-facet*`][f1d8]) with `:direction` `:input` does not call
    [`check-no-writers`][ea8f]. This knob is intended to be bound locally for
    debugging purposes.

<a id="x-28MGL-CUBE-3A-2ALET-OUTPUT-THROUGH-P-2A-20VARIABLE-29"></a>
<a id="MGL-CUBE:*LET-OUTPUT-THROUGH-P*%20VARIABLE"></a>

- [variable] **\*let-output-through-p\*** *nil*

    If true, [`with-facets`][ddd4] (more precisely, the default implementation of
    [`call-with-facet*`][f1d8]) with `:direction` `:io` or `:output` does not call
    [`check-no-watchers`][face]. This knob is intended to be bound locally for
    debugging purposes.

<a id="x-28MGL-CUBE-3ACHECK-NO-WRITERS-20FUNCTION-29"></a>
<a id="MGL-CUBE:CHECK-NO-WRITERS%20FUNCTION"></a>

- [function] **check-no-writers** *cube facet-name message-format &rest message-args*

    Signal an error if `cube` has facets (with names other than
    `facet-name`) being written (i.e. direction is `:io` or `:output`).

<a id="x-28MGL-CUBE-3ACHECK-NO-WATCHERS-20FUNCTION-29"></a>
<a id="MGL-CUBE:CHECK-NO-WATCHERS%20FUNCTION"></a>

- [function] **check-no-watchers** *cube facet-name message-format &rest message-args*

    Signal an error if `cube` has facets (with names other than
    `facet-name`) being regardless of the direction.

<a id="x-28MGL-CUBE-3A-40CUBE-LIFETIME-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-LIFETIME%20MGL-PAX:SECTION"></a>

## 8 Lifetime

Lifetime management of facets is manual (but facets of garbage
cubes are freed automatically by a finalizer, see [`make-facet*`][6afc]). One
may destroy a single facet or all facets of a cube with
[`destroy-facet`][39a0] and [`destroy-cube`][dbac], respectively. Also see
[Facet Barriers][0fc8].

<a id="x-28MGL-CUBE-3ADESTROY-FACET-20FUNCTION-29"></a>
<a id="MGL-CUBE:DESTROY-FACET%20FUNCTION"></a>

- [function] **destroy-facet** *cube facet-name*

    Free resources associated with the facet with `facet-name` and remove
    it from [`facets`][b1f9] of `cube`.

<a id="x-28MGL-CUBE-3ADESTROY-CUBE-20FUNCTION-29"></a>
<a id="MGL-CUBE:DESTROY-CUBE%20FUNCTION"></a>

- [function] **destroy-cube** *cube*

    Destroy all facets of `cube` with [`destroy-facet`][39a0].

In some cases it is useful to declare the intent to use a facet in
the future to prevent its destruction. Hence, every facet has
reference count which starts from 0. The reference count is
incremented and decremented by [`add-facet-reference-by-name`][db31] and
[`remove-facet-reference-by-name`][b78f], respectively. If it is positive,
then the facet will not be destroyed by explicit [`destroy-facet`][39a0] and
[`destroy-cube`][dbac] calls, but it will still be destroyed by the finalizer
to prevent resource leaks caused by stray references.

<a id="x-28MGL-CUBE-3AADD-FACET-REFERENCE-BY-NAME-20FUNCTION-29"></a>
<a id="MGL-CUBE:ADD-FACET-REFERENCE-BY-NAME%20FUNCTION"></a>

- [function] **add-facet-reference-by-name** *cube facet-name*

    Make sure `facet-name` exists on `cube` and increment its reference
    count. Return the [`facet`][36d6] behind `facet-name`.

<a id="x-28MGL-CUBE-3AREMOVE-FACET-REFERENCE-BY-NAME-20FUNCTION-29"></a>
<a id="MGL-CUBE:REMOVE-FACET-REFERENCE-BY-NAME%20FUNCTION"></a>

- [function] **remove-facet-reference-by-name** *cube facet-name*

    Decrement the reference count of the facet with `facet-name` of `cube`.
    It is an error if the facet does not exists or if the reference
    count becomes negative.

<a id="x-28MGL-CUBE-3AREMOVE-FACET-REFERENCE-20FUNCTION-29"></a>
<a id="MGL-CUBE:REMOVE-FACET-REFERENCE%20FUNCTION"></a>

- [function] **remove-facet-reference** *facet*

    Decrement the reference count of `facet`. It is an error if the facet
    is already destroyed or if the reference count becomes negative.
    This function has the same purpose as
    [`remove-facet-reference-by-name`][b78f], but by having a single `facet`
    argument, it's more suited for use in finalizers because it does not
    keep the whole [`cube`][1659] alive.

<a id="x-28MGL-CUBE-3A-40CUBE-FACET-BARRIER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CUBE:@CUBE-FACET-BARRIER%20MGL-PAX:SECTION"></a>

### 8.1 Facet Barriers

A facility to control lifetime of facets tied to a dynamic extent.
Also see [Lifetime][a810].

<a id="x-28MGL-CUBE-3AWITH-FACET-BARRIER-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CUBE:WITH-FACET-BARRIER%20MGL-PAX:MACRO"></a>

- [macro] **with-facet-barrier** *(cube-type ensures destroys) &body body*

    When `body` exits, destroy facets which:
    
    - are of cubes with `cube-type`
    
    - have a facet name among `destroys`
    
    - were created in the dynamic extent of `body`
    
    Before destroying the facets, it is ensured that facets with names
    among `ensures` are up-to-date. `with-facet-barrier`s can be nested, in
    case of multiple barriers matching the cube's type and the created
    facet's name, the innermost one takes precedence.
    
    The purpose of this macro is twofold. First, it makes it easy to
    temporarily work with a certain facet of many cubes without leaving
    newly created facets around. Second, it can be used to make sure
    that facets whose extent is tied to some dynamic boundary (such as
    the thread in which they were created) are destroyed.

<a id="x-28MGL-CUBE-3ACOUNT-BARRED-FACETS-20FUNCTION-29"></a>
<a id="MGL-CUBE:COUNT-BARRED-FACETS%20FUNCTION"></a>

- [function] **count-barred-facets** *facet-name &key (type 'cube)*

    Count facets with `facet-name` of cubes of `type` which will be
    destroyed by a facet barrier.

  [0706]: #MGL-CUBE:WITH-FACET%20MGL-PAX:MACRO "MGL-CUBE:WITH-FACET MGL-PAX:MACRO"
  [0e24]: #MGL-CUBE:DEFINE-FACET-NAME%20MGL-PAX:MACRO "MGL-CUBE:DEFINE-FACET-NAME MGL-PAX:MACRO"
  [0ef1]: #MGL-CUBE:@CUBE-FACETS%20MGL-PAX:SECTION "Facets"
  [0fc8]: #MGL-CUBE:@CUBE-FACET-BARRIER%20MGL-PAX:SECTION "Facet Barriers"
  [1659]: #MGL-CUBE:CUBE%20CLASS "MGL-CUBE:CUBE CLASS"
  [1eda]: #MGL-CUBE:@CUBE-FACET-EXTENSION-API%20MGL-PAX:SECTION "Facet Extension API"
  [1fda]: mat-manual.md#MGL-MAT:BACKING-ARRAY%20MGL-CUBE:FACET-NAME "MGL-MAT:BACKING-ARRAY MGL-CUBE:FACET-NAME"
  [2415]: pax-manual.md "PAX Manual"
  [2ce8]: #MGL-CUBE:UNWATCH-FACET%20GENERIC-FUNCTION "MGL-CUBE:UNWATCH-FACET GENERIC-FUNCTION"
  [36d6]: #MGL-CUBE:FACET%20STRUCTURE "MGL-CUBE:FACET STRUCTURE"
  [3912]: #MGL-CUBE:@CUBE-LINKS%20MGL-PAX:SECTION "Links"
  [39a0]: #MGL-CUBE:DESTROY-FACET%20FUNCTION "MGL-CUBE:DESTROY-FACET FUNCTION"
  [43a2]: #MGL-CUBE:@CUBE-INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [5fd4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL (MGL-PAX:CLHS TYPE)"
  [65d4]: #MGL-CUBE:FACET-DESCRIPTION%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29 "MGL-CUBE:FACET-DESCRIPTION (MGL-PAX:STRUCTURE-ACCESSOR MGL-CUBE:FACET)"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [696b]: #MGL-CUBE:FACET-UP-TO-DATE-P*%20GENERIC-FUNCTION "MGL-CUBE:FACET-UP-TO-DATE-P* GENERIC-FUNCTION"
  [69de]: #MGL-CUBE:DIRECTION%20TYPE "MGL-CUBE:DIRECTION TYPE"
  [6afc]: #MGL-CUBE:MAKE-FACET*%20GENERIC-FUNCTION "MGL-CUBE:MAKE-FACET* GENERIC-FUNCTION"
  [6fe2]: #MGL-CUBE:*LET-INPUT-THROUGH-P*%20VARIABLE "MGL-CUBE:*LET-INPUT-THROUGH-P* VARIABLE"
  [799d]: #MGL-CUBE:FACET-NAME%20MGL-PAX:LOCATIVE "MGL-CUBE:FACET-NAME MGL-PAX:LOCATIVE"
  [7a49]: #MGL-CUBE:SELECT-COPY-SOURCE-FOR-FACET*%20GENERIC-FUNCTION "MGL-CUBE:SELECT-COPY-SOURCE-FOR-FACET* GENERIC-FUNCTION"
  [7bcf]: #MGL-CUBE:SYNCHRONIZATION%20%28MGL-PAX:ACCESSOR%20MGL-CUBE:CUBE%29 "MGL-CUBE:SYNCHRONIZATION (MGL-PAX:ACCESSOR MGL-CUBE:CUBE)"
  [8324]: #MGL-CUBE:*MAYBE-SYNCHRONIZE-CUBE*%20VARIABLE "MGL-CUBE:*MAYBE-SYNCHRONIZE-CUBE* VARIABLE"
  [8ce7]: #MGL-CUBE:FACET-NAME%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29 "MGL-CUBE:FACET-NAME (MGL-PAX:STRUCTURE-ACCESSOR MGL-CUBE:FACET)"
  [a34e]: #MGL-CUBE:FACET-UP-TO-DATE-P%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29 "MGL-CUBE:FACET-UP-TO-DATE-P (MGL-PAX:STRUCTURE-ACCESSOR MGL-CUBE:FACET)"
  [a5a6]: #MGL-CUBE:@CUBE-SYNCHRONIZATION%20MGL-PAX:SECTION "Synchronization"
  [a810]: #MGL-CUBE:@CUBE-LIFETIME%20MGL-PAX:SECTION "Lifetime"
  [b1f9]: #MGL-CUBE:FACETS%20FUNCTION "MGL-CUBE:FACETS FUNCTION"
  [b78f]: #MGL-CUBE:REMOVE-FACET-REFERENCE-BY-NAME%20FUNCTION "MGL-CUBE:REMOVE-FACET-REFERENCE-BY-NAME FUNCTION"
  [bf71]: #MGL-CUBE:*LET-OUTPUT-THROUGH-P*%20VARIABLE "MGL-CUBE:*LET-OUTPUT-THROUGH-P* VARIABLE"
  [d720]: #MGL-CUBE:@CUBE-DEFAULT-CALL-WITH-FACET*%20MGL-PAX:SECTION "The Default Implementation of `call-with-facet*`"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [db31]: #MGL-CUBE:ADD-FACET-REFERENCE-BY-NAME%20FUNCTION "MGL-CUBE:ADD-FACET-REFERENCE-BY-NAME FUNCTION"
  [dbac]: #MGL-CUBE:DESTROY-CUBE%20FUNCTION "MGL-CUBE:DESTROY-CUBE FUNCTION"
  [ddd4]: #MGL-CUBE:WITH-FACETS%20MGL-PAX:MACRO "MGL-CUBE:WITH-FACETS MGL-PAX:MACRO"
  [deec]: #MGL-CUBE:FACET-VALUE%20%28MGL-PAX:STRUCTURE-ACCESSOR%20MGL-CUBE:FACET%29 "MGL-CUBE:FACET-VALUE (MGL-PAX:STRUCTURE-ACCESSOR MGL-CUBE:FACET)"
  [e2c4]: #MGL-CUBE:*DEFAULT-SYNCHRONIZATION*%20VARIABLE "MGL-CUBE:*DEFAULT-SYNCHRONIZATION* VARIABLE"
  [ea8f]: #MGL-CUBE:CHECK-NO-WRITERS%20FUNCTION "MGL-CUBE:CHECK-NO-WRITERS FUNCTION"
  [f1d8]: #MGL-CUBE:CALL-WITH-FACET*%20GENERIC-FUNCTION "MGL-CUBE:CALL-WITH-FACET* GENERIC-FUNCTION"
  [f36f]: #MGL-CUBE:@CUBE-BASICS%20MGL-PAX:SECTION "Basics"
  [f470]: mat-manual.md "MAT Manual"
  [f791]: #MGL-CUBE:WATCH-FACET%20GENERIC-FUNCTION "MGL-CUBE:WATCH-FACET GENERIC-FUNCTION"
  [face]: #MGL-CUBE:CHECK-NO-WATCHERS%20FUNCTION "MGL-CUBE:CHECK-NO-WATCHERS FUNCTION"
