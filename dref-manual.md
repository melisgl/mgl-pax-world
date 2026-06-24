<a id="x-28DREF-3A-40DREF-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@DREF-MANUAL%20MGL-PAX:SECTION"></a>

# DRef Manual

## Table of Contents

- [1 Links and Systems][a584]
- [2 Introduction][ad80]
- [3 References][14ea]
    - [3.1 Dissecting References][6f15]
    - [3.2 References Glossary][852d]
- [4 `dtype`s][a459]
- [5 Listing Definitions][e1d4]
- [6 Basic Operations][662d]
- [7 Basic Locative Types][1d1d]
    - [7.1 Locatives for Variables][462c]
    - [7.2 Locatives for Macros][d45d]
    - [7.3 Locatives for Functions and Methods][1d59]
    - [7.4 Locatives for Types and Declarations][7a04]
    - [7.5 Locatives for the Condition System][408d]
    - [7.6 Locatives for Packages and Readtables][c339]
    - [7.7 Locatives for Unknown Definitions][58f1]
    - [7.8 Locatives for DRef Constructs][da93]
- [8 Backends][884d]
- [9 Extending DRef][68fb]
    - [9.1 Extension Tutorial][9d60]
    - [9.2 Locative Type Hierarchy][c9ab]
    - [9.3 Defining Locative Types][f494]
        - [9.3.1 Symbol Locatives][59c9]
    - [9.4 Extending `locate`][4c16]
        - [9.4.1 Initial Definition][87fc]
        - [9.4.2 Canonicalization][9383]
        - [9.4.3 Defining Lookups, Locators and Casts][adc7]
    - [9.5 Extending Everything Else][793d]
        - [9.5.1 Definition Properties][c9de]
    - [9.6 `dref-class`es][6354]
    - [9.7 Source Locations][a078]

###### \[in package DREF\]
<a id="x-28DREF-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION"></a>

## 1 Links and Systems

Here is the [official
repository](https://github.com/melisgl/dref/) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/dref-manual.html)
for the latest version.

<a id="x-28-22dref-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22dref%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"dref"**

    - _Version:_ 0.5.0
    - _Description:_ Reify definitions, provide portable access to
        arglists, docstrings and source locations in an extensible
        framework.
    - _Long Description:_ [`defun`][f472] defines a first-class object: a
        [`function`][119e]. [`defvar`][7334] does not. The DRef library provides a way
        to refer to all definitions and smooths over the differences between
        implementations. This system has minimal dependencies. It autoloads
        the [`dref/full`][0c7e] `asdf:system`.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <http://github.com/melisgl/dref/>
    - _Bug tracker:_ <https://github.com/melisgl/dref/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/dref.git)
    - *Depends on:* [autoload][5968], mgl-pax-bootstrap, [named-readtables][718a], pythonic-string-reader
    - *Auto depends on:* [dref/full][0c7e]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22dref-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22dref%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"dref/full"**

    - _Description:_ DRef with everything loaded. There should be no need
        to explicitly load this system (or depend on it) because it is
        autoloaded as necessary by all publicly accessible functionality in
        [`dref`][021a].
        
        However, to get the dependencies, install this system.
        
        For a discussion of conditional dependencies, see [Backends][884d].
    - *Depends on:* alexandria, closer-mop, [dref][021a], [mgl-pax][6fdb], sb-introspect(?), swank(?)
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@INTRODUCTION%20MGL-PAX:SECTION"></a>

## 2 Introduction

*What if definitions were first-class objects?*

Some [defining forms][23a8] do not create first-class
[objects][3c8a]. For example, [`defun`][f472] creates
[`function`][119e] objects, but [`defvar`][7334] does not create variable
objects, as no such thing exists in Common Lisp. The main purpose of
this library is to fill this gap with the introduction of
[`dref`][d930] objects:

```common-lisp
(defvar *my-var* nil
  "This is my var.")
(dref '*my-var* 'variable)
==> #<DREF *MY-VAR* VARIABLE>
```

`dref`s just package up a [name][5fc4] (`*my-var*`) and a
[locative][7ac8] ([`variable`][6c83]) then check that the definition
actually exists:

```common-lisp
(dref 'junk 'variable)
.. debugger invoked on LOCATE-ERROR:
..   Could not locate JUNK VARIABLE.
```

The [Basic Operations][662d] on definitions in DRef are [`arglist`][e6bd], [`docstring`][affc]
and [`source-location`][32da].

```common-lisp
(docstring (dref '*my-var* 'variable))
=> "This is my var."
=> NIL
```

For definitions associated with objects, the definition can be
[`locate`][8f19]d from the object:

```common-lisp
(locate #'print)
==> #<DREF PRINT FUNCTION>
```

These objects designate their definitions, so `(docstring #'print)`
works. Extending DRef and these operations is possible through
[Defining Locative Types][f494]. It is also possible to define
new operations. For example, [PAX][2415] makes
[`pax:document`][432c] extensible through [`pax:document-object*`][8269].

Finally, existing definitions can be queried with [`definitions`][e196] and
[`dref-apropos`][65b4]:

```common-lisp
(definitions 'dref-ext:arglist*)
==> (#<DREF ARGLIST* GENERIC-FUNCTION>
-->  #<DREF ARGLIST* (METHOD (CLASS-DREF))>
-->  #<DREF ARGLIST* (METHOD (COMPILER-MACRO-DREF))>
-->  #<DREF ARGLIST* (METHOD (FUNCTION-DREF))>
-->  #<DREF ARGLIST* (METHOD (LAMBDA-DREF))>
-->  #<DREF ARGLIST* (METHOD (MACRO-DREF))>
-->  #<DREF ARGLIST* (METHOD (METHOD-DREF))>
-->  #<DREF ARGLIST* (METHOD (SETF-DREF))>
-->  #<DREF ARGLIST* (METHOD (TYPE-DREF))> #<DREF ARGLIST* (METHOD (DREF))>
-->  #<DREF ARGLIST* (METHOD (MGL-PAX::GO-DREF))>
-->  #<DREF ARGLIST* (METHOD (T))>
-->  #<DREF ARGLIST* (UNKNOWN
-->                   (DECLAIM ARGLIST*
-->                            FTYPE))>)
```

```common-lisp
(dref-apropos 'locate-error :package :dref)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)

(dref-apropos "ate-err" :package :dref :external-only t)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)
```


<a id="x-28DREF-3A-40REFERENCES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@REFERENCES%20MGL-PAX:SECTION"></a>

## 3 References

After the [Introduction][ad80], here we get into the details. Of special
interest are:

- The [`xref`][cda7] function to construct an arbitrary [reference][43bd] without any
  checking of validity.

- [`locate`][8f19] and [`dref`][7e92] to look up the [definition][2143] of an
  object (e.g `#'print`) or a [reference][43bd] (e.g. `(xref 'print
  'function)`).

- [`resolve`][63b4] to find the first-class (non-[`xref`][1538]) object the
  definition refers to, if any.

The [Basic Operations][662d] ([`arglist`][e6bd], [`docstring`][affc], [`source-location`][32da]) know how to
deal with references (discussed in the [Extending DRef][68fb]).

<a id="x-28DREF-3AXREF-20CLASS-29"></a>
<a id="DREF:XREF%20CLASS"></a>

- [class] **xref**

    An `xref` (cross-reference) is a [reference][43bd]. It may
    represent some kind of [definition][2143] of its [name][5fc4] in the context given
    by its [locative][7ac8]. The definition may not exist and the locative may
    even be [invalid][7ac8]. The subclass [`dref`][d930] represents
    definitions that exist.

<a id="x-28DREF-3AXREF-20FUNCTION-29"></a>
<a id="DREF:XREF%20FUNCTION"></a>

- [function] **xref** *name locative*

    A shorthand for `(make-instance 'xref :name name :locative locative)`
    to create [`xref`][1538] objects. It does no error checking: the
    [`locative-type`][97ba] of `locative-type` need not be defined, and the
    [`locative-args`][2444] need not be valid. Use [`locate`][8f19] or the [`dref`][7e92] function to
    create [`dref`][d930] objects.

<a id="x-28DREF-3AXREF-3D-20FUNCTION-29"></a>
<a id="DREF:XREF%3D%20FUNCTION"></a>

- [function] **xref=** *xref1 xref2*

    See if `xref1` and `xref2` have the same [`xref-name`][5447] and [`xref-locative`][a70d]
    under [`equal`][3fb5]. Comparing like this makes most sense for
    [`dref`][d930]s. However, two [`xref`][1538]s different under `xref=`
    may denote the same [`dref`][d930]s.

<a id="x-28DREF-3ADREF-20CLASS-29"></a>
<a id="DREF:DREF%20CLASS"></a>

- [class] **dref** *[xref][1538]*

    `dref`s can be thought of as [definition][2143]s that
    actually exist, although changes in the system can invalidate
    them (for example, a `dref` to a function definition can be
    invalidated by [`fmakunbound`][609c]). `dref`s must be created with [`locate`][8f19] or
    the [`dref`][7e92] function.
    
    Two `dref`s created in the same [dynamic environment][62e7] denote the
    same thing if and only if they are [`xref=`][0617].

<a id="x-28DREF-3ALOCATE-20FUNCTION-29"></a>
<a id="DREF:LOCATE%20FUNCTION"></a>

- [function] **locate** *object &optional (errorp t)*

    Return a [`dref`][d930] representing the [definition][2143] of `object`.
    
    `object` must be a supported first-class object, a `dref`, or an
    [`xref`][1538]:
    
    ```common-lisp
    (locate #'print)
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (locate #'print))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (xref 'print 'function))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    When `object` is a `dref`, it is simply returned.
    
    Else, a `locate-error`([`0`][6334] [`1`][6932]) is signalled if `object` is an `xref` with an
    invalid [locative][7ac8], or if no corresponding definition is found. If
    `errorp` is `nil`, then `nil` is returned instead.
    
    ```common-lisp
    (locate (xref 'no-such-function 'function))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate NO-SUCH-FUNCTION FUNCTION.
    ..   NO-SUCH-FUNCTION does not name a function.
    ```
    
    ```common-lisp
    (locate (xref 'print '(function xxx)))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate PRINT #'XXX.
    ..   Bad arguments (XXX) for locative FUNCTION with lambda list NIL.
    ```
    
    ```common-lisp
    (locate "xxx")
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate "xxx".
    ```
    
    Use the [`xref`][cda7] function to construct an `xref` without error checking.
    
    See [Extending `locate`][4c16].

<a id="x-28DREF-3ADREF-20FUNCTION-29"></a>
<a id="DREF:DREF%20FUNCTION"></a>

- [function] **dref** *name locative &optional (errorp t)*

    Shorthand for `(locate (xref name locative) errorp)`.

<a id="x-28DREF-3ARESOLVE-20FUNCTION-29"></a>
<a id="DREF:RESOLVE%20FUNCTION"></a>

- [function] **resolve** *object &optional (errorp t)*

    If `object` is an [`xref`][1538], then return the first-class object
    associated with its definition if any. Return `object` if it's not an
    `xref`. Thus, the value returned is never an `xref`. The second return
    value is whether resolving succeeded.
    
    ```common-lisp
    (resolve (dref 'print 'function))
    ==> #<FUNCTION PRINT>
    => T
    ```
    
    ```common-lisp
    (resolve #'print)
    ==> #<FUNCTION PRINT>
    => T
    ```
    
    If `object` is an `xref`, and the definition for it cannot be [`locate`][8f19]d,
    then `locate-error`([`0`][6334] [`1`][6932]) is signalled.
    
    ```common-lisp
    (resolve (xref 'undefined 'variable))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate UNDEFINED VARIABLE.
    ```
    
    If there is a definition, but there is no first-class object
    corresponding to it, then `resolve-error`([`0`][58ba] [`1`][7825]) is signalled or `nil` is
    returned depending on `errorp`:
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable))
    .. debugger invoked on RESOLVE-ERROR:
    ..   Could not resolve *PRINT-LENGTH* VARIABLE.
    ```
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable) nil)
    => NIL
    => NIL
    ```
    
    `resolve` is a partial inverse of `locate`: if a [`dref`][d930] is
    `resolve`able, then `locate`ing the object it resolves to recovers the
    `dref` equivalent to the original ([`xref=`][0617] and of the same type but not
    [`eq`][5a82]).
    
    Can be extended via [`resolve*`][d3b3].

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29"></a>
<a id="DREF-EXT:LOCATE-ERROR%20CONDITION"></a>

- [condition] **locate-error** *[error][d162] condition-context-mixin*

    Signalled by [`locate`][8f19] when the definition cannot be
    found, and `errorp` is true.

<a id="x-28DREF-EXT-3ARESOLVE-ERROR-20CONDITION-29"></a>
<a id="DREF-EXT:RESOLVE-ERROR%20CONDITION"></a>

- [condition] **resolve-error** *[error][d162] condition-context-mixin*

    Signalled by [`resolve`][63b4] when the object defined cannot
    be returned, and `errorp` is true.

<a id="x-28DREF-3A-40DISSECTING-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@DISSECTING-REFERENCES%20MGL-PAX:SECTION"></a>

### 3.1 Dissecting References

<a id="x-28DREF-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>
<a id="DREF:XREF-NAME%20%28MGL-PAX:READER%20DREF:XREF%29"></a>

- [reader] **xref-name** *[xref][1538] (:name)*

    The [name][5fc4] of the reference.

<a id="x-28DREF-3AXREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>
<a id="DREF:XREF-LOCATIVE%20%28MGL-PAX:READER%20DREF:XREF%29"></a>

- [reader] **xref-locative** *[xref][1538] (:locative)*

    The [locative][7ac8] of the reference.
    
    The locative is normalized by replacing single-element lists with
     their only element:
    
    ```common-lisp
    (xref 'print 'function)
    ==> #<XREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (xref 'print '(function))
    ==> #<XREF PRINT FUNCTION>
    ```

<a id="x-28DREF-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>
<a id="DREF:DREF-NAME%20%28MGL-PAX:READER%20DREF:DREF%29"></a>

- [reader] **dref-name** *[dref][d930]*

    The same as [`xref-name`][5447], but only works on
    [`dref`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-3ADREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>
<a id="DREF:DREF-LOCATIVE%20%28MGL-PAX:READER%20DREF:DREF%29"></a>

- [reader] **dref-locative** *[dref][d930]*

    The same as [`xref-locative`][a70d], but only works on
    [`dref`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-3ADREF-ORIGIN-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>
<a id="DREF:DREF-ORIGIN%20%28MGL-PAX:READER%20DREF:DREF%29"></a>

- [reader] **dref-origin** *[dref][d930]*

    The object from which [`locate`][8f19] constructed this
    [`dref`][d930]. `dref-origin` may have [presentation][ade6] arguments, which
    are not included in [`locative-args`][2444] as is the case with the `initform`
    argument of the [`variable`][6c83] locative:
    
    ```common-lisp
    (dref '*standard-output* '(variable "see-below"))
    ==> #<DREF *STANDARD-OUTPUT* VARIABLE>
    ```
    
    ```common-lisp
    (dref-origin (dref '*standard-output* '(variable "see-below")))
    ==> #<XREF *STANDARD-OUTPUT* (VARIABLE "see-below")>
    ```
    
    The `initform` argument overrides the global binding of
    [`*standard-output*`][e7ee] when it's [`pax:document`][432c]ed:
    
    ```common-lisp
    (first-line
     (pax:document (dref '*standard-output* '(variable "see-below"))
                   :stream nil))
    => "- [variable] *STANDARD-OUTPUT* \"see-below\""
    ```

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATIVE-TYPE%20FUNCTION"></a>

- [function] **locative-type** *locative*

    Return [locative type][a11d] of the [locative][7ac8] `locative`. This is the first
    element of `locative` if it's a list. If it's a symbol, it's that
    symbol itself.

<a id="x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATIVE-ARGS%20FUNCTION"></a>

- [function] **locative-args** *locative*

    Return the [`rest`][fe9f] of [locative][7ac8] `locative` if it's a list. If it's a symbol,
    then return `nil`. See [locative][7ac8].

The following convenience functions are compositions of
{[`locative-type`][97ba], [`locative-args`][2444]} and {[`xref-locative`][a70d],
[`dref-locative`][cc04]}.

<a id="x-28DREF-3AXREF-LOCATIVE-TYPE-20FUNCTION-29"></a>
<a id="DREF:XREF-LOCATIVE-TYPE%20FUNCTION"></a>

- [function] **xref-locative-type** *xref*

<a id="x-28DREF-3AXREF-LOCATIVE-ARGS-20FUNCTION-29"></a>
<a id="DREF:XREF-LOCATIVE-ARGS%20FUNCTION"></a>

- [function] **xref-locative-args** *xref*

<a id="x-28DREF-3ADREF-LOCATIVE-TYPE-20FUNCTION-29"></a>
<a id="DREF:DREF-LOCATIVE-TYPE%20FUNCTION"></a>

- [function] **dref-locative-type** *dref*

<a id="x-28DREF-3ADREF-LOCATIVE-ARGS-20FUNCTION-29"></a>
<a id="DREF:DREF-LOCATIVE-ARGS%20FUNCTION"></a>

- [function] **dref-locative-args** *dref*

<a id="x-28DREF-3A-40REFERENCES-GLOSSARY-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@REFERENCES-GLOSSARY%20MGL-PAX:SECTION"></a>

### 3.2 References Glossary

<a id="x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@REFERENCE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **reference**

    A reference is a [name][5fc4] plus a [locative][7ac8], and it identifies a
    possible definition. References are of class [`xref`][1538]. When a reference
    is a [`dref`][d930], it may also be called a [definition][2143].

<a id="x-28DREF-3A-40DEFINITION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@DEFINITION%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **definition**

    A definition is a [reference][43bd] that identifies a concrete definition.
    Definitions are of class [`dref`][d930]. A definition [`resolve`][63b4]s to the
    first-class object associated with the definition if such a thing
    exists, and [`locate`][8f19] on this object returns the canonical `dref` object
    that's unique under [`xref=`][0617].
    
    The kind of a definition is given by its [locative type][a11d]. There is at
    most one definition for any given [name][5fc4] and locative type.
    Equivalently, there can be no two definitions of the same [`dref-name`][1e36]
    and [`dref-locative-type`][a22e] but different [`dref-locative-args`][0976].

<a id="x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@NAME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **name**

    Names are symbols, strings and nested lists of the previous, which
    name [functions][ba62], [types][926d],
    [packages][4dd7], etc. Together with [locative][7ac8]s, they
    form [reference][43bd]s.
    
    See [`xref-name`][5447] and [`dref-name`][1e36].

<a id="x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@LOCATIVE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **locative**

    Locatives specify a *type* of definition such as
    [`function`][ba62] or [`variable`][6c83]. Together with [name][5fc4]s,
    they form [reference][43bd]s.
    
    In their compound form, locatives may have arguments (see
    [`locative-args`][2444]) as in `(method (number))`. In fact, their atomic form
    is shorthand for the common no-argument case: that is, `function` is
    equivalent to `(function)`.
    
    A locative is valid if it names an existing [locative type][a11d] and its
    `locative-args` match that type's lambda-list (see
    [`define-locative-type`][b6c4]).
    
    ```common-lisp
    (arglist (dref 'method 'locative))
    => (&REST QUALIFIERS-AND-SPECIALIZERS)
    => :DESTRUCTURING
    ```
    
    See [`xref-locative`][a70d] and [`dref-locative`][cc04].

<a id="x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@LOCATIVE-TYPE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **locative type**

    The locative type is the part of a [locative][7ac8] that identifies what
    kind of definition is being referred to. This is always a symbol.
    
    Locative types are defined with [`define-locative-type`][b6c4] or
    [`define-pseudo-locative-type`][68b4]. See [Basic Locative Types][1d1d] for the list
    locative types built into DRef, and [PAX Locatives][292a] for
    those in PAX.
    
    Also, see [`locative-type`][97ba], [`xref-locative-type`][840e], [`dref-locative-type`][a22e],
    [Defining Locative Types][f494].

<a id="x-28DREF-3A-40PRESENTATION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@PRESENTATION%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **presentation**

    [reference][43bd]s may have arguments (see
    [Defining Locative Types][f494]) that do not affect the behaviour
    of [`locate`][8f19] and the [Basic Operations][662d], but which may be used for
    other, "presentation" purposes. For example, the [`variable`][6c83]
    locative's `initform` argument is used for presentation by
    [`pax:document`][432c]. Presentation arguments are available via
    [`dref:dref-origin`][e742] but do not feature in [`dref-locative`][cc04] to ensure the
    uniqueness of the definition under [`xref=`][0617].

<a id="x-28DREF-3A-40DTYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@DTYPES%20MGL-PAX:SECTION"></a>

## 4 `dtype`s

`dtype`s generalize [locative type][a11d]s the same way Lisp types
generalize [`class`][1f37]es. A `dtype` is either

- a [locative type][a11d] such as [`function`][ba62], [`type`][926d]
  and [`clhs`][ed5f], or

- a full [locative][7ac8] such as `(method (number))` and `(clhs section)`,
  or

- `nil` (the empty `dtype`) and `t` (that encompasses all
  [`lisp-locative-types`][30ad]), or

- named with [`define-dtype`][c635] (such as [`pseudo`][943a] and [`top`][3301]), or

- a combination of the above with [`and`][dd55], [`or`][e2d1] and
  [`not`][954a], or

- a [`member`][0376] form with [`locate`][8f19]able definitions, or

- a [`satisfies`][2b8b] form with the name of a function that takes a single
  [definition][2143] as its argument.

`dtype`s are used in [`definitions`][e196] and [`dref-apropos`][65b4] to filter the set of
definitions as in

```common-lisp
(definitions 'print :dtype '(not unknown))
==> (#<DREF PRINT (CLHS FUNCTION)> #<DREF PRINT FUNCTION>)
```

```common-lisp
(dref-apropos "type specifier" :dtype 'pseudo)
==> (#<DREF "1.4.4.6" #1=(CLHS SECTION)> #<DREF "1.4.4.6.1" #1#>
-->  #<DREF "1.4.4.6.2" #1#> #<DREF "1.4.4.6.3" #1#>
-->  #<DREF "1.4.4.6.4" #1#> #<DREF "4.2.3" #1#>
-->  #<DREF "atomic type specifier" #2=(CLHS GLOSSARY-TERM)>
-->  #<DREF "compound type specifier" #2#>
-->  #<DREF "derived type specifier" #2#> #<DREF "type specifier" #2#>)
```


<a id="x-28DREF-3ADEFINE-DTYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF:DEFINE-DTYPE%20MGL-PAX:MACRO"></a>

- [macro] **define-dtype** *name lambda-list &body body*

    Like [`deftype`][7f9a], but it may expand into other `dtype`s.
    
    The following example defines `method*` as the locative [`method`][172e]
    without its direct locative subtypes.
    
    ```common-lisp
    (define-dtype method* () '(and method (not reader) (not writer)))
    ```
    
    See [`dtypep`][963f] for the semantics and also the locative [`dtype`][85ba].

<a id="x-28DREF-3ATOP-20DREF-3ADTYPE-29"></a>
<a id="DREF:TOP%20DREF:DTYPE"></a>

- [dtype] **top**

    This is the top of the `dtype` hierarchy, much like `t` for Lisp types.
    It expands to ([`or`][e2d1] `t` [`pseudo`][943a]). While `t` matches every normal
    Lisp object and objectless definitions present in the running
    Lisp (see [`lisp-locative-types`][30ad]), `top` matches even pseudo
    definitions (see [`pseudo-locative-types`][c340]).

<a id="x-28DREF-3APSEUDO-20DREF-3ADTYPE-29"></a>
<a id="DREF:PSEUDO%20DREF:DTYPE"></a>

- [dtype] **pseudo**

    This is the union of all [`pseudo-locative-types`][c340]. It expands
    to `(or ,@(pseudo-locative-types))`.

<a id="x-28DREF-3ADTYPEP-20FUNCTION-29"></a>
<a id="DREF:DTYPEP%20FUNCTION"></a>

- [function] **dtypep** *dref dtype*

    See if `dref` is of `dtype`.
    
    - *[Atomic locatives][7ac8]:* If `dtype` is a [locative type][a11d],
      then it matches definitions with that locative type and its
      locative subtypes.
    
        Because [`constant`][c819] is defined with `variable` among its
        [ `locative-supertypes`][b6c4]:
    
        ```common-lisp
        (dtypep (dref 'pi 'constant) 'variable)
        => T
        ```
    
        ```common-lisp
        (dtypep (dref 'number 'class) 'type)
        => T
        ```
    
        It is an error if `dtype` is an [`atom`][0a7c] but not a [locative type][a11d]. On
        the other hand, (the empty) argument list of atomic locatives is
        not checked even if having no arguments makes them
        [invalid][7ac8].
    
    - *[Compound locatives][7ac8]:* Locatives in their compound
      form are validated and must match exactly (under [`equal`][3fb5], as in
      [`xref=`][0617]).
    
        ```common-lisp
        (defparameter *d* (dref 'dref* '(method (t t t))))
        (defparameter *d2* (dref 'dref* '(method :around (t t t))))
        (dtypep *d* 'method)
        => T
        (dtypep *d* '(accessor))
        .. debugger invoked on SIMPLE-ERROR:
        ..   Bad arguments NIL for locative ACCESSOR with lambda list (CLASS-NAME).
        (dtypep *d* '(method (t t t)))
        => T
        (dtypep *d2* '(method (t t t)))
        => NIL
        ```
    
    - `dtype` may be constructed with [`and`][dd55], [`or`][e2d1] and
      [`not`][954a] from Lisp types, locative types, full locatives and
      named `dtype`s:
    
        ```common-lisp
        (dtypep (dref 'locate-error 'condition) '(or condition class))
        => T
        (dtypep (dref nil 'type) '(and type (not class)))
        => T
        ```
    
    - For `(member &rest objs)`, each of `obj`s is [`locate`][8f19]d and `dref` is
      matched against them with `xref=`:
    
        ```common-lisp
        (dtypep (locate #'print) `(member ,#'print))
        => T
        ```
    
    - For `(satisfies pred)`, the predicate `pred` is funcalled with `dref`.
    
    - `dtype` may be named by [`define-dtype`][c635]:
    
        ```common-lisp
        (dtypep (locate #'car) 'top)
        => T
        ```

<a id="x-28DREF-3A-40LISTING-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@LISTING-DEFINITIONS%20MGL-PAX:SECTION"></a>

## 5 Listing Definitions

<a id="x-28DREF-3ADEFINITIONS-20FUNCTION-29"></a>
<a id="DREF:DEFINITIONS%20FUNCTION"></a>

- [function] **definitions** *name &key (dtype t) (sort t)*

    List all definitions of `name` that are of `dtype` as [`dref`s][d930].
    
    Just as `(dref name locative)` returns the canonical definition, the
    [`dref-name`][1e36]s of returned by `definitions` may be different from `name`:
    
    ```common-lisp
    (definitions "PAX")
    ==> (#<DREF "MGL-PAX" PACKAGE>)
    ```
    
    ```common-lisp
    (definitions 'mgl-pax)
    ==> (#<DREF "MGL-PAX" PACKAGE> #<DREF "mgl-pax" ASDF/SYSTEM:SYSTEM>)
    ```
    
    Similarly, [`dref-locative-type`][a22e] may be more made more specific:
    
    ```common-lisp
    (definitions 'dref:locate-error :dtype 'type)
    ==> (#<DREF LOCATE-ERROR CONDITION>)
    ```
    
    If `sort`, the returned list is ordered according to [`sort-references`][1db8].
    
    Can be extended via [`map-definitions-of-name`][97b4].

<a id="x-28DREF-3ADREF-APROPOS-20FUNCTION-29"></a>
<a id="DREF:DREF-APROPOS%20FUNCTION"></a>

- [function] **dref-apropos** *name &key package external-only case-sensitive (dtype t) (sort t)*

    Return a list of [`dref`][d930]s corresponding to existing
    definitions that match the various arguments. First, `(dref-apropos
    nil)` lists all definitions in the running Lisp and maybe more (e.g.
    [`mgl-pax:clhs`][ed5f]). Arguments specify how the list of
    definitions is filtered.
    
    `dref-apropos` itself is similar to [`cl:apropos-list`][7328], but
    
    - it finds [definition][2143]s not [`symbol`][e5af]s,
    
    - it supports an extensible definition types, and
    
    - filtering based on them.
    
    PAX has a live browsing [frontend][b7fc].
    
    Roughly speaking, when `name` or `package` is a `symbol`, it must match
    the whole [name][5fc4] of the definition:
    
    ```common-lisp
    (dref-apropos 'method :package :dref :external-only t)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
    ```
    
    On the other hand, when `name` or `package` is a [`string`][68cc], they are
    matched as substrings to the definition's name [`princ-to-string`][a541]ed:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t)
    ==> (#<DREF SETF-METHOD LOCATIVE> #<DREF METHOD CLASS>
    -->  #<DREF METHOD LOCATIVE> #<DREF METHOD-COMBINATION CLASS>
    -->  #<DREF METHOD-COMBINATION LOCATIVE>)
    ```
    
    Definitions that are not of `dtype` (see [`dtypep`][963f]) are filtered out:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t :dtype 'class)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD-COMBINATION CLASS>)
    ```
    
    When `package` is `:none`, only non-symbol [name][5fc4]s are matched:
    
    ```
    (dref-apropos "dref" :package :none)
    ==> (#<DREF "DREF" PACKAGE> #<DREF "DREF-EXT" PACKAGE>
    -->  #<DREF "DREF-TEST" PACKAGE> #<DREF "dref" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/full" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test-autoload" ASDF/SYSTEM:SYSTEM>)
    ```
    
    The exact rules of filtering are as follows. Let `c` be the [name][5fc4] of
    the candidate definition from the list of all definitions that we
    are matching against the arguments and denote its string
    representation `(princ-to-string c)` with `p`. Note that
    `princ-to-string` does not print the package of symbols. We say that
    two strings *match* if `case-sensitive` is `nil` and they are [`equalp`][2ff3], or
    `case-sensitive` is true and they are [`equal`][3fb5]. `case-sensitive` affects
    *substring* comparisons too.
    
    - If `name` is a `symbol`, then its [`symbol-name`][0d07] must *match* `p`.
    
    - If `name` is a `string`, then it must be a *substring* of `p`.
    
    - If `package` is `:any`, then `c` must be a `symbol`.
    
    - If `package` is `:none`, then `c` must *not* be a `symbol`.
    
    - If `package` is not `nil`, `:any` or `:none`, then `c` must be a symbol.
    
    - If `package` is a [`package`][1d5a], it must be [`eq`][5a82] to the
      [`symbol-package`][e5ab] of `c`.
    
    - If `package` is a `symbol` other than `nil`, `:any` and `:none`, then its
      `symbol-name` must *match* the [`package-name`][db68] or one of the
      [`package-nicknames`][4b93] of `symbol-package` of `c`.
    
    - If `package` is a `string`, then it must be a *substring* of the
      `package-name` of `symbol-package` of `c`.
    
    - If `external-only` and `c` is a symbol, then `c` must be external in
      a matching package.
    
    - `dtype` matches candidate definition `d` if `(dtypep d dtype)`.
    
    If `sort`, the returned list is ordered according to [`sort-references`][1db8].
    
    Can be extended via MAP-REFERENCES-OF-TYPE and
    [`map-definitions-of-name`][97b4].

<a id="x-28DREF-3AWITH-DEFINITIONS-CACHED-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF:WITH-DEFINITIONS-CACHED%20MGL-PAX:MACRO"></a>

- [macro] **with-definitions-cached** *(&key (dtype ''top)) &body body*

    Allow caching of definition lookups and [`dtype`][a459] computations.
    This can provide a speedup when multiple calls are made to
    [`definitions`][e196] or [`dref-apropos`][65b4] within `body`.
    
    Using this macro makes the promise that, during the [dynamic
    extent][36e9] of `body`, the following are unchanged:
    
    - the set of [definition][2143]s of `dtype`,
    
    - the set of all [locative type][a11d]s,
    
    - the set of all [`dtype`s][a459].
    
    Note that definitions *not* of `dtype` are allowed to be made or
    deleted.

<a id="x-28DREF-EXT-3ASORT-REFERENCES-20FUNCTION-29"></a>
<a id="DREF-EXT:SORT-REFERENCES%20FUNCTION"></a>

- [function] **sort-references** *references &key key*

    Order `references` in an implementation independent way.
    
    - Non-symbol named references go first and are
    [sorted by the locative type][cf03] first, then by name and
    locative args.
    
    - Symbol-based references go after and are
    sorted by name first, then [by the
    locative-type][cf03], finally
    by the locative args.

<a id="x-28DREF-EXT-3ASORT-LOCATIVE-TYPES-20FUNCTION-29"></a>
<a id="DREF-EXT:SORT-LOCATIVE-TYPES%20FUNCTION"></a>

- [function] **sort-locative-types** *locative-types*

    Sort `locative-types` in an implementation independent way.
    The order is alphabetical in [`symbol-name`][0d07], with the [`package-name`][db68]
    acting as a tie breaker. If `unknown` is present among `locative-types`,
    it goes last.

<a id="x-28DREF-3A-40REVERSE-DEFINITION-ORDER-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="DREF:@REVERSE-DEFINITION-ORDER%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **reverse definition order**

    The lists of [locative type][a11d]s returned by e.g. [`locative-types`][99b0],
    [`lisp-locative-types`][30ad], [`pseudo-locative-types`][c340] and [`locative-aliases`][94d1] are
    in reverse order of the time of their definition. This order is not
    affected by redefinition, regardless of whether it's by
    [`define-locative-type`][b6c4], [`define-pseudo-locative-type`][68b4],
    [`define-symbol-locative-type`][ee9b] or [`define-locative-alias`][548e].

<a id="x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29"></a>
<a id="DREF:LOCATIVE-TYPES%20FUNCTION"></a>

- [function] **locative-types**

    Return a list of non-[alias][94d1] locative types.
    This is the [`union`][d6c7] of [`lisp-locative-types`][30ad] and [`pseudo-locative-types`][c340],
    which is the set of constituents of the `dtype` [`top`][3301].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29"></a>
<a id="DREF:LISP-LOCATIVE-TYPES%20FUNCTION"></a>

- [function] **lisp-locative-types**

    Return the locative types that correspond to Lisp definitions,
    which typically have [`source-location`][32da]. These are defined with
    [`define-locative-type`][b6c4] and [`define-symbol-locative-type`][ee9b] and are the
    constituents of `dtype` `t`.
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29"></a>
<a id="DREF:PSEUDO-LOCATIVE-TYPES%20FUNCTION"></a>

- [function] **pseudo-locative-types**

    Return the locative types that correspond to non-Lisp definitions.
    These are the ones defined with [`define-pseudo-locative-type`][68b4] and are
    the constituents of `dtype` [`pseudo`][943a].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3ALOCATIVE-ALIASES-20FUNCTION-29"></a>
<a id="DREF:LOCATIVE-ALIASES%20FUNCTION"></a>

- [function] **locative-aliases**

    Return the list of locatives aliases, defined with [`define-locative-alias`][548e].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3A-40BASIC-OPERATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@BASIC-OPERATIONS%20MGL-PAX:SECTION"></a>

## 6 Basic Operations

The following functions take a single argument, which may be a
[`dref`][d930], or an object denoting its own definition (see
[`locate`][8f19]).

<a id="x-28DREF-3AARGLIST-20FUNCTION-29"></a>
<a id="DREF:ARGLIST%20FUNCTION"></a>

- [function] **arglist** *object*

    Return the arglist of the definition of `object` or `nil` if the
    arglist cannot be determined.
    
    The second return value indicates whether the arglist has been
    found. As the second return value, `:ordinary` indicates an [ordinary
    lambda list][059c], `:macro` a [macro lambda list][cc32], `:deftype` a
    [deftype lambda list][817d], and `:destructuring` a [destructuring
    lambda list][6067]. Other non-`nil` values are also allowed.
    
    ```common-lisp
    (arglist #'arglist)
    => (OBJECT)
    => :ORDINARY
    ```
    
    ```common-lisp
    (arglist (dref 'define-locative-type 'macro))
    => (LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL
        DOCSTRING DREF-DEFCLASS-FORM)
    => :MACRO
    ```
    
    ```common-lisp
    (arglist (dref 'arglist* '(method (method-dref))))
    => ((DREF METHOD-DREF))
    => :SPECIALIZED
    ```
    
    ```common-lisp
    (arglist (dref 'method 'locative))
    => (&REST QUALIFIERS-AND-SPECIALIZERS)
    => :DESTRUCTURING
    ```
    
    This function supports [`macro`s][f3cc],
    [`compiler-macro`s][41fd], [`setf`][d83a] functions,
    [`function`s][ba62], [`generic-function`s][5875],
    [`method`s][172e], [`type`s][926d], [`locative`s][0b3a]. Note
    that `arglist` depends on the quality of `swank-backend:arglist`. With
    the exception of SBCL, which has perfect support, all Lisp
    implementations have minor omissions:
    
    - [`deftype`][7f9a] lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL;
    
    - default values in `macro` lambda lists on AllegroCL;
    
    - various edge cases involving traced functions.
    
    Can be extended via [`arglist*`][0a96].

<a id="x-28DREF-3AARGLIST-PARAMETERS-20FUNCTION-29"></a>
<a id="DREF:ARGLIST-PARAMETERS%20FUNCTION"></a>

- [function] **arglist-parameters** *arglist &optional (arglist-type :ordinary)*

    A utility to extract the names of the parameters from `arglist` of
    `arglist-type`. See the function [`arglist`][e6bd], for the possible values of
    `arglist-type`.
    
    Note that if `arglist` is `nil`, then `arglist-type` may also be `nil`, in
    which case `nil` is returned.
    
    ```common-lisp
    (arglist #'source-location)
    => (OBJECT &KEY ERROR)
    => :ORDINARY
    ```
    
    ```common-lisp
    (multiple-value-call 'arglist-parameters (arglist #'source-location))
    => (OBJECT ERROR)
    ```
    
    Can be extended via [`arglist-parameters*`][d9ad].

<a id="x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29"></a>
<a id="MGL-PAX:DOCSTRING%20FUNCTION"></a>

- [function] **docstring** *object*

    Return the docstring from the definition of `object`.
    As the second value, return the [`*package*`][5ed1] that was in effect when
    the docstring was installed or `nil` if it cannot be determined (this
    is used by [`pax:document`][432c] when determinining the
    [Package and Readtable][ab7e] of docstrings). This function is similar
    in purpose to [`cl:documentation`][68f1].
    
    Note that some locative types such as [`asdf:system`s][c097] and
    [`declaration`s][6e04] have no docstrings, and some Lisp
    implementations do not record all docstrings. The following are
    known to be missing:
    
    - [`compiler-macro`][41fd] docstrings on ABCL, AllegroCL, CCL, ECL;
    
    - [`method-combination`][82e0] docstrings on ABCL, AllegroCL.
    
    Can be extended via [`docstring*`][9fd4].

<a id="x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29"></a>
<a id="DREF:SOURCE-LOCATION%20FUNCTION"></a>

- [function] **source-location** *object &key error*

    Return the Swank source location for the [defining form][23a8]
    of `object`.
    
    The returned Swank location object is to be accessed only through
    the [Source Locations][a078] API or to be passed to e.g Slime's
    `slime-goto-source-location`.
    
    If no source location was found,
    
    - if `error` is `nil`, then return `nil`;
    
    - if `error` is `:error`, then return a list of the form `(:error
      <error-message>)` suitable for `slime-goto-source-location`;
    
    - if `error` is `t`, then signal an [`error`][d162] condition with the same error
      message as in the previous case.
    
    Note that the availability of source location information varies
    greatly across Lisp implementations.
    
    With the `nil` [`backend`][1048], `dref:source-location` loses
    precision beyond toplevel forms.
    
    Can be extended via [`source-location*`][444d].

<a id="x-28DREF-3A-40BASIC-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@BASIC-LOCATIVE-TYPES%20MGL-PAX:SECTION"></a>

## 7 Basic Locative Types

The following are the [locative type][a11d]s supported out of the box.
Like all locative types, they are named by symbols. When there is a
CL type corresponding to the reference's locative type, the
references can be [`resolve`][63b4]d to a unique object as is the case in

```common-lisp
(resolve (dref 'print 'function))
==> #<FUNCTION PRINT>
=> T
```

Even if there is no such CL type, the [`arglist`][e6bd], the [`docstring`][affc], and
the [`source-location`][32da] of the defining form is usually recorded unless
otherwise noted.

The basic locative types and their inheritance structure is loosely
based on the `doc-type` argument of [`cl:documentation`][c5ae].

<a id="x-28DREF-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@VARIABLELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.1 Locatives for Variables

<a id="x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="VARIABLE%20MGL-PAX:LOCATIVE"></a>

- [locative] **variable** *&optional initform*

    - Direct locative subtypes: [concept][56b9], [glossary-term][5119], [section][672f], [constant][c819]

    Refers to a global special variable.
    `initform`, or if not specified, the global value of the variable is
    to be used for [presentation][ade6].
    
    ```common-lisp
    (dref '*print-length* 'variable)
    ==> #<DREF *PRINT-LENGTH* VARIABLE>
    ```
    
    `variable` references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:CONSTANT%20MGL-PAX:LOCATIVE"></a>

- [locative] **constant** *&optional initform*

    - Direct locative supertypes: [variable][6c83]

    Refers to a constant variable defined with [`defconstant`][8934]. `initform`,
    or if not specified, the value of the constant is included in the
    documentation. The [`constant`][c819] locative is like the [`variable`][6c83] locative,
    but it also checks that its object is [`constantp`][a26f].
    
    `constant` references do not [`resolve`][63b4].

<a id="x-28DREF-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@MACROLIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.2 Locatives for Macros

<a id="x-28SETF-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="SETF%20MGL-PAX:LOCATIVE"></a>

- [locative] **setf**

    - Direct locative subtypes: [setf-method][1a03], [setf-function][19f6]

    Refers to a [setf expander][35a2] (see [`defsetf`][66dc] and [`define-setf-expander`][d2cb]).
    
    [Setf functions][99b05] (e.g. `(defun (setf name) ...)` or the same
    with [`defgeneric`][c7f7]) are handled by the [`setf-function`][19f6],
    [`setf-generic-function`][ab5e], and `setf-method` locatives.
    
    `setf` expander references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **macro**

    Refers to a global macro, typically defined with [`defmacro`][14cb], or to a
    [special operator][9a71].
    
    `macro` references resolve to the [`macro-function`][e924] of their `name` or
    signal `resolve-error`([`0`][58ba] [`1`][7825]) if that's `nil`.

<a id="x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:SYMBOL-MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **symbol-macro**

    Refers to a global symbol macro, defined with [`define-symbol-macro`][46c0].
    Note that since `define-symbol-macro` does not support docstrings, PAX
    defines methods on the [`documentation`][68f1] generic function specialized on
    `(doc-type (eql 'symbol-macro))`.
    
    ```
    (define-symbol-macro my-mac 42)
    (setf (documentation 'my-mac 'symbol-macro)
          "This is MY-MAC.")
    (documentation 'my-mac 'symbol-macro)
    => "This is MY-MAC."
    ```
    
    `symbol-macro` references do not [`resolve`][63b4].

<a id="x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="COMPILER-MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **compiler-macro**

    - Direct locative subtypes: [setf-compiler-macro][5df4]

    Refers to a [`compiler-macro-function`][c575], typically defined with
    [`define-compiler-macro`][23d5].

<a id="x-28DREF-3ASETF-COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DREF:SETF-COMPILER-MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **setf-compiler-macro**

    - Direct locative supertypes: [compiler-macro][41fd]

    Refers to a compiler macro with a [setf function name][867c].
    
    `setf-compiler-macro` references do not [`resolve`][63b4].

<a id="x-28DREF-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@FUNCTIONLIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.3 Locatives for Functions and Methods

<a id="x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **function**

    - Direct locative subtypes: [structure-accessor][090c], [setf-function][19f6], [generic-function][5875]

    Refers to a global function, typically defined with [`defun`][f472]. The
    [name][5fc4] must be a [function name][5191]. It is also allowed to
    reference [`generic-function`][efe2]s as `function`s:
    
    ```common-lisp
    (dref 'docstring 'function)
    ==> #<DREF DOCSTRING FUNCTION>
    ```

<a id="x-28DREF-3ASETF-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DREF:SETF-FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **setf-function**

    - Direct locative supertypes: [function][ba62], [setf][d83a]
    
    - Direct locative subtypes: [structure-accessor][090c], [setf-generic-function][ab5e]

    Refers to a global [`function`][a51f] with a [setf function name][867c].
    
    ```common-lisp
    (defun (setf ooh) ())
    (locate #'(setf ooh))
    ==> #<DREF OOH SETF-FUNCTION>
    (dref 'ooh 'setf-function)
    ==> #<DREF OOH SETF-FUNCTION>
    (dref '(setf ooh) 'function)
    ==> #<DREF OOH SETF-FUNCTION>
    ```

<a id="x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="GENERIC-FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **generic-function**

    - Direct locative supertypes: [function][ba62]
    
    - Direct locative subtypes: [setf-generic-function][ab5e]

    Refers to a [`generic-function`][efe2], typically defined with
    [`defgeneric`][c7f7]. The [name][5fc4] must be a [function name][5191].

<a id="x-28DREF-3ASETF-GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DREF:SETF-GENERIC-FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **setf-generic-function**

    - Direct locative supertypes: [generic-function][5875], [setf-function][19f6]

    Refers to a global [`generic-function`][efe2] with a [setf function name][867c].
    
    ```common-lisp
    (defgeneric (setf oog) ())
    (locate #'(setf oog))
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    (dref 'oog 'setf-function)
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    (dref '(setf oog) 'function)
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    ```

<a id="x-28METHOD-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="METHOD%20MGL-PAX:LOCATIVE"></a>

- [locative] **method** *&rest qualifiers-and-specializers*

    - Direct locative subtypes: [writer][e548], [reader][cc04f], [setf-method][1a03]

    Refers to a `method`. [name][5fc4] must be a [function name][5191].
    `method-qualifiers-and-specializers` has the form
    
        (<QUALIFIER>* <SPECIALIZERS>)
    
    For example, the method
    
    ```common-lisp
    (defgeneric foo-gf (x y z)
      (:method :around (x (y (eql 'xxx)) (z string))
        (values x y z)))
    ```
    
    can be referred to as
    
    ```common-lisp
    (dref 'foo-gf '(method :around (t (eql xxx) string)))
    ==> #<DREF FOO-GF (METHOD :AROUND (T (EQL XXX) STRING))>
    ```
    
    `method` is not [`exportable-locative-type-p`][c930].

<a id="x-28DREF-3ASETF-METHOD-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DREF:SETF-METHOD%20MGL-PAX:LOCATIVE"></a>

- [locative] **setf-method** *&rest method-qualifiers-and-specializers*

    - Direct locative supertypes: [method][172e], [setf][d83a]
    
    - Direct locative subtypes: [accessor][00d4]

    Refers to a [`method`][51c3] of a `setf-generic-function`.
    
    ```common-lisp
    (defgeneric (setf oog) (v)
      (:method ((v string))))
    (locate (find-method #'(setf oog) () (list (find-class 'string))))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    (dref 'oog '(setf-method (string)))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    (dref '(setf oog) '(method (string)))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    ```

<a id="x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="METHOD-COMBINATION%20MGL-PAX:LOCATIVE"></a>

- [locative] **method-combination**

    Refers to a [`method-combination`][9b70], defined with
    [`define-method-combination`][006c].
    
    `method-combination` references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:READER%20MGL-PAX:LOCATIVE"></a>

- [locative] **reader** *class-name*

    - Direct locative supertypes: [method][172e]
    
    - Direct locative subtypes: [accessor][00d4]

    Refers to a `:reader` method in a [`defclass`][ead6]:
    
    ```common-lisp
    (defclass foo ()
      ((xxx :reader foo-xxx)))
    
    (dref 'foo-xxx '(reader foo))
    ==> #<DREF FOO-XXX (READER FOO)>
    ```

<a id="x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:WRITER%20MGL-PAX:LOCATIVE"></a>

- [locative] **writer** *class-name*

    - Direct locative supertypes: [method][172e]
    
    - Direct locative subtypes: [accessor][00d4]

    Like [`accessor`][00d4], but refers to a `:writer` method in a [`defclass`][ead6].

<a id="x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:ACCESSOR%20MGL-PAX:LOCATIVE"></a>

- [locative] **accessor** *class-name*

    - Direct locative supertypes: [reader][cc04f], [writer][e548], [setf-method][1a03]

    Refers to an `:accessor` in a [`defclass`][ead6].
    
    An `:accessor` in `defclass` creates a reader and a writer method.
    Somewhat arbitrarily, `accessor` references [`resolve`][63b4] to the writer
    method but can be [`locate`][8f19]d with either.

<a id="x-28DREF-3AACCESSOR-SLOT-DEFINITION-20FUNCTION-29"></a>
<a id="DREF:ACCESSOR-SLOT-DEFINITION%20FUNCTION"></a>

- [function] **accessor-slot-definition** *dref*

    Return the SLOT-DEFINITION object corresponding to `dref`, which may
    denote a `reader`, a `writer` or an `accessor`.

<a id="x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:STRUCTURE-ACCESSOR%20MGL-PAX:LOCATIVE"></a>

- [locative] **structure-accessor** *&optional structure-class-name*

    - Direct locative supertypes: [setf-function][19f6], [function][ba62]

    Refers to an accessor function generated by [`defstruct`][eac1] or [`defstruct*`][1281].
    A [`locate-error`][6334] condition is signalled if the wrong
    `structure-class-name` is provided.
    
    Note that there is no portable way to detect structure accessors,
    and on some platforms, `(locate #'my-accessor)`, [`definitions`][e196] and
    [`dref-apropos`][65b4] will return [`function`][a51f] references instead. On such
    platforms, `structure-accessor` references do not [`resolve`][63b4].

<a id="x-28DREF-3ADEFSTRUCT-2A-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF:DEFSTRUCT*%20MGL-PAX:MACRO"></a>

- [macro] **defstruct\*** *name-and-options &rest slot-descriptions*

    Like [`defstruct`][eac1], but support `:documentation` among slot options.
    The documentation is attached to the slot's `structure-accessor`.
    Example:
    
        (defstruct* my-struct
          (my-slot nil :documentation "docstring"))
    
    In addition to the normal `defstruct` processing, the above also does
    the moral equivalent of
    
        (setf (documentation 'my-struct-my-slot 'function) "docstring")

<a id="x-28DREF-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@TYPELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.4 Locatives for Types and Declarations

<a id="x-28TYPE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="TYPE%20MGL-PAX:LOCATIVE"></a>

- [locative] **type**

    - Direct locative subtypes: [class][2060]

    This locative can refer to [types and classes][0ff7] as well as
    [conditions][e237], simply put, to things defined by [`deftype`][7f9a],
    [`defclass`][ead6] and [`define-condition`][f7e4].
    
    ```common-lisp
    (deftype my-type () t)
    (dref 'my-type 'type)
    ==> #<DREF MY-TYPE TYPE>
    ```
    
    ```common-lisp
    (dref 'xref 'type)
    ==> #<DREF XREF CLASS>
    ```
    
    ```common-lisp
    (dref 'locate-error 'type)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```
    
    `type` references do not [`resolve`][63b4].

<a id="x-28CLASS-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="CLASS%20MGL-PAX:LOCATIVE"></a>

- [locative] **class**

    - Direct locative supertypes: [type][926d]
    
    - Direct locative subtypes: [condition][c479], [structure][da65]

    Naturally, `class` is the locative type for [`class`][1f37]es.
    
    [`arglist`][e6bd] returns the [compound type specifier][515e] syntax
    associated with [system class][ffd0]es. E.g. for the [`integer`][9b12]
    class:
    
    ```common-lisp
    (arglist (find-class 'integer))
    => (&OPTIONAL LOWER-LIMIT UPPER-LIMIT)
    => :DEFTYPE
    ```

<a id="x-28STRUCTURE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="STRUCTURE%20MGL-PAX:LOCATIVE"></a>

- [locative] **structure**

    - Direct locative supertypes: [class][2060]

    Refers to a [`structure-class`][e608], typically defined with [`defstruct`][eac1].
    
    Also, see [`defstruct*`][1281].

<a id="x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DECLARATION%20MGL-PAX:LOCATIVE"></a>

- [locative] **declaration**

    Refers to a declaration, used in [`declare`][1574], [`declaim`][ebea] and [`proclaim`][d3e1].
    
    User code may also define new declarations with CLTL2 functionality,
    but their [`arglist`][e6bd] is always `nil`. On implementations that support it,
    [`docstring`][affc] returns `(documentation name 'declaration)`.
    
    ```
    (cl-environments:define-declaration my-decl (&rest things)
      (values :declare (cons 'foo things)))
    ```
    
    `declaration` references do not [`resolve`][63b4].
    
    Also, [`source-location`][32da] on declarations currently only works on SBCL.

<a id="x-28DREF-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@CONDITION-SYSTEM-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.5 Locatives for the Condition System

<a id="x-28CONDITION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="CONDITION%20MGL-PAX:LOCATIVE"></a>

- [locative] **condition**

    - Direct locative supertypes: [class][2060]

    Although `condition` is not [`subtypep`][daac] of [`class`][1f37], actual condition
    objects are commonly instances of a condition class that is a CLOS
    class. HyperSpec [ISSUE:CLOS-CONDITIONS][023a] and
    [ISSUE:CLOS-CONDITIONS-AGAIN][da2e] provide the relevant history.
    
    Whenever a `class` denotes a `condition`, its [`dref-locative-type`][a22e] will be
    `condition`:
    
    ```common-lisp
    (dref 'locate-error 'class)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```

<a id="x-28RESTART-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="RESTART%20MGL-PAX:LOCATIVE"></a>

- [locative] **restart**

    A locative to refer to the definition of a restart defined by
    [`define-restart`][bb23].

<a id="x-28DREF-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF:DEFINE-RESTART%20MGL-PAX:MACRO"></a>

- [macro] **define-restart** *symbol lambda-list &body docstring*

    Associate a definition with the name of a restart, which must be a symbol.
    `lambda-list` should be what calls like `(invoke-restart '<symbol>
    ...)` must conform to, but this is not enforced.
    
    PAX "defines" standard CL restarts such as [`use-value`][86cc] with
    `define-restart`:
    
    ```common-lisp
    (dref 'use-value 'restart)
    ==> #<DREF USE-VALUE RESTART>
    (assert (docstring *))
    (assert (source-location **))
    ```
    
    Note that while there is a [`cl:restart`][38e4] class, its *instances* have no
    docstring or source location.

<a id="x-28DREF-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@PACKAGELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.6 Locatives for Packages and Readtables

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE"></a>

- [locative] **asdf/system:system**

    Refers to an already loaded `asdf:system` (those in `asdf:registered-systems`).
    The [name][5fc4] may be anything `asdf:find-system` supports.
    
    `asdf:system` is not [`exportable-locative-type-p`][c930].

<a id="x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="PACKAGE%20MGL-PAX:LOCATIVE"></a>

- [locative] **package**

    Refers to a [`package`][1d5a], defined by [`defpackage`][9b43] or [`make-package`][6e6e].
    The [name][5fc4] may be anything [`find-package`][4dc9] supports.
    
    `package` is not [`exportable-locative-type-p`][c930].

<a id="x-28READTABLE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="READTABLE%20MGL-PAX:LOCATIVE"></a>

- [locative] **readtable**

    Refers to a named [`readtable`][d646] defined with
    [`named-readtables:defreadtable`][6a02], which associates a global name and a
    docstring with the readtable object. The [name][5fc4] may be anything
    [`find-readtable`][5d41] supports.
    
    `readtable` references [`resolve`][63b4] to `find-readtable` on their [name][5fc4].

<a id="x-28DREF-3A-40UNKNOWN-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@UNKNOWN-DEFINITIONS%20MGL-PAX:SECTION"></a>

### 7.7 Locatives for Unknown Definitions

<a id="x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:UNKNOWN%20MGL-PAX:LOCATIVE"></a>

- [locative] **unknown** *dspec*

    This locative type allows PAX to work in a limited way with
    definition types it doesn't know. `unknown` definitions come from
    [`definitions`][e196], which uses `swank/backend:find-definitions`. The
    following examples show PAX stuffing the Swank
    dspec `(:define-alien-type double-float)` into an [`unknown`][a951] locative
    on SBCL.
    
    ```common-lisp
    (definitions 'double-float)
    ==> (#<DREF DOUBLE-FLOAT CLASS>
    -->  #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>)
    ```
    
    ```common-lisp
    (dref 'double-float '(unknown (:define-alien-type double-float)))
    ==> #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>
    ```
    
    [`arglist`][e6bd] and [`docstring`][affc] return `nil` for `unknown`s, but [`source-location`][32da]
    works.

<a id="x-28DREF-3A-40DREF-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@DREF-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.8 Locatives for DRef Constructs

<a id="x-28DREF-3ADTYPE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DREF:DTYPE%20MGL-PAX:LOCATIVE"></a>

- [locative] **dtype**

    - Direct locative subtypes: [locative][0b3a]

    Locative for [`dtype`s][a459] defined with [`define-dtype`][c635] and `locative` types.
    `dtype` is to `locative` as [`type`][7c9f] is to [`class`][1f37].
    
    The [`top`][3301] of the `dtype` hierarchy:
    
    ```common-lisp
    (dref 'top 'dtype)
    ==> #<DREF TOP DTYPE>
    ```
    
    This very definition:
    
    ```common-lisp
    (dref 'dtype 'locative)
    ==> #<DREF DTYPE LOCATIVE>
    ```

<a id="x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:LOCATIVE%20MGL-PAX:LOCATIVE"></a>

- [locative] **locative**

    - Direct locative supertypes: [dtype][85ba]

    This is the locative for [locative type][a11d]s defined with
    [`define-locative-type`][b6c4], [`define-pseudo-locative-type`][68b4] and
    [`define-locative-alias`][548e].
    
    ```
    (first-line (source-location-snippet
                 (source-location (dref 'macro 'locative))))
    => "(define-locative-type macro ()"
    ```

<a id="x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="LAMBDA%20MGL-PAX:LOCATIVE"></a>

- [locative] **lambda** *&key arglist arglist-type docstring docstring-package file file-position snippet &allow-other-keys*

    A [pseudo locative type][c340] that carries its
    [`arglist`][e6bd], [`docstring`][affc] and [`source-location`][32da] in the locative itself. See
    [`make-source-location`][3bdc] for the description of `file`, [`file-position`][6f95], and
    `snippet`. `lambda` references do not [`resolve`][63b4]. The [name][5fc4] must be `nil`.
    
    ```common-lisp
    (arglist (dref nil '(lambda :arglist ((x y) z)
                                :arglist-type :macro)))
    => ((X Y) Z)
    => :MACRO
    ```
    
    ```common-lisp
    (docstring (dref nil '(lambda :docstring "xxx"
                                  :docstring-package :dref)))
    => "xxx"
    ==> #<PACKAGE "DREF">
    ```
    
    ```common-lisp
    (source-location-file
     (source-location (dref nil '(lambda :file "xxx.el"))))
    => "xxx.el"
    ```
    
    Also, see the [`pax:include`][5cd7] locative.

<a id="x-28DREF-3A-40BACKENDS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF:@BACKENDS%20MGL-PAX:SECTION"></a>

## 8 Backends

On SBCL, the `swank` `asdf:system` is not a dependency of
[`dref/full`][0c7e] because DRef is able to function without
it. However, when Swank is available in the Lisp image, DRef can use
it to provide more precise [`source-location`][32da]s.

On other Lisps, DRef curently gets [`arglist`][e6bd]s and `source-location`s via
Swank and `swank` is an ASDF dependency of `dref/full`.

<a id="x-28DREF-3ABACKEND-AVAILABLE-P-20FUNCTION-29"></a>
<a id="DREF:BACKEND-AVAILABLE-P%20FUNCTION"></a>

- [function] **backend-available-p** *backend*

    Check if `backend` is currently available in the running Lisp.
    `backend` may be `:swank` or `nil`. Currently, on SBCL, both are
    available; on other Lisps, only `:swank` is.

<a id="x-28DREF-3ABACKEND-20FUNCTION-29"></a>
<a id="DREF:BACKEND%20FUNCTION"></a>

- [function] **backend**

    Return the backend being used. This is either `:swank` or `nil`. The
    default backend is `:swank` if it is available when first loading
    [`dref`][021a] into the image, else it is `nil`.
    
    Note that on Lisps other than SBCL, this is currently always `:swank`,
    which is available due to the ASDF dependency of
    [`dref/full`][0c7e] on the `swank` `asdf:system`.

<a id="x-28DREF-3ABACKEND-20SETF-29"></a>
<a id="DREF:BACKEND%20SETF"></a>

- [setf] **backend** *backend*

    [`setf`][a138]ing [`backend`][1048] to a `backend` that is not
    [`backend-available-p`][f5a0] is an error.

<a id="x-28DREF-3AWITH-BACKEND-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF:WITH-BACKEND%20MGL-PAX:MACRO"></a>

- [macro] **with-backend** *(backend) &body body*

    Like [`setf`][a138] [`backend`][14a4], but only change `backend` during the dynamic
    extent of `body`.

<a id="x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@EXTENDING-DREF%20MGL-PAX:SECTION"></a>

## 9 Extending DRef

<a id="x-28DREF-EXT-3A-40EXTENSION-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@EXTENSION-TUTORIAL%20MGL-PAX:SECTION"></a>

### 9.1 Extension Tutorial

Let's see how to tell DRef about new kinds of definitions through
the example of the implementation of the [`class`][2060] locative. Note that
this is a verbatim [`pax:include`][5cd7] of the sources. Please
ignore any internal machinery. The first step is to define the
[locative type][a11d]:

```
(define-locative-type class (type)
  "Naturally, CLASS is the locative type for [CLASS][class]es.

  ARGLIST returns the [compound type specifier][clhs] syntax
  associated with [system class][clhs]es. E.g. for the [INTEGER][clhs]
  class:

  ```cl-transcript (:dynenv dref-std-env)
  (arglist (find-class 'integer))
  => (&OPTIONAL LOWER-LIMIT UPPER-LIMIT)
  => :DEFTYPE
  ```")

```

Then, we make it possible to look up [`class`][1f37] definitions:

```
(define-locator class ((class class))
  (make-instance 'class-dref :name (class-name class) :locative 'class))

(define-lookup class (symbol locative-args)
  (unless (and (symbolp symbol)
               (find-class symbol nil))
    (locate-error "~S does not name a class." symbol))
  (make-instance 'class-dref :name symbol :locative 'class))

```

[`define-locator`][16b6] makes `(locate (find-class 'dref))` work, while
[`define-lookup`][49b5] is for `(dref 'dref 'class)`. Naturally, for locative
types that do not define first-class objects, the first method
cannot be defined.

Finally, we define a [`resolve*`][d3b3] method to recover the [`class`][1f37]
object from a [`class-dref`][b3a7]. We also specialize [`docstring*`][9fd4] and
[`source-location*`][444d]:

```
(defmethod resolve* ((dref class-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((class class))
  (documentation* class t))

(defmethod source-location* ((dref class-dref))
  #+sbcl
  (sb-one-source-location (dref-name dref) :class)
  #-sbcl
  (swank-source-location* (resolve dref) (dref-name dref) 'class))

(defmethod arglist* ((dref class-dref))
  (clhs-type-specifier-arglist (dref-name dref)))

```

We took advantage of having just made the class locative type being
[`resolve`][63b4]able, by specializing [`docstring*`][9fd4] on the [`class`][1f37] class.
[`source-location*`][444d] was specialized on [`class-dref`][b3a7] to demonstrate how
this can be done for non-`resolve`able locative types.

Classes have no arglist, so no [`arglist*`][0a96] method is needed. In the
following, we describe the pieces in detail.

<a id="x-28DREF-EXT-3A-40LOCATIVE-TYPE-HIERARCHY-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@LOCATIVE-TYPE-HIERARCHY%20MGL-PAX:SECTION"></a>

### 9.2 Locative Type Hierarchy

[Locative types][a11d] form their own hierarchy, that
is only superficially similar to the Lisp [`class`][1f37] hierarchy.
The hierarchies of [`lisp-locative-types`][30ad] and [`pseudo-locative-types`][c340]
are distinct. That is, the [`dref-class`][25be] of a Lisp locative type must
not be a subclass of a [`pseudo`][943a] one, and vice versa. This is enforced
by [`define-locative-type`][b6c4] and [`define-pseudo-locative-type`][68b4].

<a id="x-28DREF-EXT-3ADREF-CLASS-20FUNCTION-29"></a>
<a id="DREF-EXT:DREF-CLASS%20FUNCTION"></a>

- [function] **dref-class** *locative-type*

    Return the name of the [`class`][1f37] used to represent [definition][2143]s with
    `locative-type`. This is always a subclass of [`dref`][d930]. Returns
    `nil` if `locative-type` is not a valid locative type.
    
    Note that the actual [`type-of`][9caa] a `dref` is mostly intended for
    [Extending DRef][68fb]. Hence, it is hidden when a `dref` is printed:
    
    ```common-lisp
    (dref 'print 'function)
    ==> #<DREF PRINT FUNCTION>
    (type-of *)
    => FUNCTION-DREF
    ```
    
    Due to [Canonicalization][9383], the actual type may be a proper subtype of
    `dref-class`:
    
    ```common-lisp
    (dref 'documentation 'function)
    ==> #<DREF DOCUMENTATION GENERIC-FUNCTION>
    (type-of *)
    => GENERIC-FUNCTION-DREF
    (subtypep 'generic-function-dref 'function-dref)
    => T
    => T
    ```

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUPERS-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS%20FUNCTION"></a>

- [function] **locative-type-direct-supers** *locative-type*

    List the [locative type][a11d]s whose [`dref-class`][25be]es are direct superclasses
    of the `dref-class` of `locative-type`. These can be considered
    supertypes of `locative-type` in the sense of [`dtypep`][963f].
    
    This is ordered as in the corresponding definition.

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUBS-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS%20FUNCTION"></a>

- [function] **locative-type-direct-subs** *locative-type*

    List the [locative type][a11d]s whose [`dref-class`][25be]es are direct subclasses
    of the `dref-class` of `locative-type`. These can be considered subtypes
    of `locative-type` in the sense of [`dtypep`][963f].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-EXT-3ALOCATIVE-SUBTYPE-P-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATIVE-SUBTYPE-P%20FUNCTION"></a>

- [function] **locative-subtype-p** *locative-type-1 locative-type-2*

    Check if `locative-type-1` is in the transitive closure of
    `locative-type-2` via [`locative-type-direct-subs`][130a]. It is an error if
    `locative-type-1` or `locative-type-2` is not a valid locative type.

<a id="x-28DREF-EXT-3A-40DEFINING-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@DEFINING-LOCATIVE-TYPES%20MGL-PAX:SECTION"></a>

### 9.3 Defining Locative Types

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **define-locative-type** *locative-type-and-lambda-list locative-supertypes &optional docstring dref-defclass-form*

    Declare `locative-type` as a [`locative`][0b3a],
    which is the first step in [Extending DRef][68fb].
    
    - *Simple example*
    
        To define a locative type called `dummy` that takes no arguments
        and is not a locative subtype of any other locative type:
    
        ```
        (define-locative-type dummy ()
          "Dummy docstring.")
        ```
    
        With this definition, only the locatives `dummy` and its
        equivalent form `(dummy)` are valid. The above defines a `dref`([`0`][d930] [`1`][7e92])
        subclass called `dummy-dref` in the current package. All
        definitions with locative type `dummy` and its locatives
        subtypes must be instances of `dummy-dref`.
    
        `(locate 'dummy 'locative)` refers to this definition. That is,
        [`arglist`][e6bd], [`docstring`][affc] and [`source-location`][32da] all work on
        it.
    
    - *Complex example*
    
        `dummy` may have arguments `x` and `y` and inherit from locative
        types `l1` and `l2`:
    
        ```
        (define-locative-type (dummy x &key y) (l1 l2)
          "Dummy docstring."
          (defclass dummy-dref ()
            ((xxx :initform nil :accessor dummy-xxx))))
        ```
    
        One may change name of `dummy-dref`, specify superclasses and
        add slots as with [`defclass`][ead6]. Behind the scenes, the `dref` classes
        of `l1` and `l2` are added automatically to the list of
        superclasses.
    
    Arguments:
    
    - The general form of `locative-type-and-lambda-list`
      is (`locative-type` [`&rest`][4336] `lambda-list`), where `locative-type` is a
      [`symbol`][e5af], and `lambda-list` is a [destructuring lambda list][6067].
      The [`locative-args`][2444] of [`dref`][d930]s with [locative type][a11d]
      `locative-type` (the argument given to this macro) always conform to
      this lambda list. See [`check-locative-args`][10a7].
    
        If `locative-type-and-lambda-list` is a single symbol, then that's
        interpreted as `locative-type`, and `lambda-list` is `nil`.
    
    - `locative-supertypes` is a list of [locative type][a11d]s whose
      [`dref-class`][25be]es are prepended to the list of superclasses this
      definition.
    
    Locative types defined with `define-locative-type` can be listed with
    [`lisp-locative-types`][30ad].

<a id="x-28DREF-EXT-3ADEFINE-PSEUDO-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-PSEUDO-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **define-pseudo-locative-type** *locative-type-and-lambda-list locative-supertypes &optional docstring dref-defclass-form*

    Like [`define-locative-type`][b6c4], but declare that
    `locative-type` does not correspond to definitions in the
    running Lisp. Definitions with pseudo locatives are of `dtype` [`pseudo`][943a]
    and are not listed by default by [`definitions`][e196].
    
    Locative types defined with `define-pseudo-locative-type` can be
    listed with [`pseudo-locative-types`][c340].

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-LOCATIVE-ALIAS%20MGL-PAX:MACRO"></a>

- [macro] **define-locative-alias** *alias locative-type &body docstring*

    Define `alias` that can be substituted for `locative-type` (both
    [`symbol`][e5af]s) for the purposes of [`locate`][8f19]ing. `locative-type` must
    exist (i.e. be among [`locative-types`][99b0]). For example, let's define
    `object` as an alias of the [`class`][2060] locative:
    
    ```
    (define-locative-alias object class)
    ```
    
    Then, `locate`ing with `object` will find the `class`:
    
    ```
    (dref 'xref 'object)
    ==> #<DREF XREF CLASS>
    ```
    
    The [`locative-args`][2444] of `object` (none in the above) are passed on to
    `class`.
    
    ```
    (arglist (dref 'object 'locative))
    => (&REST ARGS)
    => :DESTRUCTURING
    ```
    
    Note that [`locative-aliases`][94d1] are not `locative-types` and are not valid
    `dtype`s.
    
    Also, see [Locative Aliases][0fa3] in PAX.

<a id="x-28DREF-EXT-3A-40SYMBOL-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@SYMBOL-LOCATIVES%20MGL-PAX:SECTION"></a>

#### 9.3.1 Symbol Locatives

Let's see how the opaque [`define-symbol-locative-type`][ee9b] and the
obscure [`define-definer-for-symbol-locative-type`][3b96] macros work together
to simplify the common task of associating definition with a symbol
in a certain context.

<a id="x-28DREF-EXT-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **define-symbol-locative-type** *locative-type-and-lambda-list locative-supertypes &optional docstring dref-class-def*

    Similar to [`define-locative-type`][b6c4], but it assumes that all things
    [`locate`][8f19]able with [`locative-type`][97ba] are going to be symbols defined with a
    definer defined with [`define-definer-for-symbol-locative-type`][3b96]. Symbol
    locatives are for attaching a definition (along with arglist,
    documentation and source location) to a symbol in a particular
    context. An example will make everything clear:
    
    ```
    (define-symbol-locative-type direction ()
      "A direction is a symbol.")
    
    (define-definer-for-symbol-locative-type define-direction direction
      "With DEFINE-DIRECTION, one can document what a symbol means when
      interpreted as a DIRECTION.")
    
    (define-direction up ()
      "UP is equivalent to a coordinate delta of (0, -1).")
    ```
    
    After all this, `(dref 'up 'direction)` refers to the
    `define-direction` form above.
    
    The [`dref-class`][25be] of the defined locative type inherits from
    [`symbol-locative-dref`][34b9], which may be used for specializing when
    implementing new operations.

<a id="x-28DREF-EXT-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **define-definer-for-symbol-locative-type** *name locative-type &body docstring*

    Define a macro with `name` that can be used to attach a lambda list,
    documentation, and source location to a symbol in the context of
    `locative-type`. The defined macro's arglist is `(symbol lambda-list
    &optional docstring)`. `locative-type` is assumed to have been defined
    with [`define-symbol-locative-type`][ee9b].

<a id="x-28DREF-EXT-3A-40EXTENDING-LOCATE-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@EXTENDING-LOCATE%20MGL-PAX:SECTION"></a>

### 9.4 Extending `locate`

Internally, [`locate`][8f19] finds an initial [`dref`][d930] of its `object`
argument with a [lookup][49b5] or with a
[locator][16b6]. This initial `dref` is then canonicalized
with a series of [casts][2066]. In more detail, the process
is as follows.

- If the `object` argument of `locate` is a `dref`, then it is returned
  without processing.

Else, `locate` first needs to find the initial definition.

<a id="x-28DREF-EXT-3A-40INITIAL-DEFINITION-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@INITIAL-DEFINITION%20MGL-PAX:SECTION"></a>

#### 9.4.1 Initial Definition

[`locate`][8f19] can find the initial definition in one of two ways:

- *With direct
lookup*

    If `object` is an `xref`([`0`][1538] [`1`][cda7]), then the [lookup][49b5]
    for ([`xref-locative-type`][840e] `object`) is invoked. For an `xref` with the
    locative `(method (number))`, this would be the lookup
    defined as

    ```
    (define-lookup method (name locative-args) ...)
    ```

- *With locator
search*

    Else, `object` is a normal Lisp object, such as a [`method`][51c3]
    object from [`find-method`][6d46]. The first of [`lisp-locative-types`][30ad] whose
    [locator][16b6] succeeds provides the initial
    definition, which may be defined like this:

    ```
    (define-locator method ((obj method)) ...)
    ```

    This is a locator that returns definitions with the [`method`][172e]
    locative type and takes an argument named `obj` of class
    [`method`][51c3] (which is like a specializer in [`defmethod`][6832]).

    - `lisp-locative-types` are tried one by one in the order
      specified there.

    - For a given locative type, if there are multiple locators,
      standard CLOS method selection applies.


<a id="x-28DREF-EXT-3A-40CANONICALIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@CANONICALIZATION%20MGL-PAX:SECTION"></a>

#### 9.4.2 Canonicalization

The initial definition thus found is then canonicalized so that
there is a unique [definition][2143] under [`xref=`][0617]:

```
(locate #'arglist*)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
(dref 'arglist* 'function)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
(dref 'arglist* 'generic-function)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
```

Canonicalization is performed by recursively attempting to
[downcast][2066] the current definition to one of its
[`locative-type-direct-subs`][130a] in a depth-first manner, backtracking if a
cast fails.

<a id="x-28DREF-EXT-3A-40DEFAULT-DOWNCAST-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@DEFAULT-DOWNCAST%20MGL-PAX:SECTION"></a>

##### Default Downcast

By default, downcasting to [direct locative subtypes][130a] is performed by looking up the
definition where the locative type is replaced with its sub while
the name and the locative args remain the same.

<a id="x-28DREF-EXT-3A-40CAST-NAME-CHANGE-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@CAST-NAME-CHANGE%20MGL-PAX:SECTION"></a>

##### Cast Name Change

[Casts][2066] must be careful about changing [`dref-name`][1e36].

Their `dref` argument and the [`dref`][d930] returned must have the
same `dref-name` (under [`equal`][3fb5], see [`xref=`][0617]) or it must be possible to
upcast the returned value to the `dref` argument's `dref-locative-type`.

- *Implementation note*

    The purpose of this rule is to allow [`dtypep`][963f] answer this correctly:

    ```common-lisp
    (defclass foo ()
      ((a :accessor foo-a)))
    (dref '(setf foo-a) '(method (t foo)))
    ==> #<DREF FOO-A (ACCESSOR FOO)>
    (dtypep * '(method (t foo)))
    => T
    ;; Internally, DTYPEP upcast #<DREF FOO-A (ACCESSOR FOO)>
    ;; and checks that the locative args of the resulting
    ;; definition match those in (METHOD (T FOO)).
    (locate* ** 'method)
    ==> #<DREF (SETF FOO-A) (METHOD (T FOO))>
    ```

    For even more background, also note that if the name remains the
    same but locative args change, then `dtypep` can simply check with
    [`dref`][7e92] if there is a definition of the name with the
    given locative:

    ```common-lisp
    (defclass foo ()
      ((r :reader foo-r)))
    (dref 'foo-r '(reader foo))
    ==> #<DREF FOO-R (READER FOO)>
    (dtypep * '(method (foo)))
    => T
    ;; Behind the scenes, DTYPEP does this:
    (xref= ** (dref 'foo-r '(method (foo))))
    => T
    ```


<a id="x-28DREF-EXT-3A-40DEFINING-LOOKUPS-LOCATORS-AND-CASTS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@DEFINING-LOOKUPS-LOCATORS-AND-CASTS%20MGL-PAX:SECTION"></a>

#### 9.4.3 Defining Lookups, Locators and Casts

As we have seen, the [Initial Definition][87fc] is provided either by a
lookup or a locator, then [Canonicalization][9383] works with
casts. Here, we look at how to define these.

*Implementation note:* All three are currently implemented as
methods of generic functions with [`eql` specializers][29a1] for
the locative type, which may easily prove to be problematic down the
road. To make future changes easier, the generic function and the
methods are hidden behind e.g. the [`define-lookup`][49b5] and [`call-lookup`][2ab8]
macros.

<a id="x-28DREF-EXT-3A-2ACHECK-LOCATE-2A-20VARIABLE-29"></a>
<a id="DREF-EXT:*CHECK-LOCATE*%20VARIABLE"></a>

- [variable] **\*check-locate\*** *nil*

    Enable runtime verification of invariants during [`locate`][8f19] calls.
    This carries a performance penalty and is intended for testing and
    debugging.
    
    In particular, enforce the rule of [Cast Name Change][c68e] and that [lookups][49b5], [locators][16b6] and
    [casts][2066] obey the following:
    
    - The value returned must be either `nil` or a `dref`([`0`][d930] [`1`][7e92]). Alternatively,
      `locate-error`([`0`][6334] [`1`][6932]) may be signalled.
    
    - If a `dref` is returned, then its [`dref-locative-type`][a22e] must be
      [`locative-type`][97ba], and its class must be the [`dref-class`][25be] of
      `locative-type`.
    
    - [`locative-args`][2444] must be congruent with the destructuring lambda list
      in the definition of `locative-type`.

<a id="x-28DREF-EXT-3ADEFINE-LOOKUP-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-LOOKUP%20MGL-PAX:MACRO"></a>

- [macro] **define-lookup** *locative-type (name locative-args) &body body*

    Define a method of looking up [definition][2143]s of `locative-type`
    with the given `locative-args`. Lookups are invoked by [`locate`][8f19] when its
    `object` argument is an `xref`([`0`][1538] [`1`][cda7]) with `locative-type` but it is not a `dref`([`0`][d930] [`1`][7e92]),
    as in the case of `(dref 'print 'function)`. When called, the
    variables `name` and `locative-args` are bound to [`xref-name`][5447] and
    [`xref-locative-args`][1490] of the `xref`. `locative-args` is validated with
    [`check-locative-args`][10a7] before `body` is evaluated.
    
    ```
    (define-lookup variable (name locative-args)
      (unless (special-variable-name-p name)
        (locate-error))
      (make-instance 'variable-dref :name name :locative 'variable))
    ```
    
    - `locative-type` is a valid [locative type][a11d].
    
    - `name` and `locative-args` are both [`symbol`][e5af]s.
    
    The above are enforced at macro-expansion time.
    
    - `body` must follow the rules in [`*check-locate*`][b038].

<a id="x-28DREF-EXT-3ACALL-LOOKUP-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:CALL-LOOKUP%20MGL-PAX:MACRO"></a>

- [macro] **call-lookup** *name locative-type locative-args*

    Call the [lookup][49b5] for `locative-type` with `name`
    and `locative-args`.

<a id="x-28DREF-EXT-3ADEFINE-LOCATOR-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-LOCATOR%20MGL-PAX:MACRO"></a>

- [macro] **define-locator** *locative-type ((object class)) &body body*

    Define a method of finding the [definition][2143] with `locative-type` of
    instances of `class`. When a locator's `body` is evaluated, `object` is
    bound to such an instance.
    
    ```
    (define-locator class ((class class))
      (make-instance 'class-dref :name (class-name class) :locative 'class))
    ```
    
    - `locative-type` is one of [`lisp-locative-types`][30ad]. This is because
      [`pseudo-locative-types`][c340] never [`resolve`][63b4] to first-class objects.
    
    - `object` is a [`symbol`][e5af].
    
    - `class` names a [`class`][1f37] that is not a subtype of
      [`xref`][1538]. For how to convert definitions from one locative
      type to another, see [`define-cast`][2066].
    
    The above are enforced at macro-expansion time.
    
    - `body` must follow the rules in [`*check-locate*`][b038].
    
    In contrast to when the [Initial Definition][87fc] is created from an
    `xref` (see [`define-lookup`][49b5]), here [`locative-args`][2444] are determined from
    `object`.

<a id="x-28DREF-EXT-3ACALL-LOCATOR-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:CALL-LOCATOR%20MGL-PAX:MACRO"></a>

- [macro] **call-locator** *object locative-type*

    Call the [locator][16b6] for `locative-type` with `object`.

<a id="x-28DREF-EXT-3ADEFINE-CAST-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:DEFINE-CAST%20MGL-PAX:MACRO"></a>

- [macro] **define-cast** *locative-type ((dref dref-class)) &body body*

    Define a method of converting a [definition][2143] to another
    with `locative-type`. When a cast's `body` is evaluated, `dref` is bound
    to an instance `dref-class`, which denotes a valid but potentially
    [non-canonical][9383] definition.
    
    Note that the [Default Downcast][8529] often suffices, and defining a cast
    is only necessary if the [name][c68e] or the locative
    args change:
    
    ```
    (define-cast accessor ((dref reader-dref))
      (let ((name (dref-name dref))
            (class (second (dref-locative dref))))
        (when (ignore-errors (find-accessor-slot-definition name class))
          (make-instance 'accessor-dref :name name
                          :locative `(accessor ,class)))))
    ```
    
    - `locative-type` is a valid [locative type][a11d].
    
    - If `locative-type` is one of [`pseudo-locative-types`][c340], then `dref-class`
      must be of another pseudo locative type.
    
    - `dref-class` is either a direct *downcast* or a potentially
      non-direct *upcast*.
    
        - *Downcast:* In this case, `locative-type` is one of
          [`locative-type-direct-subs`][130a] of (`dref-class-to-locative-type`
          `dref-class`).
    
            Downcasting to non-direct subtypes is done in multiple
            steps. Consequently, the `body` of a downcast can rely on
            ([`class-of`][6a98] `dref`) being [`class`][1f37], not any subclass thereof.
    
        - *Upcast:* `locative-type` is different but reachable
          from (`dref-class-to-locative-type` `dref-class`) by repeatedly
          choosing one of [`locative-type-direct-supers`][80a8]. Upcasting to
          non-direct supertypes is done in one step.
    
    The above are enforced at macro-expansion time.
    
    - `body` must follow the rules in [`*check-locate*`][b038], including those in
      [Cast Name Change][c68e].

<a id="x-28DREF-EXT-3ACALL-CAST-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:CALL-CAST%20MGL-PAX:MACRO"></a>

- [macro] **call-cast** *locative-type dref*

    Call the [cast][2066] to `locative-type` with `dref`.

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20FUNCTION-29"></a>
<a id="DREF-EXT:LOCATE-ERROR%20FUNCTION"></a>

- [function] **locate-error** *&optional format-control &rest format-args*

    Call this function to signal a [`locate-error`][6334] condition from the
    [dynamic extent][36e9] of a [`locate`][8f19] call, that is, from the `body`s
    of [`define-lookup`][49b5], [`define-locator`][16b6] and [`define-cast`][2066]. It is an error to
    call `locate-error` elsewhere.
    
    `format-control`, if non-`nil`, is a [format control][b8d5] for which
    `format-args` are suitable.

<a id="x-28DREF-EXT-3ACHECK-LOCATIVE-ARGS-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:CHECK-LOCATIVE-ARGS%20MGL-PAX:MACRO"></a>

- [macro] **check-locative-args** *locative-type locative-args*

    Signal a [`locate-error`][6334] condition if `locative-args` do not match the
    `lambda-list` argument of `locative-type` (not evaluated).

<a id="x-28DREF-EXT-3A-40EXTENDING-EVERYTHING-ELSE-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@EXTENDING-EVERYTHING-ELSE%20MGL-PAX:SECTION"></a>

### 9.5 Extending Everything Else

<a id="x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:RESOLVE*%20GENERIC-FUNCTION"></a>

- [generic-function] **resolve\*** *dref*

    Return the object defined by the definition `dref`
    refers to. Signal a [`resolve-error`][58ba] condition by calling the
    [`resolve-error`][7825] function if the lookup fails.
    
    To keep [`resolve`][63b4] a partial inverse of [`locate`][8f19], [`define-locator`][16b6] may be
    necessary for `resolve`able definitions. This function is for
    extending `resolve`. Do not call it directly.
    
    It is an error for methods of this generic function to return an
    [`xref`][1538].

<a id="x-28DREF-EXT-3ARESOLVE-ERROR-20FUNCTION-29"></a>
<a id="DREF-EXT:RESOLVE-ERROR%20FUNCTION"></a>

- [function] **resolve-error** *&rest format-and-args*

    Call this function to signal a [`resolve-error`][58ba] condition from the
    [dynamic extent][36e9] of a [`resolve*`][d3b3] method. It is an error to call
    `resolve-error` elsewhere.
    
    `format-and-args`, if non-`nil`, is a format string and arguments
    suitable for [`format`][ad78].

<a id="x-28DREF-EXT-3AMAP-DEFINITIONS-OF-NAME-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:MAP-DEFINITIONS-OF-NAME%20GENERIC-FUNCTION"></a>

- [generic-function] **map-definitions-of-name** *fn name locative-type*

    Call `fn` with [`dref`][d930]s which can be [`locate`][8f19]d
    with an `xref`([`0`][1538] [`1`][cda7]) with `name`, `locative-type` and some [`locative-args`][2444]. The
    strange wording here is because there may be multiple ways (and thus
    `xref`s) that refer to the same definition.
    
    For most locative types, there is at most one such definition, but
    for [`method`][51c3], for example, there may be many. The default method
    simply does `(dref name locative-type nil)` and calls `fn` with result
    if [`dref`][7e92] succeeds.
    
    `fn` is not called with the same (under [`xref=`][0617]) definition multiple
    times.

<a id="x-28DREF-EXT-3AMAP-DEFINITIONS-OF-TYPE-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:MAP-DEFINITIONS-OF-TYPE%20GENERIC-FUNCTION"></a>

- [generic-function] **map-definitions-of-type** *fn locative-type*

    Call `fn` with [`dref`][d930]s that can be [`locate`][8f19]d
    with an `xref`([`0`][1538] [`1`][cda7]) with `locative-type` with some `name` and [`locative-args`][2444].
    
    - Return `nil` if all possibilities have been exhausted.
    
    - Return true if there may be definitions left unmapped. In this
      case, the caller is responsible for trying all interned symbols
      with [`map-definitions-of-name`][97b4]. The default method simply returns
      true.
    
    `fn` may be called with `dref`s that are [`xref=`][0617] and differ only in their
    [`dref-origin`][e742].

<a id="x-28DREF-EXT-3AARGLIST-2A-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:ARGLIST*%20GENERIC-FUNCTION"></a>

- [generic-function] **arglist\*** *object*

    To extend [`arglist`][e6bd], specialize `object` on a normal
    Lisp type or on a subclass of [`dref`][d930].
    
    `arglist` first calls `arglist*` with its `object` argument. If that
    doesn't work (i.e. the second value returned is `nil`), then it calls
    `arglist*` with `object` either [`resolve`][63b4]d (if it's a `dref`) or [`locate`][8f19]d (if
    it's not a `dref`).
    
    - The default method returns `nil`, `nil`.
    
    - There is also a method specialized on [`dref`s][d930], that looks
      up the [`definition-property`][5f91] called `arglist` and returns its value
      with [`values-list`][dbd4]. Thus, an arglist and its kind can be specified
      with something like
    
        ```
        (setf (definition-property xref 'arglist)
              (list arglist :destructuring))
        ```
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-3AARGLIST-PARAMETERS-2A-20GENERIC-FUNCTION-29"></a>
<a id="DREF:ARGLIST-PARAMETERS*%20GENERIC-FUNCTION"></a>

- [generic-function] **arglist-parameters\*** *arglist arglist-type*

    To extend [`arglist-parameters`][b123]*, [`eql`][38a2]-specialize
    `arglist-type` on, say, :BOA (see [BOA lambda lists][830e]. This must
    be done if `arglist`* is extended to return an unsupported value.
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:DOCSTRING*%20GENERIC-FUNCTION"></a>

- [generic-function] **docstring\*** *object*

    To extend [`docstring`][affc], specialize `object` on a normal
    Lisp type or on a subclass of [`dref`][d930].
    
    `docstring` first calls `docstring*` with its `object` argument. If that
    doesn't work (i.e. `nil` is returned), then it calls `docstring*` with
    `object` either [`resolve`][63b4]d (if it's a `dref`) or [`locate`][8f19]d (if it's not a
    `dref`).
    
    - The default method returns `nil`.
    
    - A three-stage logic is implemented by an unspecialized `:around`
      method for docstrings and packages.
    
        - First, the primary method is called. If it returns a non-`nil`
          docstring and package, then these are returned by `docstring*`.
    
        - *Definition default*: The [`definition-property`][5f91] with indicator
          `docstring` of the `locate`d definition of `object` provides
          defaults for the docstring and/or the package, whichever are
          `nil`. These can be set for example as
    
            ```
            (setf (definition-property xref 'docstring)
                  (list docstring *package*))
            ```
    
            Note that the docstring and the package or both may be `nil`.
    
        - *Package-wide default*: If the [`dref-name`][1e36] of the `locate`d
          definition is a symbol, then an irregular `definition-property`
          provides further defaults. To default the docstring package of
          all names of package `x` to package `y`, you could write
    
            ```
            (setf (definition-property `(:package ,(find-package :x)) 'docstring)
                  (list "FIXME: Missing documentation." (find-package :y)))
            ```
    
    See [Package and Readtable][ab7e].
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-2A-20GENERIC-FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION*%20GENERIC-FUNCTION"></a>

- [generic-function] **source-location\*** *object*

    To extend [`source-location`][32da], specialize `object` on a
    normal Lisp type or on a subclass of [`dref`][d930].
    
    `source-location` first calls `source-location*` with its `object`
    argument. If that doesn't work (i.e. `nil` or `(:error <message>)` is
    returned), then it calls `source-location*` with `object` either
    [`resolve`][63b4]d (if it's a `dref`) or [`locate`][8f19]d (if it's not a `dref`).
    
    `source-location` returns the last of the `(:error <message>)`s
    encountered or a generic error message if only `nil`s were returned.
    
    - The default method returns `nil`.
    
    - There is also a method specialized on [`dref`s][d930], that looks
      up the [`definition-property`][5f91] called `source-location`. If present, it
      must be a function of no arguments that returns a source location
      or `nil`. Typically, this is set up in the defining macro like this:
    
        ```
        (setf (definition-property xref 'source-location)
              (this-source-location))
        ```
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3ANTH-VALUE-OR-WITH-OBJ-OR-DEF-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:NTH-VALUE-OR-WITH-OBJ-OR-DEF%20MGL-PAX:MACRO"></a>

- [macro] **nth-value-or-with-obj-or-def** *(obj nth-value) &body body*

    Evaluate `body`. If its `nth-value` is `nil`, evaluate it again with `obj`
    bound to a [`resolve`][63b4]d object (if `obj` was a definition) or a
    definition (if `obj` was not a definition). This is intended for
    implementing new operations with the fallback mechanism of [`arglist`][e6bd]*,
    [`docstring`][affc]* and [`source-location*`][444d].

<a id="x-28DREF-EXT-3A-40DEFINITION-PROPERTIES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@DEFINITION-PROPERTIES%20MGL-PAX:SECTION"></a>

#### 9.5.1 Definition Properties

Arbitrary data may be associated with definitions.
This mechanism is used by [`arglist*`][0a96], [`docstring*`][9fd4] and
[`source-location*`][444d] for easy extension.

The following functions take an `xref` argument and not a `dref`([`0`][d930] [`1`][7e92]) to
allow working with [non-canonical][9383] or
non-existent definitions.

<a id="x-28DREF-EXT-3ADEFINITION-PROPERTY-20FUNCTION-29"></a>
<a id="DREF-EXT:DEFINITION-PROPERTY%20FUNCTION"></a>

- [function] **definition-property** *obj indicator*

    Return the value of the property associated with `obj` whose name
    is [`eql`][38a2] to `indicator`. The second return value indicates whether the
    property was found. [`setf`][a138]able.
    
    - *Regular definition*: `obj` is commonly an `xref`([`0`][1538] [`1`][cda7]). `xref`s that are
      [`xref=`][0617] are equivalent for the purposes of `definition-property`.
    
    - *Irregular case*: If `obj` is not an `xref` (and, by extension, not a
      `dref`([`0`][d930] [`1`][7e92])), it is equivalent to other objects to which it is [`equal`][3fb5].
    
    For example, see the [`docstring*`][9fd4] generic-function, which uses both
    the regular and the irregular cases.

<a id="x-28DREF-EXT-3ADELETE-DEFINITION-PROPERTY-20FUNCTION-29"></a>
<a id="DREF-EXT:DELETE-DEFINITION-PROPERTY%20FUNCTION"></a>

- [function] **delete-definition-property** *obj indicator*

    Delete the property `indicator` of `obj` established by setting
    [`definition-property`][5f91]. Return true if the property was found.

<a id="x-28DREF-EXT-3ADEFINITION-PROPERTIES-20FUNCTION-29"></a>
<a id="DREF-EXT:DEFINITION-PROPERTIES%20FUNCTION"></a>

- [function] **definition-properties** *xref*

    Return the properties of `xref` as an association list.

<a id="x-28DREF-EXT-3ADELETE-DEFINITION-PROPERTIES-20FUNCTION-29"></a>
<a id="DREF-EXT:DELETE-DEFINITION-PROPERTIES%20FUNCTION"></a>

- [function] **delete-definition-properties** *xref*

    Delete all properties associated with `xref`.

<a id="x-28DREF-EXT-3AMOVE-DEFINITION-PROPERTIES-20FUNCTION-29"></a>
<a id="DREF-EXT:MOVE-DEFINITION-PROPERTIES%20FUNCTION"></a>

- [function] **move-definition-properties** *from-xref to-xref*

    Associate all properties of `from-xref` with `to-xref`, as if readding
    them one-by-one with `(setf definition-property)`, and
    deleting them from `from-xref` with [`delete-definition-property`][09b7].

<a id="x-28DREF-EXT-3A-40DREF-CLASSES-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@DREF-CLASSES%20MGL-PAX:SECTION"></a>

### 9.6 `dref-class`es

These are the [`dref-class`][25be]es corresponding to [Basic Locative Types][1d1d].
They are exported to make it possible to go beyond the
[Basic Operations][662d] (e.g. [`pax:document-object*`][8269]). For
[Defining Locative Types][f494], they are not necessary, as
[`define-locative-type`][b6c4] handles inheritance automatically based on its
`locative-supertypes` argument.

**[for Variables][462c]**

<a id="x-28DREF-EXT-3AVARIABLE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:VARIABLE-DREF%20CLASS"></a>

- [class] **variable-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`variable`][6c83].

<a id="x-28DREF-EXT-3ACONSTANT-DREF-20CLASS-29"></a>
<a id="DREF-EXT:CONSTANT-DREF%20CLASS"></a>

- [class] **constant-dref** *[variable-dref][ad35]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:constant`][c819].

**[for Macros][d45d]**

<a id="x-28DREF-EXT-3AMACRO-DREF-20CLASS-29"></a>
<a id="DREF-EXT:MACRO-DREF%20CLASS"></a>

- [class] **macro-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:macro`][f3cc].

<a id="x-28DREF-EXT-3ASYMBOL-MACRO-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SYMBOL-MACRO-DREF%20CLASS"></a>

- [class] **symbol-macro-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:symbol-macro`][be85].

<a id="x-28DREF-EXT-3ACOMPILER-MACRO-DREF-20CLASS-29"></a>
<a id="DREF-EXT:COMPILER-MACRO-DREF%20CLASS"></a>

- [class] **compiler-macro-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`compiler-macro`][41fd].

<a id="x-28DREF-EXT-3ASETF-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SETF-DREF%20CLASS"></a>

- [class] **setf-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`setf`][d83a].

<a id="x-28DREF-EXT-3ASETF-COMPILER-MACRO-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SETF-COMPILER-MACRO-DREF%20CLASS"></a>

- [class] **setf-compiler-macro-dref** *[compiler-macro-dref][59cf]*

    [`dref-ext:dref-class`][25be] of [`dref:setf-compiler-macro`][5df4].

**[for Functions][1d59]**

<a id="x-28DREF-EXT-3AFUNCTION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:FUNCTION-DREF%20CLASS"></a>

- [class] **function-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`function`][ba62].

<a id="x-28DREF-EXT-3ASETF-FUNCTION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SETF-FUNCTION-DREF%20CLASS"></a>

- [class] **setf-function-dref** *[function-dref][e576] [setf-dref][0db5]*

    [`dref-ext:dref-class`][25be] of [`dref:setf-function`][19f6].

<a id="x-28DREF-EXT-3AGENERIC-FUNCTION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:GENERIC-FUNCTION-DREF%20CLASS"></a>

- [class] **generic-function-dref** *[function-dref][e576]*

    [`dref-ext:dref-class`][25be] of [`generic-function`][5875].

<a id="x-28DREF-EXT-3ASETF-GENERIC-FUNCTION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SETF-GENERIC-FUNCTION-DREF%20CLASS"></a>

- [class] **setf-generic-function-dref** *[generic-function-dref][df33] [setf-function-dref][798d]*

    [`dref-ext:dref-class`][25be] of [`dref:setf-generic-function`][ab5e].

<a id="x-28DREF-EXT-3AMETHOD-DREF-20CLASS-29"></a>
<a id="DREF-EXT:METHOD-DREF%20CLASS"></a>

- [class] **method-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`method`][172e].

<a id="x-28DREF-EXT-3ASETF-METHOD-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SETF-METHOD-DREF%20CLASS"></a>

- [class] **setf-method-dref** *[method-dref][2c45] [setf-dref][0db5]*

    [`dref-ext:dref-class`][25be] of [`dref:setf-method`][1a03].

<a id="x-28DREF-EXT-3AMETHOD-COMBINATION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:METHOD-COMBINATION-DREF%20CLASS"></a>

- [class] **method-combination-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`method-combination`][82e0].

<a id="x-28DREF-EXT-3AREADER-DREF-20CLASS-29"></a>
<a id="DREF-EXT:READER-DREF%20CLASS"></a>

- [class] **reader-dref** *[method-dref][2c45]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:reader`][cc04f].

<a id="x-28DREF-EXT-3AWRITER-DREF-20CLASS-29"></a>
<a id="DREF-EXT:WRITER-DREF%20CLASS"></a>

- [class] **writer-dref** *[method-dref][2c45]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:writer`][e548].

<a id="x-28DREF-EXT-3AACCESSOR-DREF-20CLASS-29"></a>
<a id="DREF-EXT:ACCESSOR-DREF%20CLASS"></a>

- [class] **accessor-dref** *[reader-dref][ec6f] [writer-dref][2638] [setf-method-dref][5ab8]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:accessor`][00d4].

<a id="x-28DREF-EXT-3ASTRUCTURE-ACCESSOR-DREF-20CLASS-29"></a>
<a id="DREF-EXT:STRUCTURE-ACCESSOR-DREF%20CLASS"></a>

- [class] **structure-accessor-dref** *[setf-function-dref][798d] [function-dref][e576]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:structure-accessor`][090c].

**[for Types and Declarations][7a04]**

<a id="x-28DREF-EXT-3ATYPE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:TYPE-DREF%20CLASS"></a>

- [class] **type-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`type`][926d].

<a id="x-28DREF-EXT-3ACLASS-DREF-20CLASS-29"></a>
<a id="DREF-EXT:CLASS-DREF%20CLASS"></a>

- [class] **class-dref** *[type-dref][b4e9]*

    [`dref-ext:dref-class`][25be] of [`class`][2060].

<a id="x-28DREF-EXT-3ADECLARATION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:DECLARATION-DREF%20CLASS"></a>

- [class] **declaration-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`declaration`][6e04].

**[for the Condition System][408d]**

<a id="x-28DREF-EXT-3ACONDITION-DREF-20CLASS-29"></a>
<a id="DREF-EXT:CONDITION-DREF%20CLASS"></a>

- [class] **condition-dref** *[class-dref][b3a7]*

    [`dref-ext:dref-class`][25be] of [`condition`][c479].

<a id="x-28DREF-EXT-3ARESTART-DREF-20CLASS-29"></a>
<a id="DREF-EXT:RESTART-DREF%20CLASS"></a>

- [class] **restart-dref** *[symbol-locative-dref][34b9]*

    [`dref-ext:dref-class`][25be] of [`restart`][e023].

**[for Packages and Readtables][c339]**

<a id="x-28DREF-EXT-3AASDF-SYSTEM-DREF-20CLASS-29"></a>
<a id="DREF-EXT:ASDF-SYSTEM-DREF%20CLASS"></a>

- [class] **asdf-system-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`asdf/system:system`][c097].

<a id="x-28DREF-EXT-3APACKAGE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:PACKAGE-DREF%20CLASS"></a>

- [class] **package-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`package`][4dd7].

<a id="x-28DREF-EXT-3AREADTABLE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:READTABLE-DREF%20CLASS"></a>

- [class] **readtable-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`readtable`][7506].

**[for Unknown Definitions][58f1]**

<a id="x-28DREF-EXT-3AUNKNOWN-DREF-20CLASS-29"></a>
<a id="DREF-EXT:UNKNOWN-DREF%20CLASS"></a>

- [class] **unknown-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:unknown`][a951].

**[for DRef Constructs][da93]**

<a id="x-28DREF-EXT-3ADTYPE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:DTYPE-DREF%20CLASS"></a>

- [class] **dtype-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`dref:dtype`][85ba].

<a id="x-28DREF-EXT-3ALOCATIVE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:LOCATIVE-DREF%20CLASS"></a>

- [class] **locative-dref** *[dtype-dref][6aa6]*

    [`dref-ext:dref-class`][25be] of [`mgl-pax:locative`][0b3a].

<a id="x-28DREF-EXT-3ASYMBOL-LOCATIVE-DREF-20CLASS-29"></a>
<a id="DREF-EXT:SYMBOL-LOCATIVE-DREF%20CLASS"></a>

- [class] **symbol-locative-dref** *[dref][d930]*

    All [locative type][a11d]s defined with
    [`define-symbol-locative-type`][ee9b] inherit from this class.

<a id="x-28DREF-EXT-3ALAMBDA-DREF-20CLASS-29"></a>
<a id="DREF-EXT:LAMBDA-DREF%20CLASS"></a>

- [class] **lambda-dref** *[dref][d930]*

    [`dref-ext:dref-class`][25be] of [`lambda`][4796].

<a id="x-28DREF-EXT-3A-40SOURCE-LOCATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="DREF-EXT:@SOURCE-LOCATIONS%20MGL-PAX:SECTION"></a>

### 9.7 Source Locations

These represent the file or buffer position of a [defining
form][23a8] and are returned by the [`source-location`][32da] function. For
the details, see the Elisp function `slime-goto-source-location`.

<a id="x-28DREF-EXT-3AMAKE-SOURCE-LOCATION-20FUNCTION-29"></a>
<a id="DREF-EXT:MAKE-SOURCE-LOCATION%20FUNCTION"></a>

- [function] **make-source-location** *&key file file-position buffer buffer-position snippet*

    Make a Swank source location. The ultimate reference is
    `slime-goto-source-location` in `slime.el`. When `snippet` is
    provided, the match nearest to `file-position` is determined (see the
    Elisp `slime-isearch` and [`source-location-adjusted-file-position`][daacd]).

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-P-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-P%20FUNCTION"></a>

- [function] **source-location-p** *object*

    See if `object` is a source location object.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-FILE%20FUNCTION"></a>

- [function] **source-location-file** *location*

    Return the name of the file of the [defining form][23a8].
    This may be `nil`, for example, if `location` is of a [defining
    form][23a8] that was entered at the REPL, or compiled in the
    `*slime-scratch*` buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-POSITION-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-FILE-POSITION%20FUNCTION"></a>

- [function] **source-location-file-position** *location*

    Return the file position of the [defining form][23a8] or `nil`
    if it's not available. The first position is 0.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-BUFFER%20FUNCTION"></a>

- [function] **source-location-buffer** *location*

    Return the name of the Emacs buffer of the [defining form][23a8] or
    `nil` if there is no such Emacs buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-POSITION-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-BUFFER-POSITION%20FUNCTION"></a>

- [function] **source-location-buffer-position** *location*

    Return the position of the [defining form][23a8] in
    [`source-location-buffer`][39c2] or `nil` if it's not available. The first
    position is 1.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-SNIPPET-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-SNIPPET%20FUNCTION"></a>

- [function] **source-location-snippet** *location*

    Return the [defining form][23a8] or a prefix of it as a string or `nil`
    if it's not available.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-ADJUSTED-FILE-POSITION-20FUNCTION-29"></a>
<a id="DREF-EXT:SOURCE-LOCATION-ADJUSTED-FILE-POSITION%20FUNCTION"></a>

- [function] **source-location-adjusted-file-position** *location*

    Return the actual file position `location` points to allowing for
    some deviation from the raw [`source-location-file-position`][be18], which is
    adjusted by searching for the nearest occurrence of
    [`source-location-snippet`][6ec3] in the file. The file is read using the same
    external format that ASDF would use to compile it. Needless to say,
    this can be a very expensive operation.
    
    If [`source-location-file`][ae5a] is `nil`, `nil` is returned. If there is no
    snippet, or it doesn't match, then `source-location-file-position` (or
    0 if that's `nil`) is returned.
    
    This is a non-interactive companion to the Elisp function
    `slime-location-offset`, supporting only file positions and
    non-partial matching of snippets.

<a id="x-28DREF-EXT-3ATHIS-SOURCE-LOCATION-20MGL-PAX-3AMACRO-29"></a>
<a id="DREF-EXT:THIS-SOURCE-LOCATION%20MGL-PAX:MACRO"></a>

- [macro] **this-source-location**

    The value of this macro form is a function of no arguments that
    returns its own [`source-location`][32da].

  [006c]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm "DEFINE-METHOD-COMBINATION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [00d4]: #MGL-PAX:ACCESSOR%20MGL-PAX:LOCATIVE "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [021a]: #%22dref%22%20ASDF%2FSYSTEM:SYSTEM "\"dref\" ASDF/SYSTEM:SYSTEM"
  [023a]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss049_w.htm "\"ISSUE:CLOS-CONDITIONS\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0376]: http://www.lispworks.com/documentation/HyperSpec/Body/a_member.htm "MEMBER (MGL-PAX:CLHS NIL)"
  [059c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#ordinary_lambda_list "\"ordinary lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [0617]: #DREF:XREF%3D%20FUNCTION "DREF:XREF= FUNCTION"
  [090c]: #MGL-PAX:STRUCTURE-ACCESSOR%20MGL-PAX:LOCATIVE "MGL-PAX:STRUCTURE-ACCESSOR MGL-PAX:LOCATIVE"
  [0976]: #DREF:DREF-LOCATIVE-ARGS%20FUNCTION "DREF:DREF-LOCATIVE-ARGS FUNCTION"
  [09b7]: #DREF-EXT:DELETE-DEFINITION-PROPERTY%20FUNCTION "DREF-EXT:DELETE-DEFINITION-PROPERTY FUNCTION"
  [0a7c]: http://www.lispworks.com/documentation/HyperSpec/Body/a_atom.htm "ATOM (MGL-PAX:CLHS NIL)"
  [0a96]: #DREF-EXT:ARGLIST*%20GENERIC-FUNCTION "DREF-EXT:ARGLIST* GENERIC-FUNCTION"
  [0b3a]: #MGL-PAX:LOCATIVE%20MGL-PAX:LOCATIVE "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0c7e]: #%22dref%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM "\"dref/full\" ASDF/SYSTEM:SYSTEM"
  [0d07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm "SYMBOL-NAME (MGL-PAX:CLHS FUNCTION)"
  [0db5]: #DREF-EXT:SETF-DREF%20CLASS "DREF-EXT:SETF-DREF CLASS"
  [0fa3]: pax-manual.md#MGL-PAX:@LOCATIVE-ALIASES%20MGL-PAX:SECTION "Locative Aliases"
  [0ff7]: http://www.lispworks.com/documentation/HyperSpec/Body/04_.htm "\"4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [1048]: #DREF:BACKEND%20FUNCTION "DREF:BACKEND FUNCTION"
  [10a7]: #DREF-EXT:CHECK-LOCATIVE-ARGS%20MGL-PAX:MACRO "DREF-EXT:CHECK-LOCATIVE-ARGS MGL-PAX:MACRO"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [1281]: #DREF:DEFSTRUCT*%20MGL-PAX:MACRO "DREF:DEFSTRUCT* MGL-PAX:MACRO"
  [130a]: #DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS%20FUNCTION "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS FUNCTION"
  [1490]: #DREF:XREF-LOCATIVE-ARGS%20FUNCTION "DREF:XREF-LOCATIVE-ARGS FUNCTION"
  [14a4]: #DREF:BACKEND%20SETF "DREF:BACKEND SETF"
  [14cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [14ea]: #DREF:@REFERENCES%20MGL-PAX:SECTION "References"
  [1538]: #DREF:XREF%20CLASS "DREF:XREF CLASS"
  [1574]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm "DECLARE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [16b6]: #DREF-EXT:DEFINE-LOCATOR%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOCATOR MGL-PAX:MACRO"
  [172e]: #METHOD%20MGL-PAX:LOCATIVE "METHOD MGL-PAX:LOCATIVE"
  [19f6]: #DREF:SETF-FUNCTION%20MGL-PAX:LOCATIVE "DREF:SETF-FUNCTION MGL-PAX:LOCATIVE"
  [1a03]: #DREF:SETF-METHOD%20MGL-PAX:LOCATIVE "DREF:SETF-METHOD MGL-PAX:LOCATIVE"
  [1d1d]: #DREF:@BASIC-LOCATIVE-TYPES%20MGL-PAX:SECTION "Basic Locative Types"
  [1d59]: #DREF:@FUNCTIONLIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Functions and Methods"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1db8]: #DREF-EXT:SORT-REFERENCES%20FUNCTION "DREF-EXT:SORT-REFERENCES FUNCTION"
  [1e36]: #DREF:DREF-NAME%20%28MGL-PAX:READER%20DREF:DREF%29 "DREF:DREF-NAME (MGL-PAX:READER DREF:DREF)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [2060]: #CLASS%20MGL-PAX:LOCATIVE "CLASS MGL-PAX:LOCATIVE"
  [2066]: #DREF-EXT:DEFINE-CAST%20MGL-PAX:MACRO "DREF-EXT:DEFINE-CAST MGL-PAX:MACRO"
  [2143]: #DREF:@DEFINITION%20MGL-PAX:GLOSSARY-TERM "definition"
  [23a8]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#defining_form "\"defining form\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [23d5]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm "DEFINE-COMPILER-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [2415]: pax-manual.md "PAX Manual"
  [2444]: #DREF-EXT:LOCATIVE-ARGS%20FUNCTION "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [25be]: #DREF-EXT:DREF-CLASS%20FUNCTION "DREF-EXT:DREF-CLASS FUNCTION"
  [2638]: #DREF-EXT:WRITER-DREF%20CLASS "DREF-EXT:WRITER-DREF CLASS"
  [292a]: pax-manual.md#MGL-PAX:@PAX-LOCATIVES%20MGL-PAX:SECTION "PAX Locatives"
  [29a1]: http://www.lispworks.com/documentation/HyperSpec/Body/07_fb.htm "\"7.6.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [2ab8]: #DREF-EXT:CALL-LOOKUP%20MGL-PAX:MACRO "DREF-EXT:CALL-LOOKUP MGL-PAX:MACRO"
  [2b8b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_satisf.htm "SATISFIES (MGL-PAX:CLHS TYPE)"
  [2c45]: #DREF-EXT:METHOD-DREF%20CLASS "DREF-EXT:METHOD-DREF CLASS"
  [2ff3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm "EQUALP (MGL-PAX:CLHS FUNCTION)"
  [30ad]: #DREF:LISP-LOCATIVE-TYPES%20FUNCTION "DREF:LISP-LOCATIVE-TYPES FUNCTION"
  [32da]: #DREF:SOURCE-LOCATION%20FUNCTION "DREF:SOURCE-LOCATION FUNCTION"
  [3301]: #DREF:TOP%20DREF:DTYPE "DREF:TOP DREF:DTYPE"
  [34b9]: #DREF-EXT:SYMBOL-LOCATIVE-DREF%20CLASS "DREF-EXT:SYMBOL-LOCATIVE-DREF CLASS"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander "\"setf expander\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent "\"dynamic extent\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [38a2]: http://www.lispworks.com/documentation/HyperSpec/Body/a_eql.htm "EQL (MGL-PAX:CLHS NIL)"
  [38e4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rst.htm "RESTART (MGL-PAX:CLHS CLASS)"
  [39c2]: #DREF-EXT:SOURCE-LOCATION-BUFFER%20FUNCTION "DREF-EXT:SOURCE-LOCATION-BUFFER FUNCTION"
  [3b96]: #DREF-EXT:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO "DREF-EXT:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [3bdc]: #DREF-EXT:MAKE-SOURCE-LOCATION%20FUNCTION "DREF-EXT:MAKE-SOURCE-LOCATION FUNCTION"
  [3c8a]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#object "\"object\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [408d]: #DREF:@CONDITION-SYSTEM-LOCATIVES%20MGL-PAX:SECTION "Locatives for the Condition System"
  [41fd]: #COMPILER-MACRO%20MGL-PAX:LOCATIVE "COMPILER-MACRO MGL-PAX:LOCATIVE"
  [432c]: pax-manual.md#MGL-PAX:DOCUMENT%20FUNCTION "MGL-PAX:DOCUMENT FUNCTION"
  [4336]: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm "\"3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [43bd]: #DREF:@REFERENCE%20MGL-PAX:GLOSSARY-TERM "reference"
  [444d]: #DREF-EXT:SOURCE-LOCATION*%20GENERIC-FUNCTION "DREF-EXT:SOURCE-LOCATION* GENERIC-FUNCTION"
  [462c]: #DREF:@VARIABLELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Variables"
  [46c0]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm "DEFINE-SYMBOL-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4796]: #LAMBDA%20MGL-PAX:LOCATIVE "LAMBDA MGL-PAX:LOCATIVE"
  [49b5]: #DREF-EXT:DEFINE-LOOKUP%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOOKUP MGL-PAX:MACRO"
  [4b93]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_ni.htm "PACKAGE-NICKNAMES (MGL-PAX:CLHS FUNCTION)"
  [4c16]: #DREF-EXT:@EXTENDING-LOCATE%20MGL-PAX:SECTION "Extending `locate`"
  [4dc9]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_p.htm "FIND-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [4dd7]: #PACKAGE%20MGL-PAX:LOCATIVE "PACKAGE MGL-PAX:LOCATIVE"
  [5119]: pax-manual.md#MGL-PAX:GLOSSARY-TERM%20MGL-PAX:LOCATIVE "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [515e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#compound_type_specifier "\"compound type specifier\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [5191]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name "\"function name\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [51c3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD (MGL-PAX:CLHS CLASS)"
  [5447]: #DREF:XREF-NAME%20%28MGL-PAX:READER%20DREF:XREF%29 "DREF:XREF-NAME (MGL-PAX:READER DREF:XREF)"
  [548e]: #DREF-EXT:DEFINE-LOCATIVE-ALIAS%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [56b9]: pax-manual.md#MGL-PAX:CONCEPT%20MGL-PAX:LOCATIVE "MGL-PAX:CONCEPT MGL-PAX:LOCATIVE"
  [5875]: #GENERIC-FUNCTION%20MGL-PAX:LOCATIVE "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [58ba]: #DREF-EXT:RESOLVE-ERROR%20CONDITION "DREF-EXT:RESOLVE-ERROR CONDITION"
  [58f1]: #DREF:@UNKNOWN-DEFINITIONS%20MGL-PAX:SECTION "Locatives for Unknown Definitions"
  [5968]: autoload-manual.md#%22autoload%22%20ASDF%2FSYSTEM:SYSTEM "\"autoload\" ASDF/SYSTEM:SYSTEM"
  [59c9]: #DREF-EXT:@SYMBOL-LOCATIVES%20MGL-PAX:SECTION "Symbol Locatives"
  [59cf]: #DREF-EXT:COMPILER-MACRO-DREF%20CLASS "DREF-EXT:COMPILER-MACRO-DREF CLASS"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5ab8]: #DREF-EXT:SETF-METHOD-DREF%20CLASS "DREF-EXT:SETF-METHOD-DREF CLASS"
  [5cd7]: pax-manual.md#MGL-PAX:INCLUDE%20MGL-PAX:LOCATIVE "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5d41]: named-readtables-manual.md#EDITOR-HINTS.NAMED-READTABLES:FIND-READTABLE%20FUNCTION "EDITOR-HINTS.NAMED-READTABLES:FIND-READTABLE FUNCTION"
  [5df4]: #DREF:SETF-COMPILER-MACRO%20MGL-PAX:LOCATIVE "DREF:SETF-COMPILER-MACRO MGL-PAX:LOCATIVE"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5f91]: #DREF-EXT:DEFINITION-PROPERTY%20FUNCTION "DREF-EXT:DEFINITION-PROPERTY FUNCTION"
  [5fc4]: #DREF:@NAME%20MGL-PAX:GLOSSARY-TERM "name"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list "\"destructuring lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [62e7]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_environment "\"dynamic environment\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [6334]: #DREF-EXT:LOCATE-ERROR%20CONDITION "DREF-EXT:LOCATE-ERROR CONDITION"
  [6354]: #DREF-EXT:@DREF-CLASSES%20MGL-PAX:SECTION "`dref-class`es"
  [63b4]: #DREF:RESOLVE%20FUNCTION "DREF:RESOLVE FUNCTION"
  [65b4]: #DREF:DREF-APROPOS%20FUNCTION "DREF:DREF-APROPOS FUNCTION"
  [662d]: #DREF:@BASIC-OPERATIONS%20MGL-PAX:SECTION "Basic Operations"
  [66dc]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defset.htm "DEFSETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [672f]: pax-manual.md#MGL-PAX:SECTION%20MGL-PAX:LOCATIVE "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [68b4]: #DREF-EXT:DEFINE-PSEUDO-LOCATIVE-TYPE%20MGL-PAX:MACRO "DREF-EXT:DEFINE-PSEUDO-LOCATIVE-TYPE MGL-PAX:MACRO"
  [68cc]: http://www.lispworks.com/documentation/HyperSpec/Body/a_string.htm "STRING (MGL-PAX:CLHS NIL)"
  [68f1]: sbcl-manual.md#DOCUMENTATION%20GENERIC-FUNCTION "DOCUMENTATION GENERIC-FUNCTION"
  [68fb]: #DREF-EXT:@EXTENDING-DREF%20MGL-PAX:SECTION "Extending DRef"
  [6932]: #DREF-EXT:LOCATE-ERROR%20FUNCTION "DREF-EXT:LOCATE-ERROR FUNCTION"
  [6a02]: named-readtables-manual.md#EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE%20MGL-PAX:MACRO "EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE MGL-PAX:MACRO"
  [6a98]: http://www.lispworks.com/documentation/HyperSpec/Body/f_clas_1.htm "CLASS-OF (MGL-PAX:CLHS FUNCTION)"
  [6aa6]: #DREF-EXT:DTYPE-DREF%20CLASS "DREF-EXT:DTYPE-DREF CLASS"
  [6c83]: #VARIABLE%20MGL-PAX:LOCATIVE "VARIABLE MGL-PAX:LOCATIVE"
  [6d46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm "FIND-METHOD (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [6e04]: #DECLARATION%20MGL-PAX:LOCATIVE "DECLARATION MGL-PAX:LOCATIVE"
  [6e6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pkg.htm "MAKE-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [6ec3]: #DREF-EXT:SOURCE-LOCATION-SNIPPET%20FUNCTION "DREF-EXT:SOURCE-LOCATION-SNIPPET FUNCTION"
  [6f15]: #DREF:@DISSECTING-REFERENCES%20MGL-PAX:SECTION "Dissecting References"
  [6f95]: http://www.lispworks.com/documentation/HyperSpec/Body/f_file_p.htm "FILE-POSITION (MGL-PAX:CLHS FUNCTION)"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [718a]: named-readtables-manual.md#%22named-readtables%22%20ASDF%2FSYSTEM:SYSTEM "\"named-readtables\" ASDF/SYSTEM:SYSTEM"
  [7328]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apropo.htm "APROPOS-LIST (MGL-PAX:CLHS FUNCTION)"
  [7334]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFVAR (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [7506]: #READTABLE%20MGL-PAX:LOCATIVE "READTABLE MGL-PAX:LOCATIVE"
  [7825]: #DREF-EXT:RESOLVE-ERROR%20FUNCTION "DREF-EXT:RESOLVE-ERROR FUNCTION"
  [793d]: #DREF-EXT:@EXTENDING-EVERYTHING-ELSE%20MGL-PAX:SECTION "Extending Everything Else"
  [798d]: #DREF-EXT:SETF-FUNCTION-DREF%20CLASS "DREF-EXT:SETF-FUNCTION-DREF CLASS"
  [7a04]: #DREF:@TYPELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Types and Declarations"
  [7ac8]: #DREF:@LOCATIVE%20MGL-PAX:GLOSSARY-TERM "locative"
  [7c9f]: http://www.lispworks.com/documentation/HyperSpec/Body/d_type.htm "TYPE (MGL-PAX:CLHS DECLARATION)"
  [7e92]: #DREF:DREF%20FUNCTION "DREF:DREF FUNCTION"
  [7f9a]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [80a8]: #DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS%20FUNCTION "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS FUNCTION"
  [817d]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#deftype_lambda_list "\"deftype lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [8269]: pax-manual.md#MGL-PAX:DOCUMENT-OBJECT*%20GENERIC-FUNCTION "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [82e0]: #METHOD-COMBINATION%20MGL-PAX:LOCATIVE "METHOD-COMBINATION MGL-PAX:LOCATIVE"
  [830e]: http://www.lispworks.com/documentation/HyperSpec/Body/03_df.htm "\"3.4.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [840e]: #DREF:XREF-LOCATIVE-TYPE%20FUNCTION "DREF:XREF-LOCATIVE-TYPE FUNCTION"
  [8529]: #DREF-EXT:@DEFAULT-DOWNCAST%20MGL-PAX:SECTION "Default Downcast"
  [852d]: #DREF:@REFERENCES-GLOSSARY%20MGL-PAX:SECTION "References Glossary"
  [85ba]: #DREF:DTYPE%20MGL-PAX:LOCATIVE "DREF:DTYPE MGL-PAX:LOCATIVE"
  [867c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function_name "\"setf function name\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [86cc]: http://www.lispworks.com/documentation/HyperSpec/Body/a_use_va.htm "USE-VALUE (MGL-PAX:CLHS NIL)"
  [87fc]: #DREF-EXT:@INITIAL-DEFINITION%20MGL-PAX:SECTION "Initial Definition"
  [884d]: #DREF:@BACKENDS%20MGL-PAX:SECTION "Backends"
  [8934]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8f19]: #DREF:LOCATE%20FUNCTION "DREF:LOCATE FUNCTION"
  [926d]: #TYPE%20MGL-PAX:LOCATIVE "TYPE MGL-PAX:LOCATIVE"
  [9383]: #DREF-EXT:@CANONICALIZATION%20MGL-PAX:SECTION "Canonicalization"
  [943a]: #DREF:PSEUDO%20DREF:DTYPE "DREF:PSEUDO DREF:DTYPE"
  [94d1]: #DREF:LOCATIVE-ALIASES%20FUNCTION "DREF:LOCATIVE-ALIASES FUNCTION"
  [954a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_not.htm "NOT (MGL-PAX:CLHS TYPE)"
  [963f]: #DREF:DTYPEP%20FUNCTION "DREF:DTYPEP FUNCTION"
  [97b4]: #DREF-EXT:MAP-DEFINITIONS-OF-NAME%20GENERIC-FUNCTION "DREF-EXT:MAP-DEFINITIONS-OF-NAME GENERIC-FUNCTION"
  [97ba]: #DREF-EXT:LOCATIVE-TYPE%20FUNCTION "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [99b0]: #DREF:LOCATIVE-TYPES%20FUNCTION "DREF:LOCATIVE-TYPES FUNCTION"
  [99b05]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function "\"setf function\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9a71]: http://www.lispworks.com/documentation/HyperSpec/Body/f_specia.htm "SPECIAL-OPERATOR-P (MGL-PAX:CLHS FUNCTION)"
  [9b12]: http://www.lispworks.com/documentation/HyperSpec/Body/t_intege.htm "INTEGER (MGL-PAX:CLHS CLASS)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9b70]: http://www.lispworks.com/documentation/HyperSpec/Body/t_meth_1.htm "METHOD-COMBINATION (MGL-PAX:CLHS CLASS)"
  [9bf9]: #DREF:@REVERSE-DEFINITION-ORDER%20MGL-PAX:GLOSSARY-TERM "reverse definition order"
  [9caa]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm "TYPE-OF (MGL-PAX:CLHS FUNCTION)"
  [9d60]: #DREF-EXT:@EXTENSION-TUTORIAL%20MGL-PAX:SECTION "Extension Tutorial"
  [9fd4]: #DREF-EXT:DOCSTRING*%20GENERIC-FUNCTION "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a078]: #DREF-EXT:@SOURCE-LOCATIONS%20MGL-PAX:SECTION "Source Locations"
  [a11d]: #DREF:@LOCATIVE-TYPE%20MGL-PAX:GLOSSARY-TERM "locative type"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a22e]: #DREF:DREF-LOCATIVE-TYPE%20FUNCTION "DREF:DREF-LOCATIVE-TYPE FUNCTION"
  [a26f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consta.htm "CONSTANTP (MGL-PAX:CLHS FUNCTION)"
  [a459]: #DREF:@DTYPES%20MGL-PAX:SECTION "`dtype`s"
  [a51f]: http://www.lispworks.com/documentation/HyperSpec/Body/a_fn.htm "FUNCTION (MGL-PAX:CLHS NIL)"
  [a541]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRINC-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [a584]: #DREF:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION "Links and Systems"
  [a70d]: #DREF:XREF-LOCATIVE%20%28MGL-PAX:READER%20DREF:XREF%29 "DREF:XREF-LOCATIVE (MGL-PAX:READER DREF:XREF)"
  [a951]: #MGL-PAX:UNKNOWN%20MGL-PAX:LOCATIVE "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ab5e]: #DREF:SETF-GENERIC-FUNCTION%20MGL-PAX:LOCATIVE "DREF:SETF-GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [ab7e]: pax-manual.md#MGL-PAX:@PACKAGE-AND-READTABLE%20MGL-PAX:SECTION "Package and Readtable"
  [ad35]: #DREF-EXT:VARIABLE-DREF%20CLASS "DREF-EXT:VARIABLE-DREF CLASS"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: #DREF:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [adc7]: #DREF-EXT:@DEFINING-LOOKUPS-LOCATORS-AND-CASTS%20MGL-PAX:SECTION "Defining Lookups, Locators and Casts"
  [ade6]: #DREF:@PRESENTATION%20MGL-PAX:GLOSSARY-TERM "presentation"
  [ae5a]: #DREF-EXT:SOURCE-LOCATION-FILE%20FUNCTION "DREF-EXT:SOURCE-LOCATION-FILE FUNCTION"
  [affc]: #MGL-PAX:DOCSTRING%20FUNCTION "MGL-PAX:DOCSTRING FUNCTION"
  [b038]: #DREF-EXT:*CHECK-LOCATE*%20VARIABLE "DREF-EXT:*CHECK-LOCATE* VARIABLE"
  [b123]: #DREF:ARGLIST-PARAMETERS%20FUNCTION "DREF:ARGLIST-PARAMETERS FUNCTION"
  [b3a7]: #DREF-EXT:CLASS-DREF%20CLASS "DREF-EXT:CLASS-DREF CLASS"
  [b4e9]: #DREF-EXT:TYPE-DREF%20CLASS "DREF-EXT:TYPE-DREF CLASS"
  [b6c4]: #DREF-EXT:DEFINE-LOCATIVE-TYPE%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b7fc]: pax-manual.md#MGL-PAX:@APROPOS%20MGL-PAX:SECTION "Apropos"
  [b8d5]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#format_control "\"format control\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [ba62]: #FUNCTION%20MGL-PAX:LOCATIVE "FUNCTION MGL-PAX:LOCATIVE"
  [bb23]: #DREF:DEFINE-RESTART%20MGL-PAX:MACRO "DREF:DEFINE-RESTART MGL-PAX:MACRO"
  [be18]: #DREF-EXT:SOURCE-LOCATION-FILE-POSITION%20FUNCTION "DREF-EXT:SOURCE-LOCATION-FILE-POSITION FUNCTION"
  [be85]: #MGL-PAX:SYMBOL-MACRO%20MGL-PAX:LOCATIVE "MGL-PAX:SYMBOL-MACRO MGL-PAX:LOCATIVE"
  [c097]: #ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c339]: #DREF:@PACKAGELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Packages and Readtables"
  [c340]: #DREF:PSEUDO-LOCATIVE-TYPES%20FUNCTION "DREF:PSEUDO-LOCATIVE-TYPES FUNCTION"
  [c479]: #CONDITION%20MGL-PAX:LOCATIVE "CONDITION MGL-PAX:LOCATIVE"
  [c575]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp_ma.htm "COMPILER-MACRO-FUNCTION (MGL-PAX:CLHS FUNCTION)"
  [c5ae]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [c635]: #DREF:DEFINE-DTYPE%20MGL-PAX:MACRO "DREF:DEFINE-DTYPE MGL-PAX:MACRO"
  [c68e]: #DREF-EXT:@CAST-NAME-CHANGE%20MGL-PAX:SECTION "Cast Name Change"
  [c7f7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defgen.htm "DEFGENERIC (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c819]: #MGL-PAX:CONSTANT%20MGL-PAX:LOCATIVE "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c930]: pax-manual.md#MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P%20GENERIC-FUNCTION "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [c9ab]: #DREF-EXT:@LOCATIVE-TYPE-HIERARCHY%20MGL-PAX:SECTION "Locative Type Hierarchy"
  [c9de]: #DREF-EXT:@DEFINITION-PROPERTIES%20MGL-PAX:SECTION "Definition Properties"
  [cc04]: #DREF:DREF-LOCATIVE%20%28MGL-PAX:READER%20DREF:DREF%29 "DREF:DREF-LOCATIVE (MGL-PAX:READER DREF:DREF)"
  [cc04f]: #MGL-PAX:READER%20MGL-PAX:LOCATIVE "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cc32]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_m.htm#macro_lambda_list "\"macro lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [cda7]: #DREF:XREF%20FUNCTION "DREF:XREF FUNCTION"
  [cf03]: #DREF-EXT:SORT-LOCATIVE-TYPES%20FUNCTION "DREF-EXT:SORT-LOCATIVE-TYPES FUNCTION"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_3.htm "DEFINE-SETF-EXPANDER (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d3b3]: #DREF-EXT:RESOLVE*%20GENERIC-FUNCTION "DREF-EXT:RESOLVE* GENERIC-FUNCTION"
  [d3e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm "PROCLAIM (MGL-PAX:CLHS FUNCTION)"
  [d45d]: #DREF:@MACROLIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Macros"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d6c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_unionc.htm "UNION (MGL-PAX:CLHS FUNCTION)"
  [d83a]: #SETF%20MGL-PAX:LOCATIVE "SETF MGL-PAX:LOCATIVE"
  [d930]: #DREF:DREF%20CLASS "DREF:DREF CLASS"
  [d9ad]: #DREF:ARGLIST-PARAMETERS*%20GENERIC-FUNCTION "DREF:ARGLIST-PARAMETERS* GENERIC-FUNCTION"
  [da2e]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss048_w.htm "\"ISSUE:CLOS-CONDITIONS-AGAIN\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [da65]: #STRUCTURE%20MGL-PAX:LOCATIVE "STRUCTURE MGL-PAX:LOCATIVE"
  [da93]: #DREF:@DREF-LOCATIVES%20MGL-PAX:SECTION "Locatives for DRef Constructs"
  [daac]: http://www.lispworks.com/documentation/HyperSpec/Body/f_subtpp.htm "SUBTYPEP (MGL-PAX:CLHS FUNCTION)"
  [daacd]: #DREF-EXT:SOURCE-LOCATION-ADJUSTED-FILE-POSITION%20FUNCTION "DREF-EXT:SOURCE-LOCATION-ADJUSTED-FILE-POSITION FUNCTION"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dbd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST (MGL-PAX:CLHS FUNCTION)"
  [dd55]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND (MGL-PAX:CLHS TYPE)"
  [df33]: #DREF-EXT:GENERIC-FUNCTION-DREF%20CLASS "DREF-EXT:GENERIC-FUNCTION-DREF CLASS"
  [e023]: #RESTART%20MGL-PAX:LOCATIVE "RESTART MGL-PAX:LOCATIVE"
  [e196]: #DREF:DEFINITIONS%20FUNCTION "DREF:DEFINITIONS FUNCTION"
  [e1d4]: #DREF:@LISTING-DEFINITIONS%20MGL-PAX:SECTION "Listing Definitions"
  [e237]: http://www.lispworks.com/documentation/HyperSpec/Body/09_.htm "\"9\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e2d1]: http://www.lispworks.com/documentation/HyperSpec/Body/t_or.htm "OR (MGL-PAX:CLHS TYPE)"
  [e548]: #MGL-PAX:WRITER%20MGL-PAX:LOCATIVE "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [e576]: #DREF-EXT:FUNCTION-DREF%20CLASS "DREF-EXT:FUNCTION-DREF CLASS"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e608]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_cl.htm "STRUCTURE-CLASS (MGL-PAX:CLHS CLASS)"
  [e6bd]: #DREF:ARGLIST%20FUNCTION "DREF:ARGLIST FUNCTION"
  [e742]: #DREF:DREF-ORIGIN%20%28MGL-PAX:READER%20DREF:DREF%29 "DREF:DREF-ORIGIN (MGL-PAX:READER DREF:DREF)"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [e924]: http://www.lispworks.com/documentation/HyperSpec/Body/f_macro_.htm "MACRO-FUNCTION (MGL-PAX:CLHS FUNCTION)"
  [eac1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defstr.htm "DEFSTRUCT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ead6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm "DEFCLASS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ebea]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ec6f]: #DREF-EXT:READER-DREF%20CLASS "DREF-EXT:READER-DREF CLASS"
  [ed5f]: pax-manual.md#MGL-PAX:CLHS%20MGL-PAX:LOCATIVE "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ee9b]: #DREF-EXT:DEFINE-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO "DREF-EXT:DEFINE-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [efe2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm "GENERIC-FUNCTION (MGL-PAX:CLHS CLASS)"
  [f3cc]: #MGL-PAX:MACRO%20MGL-PAX:LOCATIVE "MGL-PAX:MACRO MGL-PAX:LOCATIVE"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f494]: #DREF-EXT:@DEFINING-LOCATIVE-TYPES%20MGL-PAX:SECTION "Defining Locative Types"
  [f5a0]: #DREF:BACKEND-AVAILABLE-P%20FUNCTION "DREF:BACKEND-AVAILABLE-P FUNCTION"
  [f7e4]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_5.htm "DEFINE-CONDITION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [fe9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm "REST (MGL-PAX:CLHS FUNCTION)"
  [ffd0]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#system_class "\"system class\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
