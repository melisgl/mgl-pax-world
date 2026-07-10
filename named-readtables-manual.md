<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-MANUAL%20MGL-PAX:SECTION"></a>

# Named Readtables Manual

## Table of Contents

- [1 Introduction][7831]

    - [1.1 Links and Systems][b13e]

    - [1.2 Acknowledgements][76be]

- [2 Overview][f7fa]

    - [2.1 Notes on the API][7326]

    - [2.2 Important API idiosyncrasies][6b83]

    - [2.3 Preregistered Readtables][8115]

    - [2.4 Examples][911e]

- [3 Reference][4e14]

###### \[in package EDITOR-HINTS.NAMED-READTABLES with nicknames NAMED-READTABLES\]

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@INTRODUCTION%20MGL-PAX:SECTION"></a>

## 1 Introduction

Named-Readtables is a library that provides a namespace for
readtables akin to the already-existing namespace of packages. In
particular:

- you can associate readtables with names, and retrieve
  readtables by names;

- you can associate source files with readtable names, and be
  sure that the right readtable is active when compiling/loading
  the file;

- similiarly, your development environment now has a chance to
  automatically determine what readtable should be active while
  processing source forms on interactive commands (e.g. think of
  `C-c C-c` in Slime (yet to be done)).

It follows that Named-Readtables is a facility for using readtables in
a localized way.

Additionally, it also attempts to become a facility for using
readtables in a *modular* way. In particular:

- it provides a macro to specify the content of a readtable at a
  glance;

- it makes it possible to use multiple inheritance between readtables.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION"></a>

### 1.1 Links and Systems

The official repository is <https://github.com/melisgl/named-readtables>,
and this document in available in various formats on
<https://fixnum.com> for the latest version.

<a id="x-28-22named-readtables-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22named-readtables%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- \[system\] **"named-readtables"**

    - *Version:* 0.9

    - *Description:* Library that creates a namespace for readtables akin
    to the namespace of packages.

    - *Licence:* BSD, see LICENSE

    - *Author:* Tobias C. Rittweiler <trittweiler@common-lisp.net>

    - *Maintainer:* Gábor Melis <mega@retes.hu>

    - *Mailto:* [mega@retes.hu](mailto:mega@retes.hu)

    - *Homepage:* <https://github.com/melisgl/named-readtables>

    - *Bug tracker:* <https://github.com/melisgl/named-readtables/issues>

    - *Source control:* [GIT](https://github.com/melisgl/named-readtables.git)

    - *Depends on:* mgl-pax-bootstrap

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40ACKNOWLEDGEMENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@ACKNOWLEDGEMENTS%20MGL-PAX:SECTION"></a>

### 1.2 Acknowledgements

Thanks to Robert Goldman for making me want to write this library.

Thanks to Stephen Compall, Ariel Badichi, David Lichteblau, Bart
Botta, David Crawford, and Pascal Costanza for being early adopters,
providing comments and bugfixes.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40OVERVIEW-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@OVERVIEW%20MGL-PAX:SECTION"></a>

## 2 Overview

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40API-NOTES-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@API-NOTES%20MGL-PAX:SECTION"></a>

### 2.1 Notes on the API

The API heavily imitates the API of packages. This has the nice
property that any experienced Common Lisper will take it up without
effort.

    DEFREADTABLE              -   DEFPACKAGE
    IN-READTABLE              -   IN-PACKAGE
    MERGE-READTABLES-INTO     -   USE-PACKAGE
    MAKE-READTABLE            -   MAKE-PACKAGE
    UNREGISTER-READTABLE      -   DELETE-PACKAGE
    RENAME-READTABLE          -   RENAME-PACKAGE
    FIND-READTABLE            -   FIND-PACKAGE
    READTABLE-NAME            -   PACKAGE-NAME
    LIST-ALL-NAMED-READTABLES -   LIST-ALL-PACKAGES

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40API-IDIOSYNCRASIES-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@API-IDIOSYNCRASIES%20MGL-PAX:SECTION"></a>

### 2.2 Important API idiosyncrasies

There are three major differences between the API of Named-Readtables,
and the API of packages.

- **Readtable names are symbols not strings.**

    Time has shown that the fact that packages are named by strings
    causes severe headache because of the potential of package names
    colliding with each other.

    Hence, readtables are named by symbols lest to make the
    situation worse than it already is. Consequently, readtables
    named `cl-oracle:sql-syntax` and `cl-mysql:sql-syntax` can
    happily coexist next to each other. Or, taken to an extreme,
    `scheme:syntax` and `elisp:syntax`.

    If, for example to duly signify the importance of your cool
    readtable hack, you really think it deserves a global name, you
    can always resort to keywords.

- **The inheritance is resolved statically, not dynamically.**

    A package that uses another package will have access to all the
    other package's exported symbols, even to those that will be
    added after its definition. I.e. the inheritance is resolved at
    run-time, that is dynamically.

    Unfortunately, we cannot do the same for readtables in a
    portable manner.

    Therefore, we do not talk about "using" another readtable but
    about "merging" the other readtable's definition into the
    readtable we are going to define. I.e. the inheritance is
    resolved once at definition time, that is statically.

    (Such merging can more or less be implemented portably albeit at
    a certain cost. Most of the time, this cost manifests itself at
    the time a readtable is defined, i.e. once at compile-time, so
    it may not bother you. Nonetheless, we provide extra support for
    Sbcl, ClozureCL, and AllegroCL at the moment. Patches for your
    implementation of choice are welcome, of course.)

- **[`defreadtable`][6a02] does not have compile-time effects.**

    If you define a package via [`defpackage`][9b43], you can make that
    package the currently active package for the subsequent
    compilation of the same file via [`in-package`][125e]. The same is,
    however, not true for `defreadtable` and [`in-readtable`][ee2d] for the
    following reason:

    It's unlikely that the need for special reader-macros arises for
    a problem which can be solved in just one file. Most often,
    you're going to define the reader macro functions, and set up
    the corresponding readtable in an extra file.

    If `defreadtable` had compile-time effects, you'd have to wrap
    each definition of a reader-macro function in an [`eval-when`][9c9c] to
    make its definition available at compile-time. Because that's
    simply not the common case, `defreadtable` does not have a
    compile-time effect.

    If you want to use a readtable within the same file as its
    definition, wrap the `defreadtable` and the reader-macro function
    definitions in an explicit `eval-when`.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40PREREGISTERED-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@PREREGISTERED%20MGL-PAX:SECTION"></a>

### 2.3 Preregistered Readtables

- `nil`, `:standard`, and `:common-lisp` designate the
*standard readtable*.

- `:modern` designates a *case-preserving* *standard-readtable*.

- `:current` designates the *current readtable*.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40EXAMPLES-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@EXAMPLES%20MGL-PAX:SECTION"></a>

### 2.4 Examples

```commonlisp
(defreadtable elisp:syntax
   (:merge :standard)
   (:macro-char #\? #'elisp::read-character-literal t)
   (:macro-char #\[ #'elisp::read-vector-literal t)
   ...
   (:case :preserve))

(defreadtable scheme:syntax
   (:merge :standard)
   (:macro-char #\[ #'(lambda (stream char)
                         (read-delimited-list #\] stream)))
   (:macro-char #\# :dispatch)
   (:dispatch-macro-char #\# #\t #'scheme::read-#t)
   (:dispatch-macro-char #\# #\f #'scheme::read-#f)
   ...
   (:case :preserve))

(in-readtable elisp:syntax)

...

(in-readtable scheme:syntax)

...
```

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:@REFERENCE%20MGL-PAX:SECTION"></a>

## 3 Reference

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ADEFREADTABLE-20MGL-PAX-3AMACRO-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE%20MGL-PAX:MACRO"></a>

- \[macro\] **defreadtable** *name \&body options*

    Define a new named readtable, whose name is given by the symbol `name`.
    Or, if a readtable is already registered under that name, redefine
    that one.

    The readtable can be populated using the following `options`:

    - If the first element of `options` is a string then it is associated
      with the readtable as in `(setf (documentation name 'readtable)
      docstring)`.

    - `(:merge readtable-designators+)`

        Merge the macro character definitions from the readtables
        designated into the new readtable being defined as per
        [`merge-readtables-into`][1625]. The copied options are
        `:dispatch-macro-char`, `:macro-char` and `:syntax-from`, but not
        [`readtable-case`][48f1].

        If no `:merge` clause is given, an empty readtable is used. See
        [`make-readtable`][fd4c].

    - `(:fuse readtable-designators+)`

        Like `:merge` except:

        Error conditions of type [`reader-macro-conflict`][eab7] that are signaled
        during the merge operation will be silently *continued*. It
        follows that reader macros in earlier entries will be
        overwritten by later ones. For backward compatibility, `:fuze` is
        accepted as an alias of `:fuse`.

    - `(:dispatch-macro-char macro-char sub-char function)`

        Define a new sub character `sub-char` for the dispatching macro
        character `macro-char`, per [`set-dispatch-macro-character`][5b1b]. You
        probably have to define `macro-char` as a dispatching macro
        character by the following option first.

    - `(:macro-char macro-char function [non-terminating-p])`

        Define a new macro character in the readtable, per
        [`set-macro-character`][a8c1]. If `function` is the keyword
        `:dispatch`, `macro-char` is made a dispatching macro character,
        per [`make-dispatch-macro-character`][1ee4].

    - `(:syntax-from from-readtable-designator from-char to-char)`

        Set the character syntax of `to-char` in the readtable being
        defined to the same syntax as `from-char` as per
        [`set-syntax-from-char`][3867].

    - `(:case case-mode)`

        Defines the *case sensitivity mode* of the resulting readtable.

    Any number of option clauses may appear. The options are grouped by
    their type, but in each group the order the options appeared
    textually is preserved. The following groups exist and are executed
    in the following order: `:merge` and `:fuse` (one group), `:case`,
    `:macro-char` and `:dispatch-macro-char` (one group), finally
    `:syntax-from`.

    > *Notes*:

    > - The readtable is defined at load-time. If you want to have it
    >   available at compilation time -- say to use its reader-macros in the
    >   same file as its definition -- you have to wrap the `defreadtable`
    >   form in an explicit [`eval-when`][9c9c].

    > - On redefinition, the target readtable is made empty first before
    >   it's refilled according to the clauses.

    > - `nil`, `:standard`, `:common-lisp`, `:modern`, and `:current` are
    >   [Preregistered Readtables][8115] names.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AIN-READTABLE-20MGL-PAX-3AMACRO-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:IN-READTABLE%20MGL-PAX:MACRO"></a>

- \[macro\] **in-readtable** *name*

    Set [`*readtable*`][b79a] to the readtable referred to by the symbol `name` and
    return the readtable. This may signal [`readtable-does-not-exist`][02bf].

    - Everything `in-readtable` does is also performed at [compile time][27c6] if the call appears as a [top level form][0f52].

    - The effects of `in-readtable` are file-local since both [`compile-file`][0b69]
      and [`load`][b5ec] rebind `*readtable*`.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMAKE-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:MAKE-READTABLE%20FUNCTION"></a>

- \[function\] **make-readtable** *\&optional name \&key merge*

    Creates and returns a new readtable under the specified
    `name`.

    `merge` takes a list of [`named-readtable-designator`][4e61]s and specifies the
    readtables the new readtable is created from. (See the `:merge` clause
    of [`defreadtable`][6a02] for details.)

    If `merge` is `nil`, an empty readtable is used instead.

    If `name` is not given, an anonymous empty readtable is returned.

    > *Note:* An empty readtable is a readtable where each character's
    > syntax is the same as in the *standard readtable* except that each
    > macro character has been made a constituent. Basically: whitespace
    > stays whitespace, everything else is constituent.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMERGE-READTABLES-INTO-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:MERGE-READTABLES-INTO%20FUNCTION"></a>

- \[function\] **merge-readtables-into** *result-readtable \&rest named-readtables*

    Copy macro character definitions of each readtable in
    `named-readtables` into `result-readtable`.

    If a macro character appears in more than one of the readtables,
    i.e. if a conflict is discovered during the merge, an error of type
    [`reader-macro-conflict`][eab7] is signaled.

    The copied options are `:dispatch-macro-char`, `:macro-char` and
    `:syntax-from`, but not [`readtable-case`][48f1].

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AFIND-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:FIND-READTABLE%20FUNCTION"></a>

- \[function\] **find-readtable** *name*

    Looks for the readtable specified by `name` and returns it if it is
    found. Returns `nil` otherwise.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AENSURE-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:ENSURE-READTABLE%20FUNCTION"></a>

- \[function\] **ensure-readtable** *name \&optional default*

    Looks up the readtable specified by `name` and returns it if it's found.
    If it is not found, it registers the readtable designated by `default`
    under the name represented by `name`; or if no default argument is
    given, it signals an error of type [`readtable-does-not-exist`][02bf]
    instead.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ARENAME-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:RENAME-READTABLE%20FUNCTION"></a>

- \[function\] **rename-readtable** *old-name new-name*

    Replaces the associated name of the readtable designated by
    `old-name` with `new-name`. If a readtable is already registered under
    `new-name`, an error of type [`readtable-does-already-exist`][78ad] is
    signaled.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-NAME-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:READTABLE-NAME%20FUNCTION"></a>

- \[function\] **readtable-name** *named-readtable*

    Returns the name of the readtable designated by `named-readtable`,
    or `nil`.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREGISTER-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:REGISTER-READTABLE%20FUNCTION"></a>

- \[function\] **register-readtable** *name readtable*

    Associate `readtable` with `name`. Returns the readtable.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AUNREGISTER-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:UNREGISTER-READTABLE%20FUNCTION"></a>

- \[function\] **unregister-readtable** *named-readtable*

    Remove the association of `named-readtable`. Returns `t` if successfull,
    `nil` otherwise.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ACOPY-NAMED-READTABLE-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:COPY-NAMED-READTABLE%20FUNCTION"></a>

- \[function\] **copy-named-readtable** *named-readtable*

    Like [`copy-readtable`][6d9f] but takes a [`named-readtable-designator`][4e61] as argument.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ALIST-ALL-NAMED-READTABLES-20FUNCTION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:LIST-ALL-NAMED-READTABLES%20FUNCTION"></a>

- \[function\] **list-all-named-readtables**

    Returns a list of all registered readtables. The returned list is
    guaranteed to be fresh, but may contain duplicates.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ANAMED-READTABLE-DESIGNATOR-20TYPE-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:NAMED-READTABLE-DESIGNATOR%20TYPE"></a>

- \[type\] **named-readtable-designator**

    Either a symbol or a readtable itself.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-ERROR-20CONDITION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:READTABLE-ERROR%20CONDITION"></a>

- \[condition\] **readtable-error** *[error][d162]*

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADER-MACRO-CONFLICT-20CONDITION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:READER-MACRO-CONFLICT%20CONDITION"></a>

- \[condition\] **reader-macro-conflict** *[readtable-error][371c]*

    Continuable.

    This condition is signaled during the merge process if a reader
    macro (be it a macro character or the sub character of a dispatch
    macro character) is present in the both source and the target
    readtable and the two respective reader macro functions differ.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-ALREADY-EXIST-20CONDITION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-ALREADY-EXIST%20CONDITION"></a>

- \[condition\] **readtable-does-already-exist** *[readtable-error][371c]*

    Continuable.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-NOT-EXIST-20CONDITION-29"></a>
<a id="EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-NOT-EXIST%20CONDITION"></a>

- \[condition\] **readtable-does-not-exist** *[readtable-error][371c]*

[02bf]: #EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-NOT-EXIST%20CONDITION "EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-NOT-EXIST CONDITION"

[0b69]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp_fi.htm "COMPILE-FILE (MGL-PAX:CLHS FUNCTION)"

[0f52]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#top_level_form "\"top level form\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"

[125e]: http://www.lispworks.com/documentation/HyperSpec/Body/m_in_pkg.htm "IN-PACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"

[1625]: #EDITOR-HINTS.NAMED-READTABLES:MERGE-READTABLES-INTO%20FUNCTION "EDITOR-HINTS.NAMED-READTABLES:MERGE-READTABLES-INTO FUNCTION"

[1ee4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_dis.htm "MAKE-DISPATCH-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"

[27c6]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#compile_time "\"compile time\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"

[371c]: #EDITOR-HINTS.NAMED-READTABLES:READTABLE-ERROR%20CONDITION "EDITOR-HINTS.NAMED-READTABLES:READTABLE-ERROR CONDITION"

[3867]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_sy.htm "SET-SYNTAX-FROM-CHAR (MGL-PAX:CLHS FUNCTION)"

[48f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE (MGL-PAX:CLHS FUNCTION)"

[4e14]: #EDITOR-HINTS.NAMED-READTABLES:@REFERENCE%20MGL-PAX:SECTION "Reference"

[4e61]: #EDITOR-HINTS.NAMED-READTABLES:NAMED-READTABLE-DESIGNATOR%20TYPE "EDITOR-HINTS.NAMED-READTABLES:NAMED-READTABLE-DESIGNATOR TYPE"

[5b1b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set__1.htm "SET-DISPATCH-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"

[6a02]: #EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE%20MGL-PAX:MACRO "EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE MGL-PAX:MACRO"

[6b83]: #EDITOR-HINTS.NAMED-READTABLES:@API-IDIOSYNCRASIES%20MGL-PAX:SECTION "Important API idiosyncrasies"

[6d9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_rdt.htm "COPY-READTABLE (MGL-PAX:CLHS FUNCTION)"

[7326]: #EDITOR-HINTS.NAMED-READTABLES:@API-NOTES%20MGL-PAX:SECTION "Notes on the API"

[76be]: #EDITOR-HINTS.NAMED-READTABLES:@ACKNOWLEDGEMENTS%20MGL-PAX:SECTION "Acknowledgements"

[7831]: #EDITOR-HINTS.NAMED-READTABLES:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"

[78ad]: #EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-ALREADY-EXIST%20CONDITION "EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-ALREADY-EXIST CONDITION"

[8115]: #EDITOR-HINTS.NAMED-READTABLES:@PREREGISTERED%20MGL-PAX:SECTION "Preregistered Readtables"

[911e]: #EDITOR-HINTS.NAMED-READTABLES:@EXAMPLES%20MGL-PAX:SECTION "Examples"

[9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"

[9c9c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm "EVAL-WHEN (MGL-PAX:CLHS MGL-PAX:MACRO)"

[a8c1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_ma.htm "SET-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"

[b13e]: #EDITOR-HINTS.NAMED-READTABLES:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION "Links and Systems"

[b5ec]: http://www.lispworks.com/documentation/HyperSpec/Body/f_load.htm "LOAD (MGL-PAX:CLHS FUNCTION)"

[b79a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* (MGL-PAX:CLHS VARIABLE)"

[d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"

[eab7]: #EDITOR-HINTS.NAMED-READTABLES:READER-MACRO-CONFLICT%20CONDITION "EDITOR-HINTS.NAMED-READTABLES:READER-MACRO-CONFLICT CONDITION"

[ee2d]: #EDITOR-HINTS.NAMED-READTABLES:IN-READTABLE%20MGL-PAX:MACRO "EDITOR-HINTS.NAMED-READTABLES:IN-READTABLE MGL-PAX:MACRO"

[f7fa]: #EDITOR-HINTS.NAMED-READTABLES:@OVERVIEW%20MGL-PAX:SECTION "Overview"

[fd4c]: #EDITOR-HINTS.NAMED-READTABLES:MAKE-READTABLE%20FUNCTION "EDITOR-HINTS.NAMED-READTABLES:MAKE-READTABLE FUNCTION"
