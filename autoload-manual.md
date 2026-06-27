<a id="x-28AUTOLOAD-3A-40AUTOLOAD-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@AUTOLOAD-MANUAL%20MGL-PAX:SECTION"></a>

# Autoload Manual

## Table of Contents

- [1 Links and Systems][d60b]
- [2 Introduction][471f]
- [3 Basics][fa90]
    - [3.1 Loading Systems][ddfa]
    - [3.2 Conditions][f43d]
    - [3.3 Functions][4b04]
    - [3.4 Classes][38fe]
    - [3.5 Variables][f490]
    - [3.6 Packages][643f]
- [4 ASDF Integration][0c5c]
    - [4.1 Automatically Generating Loaddefs][c1d4]

###### \[in package AUTOLOAD\]
<a id="x-28AUTOLOAD-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION"></a>

## 1 Links and Systems

The official repository is <https://github.com/melisgl/autoload>, and
this document in available in various formats on
<https://fixnum.com> for the latest version.)
for the latest version.

<a id="x-28-22autoload-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22autoload%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"autoload"**

    - _Version:_ 0.1.0
    - _Description:_ An ASDF autoloading facility. See
        [Autoload Manual][6caf].
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <https://github.com/melisgl/autoload>
    - _Bug tracker:_ <https://github.com/melisgl/autoload/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/autoload.git)
    - *Depends on:* closer-mop, mgl-pax-bootstrap, trivial-indent

<a id="x-28-22autoload-doc-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22autoload-doc%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"autoload-doc"**

    - _Description:_ Parts of the Autoload library that depend on
        [`mgl-pax`][6fdb] are in this system to avoid the circular
        dependencies that would arise because [`mgl-pax`][6fdb]
        depends on [`autoload`][5968]. Note that
        [`mgl-pax/navigate`][f155] and [`mgl-pax/document`][4bb8] depend on this system, which renders most of this an
        implementation detail.
    - *Depends on:* [autoload][5968], [dref][021a], [mgl-pax][6fdb], [named-readtables][718a], pythonic-string-reader

<a id="x-28AUTOLOAD-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@INTRODUCTION%20MGL-PAX:SECTION"></a>

## 2 Introduction

Autoload was factored out of [PAX][2415], so let's
explore the motivation in that context. PAX is a large system with
substantial dependencies, but what you actually need in deployment
is tiny. The rest is for interactive use and documentation
generation. By autoloading the optional parts, we can keep
deployment size down and avoid annoyingly long compile times caused
by code for unused features (and their transitive dependencies).

Users could load the relevant systems manually when they need them,
but that would be quite disruptive and they would need to remember
what system to load. Also, code whose dependencies may not be loaded
needs to jump through hoops (e.g. [`funcall`][03c7] + [`intern`][b4f0]) or rely on
forward declarations. Autoload takes care of these issues: you can
factor out code that isn't always necessary into some
sub-asdf-system along with its dependencies. This removes the
pressure to drop dependencies just to keep deployments lean and
spares some users the long wait of compiling
`ironclad`.

More abstractly, libraries often choose to limit dependencies,
even if it means sacrificing features or duplicating code, to
minimize

- compilation time,

- memory usage in deployment, and

- the risk of breakage through dependencies.

Autoload mitigates these issues by loading heavy dependencies on
demand. The core idea is

```
(defmacro autoload (name asdf-system)
  `(defun ,name (&rest args)
     (asdf:load-system ,asdf-system)
     (apply ',name args)))
```

Suppose we have a library called `my-lib` that autoloads
`my-lib/full`. In `my-lib`, we could use `autoload`
as

```
(autoload foo "my-lib/full")
```

and have

```
(defun foo (x)
  "doc"
  (1+ x))
```

in `my-lib/full`.

However, manually keeping the [loaddef][e4a5]s (e.g. the `autoload` form
above) in sync with the definitions is fragile, so we introduce the
[`defun/auto`][a825] [autodef][af1d] to mark autoloaded functions in the
`my-lib/full` system:

```
(defun/auto foo (x)
  "doc"
  (1+ x))
```

<br>

**[ASDF Integration][0c5c]**

To [generate loaddefs][c1d4], we add a few lines to
the system definitions:

```
(asdf:defsystem "my-lib"
  :defsystem-depends-on ("autoload")
  :class "autoload:autoload-system"
  :auto-depends-on ("my-lib/full")
  :auto-loaddefs "loaddefs.lisp"
  :components ((:file "loaddefs")
               ...))
```

```
(asdf:defsystem "my-lib/full"
  :defsystem-depends-on ("autoload")
  :class "autoload:autoload-system"
  :components (...))
```

Then, the loaddefs can be extracted:

```
(extract-loaddefs "my-lib")
=> ((autoload foo "my-lib/full" :arglist "(x)" :docstring "doc"))
```

This is implemented by loading the [`:auto-depends-on`][9b08] of `my-lib` and
recording `defun/auto`s. [`extract-loaddefs`][dd7e] is a low-level utility used
by [`record-loaddefs`][e90c], which writes its results to the
system's [`:auto-loaddefs`][0724], `"loaddefs.lisp"` in the above example. So,
all we need to do is call `record-loaddefs` to regenerate the loaddefs
file:

```
(record-loaddefs "my-lib")
```

To prevent the loaddefs file from getting out of sync with the
definitions, `asdf:test-system` calls [`check-loaddefs`][451b] by default.

ASDF, and by extension [Quicklisp][ae25], doesn't know about the declared
[`:auto-depends-on`][9b08], so `(ql:quickload "my-lib")` does not install the
autoloaded dependencies. They can be installed manually with

```
(autodeps "my-lib" :installer #'ql:quickload)
```

If all the autoloaded dependencies are installed, one can eagerly
load them to ensure that autoloading is not triggered later (e.g.
in deployment):

```
(map nil #'asdf:load-system (autodeps "my-lib"))
```

<br>

**Other Features**

Autoloading is not only for [Functions][4b04]:

- Autoloading [Classes][38fe] at the time of their first instantiation is
  supported.

- [Variables][f490] can be marked for early definition and have their
  initial values assigned if the initial value form provably doesn't
  have dependencies. If that's not the case, subject to platform
  support, the definition in the loaded system injects a global
  binding even in the presence of local bindings.

- Multiple [Packages][643f] can have their final states and
  interdependencies reconstructed before loading their systems even
  if they were mutated operations like [`import`][8f46] and [`export`][0c4f].


<a id="x-28AUTOLOAD-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@BASICS%20MGL-PAX:SECTION"></a>

## 3 Basics

<a id="x-28AUTOLOAD-3A-40LOADDEF-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="AUTOLOAD:@LOADDEF%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **loaddef**

    A loaddef is a preliminary definition that serves as a stand-in
    until the fully-realized implementation is loaded. Accessing it may
    or may not [load a system][ddfa]. See
    [`loaddef-function-p`][8c12], [`loaddef-class-p`][42c5], [`loaddef-variable-p`][fd18] and
    [`loaddef-package-p`][9776].

<a id="x-28AUTOLOAD-3A-40AUTODEF-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="AUTOLOAD:@AUTODEF%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **autodef**

    An autodef (e.g. `(defun/auto name ...)`) performs the job of its
    plain counterpart ([`defun`][f472]). In addition, it marks the definition (of
    `name` as a function) for [Automatically Generating Loaddefs][c1d4] and, at the time of the
    first such autodef, it signals an [`autoload-warning`][da95] if `name` has not
    been declared as a [loaddef][e4a5] (has never been [`loaddef-function-p`][8c12]). See
    [`defun/auto`][a825], [`defclass/auto`][ee20], [`defvar/auto`][3cff] and [`defpackage/auto`][aa0e].

<a id="x-28AUTOLOAD-3A-40LOADING-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@LOADING-SYSTEMS%20MGL-PAX:SECTION"></a>

### 3.1 Loading Systems



Function and class [loaddef][e4a5]s trigger the loading of `asdf:system`s.
Unlike normal ASDF dependencies (declared in `:depends-on`), autoload
dependencies (which may be declared in [`:auto-depends-on`][9b08]) are allowed
to be circular. The rules for loading are as follows.

1. It is an [`autoload-error`][a515] if loading is triggered during [compile
   time][27c6] or during a [`load`][b5ec] of either a [source file][e8f2] or a
   [compiled file][53ee]. This is to prevent infinite autoload
   recursion.

2. It is an `autoload-error` if the system does not exist.

3. The system is loaded under [`with-compilation-unit`][e7bf] `:override` `t` and
   [`with-standard-io-syntax`][39df] but with [`*print-readably*`][8aca] `nil`. Other
   non-portable measures may be taken to standardize the dynamic
   environment. Errors signalled during the load are not handled or
   resignalled by the Autoload library.

4. It is an `autoload-error` if the [loaddef][e4a5] is not replaced by a
   normal definition or deleted by the loaded system, that is, when
   it remains a [loaddef][e4a5] (e.g. in terms of [`loaddef-function-p`][8c12]).




<a id="x-28AUTOLOAD-3A-40CONDITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@CONDITIONS%20MGL-PAX:SECTION"></a>

### 3.2 Conditions

<a id="x-28AUTOLOAD-3AAUTOLOAD-ERROR-20CONDITION-29"></a>
<a id="AUTOLOAD:AUTOLOAD-ERROR%20CONDITION"></a>

- [condition] **autoload-error** *[error][d162]*

    Signalled for some failures during [Loading Systems][ddfa].

<a id="x-28AUTOLOAD-3AAUTOLOAD-WARNING-20CONDITION-29"></a>
<a id="AUTOLOAD:AUTOLOAD-WARNING%20CONDITION"></a>

- [condition] **autoload-warning** *[simple-warning][fc62]*

    See [`autoload`][7da0], [autodef][af1d] and [`:auto-depends-on`][9b08] for
    when this is signalled.

<a id="x-28AUTOLOAD-3A-40FUNCTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@FUNCTIONS%20MGL-PAX:SECTION"></a>

### 3.3 Functions

<a id="x-28AUTOLOAD-3AAUTOLOAD-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:AUTOLOAD%20MGL-PAX:MACRO"></a>

- [macro] **autoload** *name system-name &key arglist docstring*

    *This is the [loaddef][e4a5] for [autodef][af1d] [`defun/auto`][a825].*
    
    Define a function stub with `name` and return `name`. The arguments are
    not evaluated. If `name` has an [`fdefinition`][eea4] and it is not
    [`loaddef-function-p`][8c12], then this does nothing and returns `nil`.
    
    The stub first [loads][ddfa] `system-name`, then it [`apply`][d811]s
    the function `name` to the arguments originally provided to the stub.
    
    The stub is not defined at [compile time][27c6], which matches the
    required semantics of [`defun`][f472]. `name` is [`declaim`][ebea]ed with [`ftype`][05c1]
    [`function`][119e] and [`notinline`][9514].
    
    - `arglist` will be installed as the stub's arglist if specified and
      it's supported on the platform (currently only SBCL). If `arglist`
      is a string, the effective value of `arglist` is read from it. If
      the read fails, an [`autoload-warning`][da95] is signalled and processing
      continues as if `arglist` had not been provided.
    
        Arglists are for interactive purposes only. For example, they
        are shown by [SLIME autodoc][d78c] and returned by [`dref:arglist`][e6bd].
    
    - `docstring`, if non-`nil`, will be the stub's docstring. If `nil`, then
      a generic docstring that says what system it autoloads will be
      used.
    
    When `autoload` is macroexpanded during the compilation or loading of
    an [`autoload-system`][cd2d], it signals an `autoload-warning` if `system-name` is
    not among those declared in [`:auto-depends-on`][9b08].

<a id="x-28AUTOLOAD-3ALOADDEF-FUNCTION-P-20FUNCTION-29"></a>
<a id="AUTOLOAD:LOADDEF-FUNCTION-P%20FUNCTION"></a>

- [function] **loaddef-function-p** *name*

    See if an [`autoload`][7da0] for `name` was established, and since then it has
    not been redefined (e.g. with [`defun/auto`][a825], [`defun`][f472]) or made
    [`fmakunbound`][609c].

<a id="x-28AUTOLOAD-3ADEFUN-2FAUTO-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:DEFUN%2FAUTO%20MGL-PAX:MACRO"></a>

- [macro] **defun/auto** *name lambda-list &body body*

    *This is the [autodef][af1d] for the [loaddef][e4a5] [`autoload`][7da0].*
    
    Like [`defun`][f472], but also silence redefinition warnings. `name` may be of
    the form `(definer name)`. In that case, instead of `defun`, `definer`
    is used to establish the underlying function binding.
    
    **Loaddef:** The corresponding [loaddef][e4a5] is an `autoload` form.
    [`extract-loaddefs`][dd7e] with `process-arglist` `t` installs `lambda-list` as the
    `arglist`. If `process-arglist` is `nil`, then `arglist` will not be passed
    to `autoload`.

<a id="x-28AUTOLOAD-3ADEFGENERIC-2FAUTO-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:DEFGENERIC%2FAUTO%20MGL-PAX:MACRO"></a>

- [macro] **defgeneric/auto** *name lambda-list &body options*

    A shorthand for `(` [`defun/auto`][a825] `(defgeneric name) ...)`.

<a id="x-28AUTOLOAD-3A-40CLASSES-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@CLASSES%20MGL-PAX:SECTION"></a>

### 3.4 Classes

<a id="x-28AUTOLOAD-3AAUTOLOAD-CLASS-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:AUTOLOAD-CLASS%20MGL-PAX:MACRO"></a>

- [macro] **autoload-class** *class-name system-name &key docstring (metaclass 'standard-class)*

    *This is the [loaddef][e4a5] for [autodef][af1d] [`defclass/auto`][ee20].*
    
    Define a dummy class with `class-name` and arrange for `system-name` to
    be [loaded][ddfa] when the class or any of its
    subclasses are [instantiated][dddd]. Returns the
    class object. The arguments are not evaluated. If `class-name`
    [denotes][51fe] a [`class`][1f37] and it is not [`loaddef-class-p`][42c5],
    then it does nothing and returns `nil`.
    
    When `autoload-class` is macroexpanded during the compilation or
    loading of an [`autoload-system`][cd2d], it signals an [`autoload-warning`][da95] if
    `system-name` is not among those declared in [`:auto-depends-on`][9b08].
    
    - `docstring`, if non-`nil`, will be the stub's docstring. If `nil`, then
      a generic docstring that says what system it autoloads will be
      used.
    
    - `metaclass` is a symbol denoting [`standard-class`][c77f] or a subclass of it.
      Also, classes with this metaclass must be allowed to inherit from
      standard classes. In MOP terms, `closer-mop:validate-superclass`
      must return true when called with an instance of `metaclass` and an
      instance of `standard-class`.
    
    The dummy class is also defined at [compile time][27c6] to
    approximate the semantics of [`defclass`][ead6]. It has `metaclass` with a
    single superclass and no slots. These are visible through
    introspection (e.g. via `closer-mop:class-direct-superclasses`), which
    does not trigger autoloading.
    
    > *Note*: [`initialize-instance`][1466] `:around` methods specialized on a
    > subclass of `class-name` may run twice in the context of the
    > `make-instance` that triggers autoloading.

<a id="x-28AUTOLOAD-3ALOADDEF-CLASS-P-20FUNCTION-29"></a>
<a id="AUTOLOAD:LOADDEF-CLASS-P%20FUNCTION"></a>

- [function] **loaddef-class-p** *name*

    See if an [`autoload-class`][9d6b] for `name` was established, and since then
    it has not been redefined (e.g. with [`defclass/auto`][ee20], [`defclass`][ead6]) or
    deleted (with `(setf (find-class ...) nil)`). Subclasses do not
    inherit this property.

<a id="x-28AUTOLOAD-3ADEFCLASS-2FAUTO-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:DEFCLASS%2FAUTO%20MGL-PAX:MACRO"></a>

- [macro] **defclass/auto** *name direct-superclasses direct-slots &rest options*

    *This is the [autodef][af1d] for the [loaddef][e4a5] [`autoload-class`][9d6b].*
    
    Like [`defclass`][ead6]. `name` may be of the form `(definer name)`. In that
    case, instead of `defclass`, `definer` is used to establish the
    underlying class definition.
    
    **Loaddef:** The corresponding [loaddef][e4a5] is an `autoload-class` form.
    Note that the metaclass of the class `name` must already be defined
    when the loaddef is evaluated.

<a id="x-28AUTOLOAD-3A-40VARIABLES-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@VARIABLES%20MGL-PAX:SECTION"></a>

### 3.5 Variables

<a id="x-28AUTOLOAD-3ALOADDEF-VARIABLE-P-20FUNCTION-29"></a>
<a id="AUTOLOAD:LOADDEF-VARIABLE-P%20FUNCTION"></a>

- [function] **loaddef-variable-p** *name*

    See if a loaddef was [generated][c1d4] from a
    [`defvar/auto`][3cff] for `name`, but this [autodef][af1d] has not been evaluated.

<a id="x-28AUTOLOAD-3ADEFVAR-2FAUTO-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:DEFVAR%2FAUTO%20MGL-PAX:MACRO"></a>

- [macro] **defvar/auto** *var &optional val doc*

    *This is an [autodef][af1d] with no public [loaddef][e4a5]. See below.*
    
    Unlike [`defvar`][7334], this works with the *global* binding on Lisps that
    support it (currently Allegro, CCL, ECL, SBCL). This is to handle
    the case when a system that uses `defvar` with a default value is
    autoloaded while that variable is locally bound:
    
    ```common-lisp
    ;; Some base system only foreshadows *X*.
    (declaim (special *x*))
    (let ((*x* 1))
      ;; Imagine that the system that defines *X* is autoloaded here.
      (defvar/auto *x* 2)
      *x*)
    => 1
    ```
    
    **Loaddef:** The corresponding [loaddef][e4a5] is not public and must be
    [generated][c1d4]. The generated loaddef declaims
    the variable special and maybe sets its initial value and docstring.
    If the initial value form in `defvar/auto` is detected as a simple
    constant form, then it is evaluated and its value is assigned to the
    variable as in `defvar`. Simple constant forms are strings, numbers,
    characters, keywords, constants in the CL package, and [`quote`][f5d0]d nested
    lists containing any of the previous or any symbol from the `cl`
    package and other packages for which loaddefs have been generated in
    the same [`extract-loaddefs`][dd7e] call (see [`defpackage/auto`][aa0e])).
    
    In case the global binding of `var` has been set between the
    corresponding loaddef and its first autodef, `val` is evaluated for
    side effect.

<a id="x-28AUTOLOAD-3A-40PACKAGES-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@PACKAGES%20MGL-PAX:SECTION"></a>

### 3.6 Packages

<a id="x-28AUTOLOAD-3ALOADDEF-PACKAGE-P-20FUNCTION-29"></a>
<a id="AUTOLOAD:LOADDEF-PACKAGE-P%20FUNCTION"></a>

- [function] **loaddef-package-p** *name*

    See if a loaddef was [generated][c1d4] from a
    [`defpackage/auto`][aa0e] for `name`, but this [autodef][af1d] has not been evaluated
    nor has the package been deleted.

<a id="x-28AUTOLOAD-3ADEFPACKAGE-2FAUTO-20MGL-PAX-3AMACRO-29"></a>
<a id="AUTOLOAD:DEFPACKAGE%2FAUTO%20MGL-PAX:MACRO"></a>

- [macro] **defpackage/auto** *name &rest options*

    *This is an [autodef][af1d] with no public [loaddef][e4a5]. See below.*
    
    Unlike [`defpackage`][9b43], if the package is already defined,
    `defpackage/auto` extends it additively. The additivity means that
    instead of replacing the package definition or signalling errors on
    redefinition, it expands into individual package-altering operations
    such as [`shadow`][d0c4], [`use-package`][2264] and [`export`][0c4f]. This allows the package
    state to be built incrementally, but it also means that
    the `(definer name)` syntax cannot be supported. `defpackage/auto` is
    idempotent.
    
    In addition, `defpackage/auto` deviates from `defpackage` in the
    following ways.
    
    - The default `:use` list is empty.
    
    - `:size` is not supported.
    
    - Implementation-specific extensions such as `:local-nicknames` are
      not supported. Use `add-package-local-nickname` after the
      `defpackage/auto`.
    
    **Loaddef:** The corresponding [loaddef][e4a5] is not public and must be
    [generated][c1d4]. As in the expansion of
    `defpackage/auto` itself, the generated operations are additive.
    
    - The generated loaddef reconstructs the package states as they
      exist after all [`:auto-depends-on`][9b08] systems are loaded. Thus, manual
      modifications after the `defpackage/auto` definition (e.g. by
      additional `export`s) are reflected in the loaddef.
    
    - To handle circular dependencies, the loaddefs of all [autodef][af1d]
      packages and those passed in the [`packages`][1d5a] argument to
      [`extract-loaddefs`][dd7e] are generated in an interleaved manner. First,
      all packages are created, then their state is reconstructed in
      phases following [`defpackage`][9b43].
    
    - Any reference to non-existent packages (e.g. in `:use`) or symbols
      in non-existent packages (e.g. `:import-from`) is silently skipped
      when the loaddef is evaluated.
    
    Instead of `defpackage/auto`, one may use, for example, `defpackage` or
    `uiop:define-package` and arrange for [Automatically Generating Loaddefs][c1d4] for the
    package by listing it in `:packages` of [`:auto-loaddefs`][0724].

<a id="x-28AUTOLOAD-3A-40ASDF-INTEGRATION-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@ASDF-INTEGRATION%20MGL-PAX:SECTION"></a>

## 4 ASDF Integration

<a id="x-28AUTOLOAD-3AAUTOLOAD-SYSTEM-20CLASS-29"></a>
<a id="AUTOLOAD:AUTOLOAD-SYSTEM%20CLASS"></a>

- [class] **autoload-system** *asdf/system:system*

    Inheriting from this class in an `asdf:defsystem`
    form enables the features documented in the reader methods. Consider
    the following example.
    
    ```
    (asdf:defsystem "my-system"
      :defsystem-depends-on ("autoload")
      :class "autoload:autoload-system"
      :auto-depends-on ("dyndep")
      :auto-loaddefs "loaddefs.lisp"
      :components ((:file "package")
                   (:file "loaddefs")
                   ...))
    ```
    
    With the above,
    
    - It is an error if an [`autoload`][7da0] refers to a system other than
      `dyndep`.
    
    - `(`[`record-loaddefs`][e90c] `"my-system")` will update
      `loaddefs.lisp`.
    
    - `(asdf:test-system "my-system")` [checks][451b] that
      `loaddefs.lisp` is up-to-date.
    
    If the package definitions are also generated with
    [`record-loaddefs`][e90c] (e.g. because there is a
    [`defpackage/auto`][aa0e] in `dyndep` or [`:auto-loaddefs`][0724] specifies
    `:packages`), then we can do without the `package.lisp` file:
    
    ```
    (asdf:defsystem "my-system"
      :defsystem-depends-on ("autoload")
      :class "autoload:autoload-system"
      :auto-depends-on ("dyndep")
      :auto-loaddefs ("loaddefs.lisp" :packages #:my-pkg)
      :components ((:file "loaddefs")
                   ...))
    ```

<a id="x-28AUTOLOAD-3AAUTOLOAD-CL-SOURCE-FILE-20CLASS-29"></a>
<a id="AUTOLOAD:AUTOLOAD-CL-SOURCE-FILE%20CLASS"></a>

- [class] **autoload-cl-source-file** *asdf/lisp-action:cl-source-file*

    This is the `:default-component-class` of
    [`autoload-system`][cd2d]. [ASDF Integration][0c5c] relies on source files belonging to
    this class. When combining autoload with another ASDF extension that
    has its own `asdf:cl-source-file` subclass, define a new class that
    inherits from both, and use that as `:default-component-class`.

<a id="x-28AUTOLOAD-3ASYSTEM-AUTO-DEPENDS-ON-20-28MGL-PAX-3AREADER-20AUTOLOAD-3AAUTOLOAD-SYSTEM-29-29"></a>
<a id="AUTOLOAD:SYSTEM-AUTO-DEPENDS-ON%20%28MGL-PAX:READER%20AUTOLOAD:AUTOLOAD-SYSTEM%29"></a>

- [reader] **system-auto-depends-on** *[autoload-system][cd2d] (:auto-depends-on = nil)*

    This is the list of the names of systems that this
    system may autoload. The names are canonicalized with
    `asdf:coerce-name`. It is an [`autoload-warning`][da95] if a [loaddef][e4a5] refers
    to a system not listed here. This is also used by
    [`extract-loaddefs`][dd7e] and affects the checks performed by
    [Loading Systems][ddfa].

<a id="x-28AUTOLOAD-3ASYSTEM-AUTO-LOADDEFS-20-28MGL-PAX-3AREADER-20AUTOLOAD-3AAUTOLOAD-SYSTEM-29-29"></a>
<a id="AUTOLOAD:SYSTEM-AUTO-LOADDEFS%20%28MGL-PAX:READER%20AUTOLOAD:AUTOLOAD-SYSTEM%29"></a>

- [reader] **system-auto-loaddefs** *[autoload-system][cd2d] (:auto-loaddefs = nil)*

    When non-`nil`, this specifies arguments for
    [Automatically Generating Loaddefs][c1d4]. It may be a single pathname designator or a
    list of the form
    
        (loaddefs-file &key (process-arglist t) (process-docstring t)
                            packages (test t))
    
    - `loaddefs-file` designates the pathname where [`record-loaddefs`][e90c] writes the [extracted loaddefs][dd7e].
      The pathname is relative to `asdf:system-source-directory` of
      `system` and is [`open`][6547]ed with `:if-exists` `:supersede`.
    
    - `process-arglist`, `process-docstring` and `packages`
      are passed on by `record-loaddefs` to `extract-loaddefs`.
    
    - If `test`, then [`check-loaddefs`][451b] is run by `asdf:test-system`.
    
    Conditions signalled while ASDF is compiling or loading the file
    given have a [`record-loaddefs`][3d01] restart.

<a id="x-28AUTOLOAD-3AAUTODEPS-20FUNCTION-29"></a>
<a id="AUTOLOAD:AUTODEPS%20FUNCTION"></a>

- [function] **autodeps** *system &key (cross-autoloaded t) installer*

    Return the list of system names that may be autoloaded by `system` or
    any of its direct or indirect dependencies. This recursively visits
    systems in the dependency tree, traversing both normal (`:depends-on`)
    and autoloaded ([`:auto-depends-on`][9b08]) dependencies. It works even if
    `system` is not an [`autoload-system`][cd2d].
    
    - `cross-autoloaded` controls whether systems only reachable from
      `system` via intermediate autoloaded dependencies are visited. Thus,
      if `cross-autoloaded` is `nil`, then the returned list is the first
      boundary of autoloaded systems.
    
    - If `installer` is non-`nil`, it is called when an autoloaded system
      that is not installed (i.e. `asdf:find-system` fails) is visited.
      `installer` is passed a single argument, the name of the system to
      be installed. It may or may not install the system.
    
    If an autoloaded system is not installed (i.e. `asdf:find-system`
    fails, even after `installer` had a chance), then its dependencies are
    unknown and cannot be traversed. Note that autoloaded systems that
    are not installed are still visited and included in the returned
    list.
    
    The following example makes sure that all autoloaded dependencies
    (direct or indirect) of `my-system` are installed:
    
        (autodeps "my-system" :installer #'ql:quickload)

<a id="x-28AUTOLOAD-3A-40AUTOMATIC-LOADDEFS-20MGL-PAX-3ASECTION-29"></a>
<a id="AUTOLOAD:@AUTOMATIC-LOADDEFS%20MGL-PAX:SECTION"></a>

### 4.1 Automatically Generating Loaddefs

<a id="x-28AUTOLOAD-3AEXTRACT-LOADDEFS-20FUNCTION-29"></a>
<a id="AUTOLOAD:EXTRACT-LOADDEFS%20FUNCTION"></a>

- [function] **extract-loaddefs** *system &key (process-arglist t) (process-docstring t) packages*

    List the [loaddef][e4a5] forms of the [autodef][af1d] definitions in
    [`:auto-depends-on`][9b08] of `system`.
    
    There is rarely a need to call this function directly, as
    [`record-loaddefs`][e90c] and [`check-loaddefs`][451b] provide
    [ASDF Integration][0c5c].
    
    > *Note*: This is an expensive operation, as it loads or reloads the
    > direct dependencies listed in `:auto-depends-on` one by one with
    > `asdf:load-system` `:force` `t` to find the [autodef][af1d]s. As a side-effect,
    > this can later cause spurious recompilation of systems that depend
    > on the force-loaded systems.
    
    See the individual [autodef][af1d]s for descriptions of the generated
    loaddefs.
    
    - If `process-docstring`, then the docstrings extracted from [autodef][af1d]
      definitions will be associated with the definition.
    
    Note that if a definition is not made with an [autodef][af1d], then
    `extract-loaddefs` will not detect it. For such functions, [loaddef][e4a5]s
    must be written manually.

<a id="x-28AUTOLOAD-3AWRITE-LOADDEFS-20FUNCTION-29"></a>
<a id="AUTOLOAD:WRITE-LOADDEFS%20FUNCTION"></a>

- [function] **write-loaddefs** *loaddefs stream*

    Write `loaddefs` to `stream` so they can be [`load`][b5ec]ed or included in an
    `asdf:defsystem`.

<a id="x-28AUTOLOAD-3ARECORD-LOADDEFS-20FUNCTION-29"></a>
<a id="AUTOLOAD:RECORD-LOADDEFS%20FUNCTION"></a>

- [function] **record-loaddefs** *system*

    [`extract-loaddefs`][dd7e] from `system` and [`write-loaddefs`][6d25]. The arguments of
    these functions are taken from `system`'s [`:auto-loaddefs`][0724].
    
    As `extract-loaddefs` loads the direct autoloaded dependencies,
    compiler warnings (e.g. about undefined specials and functions) may
    occur that go away once the generated loaddefs are in place. The
    easiest way to trigger this is to call `record-loaddefs` before these
    dependencies have been loaded. In this case, temporarily emptying
    the loaddefs file and fixing these warnings is recommended.
    
    `record-loaddefs` may also be used as a [condition handler][ea6a], in
    which case it invokes the [`record-loaddefs`][3d01] restart.

<a id="x-28AUTOLOAD-3ACHECK-LOADDEFS-20FUNCTION-29"></a>
<a id="AUTOLOAD:CHECK-LOADDEFS%20FUNCTION"></a>

- [function] **check-loaddefs** *system &key (errorp t)*

    In the [`autoload-system`][cd2d] `system`, check that both recorded and manual
    autoload declarations are correct.
    
    - If [`:auto-loaddefs`][0724] is specified, check that the file generated by
      [`record-loaddefs`][e90c] is up-to-date.
    
    - Check that all manual (non-generated) [loaddef][e4a5]s in `system` are
      resolved (e.g. no longer [`loaddef-function-p`][8c12]) by loading
      [`:auto-depends-on`][9b08].
    
    If `errorp`, then signal an error if a check fails or the loaddefs
    file cannot be read. If [`:auto-loaddefs`][0724] is specified, then the
    [`record-loaddefs`][3d01] restart is provided.
    
    If `errorp` is `nil`, then instead of signalling an error, return `nil`.
    
    This function is called automatically by `asdf:test-op` on an
    `autoload-system` method if [`:auto-loaddefs`][0724] has `:test` `t`.

<a id="x-28AUTOLOAD-3ARECORD-LOADDEFS-20RESTART-29"></a>
<a id="AUTOLOAD:RECORD-LOADDEFS%20RESTART"></a>

- [restart] **record-loaddefs**

    Provided by [`check-loaddefs`][451b] and also when the compilation of the
    loaddefs file declared in [`:auto-loaddefs`][0724] fails. The function
    [`record-loaddefs`][e90c] can be used as a condition handler to invoke this
    restart.

  [021a]: dref-manual.md#%22dref%22%20ASDF%2FSYSTEM:SYSTEM "\"dref\" ASDF/SYSTEM:SYSTEM"
  [03c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL (MGL-PAX:CLHS FUNCTION)"
  [05c1]: http://www.lispworks.com/documentation/HyperSpec/Body/d_ftype.htm "FTYPE (MGL-PAX:CLHS DECLARATION)"
  [0724]: #AUTOLOAD:SYSTEM-AUTO-LOADDEFS%20%28MGL-PAX:READER%20AUTOLOAD:AUTOLOAD-SYSTEM%29 "AUTOLOAD:SYSTEM-AUTO-LOADDEFS (MGL-PAX:READER AUTOLOAD:AUTOLOAD-SYSTEM)"
  [0c4f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT (MGL-PAX:CLHS FUNCTION)"
  [0c5c]: #AUTOLOAD:@ASDF-INTEGRATION%20MGL-PAX:SECTION "ASDF Integration"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [1466]: http://www.lispworks.com/documentation/HyperSpec/Body/f_init_i.htm "INITIALIZE-INSTANCE (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [2264]: http://www.lispworks.com/documentation/HyperSpec/Body/f_use_pk.htm "USE-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [2415]: pax-manual.md "PAX Manual"
  [27c6]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#compile_time "\"compile time\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [38fe]: #AUTOLOAD:@CLASSES%20MGL-PAX:SECTION "Classes"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3cff]: #AUTOLOAD:DEFVAR%2FAUTO%20MGL-PAX:MACRO "AUTOLOAD:DEFVAR/AUTO MGL-PAX:MACRO"
  [3d01]: #AUTOLOAD:RECORD-LOADDEFS%20RESTART "AUTOLOAD:RECORD-LOADDEFS RESTART"
  [42c5]: #AUTOLOAD:LOADDEF-CLASS-P%20FUNCTION "AUTOLOAD:LOADDEF-CLASS-P FUNCTION"
  [451b]: #AUTOLOAD:CHECK-LOADDEFS%20FUNCTION "AUTOLOAD:CHECK-LOADDEFS FUNCTION"
  [471f]: #AUTOLOAD:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [4b04]: #AUTOLOAD:@FUNCTIONS%20MGL-PAX:SECTION "Functions"
  [4bb8]: pax-manual.md#%22mgl-pax%2Fdocument%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/document\" ASDF/SYSTEM:SYSTEM"
  [51fe]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_c.htm "FIND-CLASS (MGL-PAX:CLHS FUNCTION)"
  [53ee]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#compiled_file "\"compiled file\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [5968]: #%22autoload%22%20ASDF%2FSYSTEM:SYSTEM "\"autoload\" ASDF/SYSTEM:SYSTEM"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [643f]: #AUTOLOAD:@PACKAGES%20MGL-PAX:SECTION "Packages"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [6caf]: autoload-manual.md "Autoload Manual"
  [6d25]: #AUTOLOAD:WRITE-LOADDEFS%20FUNCTION "AUTOLOAD:WRITE-LOADDEFS FUNCTION"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [718a]: named-readtables-manual.md#%22named-readtables%22%20ASDF%2FSYSTEM:SYSTEM "\"named-readtables\" ASDF/SYSTEM:SYSTEM"
  [7334]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFVAR (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [7da0]: #AUTOLOAD:AUTOLOAD%20MGL-PAX:MACRO "AUTOLOAD:AUTOLOAD MGL-PAX:MACRO"
  [8aca]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rda.htm "*PRINT-READABLY* (MGL-PAX:CLHS VARIABLE)"
  [8c12]: #AUTOLOAD:LOADDEF-FUNCTION-P%20FUNCTION "AUTOLOAD:LOADDEF-FUNCTION-P FUNCTION"
  [8f46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_import.htm "IMPORT (MGL-PAX:CLHS FUNCTION)"
  [9514]: http://www.lispworks.com/documentation/HyperSpec/Body/d_inline.htm "NOTINLINE (MGL-PAX:CLHS DECLARATION)"
  [9776]: #AUTOLOAD:LOADDEF-PACKAGE-P%20FUNCTION "AUTOLOAD:LOADDEF-PACKAGE-P FUNCTION"
  [9b08]: #AUTOLOAD:SYSTEM-AUTO-DEPENDS-ON%20%28MGL-PAX:READER%20AUTOLOAD:AUTOLOAD-SYSTEM%29 "AUTOLOAD:SYSTEM-AUTO-DEPENDS-ON (MGL-PAX:READER AUTOLOAD:AUTOLOAD-SYSTEM)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9d6b]: #AUTOLOAD:AUTOLOAD-CLASS%20MGL-PAX:MACRO "AUTOLOAD:AUTOLOAD-CLASS MGL-PAX:MACRO"
  [a515]: #AUTOLOAD:AUTOLOAD-ERROR%20CONDITION "AUTOLOAD:AUTOLOAD-ERROR CONDITION"
  [a825]: #AUTOLOAD:DEFUN%2FAUTO%20MGL-PAX:MACRO "AUTOLOAD:DEFUN/AUTO MGL-PAX:MACRO"
  [aa0e]: #AUTOLOAD:DEFPACKAGE%2FAUTO%20MGL-PAX:MACRO "AUTOLOAD:DEFPACKAGE/AUTO MGL-PAX:MACRO"
  [ae25]: https://www.quicklisp.org/ "Quicklisp"
  [af1d]: #AUTOLOAD:@AUTODEF%20MGL-PAX:GLOSSARY-TERM "autodef"
  [b4f0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN (MGL-PAX:CLHS FUNCTION)"
  [b5ec]: http://www.lispworks.com/documentation/HyperSpec/Body/f_load.htm "LOAD (MGL-PAX:CLHS FUNCTION)"
  [c1d4]: #AUTOLOAD:@AUTOMATIC-LOADDEFS%20MGL-PAX:SECTION "Automatically Generating Loaddefs"
  [c77f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_std_cl.htm "STANDARD-CLASS (MGL-PAX:CLHS CLASS)"
  [cd2d]: #AUTOLOAD:AUTOLOAD-SYSTEM%20CLASS "AUTOLOAD:AUTOLOAD-SYSTEM CLASS"
  [d0c4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_shadow.htm "SHADOW (MGL-PAX:CLHS FUNCTION)"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d60b]: #AUTOLOAD:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION "Links and Systems"
  [d78c]: https://slime.common-lisp.dev/doc/html/slime_002dautodoc_002dmode.html#slime_002dautodoc_002dmode "SLIME autodoc"
  [d811]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apply.htm "APPLY (MGL-PAX:CLHS FUNCTION)"
  [da95]: #AUTOLOAD:AUTOLOAD-WARNING%20CONDITION "AUTOLOAD:AUTOLOAD-WARNING CONDITION"
  [dd7e]: #AUTOLOAD:EXTRACT-LOADDEFS%20FUNCTION "AUTOLOAD:EXTRACT-LOADDEFS FUNCTION"
  [dddd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm "MAKE-INSTANCE (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [ddfa]: #AUTOLOAD:@LOADING-SYSTEMS%20MGL-PAX:SECTION "Loading Systems"
  [e4a5]: #AUTOLOAD:@LOADDEF%20MGL-PAX:GLOSSARY-TERM "loaddef"
  [e6bd]: dref-manual.md#DREF:ARGLIST%20FUNCTION "DREF:ARGLIST FUNCTION"
  [e7bf]: sbcl-manual.md#WITH-COMPILATION-UNIT%20MGL-PAX:MACRO "WITH-COMPILATION-UNIT MGL-PAX:MACRO"
  [e8f2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#source_file "\"source file\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [e90c]: #AUTOLOAD:RECORD-LOADDEFS%20FUNCTION "AUTOLOAD:RECORD-LOADDEFS FUNCTION"
  [ea6a]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#condition_handler "\"condition handler\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [ead6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm "DEFCLASS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ebea]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ee20]: #AUTOLOAD:DEFCLASS%2FAUTO%20MGL-PAX:MACRO "AUTOLOAD:DEFCLASS/AUTO MGL-PAX:MACRO"
  [eea4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm "FDEFINITION (MGL-PAX:CLHS FUNCTION)"
  [f155]: pax-manual.md#%22mgl-pax%2Fnavigate%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/navigate\" ASDF/SYSTEM:SYSTEM"
  [f43d]: #AUTOLOAD:@CONDITIONS%20MGL-PAX:SECTION "Conditions"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f490]: #AUTOLOAD:@VARIABLES%20MGL-PAX:SECTION "Variables"
  [f5d0]: http://www.lispworks.com/documentation/HyperSpec/Body/s_quote.htm "QUOTE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [fa90]: #AUTOLOAD:@BASICS%20MGL-PAX:SECTION "Basics"
  [fc62]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_wa.htm "SIMPLE-WARNING (MGL-PAX:CLHS CONDITION)"
  [fd18]: #AUTOLOAD:LOADDEF-VARIABLE-P%20FUNCTION "AUTOLOAD:LOADDEF-VARIABLE-P FUNCTION"
