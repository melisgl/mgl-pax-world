<a id="x-28MGL-PAX-3A-40PAX-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-MANUAL%20MGL-PAX:SECTION"></a>

# PAX Manual

## Table of Contents

- [1 Introduction][685e]
- [2 Links and Systems][ba90]
- [3 Emacs Setup][8541]
    - [3.1 Functionality Provided][d4a9]
    - [3.2 Installing from Quicklisp][f3f4]
    - [3.3 Loading PAX][d3fc]
    - [3.4 Setting up Keys][cfab]
- [4 Background][f74b]
- [5 Basics][94c7]
- [6 PAX Locatives][292a]
- [7 Navigating Sources in Emacs][3386]
    - [7.1 `M-.` Defaulting][460e]
    - [7.2 `M-.` Prompting][ed46]
        - [7.2.1 `M-.` Minibuffer Syntax][8106]
        - [7.2.2 `M-.` Completion][e444]
- [8 Generating Documentation][2c93]
    - [8.1 The `document` Function][dc0a]
        - [8.1.1 `documentable`][0702]
        - [8.1.2 Return Values][7dc7]
        - [8.1.3 `pages`][9c7d]
        - [8.1.4 Package and Readtable][ab7e]
    - [8.2 Browsing Live Documentation][a595]
        - [8.2.1 Browsing with w3m][83d5]
        - [8.2.2 Browsing with Other Browsers][c434]
        - [8.2.3 Apropos][b7fc]
        - [8.2.4 PAX Live Home Page][9d50]
    - [8.3 Markdown Support][c2d3]
        - [8.3.1 Markdown in Docstrings][7bf5]
        - [8.3.2 Markdown in Titles][165c]
        - [8.3.3 Syntax Highlighting][bc83]
        - [8.3.4 MathJax][a17d]
    - [8.4 Codification][f1ab]
    - [8.5 Linking][19e3]
        - [8.5.1 Reflink][cbc4]
        - [8.5.2 Autolink][ec7a]
        - [8.5.3 Linking to the HyperSpec][7cc3]
        - [8.5.4 Linking to Sections][22c2]
        - [8.5.5 Filtering Links][b2e4]
        - [8.5.6 Link Format][c0d2]
    - [8.6 Local Definition][9db9]
    - [8.7 Overview of Escaping][2634]
    - [8.8 Indexing][adf8]
        - [8.8.1 Indexing Definitions][34d3]
        - [8.8.2 Indexing Concepts][c001]
        - [8.8.3 Configuring Indices][b0b0]
    - [8.9 Output Formats][8d9b]
        - [8.9.1 Plain Output][c879]
        - [8.9.2 Markdown Output][dd29]
        - [8.9.3 PDF Output][19ad]
        - [8.9.4 Dummy Output][f7e6]
    - [8.10 Documentation Generation Implementation Notes][d1ca]
    - [8.11 Utilities for Generating Documentation][1b1b]
        - [8.11.1 HTML Output][36e1]
        - [8.11.2 GitHub Workflow][dff6]
        - [8.11.3 PAX World][1281]
- [9 Transcripts][6300]
    - [9.1 Transcribing in Documentation][9bd5]
    - [9.2 Transcribing with Emacs][f5bd]
    - [9.3 Controlling the Dynamic Environment][6b59]
    - [9.4 Transcript API][9dbc]
    - [9.5 Transcript Consistency Checking][4c39]
        - [9.5.1 Finer-Grained Consistency Checks][6e18]
        - [9.5.2 Utilities for Consistency Checking][8423]
- [10 Parsing][378f]
    - [10.1 Parsing Names][e65d]
        - [10.1.1 Raw Names in Words][f0d5]
        - [10.1.2 Names in Raw Names][016d]
    - [10.2 Parsing Locatives][ab38]
- [11 Writing Extensions][c4ce3]
    - [11.1 Adding New Locatives][54d8]
    - [11.2 Locative Aliases][0fa3]
    - [11.3 Extending `document`][574a]
    - [11.4 Sections][8a58]
    - [11.5 Glossary Terms][d1dc]

###### \[in package MGL-PAX with nicknames PAX\]
<a id="x-28MGL-PAX-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@INTRODUCTION%20MGL-PAX:SECTION"></a>

## 1 Introduction

*What if documentation really lived in the code?*

Docstrings are already there. If some narrative glued them together,
we'd be able develop and explore the code along with the
documentation due to their physical proximity. The main tool that
PAX provides for this is [`defsection`][72b4]:

```
(defsection @foo-random-manual (:title "Foo Random manual")
  "Foo Random is a random number generator library."
  (foo-random-state class)
  (uniform-random function)
  (@foo-random-examples section))
```

Like this one, sections can have docstrings and
[references][5225] to
definitions (e.g. `(uniform-random function)`). These docstrings and
references are the glue. To support interactive development, PAX

- makes [SLIME][6be7]'s [`M-.`][cb15] work with references and

- adds a documentation browser.

See [Emacs Setup][8541].

Beyond interactive workflows, [Generating Documentation][2c93] from
sections and all the referenced items in Markdown or HTML format is
also implemented.

With the simplistic tools provided, one may emphasize the narrative
as with Literate Programming, but documentation is generated from
code, not vice versa, and there is no support for chunking.

*Code is first, code must look pretty, documentation is code*.

##### Docstrings

PAX automatically recognizes and [marks up code][f1ab] with
backticks and [links][19e3] names in code to their definitions.
Take, for instance, SBCL's [`abort`][479a] function, whose docstring is
written in the usual style, uppercasing names of symbols:

```
(docstring #'abort)
=> "Transfer control to a restart named ABORT, signalling
a CONTROL-ERROR if none exists."
```

Note how in the generated documentation, `abort` is set with a
monospace font, while `control-error` is [Autolink][ec7a]ed:

- \[function\] **ABORT** *\&OPTIONAL CONDITION*

    Transfer control to a restart named `abort`, signalling
    a [`control-error`][6bc0] if none exists.

[6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR CONDITION"

The following [transcript][6300] shows the raw Markdown for
the previous example.

```
(document #'abort :format :markdown)
.. - [function] **ABORT** *&OPTIONAL CONDITION*
..
..     Transfer control to a restart named `ABORT`, signalling
..     a [`CONTROL-ERROR`][7c2c] if none exists.
..
..   [7c2c]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR (MGL-PAX:CLHS CONDITION)"
..
```

##### A Complete Example

Here is an example of how it all works together:

```
(mgl-pax:define-package :foo-random
  (:documentation "This package provides various utilities for random.
  See FOO-RANDOM:@FOO-RANDOM-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(in-package :foo-random)

(defsection @foo-random-manual (:title "Foo Random manual")
  "FOO-RANDOM is a random number generator library inspired by CL:RANDOM.
  Functions such as UNIFORM-RANDOM use *FOO-STATE* and have a
  :RANDOM-STATE keyword arg."
  (foo-random-state class)
  (state (reader foo-random-state))
  "Hey we can also print states!"
  (print-object (method (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; This is a subsection.
  (@foo-random-examples section))

(defclass foo-random-state ()
  ((state :reader state)))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
  uniform distribution."
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return a random number from a zero mean normal distribution with
  STDDEV."
  nil)

(defsection @foo-random-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
  with FOO:

  ```cl-transcript
  (values (princ :hello) (list 1 2))
  .. HELLO
  => :HELLO
  => (1 2)

  (make-instance 'foo-random-state)
  ==> #<FOO-RANDOM-STATE >
  ```")
```

Note how `(variable *foo-state*)` in the [`defsection`][72b4] form both
exports `*foo-state*` and includes its documentation in
`@foo-random-manual`. The symbols [`variable`][6c83] and
[`function`][ba62] are just two instances of [locative][7ac8]s,
which are used in `defsection` to refer to definitions tied to
symbols.

`(document @foo-random-manual)` generates fancy Markdown or HTML
output with [automatic markup][f25f] and [Autolink][ec7a]s uppercase [word][d7b0]s found in docstrings,
numbers sections, and creates a table of contents.

One can even generate documentation for different but related
libraries at the same time with the output going to different files
but with cross-page links being automatically added for symbols
mentioned in docstrings. In fact, this is what [PAX World][1281] does. See
[Generating Documentation][2c93] for some convenience functions to cover
the most common cases.

The [transcript][6300] in the code block tagged with
`cl-transcript` is automatically checked for up-to-dateness when
documentation is generated.

<a id="x-28MGL-PAX-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION"></a>

## 2 Links and Systems

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version. There is also a [PAX channel][pax-yt] on
YouTube with a couple of videos.

[pax-yt]: https://www.youtube.com/playlist?list=PLxbqYr4DvjX68AEdLky4IiHG69VJu6f5s

PAX is built on top of the [DRef library][5225] (bundled in the same repository).

- *Installation for deployment*

    The base system is [mgl-pax][6fdb]. It has very few
    dependencies and is sufficient as a dependency for systems using
    the [Basics][94c7] to add documentation. This is to keep deployed code
    small. To install only the bare minimum, with no intention of
    using [Navigating Sources in Emacs][3386], [Generating Documentation][2c93],
    [Browsing Live Documentation][a595] or using [Transcripts][6300], under
    Quicklisp for example, PAX could be installed as:

        (ql:quickload "mgl-pax")

- *Installation for development*

    The heavier dependencies are on the other systems, which
    correspond to the main functionalities provided, intended to be
    used primarily during development. To install the dependencies
    for all features under Quicklisp, do

        (ql:quickload "mgl-pax/full")

    Having thus installed the dependencies, it is enough to load the
    base system, which will autoload the other systems as necessary.


<a id="x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax"**

    - _Version:_ 0.4.12
    - _Description:_ Documentation system, browser, generator. See the
        [PAX Manual][2415].
    - _Long Description:_ The base system. See [Links and Systems][ba90].
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <http://github.com/melisgl/mgl-pax>
    - _Bug tracker:_ <https://github.com/melisgl/mgl-pax/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/mgl-pax.git)
    - *Depends on:* [autoload][5968], [dref][021a], mgl-pax-bootstrap, [named-readtables][718a], pythonic-string-reader
    - *Auto depends on:* [mgl-pax/document][4bb8], [mgl-pax/navigate][f155], [mgl-pax/transcribe][58259], [mgl-pax/web][a8c5]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Fnavigate%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax/navigate"**

    - _Description:_ Support for [Navigating Sources in Emacs][3386] via Slime's
        [`M-.`][cb15] in [MGL-PAX][2415].
    - *Depends on:* alexandria, [autoload-doc][af7b], [dref/full][0c7e], [mgl-pax][6fdb], swank(?)
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Fdocument%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax/document"**

    - _Description:_ Support for [Generating Documentation][2c93] in
        [MGL-PAX][2415].
    - *Depends on:* 3bmd, 3bmd-ext-code-blocks, 3bmd-ext-math, alexandria, [autoload-doc][af7b], colorize, md5, [mgl-pax/navigate][f155], [mgl-pax/transcribe][58259], [trivial-utf-8][d9f2]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22mgl-pax-2Fweb-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Fweb%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax/web"**

    - _Description:_ Web server for [Browsing Live Documentation][a595]
        in [MGL-PAX][2415].
    - *Depends on:* hunchentoot, [mgl-pax/document][4bb8]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Ftranscribe%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax/transcribe"**

    - _Description:_ Support for [Transcripts][6300] in
        [MGL-PAX][2415].
    - *Depends on:* alexandria, [mgl-pax/navigate][f155]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-pax/full"**

    - _Description:_ The [`mgl-pax`][6fdb] system with all features
        preloaded.
    - *Depends on:* [mgl-pax/document][4bb8], [mgl-pax/navigate][f155], [mgl-pax/transcribe][58259], [mgl-pax/web][a8c5]
    - *Defsystem depends on:* [autoload][5968]

<a id="x-28MGL-PAX-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EMACS-SETUP%20MGL-PAX:SECTION"></a>

## 3 Emacs Setup

Here is a quick recipe for setting up PAX for use via [SLIME][6be7] to
take advantage of the [conveniences on offer][d4a9].
Conversely, there is no need to do any of this just to use
[`defsection`][72b4], write docstrings and for [Generating Documentation][2c93].

If PAX was installed from [Quicklisp][1539], then evaluate this in CL to
[install][1aed] the Elisp code in a stable location:

    (mgl-pax:install-pax-elisp "~/quicklisp/")

Assuming the Elisp file is in the `~/quicklisp/` directory, add
something like this to your `.emacs`:

```elisp
(add-to-list 'load-path "~/quicklisp/")
(require 'mgl-pax)
(global-set-key (kbd "C-.") 'mgl-pax-document)
(global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
(global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
```

Alternatively, with `use-package`:

```elisp
(use-package mgl-pax
  :load-path "~/quicklisp"
  :after slime
  :demand t
  :bind (("C-." . mgl-pax-document)
         ("s-x t" . mgl-pax-transcribe-last-expression)
         ("s-x r" . mgl-pax-retranscribe-region))
  :config
  (mgl-pax-hijack-slime-doc-keys))
```

When [Browsing with Other Browsers][c434], for clicking on the locative
next to a definition to visit the corresponding source location in
Emacs, permission needs to be given:

```elisp
(setq slime-enable-evaluate-in-emacs t)
```


<a id="x-28MGL-PAX-3A-40EMACS-FUNCTIONALITY-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EMACS-FUNCTIONALITY%20MGL-PAX:SECTION"></a>

### 3.1 Functionality Provided

- For [Navigating Sources in Emacs][3386], loading `mgl-pax` extends
  `slime-edit-definitions` ([`M-.`][cb15]) by adding
  `mgl-pax-edit-definitions` to `slime-edit-definition-hooks`. There
  are no related variables to customize.

- For [Browsing Live Documentation][a595], `mgl-pax-browser-function` and
  `mgl-pax-web-server-port` can be customized in Elisp. To browse
  within Emacs, choose `w3m-browse-url` (see [w3m][7439]), and make sure
  both the w3m binary and the w3m Emacs package are installed. On
  Debian, simply install the `w3m-el` package. With other browser
  functions, a HUNCHENTOOT web server is started.

- See [Transcribing with Emacs][f5bd] for how to use the transcription
  features. There are no related variables to customize.

Independently from the Common Lisp side, the Elisp functions
`mgl-pax-hideshow-documentation` and `mgl-pax-hideshow-comments`
help focus on the code only by folding or unfolding
[`mgl-pax:defsection`][72b4], [`mgl-pax:define-glossary-term`][8ece] forms and long
strings, or comments.

<a id="x-28MGL-PAX-3A-40EMACS-QUICKLISP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EMACS-QUICKLISP%20MGL-PAX:SECTION"></a>

### 3.2 Installing from Quicklisp

If you installed PAX with Quicklisp, the location of `mgl-pax.el`
may change with updates, and you may want to copy the current
version of `mgl-pax.el` to a stable location by evaluating this in
CL:

    (mgl-pax:install-pax-elisp "~/quicklisp/")

If working from, say, a git checkout, there is no need for this
step.

<a id="x-28MGL-PAX-3AINSTALL-PAX-ELISP-20FUNCTION-29"></a>
<a id="MGL-PAX:INSTALL-PAX-ELISP%20FUNCTION"></a>

- [function] **install-pax-elisp** *target-dir*

    Install `mgl-pax.el` distributed with this package in `target-dir`.

<a id="x-28MGL-PAX-3A-40EMACS-LOADING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EMACS-LOADING%20MGL-PAX:SECTION"></a>

### 3.3 Loading PAX

Assuming the Elisp file is in the `~/quicklisp/` directory, add
something like this to your `.emacs`:

```elisp
(add-to-list 'load-path "~/quicklisp/")
(require 'mgl-pax)
```

If the Elisp variable `mgl-pax-autoload` is true (the default), then
PAX will be loaded in the connected Lisp on-demand via [SLIME][6be7].

If loading fails, `mgl-pax` will be unloaded from Emacs and any
[overridden Slime key bindings][cfab] restored.

<a id="x-28MGL-PAX-3A-40EMACS-KEYS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EMACS-KEYS%20MGL-PAX:SECTION"></a>

### 3.4 Setting up Keys

The recommended key bindings are this:

```
(global-set-key (kbd "C-.") 'mgl-pax-document)
(global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
(global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
```

The global key bindings above are global because their commands work
in any mode. If that's not desired, one may bind `C-.` locally in
all Slime related modes like this:

```elisp
(slime-bind-keys slime-parent-map nil '(("C-." mgl-pax-document)))
```

If the customizable variable `mgl-pax-hijack-slime-doc-keys` is
true, then upon loading `mgl-pax`, the following changes are made to
`slime-doc-map` (assuming it's bound to `C-c C-d`):

- `C-c C-d a`: replaces `slime-apropos` with `mgl-pax-apropos`

- `C-c C-d z`: replaces `slime-apropos-all` with `mgl-pax-apropos-all`

- `C-c C-d p`: replaces `slime-apropos-package` with `mgl-pax-apropos-package`

- `C-c C-d f`: replaces `slime-describe-function` with `mgl-pax-document`

- `C-c C-d d`: replaces `slime-describe-symbol` with
   `mgl-pax-hideshow-documentation`

- `C-c C-d c`: installs `mgl-pax-hideshow-comments`

- `C-c C-d u`: installs `mgl-pax-edit-parent-section`

Calling `mgl-pax-unhijack-slime-doc-keys` reverts these changes.

<a id="x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BACKGROUND%20MGL-PAX:SECTION"></a>

## 4 Background

As a user, I frequently run into documentation that's incomplete
and out of date, so I tend to stay in the editor and explore the
code by jumping around with [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`).
As a library author, I spend a great deal of time polishing code but
precious little writing documentation.

In fact, I rarely write anything more comprehensive than docstrings
for exported stuff. Writing docstrings feels easier than writing a
separate user manual, and they are always close at hand during
development. The drawback of this style is that users of the library
have to piece the big picture together themselves.

That's easy to solve, I thought, let's just put all the narrative
that holds docstrings together in the code and be a bit like a
Literate Programmer turned inside out. The original prototype, which
did almost everything I wanted, was this:

```
(defmacro defsection (name docstring)
  `(defun ,name () ,docstring))
```

Armed with this `defsection`, I soon found myself
organizing code following the flow of user-level documentation and
relegated comments to implementation details entirely. However, some
parts of `defsection` docstrings were just listings of
all the functions, macros and variables related to the narrative,
and this list was repeated in the [`defpackage`][9b43] form complete with
little comments that were like section names. A clear violation of
[OAOO][7d18], one of them had to go, so `defsection` got a list
of symbols to export.

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
were named by the same symbol. This did not concern exporting, of
course, but it didn't help readability. Distractingly, on such
symbols, `M-.` was popping up selection dialogs. There were two
birds to kill, and the symbol got accompanied by a type, which was
later generalized into the concept of locatives:

```
(defsection @introduction ()
  "A single line for one man ..."
  (foo class)
  (bar function))
```

After a bit of elisp hacking, `M-.` was smart enough to
disambiguate based on the locative found in the vicinity of the
symbol, and everything was good for a while.

Then, I realized that sections could refer to other sections if
there were a [`section`][672f] locative. Going down that path, I soon began to
feel the urge to generate pretty documentation as all the necessary
information was available in the `defsection` forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings, there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able figure out what's being
referred to.

I settled on [Markdown][a317] as a reasonably non-intrusive format, and a
few thousand lines later PAX was born. Since then, [locative][7ac8]s
and [reference][43bd]s were factored out into the [DRef][5225] library to let PAX focus on `M-.` and
documentation.

<a id="x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BASICS%20MGL-PAX:SECTION"></a>

## 5 Basics

Now let's examine the most important pieces.

<a id="x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFSECTION%20MGL-PAX:MACRO"></a>

- [macro] **defsection** *name (&key (package '\*package\*) (readtable '\*readtable\*) (export t) title link-title-to (discard-documentation-p \*discard-documentation-p\*) concepts) &body entries*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `name` is defined and
    is bound to a [`section`][5fac] object. By convention, section names
    start with the character `@`. See [Introduction][685e] for an example.
    
    **Entries**
    
    `entries` consists of docstrings and references in any order.
    Docstrings are arbitrary strings in Markdown format.
    
    References are [`xref`][1538]s given in the form `(name locative)`.
    For example, `(foo function)` refers to the function `foo`, `(@bar
    section)` says that `@bar` is a subsection of this
    one. `(baz (method (t t t)))` refers to the default method of the
    three argument generic function `baz`. `(foo function)` is
    equivalent to `(foo (function))`. See the DRef [Introduction][ad80]
    for more.
    
    The same name may occur in multiple references, typically with
    different [locative][7ac8]s, but this is not required.
    
    The references are not [`locate`][8f19]d until documentation is generated, so
    they may refer to things yet to be defined.
    
    **Exporting**
    
    If `export` is true (the default), `name` and the [name][88cf]s of references
    among `entries` which are [`symbol`][e5af]s are candidates for exporting. A
    candidate symbol is exported if
    
    - it is [accessible][3473] in `package`, and
    
    - there is a reference to it in the section being defined which is
      approved by [`exportable-reference-p`][e51f].
    
    See [`define-package`][63f30] if you use the export feature. The idea with
    conflating documentation and exporting is to force documentation of
    all exported symbols and to reduce duplication.
    
    **Misc**
    
    - `title` is a string containing Markdown or `nil`. If non-`nil`, it
      determines the text of the heading in the generated output.
      `link-title-to` is a reference given as an `(name locative)` pair or
      `nil`, to which the heading will link when generating HTML. If not
      specified, the heading will link to its own anchor.
    
    - When `discard-documentation-p` (defaults to
      [`*discard-documentation-p*`][730f]) is true, `entries` will not be recorded
      to save memory.
    
    - `concepts` is a list of concept names and [concept key][b19d]s, for which
      this section is a *primary source*. See [Indexing Concepts][c001].

<a id="x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DISCARD-DOCUMENTATION-P*%20VARIABLE"></a>

- [variable] **\*discard-documentation-p\*** *nil*

    The default value of [`defsection`][72b4]'s `discard-documentation-p` argument.
    One may want to set `*discard-documentation-p*` to true before
    building a binary application.

<a id="x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-PACKAGE%20MGL-PAX:MACRO"></a>

- [macro] **define-package** *package &body options*

    This is like [`cl:defpackage`][9b43] but silences warnings and errors
    signalled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling [`export`][0c4f] (as is the case with [`defsection`][72b4]) as
    opposed to adding `:export` forms to the `defpackage` form and the
    package definition is subsequently reevaluated. See the section on
    [package variance](http://www.sbcl.org/manual/#Package-Variance) in
    the SBCL manual.
    
    The bottom line is that if you rely on `defsection` to do the
    exporting, then you'd better use `define-package`.

<a id="x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-GLOSSARY-TERM%20MGL-PAX:MACRO"></a>

- [macro] **define-glossary-term** *name (&key title url (discard-documentation-p \*discard-documentation-p\*) concepts) &body docstring*

    Define a global variable with `name`, and set it to a [`glossary-term`][8251] object. `title`, `url` and `docstring` are Markdown strings or
    `nil`. Glossary terms are [`document`][432c]ed in the lightweight bullet +
    locative + name/title style. See the glossary entry [name][88cf] for an
    example.
    
    When a glossary term is linked to in documentation, its `title` will
    be the link text instead of the name of the symbol (as with
    [`section`][5fac]s).
    
    Glossary entries with a non-`nil` `url` are like external links: they
    are linked to their `url` in the generated documentation. These offer
    a more reliable alternative to using Markdown reference links and
    are usually not included in `section`s.
    
    - When `discard-documentation-p` (defaults to [`*discard-documentation-p*`][730f])
      is true, `docstring` will not be recorded to save memory.
    
    - `concepts` is a list of concept names and [concept key][b19d]s, for which
      this term is a *primary source*. See [Indexing Concepts][c001].

<a id="x-28MGL-PAX-3ANOTE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:NOTE%20MGL-PAX:MACRO"></a>

- [macro] **note** *&body args*

    Define a note with an optional `name` and an optional
    `docstring`. The [`docstring`][affc] of the note is its
    own docstring concatenated with docstrings of other notes in the
    lexical scope of `body`.
    
    `args` has the form \[`name`\] \[`docstring`\] `body`, where the square
    brackets indicate optional arguments. See below for the details of
    parsing `args`.
    
    **`note` is experimental and as such subject to change.**
    
    `note` can occur in an any evaluated position without changing its
    `body`'s run-time behaviour or introducing any run-time overhead. [Top
    level forms][0f52] remain top level when wrapped in `note`. The names
    of notes live in the same global namespace regardless of nesting or
    whether they are [top level form][0f52]s. *These properties come at
    the price of `note` being weird: it defines named notes at
    macro-expansion time (or load time). But the definitions are
    idempotent, so it's fine to macroexpand `note` any number of times.*
    
    Notes are similar to Lisp comments, but they can be included in the
    documentation. In fact, notes are auto-included: a [Specific Link][0361] to
    a note is equivalent to including it with the [`docstring`][ce75] locative.
    Thus, `note` can double as a tool to avoid boilerplate. An
    [Unspecific Link][8e71] to a note has no effect.
    
    Notes are intended to help reduce the distance between code and its
    documentation when there is no convenient definition docstring to
    use nearby.
    
    ```common-lisp
    (note @xxx "We change the package."
      (in-package :mgl-pax))
    ==> #<PACKAGE "MGL-PAX">
    (values (docstring (dref '@xxx 'note)))
    => "We change the package."
    ```
    
    Here is an example of how to overdo things:
    
    ```common-lisp
    (note @1+*
      "This is a seriously overdone example."
      (defun 1+* (x)
        "[@1+* note][docstring]"
        (if (stringp x)
            (note (@1+*/1 :join #\Newline)
              "- If X is a STRING, then it is parsed as a REAL number."
              (let ((obj (read-from-string x)))
                (note "It is an error if X does not contain a REAL."
                  (unless (realp obj)
                    (assert nil)))
                (1+ obj)))
            (note "- Else, X is assumed to be REAL number, and we simply
                     add 1 to it."
              (1+ x)))))
    
    (1+* "7")
    => 8
    
    (values (docstring (dref '@1+* 'note)))
    => "This is a seriously overdone example.
    
    - If X is a STRING, then it is parsed as a REAL number.
    It is an error if X does not contain a REAL.
    
    - Else, X is assumed to be REAL number, and we simply
    add 1 to it."
    ```
    
    The parsing of `args`:
    
    - If the first element of `args` is not a string, then it is a `name` (a
      non-`nil` [`symbol`][e5af]) or name with options, currently destructured
      as `(name &key join)`. As in [`defsection`][72b4] and [`define-glossary-term`][8ece],
      the convention is that `name` starts with a `@` character.
    
        `join` is [`princ`][676d]ed before the docstring of a child note is output.
        Its default value is a string of two newline characters.
    
    - The next element of `args` is a Markdown docstring. See
      [Markdown in Docstrings][7bf5].
    
    - The rest of `args` is the `body`. If `body` is empty, then `nil` is
      returned.
    
    Note that named and nameless notes can contain other named or
    nameless notes without restriction, but nameless notes without a
    lexically enclosing named note are just an [implicit progn][c2fd]
    with `body`, and their docstring is discarded.
    
    If `note` occurs as a [top level form][0f52], then its [`source-location`][32da]
    is reliably recorded. Else, the quality of the source location
    varies, but it is at least within the right top level form on all
    implementations. On SBCL, exact source location is supported.

<a id="x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-LOCATIVES%20MGL-PAX:SECTION"></a>

## 6 PAX Locatives

In addition DRef's [own][1d1d],
PAX defines a few locative types using the facilities in described
in [Adding New Locatives][54d8]. [Locative][7ac8]s allow
[reference][43bd]ing definitions, which is used in [`defsection`][72b4],
[Navigating Sources in Emacs][3386] and docstrings (see [Codification][f1ab] and [Linking][19e3]
in the context of [Generating Documentation][2c93]).

<a id="x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:SECTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **section**

    - Direct locative supertypes: [variable][6c83]
    
    - Direct locative subtypes: dynamic-section

    Refers to a [`section`][5fac] defined by [`defsection`][72b4].
    
    `section` is [`exportable-locative-type-p`][c930] but not exported by
    default (see [`exportable-reference-p`][e51f]).

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM%20MGL-PAX:LOCATIVE"></a>

- [locative] **glossary-term**

    - Direct locative supertypes: [variable][6c83]

    Refers to a [`glossary-term`][8251] defined by [`define-glossary-term`][8ece].
    
    `glossary-term` is [`exportable-locative-type-p`][c930] but not exported by
    default (see [`exportable-reference-p`][e51f]).

<a id="x-28MGL-PAX-3ACONCEPT-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:CONCEPT%20MGL-PAX:LOCATIVE"></a>

- [locative] **concept**

    - Direct locative supertypes: [variable][6c83]

    Refers to a named group of [concept key][b19d]s defined by [`define-concept`][b80d]
    for multiplexing its keys to definitions that link to it in their
    docstrings. See [Indexing Concepts][c001] for more.
    
    `concept` is [`exportable-locative-type-p`][c930] but not exported by
    default (see [`exportable-reference-p`][e51f]).
    
    `concept`s have no [`arglist`][e6bd] or [`docstring`][affc].

<a id="x-28MGL-PAX-3ANOTE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:NOTE%20MGL-PAX:LOCATIVE"></a>

- [locative] **note**

    Refers to named notes defined by the [`note`][e2ae] macro.
    
    When [Generating Documentation][2c93], if a single link would be made to a
    `note` (be it either a [Specific Link][0361] or an unambiguous
    [Unspecific Link][8e71]), then the `note`'s [`docstring`][affc] is included as if with
    the [`docstring`][ce75] locative.
    
    `note` is [`exportable-locative-type-p`][c930] but not exported by default (see
    [`exportable-reference-p`][e51f]).

<a id="x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:DISLOCATED%20MGL-PAX:LOCATIVE"></a>

- [locative] **dislocated**

    Refers to a symbol in a non-specific context. Useful for
    suppressing [Unspecific Autolink][e2a4]ing. For example, if there is a
    function called `foo` then
    
        `FOO`
    
    will be linked (if [`*document-link-code*`][d9ee]) to its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. With a dislocated locative, [`locate`][8f19] always fails with a
    [`locate-error`][6334] condition. Also see [Escaping Autolinking][a2706].
    
    `dislocated` references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:ARGUMENT%20MGL-PAX:LOCATIVE"></a>

- [locative] **argument**

    An alias for [`dislocated`][e391], allowing one to refer to an
    argument of a macro without accidentally linking to a class that has
    the same name as that argument. In the following example,
    `format` may link to `cl:format` (if we generated
    documentation for it):
    
    ```
    "See FORMAT in DOCUMENT."
    ```
    
    Since `argument` is a locative, we can prevent that linking by writing:
    
    ```
    "See the FORMAT argument of DOCUMENT."
    ```
    
    `argument` references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:INCLUDE%20MGL-PAX:LOCATIVE"></a>

- [locative] **include** *source &key line-prefix header footer header-nl footer-nl*

    This [`pseudo`][943a] locative refers to a region of a file. `source` can be a
    [`string`][b93c] or a [`pathname`][0317], in which case the whole file
    is being pointed to, or it can explicitly supply `start`, `end`
    locatives. `include` is typically used to include non-lisp files in
    the documentation (say Markdown or Elisp as in the next example) or
    regions of Lisp source files. This can reduce clutter and
    duplication.
    
    ```
    (defsection @example-section ()
      (mgl-pax.el (include #.(asdf:system-relative-pathname
                              :mgl-pax "src/mgl-pax.el")
                           :header-nl "```elisp" :footer-nl "```"))
      (foo-example (include (:start (dref-ext:make-source-location function)
                             :end (dref-ext:source-location-p function))
                            :header-nl "```"
                            :footer-nl "```")))
    ```
    
    In the above example, when documentation is generated, the entire
    `src/mgl-pax.el` file is included in the Markdown output surrounded
    by the strings given as `header-nl` and `footer-nl`. The documentation
    of `foo-example` will be the region of the file from the
    [`source-location`][32da] of the `start` reference (inclusive) to the
    `source-location` of the `end` reference (exclusive). If only one of
    `start` and `end` is specified, then they default to the beginning and
    end of the file, respectively.
    
    Since `start` and `end` are literal references, pressing `M-.` on
    `pax.el` will open the `src/mgl-pax.el` file and put the cursor on
    its first character. `M-.` on `foo-example` will go to the source
    location of the `foo` function.
    
    With the [`lambda`][4796] locative, one can specify positions in arbitrary
    files.
    
    - `source` is either an absolute pathname designator or a list
      matching the [destructuring lambda list][6067] `(&key start end)`,
      where `start` and `end` must be `nil` or `(<name> <locative>)`
      lists (not evaluated) like a [`defsection`][72b4] entry. Their
      `source-location`s constitute the bounds of the region of the file
      to be included. Note that the file of the source location of `start`
      and `end` must be the same. If `source` is a pathname designator, then
      it must be absolute so that the locative is context independent.
    
    - If specified, `line-prefix` is a string that's prepended to each
      line included in the documentation. For example, a string of four
      spaces makes Markdown think it's a code block.
    
    - `header` and `footer`, if non-`nil`, are printed before the included
      string.
    
    - `header-nl` and `footer-nl`, if non-`nil`, are printed between two
      [`fresh-line`][3808] calls.
    
    `include` is not [`exportable-locative-type-p`][c930], and `include` references do
    not [`resolve`][63b4].

<a id="x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:DOCSTRING%20MGL-PAX:LOCATIVE"></a>

- [locative] **docstring**

    `docstring` is a [`pseudo`][943a] locative for including the parse tree of
    the Markdown [`docstring`][affc] of a definition in the parse tree
    of a docstring when generating documentation. It has no source
    location information and only works as an explicit link. This
    construct is intended to allow docstrings to live closer to their
    implementation, which typically involves a non-exported definition.
    
    ```common-lisp
    (defun div2 (x)
      "X must be [even* type][docstring]."
      (/ x 2))
    
    (deftype even* ()
      "an even integer"
      '(satisfies evenp))
    
    (document #'div2)
    .. - [function] DIV2 X
    ..
    ..     X must be an even integer.
    ..
    ```
    
    There is no way to [`locate`][8f19] `docstring`s, so nothing to [`resolve`][63b4] either.

<a id="x-28GO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="GO%20MGL-PAX:LOCATIVE"></a>

- [locative] **go** *(name locative)*

    Redirect to a definition in the context of the [reference][43bd]
    designated by `name` and `locative`. This [`pseudo`][943a] locative is intended for
    things that have no explicit global definition.
    
    As an example, consider this part of a hypothetical documentation of
    CLOS:
    
        (defsection @clos ()
          (defmethod macro)
          (call-next-method (go (defmethod macro))))
    
    The `go` reference exports the symbol [`call-next-method`][6832] and also
    produces a terse redirection message in the documentation.
    
    `go` behaves as described below.
    
    - A `go` reference [`resolve`][63b4]s to what `name` with `locative` resolves to:
    
        ```common-lisp
        (resolve (dref 'xxx '(go (print function))))
        ==> #<FUNCTION PRINT>
        => T
        ```
    
    - The [`docstring`][affc] of a `go` reference is `nil`.
    
    - [`source-location`][32da] (thus `M-.`) returns the source location of the
      embedded reference:
    
        ```common-lisp
        (equal (source-location (dref 'xxx '(go (print function))))
               (source-location (dref 'print 'function)))
        => T
        ```

<a id="x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:CLHS%20MGL-PAX:LOCATIVE"></a>

- [locative] **clhs** *&optional nested-locative*

    Refers to definitions, glossary entries, sections, issues and
    issue summaries in the Common Lisp HyperSpec. These have no source
    location so [`M-.`][cb15] will not work. What works is linking in
    documentation, including [Browsing Live Documentation][a595]. The generated
    links are relative to [`*document-hyperspec-root*`][f585] and work even if
    [`*document-link-to-hyperspec*`][875e] is `nil`. All matching is
    case-insensitive.
    
    - *definitions*: These are typically unnecessary as [`document`][432c] will
      produce the same link for e.g. `PPRINT`, `[PPRINT][function]`,
      or `[pprint][]` if `*document-link-to-hyperspec*` is non-`nil` and the
      [`pprint`][6af6] function in the running Lisp is not [linkable][7eb5]. When
      [Browsing Live Documentation][a595], a slight difference is that
      everything is linkable, so using the `clhs` link bypasses the page
      with the definition in the running Lisp.
    
        - *unambiguous definition*: `[pprint][clhs]` ([pprint][6af6])
    
        - *disambiguation page*: `[function][clhs]` ([function][aeb6])
    
        - *specific*: `[function][(clhs class)]` ([function][119e])
    
    - *glossary terms*:
    
        - `[lambda list][(clhs glossary-term)]`
          ([lambda list][98ff])
    
    - *issues*:
    
        - `[ISSUE:AREF-1D][clhs]` ([ISSUE:AREF-1D][63ef])
    
        - `[ISSUE:AREF-1D][(clhs section)]` ([ISSUE:AREF-1D][63ef])
    
    - *issue summaries*: These render
       as ([SUMMARY:CHARACTER-PROPOSAL:2-6-5][935f]):
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]`
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][(clhs section)]`
    
        Since these summary ids are not particularly reader friendly,
        the anchor text a [Specific Reflink with Text][fb17] may be used:
    
        - `[see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs section)]`
          ([see this][935f]).
    
    - *sections*:
    
        - *by section number*: `[3.4][clhs]` or `[3.4][(clhs
          section)]` ([3.4][e4420])
    
        - *by section title*: With the locative `(clhs section)`,
          substring matching against the title starting at word
          boundaries is performed. With the locative [`clhs`][ed5f] (where
          `SECTION` is not specified explicitly), the name must match
          the title exactly. For example, `[lambda list][(clhs
          section)]` refers to the same definition as `[lambda
          lists][clhs]` ([Lambda Lists][e4420]).
    
        - *by filename*: `[03_d][clhs]` or `[03_d][(clhs
          section)]` ([03\_d][e4420])
    
        - *by alias*
    
            - [Format directives][d273] are aliases of
              the sections describing them. Thus, `[~c][clhs]` is
              equivalent to `[22.3.1.1][clhs]` and `[Tilde C:
              Character][clhs]`. The full list is
              [~C][0cac] [~%][7bd6] [~\&][0684] [~|][3fa1]
            [~~][65bc] [~R][9927] [~D][3e6e] [~B][6897]
            [~O][76df] [~X][f9fa] [~F][cae2] [~E][1567]
            [~G][76ab] [~\$][5b4d] [~A][f275b] [~S][b39f]
            [~W][e6d3] [~\_][31c5] [~\<][ed9f] [~:>][ed9f]
            [~I][1959] [~/][db38] [~T][2352] [~\< Justification][11f1]
            [~>][bf38] [~\*][bdd6] [~\[][76ea] [~\]][945b]
            [~{][550b] [~}][886c] [~?][0db0] [~(][ac30]
            [~)][051b] [~P][60e4] [~;][d296] [~^][02e3]
            [~Newline][0165].
    
            - Similarly, [reader macro][3972] characters are aliases of
              the sections describing them. The full list is
              [(][040b] [)][e43c] ['][8e92] [;][56ba]
            ["][cd66] [\`][309c2] [,][826b] [#][698d]
            [\#\\][7b6f] [#'][8a5e] [#(][7a7f] [#\*][adf2]
            [\#:][ac5e] [#.][ffd7] [#B][c93e] [#O][68d2]
            [\#X][227d] [#R][2826] [#C][bfaa] [#A][7163]
            [\#S][dded] [#P][225d] [#=][fa43] [##][dd03]
            [\#+][d5e1] [#-][81b3] [#|][342d] [#\<][0f42]
            [\#)][9478].
    
            - Finally, [loop keywords][21f4] have
              aliases to the sections describing them. For example, the
              strings `loop:for`, `for` and `:for` are aliases of `clhs`
              [`6.1.2.1`][142c]. The `loop:*` aliases are convenient for
              completion at the prompt when
              [Browsing Live Documentation][a595], while the other aliases are
              for defaulting to buffer contents.
    
    As the above examples show, the `nested-locative` argument of the [`clhs`][ed5f]
    locative may be omitted. In that case, definitions, glossary terms,
    issues, issue summaries, and sections are considered in that order.
    Sections are considered last because a substring of a section title
    can be matched by chance easily.
    
    All examples so far used [Reflink][cbc4]s. [Autolink][ec7a]ing also works if the
    [name][88cf] is marked up as code or is [codified][f1ab] (e.g. in
    `COS clhs` ([`cos`][c4a3] clhs).
    
    As mentioned above, `M-.` does not do anything over `clhs`
    references. Slightly more usefully, the [live documentation
    browser][a595] understands `clhs` links so one
    can enter inputs like `3.4 clhs`, `"lambda list" clhs` or
    `error (clhs function)`.
    
    `clhs` references do not [`resolve`][63b4].

<a id="x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@NAVIGATING-IN-EMACS%20MGL-PAX:SECTION"></a>

## 7 Navigating Sources in Emacs

Integration into [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`) allows
one to visit the [`source-location`][32da] of a [definition][2143]. PAX extends
standard Slime functionality by

- adding support for all kinds of definitions (see e.g.
  [`asdf:system`][c097], [`readtable`][7506] in
  [Basic Locative Types][1d1d]), not just the ones Slime knows about,

- providing a portable way to refer to even standard definitions,

- disambiguating the definition based on buffer content, and

- adding more powerful completions.

The definition is either determined from the buffer content at point
or is prompted for. At the prompt, TAB-completion is available for
both names and locatives. With a prefix argument (`C-u M-.`), the
buffer contents are not consulted, and `M-.` always prompts.

The `M-.` extensions can be enabled by loading `src/mgl-pax.el`. See
[Emacs Setup][8541]. In addition, the Elisp command
`mgl-pax-edit-parent-section` visits the source location of the
section containing the definition with point in it.

A close relative of `M-.` is `C-.` for [Browsing Live Documentation][a595].

<a id="x-28MGL-PAX-3A-40M--2E-DEFAULTING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@M-.-DEFAULTING%20MGL-PAX:SECTION"></a>

### 7.1 `M-.` Defaulting

When [`M-.`][cb15] is invoked, it first tries to find a [name][88cf] in the
current buffer at point. If no name is found, then it
[prompts][ed46].

First, `(slime-sexp-at-point)` is taken as a [word][d7b0], from which the
[name][88cf] will be [parsed][378f]. Then, candidate locatives are
looked for before and after the [word][d7b0]. Thus, if a locative is the
previous or the next expression, then `M-.` will go straight to the
definition which corresponds to the locative. If that fails, `M-.`
will try to find the definitions in the normal way, which may
involve popping up an xref buffer and letting the user interactively
select one of possible definitions.

`M-.` works on parenthesized references, such as those in
[`defsection`][72b4]:

```
(defsection @foo ()
  (cos function))
```

Here, when the cursor is on one of the characters of `cos` or just
after `cos`, pressing `M-.` will visit the definition of the
function [`cos`][c4a3].

To play nice with [Generating Documentation][2c93], forms suitable for
[Autolink][ec7a]ing are recognized:

    function cos
    cos function

... as well as [Reflink][cbc4]s:

    [cos][function]
    [see this][cos function]

... and [Markdown inline code][68c1]:

    cos `function`
    `cos` function
    `cos` `function`

Everything works the same way in comments and docstrings as in code.
In the next example, pressing `M-.` on [`resolve*`][d3b3] will visit its
denoted method:

```
;;; See RESOLVE* (method (function-dref)) for how this all works.
```


<a id="x-28MGL-PAX-3A-40M--2E-PROMPTING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@M-.-PROMPTING%20MGL-PAX:SECTION"></a>

### 7.2 `M-.` Prompting

<a id="x-28MGL-PAX-3A-40M--2E-MINIBUFFER-SYNTAX-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@M-.-MINIBUFFER-SYNTAX%20MGL-PAX:SECTION"></a>

#### 7.2.1 `M-.` Minibuffer Syntax

At the minibuffer prompt, the [definition][2143]s to edit
can be specified as follows.

- `name`: Refers to all [`dref:definitions`][e196] of `name` with a [Lisp locative
  type][30ad]. See these `name -> definitions`
  examples:

        print    ->  PRINT FUNCTION
        PRINT    ->  PRINT FUNCTION
        MGL-PAX  ->  "mgl-pax" ASDF:SYSTEM, "MGL-PAX" package
        pax      ->  "PAX" PACKAGE
        "PAX"    ->  "PAX" PACKAGE

    Note that depending on the Lisp implementation there may be more
    definitions. For example, SBCL has an [`unknown`][a951]
    `:defoptimizer` definition for `print`.

- `name` `locative`: Refers to a single definition (as in `(dref:dref
  name locative)`). Example inputs of this form:

        print function
        dref-ext:docstring* (method (t))

- `locative` `name`: This has the same form as the previous: two sexps,
  but here the first one is the locative. If ambiguous, this is
  considered in addition to the previous one. Example inputs:

        function print
        (method (t)) dref-ext:docstring*

In all of the above `name` is a [raw name][f5af], meaning that `print`
will be recognized as `print` and `pax` as `"PAX"`.

The package in which symbols are read is the Elisp
`slime-current-package`. In Lisp buffers, this is the buffer's
package, else it's the package of the Slime repl buffer.

<a id="x-28MGL-PAX-3A-40M--2E-COMPLETION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@M-.-COMPLETION%20MGL-PAX:SECTION"></a>

#### 7.2.2 `M-.` Completion

When `M-.` prompts for the definition to edit, TAB-completion is
available in the minibuffer for both names and locatives. To reduce
clutter, string names are completed only if they are typed
explicitly with an opening quotation mark, and they are
case-sensitive. Examples:

- `pri<TAB>` invokes the usual Slime completion.

- `print <TAB>` (note the space) lists `function`([`0`][119e] [`1`][81f7]) and (`pax:clhs`
  [`function`][aeb6]) as locatives.

- `class dref:<TAB>` lists `dref:xref`([`0`][1538] [`1`][cda7]) and `dref:dref`([`0`][d930] [`1`][7e92]) (all the classes
  in the package `dref`).

- `pax:locative <TAB>` lists all [locative type][a11d]s (see the CL
  function [`dref:locative-types`][99b0]).

- `package "MGL<TAB>` lists the names of packages that start with
  `"MGL"`.

- `package <TAB>` lists the names of all packages as strings and
   also [`class`][1f37], `mgl-pax:locative` because [`package`][1d5a] denotes a class and
   also a locative.

For more powerful search, see [Apropos][b7fc].

<a id="x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@GENERATING-DOCUMENTATION%20MGL-PAX:SECTION"></a>

## 8 Generating Documentation

<a id="x-28MGL-PAX-3A-40DOCUMENT-FUNCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENT-FUNCTION%20MGL-PAX:SECTION"></a>

### 8.1 The `document` Function

<a id="x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29"></a>
<a id="MGL-PAX:DOCUMENT%20FUNCTION"></a>

- [function] **document** *documentable &key (stream t) pages (format :plain)*

    Write `documentable` in `format` to `stream` diverting some output to `pages`.
    `format` is one of [`:plain`][c879],
    [`:markdown`][dd29], [`:html`][36e1] and
    [`:pdf`][19ad] or [`nil`][f7e6]. `stream` may be a
    [`stream`][d5a9] object, `t` or `nil` as with [`cl:format`][ad78].
    
    To look up the documentation of the [`document`][432c] function itself:
    
        (document #'document)
    
    The same with fancy markup:
    
        (document #'document :format :markdown)
    
    To document a [`section`][5fac]:
    
        (document pax::@pax-manual)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list pax::@pax-manual dref::@dref-manual) :format :markdown)
    
    See [Utilities for Generating Documentation][1b1b] for more.
    
    Definitions that do not define a first-class object are supported
    via [DRef][5225]:
    
        (document (dref:locate 'foo 'type))
    
    There are quite a few special variables that affect how output is
    generated, see [Codification][f1ab], [Linking to the HyperSpec][7cc3],
    [Linking to Sections][22c2], [Link Format][c0d2] and [Output Formats][8d9b].
    
    For the details, see the following sections, starting with
    [`documentable`][0702]. Also see [Writing Extensions][c4ce3] and [`document-object*`][8269].

<a id="x-28MGL-PAX-3A-40DOCUMENTABLE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENTABLE%20MGL-PAX:SECTION"></a>

#### 8.1.1 `documentable`

The `documentable` argument of [`document`][432c] may be a single object (e.g.
`#'print`'), a [definition][2143] such as `(dref 'print 'function)`,
a string, or a nested list of these. More precisely, `documentable` is
one of the following:

- *single definition designator*: A [`dref`][d930] or anything else
  that is [`locate`][8f19]able. This includes non-`dref` [`xref`][1538]s and
  first-class objects such as [`function`][119e]s. The generated
  documentation typically includes the definition's [`docstring`][affc]. See
  [Markdown Output][dd29] for more.

- *docstring*: A string, in which case it is processed like a
  docstring in [`defsection`][72b4]. That is, with [docstring sanitization][7bf5], [Codification][f1ab], and [Linking][19e3].

- *list of documentables*: A nested list of `locate`able objects and
  docstrings. The objects in it are documented in depth-first order.
  The structure of the list is otherwise unimportant.


<a id="x-28MGL-PAX-3A-2ADOCUMENT-TIGHT-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-TIGHT*%20VARIABLE"></a>

- [variable] **\*document-tight\*** *nil*

    If `nil`, then [`document`][432c] adds a newline between consecutive
    [atomic][13c7] documentables on the same [page][9c7d].

<a id="x-28MGL-PAX-3A-40DOCUMENT-RETURN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENT-RETURN%20MGL-PAX:SECTION"></a>

#### 8.1.2 Return Values

If `pages` are `nil`, then [`document`][432c] - like [`cl:format`][ad78] - returns a
string (when [`stream`][d5a9] is `nil`) else `nil`.

If `pages`, then a list of output designators are returned, one for
each non-empty page (to which some output has been written), which
are determined as follows.

- The string itself if the output was to a string.

- The stream if the output was to a stream.

- The pathname of the file if the output was to a file.

If the default page given by the `stream` argument of `document` was
written to, then its output designator is the first element of the
returned list. The rest of the designators correspond to the
non-empty pages in the `pages` argument of `document` in that order.

<a id="x-28MGL-PAX-3A-40PAGES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAGES%20MGL-PAX:SECTION"></a>

#### 8.1.3 `pages`

The `pages` argument of [`document`][432c] is to create multi-page documents
by routing some of the generated output to files, strings or
streams. `pages` is a list of page specs. A page spec is a [property
list][b18e] with keys `:objects`, `:output`, `:uri-fragment`,
`:source-uri-fn`, `:header-fn` and `:footer-fn`.

`pages` may look something like this:

```
`((;; The section about SECTIONs and everything below it ...
   :objects (, @sections)
   ;; ... is so boring that it's not worth the disk space, so
   ;; send it to a string.
   :output (nil)
   ;; Explicitly tell other pages not to link to these guys.
   :uri-fragment nil)
  ;; Send the @EXTENSION-API section and everything reachable
  ;; from it ...
  (:objects (, @extension-api)
   ;; ... to build/tmp/pax-extension-api.html.
   :output "build/tmp/pax-extension-api.html"
   ;; However, on the web server html files will be at this
   ;; location relative to some common root, so override the
   ;; default:
   :uri-fragment "doc/dev/pax-extension-api.html"
   ;; Set html page title, stylesheet, charset.
   :header-fn 'write-html-header
   ;; Just close the body.
   :footer-fn 'write-html-footer)
  ;; Catch references that were not reachable from the above. It
  ;; is important for this page spec to be last.
  (:objects (, @pax-manual)
   :output "build/tmp/manual.html"
   ;; Links from the extension api page to the manual page will
   ;; be to ../user/pax-manual#<anchor>, while links going to
   ;; the opposite direction will be to
   ;; ../dev/pax-extension-api.html#<anchor>.
   :uri-fragment "doc/user/pax-manual.html"
   :header-fn 'write-html-header
   :footer-fn 'write-html-footer))
```

Documentation is initially sent to a default stream (the `stream`
argument of `document`), but output is temporary redirected to `:output`
for the duration of generating documentation for a docstring or
[definition][2143] that matches `:objects`, shadowing any existing
redirection if any. This nesting typically happens when a [`section`][5fac]
references another definition and they both have their own page
specs.

`:objects` is a list of objects whose definitions are [`locate`][8f19]able. In
addition, docstrings can be included. The latter can be useful if
[`documentable`][0702] includes a docstring.

- A docstring matches `:objects` if it is [`eq`][5a82] to one of its elements.

- A [definition][2143] matches `:objects` if it is [`xref=`][0617] to one of its
  [`dref`][d930] elements.

If multiple page specs match, then the first one has precedence.

- `:output` can be a number things:

    - If it's `nil`, then output will be collected in a string.

    - If it's `t`, then output will be sent to [`*standard-output*`][e7ee].

    - If it's a stream, then output will be sent to that stream.

    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on to [`cl:open`][6547]. One extra
      keyword argument is `:ensure-directories-exist`. If it's true,
      [`ensure-directories-exist`][876d] will be called on the pathname before
      it's opened.

    Note that even if `pages` is specified, `stream` acts as a catch all,
    absorbing the generated documentation for references not claimed by
    any pages.

- `:header-fn`, if not `nil`, is a function of a single stream argument,
  which is called just before the first write to the page. Since
  `:format` `:html` only generates HTML fragments, this makes it
  possible to print arbitrary headers, typically setting the title,
  CSS stylesheet, or charset.

- `:footer-fn` is similar to `:header-fn`, but it's called after the
   last write to the page. For HTML, it typically just closes the
   body.

- `:uri-fragment` is a string such as `"doc/manual.html"` that specifies
  where the page will be deployed on a webserver. It defines how
  links between pages will look. If it's not specified and `:output`
  refers to a file, then it defaults to the name of the file. If
  `:uri-fragment` is `nil`, then no links will be made to or from that
  page.

- `:source-uri-fn` is a function of a single, [`dref`][d930] argument.
  If it returns a value other than `nil`, then it must be a string
  representing an URI. This affects [`*document-mark-up-signatures*`][8fb6]
  and [`*document-fancy-html-navigation*`][6ab0]. Also see
  [`make-git-source-uri-fn`][587f].


<a id="x-28MGL-PAX-3A-40PACKAGE-AND-READTABLE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PACKAGE-AND-READTABLE%20MGL-PAX:SECTION"></a>

#### 8.1.4 Package and Readtable

While generating documentation, symbols may be read from
docstrings and printed. Our goal in general is to use the [`*package*`][5ed1]
and [`*readtable*`][b79a] in effect at the time the docstring was [`read`][fe58]. This
keeps the correspondence between

- [`M-.`][cb15] and [Linking][19e3], and

- interned symbols and [Codification][f1ab].

What values of `*package*` and `*readtable*` are used is determined
separately for each definition being documented. For a [`section`][5fac], its
[`section-package`][a5b1] and [`section-readtable`][88a7] are in effect.

For non-`section` definitions, the package and the readtable are given
by the following rules. If multiple rules provide a non-`nil` package,
then the first such rule takes precedence and similarly for the
readtable.

- `docstring`: The second value returned by [`docstring`][affc] for the
  definition provides the package.

    - If the value of `*package*` in effect at the time of definition
      was captured (e.g. by [`define-locative-type`][b6c4]), then `docstring`
      returns that.

    - The user may provide defaults for the package. See
      [`dref-ext:docstring*`][9fd4] (especially the *package-wide default*).

- *Home section*: If the definition has a [`home-section`][fda4], then the
  home section's `section-package` and `section-readtable` are used.

- *Arglist heuristic*: If the definition has an argument list, then
  the package of the first argument that's not external in any
  package is used.

- *Name heuristic*: If the definition is [name][5fc4]d by a symbol,
  then its [`symbol-package`][e5ab] is used, and `*readtable*` is set to the
  standard readtable `(named-readtables:find-readtable
  :common-lisp)`.

- *Default*: `*package*` is set to the `cl-user` package and
  `*readtable*` to the standard readtable.

The values thus determined come into effect after the name itself is
printed, for printing of the arglist and the docstring.

    CL-USER> (pax:document #'foo)
    - [function] FOO <!> X Y &KEY (ERRORP T)
    
        Do something with X and Y.

In the above, the `<!>` marks the place where `*package*` and
`*readtable*` are bound.

<a id="x-28MGL-PAX-3AHOME-SECTION-20FUNCTION-29"></a>
<a id="MGL-PAX:HOME-SECTION%20FUNCTION"></a>

- [function] **home-section** *object*

    The home section of an object is a [`section`][5fac] that contains the
    object's definition in its [`section-entries`][d850] or `nil`. In the
    overwhelming majority of cases there should be at most one
    containing section.
    
    If there are multiple containing sections, the following apply.
    
    - If the [name][5fc4] of the definition is a non-keyword symbol, only
      those containing sections are considered whose package is closest
      to the [`symbol-package`][e5ab] of the name, where closest is defined as
      having the longest common prefix between the two [`package-name`][db68]s.
    
    - If there are multiple sections with equally long matches or the
      name is not a non-keyword symbol, then it's undefined which one is
      the home section.
    
    For example, `(mgl-pax:document function)` is an entry in the
    `mgl-pax::@basics` section. Unless another section that contains it
    is defined in the `mgl-pax` package, the home section is guaranteed
    to be `mgl-pax::@basics` because the `symbol-package`s of
    [`mgl-pax:document`][432c] and `mgl-pax::@basics` are the same (hence their
    common prefix is maximally long).
    
    This scheme would also work, for example, if the [home package][407c]
    of `document` were `mgl-pax/impl`, and it were reexported from
    `mgl-pax` because the only way to externally change the home package
    would be to define a containing section in a package like
    `mgl-pax/imp`.
    
    Thus, relying on the package system makes it possible to find the
    intended home section of a definition among multiple containing
    sections with high probability. However, for names which are not
    symbols, there is no package system to advantage of.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES*%20VARIABLE"></a>

- [variable] **\*document-normalize-packages\*** *t*

    Whether to print `[in package <package-name>]` in the documentation
    when the package changes.

<a id="x-28MGL-PAX-3A-40BROWSING-LIVE-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BROWSING-LIVE-DOCUMENTATION%20MGL-PAX:SECTION"></a>

### 8.2 Browsing Live Documentation

Documentation for definitions in the running Lisp can be browsed
directly without generating documentation in the offline manner.
HTML documentation, complete with [Codification][f1ab] and [Linking][19e3], is
generated from docstrings of all kinds of definitions and PAX
[`section`][5fac]s in the running Lisp on the fly. This allows ad-hoc
exploration of the Lisp, much like `describe-function`,
`apropos-command` and other online help commands in Emacs, for which
direct parallels are provided.

Still, even without Emacs and [SLIME][6be7], limited functionality can be
accessed through [PAX Live Home Page][9d50] by starting the live
documentation web server [manually][72cc].

If [Emacs Setup][8541] has been done, the Elisp function
`mgl-pax-document` (maybe bound to `C-.`) generates and displays
documentation as a single HTML page. If necessary, a disambiguation
page is generated with the documentation of all matching
definitions. For example, to view the documentation of this very
`section`, one can do:

    M-x mgl-pax-document
    View Documentation of: pax::@browsing-live-documentation

Alternatively, pressing `C-.` with point over the text
`pax::@browsing-live-documentation` in a buffer achieves the same
effect.

In interactive use, `mgl-pax-document` behaves similarly to
[`M-.`][3386] except:

- It shows the [`document`][432c]ation of some definition and does not visit
  its [`source-location`][32da].

- It considers definitions with all [`locative-types`][99b0] not just
  [`lisp-locative-types`][30ad] because it doesn't need `source-location`.

    This also means that completion works for [`clhs`][ed5f]
    definitions:

    - `"lambda list<TAB>` lists `"lambda list"` and `"lambda list
      keywords"`, both HyperSpec glossary entries. This is similar
      to `common-lisp-hyperspec-glossary-term` in Elisp but also
      works for HyperSpec section titles.

    - `"#<tab>` lists all sharpsign reader macros (similar to
      `common-lisp-hyperspec-lookup-reader-macro` in Elisp).

    - `"~<tab>` lists all [`cl:format`][ad78] directives (similar to
      `common-lisp-hyperspec-format` in Elisp).

    - `"loop:~<TAB>` lists all [loop keywords][e016].

- It works in non-`lisp-mode` buffers by reinterpreting a few lines
  of text surrounding point as lisp code (hence the suggested
  *global* binding) in package `mgl-pax`.

- It supports fragment syntax at the prompt:

        NAME LOCATIVE FRAGMENT-NAME FRAGMENT-LOCATIVE

    This is like `name locative`, but the browser scrolls to the
    definition of `fragment-name fragment-locative` within that
    page.

    For example, entering this at the prompt will generate the
    entire PAX manual as a single page and scroll to the very
    section you are reading within it:

        pax::@pax-manual pax:section pax::@browsing-live-documentation pax:section

- If the empty string is entered at the prompt, and there is no
  existing w3m buffer or w3m is not used, then [PAX Live Home Page][9d50]
  is visited. If there is a w3m buffer, then entering the empty
  string displays that buffer.

The convenience function
`mgl-pax-current-definition-toggle-view` (`C-c C-d c`) documents the
definition with point in it.

<a id="x-28MGL-PAX-3A-40BROWSING-WITH-W3M-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BROWSING-WITH-W3M%20MGL-PAX:SECTION"></a>

#### 8.2.1 Browsing with w3m

When the value of the Elisp variable `mgl-pax-browser-function`
is `w3m-browse-url` (see [Emacs Setup][8541]), the Emacs w3m browser is
used without the need for a web server, and also offering somewhat
tighter integration than [Browsing with Other Browsers][c434].

With [w3m's default key bindings][1743], moving the cursor between links involves
`tab` and `s-tab` (or `<up>` and `<down>`). `ret` and `<right>`
follow a link, while `b` and `<left>` go back in history.

In addition, the following PAX-specific key bindings are available:

- `M-.` visits the source location of the definition corresponding
  to the link under the point.

- Invoking `mgl-pax-document` on a section title link will show the
  documentation of that section on its own page.

- `n` moves to the next PAX definition on the page.

- `p` moves to the previous PAX definition on the page.

- `u` follows the first `Up:` link (to the first containing
  [`section`][5fac]) if any.

- `U` is like `u` but positions the cursor at the top of the page.

- `v` visits the source location of the current definition (the one
  under the cursor or the first one above it).

- `V` visits the source location of the first definition on the
  page.


<a id="x-28MGL-PAX-3A-40BROWSING-WITH-OTHER-BROWSERS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BROWSING-WITH-OTHER-BROWSERS%20MGL-PAX:SECTION"></a>

#### 8.2.2 Browsing with Other Browsers

When the value of the Elisp variable `mgl-pax-browser-function`
is not `w3m-browse-url` (see [Emacs Setup][8541]), requests are served via
a web server started in the running Lisp, and documentation is most
likely displayed in a separate browser window.

By default, `mgl-pax-browser-function` is `nil`, which makes PAX use
`browse-url-browser-function`. You may want to customize the related
`browse-url-new-window-flag` or, for Chrome, set
`browse-url-chrome-arguments` to `("--new-window")`.

By default, `mgl-pax-web-server-port` is `nil`, and PAX will pick a
free port automatically.

In the browser, clicking on the locative on the left of the
name (e.g. in `- [function] PRINT`) will raise and focus the Emacs
window (if Emacs is not in text mode, and also subject to window
manager focus stealing settings), then go to the corresponding
source location. For sections, clicking on the lambda link will do
the same (see [`*document-fancy-html-navigation*`][6ab0]).

Finally, note that the `url`s exposed by the web server are subject to
change, and even the port used may vary by session if the Elisp
variable `mgl-pax-web-server-port` is nil.

<a id="x-28MGL-PAX-3A-2ABROWSE-HTML-STYLE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*BROWSE-HTML-STYLE*%20VARIABLE"></a>

- [variable] **\*browse-html-style\*** *:charter*

    The HTML style to use for browsing live documentation. Affects only
    non-w3m browsers. See [`*document-html-default-style*`][90fa] for the possible
    values.
    
    If you change this variable, you may need to do a hard refresh in
    the browser (often `C-<f5>`).

<a id="x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@APROPOS%20MGL-PAX:SECTION"></a>

#### 8.2.3 Apropos

The Elisp functions `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
`mgl-pax-apropos-package` can display the results of [`dref-apropos`][65b4] in
the [live documentation browser][a595].
These extend the functionality of `slime-apropos`,
`slime-apropos-all` and `slime-apropos-package` to support more
kinds of [definition][2143]s in an extensible way. The correspondence
is so close that the PAX versions might [take over the Slime key
bindings][8541].

Note that apropos functionality is also exposed via the
[PAX Live Home Page][9d50].

More concretely, the PAX versions supports the following extensions:

- Definitions with string names. One can search for
  [`asdf:system`s][c097], [`package`s][4dd7] and
  [`clhs`][ed5f] sections, glossary entries, format directives,
  reader macro characters, loop keywords.

- Exact or substring matching of the name and the package.

- Matching only symbol or string names.

On the [PAX Live Home Page][9d50], one may [Browse by Locative Types][659d], which
gives access to some of the apropos functionality via the browser
without involving Emacs.

On the result page:

- A `dref-apropos` form to reproduce the results at the REPL is shown.

- One may toggle the `external-only` and `case-sensitive` boolean
  arguments.

- One may switch between list, and detailed view. The list view only
  shows the first, [bulleted line][dd29] for each
  definition, while the detailed view includes the full
  documentation of definitions with the exception of [`section`][5fac]s.

- The returned references are presented in two groups: those with
  non-symbol and those with symbol [name][88cf]s. The non-symbol group is
  sorted by locative type then by name. The symbol group is sorted
  by name then by locative type.

With `mgl-pax-apropos-all` and `mgl-pax-apropos-package` being
simple convenience functions on top of `mgl-pax-apropos`, we only
discuss the latter in detail here. For the others, see the Elisp
docstrings.

<a id="x-28MGL-PAX-3A-40APROPOS-STRING-ARGUMENT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@APROPOS-STRING-ARGUMENT%20MGL-PAX:SECTION"></a>

##### The `string` Argument of `mgl-pax-apropos`

The `string` argument consists of a name pattern and a [`dtype`][a459].

The name pattern has the following forms.

- `:print` matches definitions whose names are the string `print`
  or a symbol with [`symbol-name`][0d07] `print`. Vertical bar form as in
  `:|prInt|` is also also supported and is useful in when
  `case-sensitive` is true.

- `"print"` matches definitions whose names contain `print` as
  a substring.

- `print` is like the previous, substring matching case. Use this
  form to save typing if the pattern does not contain spaces and
  does not start with a colon.

- The empty string matches everything.

After the name pattern, `string` may contain a
[`dtype`][a459] that the definitions must match.

- `print t` matches definitions with [`lisp-locative-types`][30ad], which is
  the default (equivalent to `print`).

- `print function` matches functions whose names contain
  `print` (e.g. [`cl:print`][d451] and [`cl:pprint`][6af6]).

- `:print function` is like the previous example but with exact
  name match (so it matches `cl:print` but not `cl:pprint`).

- `print variable` matches for example [`*print-escape*`][ff76].

- `print (or variable function)` matches all variables and functions
  with `print` in their names.

- `array (or type (not class))` matches [`deftype`][7f9a]s and but not [`class`][1f37]es
  with the string `array`([`0`][1f99] [`1`][cb94]) in their names.

- &nbsp;`pax:section` (note the leading space) matches all PAX
  sections (`external-only` `nil` is necessary to see many of them).

- `print dref:pseudo` matches definitions with [`pseudo-locative-types`][c340]
  such as `mgl-pax:clhs`.

- `print dref:top` matches definitions with all locative
  types ([`locative-types`][99b0]).


<a id="x-28MGL-PAX-3A-40APROPOS-PACKAGE-ARGUMENT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@APROPOS-PACKAGE-ARGUMENT%20MGL-PAX:SECTION"></a>

##### The `package` Argument of `mgl-pax-apropos`

When `mgl-pax-apropos` is invoked with a prefix argument, it
prompts for a package pattern among other things. The pattern may be
like the following examples.

- `:none` restricts matches to non-symbol names.

- `:any` restricts matches to symbol names.

- `:cl` restricts matches to symbols in the CL package.

- `:|x y|` is similar to the previous, but the vertical bar syntax
  allows for spaces in names.

- `mgl` restricts matches to packages whose name contains `mgl` as a
  substring.

- `"x y"` is the same as the previous, but the explicit quotes allow
  for spaces in names.

The above examples assume case-insensitive matching.

<a id="x-28MGL-PAX-3A-40PAX-LIVE-HOME-PAGE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-LIVE-HOME-PAGE%20MGL-PAX:SECTION"></a>

#### 8.2.4 PAX Live Home Page

When [Browsing Live Documentation][a595], the home page provides
quick access to documentation of the definitions in the system. In
Emacs, when `mgl-pax-document` is invoked with the empty string, it
visits the home page.

The home page may also be accessed directly by going to the root
page of the web server (if one is started). Here, unless the home
page is viewed [with w3m][83d5], one may directly look
up documentation and access [Apropos][b7fc] via the input boxes provided.

<a id="x-28MGL-PAX-3AENSURE-WEB-SERVER-20FUNCTION-29"></a>
<a id="MGL-PAX:ENSURE-WEB-SERVER%20FUNCTION"></a>

- [function] **ensure-web-server** *&key port hyperspec-root*

    Start or update a web server on `port` of localhost for
    [Browsing Live Documentation][a595]. Returns the base `url` of the
    server (e.g. `http://localhost:32790`), which goes to the
    [PAX Live Home Page][9d50]. If the web server is running
    already `(ensure-web-server)` simply returns its base `url`.
    
    Note that even when using Emacs but [Browsing with Other Browsers][c434],
    the web server is started automatically. When [Browsing with w3m][83d5], no
    web server is involved at all. Calling this function explicitly is
    only needed if the Emacs integration is not used, or to override
    `port` and `hyperspec-root`.
    
    - If `port` is `nil` or 0, then the server will use any free port.
    
    - If there is a server already running and `port` is not `nil` or 0,
      then the server is restarted on `port`.
    
    - If `hyperspec-root` is `nil`, the HyperSpec pages will be served from
      any previously provided `hyperspec-root` or, failing that, from
      [`*document-hyperspec-root*`][f585].
    
    - If `hyperspec-root` is non-`nil`, then pages in the HyperSpec will be
      served from `hyperspec-root`. The following command changes the root
      without affecting the server in any other way:
    
            (ensure-web-server :hyperspec-root "/usr/share/doc/hyperspec/")

<a id="x-28MGL-PAX-3A-40TOP-LEVEL-PAX-SECTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TOP-LEVEL-PAX-SECTIONS%20MGL-PAX:SECTION"></a>

##### Top-level PAX Sections

The [PAX Live Home Page][9d50] lists the top-level PAX sections: those
that have no other [`section`][5fac]s referencing them (see [`defsection`][72b4]).

<a id="x-28MGL-PAX-3A-40ASDF-SYSTEMS-AND-RELATED-PACKAGES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@ASDF-SYSTEMS-AND-RELATED-PACKAGES%20MGL-PAX:SECTION"></a>

##### `asdf:system`s and Related `package`s

The [PAX Live Home Page][9d50] lists all `asdf:system`s and [`package`][1d5a]s in the
Lisp. For easier overview, they are grouped based on their
[`source-location`][32da]s. Two systems are in the same group if the directory
of one (i.e. the directory of the `.asd` file in which it was
defined) is the same or is below the other's.

A `package` presented under a group of systems, if the `source-location`
of the package is below the the top-most directory among the systems
in the group.

<a id="x-28MGL-PAX-3A-40SYSTEMLESS-PACKAGES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SYSTEMLESS-PACKAGES%20MGL-PAX:SECTION"></a>

##### Systemless Packages

The [PAX Live Home Page][9d50] lists [`package`][1d5a]s
[unrelated][b033] to any `asdf:system`
as systemless.

<a id="x-28MGL-PAX-3A-40BROWSE-BY-LOCATIVE-TYPE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BROWSE-BY-LOCATIVE-TYPE%20MGL-PAX:SECTION"></a>

##### Browse by Locative Types

The [PAX Live Home Page][9d50] provides quick links to [Apropos][b7fc] result
pages for all [Basic Locative Types][1d1d] which may have
definitions.

<a id="x-28MGL-PAX-3A-40RELATED-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@RELATED%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **related**

    Two definitions are *related* if the directory of one's
    [`source-location`][32da]s contains the directory of the other's.

<a id="x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-SUPPORT%20MGL-PAX:SECTION"></a>

### 8.3 Markdown Support

[Markdown][a317] in docstrings and titles is processed with the [3BMD][1904] library.

<a id="x-28MGL-PAX-3A-40MARKDOWN-IN-DOCSTRINGS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-IN-DOCSTRINGS%20MGL-PAX:SECTION"></a>

#### 8.3.1 Markdown in Docstrings

- Docstrings can be indented in any of the usual styles. PAX
  normalizes indentation by stripping the longest run of leading
  spaces common to all non-blank lines except the first. Thus, the
  following two docstrings are equivalent:

        (defun foo ()
          "This is
          indented
          differently")
        
        (defun foo ()
          "This is
        indented
        differently")

- When [Browsing Live Documentation][a595], the page displayed can be of,
say, a single function within what would constitute the offline
documentation of a library. Because Markdown reference link
definitions, for example

        [Daring Fireball]: http://daringfireball.net/

    can be defined anywhere, they wouldn't be resolvable in that
    case, their use is discouraged. Currently, only reflink
    definitions within the documentation of the same
    [definition][2143] are guaranteed to be resolvable. This is left
    intentionally vague because the specifics are subject to change.

    See [`define-glossary-term`][8ece] for a better alternative to Markdown
    reference links.

Docstrings of definitions that do not have a [`home-section`][fda4] and are
not PAX constructs themselves (e.g [`section`][5fac], [`glossary-term`][8251], [`note`][e2ae]) are
assumed to have been written with no knowledge of PAX and to conform
to Markdown only by accident. These docstrings are thus sanitized
more aggressively.

- Indentation of what looks like blocks of Lisp code is rounded up to
a multiple of 4. More precisely, non-zero indented lines between
blank lines or the docstring boundaries are reindented if the first
non-space character of the first line is an `(` or a `;` character.

- Special HTML characters `<&` are escaped.

- Furthermore, to reduce the chance of inadvertently introducing a
Markdown heading, if a line starts with a string of `#` characters,
then the first one is automatically escaped. Thus, the following two
docstrings are equivalent:

        The characters #\Space, #\Tab and
        #\Return are in the whitespace group.
    
        The characters #\Space, #\Tab and
        \#\Return are in the whitespace group.


<a id="x-28MGL-PAX-3A-40MARKDOWN-IN-TITLES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-IN-TITLES%20MGL-PAX:SECTION"></a>

#### 8.3.2 Markdown in Titles

<a id="x-28MGL-PAX-3A-40TITLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@TITLE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **title**

    A title is a `string`([`0`][b93c] [`1`][dae6]) associated with a [definition][2143] (e.g. with
    the `title` argument of [`defsection`][72b4] or [`define-glossary-term`][8ece]). Titles
    are accessible via [`doctitle`][e619] and processed according to
    [Markdown in Titles][165c].

Titles undergo [Codification][f1ab] and may be a single paragraph
containing explicit [Markdown inline code][68c1], [Markdown emphasis][c6247],
[Markdown image][d534]s, inline [MathJax][a17d] and HTML entities (e.g. `&quot;`).
Other kinds of Markdown markup and block elements are not allowed.

<a id="x-28MGL-PAX-3ADOCTITLE-20FUNCTION-29"></a>
<a id="MGL-PAX:DOCTITLE%20FUNCTION"></a>

- [function] **doctitle** *object*

    Return the [title][090e] of `object` if it has one or `nil`. For
    [Codification][f1ab], the title is interpreted in the package returned by
    [`docstring`][affc]. `doctitle` can be extended via [`doctitle*`][c34e].

<a id="x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-SYNTAX-HIGHLIGHTING%20MGL-PAX:SECTION"></a>

#### 8.3.3 Syntax Highlighting

For syntax highlighting, GitHub's [fenced code blocks][1322] Markdown
extension to mark up code blocks with triple backticks is enabled so
all you need to do is write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from PAX and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [3BMD][1904] and [Colorize][3076] for the details.

<a id="x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MATHJAX%20MGL-PAX:SECTION"></a>

#### 8.3.4 MathJax

Displaying pretty mathematics between in TeX format is
supported via MathJax.

- *Inline*

    It can be done inline (within a paragraph):

        Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

    which is displayed as

    Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

    To avoid rendering `between $5 and $6` with inline math, both
    the opening and the closing `$` character must be followed /
    preceded by a non-space character. This agrees with Pandoc.

    Alternatively, the ``$`x_0`$`` syntax may be used (renders as
    $`x_0`$), which has no restriction on spacing.

- *Block*

    The `$$` is supported as a block element:

        Pretty, eh?
        
        $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$
        
        Yes.

    which will be rendered in its own paragraph:

    Pretty, eh?

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

    Yes.

    `$$` is also legal to use inline, but it's not recommended as it
    gets rendered inline on GitHub but as display math in
    [PDF Output][19ad].

MathJax will leave inline code (e.g. those between single backticks)
and code blocks (triple backticks) alone. Outside code, use
`<span>$</span>` to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String Reader][d3fc5] can help with that.

<a id="x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@CODIFICATION%20MGL-PAX:SECTION"></a>

### 8.4 Codification

<a id="x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE*%20VARIABLE"></a>

- [variable] **\*document-uppercase-is-code\*** *t*

    When true, [interesting][7445] [name][88cf]s extracted from [codifiable][b89a] [word][d7b0]s
    marked up as code with backticks. For example, this docstring
    
        "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
        CaMeL Capital"
    
    is equivalent to this:
    
        "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
        CaMel Capital"
    
    and renders as
    
    `t` [`print`][d451] [`class`][1f37]es [`section`][5fac] `mgl-pax` `asdf` CaMel Capital
    
    where the links are added due to [`*document-link-code*`][d9ee].
    
    To suppress codification, add a backslash to the beginning of the
    a [codifiable][b89a] word or right after the leading `*` if it would
    otherwise be parsed as Markdown emphasis:
    
        "\\SECTION *\\PACKAGE*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*document-uppercase-is-code*` is false.

<a id="x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@CODIFIABLE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **codifiable**

    A [word][d7b0] is *codifiable* if
    
    - it has a single uppercase character (e.g. it's `T`) and no
      lowercase characters at all, or
    
    - there is more than one uppercase character and no lowercase
      characters between them (e.g. `CLASSes`, `unREADable`,
      `CLASS-NAMEs` but not `Classes` or `aTe`).

<a id="x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@INTERESTING%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **interesting**

    A [name][88cf] is *interesting* if
    
    - it names a symbol external to its package, or
    
    - it is at least 3 characters long and names an interned symbol, or
    
    - it names a [Local Definition][9db9].
    
    See [Package and Readtable][ab7e].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE*%20VARIABLE"></a>

- [variable] **\*document-downcase-uppercase-code\*** *nil*

    If true, then all [Markdown inline code][68c1] (e.g. \`code\`, *which
    renders as* `code`) – including [Codification][f1ab] – which has no
    lowercase characters is downcased in the output. Characters of
    literal strings in the code may be of any case. If this variable is
    `:only-in-markup` and the output format does not support markup (e.g.
    it's `:plain`), then no downcasing is performed. For example,
    
        `(PRINT "Hello")`
    
    is downcased to
    
        `(print "Hello")`
    
    because it only contains uppercase characters outside the string.
    However,
    
        `MiXed "RESULTS"`
    
    is not altered because it has lowercase characters.
    
    If the first two characters are backslashes, then no downcasing is
    performed, in addition to [Escaping Autolinking][a2706]. Use this to mark
    inline code that's not Lisp.
    
        Press `\\M-.` in Emacs.

<a id="x-28MGL-PAX-3A-40LINKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKING%20MGL-PAX:SECTION"></a>

### 8.5 Linking

PAX supports linking to [definition][2143]s either with
explicit [Reflink][cbc4]s or with [Autolink][ec7a]s.

When generating offline documentation, only the definitions in
[`documentable`][0702] may be [linkable][7eb5], but when
[Browsing Live Documentation][a595], everything is linkable as
documentation is generated on-demand.

Many examples in this section link to standard Common Lisp
definitions. In the offline case, these will link to [external
URLs][f585], while in the live case to
disambiguation pages that list the definition in the running Lisp
and in the HyperSpec.

*Invoking [`M-.`][3386] on `word` or `name` in
any of the following examples will disambiguate based on the textual
context, determining the locative.* This is because navigation and
linking use the same [Parsing][378f] algorithm, although linking is a bit
more strict about trimming, depluralization, and it performs
[Filtering Links][b2e4]. On the other hand, `M-.` cannot visit the
[`clhs`][ed5f] references because there are no associated source
locations.

<a id="x-28MGL-PAX-3A-40STABLE-PRINTED-LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@STABLE-PRINTED-LOCATIVE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **stable printed locative**

    The [Link Format][c0d2] relies on [definition][2143]s having a unique
    textual representation that doesn't change. More concretely, if
    [`prin1`][6384] under [`with-standard-io-syntax`][39df] but with [`*print-readably*`][8aca] `nil`
    produces the same unique string deterministically, then linking to
    [definition][2143]s works even with non-readable locatives. The
    uniqueness condition requires that if two definitions are different
    under [`xref=`][0617], then their textual representations are also different.
    
    On the other hand, for example, a method involving an `eql`([`0`][db03] [`1`][5fd4])
    specializer with an object printed with [`print-unreadable-object`][9439]
    `:identity` `t` does not produce a stable string and links will break.

<a id="x-28MGL-PAX-3A-40REFLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@REFLINK%20MGL-PAX:SECTION"></a>

#### 8.5.1 Reflink

The [Markdown reference link][8c00] syntax `[label][id]` is
repurposed for linking to [definition][2143]s. In the following, we
discuss the various forms of reflinks.

<a id="x-28MGL-PAX-3A-40SPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SPECIFIC-REFLINK%20MGL-PAX:SECTION"></a>

##### Specific Reflink

*Format:* `[` [`word`][d7b0] `][` [`locative`][0b3a] `]`

The first [name][88cf] in `word` (with depluralization) that forms a valid
[`dref`][d930] with `locative` is determined, and that definition is
linked to. If there is no such `dref`, then an [`unresolvable-reflink`][64be]
warning is signalled.

*Examples:*

- ``[`EQL`][type]`` *renders as* [`eql`][5fd4].

- `[EQL][type]` *renders as* [`eql`][5fd4].

The Markdown link definition (i.e. [`type`][7c9f] above) needs no backticks
to mark it as code, but here and below, the second example relies on
[`*document-uppercase-is-code*`][f25f] being true.

<a id="x-28MGL-PAX-3A-40SPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SPECIFIC-REFLINK-WITH-TEXT%20MGL-PAX:SECTION"></a>

##### Specific Reflink with Text

*Format:* `[link text][` [`name`][88cf] [`locative`][0b3a] `]`

If `name` and `locative` form a valid [`dref`][d930], then that
definition is linked to with link text `link text`. If there is no
such `dref`, then an [`unresolvable-reflink`][64be] warning is signalled.

In this form, if `name` starts with `#\"`, then it's read as a string,
else as a symbol.

*Examples:*

- `[see this][eql type]` *renders as* [see this][5fd4].

- `[see this]["MGL-PAX" package]` *renders as* see this.


<a id="x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNSPECIFIC-REFLINK%20MGL-PAX:SECTION"></a>

##### Unspecific Reflink

*Format:* `[` [`word`][d7b0] `][]`

The first [name][88cf] in `word` (with depluralization, symbols only) that
has some [`definitions`][e196] is determined, and those definitions are linked
to. If no [name][88cf] with any definition is found, then an
[`unresolvable-reflink`][64be] warning is signalled.

*Examples:*

- single link: `[print][]` *renders as* [`print`][d451].

- multiple links: `[eql][]` *renders as* `eql`([`0`][db03] [`1`][5fd4]).

- no definitions: `[bad-name][]` *renders as* BAD-NAME.


<a id="x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNSPECIFIC-REFLINK-WITH-TEXT%20MGL-PAX:SECTION"></a>

##### Unspecific Reflink with Text

*Format:* `[link text][` [`name`][88cf] `]`

The [`definitions`][e196] of `name` are determined, and those definitions are
linked to. If `name` has no definitions, then an [`unresolvable-reflink`][64be]
warning is signalled.

*Examples:*

- `[see this][print]` *renders as* [see this][d451].

- `[see this][eql]` *renders as* see this([`0`][db03] [`1`][5fd4]).


<a id="x-28MGL-PAX-3A-40MARKDOWN-REFLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-REFLINK%20MGL-PAX:SECTION"></a>

##### Markdown Reflink

*Format:* `[label][id]`

This is a normal [Markdown reference link][8c00] if `id` is not a valid locative.

- `[see this][user-defined]` renders unchanged.

    ```common-lisp
    (dref:dref 'user-defined 'locative)
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate USER-DEFINED LOCATIVE.
    ..   USER-DEFINED is not a valid locative type or locative alias.
    ```

    ```common-lisp
    (document "[see this][user-defined]" :format :markdown)
    .. [see this][user-defined]
    ..
    ```

Use `url`s with [`define-glossary-term`][8ece] as a better alternative to
Markdown reference links (see [Markdown in Docstrings][7bf5]).

<a id="x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNRESOLVABLE-REFLINKS%20MGL-PAX:SECTION"></a>

##### Unresolvable Links

<a id="x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29"></a>
<a id="MGL-PAX:UNRESOLVABLE-REFLINK%20CONDITION"></a>

- [condition] **unresolvable-reflink** *[warning][bcb6] condition-context-mixin*

    When [`document`][432c] encounters a [Reflink][cbc4] that looks
    like a PAX construct but has no matching definition, it signals an
    `unresolvable-reflink` warning.
    
    - If the [`output-reflink`][2ca9] restart is invoked, then no warning is
      printed and the Markdown link is left unchanged. `muffle-warning`([`0`][b8b4] [`1`][6f51]) is
      equivalent to `output-reflink`.
    
    - If the [`output-label`][c818] restart is invoked, then no warning is printed
      and the Markdown link is replaced by its label. For example,
      `[NONEXISTENT][function]` becomes `nonexistent`.
    
    - If the warning is not handled, then it is printed to
      [`*error-output*`][66c6], and it behaves as if `output-label` was invoked.

<a id="x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29"></a>
<a id="MGL-PAX:OUTPUT-REFLINK%20FUNCTION"></a>

- [function] **output-reflink** *&optional condition*

    Invoke the `output-reflink` restart. See [`unresolvable-reflink`][64be].

<a id="x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29"></a>
<a id="MGL-PAX:OUTPUT-LABEL%20FUNCTION"></a>

- [function] **output-label** *&optional condition*

    Invoke the `output-label` restart. See [`unresolvable-reflink`][64be].

<a id="x-28MGL-PAX-3A-40AUTOLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@AUTOLINK%20MGL-PAX:SECTION"></a>

#### 8.5.2 Autolink

[Markdown inline code][68c1] automatically links to the corresponding
definitions without having to use [Reflink][cbc4]s. This works especially
well in conjunction with [Codification][f1ab]. The following examples
assume that [`*document-uppercase-is-code*`][f25f] is true. If that's not the
case, explicit backticks are required on [`word`][d7b0] (but not on
`locative`).

<a id="x-28MGL-PAX-3A-40SPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SPECIFIC-AUTOLINK%20MGL-PAX:SECTION"></a>

##### Specific Autolink

*Format:* [`word`][d7b0] [`locative`][0b3a] or
[`locative`][0b3a] [`word`][d7b0]

The first [name][88cf] in `word` (with depluralization) that forms a valid
[`dref`][d930] with `locative` is determined, and that definition is
linked to. If no such name is found, then [Unspecific Autolink][e2a4] is
attempted.

*Examples:*

- `PRINT function` *renders as* [`print`][d451] function.

- `type EQL` *renders as* type [`eql`][5fd4].

- `type EQL function` *renders as* type [`eql`][db03] function.

If `locative` has spaces, then it needs to be marked up as code, too.
For example,

    DREF-NAME `(reader dref)`

*renders as* [`dref-name`][1e36] `(reader dref)`.

<a id="x-28MGL-PAX-3A-40UNSPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNSPECIFIC-AUTOLINK%20MGL-PAX:SECTION"></a>

##### Unspecific Autolink

*Format:* [`word`][d7b0]

The first [name][88cf] in `word` (with depluralization, symbols only) that
has some [`definitions`][e196] is determined, and those definitions are linked
to. If no such name is found or the autolink to this name is
*suppressed* (see below), then `word` is left unchanged. If a locative
is found before or after `word`, then [Specific Autolink][38de] is tried
first.

*Examples:*

- `print` *renders as* [`print`][d451].

- `eql` *renders as* `eql`([`0`][db03] [`1`][5fd4]).

[Unspecific Autolink][e2a4]ing is suppressed if the name found has a
[Local Definition][9db9] or was linked to before in the same docstring:

- "`My other CAR is also a CAR`" *renders as* "My other [`car`][d5a2] is also a
`car`".

- "`[COS][] and COS`" *renders as* "[`cos`][c4a3] and `cos`".

- "`[EQL][type] and EQL`" *renders as* "[`eql`][5fd4] and `eql`".

- "`EQ and the EQ function`" *renders as* "[`eq`][5a82] and the [`eq`][5a82] function".

[Unspecific Autolink][e2a4]ing to `t` and `nil` is also suppressed (see
[`*document-link-to-hyperspec*`][875e]):

- "`T and NIL`" *renders as* "`t` and `nil`".

As an exception, a single link (be it either a [Specific Link][0361] or an
unambiguous [Unspecific Link][8e71]) to a [`section`][5fac], [`glossary-term`][8251] or [`note`][e2ae] is
not suppressed to allow their titles to be displayed or their
docstring to be included properly:

- "`@NAME and @NAME`" *renders as* "[name][88cf] and [name][88cf]".


<a id="x-28MGL-PAX-3A-40ESCAPING-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@ESCAPING-AUTOLINKING%20MGL-PAX:SECTION"></a>

##### Escaping Autolinking

In the common case, when [`*document-uppercase-is-code*`][f25f] is true,
prefixing an uppercase [word][d7b0] with a backslash prevents it from being
codified and thus also prevents [Autolink][ec7a]ing form kicking in. For
example,

    \DOCUMENT

renders as DOCUMENT. If it should be marked up as code but not
autolinked, the backslash must be within backticks like this:

    `\DOCUMENT`

This renders as `document`. Alternatively, the
[`dislocated`][e391] or the [`argument`][8710] locative may be used as in
`[DOCUMENT][dislocated]`.

<a id="x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKING-TO-THE-HYPERSPEC%20MGL-PAX:SECTION"></a>

#### 8.5.3 Linking to the HyperSpec

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC*%20VARIABLE"></a>

- [variable] **\*document-link-to-hyperspec\*** *t*

    If true, consider definitions found in the Common Lisp HyperSpec
    for linking. For example,
    
    - `print` *renders as* [`print`][d451].
    
    In offline documentation, this would be a link to the hyperspec
    unless `#'print` in the running Lisp is [`documentable`][0702].
    
    When [Browsing Live Documentation][a595], everything is [linkable][7eb5], so the
    generated link will go to a disambiguation page that lists the
    definition in the Lisp and in the HyperSpec.
    
    Locatives work as expected (see [`*document-link-code*`][d9ee]): `find-if`
    links to [`find-if`][5884], `function` links to `function`([`0`][119e] [`1`][81f7]), and
    `[FUNCTION][type]` links to [`function`][119e].
    
    [Unspecific Autolink][e2a4]ing to `t` and `nil` is suppressed. If desired, use
    [Reflink][cbc4]s such as `[t][]` (that links to `t`([`0`][9172] [`1`][fe21])) or
    `[T][constant]` (that links to [`t`][fe21]).
    
    Note that linking explicitly with the [`clhs`][ed5f] locative is not subject
    to the value of this variable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT*%20VARIABLE"></a>

- [variable] **\*document-hyperspec-root\*** *"http://www.lispworks.com/documentation/HyperSpec/"*

    A URL to the Common Lisp HyperSpec.
    The URL may not have a scheme, in which case `file` is assumed.
    
    The default value is the canonical location. When [invoked from
    Emacs][a595], the Elisp variable
    `common-lisp-hyperspec-root` is in effect.

<a id="x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKING-TO-SECTIONS%20MGL-PAX:SECTION"></a>

#### 8.5.4 Linking to Sections

The following variables control how to generate section numbering,
table of contents and navigation links.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-SECTIONS*%20VARIABLE"></a>

- [variable] **\*document-link-sections\*** *t*

    When true, HTML anchors and PDF destinations are generated before
    the headings (e.g. of sections), which allows the table of contents
    to contain links and also code-like references to sections (like
    `@foo-manual`) to be translated to links with the
    [`title`][72b4] being the link text.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL*%20VARIABLE"></a>

- [variable] **\*document-max-numbering-level\*** *3*

    A non-negative integer. In their hierarchy, sections on levels less
    than this value get numbered in the format of `3.1.2`. Setting it to
    0 turns numbering off.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*%20VARIABLE"></a>

- [variable] **\*document-max-table-of-contents-level\*** *3*

    An integer that determines the depth of the table of contents.
    
    - If negative, then no table of contents is generated.
    
    - If non-negative, and there are multiple top-level sections on a
      page, then they are listed at the top of the page.
    
    - If positive, then for each top-level section a table of contents
      is printed after its heading, which includes a nested tree of
      section titles whose depth is limited by this value.
    
    If [`*document-link-sections*`][1b28] is true, then the tables will link to
    the sections.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-TEXT-NAVIGATION*%20VARIABLE"></a>

- [variable] **\*document-text-navigation\*** *nil*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*document-link-sections*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION*%20VARIABLE"></a>

- [variable] **\*document-fancy-html-navigation\*** *t*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section, a self-link, and a link to the definition in the
    source code if available (see `:source-uri-fn` in [`document`][432c]). This
    component is normally hidden, it is visible only when the mouse is
    over the heading. Has no effect if [`*document-link-sections*`][1b28] is
    false.

<a id="x-28MGL-PAX-3A-40FILTERING-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@FILTERING-LINKS%20MGL-PAX:SECTION"></a>

#### 8.5.5 Filtering Links

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-CODE*%20VARIABLE"></a>

- [variable] **\*document-link-code\*** *t*

    Whether definitions of things other than [`section`][5fac]s
    are allowed to be [linkable][7eb5].

<a id="x-28MGL-PAX-3A-40LINKABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@LINKABLE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **linkable**

    When a reference is encountered to [definition][2143] D
    while processing documentation for some page C, we say that
    definition D is *linkable* (from C) if
    
    - D denotes a [`section`][5fac] and [`*document-link-sections*`][1b28] is true, or
    
    - D does not denote a `section` and [`*document-link-code*`][d9ee] is true
    
    ... and
    
    - We are [Browsing Live Documentation][a595], or
    
    - D is an external definition ([`clhs`][ed5f] or denotes a
      [`glossary-term`][8251] with a [`url`][8ece]), or
    
    - D's page is C, or
    
    - D's page is relativizable to C.
    
    In the above, *D's page* is the last of the pages in the
    [`documentable`][0702] to which D's documentation is written (see `:objects` in
    [`pages`][9c7d]), and we say that a page is *relativizable* to another if it
    is possible to construct a relative link between their
    `:uri-fragment`s.

<a id="x-28MGL-PAX-3A-40SPECIFIC-LINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SPECIFIC-LINK%20MGL-PAX:SECTION"></a>

##### Specific Link

Specific links are those [Reflink][cbc4]s and [Autolink][ec7a]s that have a
single [locative][7ac8] and therefore at most a single matching
[definition][2143]. These are [Specific Reflink][f7a5],
[Specific Reflink with Text][fb17] and [Specific Autolink][38de].

A specific link to a [linkable][7eb5] definition produces a link in the
output. If the definition is not linkable, then the output will
contain only what would otherwise be the link text.

<a id="x-28MGL-PAX-3A-40UNSPECIFIC-LINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNSPECIFIC-LINK%20MGL-PAX:SECTION"></a>

##### Unspecific Link

Unspecific links are those [Reflink][cbc4]s and [Autolink][ec7a]s that do not
specify a [locative][7ac8] and match all [definitions][d930]
with a name. These are [Unspecific Reflink][4e05],
[Unspecific Reflink with Text][34b8] and [Unspecific Autolink][e2a4].

To make the links predictable and manageable in number, the
following steps are taken.

1. Definitions that are not symbol-based (i.e. whose [`dref-name`][1e36]
is not a symbol) are filtered out to prevent unrelated
[`package`][4dd7]s, [`asdf:system`][c097]s and [`clhs`][ed5f]
sections from cluttering the documentation without the control
provided by importing symbols.

2. All references with [`locative-type`][97ba] `locative` are filtered out.

3. Non-[linkable][7eb5] definitions are removed.

4. If the definitions include a [`generic-function`][5875], then
all definitions with `locative-type` [`method`][172e],
[`accessor`][00d4], [`reader`][cc04] and [`writer`][e548] are
removed to avoid linking to a possibly large number of methods.

If at most a single definition remains, then the output is the same
as with a [Specific Link][0361]. If multiple definitions remain, then the
link text is output followed by a number of numbered links, one to
each definition.

<a id="x-28MGL-PAX-3A-40LINK-FORMAT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINK-FORMAT%20MGL-PAX:SECTION"></a>

#### 8.5.6 Link Format

The following variables control various aspects of links and URLs.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-URL-VERSIONS*%20VARIABLE"></a>

- [variable] **\*document-url-versions\*** *(2 1)*

    A list of versions of PAX URL formats to support in the
    generated documentation. The first in the list is used to generate
    links.
    
    PAX emits HTML anchors before the documentation of [`section`][5fac]s
    (see [Linking to Sections][22c2]) and other things (see [Linking][19e3]). For the
    function `foo`, in the current version (version 2), the anchor is
    `<a id="MGL-PAX:FOO%20FUNCTION">`, and its URL will end
    with `#MGL-PAX:FOO%20FUNCTION`.
    
    *Note that to make the URL independent of whether a symbol is
    [internal or external][3473] to their [`symbol-package`][e5ab], single
    colon is printed where a double colon would be expected. Package and
    symbol names are both printed verbatim except for escaping colons
    and spaces with a backslash. For exported symbols with no funny
    characters, this coincides with how [`prin1`][6384] would print the symbol,
    while having the benefit of making the URL independent of the Lisp
    printer's escaping strategy and producing human-readable output for
    mixed-case symbols. No such promises are made for non-ASCII
    characters, and their URLs may change in future versions. Locatives
    are printed with `prin1`.*
    
    Version 1 is based on the more strict HTML4 standard and the id of
    `foo` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
    by GitHub-flavoured Markdown. Version 2 has minimal clutter and is
    obviously preferred. However, in order not to break external links,
    by default, both anchors are generated.
    
    Let's understand the generated Markdown.
    
    ```
    (defun foo (x))
    
    (document #'foo :format :markdown)
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
    
    - [function] **FOO** *X*
    ")
    
    (let ((*document-url-versions* '(1)))
      (document #'foo :format :markdown))
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    
    - [function] **FOO** *X*
    ")
    ```

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MIN-LINK-HASH-LENGTH*%20VARIABLE"></a>

- [variable] **\*document-min-link-hash-length\*** *4*

    Recall that [Markdown reference link][8c00]s (like `[label][id]`) are used for
    [Linking][19e3]. It is desirable to have ids that are short to maintain
    legibility of the generated Markdown, but also stable to reduce the
    spurious diffs in the generated documentation, which can be a pain
    in a version control system.
    
    Clearly, there is a tradeoff here. This variable controls how many
    characters of the MD5 sum of the full link id (the reference as a
    string) are retained. If collisions are found due to the low number
    of characters, then the length of the hash of the colliding
    reference is increased.
    
    This variable has no effect on the HTML generated from Markdown, but
    it can make Markdown output more readable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-BASE-URL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-BASE-URL*%20VARIABLE"></a>

- [variable] **\*document-base-url\*** *nil*

    When `*document-base-url*` is non-`nil`, this is prepended to all
    Markdown relative `url`s. It must be a valid `url` without query or
    fragment parts (that is, *http://lisp.org/doc/* but not
    *http://lisp.org/doc?a=1* or *http://lisp.org/doc#fragment*). Note
    that intra-page links using only `url` fragments (e.g. and explicit
    HTML links (e.g. `<a href="...">`) in Markdown are not
    affected.

<a id="x-28MGL-PAX-3A-40LOCAL-DEFINITION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCAL-DEFINITION%20MGL-PAX:SECTION"></a>

### 8.6 Local Definition

While documentation is generated for a definition, that
definition is considered local. Other local definitions may also be
established. Local definitions inform [Codification][f1ab] through
[interesting][7445] [name][88cf]s and affect [Unspecific Autolink][e2a4]ing.

```
(defun foo (x)
  "FOO adds one to X."
  (1+ x)
```

In this example, while the docstring of `foo` is being processed, the
global definition `(dref 'foo 'function)` is also considered local,
which suppresses linking `foo` in the `foo`'s docstring back to its
definition. If `foo` has other definitions, [Unspecific Autolink][e2a4]ing to
those is also suppressed.

Furthermore, the purely local definition `(dref 'x 'argument)` is
established, causing the argument name `x` to be
[codified][f1ab] because `x` is now [interesting][7445].

See [`documenting-definition`][a412] and [`with-dislocated-names`][2d48] in
[Extending `document`][574a].

<a id="x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@OVERVIEW-OF-ESCAPING%20MGL-PAX:SECTION"></a>

### 8.7 Overview of Escaping

Let's recap how escaping [Codification][f1ab],
[downcasing][a5ee], and [Linking][19e3]
works.

- One backslash in front of a [word][d7b0] turns codification off. Use this
  to prevent codification words such as DOCUMENT, which is all
  uppercase hence [codifiable][b89a], and it names an exported symbol hence
  it is [interesting][7445].

- One backslash right after an opening backtick turns autolinking
  off.

- Two backslashes right after an opening backtick turns autolinking
  and downcasing off. Use this for things that are not Lisp code but
  which need to be in a monospace font.


In the following examples capital C/D/A letters mark the presence,
and a/b/c the absence of codification, downcasing, and autolinking
assuming all these features are enabled by
[`*document-uppercase-is-code*`][f25f], [`*document-downcase-uppercase-code*`][a5ee],
and [`*document-link-code*`][d9ee].

    DOCUMENT                => [`document`][1234]    (CDA)
    \DOCUMENT               => DOCUMENT              (cda)
    `\DOCUMENT`             => `document`            (CDa)
    `\\DOCUMENT`            => `DOCUMENT`            (Cda)
    [DOCUMENT][]            => [`document`][1234]    (CDA)
    [\DOCUMENT][]           => [DOCUMENT][1234]      (cdA)
    [`\DOCUMENT`][]         => [`document`][1234]    (CDA) *
    [`\\DOCUMENT`][]        => [`DOCUMENT`][1234]    (CdA)
    [DOCUMENT][dislocated]  => `document`            (CDa)

Note that in the example marked with `*`, the single backslash,
that would normally turn autolinking off, is ignored because it is
in an explicit link.

<a id="x-28MGL-PAX-3A-40INDEXING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@INDEXING%20MGL-PAX:SECTION"></a>

### 8.8 Indexing

PAX can create indices that cross-reference [definition][2143]s and
[concept key][b19d]s with where they are mentioned in the documentation.

<a id="x-28MGL-PAX-3A-40REFERRER-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@REFERRER%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **referrer**

    If the documentation of a [definition][2143] [links to][19e3]
    another definition, the former is considered a referrer to the
    latter. See [referent][ad8e].

<a id="x-28MGL-PAX-3A-40REFERENT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@REFERENT%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **referent**

    A [definition][2143] is considered a referent of another definition
    if the latter's documentation [links to][19e3] it. See
    [referrer][e7e0].

<a id="x-28MGL-PAX-3A-40INDEXING-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@INDEXING-DEFINITIONS%20MGL-PAX:SECTION"></a>

#### 8.8.1 Indexing Definitions

PAX generates indices with entries like this:

    foo [my-lib] (fn)
      ↩ d: My Lib Utilities
      ↩ f: bar, baz

`my-lib:foo` here is a function whose documentation has been [linked
to][19e3] by the `My Lib Utilities` [`section`][5fac] as well as the `bar`
and `baz` functions (grouped together under `f`). For example, BAR
could be defined like this:

    (defun bar (x)
      "Extends FOO."
      (1+ (foo x)))

In this example, `my-lib:foo` is the [referent][ad8e] and the others are
the [referrer][e7e0]s. If there are no referrers or they are in the same
group, then a single line is printed. Here is an example for the
latter:

    foo [my-lib] (fn) ↩ f: bar, baz


<a id="x-28MGL-PAX-3A-40INDEXING-CONCEPTS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@INDEXING-CONCEPTS%20MGL-PAX:SECTION"></a>

#### 8.8.2 Indexing Concepts

Let's first [`define-concept`][b80d]s:

```common-lisp
(define-concept @albert-einstein (:title "Albert Einstein"
                                  :keys ("Einstein, Albert")))

(define-concept @einstein-summation (:title "Einstein summation"
                                     :keys ("Einstein summation")))
```

`concept`s are [definition][2143]s, but in the generated documentation,
[links][19e3] to them are [*hidden*][56b9], and they
are not meant to be documented themselves. They exist to endow their
[referrer][e7e0]s with [concept key][b19d]s:

```common-lisp
(defun einsum (x)
  "Implement @EINSTEIN-SUMMATION by @ALBERT-EINSTEIN."
  x)
```

This makes `einsum` a [referrer][e7e0] to the concept
`@einstein-summation`, and thus will be indexed under the concept
keys of `@einstein-summation`.

[`defsection`][72b4] and [`define-glossary-term`][8ece] can associate concepts with the
section or glossary term being defined:

```common-lisp
(defsection @famous-people (:title "Famous People"
                            :concepts (@albert-einstein))
  "Let's start with one of the most famous, Albert Einstein.")
```

This is like the mention in `einsum`, but it also makes the section
a *primary source* of information. These are typeset more
prominently in the generated documentation. If the section only
mentions Einstein in passing, then don't use the `:concepts` argument,
just mention `@albert-einstein` in a section docstring as `einsum`
does.

Now, let's define a section to hold them together, configure a
minimal index, and see what comes out:

```common-lisp
(defsection @test (:title "Test" :export nil)
  (@famous-people section)
  (einsum function))

(defun print-concept-index (documentable)
  (let* ((*document-index-formats* '(:plain))
         (*document-indices* '((:concepts t :title "Concept Index")))
         (output (document documentable :stream nil)))
    (princ (subseq output (search "## Concept Index" output)))))

(print-concept-index @test)
.. ## Concept Index
..
.. - Einstein summation ↩ f: EINSUM
..
.. - Einstein, Albert
..
..     - ↩ d: Famous People
..
..     - ↩ f: EINSUM
..
```

This is okay, but let's group `summation` and `Albert` under
`Einstein` by breaking the string keys into lists of strings:

```common-lisp
(define-concept @albert-einstein (:title "Albert Einstein"
                                  :keys (("Einstein" "Albert"))))

(define-concept @einstein-summation (:title "Einstein summation"
                                     :keys (("Einstein" "summation"))))

(print-concept-index @test)
.. ## Concept Index
..
.. - Einstein
..
..     - Albert
..
..         - ↩ d: Famous People
..
..         - ↩ f: EINSUM
..
..     - summation ↩ f: EINSUM
..
```


See [`define-concept`][b80d] about *pure multiplexer concepts* (those
without `:title`s) and how concepts can inherit the keys of other
concepts.

Also, see [concept subkey][5920] on how to order concepts in the output.

<a id="x-28MGL-PAX-3ADEFINE-CONCEPT-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-CONCEPT%20MGL-PAX:MACRO"></a>

- [macro] **define-concept** *name (&key title keys)*

    Define a `concept` for [Indexing Concepts][c001]. `title` is a `string`([`0`][b93c] [`1`][dae6]) or `nil`,
    while `keys` is a list of [concept key][b19d]s and [`symbol`][e5af]s (naming other
    concepts, see below).
    
    When [Generating Documentation][2c93], if a [link][19e3] would be made
    to a `concept`, then:
    
    - The concept's `:keys` are recorded as [referent][ad8e]s with the definition
      being documented as the [referrer][e7e0].
    
        `symbol`s in `:keys` are resolved to the concepts they name, and
        they are substituted with their keys recursively (circularities
        are not allowed, but there are no checks for them).
    
    - In the docstring being processed, the text denoting the name of
      the concept is replaced by the concept's `:title` or the empty
      string if the title is `nil`. In an ambiguous [Unspecific Link][8e71], the
      links to concepts are simply removed.
    
    By convention, names of `title`d concepts start with a #\@ character,
    to indicate that these names are part of the sentence flow, as their
    titles are substituted for their mentions. Names of un`title`d
    concepts start with a #\~ character to indicate that they are pure
    index key multiplexers, and do not appear in the generated
    documentation.

<a id="x-28MGL-PAX-3A-40CONCEPT-KEY-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@CONCEPT-KEY%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **concept key**

    A concept key identifies an entry in an index.
    It is a [concept subkey][5920] or a list of [concept subkey][5920]s. Concept keys
    occur in [`define-concept`][b80d], [`defsection`][72b4] and [`define-glossary-term`][8ece].
    
    - *Single subkey*: For example, the key `"Einstein, Albert"`
      consists of a single subkey. A single subkey is equivalent to a
      one-element list.
    
    - *List of subkeys*: When Einstein summation is also referenced, we
      can instead use the lists `("Einstein," "Albert")` and `("Einstein,"
      "summation")` to ensure that their [referrer][e7e0]s are grouped together
      under `"Einstein,"`.
    
        List concept keys can be arbitrarily long, although the
        [PDF Output][19ad] imposes a limit of 9.

<a id="x-28MGL-PAX-3A-40CONCEPT-SUBKEY-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@CONCEPT-SUBKEY%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **concept subkey**

    A concept subkey (a part of a [concept key][b19d]) can be a string or a
    cons of strings. In the latter form, the [`car`][d5a2] is for printing, and
    the [`cdr`][e012] is for sorting. For example, use `("Éclair" . "Eclair")` to
    have `"Éclair"` printed but sorted as if it were `"Eclair"`. Sorting
    is done with plain [`string<`][519c], which is not Unicode-aware.
    
    Note that print names are [`escape-markdown`][3026]ed.

<a id="x-28MGL-PAX-3A-40CONFIGURING-INDICES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@CONFIGURING-INDICES%20MGL-PAX:SECTION"></a>

#### 8.8.3 Configuring Indices

<a id="x-28MGL-PAX-3A-2ADOCUMENT-INDEX-SECTIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-INDEX-SECTIONS*%20VARIABLE"></a>

- [variable] **\*document-index-sections\*** *:homeless-documentable*

    Controls what sections can get an index (if [`*document-index-formats*`][ba76]
    also allows it and they are non-empty).
    
    - `nil`: No indices are generated.
    
    - `:documentable`: Sections that appear in [`documentable`][0702] (at any
      level) can get indices. Their subsections cannot.
    
    - `:homeless-documentable`: Sections that appear in [`documentable`][0702] and
      have no [`home-section`][fda4] can get indices.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-INDEX-FORMATS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-INDEX-FORMATS*%20VARIABLE"></a>

- [variable] **\*document-index-formats\*** *(:html :pdf)*

    The list of [Output Formats][8d9b] for which index generation is allowed.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-INDICES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-INDICES*%20VARIABLE"></a>

- [variable] **\*document-indices\*** *"\<value omitted>"*

    A nested list of index specifications. Each spec is of the form
    
        (&key title documentation document-referrer-groups
              dtype concepts children)
    
    Non-`concept` [referent][ad8e]s recorded during documentation generation
    are assigned to the first (in depth-first order) spec with a
    [matching][963f] `dtype`. For [referent][ad8e]s that are concepts, their
    keys are assigned to the first spec with `:concepts` true.
    
    Specs are processed in the following way and order:
    
    - If a spec and all its `:children` are empty (nothing was assigned to
      them above), then they are not output.
    
    - Else, a dynamically generated [`section`][5fac] with `title` (a Markdown
      string) is entered.
    
    - If `documentation`([`0`][c5ae] [`1`][68f1]) is non-`nil`, then it is a Markdown docstring and
      is written as is to the output.
    
    - If `document-referrer-groups` is true, then the `abbrev`s and
      `documentation`s in [`*document-index-referrer-groups*`][85c9] are listed
      in the output.
    
    - The [referent][ad8e]s assigned to this spec are output along with their
      [referrer][e7e0]s (see the examples in [Indexing Concepts][c001]).
    
    - The `children` specs are processed.
    
    - The dynamically generated section is closed.
    
    The default value specifies separate indices for
    
    - functions and macros
    
    - variables
    
    - types
    
    - other things
    
    - `concept`s and [`glossary-term`][8251]s.
    
    They are all grouped under an "Indices" section, that has
    `:document-referrer-groups`.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-INDEX-REFERENT-LOCATIVE-TYPE-ABBREVS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-INDEX-REFERENT-LOCATIVE-TYPE-ABBREVS*%20VARIABLE"></a>

- [variable] **\*document-index-referent-locative-type-abbrevs\*** *((generic-function "\_(gf)\_") #'"\_(fn)\_" (variable "\_(var)\_")
 (asdf/system:system "\_(asdf:system)\_"))*

    A list of `(locative-type abbrev)` elements, where `abbrev` (a
    Markdown string) is printed instead of the [referent][ad8e]'s locative when
    its [`dref-locative-type`][a22ed] is [`locative-type`][97ba].
    
    The default behaviour is to bind [`*package*`][5ed1] to the package of the
    symbol naming the definition (if it's a symbol) and print the
    locative with [`*print-case*`][443b] `:downcase`, omitting the package of the
    locative type.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-INDEX-REFERRER-GROUPS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-INDEX-REFERRER-GROUPS*%20VARIABLE"></a>

- [variable] **\*document-index-referrer-groups\*** *"\<value omitted>"*

    A list of `(dtype abbrev &key documentation)` elements that defines
    the grouping of [referrer][e7e0]s of similar types in referrer lists. For
    example, the default value of this variable has this element:
    
        ((or function macro compiler-macro method) "↩ _f_:"
           :documentation "_f_: for definitions in the function namespace ...")
    
    Thus, in the following example, the definitions of `bar` and `baz`
    are grouped together under `↩ f:` because they are both [`dtypep`][963f] `(or
    function macro compiler-macro method)`:
    
        foo [my-lib] (fn)
          ↩ d: My Lib Utilities
          ↩ f: bar, baz
    
    - In general, the first matching `dtype` wins. See [`dtype`s][a459].
    
    - `abbrev` may be a Markdown string or a cons of strings to control
      sort order in the manner of [concept subkey][5920]s.
    
    - `documentation` is a Markdown string for
      `:document-referrer-groups` in [`*document-indices*`][8894].

<a id="x-28MGL-PAX-3A-40OUTPUT-FORMATS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@OUTPUT-FORMATS%20MGL-PAX:SECTION"></a>

### 8.9 Output Formats

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES*%20VARIABLE"></a>

- [variable] **\*document-mark-up-signatures\*** *t*

    When true, some things such as function names and arglists are
    rendered as bold and italic. In [HTML Output][36e1] and [PDF Output][19ad],
    locative types become links to sources (if `:source-uri-fn` is
    provided, see [`pages`][9c7d]), and the symbol becomes a self-link for your
    permalinking pleasure.
    
    For example, a reference is rendered in Markdown roughly as:
    
        - [function] foo x y
    
    With this option on, the above becomes:
    
        - [function] **foo** *x y*
    
    Also, in HTML `**foo**` will be a link to that very entry and
    `[function]` may turn into a link to sources.

<a id="x-28MGL-PAX-3A-40PLAIN-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PLAIN-OUTPUT%20MGL-PAX:SECTION"></a>

#### 8.9.1 Plain Output

This is the default `:format` of [`document`][432c], intended to be a
replacement for `cl:documentation`([`0`][c5ae] [`1`][68f1]). `:plain` (short for plain text) is
very similar to `:markdown`, but most of the markup that would make
reading in, say, the REPL unpleasant is removed.

- Markup for [Markdown emphasis][c6247], [Markdown inline code][68c1],
  [Markdown reference link][8c00]s and [fenced code blocks][1322] is stripped from
  the output.

- No link anchors are emitted.

- No [section numbering][f12d].

- No [table of contents][d4e7].


<a id="x-28MGL-PAX-3A-40MARKDOWN-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-OUTPUT%20MGL-PAX:SECTION"></a>

#### 8.9.2 Markdown Output

By default, [`dref`][d930]s are documented in the following format.

```
- [<locative-type>] <name> <arglist>

    <docstring>
```

The line with the bullet is printed with [`documenting-definition`][a412]. The
docstring is processed with [`document-docstring`][7f1f] while
[Local Definition][9db9]s established with [`with-dislocated-names`][2d48] are in
effect for all variables locally bound in a definition with [`arglist`][e6bd],
and [`*package*`][5ed1] is bound to the second return value of [`docstring`][affc].

With this default format, PAX supports all locative types, but for
some [Basic Locative Types][1d1d] defined in DRef and the
[PAX Locatives][292a], special provisions have been made.

- For definitions with a [`variable`][6c83] or [`constant`][c819] locative,
their initform is printed as their arglist. The initform is the
`initform` argument of the locative if provided, or the global symbol
value of their name. If no `initform` is provided, and the symbol is
globally unbound, then no arglist is printed.

When the printed initform is too long, it is truncated.

- Depending of what the [`setf`][d83a] locative refers to, the `arglist` of the
[setf expander][35a2], [setf function][99b05], or the method
signature is printed as with the [`method`][172e] locative.

- For definitions with a [`method`][172e] locative, the arglist printed is
the method signature, which consists of the locative's `qualifiers`
and `specializers` appended.

- For definitions with an [`accessor`][00d4], [`reader`][cc04] or
[`writer`][e548] locative, the class on which they are specialized is printed
as their arglist.

- For definitions with a [`structure-accessor`][090c] locative, the arglist
printed is the locative's `class-name` argument if provided.

- For definitions with a [`class`][2060] locative, the arglist printed is the
list of [public superclasses][ff58] with [`standard-object`][a843] and [`condition`][83e1]
omitted.

- For definitions with a [`structure`][da65] locative, the arglist printed is
the list of [public superclasses][ff58] with [`structure-object`][2038] omitted.

- For definitions with a [`condition`][c479] locative, the arglist printed is
the list of [public superclasses][ff58] with `standard-object` and
`condition` omitted.

- For definitions with a [`asdf:system`][c097] locative, their most
important slots are printed as an unnumbered list.

- For definitions with the [`locative`][0b3a] locative type, their
[`locative-type-direct-supers`][80a8] and [`locative-type-direct-subs`][130a] are
printed.

- When documentation is being generated for a definition with
the [`section`][672f] locative, a new (sub)section is opened (see
[`with-heading`][80e8]), within which documentation for its each of its
[`section-entries`][d850] is generated. A fresh line is printed after all
entries except the last.

- For definitions with a [`glossary-term`][5119] locative, no arglist is
printed, and if non-`nil`, [`glossary-term-title`][af78] is printed as name.

- For definitions with a [`concept`][56b9] locative, no documentation is
generated.

- For definitions with a [`go`][58f6] locative, its [`locative-args`][2444] are printed
as its arglist, along with a redirection message.

- See the [`include`][5cd7] locative.

- For definitions with a [`clhs`][ed5f] locative, the `locative-args` are printed
as the arglist. For `clhs` [`section`][7a4e]s, the title is included in the
arglist.

- For definitions with an [`unknown`][a951] locative, the `locative-args` are
printed as the arglist. There is no docstring.


<a id="x-28MGL-PAX-3A-40PUBLIC-SUPERCLASSES-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@PUBLIC-SUPERCLASSES%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **public superclasses**

    The public superclasses of a class are tightest envelope of
    superclasses with names exported from some package. This envelope is
    constructed by recursing depth-first into the superclass hierarchy.
    If the name of the superclass currently processed is exported from
    any package, then it is collected as a public superclass, and we do
    not recurse into its superclasses.

<a id="x-28MGL-PAX-3A-40PDF-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PDF-OUTPUT%20MGL-PAX:SECTION"></a>

#### 8.9.3 PDF Output

When invoked with `:format` `:pdf`, [`document`][432c] generates
[Markdown Output][dd29] and converts it to PDF with [Pandoc][59d9], which in turn
uses [LaTeX](https://www.latex-project.org/). Make sure that they
are installed.

```
(with-open-file (s "x.pdf" :direction :output :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
  (pax:document "Hello, World!" :stream s :format :pdf))
```

To see how the output looks like, visit
[pax-manual-v0.4.1.pdf][pax-manual-sample] and
[dref-manual-v0.4.1.pdf][dref-manual-sample].

[pax-manual-sample]: http://quotenil.com/blog-files/pax-manual-v0.4.1.pdf

[dref-manual-sample]: http://quotenil.com/blog-files/dref-manual-v0.4.1.pdf

PDF output is similar to [`*document-html-default-style*`][90fa] `:charter`
without the off-white tint and with coloured instead of underlined
links. The latter is because underlining interferes with hyphenation
in LaTeX. As in HTML output, locative types link to the respective
definition in the sources on GitHub (see [`make-git-source-uri-fn`][587f]).

Note that linking from one PDF to another is currently not supported
due to the lack of consistent support in PDF viewers. Therefore,
such links are replaced by their label or the title if any (e.g. of
a [`section`][5fac] or [`glossary-term`][8251]).

The generation of Markdown is subject to the standard
variables (again see [`document`][432c]). The Markdown to PDF conversion
can be customized with the following variables.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PROGRAM-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-PANDOC-PROGRAM*%20VARIABLE"></a>

- [variable] **\*document-pandoc-program\*** *"pandoc"*

    The name of the Pandoc binary. It need not be an absolute pathname
    as `PATH` is searched.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-OPTIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-PANDOC-PDF-OPTIONS*%20VARIABLE"></a>

- [variable] **\*document-pandoc-pdf-options\*** *(("-V" "papersize=a4") ("-V" "margin-left=1.03in") ("-V" "margin-right=1.03in")
 ("-V" "margin-top=1.435in") ("-V" "margin-bottom=1.435in")
 ("-V" "fontfamily=XCharter") ("-V" "fontsize=11pt") ("-V" "colorlinks=true")
 ("-V" "linkcolor=blue") ("-V" "urlcolor=Maroon") ("-V" "toccolor=blue")
 "--verbose")*

    The command-line options to invoke [`*document-pandoc-program*`][dd37] with.
    For ease of manipulation, related options are grouped into sublists,
    but the entire nested list is flattened to get the list of options
    to pass to Pandoc. If `--verbose` is specified, then in addition to
    Pandoc logging LaTeX sources, PAX will log to [`*error-output*`][66c6] the
    Markdown that it converts to PDF via LaTeX. The Markdown includes
    [`*document-pandoc-pdf-header-includes*`][2cd5] and
    [`*document-pandoc-pdf-metadata-block*`][ee8b].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-HEADER-INCLUDES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES*%20VARIABLE"></a>

- [variable] **\*document-pandoc-pdf-header-includes\*** *"\<too messy to include>"*

    LaTeX code (a string) to include in the preamble via
    [`header-includes`](https://pandoc.org/MANUAL.html#layout).
    
    The default includes have no configuration knobs. Look at the value
    to see how to customize it.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-METADATA-BLOCK-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-PANDOC-PDF-METADATA-BLOCK*%20VARIABLE"></a>

- [variable] **\*document-pandoc-pdf-metadata-block\*** *""*

    A [Pandoc YAML metadata block][84f2] as a string.
    
    Concatenate to this string to customize it.

<a id="x-28MGL-PAX-3A-40DUMMY-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DUMMY-OUTPUT%20MGL-PAX:SECTION"></a>

#### 8.9.4 Dummy Output

When the `format` argument of [`document`][432c] is `nil`, no output is
generated, but [Transcript Consistency Checking][4c39] is still performed.
Use this feature to quickly test documentation examples.

For example, in [Try][88e6] the test would look
like this:

```
(try:signals-not (transcription-consistency-error)
  (document ... :format nil))
```


<a id="x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENT-IMPLEMENTATION-NOTES%20MGL-PAX:SECTION"></a>

### 8.10 Documentation Generation Implementation Notes

Documentation Generation is supported on ABCL, AllegroCL, CLISP,
CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
lack of some introspective capability. SBCL generates complete
output. see [`arglist`][e6bd], [`docstring`][affc] and [`source-location`][32da] for
implementation notes.

In addition, CLISP does not support the ambiguous case of
[Browsing Live Documentation][a595] because the current implementation
relies on Swank to list definitions of symbols (as
[`variable`][6c83], [`function`][ba62], etc), and that simply
doesn't work.

<a id="x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENTATION-UTILITIES%20MGL-PAX:SECTION"></a>

### 8.11 Utilities for Generating Documentation

Two convenience functions are provided to serve the common case of
having an ASDF system with some readmes and a directory with for the
HTML documentation and the default CSS stylesheet.

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-ASDF-SYSTEM-READMES%20FUNCTION"></a>

- [function] **update-asdf-system-readmes** *object asdf-system &key (url-versions '(1)) (formats '(:markdown))*

    Convenience function to generate up to two readme files in the
    directory holding the `asdf-system` definition. `object` is passed on to
    [`document`][432c].
    
    If `:markdown` is in `formats`, then `README.md` is generated with
    anchors, links, inline code, and other markup added. Not necessarily
    the easiest on the eye in an editor but looks good on GitHub.
    
    If `:plain` is in `formats`, then `README` is generated, which is
    optimized for reading in text format. It has less cluttery markup
    and no [Autolink][ec7a]ing.
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @pax-manual :mgl-pax
                                :formats '(:markdown :plain))
    ```
    
    Note that [`*document-url-versions*`][17e0] is bound to `url-versions`, which
    defaults to using the uglier, version 1 style of `url` for the sake of
    GitHub.

<a id="x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@HTML-OUTPUT%20MGL-PAX:SECTION"></a>

#### 8.11.1 HTML Output

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS%20FUNCTION"></a>

- [function] **update-asdf-system-html-docs** *sections asdf-system &key pages (target-dir (asdf/system:system-relative-pathname asdf-system "doc/")) (update-css-p t) (style \*document-html-default-style\*)*

    Generate pretty HTML documentation for a single ASDF system,
    possibly linking to GitHub. If `update-css-p`, copy the `style` files to
    `target-dir` (see [`*document-html-default-style*`][90fa]).
    
    Example usage:
    
    ```
    (update-asdf-system-html-docs @pax-manual :mgl-pax)
    ```
    
    The same, linking to the sources on GitHub:
    
    ```
    (update-asdf-system-html-docs
      @pax-manual :mgl-pax
      :pages
      `((:objects (,mgl-pax::@pax-manual)
         :source-uri-fn ,(make-git-source-uri-fn
                          :mgl-pax
                          "https://github.com/melisgl/mgl-pax"))))
    ```

See the following variables, which control HTML generation.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-DEFAULT-STYLE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-DEFAULT-STYLE*%20VARIABLE"></a>

- [variable] **\*document-html-default-style\*** *:default*

    The HTML style to use. It's either `style` is either `:default` or
    `:charter`. The `:default` CSS stylesheet relies on the default
    fonts (sans-serif, serif, monospace), while `:charter` bundles some
    fonts for a more controlled look.
    
    The value of this variable affects the default style of
    [`update-asdf-system-html-docs`][bb12].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*%20VARIABLE"></a>

- [variable] **\*document-html-max-navigation-table-of-contents-level\*** *nil*

    `nil` or a non-negative integer. If non-`nil`, it overrides
    [`*document-max-numbering-level*`][f12d] in the dynamic HTML table of contents
    on the left of the page.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-LANG-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-LANG*%20VARIABLE"></a>

- [variable] **\*document-html-lang\*** *"en"*

    The value for the `html` element's `xml:lang` and `lang`
    attributes in the generated HTML.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-CHARSET-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-CHARSET*%20VARIABLE"></a>

- [variable] **\*document-html-charset\*** *"UTF-8"*

    The value for `charset` attribute of the `<meta http-equiv='Content-Type'
    content='text/html'>` element in the generated HTML.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-HEAD-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-HEAD*%20VARIABLE"></a>

- [variable] **\*document-html-head\*** *nil*

    Stuff to be included in the `<head>` of the generated HTML.
    
    - If `nil`, nothing is included.
    
    - If a `string`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-SIDEBAR-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-SIDEBAR*%20VARIABLE"></a>

- [variable] **\*document-html-sidebar\*** *nil*

    Stuff to be included in the HTML sidebar.
    
    - If `nil`, a default sidebar is generated, with
      [`*document-html-top-blocks-of-links*`][e216], followed by the dynamic table
      of contents, and [`*document-html-bottom-blocks-of-links*`][0ef0].
    
    - If a `string`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*%20VARIABLE"></a>

- [variable] **\*document-html-top-blocks-of-links\*** *nil*

    A list of blocks of links to be displayed on the sidebar on the left,
    above the table of contents. A block is of the form `(&key title id
    links)`, where `title` will be displayed at the top of the block in a
    HTML `div` with `id` followed by the links. `links` is a list of `(uri
    label)` elements, where `uri` maybe a string or an object being
    [`document`][432c]ed or a `reference` thereof.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*%20VARIABLE"></a>

- [variable] **\*document-html-bottom-blocks-of-links\*** *nil*

    Like [`*document-html-top-blocks-of-links*`][e216], only it is displayed
    below the table of contents.

<a id="x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@GITHUB-WORKFLOW%20MGL-PAX:SECTION"></a>

#### 8.11.2 GitHub Workflow

It is generally recommended to commit generated readmes (see
[`update-asdf-system-readmes`][13a9]) so that users have something to read
without reading the code and sites like GitHub can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`make-git-source-uri-fn`][587f]), the
commit id is in the link. This means that code changes need to be
committed first, and only then can HTML documentation be regenerated
and committed in a followup commit.

The second issue is that GitHub is not very good at serving HTML
files from the repository itself (and
[http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
on links to the sources).

The recommended workflow is to use
[gh-pages](https://pages.github.com/), which can be made relatively
painless with the `git worktree` command. The gist of it is to make
the `doc/` directory a checkout of the branch named `gh-pages`.
There is a [good
description](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html)
of this general process. Two commits are needed still, but it is
somewhat less painful.

This way the HTML documentation will be available at

    http://<username>.github.io/<repo-name>

It is probably a good idea to add sections like the
[Links and Systems][ba90] section to allow jumping between the repository
and the gh-pages site.

<a id="x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>
<a id="MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN%20FUNCTION"></a>

- [function] **make-github-source-uri-fn** *asdf-system github-uri &key git-version*

    This function is a backward-compatibility wrapper around
    [`make-git-source-uri-fn`][587f], which supersedes `make-github-source-uri-fn`.
    All arguments are passed on to `make-git-source-uri-fn`, leaving
    `uri-format-string` at its default, which is suitable for GitHub.

<a id="x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29"></a>
<a id="MGL-PAX:MAKE-GIT-SOURCE-URI-FN%20FUNCTION"></a>

- [function] **make-git-source-uri-fn** *asdf-system git-forge-uri &key git-version git-root (uri-format-string "~A/blob/~A/~A#L~S")*

    Return an object suitable as `:source-uri-fn` of a page spec (see
    the `pages` argument of [`document`][432c]). The function looks at the source
    location of the object passed to it, and if the location is found,
    the path is made relative to the `git-root` (the top-level directory
    of the git checkout), and finally an URI pointing to your git
    forge (such as GitHub) is returned.
    
    - If `asdf-system` is non-`nil`, then `git-root` defaults to the directory
      of the `.asd` file of the system. Similarly, `git-version` defaults
      current commit id in the checkout at `git-root`.
    
    - If both `git-root` and `git-version` are explicitly provided,
      `asdf-system` has no effect.
    
    If `git-root` is not specified and cannot be determined from
    `asdf-system`, then no links to the git forge will be generated.
    
    A warning is signalled whenever the source location lookup fails or
    if the source location points to a directory not below `git-root`.
    
    If `git-forge-uri` is `"https://github.com/melisgl/mgl-pax/"` and
    `git-version` is `"master"`, then the returned URI may look like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    `uri-format-string` is a [`cl:format`][ad78] control string for four arguments:
    
    - `git-forge-uri`,
    
    - `git-version`,
    
    - the relative path to the file of the source location of the reference,
    
    - and the line number.
    
    The default value of `uri-format-string` is for GitHub. If using a
    non-standard git forge, such as Sourcehut or GitLab, simply pass a
    suitable `uri-format-string` matching the URI scheme of your forge.

<a id="x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-WORLD%20MGL-PAX:SECTION"></a>

#### 8.11.3 PAX World

PAX World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents. There is an official [PAX
World](https://melisgl.github.io/mgl-pax-world/).

<a id="x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29"></a>
<a id="MGL-PAX:REGISTER-DOC-IN-PAX-WORLD%20FUNCTION"></a>

- [function] **register-doc-in-pax-world** *id sections page-specs*

    Register `sections` and `page-specs` under `id` (an arbitrary symbol) in
    PAX World. By default, [`update-pax-world`][ee51] generates documentation for
    all of these. `sections` and `page-specs` must be lists of [`section`][5fac]s and
    `page-spec`s (see [`pages`][9c7d]) or designators of functions with no
    arguments that return such lists.
    
    Using an existing `id` overwrites the previous registration.

For example, this is how PAX registers itself:

```
(defun pax-sections ()
  (list @pax-manual))

(defun pax-pages ()
  `((:objects ,(pax-sections)
     :source-uri-fn ,(make-git-source-uri-fn
                      "mgl-pax" "https://github.com/melisgl/mgl-pax"))))

(register-doc-in-pax-world :pax 'pax-sections 'pax-pages)

```

<a id="x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-PAX-WORLD%20FUNCTION"></a>

- [function] **update-pax-world** *&key (docs \*registered-pax-world-docs\*) (formats '(:html)) dir update-css-p (style \*document-html-default-style\*)*

    Generate HTML documentation for all `docs`. Files are created in
    `dir` (`(asdf:system-relative-pathname :mgl-pax "world/")` by
    default if `dir` is `nil`). `docs` is a list of entries of the form (`name`
    [`sections`][5fac] `page-specs`). The default for `docs` is all the sections and
    pages registered with [`register-doc-in-pax-world`][f4fd].
    
    In the absence of `:header-fn` `:footer-fn`, `:output`, every spec in
    `page-specs` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id="x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPTS%20MGL-PAX:SECTION"></a>

## 9 Transcripts

What are transcripts for? When writing a tutorial, one often
wants to include a REPL session with maybe a few defuns and a couple
of forms whose output or return values are shown. Also, in a
function's docstring an example call with concrete arguments and
return values speaks volumes. A transcript is a text that looks like
a REPL session, but which has a light markup for printed output and
return values, while no markup (i.e. prompt) for Lisp forms. PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case, the transcript itself can be
easily generated from the source code. The main worry associated
with including examples in the documentation is that they tend to
get out-of-sync with the code. This is solved by being able to parse
back and update transcripts.

<a id="x-28MGL-PAX-3A-40TRANSCRIBING-IN-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIBING-IN-DOCUMENTATION%20MGL-PAX:SECTION"></a>

### 9.1 Transcribing in Documentation

Consider this function:

```
(defun foo (x)
  "Return X + 1 and log what happened."
  (format t "Adding 1 to ~S~%" x)
  (1+ x))
```

Let's add an example to the docstring:

```
(defun foo (x)
  "Return X + 1 and log what happened.
  For example,

  ```cl
  (foo 7)
  .. Adding 1 to 7
  ..
  => 8
  ```"
  (format t "Adding 1 to ~S~%" x)
  (1+ x))
```

In this transcript above, output lines are prefixed with `".. "` and
return values with `"=> "`. The transcript could have been generated
with `(transcribe` `"(foo 7)" *standard-output*)` or interactively
by invoking `mgl-pax-transcribe-last-expression` in Emacs with the
point right after `(foo 7)` (see [Transcript API][9dbc] and
[Transcribing with Emacs][f5bd]).

If we want PAX to check that the transcript is consistent with the
code when [Generating Documentation][2c93], we add the `cl-transcript` tag
to the code block:

```
(defun foo (x)
  "Return X + 1 and log what happened.
  For example,

  ```cl-transcript
  (foo 7)
  .. Adding 1 to 7
  ..
  => 8
  ```"
  (format t "Adding 1 to ~S~%" x)
  (+ x 2))
```

Since the code was also changed to `(+ x 2)`, documenting this
function causes the following error:

```
(document #'foo)
.. debugger invoked on TRANSCRIPTION-VALUES-CONSISTENCY-ERROR:
..   Transcription error: Readable value "8" in source is not EQUAL to "9".
..     [While documenting (MGL-PAX::FOO COMMON-LISP:FUNCTION)]
..
..   Form:
..   "(foo 7)"
```

When [Browsing Live Documentation][a595], any errors signalled during
transcription are downgraded to warnings.

All in all, transcripts are a handy tool especially when combined
with the Emacs support to update them and with
PYTHONIC-STRING-READER's triple-quoted strings, that
allow one to work with nested strings with less noise. The
triple-quote syntax can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id="x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIBING-WITH-EMACS%20MGL-PAX:SECTION"></a>

### 9.2 Transcribing with Emacs

With [Emacs set up][8541], we can have two Elisp
functions at our disposal for working with transcripts.

- `mgl-pax-transcribe-last-expression`: Evaluates a single form, and
  insert its output and return values into the current buffer.

- `mgl-pax-retranscribe-region`: Retranscribes a region as with
  [`transcribe`][f1f0] `:update-only` `t`. The region defaults to the innermost
  enclosing `cl-transcript` block if any.

These functions work in any major and minor mode where the basic
sexp movements are sufficiently Lisp-like. They also work anywhere
in the buffer, including docstrings and comments at any indentation
level. Like [`document`][432c], they understand the `:dynenv` argument of
`cl-transcript` and pass on the other arguments to `transcribe`.

For example, consider this buffer content, which may be part of a
docstring. Move the cursor right after the end of the form as if you
were to evaluate it with `C-x C-e`. The cursor is marked by `^`:

    (values (princ :hello) (list 1 2))^

Now invoke the Elisp function `mgl-pax-transcribe` where the cursor
is to insert the transcribed output and return values:

    (values (princ :hello) (list 1 2))
    .. HELLO
    => :HELLO
    => (1 2)
    ^

Then, change the printed message and add comments to the second
return value:

    (values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => ;; This is a list.
       (1
        ;; This value is arbitrary.
        2)

Obviously, the transcript is now out-of-date, and if it is in a
`cl-transcript` code block in a docstring, then `document` will signal
a [`transcription-consistency-error`][a249]. So, let's update the transcript
by marking the region bounded by `|` and the cursor at `^` in this
example:

    |(values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => ;; This is a list.
       (1
        ;; This value is arbitrary.
        2)
    ^

Then, invoke the Elisp function `mgl-pax-retranscribe-region` to get

    (values (princ :hello-world) (list 1 2))
    .. HELLO-WORLD
    => :HELLO-WORLD
    => ;; This is a list.
       (1
        ;; This value is arbitrary.
        2)
    ^

Note how the indentation and comments of `(1 2)` were left alone,
but the output and the first return value got updated.

Also note that when the cursor is in `cl-transcript` code block,
`mgl-pax-retranscribe-region` defaults to the whole block when there
is no active region.

Alternatively, `C-u 1 mgl-pax-transcribe` will emit commented markup:

    (values (princ :hello) (list 1 2))
    ;.. HELLO
    ;=> :HELLO
    ;=> (1 2)

`C-u 0 mgl-pax-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in [`*transcribe-syntaxes*`][ebd3]. Without a
prefix argument, `mgl-pax-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

With `mgl-pax-transcribe-last-expression`, we strip the
longest run of leading spaces and semicolons common to all
lines of the expression in the buffer.

For `mgl-pax-retranscribe-region`, the longest run is
truncated so that it does not extend beyond the column of the
first form to be transcribed. Without this rule, the syntax
used

```common-lisp
;;(list 1 2)
;;;=> (1
;;;->  2)
```

would be ambiguous, as the `;=>` could refer to `=>` in the
`:default` syntax or to `;=>` in `:commented-1`.

The dynamic environment of the transcription is determined by the
`:dynenv` argument of the enclosing `cl-transcript` code block (see
[Controlling the Dynamic Environment][6b59]).

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-DYNENV%20MGL-PAX:SECTION"></a>

### 9.3 Controlling the Dynamic Environment

When the [`transcribe`][f1f0] function is called directly, the forms in the
transcript are evaluated in the current dynamic environment with the
following exceptions.

- The variables [\*][9590], [\*\*][78d1], [\*\*\*][ea37], [/][9f2f],
[//][e433], [///][3831], [-][5483], [+][72a7], [++][fbb1], [+++][4537] are
locally bound and updated as in a new [REPL][f83b] session.

- [`*standard-output*`][e7ee], [`*error-output*`][66c6],
[`*trace-output*`][2243], [`*debug-io*`][10ff], [`*query-io*`][f4bf] and
[`*terminal-io*`][633c] are redirected to capture the
output.

If `transcribe` is invoked [by Emacs][f5bd] or
[by the `document` function][9bd5], a new
dynamic environment is established by
[`standard-transcribe-dynenv`][588c] (similar in spirit to
[`with-standard-io-syntax`][39df]), which binds printer and reader variables
to fixed values to make IO more predictable. This default can be
overridden with the `:dynenv` argument of `cl-transcript`:

    ```cl-transcript (:dynenv my-transcript)
    ...
    ```

In this case, instead of calling `transcribe` directly,
`my-transcript` is called with a thunk (a function of no arguments)
that wraps a call to `transcribe`. Once `my-transcript` establishes
the desired dynamic environment, it calls its argument. The
following definition of `my-transcript` simply packages up oft-used
settings to `transcribe`.

```
(defun my-transcript (fn)
  (standard-transcribe-dynenv
    (let ((*transcribe-check-consistency*
            '((:output my-transcript-output=)
              (:readable equal)
              (:unreadable nil))))
      (funcall fn))))

(defun my-transcript-output= (string1 string2)
  (string= (my-transcript-normalize-output string1)
           (my-transcript-normalize-output string2)))

(defun my-transcript-normalize-output (string)
  (squeeze-whitespace (delete-trailing-whitespace (delete-comments string))))
```

The default for `:dynenv` is `standard-transcribe-dynenv`, and it is
generally a good idea to use it as the above example does, but this
is not required. Specify `:dynenv` `nil` explicitly if you want to use
the current dynamic environment without any changes.

A more involved solution could establish bindings for global
variables set in transcripts, unintern symbols created or even
create a temporary package for evaluation.

<a id="x-28MGL-PAX-3ASTANDARD-TRANSCRIBE-DYNENV-20FUNCTION-29"></a>
<a id="MGL-PAX:STANDARD-TRANSCRIBE-DYNENV%20FUNCTION"></a>

- [function] **standard-transcribe-dynenv** *fn*

    Bind printer and reader variables to standard values and call `fn`.
    
    The bindings are the same as with [`with-standard-io-syntax`][39df], but
    
    - [`*package*`][5ed1] and [`*readtable*`][b79a] are unaffected (because both [`document`][432c]
      and Emacs set these up);
    
    - [`*print-readably*`][8aca] is `nil`, [`*print-pretty*`][782a] is `t`, [`*print-circle*`][c8cb] is `t`
      and [`*print-right-margin*`][5ab6] is 72.
    
    This function is the default for the `:dynenv` argument of
    `cl-transcript`. A function that overrides the default may want to
    call `standard-transcribe-dynenv` with a lambda that establishes more
    bindings.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-API%20MGL-PAX:SECTION"></a>

### 9.4 Transcript API

<a id="x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29"></a>
<a id="MGL-PAX:TRANSCRIBE%20FUNCTION"></a>

- [function] **transcribe** *input output &key update-only (include-no-output update-only) (include-no-value update-only) (echo t) (check-consistency \*transcribe-check-consistency\*) default-syntax (input-syntaxes \*transcribe-syntaxes\*) (output-syntaxes \*transcribe-syntaxes\*)*

    Read forms from `input` and write them (iff `echo`) to `output`
    followed by any output and return values produced by calling [`eval`][0d6e] on
    the form.
    
    `input` can be a stream or a string, while `output` can be a stream or
    `nil`, in which case output goes into a string.
    The return value is the `output` stream or the string that was
    constructed. As the second value, a generalized boolean indicating
    whether any form was transcribed is returned.
    
    Go up to [Transcribing with Emacs][f5bd] for nice examples. A more
    mind-bending one is this:
    
    ```common-lisp
    (transcribe "(princ 42) " nil)
    => "(princ 42)
    .. 42
    => 42
    "
    => T
    ```
    
    However, the above may be a bit confusing since this documentation
    uses `transcribe` markup syntax in this very example, so let's do it
    differently. If we have a file with these contents:
    
    ```
    (values (princ 42) (list 1 2))
    ```
    
    it is transcribed to:
    
    ```
    (values (princ 42) (list 1 2))
    .. 42
    => 42
    => (1 2)
    ```
    
    Output to all standard streams is captured and printed with
    the `:output` prefix (`".."`). The return values above are printed
    with the `:readable` prefix (`"=>"`). Note how these prefixes are
    always printed on a new line to facilitate parsing.
    
    **Updating**
    
    `transcribe` is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:update-only` `t` with source:
    
    ```
    (values (princ 42) (list 1 2))
    =>
    ```
    
    we get this:
    
    ```
    (values (princ 42) (list 1 2))
    => 42
    => (1 2)
    ```
    
    With `update-only`, the printed output of a form is transcribed only
    if there were output markers in the source. Similarly, with
    `update-only`, return values are transcribed only if there were value
    markers in the source.
    
    **No Output/Values**
    
    If the form produces no output or returns no values, then whether or
    not output and values are transcribed is controlled by
    `include-no-output` and `include-no-value`, respectively. By default,
    neither is on so:
    
    ```
    (values)
    ..
    =>
    ```
    
    is transcribed to
    
    ```
    (values)
    ```
    
    With `update-only` true, we probably wouldn't like to lose those
    markers since they were put there for a reason. Hence, with
    `update-only`, `include-no-output` and `include-no-value` default to true.
    So, with `update-only` the above example is transcribed to:
    
    ```
    (values)
    ..
    => ; No value
    ```
    
    where the last line is the `:no-value` prefix.
    
    **Consistency Checks**
    
    If `check-consistency` is true, then `transcribe` signals a continuable
    [`transcription-output-consistency-error`][8492] whenever a form's output as a
    string is different from what was in `input`, provided that `input`
    contained the output. Similarly, for values, a continuable
    [`transcription-values-consistency-error`][238c] is signalled if a value read
    from the source does not print as the as the value returned by `eval`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```
    (list 1 2)
    => ;; This is commented, too.
       (1
          ;; Funny indent.
          2)
    ```
    
    See [Transcript Consistency Checking][4c39] for the full picture.
    
    **Unreadable Values**
    
    The above scheme involves [`read`][fe58], so consistency of unreadable values
    cannot be treated the same. In fact, unreadable values must even be
    printed differently for transcribe to be able to read them back:
    
    ```
    (defclass some-class () ())
    
    (defmethod print-object ((obj some-class) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~%~%end")))
    
    (make-instance 'some-class)
    ==> #<SOME-CLASS 
    -->
    --> end>
    ```
    
    where `"==>"` is the `:unreadable` prefix and `"-->"` is the
    `:unreadable-continuation` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `eval` is performed with [`string=`][4143] by default. That is, the value from
    `eval` is printed to a string and compared to the source value. Hence,
    any change to unreadable values will break consistency checks. This
    is most troublesome with instances of classes with the default
    [`print-object`][3f2e] method printing the memory address. See
    [Finer-Grained Consistency Checks][6e18].
    
    **Errors**
    
    If an [`error`][d162] condition is signalled, the error is printed to the
    output and no values are returned.
    
    ```common-lisp
    (progn
      (print "hello")
      (error "no greeting"))
    ..
    .. "hello" 
    .. debugger invoked on SIMPLE-ERROR:
    ..   no greeting
    ```
    
    To keep the textual representation somewhat likely to be portable,
    the printing is done with `(format t "#<~S ~S>" (type-of
    error) (princ-to-string error))`. [`simple-condition`][f2f5]s are formatted to
    strings with [`simple-condition-format-control`][4841] and
    [`simple-condition-format-arguments`][da14].
    
    **Syntaxes**
    
    Finally, a transcript may employ different syntaxes for the output
    and values of different forms. When `input` is read, the syntax for
    each form is determined by trying to match all prefixes from all
    syntaxes in `input-syntaxes` against a line. If there are no output or
    values for a form in `input`, then the syntax remains undetermined.
    
    When `output` is written, the prefixes to be used are looked up in
    `default-syntax` of `output-syntaxes` if `default-syntax` is not `nil`. If
    `default-syntax` is `nil`, then the syntax used by the same form in the
    `input` is used or (if that could not be determined) the syntax of the
    previous form. If there was no previous form, then the first syntax
    if `output-syntaxes` is used.
    
    To produce a transcript that's executable Lisp code,
    use `:default-syntax` `:commented-1`:
    
    ```
    (make-instance 'some-class)
    ;==> #<SOME-CLASS
    ;-->
    ;--> end>
    
    (list 1 2)
    ;=> (1
    ;->    2)
    ```
    
    To translate the above to uncommented syntax, use `:default-syntax`
    `:default`. If `default-syntax` is `nil` (the default), the same syntax
    will be used in the output as in the input as much as possible.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-CHECK-CONSISTENCY-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*TRANSCRIBE-CHECK-CONSISTENCY*%20VARIABLE"></a>

- [variable] **\*transcribe-check-consistency\*** *nil*

    The default value of [`transcribe`][f1f0]'s `check-consistency` argument.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*TRANSCRIBE-SYNTAXES*%20VARIABLE"></a>

- [variable] **\*transcribe-syntaxes\*** *((:default (:output "..") (:no-value "=> ; No value") (:readable "=>")
  (:unreadable "==>") (:unreadable-continuation "-->"))
 (:commented-1 (:output ";..") (:no-value ";=> ; No value") (:readable ";=>")
  (:readable-continuation ";->") (:unreadable ";==>")
  (:unreadable-continuation ";-->"))
 (:commented-2 (:output ";;..") (:no-value ";;=> ; No value")
  (:readable ";;=>") (:readable-continuation ";;->") (:unreadable ";;==>")
  (:unreadable-continuation ";;-->")))*

    The default syntaxes used by [`transcribe`][f1f0] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(syntax-id &rest prefixes)` where
    `prefixes` is a list of `(prefix-id prefix-string)` elements. For
    example the syntax `:commented-1` looks like this:
    
    ```
    (:commented-1
     (:output ";..")
     (:no-value ";=>  No value")
     (:readable ";=>")
     (:readable-continuation ";->")
     (:unreadable ";==>")
     (:unreadable-continuation ";-->"))
    ```
    
    All of the above prefixes must be defined for every syntax except
    for `:readable-continuation`. If that's missing (as in the `:default`
    syntax), then the following value is read with [`read`][fe58] and printed with
    [`prin1`][6384] (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix is discarded when reading.
    
    See `transcribe` for how the actual syntax to be used is selected.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-ERROR%20CONDITION"></a>

- [condition] **transcription-error** *[error][d162] condition-context-mixin*

    Represents syntactic errors in the `source` argument
    of [`transcribe`][f1f0] and also serves as the superclass of
    [`transcription-consistency-error`][a249].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **transcription-consistency-error** *[transcription-error][b81d]*

    A common superclass for
    [`transcription-output-consistency-error`][8492] and
    [`transcription-values-consistency-error`][238c].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **transcription-output-consistency-error** *[transcription-consistency-error][a249]*

    Signalled (with [`cerror`][4317]) by [`transcribe`][f1f0] when invoked
    with `:check-consistency` and the output of a form is not the same as
    what was parsed.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **transcription-values-consistency-error** *[transcription-consistency-error][a249]*

    Signalled (with [`cerror`][4317]) by [`transcribe`][f1f0] when invoked
    with `:check-consistency` and the values of a form are inconsistent
    with their parsed representation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-CONSISTENCY-CHECKING%20MGL-PAX:SECTION"></a>

### 9.5 Transcript Consistency Checking

The main use case for consistency checking is detecting
out-of-date examples in documentation, although using it for writing
tests is also a possibility. Here, we focus on the former.

When a Markdown code block tagged `cl-transcript` is processed
during [Generating Documentation][2c93], the code in it is replaced with
the output of with `(transcribe <code> nil :update-only t
:check-consistency t)`. Suppose we have the following example of the
function `greet`, that prints `hello` and returns 7.

    ```cl-transcript
    (greet)
    .. hello
    => 7
    ```

Now, if we change `greet` to print or return something else, a
[`transcription-consistency-error`][a249] will be signalled during
documentation generation. Then we may fix the documentation or
[`continue`][1867] from the error.

By default, comparisons of previous to current output, readable and
unreadable return values are performed with [`string=`][4143], [`equal`][3fb5], and
`string=`, respectively, which is great in the simple case.
Non-determinism aside, exact matching becomes brittle as soon as the
notoriously unportable pretty printer is used or when unreadable
objects are printed with their `#<>` syntax, especially when
[`print-unreadable-object`][9439] is used with `:identity t`.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS%20MGL-PAX:SECTION"></a>

#### 9.5.1 Finer-Grained Consistency Checks

To get around this problem, consistency checking of output,
readable and unreadable values can be customized individually by
supplying [`transcribe`][f1f0] with a `check-consistency` argument
like `((:output <output-check>) (:readable
<readable-check>) (:unreadable <unreadable-check>))`. In this case,
`<output-check>` may be `nil`, `t`, or a function designator.

- If it's `nil` or there is no `:output` entry in the list, then the
  output is not checked for consistency.

- If it's `t`, then the outputs are compared with the default,
  [`string=`][4143].

- If it's a function designator, then it's called with two strings
  and must return whether they are consistent with each other.

The case of `<readable-check>` and `<unreadable-check>` is similar.

Code blocks tagged `cl-transcript` can take arguments, which they
pass on to `transcribe`. The following shows how to check only the
output.

    ```cl-transcript (:check-consistency ((:output t)))
    (error "Oh, no.")
    .. debugger invoked on SIMPLE-ERROR:
    ..   Oh, no.
    
    (make-condition 'simple-error)
    ==> #<SIMPLE-ERROR {1008A81533}>
    ```

It is often a good idea to package up these settings in the
`:dynenv` argument of `cl-transcript` (see [Controlling the Dynamic Environment][6b59]).

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING%20MGL-PAX:SECTION"></a>

#### 9.5.2 Utilities for Consistency Checking

<a id="x-28MGL-PAX-3ASQUEEZE-WHITESPACE-20FUNCTION-29"></a>
<a id="MGL-PAX:SQUEEZE-WHITESPACE%20FUNCTION"></a>

- [function] **squeeze-whitespace** *string*

    Replace consecutive whitespace characters with a single space in
    `string` and trim whitespace from the right. This is useful to undo
    the effects of pretty printing when building comparison functions
    for [`transcribe`][f1f0].

<a id="x-28MGL-PAX-3ADELETE-TRAILING-WHITESPACE-20FUNCTION-29"></a>
<a id="MGL-PAX:DELETE-TRAILING-WHITESPACE%20FUNCTION"></a>

- [function] **delete-trailing-whitespace** *string*

    Delete whitespace characters after the last non-whitespace
    character in each line in `string`.

<a id="x-28MGL-PAX-3ADELETE-COMMENTS-20FUNCTION-29"></a>
<a id="MGL-PAX:DELETE-COMMENTS%20FUNCTION"></a>

- [function] **delete-comments** *string &key (pattern ";")*

    For each line in `string` delete the rest of the line after and
    including the first occurrence of `pattern`. On changed lines, delete
    trailing whitespace too. This function does not parse `string` as Lisp
    forms, hence all occurrences of `pattern` (even those seemingly in
    string literals) are recognized as comments.
    
    Let's define a comparison function:
    
    ```common-lisp
    (defun string=/no-comments (string1 string2)
      (string= (delete-comments string1) (delete-comments string2)))
    ```
    
    And use it to check consistency of output:
    
        ```cl-transcript (:check-consistency ((:output string=/no-comments)))
        (format t "hello~%world")
        .. hello     ; This is the first line.
        .. world     ; This is the second line.
        ```
    
    Just to make sure the above example works, here it is without being
    quoted.
    
    ```common-lisp
    (format t "hello~%world")
    .. hello     ; This is the first line.
    .. world     ; This is the second line.
    ```

<a id="x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PARSING%20MGL-PAX:SECTION"></a>

## 10 Parsing

<a id="x-28MGL-PAX-3A-40PARSING-NAMES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PARSING-NAMES%20MGL-PAX:SECTION"></a>

### 10.1 Parsing Names

When encountering a [word][d7b0] such as [`CLASSes`][1f37] in a docstring, PAX
needs to find the [name][88cf], and how that's done varies slightly.
[Codification][f1ab], for example, looks for [interesting][7445] names,
[Navigating Sources in Emacs][3386] for names with [Lisp][30ad] [`definitions`][e196], and [Linking][19e3] for names with [any kind of
definition][99b0].

This is not as straightforward as it sounds because it needs to
handle cases like un[`read`][fe58]able, [`print`][d451]ed, and all the various forms of
[Linking][19e3] in docstrings as well as in comments, and the `(name
locative)` syntax in [`defsection`][72b4].

<a id="x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@WORD%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **word**

    A *word* is a string from which we want to extract a [name][88cf]. When
    [Navigating][3386], the word is
    `slime-sexp-at-point` or the label of a [Markdown reference link][8c00] if point
    is over one. Similarly, when [Generating Documentation][2c93], it is a
    non-empty string between whitespace characters in a docstring or the
    label of a [Markdown reference link][8c00].

<a id="x-28MGL-PAX-3A-40RAW-NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@RAW-NAME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **raw name**

    A *raw name* is a string from which a [name][88cf] may be read. Raw names
    correspond to an intermediate parsing step between [word][d7b0]s and
    [name][88cf]s. See [Names in Raw Names][016d].

<a id="x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@NAME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **name**

    A *name* is a [DRef name][5fc4]. That is, a symbol, a
    string or a nested list of the previous associated with a
    definition, whose kind is given by a [locative][7ac8].

Depending on the context, trimming and depluralization may be
enabled (see [Raw Names in Words][f0d5]), while the possible names may be
restricted to symbols (see [Names in Raw Names][016d]).

- *Trimming:* Enabled for [`M-.` Defaulting][460e] and [Codification][f1ab].

- *Depluralization:* Enabled when the [word][d7b0] is part of the normal
  flow of text (i.e. not for [Specific Reflink with Text][fb17],
  [Unspecific Reflink with Text][34b8] and various Elisp functions such as
  `mgl-pax-apropos` unless they determine their argument from buffer
  contents).

- *Symbols only:* This is the case for [Codification][f1ab] and
  [Unspecific Autolink][e2a4] to prevent string-based definitions from
  littering the documentation with links without the control
  provided by explicitly [`import`][8f46]ing symbols.


For a word, a number of [raw name][f5af]s is generated by trimming
delimiter characters and plural markers, and for each raw name a
number of names are considered until one is found suitable in the
context. The following subsections describe the details of the
parsing algorithm.

<a id="x-28MGL-PAX-3A-40RAW-NAMES-IN-WORDS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@RAW-NAMES-IN-WORDS%20MGL-PAX:SECTION"></a>

#### 10.1.1 Raw Names in Words

From [word][d7b0]s, [raw name][f5af]s are parsed by trimming some
prefixes and suffixes. For a given word, multiple raw names are
considered in the following order.

1. The entire word.

2. Trimming the following characters from the left of the word:

        #<{;"'`

3. Trimming the following characters from the right of the word:

        ,;:.>}"'`

4. Trimming both of the previous two at the same time.

5. From the result of 4., If a [word][d7b0] ends with what looks like a plural marker (case-insensitive),
then a [name][88cf] is created by removing it. For example, from the word
`buses` the plural marker `es` is removed to produce the name `bus`.
The list of plural markers considered is `ses` (e.g. `gasses`),
`es` (e.g. `buses`), `s` (e.g. `cars`), `zes` (e.g. `fezzes`), and
`ren` (e.g. `children`).

6. From the result of 4., removing the prefix before the first, and the suffix after the last
uppercase character if it contains at least one lowercase character.


<a id="x-28MGL-PAX-3A-40NAMES-IN-RAW-NAMES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@NAMES-IN-RAW-NAMES%20MGL-PAX:SECTION"></a>

#### 10.1.2 Names in Raw Names

For each [raw name][f5af] from [Raw Names in Words][f0d5], various [name][88cf]s
may be considered until one is found that's suitable in the context.

The following examples list the names considered for a given raw
name, assuming that [`readtable-case`][48f1] is `:upcase` as well as that `foo`
and `|Foo|` are interned.

- `"foo"`: `foo`, `"foo"`, `"FOO"` (rules 1, 2, 3)

- `"FOO"`: `foo`, `"FOO"` (rules 1, 2)

- `"Foo"`: `"Foo"`, `"FOO"` (rules 2, 3)

- `"|Foo|"`: `|Foo|` (rule 4)

- `"\"foo\""`: `"foo"` (rule 5)

The rules are:

1. If the raw name is not mixed case (i.e. it doesn't have both
   upper- and lowercase characters) and it names an interned
   symbol (subject to the current [Package and Readtable][ab7e]), then that
   symbol is considered as a name.

2. The raw name itself (a string) is considered a name.

3. The raw name upcased or downcased according to
   `readtable-case` (subject to the [current
   readtable][ab7e]) but still as a string. This
   is to allow `[dref][package]` to refer to the `"DREF"`
   package regardless of whether the symbol `dref` is interned in
   the current package.

4. If the raw name is explicitly a symbol (it starts with `#\|`),
   and it names an interned symbol (subject to the current
   [Package and Readtable][ab7e]), then that symbol is considered as a name
   and nothing else.

5. If the raw name has an embedded string (it starts with `#\"`) and
   [`read-from-string`][d813] can read the embedded string from it, then that
   string is considered as a name and nothing else.


<br/>
For example, when `M-.` is pressed while point is over
`unREADable.`, the last word of the sentence `It may be
unREADable.`, the following [raw name][f5af]s are considered until one is
found with a definition:

1. The entire word, `"unREADable."`.

2. Trimming left does not produce a new raw name.

3. Trimming right removes the dot and gives `"unREADable"`.

4. Trimming both is the same as trimming right.

5. No plural markers are found.

6. The lowercase prefix and suffix is removed around the uppercase
   core, giving `"READ"`. This names an interned symbol which has a
   definition, so `M-.` will visit it.

When [Generating Documentation][2c93], [Autolink][ec7a]ing behaves similarly.

<a id="x-28MGL-PAX-3A-40PARSING-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PARSING-LOCATIVES%20MGL-PAX:SECTION"></a>

### 10.2 Parsing Locatives

Locatives are parsed almost as if by [`read`][fe58]. They are found in
buffer contents around a [word][d7b0] when [`M-.` Defaulting][460e] or
[Generating Documentation][2c93], and in the string entered when
[`M-.` Prompting][ed46], with a similar distinction when
[Browsing Live Documentation][a595].

Parsing deviates from `read` in the following ways.

- No new symbols are [`intern`][b4f0]ed during parsing. If an expression
  contains uninterned symbols, then it is not parsable as a
  locative.

- Read-time evaluation ([#.][ffd7]) follows normal `read` semantics.
  Thus, `(method ((eql #.(find-package 'cl))))` may `intern` the
  symbol `cl`.

- A locative that involves unreadable objects that print using the
  `#<` syntax (e.g. `(method ((eql #<package dref>)))`) is parsable
  in the context of a [name][88cf] if each unreadable object in the
  locative occurs in one of the [`definitions`][e196] of that name and it
  [prints to an equivalent string][2f21] (e.g. `#<package dref>` above).


<a id="x-28MGL-PAX-3A-40PRINTS-TO-AN-EQUIVALENT-STRING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@PRINTS-TO-AN-EQUIVALENT-STRING%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **prints to an equivalent string**

    An object with an unreadable representation is said to print to
    some string `s` if its [`prin1`][6384] representation (under [`with-standard-io-syntax`][39df]
    but in the current package and with [`*print-readably*`][8aca] `nil`) is the same as `s`, where consecutive whitepace characters are
    replaced with a single space in both strings, and the comparison is case-insensitive.
    
    See the related concept of [stable printed locative][3942], that requires
    the printed representation of entire locatives to be unique and
    non-changing to support [Linking][19e3].

<a id="x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EXTENSION-API%20MGL-PAX:SECTION"></a>

## 11 Writing Extensions

<a id="x-28MGL-PAX-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@ADDING-NEW-LOCATIVES%20MGL-PAX:SECTION"></a>

### 11.1 Adding New Locatives

Once everything in [Extending DRef][68fb] has been done,
there are only a couple of PAX generic functions left to extend.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:DOCUMENT-OBJECT*%20GENERIC-FUNCTION"></a>

- [generic-function] **document-object\*** *object stream*

    Write `object` in [`*format*`][3da8] to `stream`.
    Specialize this on a subclass of [`dref`][d930] if that subclass is
    not [`resolve`][63b4]able, else on the type of object it resolves to. This
    function is for extension only. Don't call it directly.

<a id="x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:EXPORTABLE-REFERENCE-P%20GENERIC-FUNCTION"></a>

- [generic-function] **exportable-reference-p** *package symbol locative-type locative-args*

    Return true if `symbol` is to be exported from
    `package` when it occurs in a [`defsection`][72b4] in a reference with
    `locative-type` and `locative-args`. `symbol` is [accessible][9b15] in
    `package`.
    
    The default method calls [`exportable-locative-type-p`][c930] with
    `locative-type` and ignores the other arguments.
    
    By default, [`section`][5fac]s, [`glossary-term`][8251]s, `concept`s and [`note`][e2ae]s are not
    exported although they are `exportable-locative-type-p`. To export
    symbols naming sections from MGL-PAX, the following method could be
    added:
    
    ```
    (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                       symbol (locative-type (eql 'section))
                                       locative-args)
      t)
    ```

<a id="x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P%20GENERIC-FUNCTION"></a>

- [generic-function] **exportable-locative-type-p** *locative-type*

    Return true if symbols in references with
    `locative-type` are to be exported by default when they occur in a
    [`defsection`][72b4]. The default method returns `t`, while the methods for
    locative types [`package`][4dd7], [`method`][172e],
    [`asdf:system`][c097], and [`include`][5cd7] return `nil`.
    
    This function is called by the default method of
    [`exportable-reference-p`][e51f] to decide what symbols `defsection` shall
    export when its `export` argument is true.

Also note that due to the [`home-section`][fda4] logic, especially for
locative types with string names, [`dref-ext:docstring*`][9fd4] should
probably return a non-`nil` package.

<a id="x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCATIVE-ALIASES%20MGL-PAX:SECTION"></a>

### 11.2 Locative Aliases

[`define-locative-alias`][548e] can be used to help [`M-.`][3386] and [Specific Autolink][38de]s disambiguate
references based on the context of a [name][88cf] as described on [Parsing][378f].

The following example shows how to make docstrings read
more naturally by defining an alias.

```
(defclass my-string ()
  ())

(defgeneric my-string (obj)
  (:documentation "Convert OBJ to MY-STRING."))

;;; This version of FOO has a harder to read docstring because
;;; it needs to disambiguate the MY-STRING reference.
(defun foo (x)
  "FOO takes an argument X, a [MY-STRING][class] object.")

;;; Define OBJECT as an alias for the CLASS locative.
(define-locative-alias object class)

;;; Note how no explicit link is needed anymore.
(defun foo (x)
  "FOO takes an argument X, a MY-STRING object.")
```

Similarly, defining the indefinite articles as aliases of the [`class`][2060]
locative can reduce the need for explicit linking.

```
(define-locative-alias a class)
(define-locative-alias an class)
```

Since these are unlikely to be universally helpful, make sure not to
export the symbols `a` and `an`.

<a id="x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EXTENDING-DOCUMENT%20MGL-PAX:SECTION"></a>

### 11.3 Extending `document`

For all definitions that it encounters, [`document`][432c] calls
[`document-object*`][8269] to generate documentation. The following utilities
are for writing new `document-object*` methods, which emit Markdown.

<a id="x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*FORMAT*%20VARIABLE"></a>

- [variable] **\*format\***

    Bound by [`document`][432c] to its `format` argument, this allows Markdown
    output to depend on the output format.

<a id="x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:WITH-HEADING%20MGL-PAX:MACRO"></a>

- [macro] **with-heading** *(stream &key dref link-title-to) &body body*

    Write a Markdown heading with the [`doctitle`][e619] of `dref` to `stream`.
    
    - `dref` defaults to the definition for which documentation is
      currently being generated.
    
    - Nested `with-heading`s produce nested headings.
    
    - If [`*document-link-sections*`][1b28], generate anchors based on `dref`.
    
    - `link-title-to` behaves like the `link-title-to` argument of
      [`defsection`][72b4].

<a id="x-28MGL-PAX-3ADOCTITLE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:DOCTITLE*%20GENERIC-FUNCTION"></a>

- [generic-function] **doctitle\*** *object*

    `doctitle*` extends [`doctitle`][e619] in the same way
    as [`docstring*`][9fd4] extends [`docstring`][affc].
    
    The default method returns `nil`.
    
    This function is for extension only. Do not call it directly.

<a id="x-28MGL-PAX-3ADOCUMENTING-DEFINITION-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DOCUMENTING-DEFINITION%20MGL-PAX:MACRO"></a>

- [macro] **documenting-definition** *(stream &key dref package readtable (arglist nil)) &body body*

    Write `dref` to `stream` as described in
    [`*document-mark-up-signatures*`][8fb6], and establish `dref` as a
     [Local Definition][9db9] for the processing of `body`.
    
    - `dref` defaults to the definition for which documentation is
      currently being generated.
    
    - If `dref` has a [`doctitle`][e619], then it is [`princ`][676d]ed after the
      [`locative-type`][97ba] (see [Markdown in Titles][165c]). Else, `(dref-name dref)`
      is printed subject to [`*document-downcase-uppercase-code*`][a5ee] but with
      all Markdown and [MathJax][a17d] markup escaped.
    
    - [`*package*`][5ed1] and [`*readtable*`][b79a] are bound to `package` and `readtable` for
      the duration of printing the `arglist` and the processing of `body`.
      If either is `nil`, then a default value is computed as described in
      [Package and Readtable][ab7e].
    
    - `arglist`:
    
        - If it is not provided, then it defaults to (`arglist` `dref`).
    
        - If `nil`, then it is not printed.
    
        - If it is a list, then it is must be a [lambda list][98ff] and
          is printed without the outermost parens and with the package
          names removed from the argument names. [Keywords][88a0] are
          printed with a leading colon.
    
        - If it is a string, then it must be valid Markdown.
    
    - It is not allowed to have [`with-heading`][80e8] within the [dynamic
      extent][36e9] of `body`.

<a id="x-28MGL-PAX-3AWITH-DISLOCATED-NAMES-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:WITH-DISLOCATED-NAMES%20MGL-PAX:MACRO"></a>

- [macro] **with-dislocated-names** *names &body body*

    For each name in `names`, establish a [Local Definition][9db9].

<a id="x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29"></a>
<a id="MGL-PAX:DOCUMENT-DOCSTRING%20FUNCTION"></a>

- [function] **document-docstring** *docstring stream &key (indentation "    ") exclude-first-line-p (paragraphp t)*

    Write `docstring` to `stream`, [sanitizing the Markdown][7bf5] from it, performing [Codification][f1ab] and
    [Linking][19e3], finally prefixing each line with `indentation`. The prefix
    is not added to the first line if `exclude-first-line-p`. If
    `paragraphp`, then add a newline before and after the output.

<a id="x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29"></a>
<a id="MGL-PAX:ESCAPE-MARKDOWN%20FUNCTION"></a>

- [function] **escape-markdown** *string &key (escape-inline t) (escape-mathjax t) (escape-html t) (escape-block t)*

    Backslash escape Markdown constructs in `string`.
    
    - If `escape-inline`, then escape the following characters:
    
            *_`[]\
    
    - If `escape-mathjax`, then escape `$` characters.
    
    - If `escape-html`, then escape the following characters:
    
            <&
    
    - If `escape-block`, then escape whatever is necessary to avoid
      starting a new Markdown block (e.g. a paragraph, heading, etc).

<a id="x-28MGL-PAX-3APRIN1-TO-MARKDOWN-20FUNCTION-29"></a>
<a id="MGL-PAX:PRIN1-TO-MARKDOWN%20FUNCTION"></a>

- [function] **prin1-to-markdown** *object &key (escape-inline t) (escape-mathjax t) (escape-html t) (escape-block t)*

    Like [`prin1-to-string`][18e1], but bind [`*print-case*`][443b] depending on
    [`*document-downcase-uppercase-code*`][a5ee] and [`*format*`][3da8], and
    [`escape-markdown`][3026].

<a id="x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SECTIONS%20MGL-PAX:SECTION"></a>

### 11.4 Sections

[`section`][5fac] objects rarely need to be dissected since
[`defsection`][72b4] and [`document`][432c] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-28MGL-PAX-3ASECTION-20CLASS-29"></a>
<a id="MGL-PAX:SECTION%20CLASS"></a>

- [class] **section**

    [`defsection`][72b4] stores its `name`, `title`, [`package`][1d5a],
    [`readtable`][d646] and `entries` arguments in [`section`][5fac]
    objects.

<a id="x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-NAME%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **section-name** *[section][5fac] (:name)*

    The name of the global variable whose value is
    this [`section`][5fac] object.

<a id="x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-PACKAGE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **section-package** *[section][5fac] (:package)*

    [`*package*`][5ed1] will be bound to this package when
    generating documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-READTABLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **section-readtable** *[section][5fac] (:readtable)*

    [`*readtable*`][b79a] will be bound to this when generating
    documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-TITLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **section-title** *[section][5fac] (:title)*

    A [title][090e] or `nil`. Used in generated
    documentation (see [Markdown Output][dd29]) and is returned by [`doctitle`][e619]
    for [`section`][5fac] objects and `section` [definition][2143]s.

<a id="x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20FUNCTION-29"></a>
<a id="MGL-PAX:SECTION-LINK-TITLE-TO%20FUNCTION"></a>

- [function] **section-link-title-to** *section*

<a id="x-28MGL-PAX-3ASECTION-ENTRIES-20FUNCTION-29"></a>
<a id="MGL-PAX:SECTION-ENTRIES%20FUNCTION"></a>

- [function] **section-entries** *section*

    A list of Markdown docstrings and [`xref`][1538]s in the order they
    occurred in [`defsection`][72b4].

<a id="x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@GLOSSARY-TERMS%20MGL-PAX:SECTION"></a>

### 11.5 Glossary Terms

[`glossary-term`][8251] objects rarely need to be dissected since
[`define-glossary-term`][8ece] and [`document`][432c] cover most needs. However, it is
plausible that one wants to subclass them and maybe redefine how
they are presented.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM%20CLASS"></a>

- [class] **glossary-term**

    See [`define-glossary-term`][8ece].

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM-NAME%20%28MGL-PAX:READER%20MGL-PAX:GLOSSARY-TERM%29"></a>

- [reader] **glossary-term-name** *[glossary-term][8251] (:name)*

    The name of the global variable whose value is
    this [`glossary-term`][8251] object.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM-TITLE%20%28MGL-PAX:READER%20MGL-PAX:GLOSSARY-TERM%29"></a>

- [reader] **glossary-term-title** *[glossary-term][8251] (:title)*

    A [title][090e] or `nil`. Used in generated
    documentation (see [Markdown Output][dd29]) and is returned by [`doctitle`][e619]
    for [`glossary-term`][8251] objects and `glossary-term` [definition][2143]s..

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-URL-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM-URL%20%28MGL-PAX:READER%20MGL-PAX:GLOSSARY-TERM%29"></a>

- [reader] **glossary-term-url** *[glossary-term][8251] (:url)*

    A string or `nil`.

  [00d4]: dref-manual.md#MGL-PAX:ACCESSOR%20MGL-PAX:LOCATIVE "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [0165]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cic.htm "\"22.3.9.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [016d]: #MGL-PAX:@NAMES-IN-RAW-NAMES%20MGL-PAX:SECTION "Names in Raw Names"
  [021a]: dref-manual.md#%22dref%22%20ASDF%2FSYSTEM:SYSTEM "\"dref\" ASDF/SYSTEM:SYSTEM"
  [02e3]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cib.htm "\"22.3.9.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0317]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME (MGL-PAX:CLHS CLASS)"
  [0361]: #MGL-PAX:@SPECIFIC-LINK%20MGL-PAX:SECTION "Specific Link"
  [040b]: http://www.lispworks.com/documentation/HyperSpec/Body/02_da.htm "\"2.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [051b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chb.htm "\"22.3.8.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0617]: dref-manual.md#DREF:XREF%3D%20FUNCTION "DREF:XREF= FUNCTION"
  [0684]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cac.htm "\"22.3.1.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0702]: #MGL-PAX:@DOCUMENTABLE%20MGL-PAX:SECTION "`documentable`"
  [090c]: dref-manual.md#MGL-PAX:STRUCTURE-ACCESSOR%20MGL-PAX:LOCATIVE "MGL-PAX:STRUCTURE-ACCESSOR MGL-PAX:LOCATIVE"
  [090e]: #MGL-PAX:@TITLE%20MGL-PAX:GLOSSARY-TERM "title"
  [0b3a]: dref-manual.md#MGL-PAX:LOCATIVE%20MGL-PAX:LOCATIVE "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0c4f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT (MGL-PAX:CLHS FUNCTION)"
  [0c7e]: dref-manual.md#%22dref%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM "\"dref/full\" ASDF/SYSTEM:SYSTEM"
  [0cac]: http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm "\"22.3.1.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0d07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm "SYMBOL-NAME (MGL-PAX:CLHS FUNCTION)"
  [0d6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL (MGL-PAX:CLHS FUNCTION)"
  [0db0]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgf.htm "\"22.3.7.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0ef0]: #MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*%20VARIABLE "MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS* VARIABLE"
  [0f42]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dht.htm "\"2.4.8.20\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0f52]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#top_level_form "\"top level form\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [0fa3]: #MGL-PAX:@LOCATIVE-ALIASES%20MGL-PAX:SECTION "Locative Aliases"
  [10ff]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*DEBUG-IO* (MGL-PAX:CLHS VARIABLE)"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [11f1]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm "\"22.3.6.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [1281]: #MGL-PAX:@PAX-WORLD%20MGL-PAX:SECTION "PAX World"
  [130a]: dref-manual.md#DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS%20FUNCTION "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS FUNCTION"
  [1322]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks "fenced code blocks"
  [13a9]: #MGL-PAX:UPDATE-ASDF-SYSTEM-READMES%20FUNCTION "MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION"
  [13c7]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#atomic "\"atomic\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [142c]: http://www.lispworks.com/documentation/HyperSpec/Body/06_aba.htm "\"6.1.2.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [1538]: dref-manual.md#DREF:XREF%20CLASS "DREF:XREF CLASS"
  [1539]: https://quicklisp.org/ "Quicklisp"
  [1567]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccb.htm "\"22.3.3.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [165c]: #MGL-PAX:@MARKDOWN-IN-TITLES%20MGL-PAX:SECTION "Markdown in Titles"
  [172e]: dref-manual.md#METHOD%20MGL-PAX:LOCATIVE "METHOD MGL-PAX:LOCATIVE"
  [1743]: https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding "w3m's default key bindings"
  [17e0]: #MGL-PAX:*DOCUMENT-URL-VERSIONS*%20VARIABLE "MGL-PAX:*DOCUMENT-URL-VERSIONS* VARIABLE"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [18e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRIN1-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [1904]: https://github.com/3b/3bmd "3BMD"
  [1959]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cec.htm "\"22.3.5.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [19ad]: #MGL-PAX:@PDF-OUTPUT%20MGL-PAX:SECTION "PDF Output"
  [19e3]: #MGL-PAX:@LINKING%20MGL-PAX:SECTION "Linking"
  [1aed]: #MGL-PAX:INSTALL-PAX-ELISP%20FUNCTION "MGL-PAX:INSTALL-PAX-ELISP FUNCTION"
  [1b1b]: #MGL-PAX:@DOCUMENTATION-UTILITIES%20MGL-PAX:SECTION "Utilities for Generating Documentation"
  [1b28]: #MGL-PAX:*DOCUMENT-LINK-SECTIONS*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-SECTIONS* VARIABLE"
  [1d1d]: dref-manual.md#DREF:@BASIC-LOCATIVE-TYPES%20MGL-PAX:SECTION "Basic Locative Types"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1e36]: dref-manual.md#DREF:DREF-NAME%20%28MGL-PAX:READER%20DREF:DREF%29 "DREF:DREF-NAME (MGL-PAX:READER DREF:DREF)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [1f99]: http://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm "ARRAY (MGL-PAX:CLHS CLASS)"
  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [2060]: dref-manual.md#CLASS%20MGL-PAX:LOCATIVE "CLASS MGL-PAX:LOCATIVE"
  [2143]: dref-manual.md#DREF:@DEFINITION%20MGL-PAX:GLOSSARY-TERM "definition"
  [21f4]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#loop_keyword "\"loop keyword\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [2243]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*TRACE-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [225d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhn.htm "\"2.4.8.14\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [227d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhi.htm "\"2.4.8.9\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [22c2]: #MGL-PAX:@LINKING-TO-SECTIONS%20MGL-PAX:SECTION "Linking to Sections"
  [2352]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfa.htm "\"22.3.6.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [238c]: #MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION"
  [2415]: pax-manual.md "PAX Manual"
  [2444]: dref-manual.md#DREF-EXT:LOCATIVE-ARGS%20FUNCTION "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [2634]: #MGL-PAX:@OVERVIEW-OF-ESCAPING%20MGL-PAX:SECTION "Overview of Escaping"
  [2826]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhj.htm "\"2.4.8.10\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [292a]: #MGL-PAX:@PAX-LOCATIVES%20MGL-PAX:SECTION "PAX Locatives"
  [2c93]: #MGL-PAX:@GENERATING-DOCUMENTATION%20MGL-PAX:SECTION "Generating Documentation"
  [2ca9]: #MGL-PAX:OUTPUT-REFLINK%20FUNCTION "MGL-PAX:OUTPUT-REFLINK FUNCTION"
  [2cd5]: #MGL-PAX:*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES*%20VARIABLE "MGL-PAX:*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES* VARIABLE"
  [2d48]: #MGL-PAX:WITH-DISLOCATED-NAMES%20MGL-PAX:MACRO "MGL-PAX:WITH-DISLOCATED-NAMES MGL-PAX:MACRO"
  [2f21]: #MGL-PAX:@PRINTS-TO-AN-EQUIVALENT-STRING%20MGL-PAX:GLOSSARY-TERM "prints to an equivalent string"
  [3026]: #MGL-PAX:ESCAPE-MARKDOWN%20FUNCTION "MGL-PAX:ESCAPE-MARKDOWN FUNCTION"
  [3076]: https://github.com/redline6561/colorize/ "Colorize"
  [309c2]: http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm "\"2.4.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [30ad]: dref-manual.md#DREF:LISP-LOCATIVE-TYPES%20FUNCTION "DREF:LISP-LOCATIVE-TYPES FUNCTION"
  [31c5]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cea.htm "\"22.3.5.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [32da]: dref-manual.md#DREF:SOURCE-LOCATION%20FUNCTION "DREF:SOURCE-LOCATION FUNCTION"
  [3386]: #MGL-PAX:@NAVIGATING-IN-EMACS%20MGL-PAX:SECTION "Navigating Sources in Emacs"
  [342d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhs.htm "\"2.4.8.19\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3473]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm "FIND-SYMBOL (MGL-PAX:CLHS FUNCTION)"
  [34b8]: #MGL-PAX:@UNSPECIFIC-REFLINK-WITH-TEXT%20MGL-PAX:SECTION "Unspecific Reflink with Text"
  [34d3]: #MGL-PAX:@INDEXING-DEFINITIONS%20MGL-PAX:SECTION "Indexing Definitions"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander "\"setf expander\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [36e1]: #MGL-PAX:@HTML-OUTPUT%20MGL-PAX:SECTION "HTML Output"
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent "\"dynamic extent\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [378f]: #MGL-PAX:@PARSING%20MGL-PAX:SECTION "Parsing"
  [3808]: http://www.lispworks.com/documentation/HyperSpec/Body/f_terpri.htm "FRESH-LINE (MGL-PAX:CLHS FUNCTION)"
  [3831]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/// (MGL-PAX:CLHS VARIABLE)"
  [38de]: #MGL-PAX:@SPECIFIC-AUTOLINK%20MGL-PAX:SECTION "Specific Autolink"
  [3942]: #MGL-PAX:@STABLE-PRINTED-LOCATIVE%20MGL-PAX:GLOSSARY-TERM "stable printed locative"
  [3972]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#reader_macro "\"reader macro\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3da8]: #MGL-PAX:*FORMAT*%20VARIABLE "MGL-PAX:*FORMAT* VARIABLE"
  [3e6e]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbb.htm "\"22.3.2.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3f2e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [3fa1]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cad.htm "\"22.3.1.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [407c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_h.htm#home_package "\"home package\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [4143]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING= (MGL-PAX:CLHS FUNCTION)"
  [4317]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm "CERROR (MGL-PAX:CLHS FUNCTION)"
  [432c]: #MGL-PAX:DOCUMENT%20FUNCTION "MGL-PAX:DOCUMENT FUNCTION"
  [43bd]: dref-manual.md#DREF:@REFERENCE%20MGL-PAX:GLOSSARY-TERM "reference"
  [443b]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cas.htm "*PRINT-CASE* (MGL-PAX:CLHS VARIABLE)"
  [4537]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "+++ (MGL-PAX:CLHS VARIABLE)"
  [460e]: #MGL-PAX:@M-.-DEFAULTING%20MGL-PAX:SECTION "`M-.` Defaulting"
  [4796]: dref-manual.md#LAMBDA%20MGL-PAX:LOCATIVE "LAMBDA MGL-PAX:LOCATIVE"
  [479a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "ABORT (MGL-PAX:CLHS FUNCTION)"
  [4841]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-CONTROL (MGL-PAX:CLHS FUNCTION)"
  [48f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE (MGL-PAX:CLHS FUNCTION)"
  [4bb8]: #%22mgl-pax%2Fdocument%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/document\" ASDF/SYSTEM:SYSTEM"
  [4c39]: #MGL-PAX:@TRANSCRIPT-CONSISTENCY-CHECKING%20MGL-PAX:SECTION "Transcript Consistency Checking"
  [4dd7]: dref-manual.md#PACKAGE%20MGL-PAX:LOCATIVE "PACKAGE MGL-PAX:LOCATIVE"
  [4e05]: #MGL-PAX:@UNSPECIFIC-REFLINK%20MGL-PAX:SECTION "Unspecific Reflink"
  [5119]: #MGL-PAX:GLOSSARY-TERM%20MGL-PAX:LOCATIVE "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [519c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING< (MGL-PAX:CLHS FUNCTION)"
  [5225]: dref-manual.md "DRef Manual"
  [5483]: http://www.lispworks.com/documentation/HyperSpec/Body/v__.htm "- (MGL-PAX:CLHS VARIABLE)"
  [548e]: dref-manual.md#DREF-EXT:DEFINE-LOCATIVE-ALIAS%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [54d8]: #MGL-PAX:@ADDING-NEW-LOCATIVES%20MGL-PAX:SECTION "Adding New Locatives"
  [550b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgd.htm "\"22.3.7.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [56b9]: #MGL-PAX:CONCEPT%20MGL-PAX:LOCATIVE "MGL-PAX:CONCEPT MGL-PAX:LOCATIVE"
  [56ba]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dd.htm "\"2.4.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [574a]: #MGL-PAX:@EXTENDING-DOCUMENT%20MGL-PAX:SECTION "Extending `document`"
  [58259]: #%22mgl-pax%2Ftranscribe%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/transcribe\" ASDF/SYSTEM:SYSTEM"
  [5875]: dref-manual.md#GENERIC-FUNCTION%20MGL-PAX:LOCATIVE "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [587f]: #MGL-PAX:MAKE-GIT-SOURCE-URI-FN%20FUNCTION "MGL-PAX:MAKE-GIT-SOURCE-URI-FN FUNCTION"
  [5884]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm "FIND-IF (MGL-PAX:CLHS FUNCTION)"
  [588c]: #MGL-PAX:STANDARD-TRANSCRIBE-DYNENV%20FUNCTION "MGL-PAX:STANDARD-TRANSCRIBE-DYNENV FUNCTION"
  [58f6]: #GO%20MGL-PAX:LOCATIVE "GO MGL-PAX:LOCATIVE"
  [5920]: #MGL-PAX:@CONCEPT-SUBKEY%20MGL-PAX:GLOSSARY-TERM "concept subkey"
  [5968]: autoload-manual.md#%22autoload%22%20ASDF%2FSYSTEM:SYSTEM "\"autoload\" ASDF/SYSTEM:SYSTEM"
  [59d9]: https://pandoc.org/ "Pandoc"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5ab6]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rig.htm "*PRINT-RIGHT-MARGIN* (MGL-PAX:CLHS VARIABLE)"
  [5b4d]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccd.htm "\"22.3.3.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [5cd7]: #MGL-PAX:INCLUDE%20MGL-PAX:LOCATIVE "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5fac]: #MGL-PAX:SECTION%20CLASS "MGL-PAX:SECTION CLASS"
  [5fc4]: dref-manual.md#DREF:@NAME%20MGL-PAX:GLOSSARY-TERM "name"
  [5fd4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL (MGL-PAX:CLHS TYPE)"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list "\"destructuring lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [60e4]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chc.htm "\"22.3.8.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [6300]: #MGL-PAX:@TRANSCRIPTS%20MGL-PAX:SECTION "Transcripts"
  [6334]: dref-manual.md#DREF-EXT:LOCATE-ERROR%20CONDITION "DREF-EXT:LOCATE-ERROR CONDITION"
  [633c]: http://www.lispworks.com/documentation/HyperSpec/Body/v_termin.htm "*TERMINAL-IO* (MGL-PAX:CLHS VARIABLE)"
  [6384]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 (MGL-PAX:CLHS FUNCTION)"
  [63b4]: dref-manual.md#DREF:RESOLVE%20FUNCTION "DREF:RESOLVE FUNCTION"
  [63ef]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009_w.htm "\"ISSUE:AREF-1D\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [63f30]: #MGL-PAX:DEFINE-PACKAGE%20MGL-PAX:MACRO "MGL-PAX:DEFINE-PACKAGE MGL-PAX:MACRO"
  [64be]: #MGL-PAX:UNRESOLVABLE-REFLINK%20CONDITION "MGL-PAX:UNRESOLVABLE-REFLINK CONDITION"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [659d]: #MGL-PAX:@BROWSE-BY-LOCATIVE-TYPE%20MGL-PAX:SECTION "Browse by Locative Types"
  [65b4]: dref-manual.md#DREF:DREF-APROPOS%20FUNCTION "DREF:DREF-APROPOS FUNCTION"
  [65bc]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cae.htm "\"22.3.1.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [66c6]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*ERROR-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [672f]: #MGL-PAX:SECTION%20MGL-PAX:LOCATIVE "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [685e]: #MGL-PAX:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [6897]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbc.htm "\"22.3.2.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [68c1]: https://daringfireball.net/projects/markdown/syntax#code "Markdown inline code"
  [68d2]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhh.htm "\"2.4.8.8\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [68f1]: sbcl-manual.md#DOCUMENTATION%20GENERIC-FUNCTION "DOCUMENTATION GENERIC-FUNCTION"
  [68fb]: dref-manual.md#DREF-EXT:@EXTENDING-DREF%20MGL-PAX:SECTION "Extending DRef"
  [698d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm "\"2.4.8\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [6ab0]: #MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION*%20VARIABLE "MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION* VARIABLE"
  [6af6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PPRINT (MGL-PAX:CLHS FUNCTION)"
  [6b59]: #MGL-PAX:@TRANSCRIPT-DYNENV%20MGL-PAX:SECTION "Controlling the Dynamic Environment"
  [6be7]: https://slime.common-lisp.dev/ "SLIME"
  [6c83]: dref-manual.md#VARIABLE%20MGL-PAX:LOCATIVE "VARIABLE MGL-PAX:LOCATIVE"
  [6e18]: #MGL-PAX:@TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS%20MGL-PAX:SECTION "Finer-Grained Consistency Checks"
  [6f51]: http://www.lispworks.com/documentation/HyperSpec/Body/r_muffle.htm "MUFFLE-WARNING (MGL-PAX:CLHS RESTART)"
  [6fdb]: #%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [7163]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhl.htm "\"2.4.8.12\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [718a]: named-readtables-manual.md#%22named-readtables%22%20ASDF%2FSYSTEM:SYSTEM "\"named-readtables\" ASDF/SYSTEM:SYSTEM"
  [72a7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "+ (MGL-PAX:CLHS VARIABLE)"
  [72b4]: #MGL-PAX:DEFSECTION%20MGL-PAX:MACRO "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [72cc]: #MGL-PAX:ENSURE-WEB-SERVER%20FUNCTION "MGL-PAX:ENSURE-WEB-SERVER FUNCTION"
  [730f]: #MGL-PAX:*DISCARD-DOCUMENTATION-P*%20VARIABLE "MGL-PAX:*DISCARD-DOCUMENTATION-P* VARIABLE"
  [7439]: https://emacs-w3m.github.io/info/emacs-w3m.html "w3m"
  [7445]: #MGL-PAX:@INTERESTING%20MGL-PAX:GLOSSARY-TERM "interesting"
  [7506]: dref-manual.md#READTABLE%20MGL-PAX:LOCATIVE "READTABLE MGL-PAX:LOCATIVE"
  [76ab]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccc.htm "\"22.3.3.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [76df]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbd.htm "\"22.3.2.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [76ea]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgb.htm "\"22.3.7.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [782a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_pre.htm "*PRINT-PRETTY* (MGL-PAX:CLHS VARIABLE)"
  [78d1]: http://www.lispworks.com/documentation/HyperSpec/Body/v__stst_.htm "** (MGL-PAX:CLHS VARIABLE)"
  [7a4e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#section "\"section\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [7a7f]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhc.htm "\"2.4.8.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7ac8]: dref-manual.md#DREF:@LOCATIVE%20MGL-PAX:GLOSSARY-TERM "locative"
  [7b6f]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dha.htm "\"2.4.8.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7bd6]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cab.htm "\"22.3.1.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7bf5]: #MGL-PAX:@MARKDOWN-IN-DOCSTRINGS%20MGL-PAX:SECTION "Markdown in Docstrings"
  [7c9f]: http://www.lispworks.com/documentation/HyperSpec/Body/d_type.htm "TYPE (MGL-PAX:CLHS DECLARATION)"
  [7cc3]: #MGL-PAX:@LINKING-TO-THE-HYPERSPEC%20MGL-PAX:SECTION "Linking to the HyperSpec"
  [7d18]: http://c2.com/cgi/wiki?OnceAndOnlyOnce "OAOO"
  [7dc7]: #MGL-PAX:@DOCUMENT-RETURN%20MGL-PAX:SECTION "Return Values"
  [7e92]: dref-manual.md#DREF:DREF%20FUNCTION "DREF:DREF FUNCTION"
  [7eb5]: #MGL-PAX:@LINKABLE%20MGL-PAX:GLOSSARY-TERM "linkable"
  [7f1f]: #MGL-PAX:DOCUMENT-DOCSTRING%20FUNCTION "MGL-PAX:DOCUMENT-DOCSTRING FUNCTION"
  [7f9a]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [80a8]: dref-manual.md#DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS%20FUNCTION "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS FUNCTION"
  [80e8]: #MGL-PAX:WITH-HEADING%20MGL-PAX:MACRO "MGL-PAX:WITH-HEADING MGL-PAX:MACRO"
  [8106]: #MGL-PAX:@M-.-MINIBUFFER-SYNTAX%20MGL-PAX:SECTION "`M-.` Minibuffer Syntax"
  [81b3]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhr.htm "\"2.4.8.18\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [81f7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8251]: #MGL-PAX:GLOSSARY-TERM%20CLASS "MGL-PAX:GLOSSARY-TERM CLASS"
  [8269]: #MGL-PAX:DOCUMENT-OBJECT*%20GENERIC-FUNCTION "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [826b]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dg.htm "\"2.4.7\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [83d5]: #MGL-PAX:@BROWSING-WITH-W3M%20MGL-PAX:SECTION "Browsing with w3m"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [8423]: #MGL-PAX:@TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING%20MGL-PAX:SECTION "Utilities for Consistency Checking"
  [8492]: #MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION"
  [84f2]: https://pandoc.org/MANUAL.html#extension-yaml_metadata_block "Pandoc YAML metadata block"
  [8541]: #MGL-PAX:@EMACS-SETUP%20MGL-PAX:SECTION "Emacs Setup"
  [85c9]: #MGL-PAX:*DOCUMENT-INDEX-REFERRER-GROUPS*%20VARIABLE "MGL-PAX:*DOCUMENT-INDEX-REFERRER-GROUPS* VARIABLE"
  [8710]: #MGL-PAX:ARGUMENT%20MGL-PAX:LOCATIVE "MGL-PAX:ARGUMENT MGL-PAX:LOCATIVE"
  [875e]: #MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC* VARIABLE"
  [876d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST (MGL-PAX:CLHS FUNCTION)"
  [886c]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cge.htm "\"22.3.7.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [8894]: #MGL-PAX:*DOCUMENT-INDICES*%20VARIABLE "MGL-PAX:*DOCUMENT-INDICES* VARIABLE"
  [88a0]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_k.htm#keyword "\"keyword\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [88a7]: #MGL-PAX:SECTION-READTABLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29 "MGL-PAX:SECTION-READTABLE (MGL-PAX:READER MGL-PAX:SECTION)"
  [88cf]: #MGL-PAX:@NAME%20MGL-PAX:GLOSSARY-TERM "name"
  [88e6]: try-manual.md "Try Manual"
  [8a58]: #MGL-PAX:@SECTIONS%20MGL-PAX:SECTION "Sections"
  [8a5e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhb.htm "\"2.4.8.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [8aca]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rda.htm "*PRINT-READABLY* (MGL-PAX:CLHS VARIABLE)"
  [8c00]: https://daringfireball.net/projects/markdown/syntax#link "Markdown reference link"
  [8d9b]: #MGL-PAX:@OUTPUT-FORMATS%20MGL-PAX:SECTION "Output Formats"
  [8e71]: #MGL-PAX:@UNSPECIFIC-LINK%20MGL-PAX:SECTION "Unspecific Link"
  [8e92]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dc.htm "\"2.4.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [8ece]: #MGL-PAX:DEFINE-GLOSSARY-TERM%20MGL-PAX:MACRO "MGL-PAX:DEFINE-GLOSSARY-TERM MGL-PAX:MACRO"
  [8f19]: dref-manual.md#DREF:LOCATE%20FUNCTION "DREF:LOCATE FUNCTION"
  [8f46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_import.htm "IMPORT (MGL-PAX:CLHS FUNCTION)"
  [8fb6]: #MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES*%20VARIABLE "MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* VARIABLE"
  [90fa]: #MGL-PAX:*DOCUMENT-HTML-DEFAULT-STYLE*%20VARIABLE "MGL-PAX:*DOCUMENT-HTML-DEFAULT-STYLE* VARIABLE"
  [9172]: http://www.lispworks.com/documentation/HyperSpec/Body/t_t.htm "T (MGL-PAX:CLHS CLASS)"
  [935f]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss045.htm "\"SUMMARY:CHARACTER-PROPOSAL:2-6-5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [9439]: http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm "PRINT-UNREADABLE-OBJECT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [943a]: dref-manual.md#DREF:PSEUDO%20DREF:DTYPE "DREF:PSEUDO DREF:DTYPE"
  [945b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgc.htm "\"22.3.7.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [9478]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhv.htm "\"2.4.8.22\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [94c7]: #MGL-PAX:@BASICS%20MGL-PAX:SECTION "Basics"
  [9590]: http://www.lispworks.com/documentation/HyperSpec/Body/v__stst_.htm "* (MGL-PAX:CLHS VARIABLE)"
  [963f]: dref-manual.md#DREF:DTYPEP%20FUNCTION "DREF:DTYPEP FUNCTION"
  [97ba]: dref-manual.md#DREF-EXT:LOCATIVE-TYPE%20FUNCTION "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [98ff]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list "\"lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9927]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm "\"22.3.2.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [99b0]: dref-manual.md#DREF:LOCATIVE-TYPES%20FUNCTION "DREF:LOCATIVE-TYPES FUNCTION"
  [99b05]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function "\"setf function\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9b15]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#accessible "\"accessible\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9bd5]: #MGL-PAX:@TRANSCRIBING-IN-DOCUMENTATION%20MGL-PAX:SECTION "Transcribing in Documentation"
  [9c7d]: #MGL-PAX:@PAGES%20MGL-PAX:SECTION "`pages`"
  [9d50]: #MGL-PAX:@PAX-LIVE-HOME-PAGE%20MGL-PAX:SECTION "PAX Live Home Page"
  [9db9]: #MGL-PAX:@LOCAL-DEFINITION%20MGL-PAX:SECTION "Local Definition"
  [9dbc]: #MGL-PAX:@TRANSCRIPT-API%20MGL-PAX:SECTION "Transcript API"
  [9f2f]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/ (MGL-PAX:CLHS VARIABLE)"
  [9fd4]: dref-manual.md#DREF-EXT:DOCSTRING*%20GENERIC-FUNCTION "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a11d]: dref-manual.md#DREF:@LOCATIVE-TYPE%20MGL-PAX:GLOSSARY-TERM "locative type"
  [a17d]: #MGL-PAX:@MATHJAX%20MGL-PAX:SECTION "MathJax"
  [a22ed]: dref-manual.md#DREF:DREF-LOCATIVE-TYPE%20FUNCTION "DREF:DREF-LOCATIVE-TYPE FUNCTION"
  [a249]: #MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION"
  [a2706]: #MGL-PAX:@ESCAPING-AUTOLINKING%20MGL-PAX:SECTION "Escaping Autolinking"
  [a317]: https://daringfireball.net/projects/markdown/ "Markdown"
  [a412]: #MGL-PAX:DOCUMENTING-DEFINITION%20MGL-PAX:MACRO "MGL-PAX:DOCUMENTING-DEFINITION MGL-PAX:MACRO"
  [a459]: dref-manual.md#DREF:@DTYPES%20MGL-PAX:SECTION "`dtype`s"
  [a595]: #MGL-PAX:@BROWSING-LIVE-DOCUMENTATION%20MGL-PAX:SECTION "Browsing Live Documentation"
  [a5b1]: #MGL-PAX:SECTION-PACKAGE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29 "MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION)"
  [a5ee]: #MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* VARIABLE"
  [a843]: http://www.lispworks.com/documentation/HyperSpec/Body/t_std_ob.htm "STANDARD-OBJECT (MGL-PAX:CLHS CLASS)"
  [a8c5]: #%22mgl-pax%2Fweb%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/web\" ASDF/SYSTEM:SYSTEM"
  [a951]: dref-manual.md#MGL-PAX:UNKNOWN%20MGL-PAX:LOCATIVE "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ab38]: #MGL-PAX:@PARSING-LOCATIVES%20MGL-PAX:SECTION "Parsing Locatives"
  [ab7e]: #MGL-PAX:@PACKAGE-AND-READTABLE%20MGL-PAX:SECTION "Package and Readtable"
  [ac30]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cha.htm "\"22.3.8.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ac5e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhe.htm "\"2.4.8.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: dref-manual.md#DREF:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [ad8e]: #MGL-PAX:@REFERENT%20MGL-PAX:GLOSSARY-TERM "referent"
  [adf2]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhd.htm "\"2.4.8.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [adf8]: #MGL-PAX:@INDEXING%20MGL-PAX:SECTION "Indexing"
  [aeb6]: http://www.lispworks.com/documentation/HyperSpec/Body/a_fn.htm "FUNCTION MGL-PAX:CLHS"
  [af78]: #MGL-PAX:GLOSSARY-TERM-TITLE%20%28MGL-PAX:READER%20MGL-PAX:GLOSSARY-TERM%29 "MGL-PAX:GLOSSARY-TERM-TITLE (MGL-PAX:READER MGL-PAX:GLOSSARY-TERM)"
  [af7b]: autoload-manual.md#%22autoload-doc%22%20ASDF%2FSYSTEM:SYSTEM "\"autoload-doc\" ASDF/SYSTEM:SYSTEM"
  [affc]: dref-manual.md#MGL-PAX:DOCSTRING%20FUNCTION "MGL-PAX:DOCSTRING FUNCTION"
  [b033]: #MGL-PAX:@ASDF-SYSTEMS-AND-RELATED-PACKAGES%20MGL-PAX:SECTION "`asdf:system`s and Related `package`s"
  [b0b0]: #MGL-PAX:@CONFIGURING-INDICES%20MGL-PAX:SECTION "Configuring Indices"
  [b18e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list "\"property list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [b19d]: #MGL-PAX:@CONCEPT-KEY%20MGL-PAX:GLOSSARY-TERM "concept key"
  [b2e4]: #MGL-PAX:@FILTERING-LINKS%20MGL-PAX:SECTION "Filtering Links"
  [b39f]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdb.htm "\"22.3.4.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [b4f0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN (MGL-PAX:CLHS FUNCTION)"
  [b6c4]: dref-manual.md#DREF-EXT:DEFINE-LOCATIVE-TYPE%20MGL-PAX:MACRO "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b79a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* (MGL-PAX:CLHS VARIABLE)"
  [b7fc]: #MGL-PAX:@APROPOS%20MGL-PAX:SECTION "Apropos"
  [b80d]: #MGL-PAX:DEFINE-CONCEPT%20MGL-PAX:MACRO "MGL-PAX:DEFINE-CONCEPT MGL-PAX:MACRO"
  [b81d]: #MGL-PAX:TRANSCRIPTION-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-ERROR CONDITION"
  [b89a]: #MGL-PAX:@CODIFIABLE%20MGL-PAX:GLOSSARY-TERM "codifiable"
  [b8b4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "MUFFLE-WARNING (MGL-PAX:CLHS FUNCTION)"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [ba62]: dref-manual.md#FUNCTION%20MGL-PAX:LOCATIVE "FUNCTION MGL-PAX:LOCATIVE"
  [ba76]: #MGL-PAX:*DOCUMENT-INDEX-FORMATS*%20VARIABLE "MGL-PAX:*DOCUMENT-INDEX-FORMATS* VARIABLE"
  [ba90]: #MGL-PAX:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION "Links and Systems"
  [bb12]: #MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS%20FUNCTION "MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS FUNCTION"
  [bc83]: #MGL-PAX:@MARKDOWN-SYNTAX-HIGHLIGHTING%20MGL-PAX:SECTION "Syntax Highlighting"
  [bcb6]: http://www.lispworks.com/documentation/HyperSpec/Body/e_warnin.htm "WARNING (MGL-PAX:CLHS CONDITION)"
  [bdd6]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cga.htm "\"22.3.7.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [bf38]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfc.htm "\"22.3.6.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [bfaa]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhk.htm "\"2.4.8.11\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [c001]: #MGL-PAX:@INDEXING-CONCEPTS%20MGL-PAX:SECTION "Indexing Concepts"
  [c097]: dref-manual.md#ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c0d2]: #MGL-PAX:@LINK-FORMAT%20MGL-PAX:SECTION "Link Format"
  [c2d3]: #MGL-PAX:@MARKDOWN-SUPPORT%20MGL-PAX:SECTION "Markdown Support"
  [c2fd]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_i.htm#implicit_progn "\"implicit progn\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [c340]: dref-manual.md#DREF:PSEUDO-LOCATIVE-TYPES%20FUNCTION "DREF:PSEUDO-LOCATIVE-TYPES FUNCTION"
  [c34e]: #MGL-PAX:DOCTITLE*%20GENERIC-FUNCTION "MGL-PAX:DOCTITLE* GENERIC-FUNCTION"
  [c434]: #MGL-PAX:@BROWSING-WITH-OTHER-BROWSERS%20MGL-PAX:SECTION "Browsing with Other Browsers"
  [c479]: dref-manual.md#CONDITION%20MGL-PAX:LOCATIVE "CONDITION MGL-PAX:LOCATIVE"
  [c4a3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "COS (MGL-PAX:CLHS FUNCTION)"
  [c4ce3]: #MGL-PAX:@EXTENSION-API%20MGL-PAX:SECTION "Writing Extensions"
  [c5ae]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [c6247]: https://daringfireball.net/projects/markdown/syntax#em "Markdown emphasis"
  [c818]: #MGL-PAX:OUTPUT-LABEL%20FUNCTION "MGL-PAX:OUTPUT-LABEL FUNCTION"
  [c819]: dref-manual.md#MGL-PAX:CONSTANT%20MGL-PAX:LOCATIVE "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c879]: #MGL-PAX:@PLAIN-OUTPUT%20MGL-PAX:SECTION "Plain Output"
  [c8cb]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cir.htm "*PRINT-CIRCLE* (MGL-PAX:CLHS VARIABLE)"
  [c930]: #MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P%20GENERIC-FUNCTION "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [c93e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhg.htm "\"2.4.8.7\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cae2]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cca.htm "\"22.3.3.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cb15]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions "`M-.`"
  [cb94]: mat-manual.md#ARRAY%20MGL-CUBE:FACET-NAME "ARRAY MGL-CUBE:FACET-NAME"
  [cbc4]: #MGL-PAX:@REFLINK%20MGL-PAX:SECTION "Reflink"
  [cc04]: dref-manual.md#MGL-PAX:READER%20MGL-PAX:LOCATIVE "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cd66]: http://www.lispworks.com/documentation/HyperSpec/Body/02_de.htm "\"2.4.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cda7]: dref-manual.md#DREF:XREF%20FUNCTION "DREF:XREF FUNCTION"
  [ce75]: #MGL-PAX:DOCSTRING%20MGL-PAX:LOCATIVE "MGL-PAX:DOCSTRING MGL-PAX:LOCATIVE"
  [cfab]: #MGL-PAX:@EMACS-KEYS%20MGL-PAX:SECTION "Setting up Keys"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d1ca]: #MGL-PAX:@DOCUMENT-IMPLEMENTATION-NOTES%20MGL-PAX:SECTION "Documentation Generation Implementation Notes"
  [d1dc]: #MGL-PAX:@GLOSSARY-TERMS%20MGL-PAX:SECTION "Glossary Terms"
  [d273]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#format_directive "\"format directive\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [d296]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cia.htm "\"22.3.9.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [d3b3]: dref-manual.md#DREF-EXT:RESOLVE*%20GENERIC-FUNCTION "DREF-EXT:RESOLVE* GENERIC-FUNCTION"
  [d3fc]: #MGL-PAX:@EMACS-LOADING%20MGL-PAX:SECTION "Loading PAX"
  [d3fc5]: https://github.com/smithzvk/pythonic-string-reader "Pythonic String Reader"
  [d451]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINT (MGL-PAX:CLHS FUNCTION)"
  [d4a9]: #MGL-PAX:@EMACS-FUNCTIONALITY%20MGL-PAX:SECTION "Functionality Provided"
  [d4e7]: #MGL-PAX:*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*%20VARIABLE "MGL-PAX:*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL* VARIABLE"
  [d534]: https://daringfireball.net/projects/markdown/syntax#img "Markdown image"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d5a9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM (MGL-PAX:CLHS CLASS)"
  [d5e1]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhq.htm "\"2.4.8.17\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d7b0]: #MGL-PAX:@WORD%20MGL-PAX:GLOSSARY-TERM "word"
  [d813]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm "READ-FROM-STRING (MGL-PAX:CLHS FUNCTION)"
  [d83a]: dref-manual.md#SETF%20MGL-PAX:LOCATIVE "SETF MGL-PAX:LOCATIVE"
  [d850]: #MGL-PAX:SECTION-ENTRIES%20FUNCTION "MGL-PAX:SECTION-ENTRIES FUNCTION"
  [d930]: dref-manual.md#DREF:DREF%20CLASS "DREF:DREF CLASS"
  [d9ee]: #MGL-PAX:*DOCUMENT-LINK-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-CODE* VARIABLE"
  [d9f2]: trivial-utf-8-manual.md#%22trivial-utf-8%22%20ASDF%2FSYSTEM:SYSTEM "\"trivial-utf-8\" ASDF/SYSTEM:SYSTEM"
  [da14]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-ARGUMENTS (MGL-PAX:CLHS FUNCTION)"
  [da65]: dref-manual.md#STRUCTURE%20MGL-PAX:LOCATIVE "STRUCTURE MGL-PAX:LOCATIVE"
  [dae6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING (MGL-PAX:CLHS FUNCTION)"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [db38]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm "\"22.3.5.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dc0a]: #MGL-PAX:@DOCUMENT-FUNCTION%20MGL-PAX:SECTION "The `document` Function"
  [dd03]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhp.htm "\"2.4.8.16\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [dd29]: #MGL-PAX:@MARKDOWN-OUTPUT%20MGL-PAX:SECTION "Markdown Output"
  [dd37]: #MGL-PAX:*DOCUMENT-PANDOC-PROGRAM*%20VARIABLE "MGL-PAX:*DOCUMENT-PANDOC-PROGRAM* VARIABLE"
  [dded]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhm.htm "\"2.4.8.13\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [dff6]: #MGL-PAX:@GITHUB-WORKFLOW%20MGL-PAX:SECTION "GitHub Workflow"
  [e012]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CDR (MGL-PAX:CLHS FUNCTION)"
  [e016]: http://www.lispworks.com/documentation/HyperSpec/Body/06_aab.htm "\"6.1.1.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e196]: dref-manual.md#DREF:DEFINITIONS%20FUNCTION "DREF:DEFINITIONS FUNCTION"
  [e216]: #MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*%20VARIABLE "MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* VARIABLE"
  [e2a4]: #MGL-PAX:@UNSPECIFIC-AUTOLINK%20MGL-PAX:SECTION "Unspecific Autolink"
  [e2ae]: #MGL-PAX:NOTE%20MGL-PAX:MACRO "MGL-PAX:NOTE MGL-PAX:MACRO"
  [e391]: #MGL-PAX:DISLOCATED%20MGL-PAX:LOCATIVE "MGL-PAX:DISLOCATED MGL-PAX:LOCATIVE"
  [e433]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "// (MGL-PAX:CLHS VARIABLE)"
  [e43c]: http://www.lispworks.com/documentation/HyperSpec/Body/02_db.htm "\"2.4.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e4420]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm "\"3.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e444]: #MGL-PAX:@M-.-COMPLETION%20MGL-PAX:SECTION "`M-.` Completion"
  [e51f]: #MGL-PAX:EXPORTABLE-REFERENCE-P%20GENERIC-FUNCTION "MGL-PAX:EXPORTABLE-REFERENCE-P GENERIC-FUNCTION"
  [e548]: dref-manual.md#MGL-PAX:WRITER%20MGL-PAX:LOCATIVE "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e619]: #MGL-PAX:DOCTITLE%20FUNCTION "MGL-PAX:DOCTITLE FUNCTION"
  [e65d]: #MGL-PAX:@PARSING-NAMES%20MGL-PAX:SECTION "Parsing Names"
  [e6bd]: dref-manual.md#DREF:ARGLIST%20FUNCTION "DREF:ARGLIST FUNCTION"
  [e6d3]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdc.htm "\"22.3.4.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e7e0]: #MGL-PAX:@REFERRER%20MGL-PAX:GLOSSARY-TERM "referrer"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [ea37]: http://www.lispworks.com/documentation/HyperSpec/Body/v__stst_.htm "*** (MGL-PAX:CLHS VARIABLE)"
  [ebd3]: #MGL-PAX:*TRANSCRIBE-SYNTAXES*%20VARIABLE "MGL-PAX:*TRANSCRIBE-SYNTAXES* VARIABLE"
  [ec7a]: #MGL-PAX:@AUTOLINK%20MGL-PAX:SECTION "Autolink"
  [ed46]: #MGL-PAX:@M-.-PROMPTING%20MGL-PAX:SECTION "`M-.` Prompting"
  [ed5f]: #MGL-PAX:CLHS%20MGL-PAX:LOCATIVE "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ed9f]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ceb.htm "\"22.3.5.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ee51]: #MGL-PAX:UPDATE-PAX-WORLD%20FUNCTION "MGL-PAX:UPDATE-PAX-WORLD FUNCTION"
  [ee8b]: #MGL-PAX:*DOCUMENT-PANDOC-PDF-METADATA-BLOCK*%20VARIABLE "MGL-PAX:*DOCUMENT-PANDOC-PDF-METADATA-BLOCK* VARIABLE"
  [f0d5]: #MGL-PAX:@RAW-NAMES-IN-WORDS%20MGL-PAX:SECTION "Raw Names in Words"
  [f12d]: #MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL*%20VARIABLE "MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* VARIABLE"
  [f155]: #%22mgl-pax%2Fnavigate%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/navigate\" ASDF/SYSTEM:SYSTEM"
  [f1ab]: #MGL-PAX:@CODIFICATION%20MGL-PAX:SECTION "Codification"
  [f1f0]: #MGL-PAX:TRANSCRIBE%20FUNCTION "MGL-PAX:TRANSCRIBE FUNCTION"
  [f25f]: #MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* VARIABLE"
  [f275b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cda.htm "\"22.3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [f2f5]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_cn.htm "SIMPLE-CONDITION (MGL-PAX:CLHS CONDITION)"
  [f3f4]: #MGL-PAX:@EMACS-QUICKLISP%20MGL-PAX:SECTION "Installing from Quicklisp"
  [f4bf]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*QUERY-IO* (MGL-PAX:CLHS VARIABLE)"
  [f4fd]: #MGL-PAX:REGISTER-DOC-IN-PAX-WORLD%20FUNCTION "MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION"
  [f585]: #MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT*%20VARIABLE "MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT* VARIABLE"
  [f5af]: #MGL-PAX:@RAW-NAME%20MGL-PAX:GLOSSARY-TERM "raw name"
  [f5bd]: #MGL-PAX:@TRANSCRIBING-WITH-EMACS%20MGL-PAX:SECTION "Transcribing with Emacs"
  [f74b]: #MGL-PAX:@BACKGROUND%20MGL-PAX:SECTION "Background"
  [f7a5]: #MGL-PAX:@SPECIFIC-REFLINK%20MGL-PAX:SECTION "Specific Reflink"
  [f7e6]: #MGL-PAX:@DUMMY-OUTPUT%20MGL-PAX:SECTION "Dummy Output"
  [f83b]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lisp_read-eval-print_loop "\"Lisp read-eval-print loop\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [f9fa]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbe.htm "\"22.3.2.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [fa43]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dho.htm "\"2.4.8.15\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [fb17]: #MGL-PAX:@SPECIFIC-REFLINK-WITH-TEXT%20MGL-PAX:SECTION "Specific Reflink with Text"
  [fbb1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "++ (MGL-PAX:CLHS VARIABLE)"
  [fda4]: #MGL-PAX:HOME-SECTION%20FUNCTION "MGL-PAX:HOME-SECTION FUNCTION"
  [fe21]: http://www.lispworks.com/documentation/HyperSpec/Body/v_t.htm "T (MGL-PAX:CLHS MGL-PAX:CONSTANT)"
  [fe58]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ (MGL-PAX:CLHS FUNCTION)"
  [ff58]: #MGL-PAX:@PUBLIC-SUPERCLASSES%20MGL-PAX:GLOSSARY-TERM "public superclasses"
  [ff76]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_esc.htm "*PRINT-ESCAPE* (MGL-PAX:CLHS VARIABLE)"
  [ffd7]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhf.htm "\"2.4.8.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
