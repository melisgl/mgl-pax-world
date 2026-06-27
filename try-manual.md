<a id="x-28TRY-3A-40TRY-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TRY-MANUAL%20MGL-PAX:SECTION"></a>

# Try Manual

## Table of Contents

- [1 Links and Systems][628a]
- [2 Tutorial][b949]
- [3 Emacs Integration][4c86]
    - [3.1 Emacs Setup][4fc4]
- [4 Events][aaf2]
    - [4.1 Middle Layer of Events][3e0c]
    - [4.2 Concrete Events][279a]
    - [4.3 Event Glue][5237]
    - [4.4 Printing Events][afb9]
    - [4.5 Event Restarts][d4ce]
    - [4.6 Outcomes][e514]
        - [4.6.1 Outcome Restarts][7ef5]
        - [4.6.2 Checks][bb56]
        - [4.6.3 Trials][e6be]
    - [4.7 Errors][7f8e]
    - [4.8 Categories][03ec]
- [5 The `is` Macro][e2e0]
    - [5.1 Format Specifier Form][6cfa]
    - [5.2 Captures][3d27]
        - [5.2.1 Automatic Captures][56ae]
        - [5.2.2 Explicit Captures][20d8]
- [6 Check Library][f7f7]
    - [6.1 Checking Conditions][ff2c]
    - [6.2 Miscellaneous Checks][37c1]
    - [6.3 Check Utilities][906a]
        - [6.3.1 Comparing Floats][9fa9]
- [7 Tests][dc28]
    - [7.1 Calling Test Functions][5379]
        - [7.1.1 Explicit `try`][1720]
        - [7.1.2 Implicit `try`][012f]
    - [7.2 Printing Events][b3f9]
    - [7.3 Counting Events][886e]
    - [7.4 Collecting Events][52e5]
    - [7.5 Rerunning Trials][e4ac]
    - [7.6 Reprocessing Trials][2337]
- [8 Implementation Notes][3eef]
- [9 Glossary][c759]

###### \[in package TRY\]
<a id="x-28TRY-3A-40LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@LINKS%20MGL-PAX:SECTION"></a>

## 1 Links and Systems

The official repository is <https://github.com/melisgl/try>, and
this document in available in various formats on
<https://fixnum.com> for the latest version.

<a id="x-28-22try-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22try%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"try"**

    - _Version:_ 0.0.8
    - _Description:_ Try is an extensible test framework with equal support
        for interactive and non-interactive workflows.
    - _Long Description:_ Try stays as close to normal Lisp evaluation
        rules as possible. Tests are functions that record the checks they
        perform as events. These events provide the means of customization
        of what to debug, print or rerun. There is a single fundamental
        check, the extensible [`is`][80d6] macro. Everything else is built on top.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <http://github.com/melisgl/try>
    - _Bug tracker:_ <https://github.com/melisgl/try/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/try.git)
    - *Depends on:* alexandria, cl-ppcre, closer-mop, ieee-floats, [mgl-pax][6fdb], trivial-gray-streams, uiop

<a id="x-28TRY-3A-40TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TUTORIAL%20MGL-PAX:SECTION"></a>

## 2 Tutorial

Try is a library for unit testing with equal support for
interactive and non-interactive workflows. Tests are functions, and
almost everything else is a condition, whose types feature
prominently in parameterization.

Try is what we get if we make tests functions and build a test
framework on top of the condition system as
[Stefil](https://common-lisp.net/project/stefil/index-old.shtml) did
but also address the issue of rerunning and replaying, make the [`is`][80d6]
check more capable, use the types of the condition hierarchy to
parametrize what to debug, print, rerun, and finally document the
whole thing.

##### Looking for Truth

[The `is` Macro][e2e0] is a replacement for [`cl:assert`][97ee] that can capture values of
subforms to provide context to failures:

```common-lisp
(is (= (1+ 5) 0))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(1+ 5) 0))
..   where
..     #1# = 6
```

This is a [PAX][2415] [transcript][6300],
output is prefixed with `".. "`. Readable and unreadable return
values are prefixed with `"=> "` and `"==> "`, respectively.

Note the `#n#` syntax due to [`*print-circle*`][c8cb].

##### Checking Multiple Values

`is` [automatically captures][56ae] values of arguments
to functions like [`1+`][1eb3] in the above example. Values of other
interesting subforms can be [explicitly captured][20d8]. `is` supports capturing multiple values and can
be taught [how to deal with macros][0743]. The combination of these
features allows [`match-values`][162a] to be implementable as a tiny
extension:

```common-lisp
(is (match-values (values (1+ 5) "sdf")
      (= * 0)
      (string= * "sdf")))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
..        (= * 0)
..        (STRING= * "sdf")))
..   where
..     #1# == 6
..            #2#
```

In the body of `match-values`, `*` is bound to
successive return values of some form, here `(values (1+ 5) "sdf")`.
`match-values` comes with an automatic rewrite rule that captures the
values of this form, which are printed above as `#1# == 6 #2#`. `is`
is flexible enough that all other checks ([`signals`][6d4e], [`signals-not`][7af9],
[`invokes-debugger`][12ce], [`invokes-debugger-not`][aaaa], [`fails`][e80e], and [`in-time`][f3af]) are
built on top of it.

##### Writing Tests

Beyond `is` (a fancy `assert`), Try provides tests, which are Lisp
functions that record their execution in [`trial`][99d0] objects. Let's define
a test and run it:

```common-lisp
(deftest should-work ()
  (is t))

(should-work)
.. SHOULD-WORK            ; TRIAL-START
..   ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
.. ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
..
==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
```

Try is driven by conditions, and the comments to the right show the
type of the condition printed on that line. The `⋅` character marks
successes.

We could have run our test with `(try 'should-work)` as well, which
does pretty much the same thing except that it defaults to never
entering the debugger, whereas calling a test function directly
enters the debugger on events whose type matches the type in the
variable [`*debug*`][856d].

```common-lisp
(try 'should-work)
.. SHOULD-WORK
..   ⋅ (IS T)
.. ⋅ SHOULD-WORK ⋅1
..
==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
```

##### Test Suites

Test suites are just tests that call other tests.

```common-lisp
(deftest my-suite ()
  (should-work)
  (is (= (foo) 5)))

(defun foo ()
  4)

(try 'my-suite)
.. MY-SUITE                 ; TRIAL-START
..   SHOULD-WORK            ; TRIAL-START
..     ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
..   ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
..   ⊠ (IS (= #1=(FOO) 5))  ; UNEXPECTED-RESULT-FAILURE
..     where
..       #1# = 4
.. ⊠ MY-SUITE ⊠1 ⋅1         ; UNEXPECTED-VERDICT-FAILURE
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
```

`⊠` marks [`unexpected-failure`][b5cb]s. Note how the failure of `(is (= (foo)
5))` caused `my-suite` to fail as well. Finally, the `⊠1` and the
`⋅1` in the `trial`'s printed representation are the [event
counts][886e].

##### Filtering Output

To focus on the important bits, we can print only the [`unexpected`][d6ad]
events:

```common-lisp
(try 'my-suite :print 'unexpected)
.. MY-SUITE
..   ⊠ (IS (= #1=(FOO) 5))
..     where
..       #1# = 4
.. ⊠ MY-SUITE ⊠1 ⋅1
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
```

Note that `should-work` is still run, and its check's success is
counted as evidenced by `⋅1`. The above effect can also be achieved
without running the tests again with [`replay-events`][8b69].

##### Debugging

Let's figure out what went wrong:

```
(my-suite)

;;; Here the debugger is invoked:
UNEXPECTED-FAILURE in check:
  (IS (= #1=(FOO) 5))
where
  #1# = 4
Restarts:
 0: [RECORD-EVENT] Record the event and continue.
 1: [FORCE-EXPECTED-SUCCESS] Change outcome to TRY:EXPECTED-RESULT-SUCCESS.
 2: [FORCE-UNEXPECTED-SUCCESS] Change outcome to TRY:UNEXPECTED-RESULT-SUCCESS.
 3: [FORCE-EXPECTED-FAILURE] Change outcome to TRY:EXPECTED-RESULT-FAILURE.
 4: [ABORT-CHECK] Change outcome to TRY:RESULT-ABORT*.
 5: [SKIP-CHECK] Change outcome to TRY:RESULT-SKIP.
 6: [RETRY-CHECK] Retry check.
 7: [ABORT-TRIAL] Record the event and abort trial TRY::MY-SUITE.
 8: [SKIP-TRIAL] Record the event and skip trial TRY::MY-SUITE.
 9: [RETRY-TRIAL] Record the event and retry trial TRY::MY-SUITE.
 10: [SET-TRY-DEBUG] Supply a new value for :DEBUG of TRY:TRY.
 11: [RETRY] Retry SLIME interactive evaluation request.
```

In the [SLIME](https://common-lisp.net/project/slime/doc/html/)
debugger, we press `v` on the frame of the call to `my-suite` to
navigate to its definition, realize what the problem is and fix
`foo`:

```common-lisp
(defun foo ()
  5)
```

Now, we select the [`retry-trial`][fae3] restart, and on the retry
`my-suite` passes. The full output is:

```
MY-SUITE
  SHOULD-WORK
    ⋅ (IS T)
  ⋅ SHOULD-WORK ⋅1
WARNING: redefining TRY::FOO in DEFUN
  ⊠ (IS (= #1=(FOO) 5))
    where
      #1# = 4
MY-SUITE retry #1
  SHOULD-WORK
    ⋅ (IS T)
  ⋅ SHOULD-WORK ⋅1
  ⋅ (IS (= (FOO) 5))
⋅ MY-SUITE ⋅2
```

##### Rerunning Stuff

Instead of working interactively, one can fix the failing test and
rerun it. Now, let's fix `my-suite` and rerun it:

```common-lisp
(deftest my-suite ()
  (should-work)
  (is nil))

(try 'my-suite)
.. MY-SUITE
..   SHOULD-WORK
..     ⋅ (IS T)
..   ⋅ SHOULD-WORK ⋅1
..   ⊠ (IS NIL)
.. ⊠ MY-SUITE ⊠1 ⋅1
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>

(deftest my-suite ()
  (should-work)
  (is t))

(try !)
.. MY-SUITE
..   - SHOULD-WORK
..   ⋅ (IS T)
.. ⋅ MY-SUITE ⋅1
..
==> #<TRIAL (MY-SUITE) EXPECTED-SUCCESS 0.004s ⋅1>
```

Here, [`!`][92af] refers to the most recent `trial` returned by [`try`][b602]. When a
trial is passed to `try` or is [`funcall`][03c7]ed, trials in it that match
the type in `try`'s `rerun` argument are rerun (here, `unexpected` by
default). `should-work` and its check are [`expected-success`][c96a]es,
hence they don't match `unexpected` and are not [rerun][e4ac].

##### Conditional Execution

Conditional execution can be achieved simply by testing the `trial`
object returned by [Tests][dc28].

```
(deftest my-suite ()
  (when (passedp (should-work))
    (is t :msg "a test that depends on SHOULD-WORK")
    (when (is nil)
      (is nil :msg "never run"))))
```

##### Skipping

Sometimes, we do not know up front that a test should not be
executed. Calling [`skip-trial`][f45a] unwinds from the [`current-trial`][e186] and
marks it skipped.

```common-lisp
(deftest my-suite ()
  (is t)
  (skip-trial)
  (is nil))

(my-suite)
==> #<TRIAL (MY-SUITE) SKIP 0.000s ⋅1>
```

In the above, `(is t)` was executed, but `(is nil)` was not.

##### Expecting Outcomes

```common-lisp
(deftest known-broken ()
  (with-failure-expected (t)
    (is nil)))

(known-broken)
.. KNOWN-BROKEN
..   × (IS NIL)
.. ⋅ KNOWN-BROKEN ×1
..
==> #<TRIAL (KNOWN-BROKEN) EXPECTED-SUCCESS 0.000s ×1>
```

`×` marks [`expected-failure`][8620]s. `(with-skip (t) ...)` makes all check
successes and failures [`expected`][b194], which are counted in their own
[`*categories*`][e949] by default but don't make the enclosing tests fail.
Also see [`with-expected-outcome`][1d97].

##### Running Tests on Definition

With [`*run-deftest-when*`][cfd3], tests can be run in various [`eval-when`][9c9c]
situations. To run tests on evaluation, as in SLIME `C-M-x`,
`slime-eval-defun`:

```common-lisp
(setq *run-deftest-when* :execute)

(deftest some-test ()
  (is t))
.. SOME-TEST
..   ⋅ (IS T)
.. ⋅ SOME-TEST ⋅1
..
=> SOME-TEST

(setq *run-deftest-when* nil)
```

##### Fixtures

There is no direct support for fixtures in Try because they are not
needed with the ability of [Rerunning Trials][e4ac] in [context][38e8].

If one insists, macros like the following are easy to write.

```
(defvar *server* nil)

(defmacro with-xxx (&body body)
  `(flet ((,with-xxx-body ()
            ,@body))
     (if *server*
         (with-xxx-body)
         (with-server (make-expensive-server)
           (with-xxx-body)))))
```

##### Packages

The suggested way of writing tests is to call test functions
explicitly:

```
(defpackage :some-test-package
  (:use #:common-lisp #:try))
(in-package :some-test-package)

(deftest test-all ()
  (test-this)
  (test-that))

(deftest test-this ()
  (test-this/more))

(deftest test-this/more ()
  (is t))

(deftest test-that ()
  (is t))

(deftest not-called ()
  (is t))

(defun test ()
  (warn-on-tests-not-run ((find-package :some-test-package))
    (try 'test-all)))

(test)
.. TEST-ALL
..   TEST-THIS
..     TEST-THIS/MORE
..       ⋅ (IS T)
..     ⋅ TEST-THIS/MORE ⋅1
..   ⋅ TEST-THIS ⋅1
..   TEST-THAT
..     ⋅ (IS T)
..   ⋅ TEST-THAT ⋅1
.. ⋅ TEST-ALL ⋅2
.. WARNING: Test NOT-CALLED not run.
==> #<TRIAL (TEST-ALL) EXPECTED-SUCCESS 0.012s ⋅2>
```

Note how the `test` function uses [`warn-on-tests-not-run`][5289] to catch any
tests defined in `some-test-package` that were not run. Tests can be
deleted by [`fmakunbound`][609c], [`unintern`][cdba], or by redefining the function with
[`defun`][f472]. Tests defined in a given package can be listed with
[`list-package-tests`][b426].

This style allows higher level tests to establish the dynamic
environment necessary for lower level tests.

<a id="x-28TRY-3A-40EMACS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EMACS%20MGL-PAX:SECTION"></a>

## 3 Emacs Integration

The Elisp `mgl-try` interactive command runs [`try`][b602] with some
testable and displays its output in a Try buffer, which has major
mode `lisp-mode` and minor modes `outline-mode` and `mgl-try-mode`.
It is assumed that the Lisp is running under
[Slime](https://slime.common-lisp.dev/).

Use `mgl-try-rerun` and `mgl-try-rerun-all` to rerun trials. They
are especially convenient to rerun [`try:!`][92af], when deciding to inspect
the results in a Try buffer for a trial that may not have been run
via Emacs.

In an Emacs Try buffer, the following key bindings are available.

- Movement:

    - Cursor keys move freely.

    - `C-p` and `C-n` move between events.

    - `p` and `n` to move between [`unexpected`][d6ad] events.

    - `P` and `N` move between events which are not
      [`expected-success`][c96a]es.

    - `<tab>` cycles visibility of the current heading's body.

    - `U` moves to the parent heading.

    - `q` is bound to `quit-window`.

- Calling tests:

    - `t` runs a test (defaults to the name of the innermost global
      test function that contains the current line) in the context
      associated with the Emacs buffer, which is similar to but
      distinct from [`*rerun-context*`][38e8]. With a prefix arg, the test is
      an [Implicit `try`][012f] with no arguments. This is suitable for
      interactive debugging under the default settings.

    - `r` [reruns][e4ac] the most recent trial conducted by
      Emacs (this is distinct from `try:!`). With a prefix argument,
      the test is called implicitly.

    - `R` is like `r`, but [`*try-rerun*`][01e7] and [`try:*rerun*`][63db] are set to
      `t`, to ensure that all tests are rerun. With a prefix argument,
      the test is called implicitly.

- Visiting source locations:

    - `v` visits the source location of the enclosing global test
      function (see `t`).

    - `M-.` visits a test function as usual.

In general, since the major mode is `lisp-mode`, the usual key
bindings are available.

<a id="x-28TRY-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EMACS-SETUP%20MGL-PAX:SECTION"></a>

### 3.1 Emacs Setup

Load `src/mgl-try.el` in Emacs.

If you installed Try with Quicklisp, the location of `mgl-try.el`
may change with updates, and you may want to copy the current
version of `mgl-try.el` to a stable location:

    (try:install-try-elisp "~/quicklisp/")

Then, assuming the Elisp file is in the quicklisp directory, add
something like this to your `.emacs`:

```elisp
(load "~/quicklisp/mgl-try.el")
```

For easy access to the functionality of the keys `t`, `r` and `r`
described in [Emacs Integration][4c86], you may want to give them a global binding:

```elisp
(global-set-key (kbd "s-t t") 'mgl-try)
(global-set-key (kbd "s-t r") 'mgl-try-rerun)
(global-set-key (kbd "s-t R") 'mgl-try-rerun-all)
```

The same with `use-package`:

```elisp
(use-package mgl-try :load-path "~/quicklisp/"
  :after slime
  :demand t
  :bind (("s-t t" . mgl-try)
         ("s-t r" . mgl-try-rerun)
         ("s-t R" . mgl-try-rerun-all)))
```


<a id="x-28TRY-3AINSTALL-TRY-ELISP-20FUNCTION-29"></a>
<a id="TRY:INSTALL-TRY-ELISP%20FUNCTION"></a>

- [function] **install-try-elisp** *target-dir*

    Install `mgl-try.el` distributed with this package in `target-dir`.

<a id="x-28TRY-3A-40EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EVENTS%20MGL-PAX:SECTION"></a>

## 4 Events

Try is built around events implemented as [`condition`][83e1]s.
Matching the types of events to [`*debug*`][856d], [`*count*`][3bb4], [`*collect*`][307c], [`*rerun*`][63db],
[`*print*`][7ee9], and [`*describe*`][aa6d] is what gives Try its flexibility.

<a id="x-28TRY-3A-40MIDDLE-LAYER-OF-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@MIDDLE-LAYER-OF-EVENTS%20MGL-PAX:SECTION"></a>

### 4.1 Middle Layer of Events

The event hierarchy is fairly involved, so let's start with the
middle layer because it is the smallest. The condition [`event`][955d] has 4
disjoint subclasses:

- [`trial-start`][b664], starting a [`trial`][99d0] (by executing a [test][dc28]),

- [`verdict`][52e1], the [`outcome`][2656] of a `trial`,

- [`result`][231f], the `outcome` of a [check][bb56], and

- [`error*`][0321], an unexpected [`cl:error`][669b] or unadorned [non-local exit][b815].

```common-lisp
(let (;; We don't want to debug nor print a backtrace for the error below.
      (*debug* nil)
      (*describe* nil))
  ;; signals TRIAL-START / VERDICT-ABORT* on entry / exit
  (with-test (demo)
    ;; signals EXPECTED-RESULT-SUCCESS
    (is t)
    ;; signals UNHANDLED-ERROR with a nested CL:ERROR
    (error "xxx")))
.. DEMO                       ; TRIAL-START
..   ⋅ (IS T)                 ; EXPECTED-RESULT-SUCCESS (⋅)
..   ⊟ "xxx" (SIMPLE-ERROR)   ; UNHANDLED-ERROR (⊟)
.. ⊟ DEMO ⊟1 ⋅1               ; VERDICT-ABORT* (⊟)
..
==> #<TRIAL (WITH-TEST (DEMO)) ABORT* 0.004s ⊟1 ⋅1>
```


<a id="x-28TRY-3A-40CONCRETE-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CONCRETE-EVENTS%20MGL-PAX:SECTION"></a>

### 4.2 Concrete Events

The non-abstract condition classes of events that are actually
signalled are called concrete.

[Checks][bb56]' [`result`][231f]s and [Trials][e6be]' [`verdict`][52e1]s have six concrete subclasses
each:

- [`expected-result-success`][609c7], [`unexpected-result-success`][b72c],
  [`expected-result-failure`][d619], [`unexpected-result-failure`][daeb],
  [`result-skip`][7c3f], [`result-abort*`][ffab]

- [`expected-verdict-success`][06c2], [`unexpected-verdict-success`][062e],
  [`expected-verdict-failure`][30c9], [`unexpected-verdict-failure`][fdf4],
  [`verdict-skip`][5786], [`verdict-abort*`][4805]

Breaking the symmetry between [Checks][bb56] and [Trials][e6be], [`trial-start`][b664] is a
concrete event class, that marks the start of a [`trial`][99d0].

[`error*`][0321] is an abstract class with two concrete subclasses:

- [`unhandled-error`][8f78], signalled when a [`cl:error`][669b] reaches the handler set
  up by [`deftest`][e7ca] or [`with-test`][8f5d], or when the debugger is invoked.

- [`nlx`][b115], signalled when no error was detected by the handler, but the
  trial finishes with a [non-local exit][b815].

These are the 15 concrete event classes.

<a id="x-28TRY-3ACONCRETE-EVENTS-OF-TYPE-20FUNCTION-29"></a>
<a id="TRY:CONCRETE-EVENTS-OF-TYPE%20FUNCTION"></a>

- [function] **concrete-events-of-type** *type*

    The hierarchy of [Events][aaf2] is hairy. Sometimes it's handy to list the
    [Concrete Events][279a] that match a given type. We use this below in the
    documentation.

<a id="x-28TRY-3A-40EVENT-GLUE-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EVENT-GLUE%20MGL-PAX:SECTION"></a>

### 4.3 Event Glue

These condition classes group various bits of the [Concrete Events][279a]
 and the [Middle Layer of Events][3e0c] for ease of reference.

Concrete event classes except [`trial-start`][b664] and [`nlx`][b115] are subclasses of
the hyphen-separated words constituting their name. For example,
[`unexpected-result-failure`][daeb] inherits from [`unexpected`][d6ad], [`result`][231f], and
[`failure`][f92d], so it matches types such as `unexpected` or `(and unexpected
result)`.

<a id="x-28TRY-3AEVENT-20CONDITION-29"></a>
<a id="TRY:EVENT%20CONDITION"></a>

- [condition] **event**

    Common abstract superclass of all events in Try.

<a id="x-28TRY-3AACT-20CONDITION-29"></a>
<a id="TRY:ACT%20CONDITION"></a>

- [condition] **act** *[event][955d]*

    [`event`][955d]s that produce evidence or determine the
    course of a [`trial`][99d0] are `act`s. All events are `act`s except [`trial-start`][b664].
    
    ```common-lisp
    (concrete-events-of-type '(not act))
    => (TRIAL-START)
    ```

[`expected`][b194] and [`unexpected`][d6ad] partition [`act`][247c].

<a id="x-28TRY-3AEXPECTED-20CONDITION-29"></a>
<a id="TRY:EXPECTED%20CONDITION"></a>

- [condition] **expected** *[act][247c]*

    Concrete condition classes with `expected` in their
    name are subclasses of `expected`. [`skip`][69a2] is also a subclass of
    `expected`.
    
    ```common-lisp
    (concrete-events-of-type 'expected)
    => (EXPECTED-RESULT-SUCCESS EXPECTED-RESULT-FAILURE RESULT-SKIP
        EXPECTED-VERDICT-SUCCESS EXPECTED-VERDICT-FAILURE VERDICT-SKIP)
    ```

<a id="x-28TRY-3AUNEXPECTED-20CONDITION-29"></a>
<a id="TRY:UNEXPECTED%20CONDITION"></a>

- [condition] **unexpected** *[act][247c]*

    Concrete condition classes with `unexpected` in their
    name are subclasses of `unexpected`. [`abort*`][8ec3] is also a subclass of
    `unexpected`.
    
    ```common-lisp
    (concrete-events-of-type 'unexpected)
    => (UNEXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-FAILURE RESULT-ABORT*
        UNEXPECTED-VERDICT-SUCCESS UNEXPECTED-VERDICT-FAILURE
        VERDICT-ABORT* UNHANDLED-ERROR NLX)
    ```

[`success`][269a], [`failure`][f92d] and [`dismissal`][0992] partition [`act`][247c].

<a id="x-28TRY-3ASUCCESS-20CONDITION-29"></a>
<a id="TRY:SUCCESS%20CONDITION"></a>

- [condition] **success** *[act][247c]*

    See [Checks][bb56] and [Trial Verdicts][5e1a] for how
    `success` or [`failure`][f92d] is decided.
    
    ```common-lisp
    (concrete-events-of-type 'success)
    => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
        EXPECTED-VERDICT-SUCCESS UNEXPECTED-VERDICT-SUCCESS)
    ```

<a id="x-28TRY-3AFAILURE-20CONDITION-29"></a>
<a id="TRY:FAILURE%20CONDITION"></a>

- [condition] **failure** *[act][247c]*

    See [`success`][269a].
    
    ```common-lisp
    (concrete-events-of-type 'failure)
    => (EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE
        EXPECTED-VERDICT-FAILURE UNEXPECTED-VERDICT-FAILURE)
    ```

<a id="x-28TRY-3ADISMISSAL-20CONDITION-29"></a>
<a id="TRY:DISMISSAL%20CONDITION"></a>

- [condition] **dismissal** *[act][247c]*

    The third possibility after [`success`][269a] and [`failure`][f92d].
    Either [`skip`][69a2] or [`abort*`][8ec3].
    
    ```common-lisp
    (concrete-events-of-type 'dismissal)
    => (RESULT-SKIP RESULT-ABORT* VERDICT-SKIP VERDICT-ABORT*
        UNHANDLED-ERROR NLX)
    ```

[`abort*`][8ec3] and [`skip`][69a2] partition [`dismissal`][0992].

<a id="x-28TRY-3AABORT-2A-20CONDITION-29"></a>
<a id="TRY:ABORT*%20CONDITION"></a>

- [condition] **abort\*** *[unexpected][d6ad] [dismissal][0992]*

    ```common-lisp
    (concrete-events-of-type 'abort*)
    => (RESULT-ABORT* VERDICT-ABORT* UNHANDLED-ERROR NLX)
    ```

<a id="x-28TRY-3ASKIP-20CONDITION-29"></a>
<a id="TRY:SKIP%20CONDITION"></a>

- [condition] **skip** *[expected][b194] [dismissal][0992]*

    ```common-lisp
    (concrete-events-of-type 'skip)
    => (RESULT-SKIP VERDICT-SKIP)
    ```

<a id="x-28TRY-3ALEAF-20CONDITION-29"></a>
<a id="TRY:LEAF%20CONDITION"></a>

- [condition] **leaf** *[act][247c]*

    Events that do not mark a [`trial`][99d0]'s
    start ([`trial-start`][b664]) or end ([`verdict`][52e1]) are `leaf` events. These are the
    leaves of the tree of nested trials delineated by their `trial-start`
    and `verdict` events.
    
    ```common-lisp
    (concrete-events-of-type 'leaf)
    => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
        EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE RESULT-SKIP
        RESULT-ABORT* UNHANDLED-ERROR NLX)
    ```
    
    `leaf` [`event`][955d]s are [`result`][231f]s of [Checks][bb56] and also [`error*`][0321]s.
    
    ```common-lisp
    (equal (concrete-events-of-type 'leaf)
           (concrete-events-of-type '(or result error*)))
    => T
    ```
    
    Equivalently, `leaf` is the complement of [`trial-event`][b36a].
    
    ```common-lisp
    (equal (concrete-events-of-type 'leaf)
           (concrete-events-of-type '(not trial-event)))
    => T
    ```

The following types are shorthands.

<a id="x-28TRY-3AEXPECTED-SUCCESS-20TYPE-29"></a>
<a id="TRY:EXPECTED-SUCCESS%20TYPE"></a>

- [type] **expected-success**

    A shorthand for `(and expected success)`.

<a id="x-28TRY-3AUNEXPECTED-SUCCESS-20TYPE-29"></a>
<a id="TRY:UNEXPECTED-SUCCESS%20TYPE"></a>

- [type] **unexpected-success**

    A shorthand for `(and unexpected success)`.

<a id="x-28TRY-3AEXPECTED-FAILURE-20TYPE-29"></a>
<a id="TRY:EXPECTED-FAILURE%20TYPE"></a>

- [type] **expected-failure**

    A shorthand for `(and expected failure)`.

<a id="x-28TRY-3AUNEXPECTED-FAILURE-20TYPE-29"></a>
<a id="TRY:UNEXPECTED-FAILURE%20TYPE"></a>

- [type] **unexpected-failure**

    A shorthand for `(and unexpected failure)`.

[`pass`][21d9] and [`fail`][d5ea] partition [`act`][247c].

<a id="x-28TRY-3APASS-20TYPE-29"></a>
<a id="TRY:PASS%20TYPE"></a>

- [type] **pass**

    An [`outcome`][2656] that's not an [`abort*`][8ec3] or an [`unexpected-failure`][b5cb].
    `pass` is equivalent to `(not fail)`. `pass`es are signalled with
    [`signal`][8f49].
    
    ```common-lisp
    (concrete-events-of-type 'pass)
    => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
        EXPECTED-RESULT-FAILURE RESULT-SKIP EXPECTED-VERDICT-SUCCESS
        UNEXPECTED-VERDICT-SUCCESS EXPECTED-VERDICT-FAILURE VERDICT-SKIP)
    ```

<a id="x-28TRY-3AFAIL-20TYPE-29"></a>
<a id="TRY:FAIL%20TYPE"></a>

- [type] **fail**

    An [`abort*`][8ec3] or an [`unexpected-failure`][b5cb]. `fail` conditions are signalled
    with [`error`][35ba]. See [`pass`][21d9].
    
    ```common-lisp
    (concrete-events-of-type 'fail)
    => (UNEXPECTED-RESULT-FAILURE RESULT-ABORT* UNEXPECTED-VERDICT-FAILURE
        VERDICT-ABORT* UNHANDLED-ERROR NLX)
    ```

<a id="x-28TRY-3A-40PRINTING-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@PRINTING-EVENTS%20MGL-PAX:SECTION"></a>

### 4.4 Printing Events

<a id="x-28TRY-3A-2AEVENT-PRINT-BINDINGS-2A-20VARIABLE-29"></a>
<a id="TRY:*EVENT-PRINT-BINDINGS*%20VARIABLE"></a>

- [variable] **\*event-print-bindings\*** *((\*print-circle\* t) (sb-ext:\*print-circle-not-shared\* nil))*

    [Try var][0d7a]. [`event`][955d]s are conditions signalled in code that may change printer
    variables such as [`*print-circle*`][c8cb], [`*print-length*`][8f7a], etc. To control
    how events are printed, the list of variable bindings in
    `*event-print-bindings*` is established whenever an `event` is printed
    as if with:
    
    ```
    (progv (mapcar #'first *event-print-bindings*)
           (mapcar #'second *event-print-bindings*)
      ...)
    ```
    
    The default value ensures that shared structure is recognized (see
    [Captures][3d27]). If the `#n#` syntax feels cumbersome, then change this
    variable.

<a id="x-28TRY-3A-40EVENT-RESTARTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EVENT-RESTARTS%20MGL-PAX:SECTION"></a>

### 4.5 Event Restarts

Only [`record-event`][ce49] is applicable to all [`event`][955d]s. See
[Check Restarts][2364], [Trial Restarts][5355] for more.

<a id="x-28TRY-3ARECORD-EVENT-20FUNCTION-29"></a>
<a id="TRY:RECORD-EVENT%20FUNCTION"></a>

- [function] **record-event** *&optional condition*

    This restart is always the first restart available when an [`event`][955d] is
    signalled running under [`try`][b602] (i.e. there is a [`current-trial`][e186]). `try`
    always invokes `record-event` when handling events.

<a id="x-28TRY-3A-40OUTCOMES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@OUTCOMES%20MGL-PAX:SECTION"></a>

### 4.6 Outcomes

<a id="x-28TRY-3AOUTCOME-20CONDITION-29"></a>
<a id="TRY:OUTCOME%20CONDITION"></a>

- [condition] **outcome** *[act][247c]*

    An `outcome` is the resolution of either a [`trial`][99d0] or a
    [check][bb56], corresponding to subclasses [`verdict`][52e1] and [`result`][231f].
    
    ```common-lisp
    (concrete-events-of-type '(not outcome))
    => (TRIAL-START UNHANDLED-ERROR NLX)
    ```

<a id="x-28TRY-3AWITH-EXPECTED-OUTCOME-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-EXPECTED-OUTCOME%20MGL-PAX:MACRO"></a>

- [macro] **with-expected-outcome** *(expected-type) &body body*

    When an [`outcome`][2656] is to be signalled, `expected-type` determines
    whether it's going to be [`expected`][b194]. The concrete `outcome` classes are
    `{expected,unexpected}-{result,verdict}-{success,failure}` (see
    [Events][aaf2]), of which [`result`][231f] or [`verdict`][52e1] and [`success`][269a] or [`failure`][f92d] are
    already known. If a `result` `failure` is to be signalled, then the
    moral equivalent of `(subtypep '(and result failure) expected-type)`
    is evaluated and depending on whether it's true,
    [`expected-result-failure`][d619] or [`unexpected-result-failure`][daeb] is signalled.
    
    By default, `success` is expected. The following example shows how to
    expect both `success` and `failure` for `result`s, while requiring
    `verdict`s to succeed:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or result (and verdict success)))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   × (IS NIL)
    .. ⋅ T1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ×1>
    ```
    
    This is equivalent to `(with-failure-expected () ...)`. To make
    result failures expected but result successes unexpected:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result failure) (and verdict success)))
        (with-test (t1)
          (is t)
          (is nil))))
    .. T1
    ..   ⊡ (IS T)
    ..   × (IS NIL)
    .. ⋅ T1 ⊡1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ⊡1 ×1>
    ```
    
    This is equivalent to `(with-failure-expected ('failure) ...)`. The
    final example leaves result failures unexpected but makes both
    verdict successes and failures expected:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result success) verdict))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   ⊠ (IS NIL)
    .. × T1 ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-FAILURE 0.004s ⊠1>
    ```

<a id="x-28TRY-3AWITH-FAILURE-EXPECTED-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-FAILURE-EXPECTED%20MGL-PAX:MACRO"></a>

- [macro] **with-failure-expected** *(&optional (result-expected-type t) (verdict-expected-type ''success)) &body body*

    A convenience macro on top of [`with-expected-outcome`][1d97],
    `with-failure-expected` expects [`verdict`][52e1]s to have `verdict-expected-type`
    and [`result`][231f]s to have `result-expected-type`. A simple
    `(with-failure-expected () ...)` makes all `result` [`success`][269a]es and
    [`failure`][f92d]s [`expected`][b194]. `(with-failure-expected ('failure) ..)` expects
    `failure`s only, and any `success`es will be [`unexpected`][d6ad].

<a id="x-28TRY-3AWITH-SKIP-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-SKIP%20MGL-PAX:MACRO"></a>

- [macro] **with-skip** *(&optional (skip t)) &body body*

    `with-skip` skips checks and trials. It forces an immediate
    [`skip-trial`][f45a] whenever a trial is started (which turns into a
    [`verdict-skip`][5786]) and makes checks (without intervening trials, of
    course) evaluate normally but signal [`result-skip`][7c3f]. `skip` being `nil`
    cancels the effect of any enclosing `with-skip` with `skip` true.

<a id="x-28TRY-3A-40OUTCOME-RESTARTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@OUTCOME-RESTARTS%20MGL-PAX:SECTION"></a>

#### 4.6.1 Outcome Restarts

<a id="x-28TRY-3AFORCE-EXPECTED-SUCCESS-20FUNCTION-29"></a>
<a id="TRY:FORCE-EXPECTED-SUCCESS%20FUNCTION"></a>

- [function] **force-expected-success** *&optional outcome*

    [Handle][59c3] the [`outcome`][2656] being signalled, and signal an
    [`expected-result-success`][609c7] or [`expected-verdict-success`][06c2] for when the
    `outcome` is a [`result`][231f] or a [`verdict`][52e1], respectively.

<a id="x-28TRY-3AFORCE-UNEXPECTED-SUCCESS-20FUNCTION-29"></a>
<a id="TRY:FORCE-UNEXPECTED-SUCCESS%20FUNCTION"></a>

- [function] **force-unexpected-success** *&optional outcome*

    [Handle][59c3] the [`outcome`][2656] being signalled, and signal an
    [`unexpected-result-success`][b72c] or [`unexpected-verdict-success`][062e] for when the
    `outcome` is a [`result`][231f] or a [`verdict`][52e1], respectively.

<a id="x-28TRY-3AFORCE-EXPECTED-FAILURE-20FUNCTION-29"></a>
<a id="TRY:FORCE-EXPECTED-FAILURE%20FUNCTION"></a>

- [function] **force-expected-failure** *&optional outcome*

    [Handle][59c3] the [`outcome`][2656] being signalled, and signal an
    [`expected-result-failure`][d619] or [`expected-verdict-failure`][30c9] for when the
    `outcome` is a [`result`][231f] or a [`verdict`][52e1], respectively.

<a id="x-28TRY-3AFORCE-UNEXPECTED-FAILURE-20FUNCTION-29"></a>
<a id="TRY:FORCE-UNEXPECTED-FAILURE%20FUNCTION"></a>

- [function] **force-unexpected-failure** *&optional outcome*

    [Handle][59c3] the [`outcome`][2656] being signalled, and signal an
    [`unexpected-result-failure`][daeb] or [`unexpected-verdict-failure`][fdf4] for when the
    `outcome` is a [`result`][231f] or a [`verdict`][52e1], respectively.

<a id="x-28TRY-3A-40CHECKS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CHECKS%20MGL-PAX:SECTION"></a>

#### 4.6.2 Checks

Checks are like [`cl:assert`][97ee]s, they check whether some condition holds
and signal an [`outcome`][2656]. The outcome signalled for checks is a
subclass of [`result`][231f].

Take, for example, `(is (= x 5))`. Depending on whether `x` is
indeed 5, some kind of `result` [`success`][269a] or [`failure`][f92d] will be signalled.
[`with-expected-outcome`][1d97] determines whether it's [`expected`][b194] or
[`unexpected`][d6ad], and we have one of [`expected-result-success`][609c7],
[`unexpected-result-success`][b72c], [`expected-result-failure`][d619],
[`unexpected-result-failure`][daeb] to signal. Furthermore, if [`with-skip`][b71e] is in
effect, then [`result-skip`][7c3f] is signalled.

The result is signalled with the function [`signal`][8f49] if it is a [`pass`][21d9],
else it's signalled with [`error`][35ba]. This distinction matters
only if the event is not handled, which is never the case in a
[`trial`][99d0]. However, standalone checks – those not enclosed by a trial –
invoke the debugger on `result`s which are not of type [`pass`][21d9].

The signalled `result` is not final until [`record-event`][ce49] is invoked on
it, and it can be changed with the [Outcome Restarts][7ef5] and the
[Check Restarts][2364].

<a id="x-28TRY-3ARESULT-20CONDITION-29"></a>
<a id="TRY:RESULT%20CONDITION"></a>

- [condition] **result** *[leaf][f58d] [outcome][2656]*

<a id="x-28TRY-3AEXPECTED-RESULT-SUCCESS-20CONDITION-29"></a>
<a id="TRY:EXPECTED-RESULT-SUCCESS%20CONDITION"></a>

- [condition] **expected-result-success** *[expected][b194] [result][231f] [success][269a]*

<a id="x-28TRY-3AUNEXPECTED-RESULT-SUCCESS-20CONDITION-29"></a>
<a id="TRY:UNEXPECTED-RESULT-SUCCESS%20CONDITION"></a>

- [condition] **unexpected-result-success** *[unexpected][d6ad] [result][231f] [success][269a]*

<a id="x-28TRY-3AEXPECTED-RESULT-FAILURE-20CONDITION-29"></a>
<a id="TRY:EXPECTED-RESULT-FAILURE%20CONDITION"></a>

- [condition] **expected-result-failure** *[expected][b194] [result][231f] [failure][f92d]*

<a id="x-28TRY-3AUNEXPECTED-RESULT-FAILURE-20CONDITION-29"></a>
<a id="TRY:UNEXPECTED-RESULT-FAILURE%20CONDITION"></a>

- [condition] **unexpected-result-failure** *[unexpected][d6ad] [result][231f] [failure][f92d]*

<a id="x-28TRY-3ARESULT-SKIP-20CONDITION-29"></a>
<a id="TRY:RESULT-SKIP%20CONDITION"></a>

- [condition] **result-skip** *[result][231f] [skip][69a2]*

<a id="x-28TRY-3ARESULT-ABORT-2A-20CONDITION-29"></a>
<a id="TRY:RESULT-ABORT*%20CONDITION"></a>

- [condition] **result-abort\*** *[result][231f] [abort\*][8ec3] [dismissal][0992]*

<a id="x-28TRY-3A-40CHECK-RESTARTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CHECK-RESTARTS%20MGL-PAX:SECTION"></a>

##### Check Restarts

<a id="x-28TRY-3AABORT-CHECK-20FUNCTION-29"></a>
<a id="TRY:ABORT-CHECK%20FUNCTION"></a>

- [function] **abort-check** *&optional condition*

    Change the [`outcome`][2656] of the check being signalled to [`result-abort*`][ffab].
    `result-abort*`, being a [`fail`][d5ea], will cause the check to return `nil` if
    [`record-event`][ce49] is invoked on it.

<a id="x-28TRY-3ASKIP-CHECK-20FUNCTION-29"></a>
<a id="TRY:SKIP-CHECK%20FUNCTION"></a>

- [function] **skip-check** *&optional condition*

    Change the [`outcome`][2656] of the check being signalled to [`result-skip`][7c3f].
    `result-skip`, being a [`pass`][21d9], will cause the check to return `t` if
    [`continue`][87a5] or [`record-event`][ce49] is invoked on it.

<a id="x-28TRY-3ARETRY-CHECK-20FUNCTION-29"></a>
<a id="TRY:RETRY-CHECK%20FUNCTION"></a>

- [function] **retry-check** *&optional condition*

    Initiate a [non-local exit][b815] to go reevaluate the forms
    wrapped by the check without signalling an [`outcome`][2656].

<a id="x-28TRY-3A-40TRIALS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TRIALS%20MGL-PAX:SECTION"></a>

#### 4.6.3 Trials

<a id="x-28TRY-3ATRIAL-20CLASS-29"></a>
<a id="TRY:TRIAL%20CLASS"></a>

- [class] **trial** *sb-mop:funcallable-standard-object*

    Trials are records of calls to tests (see
    [Counting Events][886e], [Collecting Events][52e5]). Their behaviour as [funcallable instance][2eef]s
    is explained in [Rerunning Trials][e4ac].
    
    There are three ways to acquire a `trial` object: by calling
    [`current-trial`][e186], through the lexical binding of the symbol that names
    the test, or through the return value of a test:
    
    ```common-lisp
    (deftest xxx ()
      (prin1 xxx))
    
    (xxx)
    .. #<TRIAL (XXX) RUNNING>
    ==> #<TRIAL (XXX) EXPECTED-SUCCESS 0.000s>
    ```
    
    `with-trial` can also provide access to its `trial`:
    
    ```common-lisp
    (with-test (t0)
      (prin1 t0))
    .. #<TRIAL (WITH-TEST (T0)) RUNNING>
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s>
    ```
    
    `trial`s are not to be instantiated by client code.

<a id="x-28TRY-3ACURRENT-TRIAL-20FUNCTION-29"></a>
<a id="TRY:CURRENT-TRIAL%20FUNCTION"></a>

- [function] **current-trial**

    [`trial`][99d0]s, like the calls to tests they stand for, nest. `current-trial`
    returns the innermost trial. If there is no currently running test,
    then an error is signalled. The returned trial is [`runningp`][5d4a].

<a id="x-28TRY-3A-40TRIAL-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TRIAL-EVENTS%20MGL-PAX:SECTION"></a>

##### Trial Events

<a id="x-28TRY-3ATRIAL-EVENT-20CONDITION-29"></a>
<a id="TRY:TRIAL-EVENT%20CONDITION"></a>

- [condition] **trial-event** *[event][955d]*

    A `trial-event` is either a [`trial-start`][b664] or a
    [`verdict`][52e1].

<a id="x-28TRY-3ATRIAL-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-EVENT-29-29"></a>
<a id="TRY:TRIAL%20%28MGL-PAX:READER%20TRY:TRIAL-EVENT%29"></a>

- [reader] **trial** *[trial-event][b36a] (:trial)*

<a id="x-28TRY-3ATRIAL-START-20CONDITION-29"></a>
<a id="TRY:TRIAL-START%20CONDITION"></a>

- [condition] **trial-start** *[trial-event][b36a]*

    `trial-start` is signalled when a test function
    (see [Tests][dc28]) is entered and a [`trial`][99d0] is started. When this happens
    that trial is already the [`current-trial`][e186], and the [Trial Restarts][5355] are
    available. It is also signalled when a trial is retried:
    
    ```common-lisp
    (let ((*print* nil)
          (n 0))
      (with-test ()
        (handler-bind ((trial-start
                         (lambda (c)
                           (format t "TRIAL-START for ~S retry#~S~%"
                                   (test-name (trial c))
                                   (n-retries (trial c))))))
          (with-test (this)
            (incf n)
            (when (< n 3)
              (retry-trial))))))
    .. TRIAL-START for THIS retry#0
    .. TRIAL-START for THIS retry#1
    .. TRIAL-START for THIS retry#2
    ..
    ```
    
    The matching of `trial-start` events is less straightforward than that
    of other [`event`][955d]s.
    
    - When a `trial-start` event matches the `collect` type (see
      [Collecting Events][52e5]), its [`trial`][0f05] is collected.
    
    - Similarly, when a `trial-start` matches the `print`
      type (see [Printing Events][b3f9]), it is printed immediately, and its trial's
      [`verdict`][52e1] will be printed too regardless of whether it matches
      `print`. If `trial-start` does not match
      `print`, it may still be printed if for example
      [`*print-parent*`][cc23] requires it.
    
    - When a `trial-start` matches the `rerun` type (see [Rerunning Trials][e4ac]), its
      [`trial`][0f05] may be rerun.
    
    - Also, see [`with-skip`][b71e].

<a id="x-28TRY-3AVERDICT-20CONDITION-29"></a>
<a id="TRY:VERDICT%20CONDITION"></a>

- [condition] **verdict** *[trial-event][b36a] [outcome][2656]*

    A `verdict` is the [`outcome`][2656] of a [`trial`][99d0]. It is one of
    `{expected,unexpected}-verdict-{success,failure}`, [`verdict-skip`][5786] and
    [`verdict-abort*`][4805]. Regarding how the verdict type is determined, see
    [Trial Verdicts][5e1a].
    
    Verdicts are signalled while their [`trial`][0f05] is
    still the [`current-trial`][e186], and [Trial Restarts][5355] are still available.
    
    ```common-lisp
    (try (lambda ()
           (handler-bind (((and verdict failure) #'retry-trial))
             (with-test (this)
               (is (zerop (random 2)))))))
    .. (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>)
    ..   THIS
    ..     ⊠ (IS (ZEROP #1=(RANDOM 2)))
    ..       where
    ..         #1# = 1
    ..   THIS retry #1
    ..     ⋅ (IS (ZEROP (RANDOM 2)))
    ..   ⋅ THIS ⋅1
    .. ⋅ (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) ⋅1
    ..
    ==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) EXPECTED-SUCCESS 0.000s ⋅1>
    ```

<a id="x-28TRY-3AEXPECTED-VERDICT-SUCCESS-20CONDITION-29"></a>
<a id="TRY:EXPECTED-VERDICT-SUCCESS%20CONDITION"></a>

- [condition] **expected-verdict-success** *[expected][b194] [verdict][52e1] [success][269a]*

<a id="x-28TRY-3AUNEXPECTED-VERDICT-SUCCESS-20CONDITION-29"></a>
<a id="TRY:UNEXPECTED-VERDICT-SUCCESS%20CONDITION"></a>

- [condition] **unexpected-verdict-success** *[unexpected][d6ad] [verdict][52e1] [success][269a]*

<a id="x-28TRY-3AEXPECTED-VERDICT-FAILURE-20CONDITION-29"></a>
<a id="TRY:EXPECTED-VERDICT-FAILURE%20CONDITION"></a>

- [condition] **expected-verdict-failure** *[expected][b194] [verdict][52e1] [failure][f92d]*

<a id="x-28TRY-3AUNEXPECTED-VERDICT-FAILURE-20CONDITION-29"></a>
<a id="TRY:UNEXPECTED-VERDICT-FAILURE%20CONDITION"></a>

- [condition] **unexpected-verdict-failure** *[unexpected][d6ad] [verdict][52e1] [failure][f92d]*

<a id="x-28TRY-3AVERDICT-SKIP-20CONDITION-29"></a>
<a id="TRY:VERDICT-SKIP%20CONDITION"></a>

- [condition] **verdict-skip** *[verdict][52e1] [skip][69a2]*

<a id="x-28TRY-3AVERDICT-ABORT-2A-20CONDITION-29"></a>
<a id="TRY:VERDICT-ABORT*%20CONDITION"></a>

- [condition] **verdict-abort\*** *[verdict][52e1] [abort\*][8ec3] [dismissal][0992]*

<a id="x-28TRY-3A-40TRIAL-VERDICTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TRIAL-VERDICTS%20MGL-PAX:SECTION"></a>

##### Trial Verdicts

When a trial has finished, a [`verdict`][52e1] is signalled. The verdict's
type is determined as follows.

- It is a [`verdict-skip`][5786] if

    - [`skip-trial`][f45a] was called on the trial, or

    - [`abort-trial`][4f9f], `skip-trial`, or [`retry-trial`][fae3] was called on an
      enclosing trial, and these were not overruled by a later
      `abort-trial` or `retry-trial` on the trial.

- It is a [`verdict-abort*`][4805] if `abort-trial` was called on the trial, and
  it wasn't overruled by a later `skip-trial` or `retry-trial`.

- If all children (including those not collected in [`children`][de7d]) of the
  trial [`pass`][21d9], then the verdict will be a [`success`][269a], else it will be a
  [`failure`][f92d].

- Subject to the [`with-expected-outcome`][1d97] in effect,
  `{expected,unexpected}-verdict-{success,failure}` is the type of
  the verdict which will be signalled.

The verdict of this type is signalled, but its type can be changed
by the [Outcome Restarts][7ef5] or the [Trial Restarts][5355] before [`record-event`][ce49]
is invoked on it.

<a id="x-28TRY-3AVERDICT-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>
<a id="TRY:VERDICT%20%28MGL-PAX:READER%20TRY:TRIAL%29"></a>

- [reader] **verdict** *[trial][99d0] (= nil)*

    The [`verdict`][52e1] [`event`][955d] signalled when this
    `trial` finished or `nil` if it has not finished yet.

<a id="x-28TRY-3ARUNNINGP-20FUNCTION-29"></a>
<a id="TRY:RUNNINGP%20FUNCTION"></a>

- [function] **runningp** *trial*

    See if the function call associated with `trial` has not returned yet.
    Trials that are not running have a [`verdict`][52e1] and are said to be
    finished.

<a id="x-28TRY-3APASSEDP-20FUNCTION-29"></a>
<a id="TRY:PASSEDP%20FUNCTION"></a>

- [function] **passedp** *trial*

    See if `trial` has finished and its [`verdict`][4bec] is a
    [`pass`][21d9].

<a id="x-28TRY-3AFAILEDP-20FUNCTION-29"></a>
<a id="TRY:FAILEDP%20FUNCTION"></a>

- [function] **failedp** *trial*

    See if `trial` has finished and its [`verdict`][4bec] is a
    [`fail`][d5ea].

<a id="x-28TRY-3A-40TRIAL-RESTARTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TRIAL-RESTARTS%20MGL-PAX:SECTION"></a>

##### Trial Restarts

There are three restarts available for manipulating running
trials: [`abort-trial`][4f9f], [`skip-trial`][f45a], and [`retry-trial`][fae3]. They may be
invoked programmatically or from the debugger. `abort-trial` is also
invoked by [`try`][b602] when encountering [`unhandled-error`][8f78].

The functions below invoke one of these restarts associated with a
[`trial`][99d0]. It is an error to call them on trials that are not [`runningp`][5d4a],
but they may be called on trials other than the [`current-trial`][e186]. In
that case, any intervening trials are skipped.

```common-lisp
;; Skipped trials are marked with '-' in the output.
(with-test (outer)
  (with-test (inner)
    (is t)
    (skip-trial nil outer)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   - INNER ⋅1
.. - OUTER ⋅1
..
==> #<TRIAL (WITH-TEST (OUTER)) SKIP 0.000s ⋅1>
```

Furthermore, all three restarts initiate a [non-local exit][b815] to
return from the trial. If during the unwinding of the stack, the
non-local-exit is cancelled (see [cancelled non-local exit][7ab6]), the appropriate
restart will be invoked upon returning from the trial. In the
following example, the non-local exit from a skip is cancelled by a
[`throw`][e760].

```common-lisp
(with-test (some-test)
  (catch 'foo
    (unwind-protect
         (skip-trial)
      (throw 'foo nil)))
  (is t :msg "check after skip"))
.. SOME-TEST
..   ⋅ check after skip
.. - SOME-TEST ⋅1
..
==> #<TRIAL (WITH-TEST (SOME-TEST)) SKIP 0.000s ⋅1>
```

In the next example, the non-local exit from a skip is cancelled by
an [`error`][669b], which triggers an `abort-trial`.

```common-lisp
(let ((*debug* nil)
      (*describe* nil))
  (with-test (foo)
    (unwind-protect
         (skip-trial)
      (error "xxx"))))
.. FOO
..   ⊟ "xxx" (SIMPLE-ERROR)
.. ⊟ FOO ⊟1
..
==> #<TRIAL (WITH-TEST (FOO)) ABORT* 0.000s ⊟1>
```

All three restarts may be invoked on any [`event`][955d], including the
trial's own [`trial-start`][b664] and [`verdict`][52e1]. If their `condition`
argument is an `event` (`retry-trial` has a special case here), they
also record it (as in [`record-event`][ce49]) to ensure that when they handle
an `event` in the debugger or programmatically that event is not
dropped.

<a id="x-28TRY-3AABORT-TRIAL-20FUNCTION-29"></a>
<a id="TRY:ABORT-TRIAL%20FUNCTION"></a>

- [function] **abort-trial** *&optional condition (trial (current-trial))*

    Invoke the `abort-trial` restart of a [`runningp`][5d4a] `trial`.
    
    When `condition` is a [`verdict`][52e1] for `trial`, `abort-trial` signals a new
    verdict of type [`verdict-abort*`][4805]. This behaviour is similar to that
    of [`abort-check`][826a]. Else, the `abort-trial` restart may record `condition`,
    then it initiates a [non-local exit][b815] to return from the test
    function with `verdict-abort*`. If during the unwinding [`skip-trial`][f45a]
    or [`retry-trial`][fae3] is called, then the abort is cancelled.
    
    Since [`abort*`][8ec3] is an [`unexpected`][d6ad] [`event`][955d], `abort-trial` is rarely used
    programmatically. Signalling any error in a trial that's not caught
    before the trial's handler catches it will get turned into an
    [`unhandled-error`][8f78], and [`try`][b602] will invoke `abort-trial` with it. Thus,
    instead of invoking `abort-trial` directly, signalling an error will
    often suffice.

<a id="x-28TRY-3ASKIP-TRIAL-20FUNCTION-29"></a>
<a id="TRY:SKIP-TRIAL%20FUNCTION"></a>

- [function] **skip-trial** *&optional condition (trial (current-trial))*

    Invoke the `skip-trial` restart of a [`runningp`][5d4a] `trial`.
    
    When `condition` is a [`verdict`][52e1] for `trial`, `skip-trial` signals a new
    verdict of type [`verdict-skip`][5786]. This behaviour is similar to that of
    [`skip-check`][fb0e]. Else, the `skip-trial` restart may record `condition`, then
    it initiates a [non-local exit][b815] to return from the test
    function with `verdict-skip`. If during the unwinding [`abort-trial`][4f9f] or
    [`retry-trial`][fae3] is called, then the skip is cancelled.
    
    ```common-lisp
    (with-test (skipped)
      (handler-bind ((unexpected-result-failure #'skip-trial))
        (is nil)))
    .. SKIPPED
    ..   ⊠ (IS NIL)
    .. - SKIPPED ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (SKIPPED)) SKIP 0.000s ⊠1>
    ```
    
    Invoking `skip-trial` on the `trial`'s own [`trial-start`][b664] skips the trial
    being started.
    
    ```common-lisp
    (let ((*print* '(or outcome leaf)))
      (with-test (parent)
        (handler-bind ((trial-start #'skip-trial))
          (with-test (child)
            (is nil)))))
    .. PARENT
    ..   - CHILD
    .. ⋅ PARENT
    ..
    ```

<a id="x-28TRY-3ARETRY-TRIAL-20FUNCTION-29"></a>
<a id="TRY:RETRY-TRIAL%20FUNCTION"></a>

- [function] **retry-trial** *&optional condition (trial (current-trial))*

    Invoke the `retry-trial` restart of [`runningp`][5d4a] `trial`. The `retry-trial`
    restart may record `condition`, then it initiates a [non-local
    exit][b815] to go back to the beginning of the test function. If the
    non-local exit completes, then
    
    - ([`n-retries`][b33f] `trial`) is incremented,
    
    - collected results and trials are cleared (see [Collecting Events][52e5]),
    
    - counts are zeroed (see [Counting Events][886e]), and
    
    - [`trial-start`][b664] is signalled again.
    
    If during the unwinding [`abort-trial`][4f9f] or [`skip-trial`][f45a] is called, then
    the retry is cancelled.
    
    `condition` (which may be `nil`) is recorded if it is an [`event`][955d] but not
    the [`verdict`][52e1] of `trial`, and the [`record-event`][ce49] restart is available.

<a id="x-28TRY-3AN-RETRIES-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>
<a id="TRY:N-RETRIES%20%28MGL-PAX:READER%20TRY:TRIAL%29"></a>

- [reader] **n-retries** *[trial][99d0] (:n-retries = 0)*

    The number of times this `trial` has
    been retried. See [`retry-trial`][fae3].

<a id="x-28TRY-3A-40ERRORS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@ERRORS%20MGL-PAX:SECTION"></a>

### 4.7 Errors

<a id="x-28TRY-3AERROR-2A-20CONDITION-29"></a>
<a id="TRY:ERROR*%20CONDITION"></a>

- [condition] **error\*** *[abort\*][8ec3] [leaf][f58d]*

    Either [`unhandled-error`][8f78] or [`nlx`][b115], `error*` causes or
    represents abnormal termination of a [`trial`][99d0]. [`abort-trial`][4f9f] can be
    called with `error*`s, but there is little need for explicitly doing
    so as [`record-event`][ce49], which [`try`][b602] invokes, takes care of this.

<a id="x-28TRY-3ATEST-NAME-20-28MGL-PAX-3AREADER-20TRY-3AERROR-2A-29-29"></a>
<a id="TRY:TEST-NAME%20%28MGL-PAX:READER%20TRY:ERROR*%29"></a>

- [reader] **test-name** *[error\*][0321] (:test-name)*

<a id="x-28TRY-3AUNHANDLED-ERROR-20CONDITION-29"></a>
<a id="TRY:UNHANDLED-ERROR%20CONDITION"></a>

- [condition] **unhandled-error** *[error\*][0321]*

    Signalled when a [`cl:error`][d162] condition reaches the
    handlers set up by [`deftest`][e7ca] or [`with-test`][8f5d], or when their
    [`*debugger-hook*`][1cdc] is invoked with a condition that's not an [`event`][955d].
    
    Note that if [`nested-condition`][0aad] (the original `cl:error`) has restarts
    [associated][bba7] with it, these are not going to be
    associated with its [`unhandled-error`][8f78] condition, which may restrict
    debugger the list of available restarts in the debugger.

<a id="x-28TRY-3ANESTED-CONDITION-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>
<a id="TRY:NESTED-CONDITION%20%28MGL-PAX:READER%20TRY:UNHANDLED-ERROR%29"></a>

- [reader] **nested-condition** *[unhandled-error][8f78] (:condition = 'nil)*

<a id="x-28TRY-3ABACKTRACE-OF-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>
<a id="TRY:BACKTRACE-OF%20%28MGL-PAX:READER%20TRY:UNHANDLED-ERROR%29"></a>

- [reader] **backtrace-of** *[unhandled-error][8f78] (:backtrace = 'nil)*

<a id="x-28TRY-3ADEBUGGER-INVOKED-P-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>
<a id="TRY:DEBUGGER-INVOKED-P%20%28MGL-PAX:READER%20TRY:UNHANDLED-ERROR%29"></a>

- [reader] **debugger-invoked-p** *[unhandled-error][8f78] (:debugger-invoked-p = 'nil)*

<a id="x-28TRY-3A-2AGATHER-BACKTRACE-2A-20VARIABLE-29"></a>
<a id="TRY:*GATHER-BACKTRACE*%20VARIABLE"></a>

- [variable] **\*gather-backtrace\*** *t*

    [Try var][0d7a]. Capturing the backtrace can be expensive. `*gather-backtrace*`
    controls whether [`unhandled-error`][8f78]s shall have their [`backtrace-of`][3ace]
    populated. Also, see [`*print-backtrace*`][7647].

<a id="x-28TRY-3ANLX-20CONDITION-29"></a>
<a id="TRY:NLX%20CONDITION"></a>

- [condition] **nlx** *[error\*][0321]*

    Representing a [non-local exit][b815] of unknown
    origin, this is signalled if a [`trial`][99d0] does not return normally
    although it should have because it was not dismissed (see [`dismissal`][0992],
    [`skip-trial`][f45a], [`abort-trial`][4f9f]). In this case, there is no [`cl:error`][669b]
    associated with the event.

<a id="x-28TRY-3A-40CATEGORIES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CATEGORIES%20MGL-PAX:SECTION"></a>

### 4.8 Categories

Categories determine how event types are printed and events of
what types are counted together.

The default value of [`*categories*`][e949] is

```
((abort*             :marker "⊟")
 (unexpected-failure :marker "⊠")
 (unexpected-success :marker "⊡")
 (skip               :marker "-")
 (expected-failure   :marker "×")
 (expected-success   :marker "⋅"))
```

which says that all concrete [`event`][955d]s that are of type [`abort*`][8ec3] (i.e.
[`result-abort*`][ffab], [`verdict-abort*`][4805], [`unhandled-error`][8f78], and [`nlx`][b115]) are to
be marked with `"⊟"` when printed (see [Printing Events][b3f9]). Also, the six
types define six counters for [Counting Events][886e]. Note that [`unexpected`][d6ad] events
have the same marker as their [`expected`][b194] counterpart but squared.

<a id="x-28TRY-3A-2ACATEGORIES-2A-20VARIABLE-29"></a>
<a id="TRY:*CATEGORIES*%20VARIABLE"></a>

- [variable] **\*categories\*** *"- see above -"*

    [Try var][0d7a]. A list of elements like `(type &key marker)`. When [Printing Events][b3f9],
    [Concrete Events][279a] are printed with the marker of the first matching
    type. When [Counting Events][886e], the counts associated with all matching types are
    incremented.

<a id="x-28TRY-3AFANCY-STD-CATEGORIES-20FUNCTION-29"></a>
<a id="TRY:FANCY-STD-CATEGORIES%20FUNCTION"></a>

- [function] **fancy-std-categories**

    Returns the default value of [`*categories*`][e949] (see [Categories][03ec]),
    which contains some fancy Unicode characters.

<a id="x-28TRY-3AASCII-STD-CATEGORIES-20FUNCTION-29"></a>
<a id="TRY:ASCII-STD-CATEGORIES%20FUNCTION"></a>

- [function] **ascii-std-categories**

    Returns a value suitable for [`*categories*`][e949], which uses only ASCII
    characters for the markers.
    
    ```
    '((abort*             :marker "!")
      (unexpected-failure :marker "F")
      (unexpected-success :marker ":")
      (skip               :marker "-")
      (expected-failure   :marker "f")
      (expected-success   :marker "."))
    ```

<a id="x-28TRY-3A-40IS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@IS%20MGL-PAX:SECTION"></a>

## 5 The `is` Macro

[`is`][80d6] is the fundamental one among [Checks][bb56], on which all
the others are built, and it is a replacement for [`cl:assert`][97ee] that can
capture values of subforms to provide context to failures:

```common-lisp
(is (= (1+ 5) 0))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(1+ 5) 0))
..   where
..     #1# = 6
```

`is` automatically captures values of arguments to functions like [`1+`][1eb3]
in the above example. Values of other interesting subforms can be
explicitly requested to be captured. `is` supports capturing multiple
values and can be taught how to deal with macros. The combination of
these features allows [`match-values`][162a] to be implementable as a tiny
extension:

```common-lisp
(is (match-values (values (1+ 5) "sdf")
      (= * 0)
      (string= * "sdf")))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
..        (= * 0)
..        (STRING= * "sdf")))
..   where
..     #1# == 6
..            #2#
```

`is` is flexible enough that all other checks ([`signals`][6d4e], [`signals-not`][7af9],
[`invokes-debugger`][12ce], [`invokes-debugger-not`][aaaa], [`fails`][e80e], and [`in-time`][f3af]) are
built on top of it.

<a id="x-28TRY-3AIS-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:IS%20MGL-PAX:MACRO"></a>

- [macro] **is** *form &key msg ctx (capture t) (print-captures t) (retry t)*

    If `form` returns `nil`, signal a [`result`][231f] [`failure`][f92d]. Else, signal a
    `result` [`success`][269a]. `is` returns normally if
    
    - the [`record-event`][ce49] restart is invoked (available when in a trial), or
    
    - the [`continue`][1867] restart is invoked (available when not in a trial), or
    
    - the condition signalled last (after [Outcome Restarts][7ef5]) is a [`pass`][21d9],
      and it is not [handle][59c3]d.
    
    If `is` returns normally after signalling an [`outcome`][2656], it returns `t` if
    the last condition signalled was a `success`, and `nil` otherwise.
    
    - `msg` and `ctx` are [Format Specifier Form][6cfa]s. `msg` is always
      evaluated (as a format specifier form), and it shall print a
      description of the check being made, stating what the desired
      outcome is. The default `msg` is the whole `is` form.
    
        `ctx` is only evaluated if `form` evaluates to `nil`. It shall provide
        contextual information about the failure.
    
        ```common-lisp
        (is (equal (prin1-to-string 'hello) "hello")
            :msg "Symbols are replacements for strings."
            :ctx ("*PACKAGE* is ~S and *PRINT-CASE* is ~S~%"
                  *package* *print-case*))
        .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
        ..   UNEXPECTED-FAILURE in check:
        ..     Symbols are replacements for strings.
        ..   where
        ..     (PRIN1-TO-STRING 'HELLO) = "HELLO"
        ..   *PACKAGE* is #<PACKAGE "TRY"> and *PRINT-CASE* is :UPCASE
        ..
        ```
    
    - If `capture` is true, the value(s) of some subforms of `form` may be
      automatically recorded in the condition and also made available
      for `ctx` via [`*is-captures*`][fb53]. See [Captures][3d27] for more.
    
    - If `print-captures` is true, the captures made are printed when the
      [`result`][231f] condition is displayed in the debugger or
      [`*describe*`][aa6d]d (see [Printing Events][b3f9]). This is the `where (PRIN1-TO-STRING
      'HELLO) ="HELLO"` part above. If `print-captures` is `nil`, the
      captures are still available in `*is-captures*` for writing custom
      `ctx` messages.
    
    - If `retry` is true, then the [`retry-check`][8cf6] restart evaluates `form`
      again and signals a new `result`. If `retry` is `nil`, then the
      `retry-check` restart returns `:retry`, which allows complex checks
      such as [`signals`][6d4e] to implement their own retry mechanism.

<a id="x-28TRY-3A-2AIS-FORM-2A-20VARIABLE-29"></a>
<a id="TRY:*IS-FORM*%20VARIABLE"></a>

- [variable] **\*is-form\***

    [`is`][80d6] binds this to its `form` argument for `ctx` and `msg`.

<a id="x-28TRY-3A-2AIS-CAPTURES-2A-20VARIABLE-29"></a>
<a id="TRY:*IS-CAPTURES*%20VARIABLE"></a>

- [variable] **\*is-captures\***

    During the evaluation of its `ctx` argument, `is` binds `*is-captures*`
    to the list of captures made. The list is ordered by the time of
    capture.

<a id="x-28TRY-3A-40FORMAT-SPECIFIER-FORM-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@FORMAT-SPECIFIER-FORM%20MGL-PAX:SECTION"></a>

### 5.1 Format Specifier Form

A format specifier form is a Lisp form, typically an argument to
a macro, standing for the `format-control` and `format-args` arguments
to the [`format`][ad78] function.

It may be a constant string:

```common-lisp
(is nil :msg "FORMAT-CONTROL~%with no args.")
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     FORMAT-CONTROL
..     with no args.
```

It may be a list whose first element is a constant string, and the
rest are the format arguments to be evaluated:

```common-lisp
(is nil :msg ("Implicit LIST ~A." "form"))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     Implicit LIST form.
```

Or it may be a form that evaluates to a list like `(format-control
&rest format-args)`:

```common-lisp
(is nil :msg (list "Full ~A." "form"))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     Full form.
```

Finally, it may evaluate to `nil`, in which case some context specific
default is implied.

<a id="x-28TRY-3ACANONICALIZE-FORMAT-SPECIFIER-FORM-20FUNCTION-29"></a>
<a id="TRY:CANONICALIZE-FORMAT-SPECIFIER-FORM%20FUNCTION"></a>

- [function] **canonicalize-format-specifier-form** *form*

    Ensure that the format specifier form `form` is in its full form.

<a id="x-28TRY-3A-40CAPTURES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CAPTURES%20MGL-PAX:SECTION"></a>

### 5.2 Captures

During the evaluation of the `form` argument of [`is`][80d6], evaluation of any
form (e.g. a subform of `form`) may be recorded, which are called
captures.

<a id="x-28TRY-3A-40AUTOMATIC-CAPTURES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@AUTOMATIC-CAPTURES%20MGL-PAX:SECTION"></a>

#### 5.2.1 Automatic Captures

[`is`][80d6] automatically captures some subforms of `form` that are likely
to be informative. In particular, if `form` is a function call, then
non-constant arguments are automatically captured:

```common-lisp
(is (= 3 (1+ 2) (- 4 3)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= 3 #1=(1+ 2) #2=(- 4 3)))
..   where
..     #1# = 3
..     #2# = 1
```

By default, automatic captures are not made for subforms deeper in
`form`, except for when `form` is a call to [`null`][25f5],
[`endp`][e8d7] and [`not`][1013]:

```common-lisp
(is (null (find (1+ 1) '(1 2 3))))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (NULL #1=(FIND #2=(1+ 1) '(1 2 3))))
..   where
..     #2# = 2
..     #1# = 2
```

```common-lisp
(is (endp (member (1+ 1) '(1 2 3))))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (ENDP #1=(MEMBER #2=(1+ 1) '(1 . #3=(2 3)))))
..   where
..     #2# = 2
..     #1# = #3#
```

Note that the argument of [`not`][1013] is not captured as it is
assumed to be `nil` or `t`. If that's not true, use [`null`][25f5].

```common-lisp
(is (not (equal (1+ 5) 6)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (NOT (EQUAL #1=(1+ 5) 6)))
..   where
..     #1# = 6
```

Other automatic captures are discussed with the relevant
functionality such as [`match-values`][162a].

<a id="x-28TRY-3A-40WRITING-AUTOMATIC-CAPTURE-RULES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@WRITING-AUTOMATIC-CAPTURE-RULES%20MGL-PAX:SECTION"></a>

##### Writing Automatic Capture Rules

<a id="x-28TRY-3ASUB-20STRUCTURE-29"></a>
<a id="TRY:SUB%20STRUCTURE"></a>

- [structure] **sub**

    A `sub` (short for substitution) says that in the original form [`is`][80d6] is
    checking, a `subform` was substituted (by `substitute-is-form`) with
    `var` (if `valuesp` is `nil`) or with ([`values-list`][dbd4] `var`) if `valuesp` is
    true. Conversely, `var` is to be bound to the evaluated `new-form` if
    `valuesp` is `nil`, and to ([`multiple-value-list`][4444] `new-form`) if `valuesp`.
    `new-form` is often [`eq`][5a82] to `subform`, but it may be different, which is
    the case when further substitutions are made within a substitution.

<a id="x-28TRY-3AMAKE-SUB-20FUNCTION-29"></a>
<a id="TRY:MAKE-SUB%20FUNCTION"></a>

- [function] **make-sub** *var subform new-form valuesp*

<a id="x-28TRY-3ASUB-VAR-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>
<a id="TRY:SUB-VAR%20%28MGL-PAX:STRUCTURE-ACCESSOR%20TRY:SUB%29"></a>

- [structure-accessor] **sub-var** *sub*

<a id="x-28TRY-3ASUB-SUBFORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>
<a id="TRY:SUB-SUBFORM%20%28MGL-PAX:STRUCTURE-ACCESSOR%20TRY:SUB%29"></a>

- [structure-accessor] **sub-subform** *sub*

<a id="x-28TRY-3ASUB-NEW-FORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>
<a id="TRY:SUB-NEW-FORM%20%28MGL-PAX:STRUCTURE-ACCESSOR%20TRY:SUB%29"></a>

- [structure-accessor] **sub-new-form** *sub*

<a id="x-28TRY-3ASUB-VALUESP-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>
<a id="TRY:SUB-VALUESP%20%28MGL-PAX:STRUCTURE-ACCESSOR%20TRY:SUB%29"></a>

- [structure-accessor] **sub-valuesp** *sub*

<a id="x-28TRY-3ASUBSTITUTE-IS-LIST-FORM-20GENERIC-FUNCTION-29"></a>
<a id="TRY:SUBSTITUTE-IS-LIST-FORM%20GENERIC-FUNCTION"></a>

- [generic-function] **substitute-is-list-form** *first form env*

    In the list `form`, whose [`car`][d5a2] is `first`, substitute
    subexpressions of interest with a [`gensym`][0e59] and return the new form. As
    the second value, return a list of [`sub`][47d5]s.
    
    For example, consider `(is (find (foo) list))`. When
    `substitute-is-list-form` is invoked on `(find (foo) list)`, it
    substitutes each argument of [`find`][4e46] with a variable, returning the new
    form `(find temp1 temp2)` and the list of two
    substitutions `((temp2 (foo) (foo) nil) (temp3 list list nil))`.
    This allows the original form to be rewritten as
    
    ```
    (let* ((temp1 (foo))
           (temp2 list))
      (find temp1 temp2))
    ```
    
    `temp1` and `temp2` may then be reported in the [`outcome`][2656] condition
    signalled by [`is`][80d6] like this:
    
        The following check failed:
          (is (find #1=(foo) #2=list))
        where
          #1# = <return-value-of-foo>
          #2# = <value-of-variable-list>

<a id="x-28TRY-3A-40EXPLICIT-CAPTURES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EXPLICIT-CAPTURES%20MGL-PAX:SECTION"></a>

#### 5.2.2 Explicit Captures

In addition to automatic captures, which are prescribed by
rewriting rules (see [Writing Automatic Capture Rules][0743]), explicit,
ad-hoc captures can also be made.

```common-lisp
(is (let ((x 1))
      (= (capture x) 2)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (LET ((X 1))
..        (= (CAPTURE X) 2)))
..   where
..     X = 1
```

If [`capture`][19f3] showing up in the form that [`is`][80d6] prints is undesirable,
then [`%`][790c] may be used instead:

```common-lisp
(is (let ((x 1))
      (= (% x) 2)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (LET ((X 1))
..        (= X 2)))
..   where
..     X = 1
```

Multiple values may be captured with [`capture-values`][351f] and its
secretive counterpart [`%%`][c1f6]:

```common-lisp
(is (= (%% (values 1 2)) 2))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(VALUES 1 2) 2))
..   where
..     #1# == 1
..            2
```

where printing `==` instead of = indicates that this
is a multiple value capture.

<a id="x-28TRY-3ACAPTURE-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:CAPTURE%20MGL-PAX:MACRO"></a>

- [macro] **capture** *form*

    Evaluate `form`, record its primary return value if within the
    dynamic extent of an [`is`][80d6] evaluation, and finally return that value.
    If `capture` is used within the lexical scope of `is`, then `capture`
    itself will show up in the form that the default `msg` prints. Thus it
    is recommended to use the equivalent [`macrolet`][1383] [`%`][790c] in the lexical
    scope as `%` is removed before printing.

<a id="x-28TRY-3ACAPTURE-VALUES-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:CAPTURE-VALUES%20MGL-PAX:MACRO"></a>

- [macro] **capture-values** *form*

    Like `capture-values`, but records and return all values returned by
    `form`. It is recommended to use the equivalent [`macrolet`][1383] [`%%`][c1f6] in the
    lexical scope as `%%` is removed before printing.

<a id="x-28TRY-3A-25-20MACROLET-29"></a>
<a id="TRY:%25%20MACROLET"></a>

- [macrolet] **%** *form*

    An alias for [`capture`][19f3] in the lexical scope of [`is`][80d6]. Removed from the
    `is` form when printed.

<a id="x-28TRY-3A-25-25-20MACROLET-29"></a>
<a id="TRY:%25%25%20MACROLET"></a>

- [macrolet] **%%** *form*

    An alias for [`capture-values`][351f] in the lexical scope of [`is`][80d6]. Removed
    from the `is` form when printed.

<a id="x-28TRY-3A-40CHECK-LIBRARY-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CHECK-LIBRARY%20MGL-PAX:SECTION"></a>

## 6 Check Library

In the following, various checks built on top of [`is`][80d6] are described.
Many of them share a number of arguments, which are described here.

- `on-return` is a boolean that determines whether the check in a
  macro that wraps `body` is made when `body` returns normally.

- `on-nlx` is a boolean that determines whether the check in a macro
  that wraps `body` is made when `body` performs a [non-local exit][b815].

- `msg` and `ctx` are [Format Specifier Form][6cfa]s as in `is`.

- `name` may be provided so that it is printed (with [`prin1`][6384]) instead of
  `body` in `msg`.


<a id="x-28TRY-3A-40CHECKING-CONDITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CHECKING-CONDITIONS%20MGL-PAX:SECTION"></a>

### 6.1 Checking Conditions

The macros [`signals`][6d4e], [`signals-not`][7af9], [`invokes-debugger`][12ce], and
[`invokes-debugger-not`][aaaa] all check whether a condition of a given type,
possibly also matching a predicate, was signalled. In addition to
those already described in [Check Library][f7f7], these macros share a
number of arguments.

Matching conditions are those that are of type `condition-type` (not
evaluated) and satisfy the predicate `pred`.

When `pred` is `nil`, it always matches. When it is a string, then it
matches if it is a substring of the printed representation of the
condition being handled (by [`princ`][676d] under [`with-standard-io-syntax`][39df]).
When it is a function, it matches if it returns true when called
with the condition as its argument.

The check is performed in the cleanup form of an [`unwind-protect`][c93f]
around `body`. If the [`current-trial`][e186] is performing an [`abort-trial`][4f9f],
[`skip-trial`][f45a] or [`retry-trial`][fae3], then [`result-skip`][7c3f] is signalled.

`handler` is called when a matching condition is found. It can be a
function, `t`, or `nil`. When it is a function, it is called from the
condition handler (`signals` and `signals-not`) or the debugger
hook (`invokes-debugger` and `invokes-debugger-not`) with the matching
condition. `handler` may perform a [non-local exit][b815]. When
`handler` is `t`, the matching condition is handled by performing a
non-local exit to just outside `body`. If the exit completes, `body` is
treated as if it had returned normally, and `on-return` is consulted.
When `handler` is `nil`, no additional action is performed when a
matching condition is found.

The default `ctx` describes the result of the matching process in
terms of [`*condition-matched-p*`][cf88] and [`*best-matching-condition*`][a07f].

<a id="x-28TRY-3A-2ACONDITION-MATCHED-P-2A-20VARIABLE-29"></a>
<a id="TRY:*CONDITION-MATCHED-P*%20VARIABLE"></a>

- [variable] **\*condition-matched-p\***

    When a check described in [Checking Conditions][ff2c] signals its
    [`outcome`][2656], this variable is bound to a boolean value to indicate
    whether a condition that matched `condition-type` and `pred` was
    found.

<a id="x-28TRY-3A-2ABEST-MATCHING-CONDITION-2A-20VARIABLE-29"></a>
<a id="TRY:*BEST-MATCHING-CONDITION*%20VARIABLE"></a>

- [variable] **\*best-matching-condition\***

    Bound when a check described in [Checking Conditions][ff2c]
    signals its [`outcome`][2656]. If [`*condition-matched-p*`][cf88], then it is the
    most recent condition that matched both `condition-type` and `pred`.
    Else, it is the most recent condition that matched
    `condition-type` or `nil` if no such conditions were detected.

<a id="x-28TRY-3ASIGNALS-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:SIGNALS%20MGL-PAX:MACRO"></a>

- [macro] **signals** *(condition-type &key pred (handler t) (on-return t) (on-nlx t) name msg ctx) &body body*

    Check that `body` signals a [`condition`][83e1] of `condition-type` (not
    evaluated) that matches `pred`. To detect matching conditions, `signals`
    sets up a [`handler-bind`][fd3c]. Thus it can only see what `body` does not
    handle. The arguments are described in [Checking Conditions][ff2c].
    
    ```common-lisp
    (signals (error)
      (error "xxx"))
    => NIL
    ```
    
    The following example shows a failure where `condition-type` matches
    but `pred` does not.
    
    ```common-lisp
    (signals (error :pred "non-matching")
      (error "xxx"))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (ERROR "xxx") signals a condition of type ERROR that matches
    ..     "non-matching".
    ..   The predicate did not match "xxx".
    ```

<a id="x-28TRY-3ASIGNALS-NOT-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:SIGNALS-NOT%20MGL-PAX:MACRO"></a>

- [macro] **signals-not** *(condition-type &key pred (handler t) (on-return t) (on-nlx t) name msg ctx) &body body*

    Check that `body` does not signal a [`condition`][83e1] of `condition-type` (not
    evaluated) that matches `pred`. To detect matching conditions,
    `signals-not` sets up a [`handler-bind`][fd3c]. Thus, it can only see what `body`
    does not handle. The arguments are described in
    [Checking Conditions][ff2c].

<a id="x-28TRY-3AINVOKES-DEBUGGER-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:INVOKES-DEBUGGER%20MGL-PAX:MACRO"></a>

- [macro] **invokes-debugger** *(condition-type &key pred (handler t) (on-return t) (on-nlx t) name msg ctx) &body body*

    Check that `body` enters the debugger with a [`condition`][83e1] of
    `condition-type` (not evaluated) that matches `pred`. To detect matching
    conditions, `invokes-debugger` sets up a [`*debugger-hook*`][1cdc]. Thus, if
    `*debugger-hook*` is changed by `body`, it may not detect the condition.
    The arguments are described in [Checking Conditions][ff2c].
    
    Note that in a trial (see [`current-trial`][e186]), all [`error`][669b]s are handled,
    and a `*debugger-hook*` is set up (see [`unhandled-error`][8f78]). Thus,
    invoking the debugger would normally cause the trial to abort.
    
    ```common-lisp
    (invokes-debugger (error :pred "xxx")
      (handler-bind ((error #'invoke-debugger))
        (error "xxx")))
    => NIL
    ```

<a id="x-28TRY-3AINVOKES-DEBUGGER-NOT-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:INVOKES-DEBUGGER-NOT%20MGL-PAX:MACRO"></a>

- [macro] **invokes-debugger-not** *(condition-type &key pred (handler t) (on-return t) (on-nlx t) name msg ctx) &body body*

    Check that `body` does not enter the debugger with a [`condition`][83e1] of
    `condition-type` (not evaluated) that matches `pred`. To detect matching
    conditions, `invokes-debugger-not` sets up a [`*debugger-hook*`][1cdc]. Thus, if
    `*debugger-hook*` is changed by `body`, it may not detect the condition.
    The arguments are described in [Checking Conditions][ff2c].

<a id="x-28TRY-3A-40MISC-CHECKS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@MISC-CHECKS%20MGL-PAX:SECTION"></a>

### 6.2 Miscellaneous Checks

<a id="x-28TRY-3AFAILS-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:FAILS%20MGL-PAX:MACRO"></a>

- [macro] **fails** *(&key name msg ctx) &body body*

    Check that `body` performs a [non-local exit][b815] but do not
    cancel it (see [cancelled non-local exit][7ab6]). See [Check Library][f7f7] for the
    descriptions of the other arguments.
    
    In the following example, `fails` signals a [`success`][269a].
    
    ```common-lisp
    (catch 'foo
      (fails ()
        (throw 'foo 7)))
    => 7
    ```
    
    Next, `fails` signals an [`unexpected-failure`][b5cb] because `body` returns
    normally.
    
    ```common-lisp
    (fails ()
      (print 'hey))
    ..
    .. HEY 
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (PRINT 'HEY) does not return normally.
    ```
    
    Note that there is no `fails-not` as [`with-test`][8f5d] fills that role.

<a id="x-28TRY-3AIN-TIME-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:IN-TIME%20MGL-PAX:MACRO"></a>

- [macro] **in-time** *(seconds &key (on-return t) (on-nlx t) name msg ctx) &body body*

    Check that `body` finishes in `seconds`. See [Check Library][f7f7] for
    the descriptions of the other arguments.
    
    ```
    (in-time (1)
      (sleep 2))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (SLEEP 2) finishes within 1s.
    ..   Took 2.000s.
    ```
    
    [`retry-check`][8cf6] restarts timing.

<a id="x-28TRY-3A-2AIN-TIME-ELAPSED-SECONDS-2A-20VARIABLE-29"></a>
<a id="TRY:*IN-TIME-ELAPSED-SECONDS*%20VARIABLE"></a>

- [variable] **\*in-time-elapsed-seconds\***

    Bound to the number of seconds passed during the evaluation of
    `body` when [`in-time`][f3af] signals its [`outcome`][2656].

<a id="x-28TRY-3A-40CHECK-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CHECK-UTILITIES%20MGL-PAX:SECTION"></a>

### 6.3 Check Utilities

These utilities are not checks (which signal [`outcome`][2656]s) but simple
functions and macros that may be useful for writing [`is`][80d6] checks.

<a id="x-28TRY-3AON-VALUES-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:ON-VALUES%20MGL-PAX:MACRO"></a>

- [macro] **on-values** *form &body body*

    `on-values` evaluates `form` and transforms its return values one by
    one based on forms in `body`. The Nth value is replaced by the return
    value of the Nth form of `body` evaluated with `*` bound
    to the Nth value. If the number of values exceeds the number of
    transformation forms in `body` then the excess values are returned as
    is.
    
    ```common-lisp
    (on-values (values 1 "abc" 7)
      (1+ *)
      (length *))
    => 2
    => 3
    => 7
    ```
    
    If the number of values is less than the number of transformation
    forms, then in later transformation forms `*` is bound
    to `nil`.
    
    ```common-lisp
    (on-values (values)
      *
      *)
    => NIL
    => NIL
    ```
    
    The first forms in `body` may be options. Options must precede
    transformation forms. With `:truncate` `t`, the excess values are
    discarded.
    
    ```common-lisp
    (on-values (values 1 "abc" 7)
      (:truncate t)
      (1+ *)
      (length *))
    => 2
    => 3
    ```
    
    The `:on-length-mismatch` option may be `nil` or a function of a single
    argument. If the number of values and the number of transformation
    forms are different, then this function is called to transform the
    list of values. `:truncate` is handled before `:on-length-mismatch`.
    
    ```common-lisp
    (on-values 1
      (:on-length-mismatch (lambda (values)
                             (if (= (length values) 1)
                                 (append values '("abc"))
                                 values)))
      (1+ *)
      *)
    => 2
    => "abc"
    ```
    
    If the same option is specified multiple times, the first one is in
    effect.

<a id="x-28TRY-3AMATCH-VALUES-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:MATCH-VALUES%20MGL-PAX:MACRO"></a>

- [macro] **match-values** *form &body body*

    `match-values` returns true iff all return values of `form` satisfy
    the predicates given by `body`, which are described in [`on-values`][eb5c]. The
    `:on-length-mismatch` and `:truncate` options of `on-values` are
    supported. If no `:on-length-mismatch` option is specified, then
    `match-values` returns `nil` on length mismatch.
    
    ```common-lisp
    ;; no values
    (is (match-values (values)))
    ;; single value success
    (is (match-values 1
          (= * 1)))
    ;; success with different types
    (is (match-values (values 1 "sdf")
          (= * 1)
          (string= * "sdf")))
    ;; too few values
    (is (not (match-values 1
               (= * 1)
               (string= * "sdf"))))
    ;; too many values
    (is (not (match-values (values 1 "sdf" 3)
               (= * 1)
               (string= * "sdf"))))
    ;; too many values, but truncated
    (is (match-values (values 1 "sdf" 3)
          (:truncate t)
          (= * 1)
          (string= * "sdf")))
    ```

<a id="x-28TRY-3AMISMATCH-25-20FUNCTION-29"></a>
<a id="TRY:MISMATCH%25%20FUNCTION"></a>

- [function] **mismatch%** *sequence1 sequence2 &key from-end (test \#'eql) (start1 0) end1 (start2 0) end2 key max-prefix-length max-suffix-length*

    Like [`cl:mismatch`][b94a] but [`capture`][19f3]s and returns the common prefix and
    the mismatched suffixes. The `test-not` argument is deprecated by
    the `clhs` and is not supported. In addition, if `max-prefix-length` and
    `max-suffix-length` are non-`nil`, they must be non-negative integers,
    and they limit the number of elements in the prefix and the
    suffixes.
    
    ```common-lisp
    (is (null (mismatch% '(1 2 3) '(1 2 4 5))))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (NULL #1=(MISMATCH% '(1 2 3) '(1 2 4 5))))
    ..   where
    ..     COMMON-PREFIX = (1 2)
    ..     MISMATCHED-SUFFIX-1 = (3)
    ..     MISMATCHED-SUFFIX-2 = (4 5)
    ..     #1# = 2
    ```
    
    ```common-lisp
    (is (null (mismatch% "Hello, World!"
                         "Hello, world!")))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (NULL #1=(MISMATCH% "Hello, World!" "Hello, world!")))
    ..   where
    ..     COMMON-PREFIX = "Hello, "
    ..     MISMATCHED-SUFFIX-1 = "World!"
    ..     MISMATCHED-SUFFIX-2 = "world!"
    ..     #1# = 7
    ```

<a id="x-28TRY-3ADIFFERENT-ELEMENTS-20FUNCTION-29"></a>
<a id="TRY:DIFFERENT-ELEMENTS%20FUNCTION"></a>

- [function] **different-elements** *sequence1 sequence2 &key (pred \#'eql) (missing :missing)*

    Return the different elements under `pred` in the given sequences as
    a list of `(:index <index> <e1> <e2>)` elements, where `e1` and `e2`
    are elements of `sequence1` and `sequence2` at `<index>`, respectively,
    and they may be `missing` if the corresponding sequence is too short.
    
    ```common-lisp
    (is (endp (different-elements '(1 2 3) '(1 b 3 d))))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (ENDP #1=(DIFFERENT-ELEMENTS '(1 2 3) '(1 B 3 D))))
    ..   where
    ..     #1# = ((:INDEX 1 2 B) (:INDEX 3 :MISSING D))
    ```

<a id="x-28TRY-3ASAME-SET-P-20FUNCTION-29"></a>
<a id="TRY:SAME-SET-P%20FUNCTION"></a>

- [function] **same-set-p** *list1 list2 &key key (test \#'eql)*

    See if `list1` and `list2` represent the same set.
    See [`cl:set-difference`][f8bf] for a description of the `key` and `test` arguments.
    
    ```common-lisp
    (try:is (try:same-set-p '(1) '(2)))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (SAME-SET-P '(1) '(2)))
    ..   where
    ..     ONLY-IN-1 = (1)
    ..     ONLY-IN-2 = (2)
    ```

<a id="x-28TRY-3AWITH-SHUFFLING-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-SHUFFLING%20MGL-PAX:MACRO"></a>

- [macro] **with-shuffling** *nil &body body*

    Execute the forms that make up the list of forms `body` in random
    order and return `nil`. This may be useful to prevent writing tests
    that accidentally depend on the order in which subtests are called.
    
    ```common-lisp
    (loop repeat 3 do
      (with-shuffling ()
        (prin1 1)
        (prin1 2)))
    .. 122112
    => NIL
    ```

<a id="x-28TRY-3A-40COMPARING-FLOATS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@COMPARING-FLOATS%20MGL-PAX:SECTION"></a>

#### 6.3.1 Comparing Floats

Float comparisons following
[https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/](https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/).

<a id="x-28TRY-3AFLOAT--7E-3D-20FUNCTION-29"></a>
<a id="TRY:FLOAT-~%3D%20FUNCTION"></a>

- [function] **float-~=** *x y &key (max-diff-in-value \*max-diff-in-value\*) (max-diff-in-ulp \*max-diff-in-ulp\*)*

    Return whether two numbers, `x` and `y`, are approximately equal either
    according to `max-diff-in-value` or `max-diff-in-ulp`.
    
    If the absolute value of the difference of two floats is not greater
    than `max-diff-in-value`, then they are considered equal.
    
    If two floats are of the same sign and the number of representable
    floats (ULP, unit in the last place) between them is less than
    `max-diff-in-ulp`, then they are considered equal.
    
    If neither `x` nor `y` is a float, then the comparison is done with [`=`][e52f].
    If one of them is a [`double-float`][0d57], then the other is converted to a
    double float, and the comparison takes place in double-float space.
    Else, both are converted to [`single-float`][31a6] and the comparison takes
    place in single-float space.

<a id="x-28TRY-3A-2AMAX-DIFF-IN-VALUE-2A-20VARIABLE-29"></a>
<a id="TRY:*MAX-DIFF-IN-VALUE*%20VARIABLE"></a>

- [variable] **\*max-diff-in-value\*** *1.0e-16*

    The default value of the `max-diff-in-value` argument of [`float-~=`][7955].

<a id="x-28TRY-3A-2AMAX-DIFF-IN-ULP-2A-20VARIABLE-29"></a>
<a id="TRY:*MAX-DIFF-IN-ULP*%20VARIABLE"></a>

- [variable] **\*max-diff-in-ulp\*** *2*

    The default value of the `max-diff-in-ulp` argument of [`float-~=`][7955].

<a id="x-28TRY-3AFLOAT--7E-3C-20FUNCTION-29"></a>
<a id="TRY:FLOAT-~%3C%20FUNCTION"></a>

- [function] **float-~\<** *x y &key (max-diff-in-value \*max-diff-in-value\*) (max-diff-in-ulp \*max-diff-in-ulp\*)*

    Return whether `x` is approximately less than `y`. Equivalent to [`<`][c3a0],
    but it also allows for approximate equality according to [`float-~=`][7955].

<a id="x-28TRY-3AFLOAT--7E-3E-20FUNCTION-29"></a>
<a id="TRY:FLOAT-~%3E%20FUNCTION"></a>

- [function] **float-~>** *x y &key (max-diff-in-value \*max-diff-in-value\*) (max-diff-in-ulp \*max-diff-in-ulp\*)*

    Return whether `x` is approximately greater than `y`. Equivalent to [`>`][5333],
    but it also allows for approximate equality according to [`float-~=`][7955].

<a id="x-28TRY-3A-40TESTS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TESTS%20MGL-PAX:SECTION"></a>

## 7 Tests

In Try, tests are Lisp functions that record their execution in
[`trial`][99d0] objects. `trial`s are to tests what function call traces are to
functions. In more detail, tests

- create a `trial` object and signal a [`trial-start`][b664] event upon entry
  to the function,

- signal a [`verdict`][52e1] condition before returning normally or via a
  [non-local exit][b815],

- return the `trial` object as the first value,

- return explicitly returned values as the second, third, and so on
  values.

See [`deftest`][e7ca] and [`with-test`][8f5d] for more precise descriptions.

<a id="x-28TRY-3ADEFTEST-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:DEFTEST%20MGL-PAX:MACRO"></a>

- [macro] **deftest** *name lambda-list &body body*

    `deftest` is a wrapper around [`defun`][f472] to define global test functions.
    See `defun` for a description of `name`, `lambda-list`, and `body`. The
    behaviour common with [`with-test`][8f5d] is described in [Tests][dc28].
    
    ```common-lisp
    (deftest my-test ()
      (write-string "hey"))
    => MY-TEST
    
    (test-bound-p 'my-test)
    => T
    
    (my-test)
    .. hey
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    ```
    
    Although the common case is for tests to have no arguments, `deftest`
    supports general function lambda lists. Within a global test,
    
    - `name` is bound to the [`trial`][99d0] object
    
    - the first return value is the trial
    
    - values are not returned implicitly
    
    - values returned with an explicit [`return-from`][3eef7] are returned as
      values after the trial
    
    ```common-lisp
    (deftest my-test ()
      (prin1 my-test)
      (return-from my-test (values 2 3)))
    
    (my-test)
    .. #<TRIAL (MY-TEST) RUNNING>
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    => 2
    => 3
    ```

<a id="x-28TRY-3A-2ARUN-DEFTEST-WHEN-2A-20VARIABLE-29"></a>
<a id="TRY:*RUN-DEFTEST-WHEN*%20VARIABLE"></a>

- [variable] **\*run-deftest-when\*** *nil*

    This may be any of `:compile-toplevel`, `:load-toplevel`, `:execute`, or
    a list thereof. The value of `*run-deftest-when*` determines in what
    [`eval-when`][9c9c] situation to call the test function immediately after it
    has been defined with [`deftest`][e7ca].
    
    For interactive development, it may be convenient to set it to
    `:execute` and have the test run when the `deftest` is evaluated (maybe
    with Slime `C-M-x`, `slime-eval-defun`). Or set it to
    `:compile-toplevel`, and have it rerun on Slime `C-c C-c`,
    `slime-compile-defun`.
    
    If the test has required arguments, an argument list is prompted for
    and read from [`*query-io*`][f4bf].

<a id="x-28TRY-3ATEST-BOUND-P-20FUNCTION-29"></a>
<a id="TRY:TEST-BOUND-P%20FUNCTION"></a>

- [function] **test-bound-p** *symbol*

    See if `symbol` names a global test (i.e. a test defined with
    [`deftest`][e7ca]). If since the execution of `deftest`, the symbol has been
    [`unintern`][cdba]ed, [`fmakunbound`][609c]ed, or redefined with [`defun`][f472], then it no
    longer names a global test.

<a id="x-28TRY-3AWITH-TEST-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-TEST%20MGL-PAX:MACRO"></a>

- [macro] **with-test** *(&optional var-or-name &key name) &body body*

    Execute `body` in a [`trial`][99d0] to group together `check`s and
    other tests in its dynamic scope. `body` is executed in its lexical
    environment even on a rerun (see [Rerunning Trials][e4ac]).
    
    If `var-or-name` is a non-`nil` symbol, it is bound to the `trial`
    object. `name` may be of any type, it is purely for presentation
    purposes. If `name` is not specified, then it defaults to `var-or-name`.
    
    To facilitate returning values, a [`block`][d2d8] is wrapped around `body`. The
    name of the block is `var-or-name` if it is a symbol, else it's `nil`.
    
    Both `var-or-name` and `name` can be specified, but in this case `var-or-name`
    must be a symbol:
    
    ```common-lisp
    (with-test (some-feature :name "obscure feature")
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST ("obscure feature")) RUNNING>
    .. "obscure feature"
    ..   ⋅ (IS T)
    .. ⋅ "obscure feature" ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("obscure feature")) EXPECTED-SUCCESS 0.200s ⋅1>
    => 1
    => 2
    ```
    
    If only `var-or-name` is specified:
    
    ```common-lisp
    (with-test (some-feature)
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST (SOME-FEATURE)) RUNNING>
    .. SOME-FEATURE
    ..   ⋅ (IS T)
    .. ⋅ SOME-FEATURE ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (SOME-FEATURE)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    If neither is specified:
    
    ```common-lisp
    (with-test ()
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST (NIL)) RUNNING>
    .. NIL
    ..   ⋅ (IS T)
    .. ⋅ NIL ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (NIL)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    Finally, using that `name` defaults to `var-or-name` and that it is
    valid to specify non-symbols for `var-or-name`, one can also write:
    
    ```common-lisp
    (with-test ("Some feature")
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST ("Some feature")) RUNNING>
    .. "Some feature"
    ..   ⋅ (IS T)
    .. ⋅ "Some feature" ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("Some feature")) EXPECTED-SUCCESS 0.200s ⋅1>
    => 1
    => 2
    ```
    
    In summary and in contrast to [`deftest`][e7ca], `with-test`
    
    - defines and runs a test at the same time,
    
    - the test function cannot have arguments,
    
    - may not bind their trial object to any variable,
    
    - may have a `block` named `nil`,
    
    - has a `name` purely for presentation purposes.
    
    `with-test` can be thought of as analogous to `(funcall (lambda ()
    body))`. The presence of the [`lambda`][650d] is important because it is
    stored in the `trial` object to support [Rerunning Trials][e4ac].

<a id="x-28TRY-3ALIST-PACKAGE-TESTS-20FUNCTION-29"></a>
<a id="TRY:LIST-PACKAGE-TESTS%20FUNCTION"></a>

- [function] **list-package-tests** *&optional (package \*package\*)*

    List all symbols in `package` that name global tests in the sense of
    [`test-bound-p`][5065].

<a id="x-28TRY-3AWITH-TESTS-RUN-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WITH-TESTS-RUN%20MGL-PAX:MACRO"></a>

- [macro] **with-tests-run** *(tests-run) &body body*

    Bind the symbol `tests-run` to an empty [`eq`][5a82] hash table and execute
    `body`. The hash table reflects call counts to global tests. Keys are
    symbols naming global tests, and the values are the number of times
    the keys have been called.

<a id="x-28TRY-3AWARN-ON-TESTS-NOT-RUN-20MGL-PAX-3AMACRO-29"></a>
<a id="TRY:WARN-ON-TESTS-NOT-RUN%20MGL-PAX:MACRO"></a>

- [macro] **warn-on-tests-not-run** *(&optional (package \*package\*)) &body body*

    A convenience utility that records the global tests run by `body`
    with [`with-tests-run`][6910] and, when `body` finishes, signals a warning for
    each global tests in `package` not run.
    
    This is how Try runs its own tests:
    
    ```
    (defun test ()
      ;; Bind *PACKAGE* so that names of tests printed have package names,
      ;; and M-. works on them in Slime.
      (let ((*package* (find-package :common-lisp)))
        (warn-on-tests-not-run ((find-package :try))
          (print (try 'test-all
                      :print 'unexpected
                      :describe 'unexpected)))))
    ```

<a id="x-28TRY-3A-40CALLING-TEST-FUNCTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@CALLING-TEST-FUNCTIONS%20MGL-PAX:SECTION"></a>

### 7.1 Calling Test Functions

Tests always run under [`try`][b602], but `try` may be explicit or implicit
depending whether the outermost test was called via `try` or directly
as a Lisp function.

Nested invocations of tests, be them explicit or implicit, do not
establish nested `try`s. [`event`][955d] handling is performed only at the
outermost level.

<a id="x-28TRY-3A-40TRYVAR-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="TRY:@TRYVAR%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **Try var**

    There are lots of special variables that affect [`try`][b602]. To avoid the
    plight of Common Lisp [`stream`][d5a9]s and guarantee consistent output and
    behaviour even if these variables are changed during a single `try`
    run, the values of variables are captured when `try` is first invoked.
    That is, when the outermost test function is entered. These
    variables are called Try vars.

<a id="x-28TRY-3A-40EXPLICIT-TRY-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@EXPLICIT-TRY%20MGL-PAX:SECTION"></a>

#### 7.1.1 Explicit `try`

We speak of an explicit [`try`][b602] when the outermost test function is
called directly by `try`.

Global test functions can be `try`ed explicitly by giving their name:

```common-lisp
(deftest my-test ()
  (is t))

(try 'my-test)
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s ⋅1>
```

However, [`with-test`][8f5d] has no global name, so to delay its execution
until `try` calls it, it needs to be wrapped in a [`lambda`][650d].

```
(try (lambda ()
       (with-test (my-test)
         (is t))))
.. (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>)
..   MY-TEST
..     ⋅ (IS T)
..   ⋅ MY-TEST ⋅1
.. ⋅ (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) ⋅1
..
==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) EXPECTED-SUCCESS 0.000s ⋅1>
```

In the example above, the [`testable`][cdc3] argument is not the
name of a test, so we see that `try` wraps an extra [`trial`][99d0]
around the lambda to ensure that the tree of `trial`s has a single
root. This `trial` object also remembers the lambda function for
[rerunning][e4ac]. This situation also arises when there are
multiple basic [Testables][cdc3].

Explicit and implicit `try`s are very similar. The differences are
that explicit `try`

- can run [Testables][cdc3] (it takes care of wrapping them in a function),

- has a function argument for each of the [`*debug*`][856d], [`*collect*`][307c], etc
  variables.

Those arguments default to [`*try-debug*`][18ff], [`*try-collect*`][0c39], etc, which
parallel and in turn default to `*debug*`, `*collect*`, etc when set to
`:unspecified`, their default value. The exception is `*try-debug*`,
which defaults to `nil`. These defaults encourage the use of an
explicit `try` call in the non-interactive case and calling the test
functions directly in the interactive one, but this is not enforced
in any way.

<a id="x-28TRY-3ATRY-20FUNCTION-29"></a>
<a id="TRY:TRY%20FUNCTION"></a>

- [function] **try** *testable &key (debug \*try-debug\*) (count \*try-count\*) (collect \*try-collect\*) (rerun \*try-rerun\*) (print \*try-print\*) (describe \*try-describe\*) (stream \*try-stream\*) (printer \*try-printer\*)*

    `try` runs [`testable`][cdc3] and handles the [`event`][955d]s to
    [count][886e], [collect][52e5], debug, [print][b3f9] the
    results of checks and trials, and to decide what tests to [`skip`][69a2] and
    what to [rerun][e4ac].
    
    `debug`, `count`, `collect`, `rerun`, `print`, and `describe` must all be valid
    specifiers for types that are either `nil` (the empty type) or have a
    non-empty intersection with the type [`event`][955d] (e.g. `t`, [`outcome`][2656],
    [`unexpected`][d6ad], [`verdict`][52e1]).
    
    `try` sets up a [`handler-bind`][fd3c] handler for `event`s and runs `testable` (see
    [Testables][cdc3]). When an `event` is signalled, the handler matches its
    type to the value of the `debug` argument (in the sense of `(typep
    event debug)`). If it matches, then the debugger is invoked with the
    event. In the debugger, the user has a number of restarts available
    to change (see [Event Restarts][d4ce], [Outcome Restarts][7ef5], [Check Restarts][2364],
    [Trial Restarts][5355], and [`set-try-debug`][f001]).
    
    If the debugger is not invoked, `try` invokes the very first restart
    available, which is always [`record-event`][ce49].
    
    Recording the event is performed as follows.
    
    - Outcome counts are updated (see [Counting Events][886e]).
    
    - The event is passed to the collector (see [Collecting Events][52e5]).
    
    - The event is passed to the printer (see [Printing Events][b3f9]).
    
    - Finally, when rerunning a trial (i.e. when `testable` is a trial),
      on a [`trial-start`][b664] event, the trial may be skipped (see [Rerunning Trials][e4ac]).
    
    `try` returns the values returned by the outermost trial. This is just
    the [`trial`][99d0] object in the absence of an explicit [`return`][5b0b] or
    [`return-from`][3eef7] (see examples in [Tests][dc28]).
    
    If `try` is called within the dynamic extent of another `try` run, then
    it simply calls `testable`, ignores the other arguments and leaves
    event handling to the enclosing `try`.

<a id="x-28TRY-3ASET-TRY-DEBUG-20FUNCTION-29"></a>
<a id="TRY:SET-TRY-DEBUG%20FUNCTION"></a>

- [function] **set-try-debug** *debug*

    Invoke the `set-try-debug` restart to override the `debug` argument of
    the currently running [`try`][b602]. `debug` must thus be a suitable type. When
    the `set-try-debug` restart is invoked interactively, `debug` is read as
    a non-evaluated form from [`*query-io*`][f4bf].

<a id="x-28TRY-3A-2ATRY-DEBUG-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-DEBUG*%20VARIABLE"></a>

- [variable] **\*try-debug\*** *nil*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:debug` argument.
    If `:unspecified`, then the value of [`*debug*`][856d] is used instead.

<a id="x-28TRY-3A-2ATRY-COUNT-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-COUNT*%20VARIABLE"></a>

- [variable] **\*try-count\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:count` argument.
    If `:unspecified`, then the value of [`*count*`][3bb4] is used instead.

<a id="x-28TRY-3A-2ATRY-COLLECT-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-COLLECT*%20VARIABLE"></a>

- [variable] **\*try-collect\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:collect` argument.
    If `:unspecified`, then the value of [`*collect*`][307c] is used instead.

<a id="x-28TRY-3A-2ATRY-RERUN-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-RERUN*%20VARIABLE"></a>

- [variable] **\*try-rerun\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:rerun` argument.
    If `:unspecified`, then the value of [`*rerun*`][63db] is used instead.

<a id="x-28TRY-3A-2ATRY-PRINT-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-PRINT*%20VARIABLE"></a>

- [variable] **\*try-print\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:print` argument.
    If `:unspecified`, then the value of [`*print*`][7ee9] is used instead.

<a id="x-28TRY-3A-2ATRY-DESCRIBE-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-DESCRIBE*%20VARIABLE"></a>

- [variable] **\*try-describe\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:describe` argument.
    If `:unspecified`, then the value of [`*describe*`][aa6d] is used instead.

<a id="x-28TRY-3A-2ATRY-STREAM-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-STREAM*%20VARIABLE"></a>

- [variable] **\*try-stream\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:stream` argument.
    If `:unspecified`, then the value of [`*stream*`][0126] is used instead.

<a id="x-28TRY-3A-2ATRY-PRINTER-2A-20VARIABLE-29"></a>
<a id="TRY:*TRY-PRINTER*%20VARIABLE"></a>

- [variable] **\*try-printer\*** *:unspecified*

    [Try var][0d7a]. The default value for [`try`][b602]'s `:printer` argument.
    If `:unspecified`, then the value of [`*printer*`][7230] is used instead.

<a id="x-28TRY-3A-2AN-RECENT-TRIALS-2A-20VARIABLE-29"></a>
<a id="TRY:*N-RECENT-TRIALS*%20VARIABLE"></a>

- [variable] **\*n-recent-trials\*** *3*

    See `*recent-trials*`.

<a id="x-28TRY-3ARECENT-TRIAL-20FUNCTION-29"></a>
<a id="TRY:RECENT-TRIAL%20FUNCTION"></a>

- [function] **recent-trial** *&optional (n 0)*

    Returns the `n`th most recent trial or `nil` if there are not enough
    trials recorded. Every [`trial`][99d0] returned by [`try`][b602] gets pushed
    onto a list of trials, but only [`*n-recent-trials*`][8f9f] are kept.

<a id="x-28TRY-3A-21-20VARIABLE-29"></a>
<a id="TRY:%21%20VARIABLE"></a>

- [variable] **!** *nil*

    The most recent trial. Equivalent to `(recent-trial 0)`.

<a id="x-28TRY-3A-21-21-20VARIABLE-29"></a>
<a id="TRY:%21%21%20VARIABLE"></a>

- [variable] **!!** *nil*

    Equivalent to `(recent-trial 1)`.

<a id="x-28TRY-3A-21-21-21-20VARIABLE-29"></a>
<a id="TRY:%21%21%21%20VARIABLE"></a>

- [variable] **!!!** *nil*

    Equivalent to `(recent-trial 2)`.

<a id="x-28TRY-3A-40TESTABLES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@TESTABLES%20MGL-PAX:SECTION"></a>

##### Testables

Valid first arguments to [`try`][b602] are called testables. A basic testable
is a [function designator][8aea], which can be

- the name of a global function (as in [`defun`][f472] or [`deftest`][e7ca]), or

- a function object (including [`trial`][99d0]s, which are funcallable).

Composite testables are turned into a list of function designators
in a recursive manner.

- When the testable is a list, this is trivial.

- When the testable is a [`package`][1d5a], [`list-package-tests`][b426] is called on
  it.

With a list of function designators, `try` does the following:

- If there is only one and it is [`test-bound-p`][5065], then the test
  function is called directly.

- Else, `try` behaves as if its `testable` argument were an anonymous
  function that calls the function designators one by one. See
  [Explicit `try`][1720] for an example.


<a id="x-28TRY-3A-40IMPLICIT-TRY-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@IMPLICIT-TRY%20MGL-PAX:SECTION"></a>

#### 7.1.2 Implicit `try`

We speak of an implicit [`try`][b602] when the outermost test is entered
via a Lisp function call. In this case, as the test function is
entered, it invokes itself behind the scenes (implicitly) via `try`:

```
(try ... :debug *debug* :collect *collect* :rerun *rerun*
     :print *print* :describe *describe*
     :stream *stream* :printer *printer*)
```

As it's invoked again, it sees that it is now running under `try` and
proceeds to execute normally.

An implicit `try` can only happen with the following two constructs.

- Global test function

    ```common-lisp
    (deftest my-test ()
      (is t))
    
    (my-test)
    .. MY-TEST
    ..   ⋅ (IS T)
    .. ⋅ MY-TEST ⋅1
    ..
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.004s ⋅1>
    ```

- [`with-test`][8f5d]

    ```common-lisp
    (with-test (my-test)
      (is t))
    .. MY-TEST
    ..   ⋅ (IS T)
    .. ⋅ MY-TEST ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (MY-TEST)) EXPECTED-SUCCESS 0.000s ⋅1>
    ```


<a id="x-28TRY-3A-2ADEBUG-2A-20VARIABLE-29"></a>
<a id="TRY:*DEBUG*%20VARIABLE"></a>

- [variable] **\*debug\*** *(and unexpected (not nlx) (not verdict))*

    [Try var][0d7a]. The default value makes [`try`][b602] invoke the debugger on [`unhandled-error`][8f78],
    [`result-abort*`][ffab], [`unexpected-result-failure`][daeb], and
    [`unexpected-result-success`][b72c]. [`nlx`][b115] is excluded because it is caught as
    the test function is being exited, but by that time the dynamic
    environment of the actual cause is likely gone. [`verdict`][52e1] is excluded
    because it is a consequence of its child outcomes.

<a id="x-28TRY-3A-2ACOUNT-2A-20VARIABLE-29"></a>
<a id="TRY:*COUNT*%20VARIABLE"></a>

- [variable] **\*count\*** *leaf*

    [Try var][0d7a]. Although the default value of [`*categories*`][e949] lumps [`result`][231f]s and
    [`verdict`][52e1]s together, with the default of [`leaf`][f58d], `verdict`s are not
    counted. See [Counting Events][886e].

<a id="x-28TRY-3A-2ACOLLECT-2A-20VARIABLE-29"></a>
<a id="TRY:*COLLECT*%20VARIABLE"></a>

- [variable] **\*collect\*** *(or trial-event unexpected)*

    [Try var][0d7a]. By default all [`trial`s][99d0] and [`unexpected`][d6ad] are
    [collected][52e5]. This is sufficient for being able to [Rerunning Trials][e4ac]
    anything in [context][38e8].

<a id="x-28TRY-3A-2ARERUN-2A-20VARIABLE-29"></a>
<a id="TRY:*RERUN*%20VARIABLE"></a>

- [variable] **\*rerun\*** *unexpected*

    [Try var][0d7a]. The default matches that of [`*collect*`][307c]. See [Rerunning Trials][e4ac].

<a id="x-28TRY-3A-2APRINT-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT*%20VARIABLE"></a>

- [variable] **\*print\*** *(or leaf dismissal)*

    [Try var][0d7a]. Events of this type are [printed][b3f9].
    
    ```common-lisp
    (concrete-events-of-type '(or leaf dismissal))
    => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
        EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE RESULT-SKIP
        RESULT-ABORT* VERDICT-SKIP VERDICT-ABORT* UNHANDLED-ERROR NLX)
    ```

<a id="x-28TRY-3A-2ADESCRIBE-2A-20VARIABLE-29"></a>
<a id="TRY:*DESCRIBE*%20VARIABLE"></a>

- [variable] **\*describe\*** *(or unexpected failure)*

    [Try var][0d7a]. By default, the context (e.g. [Captures][3d27], and the `ctx` argument of
    is and other checks) of [`unexpected`][d6ad] events is described. See [Printing Events][b3f9].

<a id="x-28TRY-3A-2ASTREAM-2A-20VARIABLE-29"></a>
<a id="TRY:*STREAM*%20VARIABLE"></a>

- [variable] **\*stream\*** *(make-synonym-stream '\*debug-io\*)*

    [Try var][0d7a].

<a id="x-28TRY-3A-2APRINTER-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINTER*%20VARIABLE"></a>

- [variable] **\*printer\*** *tree-printer*

    [Try var][0d7a].

<a id="x-28TRY-3A-40IMPLICIT-TRY-IMPLEMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@IMPLICIT-TRY-IMPLEMENTATION%20MGL-PAX:SECTION"></a>

##### Implementation of Implicit `try`

What's happening in the implementation is that a test function,
when it is called, checks whether it is running under the [`try`][b602]
function. If it isn't, then it invokes `try` with its [`trial`][99d0]. `try`
realizes the trial cannot be rerun yet (see [Rerunning Trials][e4ac]) because it
is [`runningp`][5d4a], sets up its event handlers for debugging, collecting,
printing, and invokes the trial as if it were rerun but without
skipping anything based on the `rerun` argument. Thus the following
are infinite recursions:

```
(with-test (recurse)
  (try recurse))

(with-test (recurse)
  (funcall recurse))
```


<a id="x-28TRY-3A-40PRINT-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@PRINT%20MGL-PAX:SECTION"></a>

### 7.2 Printing Events

[`try`][b602] instantiates a printer of the type given by its `printer`
argument. All [`event`][955d]s recorded by `try` are sent to this printer. The
printer then prints events that match the type given by the `print`
argument of `try`. Events that also match the `describe` argument of `try`
are printed with context information (see [`is`][80d6]) and backtraces (see
[`unhandled-error`][8f78]).

Although the printing is primarily customized with global special
variables, changing the value of those variables after the printer
object is instantiated by `try` has no effect. This is to ensure
consistent output with nested `try` calls of differing printer
setups.

<a id="x-28TRY-3ATREE-PRINTER-20CLASS-29"></a>
<a id="TRY:TREE-PRINTER%20CLASS"></a>

- [class] **tree-printer**

    `tree-printer` prints events in an indented
    tree-like structure, with each internal node corresponding to a
    [`trial`][99d0]. This is the default printer (according to [`*printer*`][7230] and
    [`*try-printer*`][c864]) and currently the only one.
    
    The following example prints all [Concrete Events][279a].
    
    ```common-lisp
    (let ((*debug* nil)
          (*print* '(not trial-start))
          (*describe* nil))
      (with-test (verdict-abort*)
        (with-test (expected-verdict-success))
        (with-expected-outcome ('failure)
          (with-test (unexpected-verdict-success)))
        (handler-bind (((and verdict success) #'force-expected-failure))
          (with-test (expected-verdict-failure)))
        (handler-bind (((and verdict success) #'force-unexpected-failure))
          (with-test (unexpected-verdict-failure)))
        (with-test (verdict-skip)
          (skip-trial))
        (is t :msg "EXPECTED-RESULT-SUCCESS")
        (with-failure-expected ('failure)
          (is t :msg "UNEXPECTED-RESULT-SUCCESS")
          (is nil :msg "EXPECTED-RESULT-FAILURE"))
        (is nil :msg "UNEXPECTED-RESULT-FAILURE")
        (with-skip ()
          (is nil :msg "RESULT-SKIP"))
        (handler-bind (((and result success) #'abort-check))
          (is t :msg "RESULT-ABORT*"))
        (catch 'foo
          (with-test (nlx-test)
            (throw 'foo nil)))
        (error "UNHANDLED-ERROR")))
    .. VERDICT-ABORT*                       ; TRIAL-START
    ..   ⋅ EXPECTED-VERDICT-SUCCESS
    ..   ⊡ UNEXPECTED-VERDICT-SUCCESS
    ..   × EXPECTED-VERDICT-FAILURE
    ..   ⊠ UNEXPECTED-VERDICT-FAILURE
    ..   - VERDICT-SKIP
    ..   ⋅ EXPECTED-RESULT-SUCCESS
    ..   ⊡ UNEXPECTED-RESULT-SUCCESS
    ..   × EXPECTED-RESULT-FAILURE
    ..   ⊠ UNEXPECTED-RESULT-FAILURE
    ..   - RESULT-SKIP
    ..   ⊟ RESULT-ABORT*
    ..   NLX-TEST                           ; TRIAL-START
    ..     ⊟ non-local exit                 ; NLX
    ..   ⊟ NLX-TEST ⊟1                      ; VERDICT-ABORT*
    ..   ⊟ "UNHANDLED-ERROR" (SIMPLE-ERROR)
    .. ⊟ VERDICT-ABORT* ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (VERDICT-ABORT*)) ABORT* 0.004s ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1>
    ```
    
    The `⊟3 ⊠1 ⊡1 -1 ×1 ⋅1` part is the counts for [`*categories*`][e949] printed
    with their markers.

<a id="x-28TRY-3A-2APRINT-PARENT-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT-PARENT*%20VARIABLE"></a>

- [variable] **\*print-parent\*** *t*

    [Try var][0d7a]. When an [`event`][955d] is signalled and its parent [`trial`][99d0]'s type matches
    `*print-parent*`, the trial is printed as if its [`trial-start`][b664] matched
     the `print` argument of [`try`][b602].
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* t))
      (with-test (t0)
        (is t)
        (is t)))
    .. T0
    ..   ⋅ (IS T)
    ..   ⋅ (IS T)
    .. ⋅ T0 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* nil))
      (with-test (t0)
        (is t)
        (is t)))
    .. ⋅ (IS T)
    .. ⋅ (IS T)
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    `*print-parent*` `nil` combined with printing [`verdict`][52e1]s results in a flat
     output:
    
    ```common-lisp
    (let ((*print* '(or leaf verdict))
          (*print-parent* nil))
      (with-test (outer)
        (with-test (inner)
          (is t :msg "inner-t"))
        (is t :msg "outer-t")))
    .. ⋅ inner-t
    .. ⋅ INNER ⋅1
    .. ⋅ outer-t
    .. ⋅ OUTER ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```

<a id="x-28TRY-3A-2APRINT-INDENTATION-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT-INDENTATION*%20VARIABLE"></a>

- [variable] **\*print-indentation\*** *2*

    [Try var][0d7a]. The number of spaces each printed [`trial`][99d0] increases the indentation
    of its children.

<a id="x-28TRY-3A-2APRINT-DURATION-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT-DURATION*%20VARIABLE"></a>

- [variable] **\*print-duration\*** *nil*

    [Try var][0d7a]. If true, the number of seconds spent during execution is printed.
    
    ```common-lisp
    (let ((*print-duration* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (timed)
        (is (progn (sleep 0.1) t))
        (is (progn (sleep 0.2) t))
        (error "xxx")))
    ..        TIMED
    ..  0.100   ⋅ (IS (PROGN (SLEEP 0.1) T))
    ..  0.200   ⋅ (IS (PROGN (SLEEP 0.2) T))
    ..          ⊟ "xxx" (SIMPLE-ERROR)
    ..  0.300 ⊟ TIMED ⊟1 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (TIMED)) ABORT* 0.300s ⊟1 ⋅2>
    ```
    
    Timing is available for all [`outcome`][2656]s (i.e. for [Checks][bb56] and [`trial`][99d0]s).
    Checks generally measure the time spent during evaluation of the
    form they are wrapping. Trials measure the time between [`trial-start`][b664]
    and the [`verdict`][52e1].
    
    Timing information is not available for `trial-start` and [`error*`][0321]
    events.

<a id="x-28TRY-3A-2APRINT-COMPACTLY-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT-COMPACTLY*%20VARIABLE"></a>

- [variable] **\*print-compactly\*** *nil*

    [Try var][0d7a]. [`event`][955d]s whose type matches `*print-compactly*` are printed less
    verbosely. [`leaf`][f58d] events are printed only with their marker, and
    [`verdict`][52e1]s of trials without printed child trials are printed with `=>
    <marker>` (see [`*categories*`][e949]).
    
    ```common-lisp
    (let ((*print-compactly* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is t)
          (is nil)
          (error "xxx"))
        (loop repeat 10 do (is t))))
    .. OUTER ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    ..   INNER ⋅⊠⊟ => ⊟
    ..   ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    .. ⊠ OUTER ⊟1 ⊠1 ⋅21
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊟1 ⊠1 ⋅21>
    ```
    
    `*print-compactly*` has no effect on events being [`describe`][6651]d.

<a id="x-28TRY-3A-2APRINT-BACKTRACE-2A-20VARIABLE-29"></a>
<a id="TRY:*PRINT-BACKTRACE*%20VARIABLE"></a>

- [variable] **\*print-backtrace\*** *t*

    [Try var][0d7a]. Whether to print backtraces gathered when [`*gather-backtrace*`][a41d].

<a id="x-28TRY-3A-2ADEFER-DESCRIBE-2A-20VARIABLE-29"></a>
<a id="TRY:*DEFER-DESCRIBE*%20VARIABLE"></a>

- [variable] **\*defer-describe\*** *nil*

    [Try var][0d7a]. When an [`event`][955d] is to be [`*describe*`][aa6d]d and its type matches
    `*defer-describe*`, then instead of printing the often longish context
    information in the tree of events, it is deferred until after [`try`][b602]
    has finished. The following example only prints [`leaf`][f58d] events (due to
    [`*print*`][7ee9] and [`*print-parent*`][cc23]) and in compact form (see
    [`*print-compactly*`][3cf4]), deferring description of events matching
    `*describe*` until the end.
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*debug* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is (= (1+ 5) 7)))))
    .. ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⊠
    ..
    .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in OUTER INNER:
    .. (IS (= #1=(1+ 5) 7))
    .. where
    ..   #1# = 6
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅10>
    ```

<a id="x-28TRY-3A-40COUNT-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@COUNT%20MGL-PAX:SECTION"></a>

### 7.3 Counting Events

[`trial`][99d0]s have a counter for each category in [`*categories*`][e949]. When an
[`event`][955d] is recorded by [`try`][b602] and its type matches [`*count*`][3bb4], the counters
of all categories matching the event type are incremented in the
[`current-trial`][e186]. When a trial finishes and a [`verdict`][52e1] is recorded, the
trial's event counters are added to that of its parent's (if any).
The counts are printed with `verdict`s (see [Printing Events][b3f9]).

If both `*count*` and `*categories*` are unchanged from their default
values, then only [`leaf`][f58d] events are counted, and we get separate
counters for [`abort*`][8ec3], [`unexpected-failure`][b5cb], [`unexpected-success`][55cd], [`skip`][69a2],
[`expected-failure`][8620], and [`expected-success`][c96a].

```common-lisp
(let ((*debug* nil))
  (with-test (outer)
    (with-test (inner)
      (is t))
    (is t)
    (is nil)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   ⋅ INNER ⋅1
..   ⋅ (IS T)
..   ⊠ (IS NIL)
.. ⊠ OUTER ⊠1 ⋅2
..
==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅2>
```

As the above example shows, [`expected-verdict-success`][06c2] and
[`expected-result-success`][609c7] are both marked with `"⋅"`, but only
`expected-result-success` is counted due to `*count*` being `leaf`.

<a id="x-28TRY-3A-40COLLECT-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@COLLECT%20MGL-PAX:SECTION"></a>

### 7.4 Collecting Events

When an [`event`][955d] is recorded and the type of the `event` matches the
`collect` type argument of [`try`][b602], then a corresponding object is pushed
onto [`children`][de7d] of the [`current-trial`][e186] for subsequent [Rerunning Trials][e4ac] or [Reprocessing Trials][2337].

In particular, if the matching event is a [`leaf`][f58d], then the event
itself is collected. If the matching event is a [`trial-event`][b36a], then
[`verdict`][4bec] of its [`trial`][0f05] is
collected. Furthermore, trials which collected anything are always
collected by their parent.

By default, both implicit and explicit calls to `try` collect the
[`unexpected`][d6ad] (see [`*collect*`][307c] and [`*try-collect*`][0c39]), and consequently all
the enclosing trials.

<a id="x-28TRY-3ACHILDREN-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>
<a id="TRY:CHILDREN%20%28MGL-PAX:READER%20TRY:TRIAL%29"></a>

- [reader] **children** *[trial][99d0] (:children = nil)*

    A list of immediate child [`verdict`][52e1]s, [`result`][231f]s, and
    [`error*`][0321]s collected in reverse chronological order (see [Collecting Events][52e5]).
    The `verdict` of this [`trial`][99d0] is not among `children`, but the `verdict`s
    of child trials' are.

<a id="x-28TRY-3A-40RERUN-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@RERUN%20MGL-PAX:SECTION"></a>

### 7.5 Rerunning Trials

When a [`trial`][99d0] is [`funcall`][03c7]ed or passed to [`try`][b602], the *test that created
the trial* is invoked, and it may be run again in its entirety or in
part. As the test runs, it may invoke other tests. Any
test (including the top-level one) is skipped if it does not
correspond to a [collected][52e5] trial or its [`trial-start`][b664] event
and [`verdict`][52e1] do not match the `rerun` argument of `try`. When that
happens, the corresponding function call immediately returns the
`trial` object. In trials that are rerun, [Checks][bb56] are executed
normally.

- A new trial is skipped (as if with [`skip-trial`][f45a]) if `rerun` is not `t`
  and

    - there is no trial representing the same function call among
      the collected but not yet rerun trials in the trial being
      rerun, or

    - the first such trial does not match the `rerun` type argument of
      `try` in that neither its `trial-start`, `verdict` events match the
      type `rerun`, nor do any of its collected [`result`][231f]s and trials.

- If `rerun` is `t`, then the test is run in its entirety, including
  even the non-collected trials. Use `rerun` [`event`][955d] to run only the
  collected trials.

- The *test that created the trial* is determined as follows.

    - If the trial was created by calling a [`deftest`][e7ca] function, then
      the test currently associated with that symbol naming the
      function is called with the arguments of the original function
      call. If the symbol is no longer [`fboundp`][95bb] (because it was
      [`fmakunbound`][609c]) or it no longer names a `deftest` (it was redefined
      with [`defun`][f472]), then an error is signalled.

    - If the trial was created by entering a [`with-test`][8f5d] form, then
      its body is executed again in the original lexical but the
      current dynamic environment. Implementationally speaking,
      `with-test` defines a local function of no arguments (likely a
      closure) that wraps its body, stores the closure in the trial
      object and calls it on a rerun in a `with-test` with the same
      `var-or-name` and same `name`.

    - If the trial was created by `try` itself to ensure that all
      events are signalled in a trial (see [Explicit `try`][1720]), then
      on a rerun the same `testable` is run again.

    All three possibilities involve entering `deftest` or `with-test`,
    or invoking `try`: the same cases that we have with [Implicit `try`][012f].
    Thus, even if a trial is rerun with `funcall`, execution is
    guaranteed to happen under `try`.


<a id="x-28TRY-3A-2ARERUN-CONTEXT-2A-20VARIABLE-29"></a>
<a id="TRY:*RERUN-CONTEXT*%20VARIABLE"></a>

- [variable] **\*rerun-context\*** *nil*

    [Try var][0d7a]. A [`trial`][99d0] or `nil`. If it's a `trial`, then [`try`][b602] will
    [rerun][e4ac] this trial skipping everything that does not lead to
    an invocation of a basic testable in its `testable` argument (see
    [Testables][cdc3]). If no route to any basic testable function can be found
    among the [collected][52e5] events of the context, then a
    warning is signalled and the context is ignored.
    
    Consider the following code evaluated in the package `try`:
    
    ```common-lisp
    (deftest test-try ()
      (let ((*package* (find-package :cl-user)))
        (test-whatever)
        (test-printing)))
    
    (deftest test-whatever ()
      (is t))
    
    (deftest test-printing ()
      (is (equal (prin1-to-string 'x) "TRY::X")))
    
    (test-try)
    .. TEST-TRY
    ..   TEST-WHATEVER
    ..     ⋅ (IS T)
    ..   ⋅ TEST-WHATEVER ⋅1
    ..   TEST-PRINTING
    ..     ⋅ (IS (EQUAL (PRIN1-TO-STRING 'X) "TRY::X"))
    ..   ⋅ TEST-PRINTING ⋅1
    .. ⋅ TEST-TRY ⋅2
    ..
    ==> #<TRIAL (TEST-TRY) EXPECTED-SUCCESS 0.500s ⋅2>
    
    ;; This could also be an implicit try such as (TEST-PRINTING),
    ;; but this way we avoid entering the debugger.
    (try 'test-printing)
    .. TEST-PRINTING
    ..   ⊠ (IS (EQUAL #1=(PRIN1-TO-STRING 'X) "TRY::X"))
    ..     where
    ..       #1# = "X"
    .. ⊠ TEST-PRINTING ⊠1
    ..
    ==> #<TRIAL (TEST-PRINTING) UNEXPECTED-FAILURE 0.200s ⊠1>
    ```
    
    `test-printing` fails because when called directly, [`*package*`][5ed1] is not
    the expected `cl-user`. However, when `*rerun-context*` is set,
    `test-printing` will be executed in the correct dynamic environment.
    
    ```common-lisp
    (setq *rerun-context*
          ;; Avoid the debugger, as a matter of style.
          (try 'test-try))
    
    (test-printing)
    .. TEST-TRY
    ..   - TEST-WHATEVER
    ..   TEST-PRINTING
    ..     ⋅ (IS (EQUAL (PRIN1-TO-STRING 'X) "TRY::X"))
    ..   ⋅ TEST-PRINTING ⋅1
    .. ⋅ TEST-TRY ⋅1
    ..
    ```
    
    Note how `test-whatever` was [`skip`][69a2]ped because it leads to no calls to
    `test-printing`.
    
    See [Emacs Integration][4c86] for a convenient way of taking advantage of this feature.

<a id="x-28TRY-3A-40REPLAY-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@REPLAY%20MGL-PAX:SECTION"></a>

### 7.6 Reprocessing Trials

<a id="x-28TRY-3AREPLAY-EVENTS-20FUNCTION-29"></a>
<a id="TRY:REPLAY-EVENTS%20FUNCTION"></a>

- [function] **replay-events** *trial &key (collect \*try-collect\*) (print \*try-print\*) (describe \*try-describe\*) (stream \*try-stream\*) (printer \*try-printer\*)*

    `replay-events` reprocesses the events [collected][52e5] in
    `trial` without actually running the tests that produced them. It
    simply signals the events collected in `trial` again to allow further
    processing. It takes the same arguments as [`try`][b602] except
    `debug`, `count` and `rerun`. The values of
    [`*categories*`][e949] and [`*count*`][3bb4] that were in effect for `trial` are used, and
    their current values are ignored to be able to keep consistent
    counts (see [Counting Events][886e]).
    
    Suppose we ran a large test using the default `:print` and into a
    rare non-deterministic bug.
    
    ```common-lisp
    (deftest some-test ()
      (with-test (inner)
        (is t)
        (is (= 10 7))   ; fake non-deterministic bug
        (is t))
      (error "my-msg"))
    
    (try 'some-test)
    ```
    
    Now, the output is too large with [`expected`][b194] events and the backtrace
    cluttering it. We could try running the test again with different
    settings, or even just [rerunning][e4ac] it, but that might make
    the bug go away. Instead of searching for the interesting bits in
    the text output, we can replay the events and print only the
    [`unexpected`][d6ad] events:
    
    ```common-lisp
    (let ((*print-backtrace* nil))
      (replay-events ! :print 'unexpected))
    .. SOME-TEST
    ..   INNER
    ..     ⊠ (IS (= 10 7))
    ..   ⊠ INNER ⊠1 ⋅2
    ..   ⊟ "my-msg" (SIMPLE-ERROR)
    .. ⊟ SOME-TEST ⊟1 ⊠1 ⋅2
    ..
    ==> #<TRIAL (SOME-TEST) ABORT* 0.500s ⊟1 ⊠1 ⋅2>
    ```
    
    Now, we decide that nesting of test is unimportant here, change to
    new printer settings and replay:
    
    ```common-lisp
    (let ((*print-backtrace* nil)
          (*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t))
      (replay-events !))
    .. ⊠⊟
    .. ⊟ SOME-TEST ⊟1 ⊠1 ⋅2
    ..
    .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in SOME-TEST INNER:
    .. (IS (= 10 7))
    ..
    .. ;; UNHANDLED-ERROR (⊟) in SOME-TEST:
    .. "my-msg" (SIMPLE-ERROR)
    ..
    ==> #<TRIAL (SOME-TEST) ABORT* 0.500s ⊟1 ⊠1 ⋅2>
    ```

<a id="x-28TRY-3A-40IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@IMPLEMENTATION-NOTES%20MGL-PAX:SECTION"></a>

## 8 Implementation Notes

Try is supported on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL and
SBCL.

- Pretty printing is non-existent on CLISP and broken on ABCL. The
  output may look garbled on them.

- Gray streams are broken on ABCL so the output may look even worse
  [https://abcl.org/trac/ticket/373](https://abcl.org/trac/ticket/373).

- ABCL, CMUCL and ECL have a bug related to losing
  [`eql`][db03]ness of source literals
  <https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665>. The
  result is somewhat cosmetic; it may cause multiple captures being
  made for the same thing.


<a id="x-28TRY-3A-40GLOSSARY-20MGL-PAX-3ASECTION-29"></a>
<a id="TRY:@GLOSSARY%20MGL-PAX:SECTION"></a>

## 9 Glossary

<a id="x-28TRY-3A-40FUNCALLABLE-INSTANCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="TRY:@FUNCALLABLE-INSTANCE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **funcallable instance**

    This is a term from the MOP. A funcallable instance is an instance
    of a class that's a subclass of `mop:funcallable-standard-class`. It
    is like a normal instance, but it can also be [`funcall`][03c7]ed.

<a id="x-28TRY-3A-40CANCELLED-NLX-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="TRY:@CANCELLED-NLX%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **cancelled non-local exit**

    This is a term from the Common Lisp ANSI standard. If during the
    unwinding of the stack initiated by a [non-local exit][b815] another
    nlx is initiated in, and exits from an [`unwind-protect`][c93f] cleanup form,
    then this second nlx is said to have cancelled the first, and the
    first nlx will not continue.
    
    ```common-lisp
    (catch 'foo
      (catch 'bar
        (unwind-protect
             (throw 'foo 'foo)
          (throw 'bar 'bar))))
    => BAR
    ```

  [0126]: #TRY:*STREAM*%20VARIABLE "TRY:*STREAM* VARIABLE"
  [012f]: #TRY:@IMPLICIT-TRY%20MGL-PAX:SECTION "Implicit `try`"
  [01e7]: #TRY:*TRY-RERUN*%20VARIABLE "TRY:*TRY-RERUN* VARIABLE"
  [0321]: #TRY:ERROR*%20CONDITION "TRY:ERROR* CONDITION"
  [03c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL (MGL-PAX:CLHS FUNCTION)"
  [03ec]: #TRY:@CATEGORIES%20MGL-PAX:SECTION "Categories"
  [062e]: #TRY:UNEXPECTED-VERDICT-SUCCESS%20CONDITION "TRY:UNEXPECTED-VERDICT-SUCCESS CONDITION"
  [06c2]: #TRY:EXPECTED-VERDICT-SUCCESS%20CONDITION "TRY:EXPECTED-VERDICT-SUCCESS CONDITION"
  [0743]: #TRY:@WRITING-AUTOMATIC-CAPTURE-RULES%20MGL-PAX:SECTION "Writing Automatic Capture Rules"
  [0992]: #TRY:DISMISSAL%20CONDITION "TRY:DISMISSAL CONDITION"
  [0aad]: #TRY:NESTED-CONDITION%20%28MGL-PAX:READER%20TRY:UNHANDLED-ERROR%29 "TRY:NESTED-CONDITION (MGL-PAX:READER TRY:UNHANDLED-ERROR)"
  [0c39]: #TRY:*TRY-COLLECT*%20VARIABLE "TRY:*TRY-COLLECT* VARIABLE"
  [0d57]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "DOUBLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [0d7a]: #TRY:@TRYVAR%20MGL-PAX:GLOSSARY-TERM "Try var"
  [0e59]: http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm "GENSYM (MGL-PAX:CLHS FUNCTION)"
  [0f05]: #TRY:TRIAL%20%28MGL-PAX:READER%20TRY:TRIAL-EVENT%29 "TRY:TRIAL (MGL-PAX:READER TRY:TRIAL-EVENT)"
  [1013]: http://www.lispworks.com/documentation/HyperSpec/Body/f_not.htm "NOT (MGL-PAX:CLHS FUNCTION)"
  [12ce]: #TRY:INVOKES-DEBUGGER%20MGL-PAX:MACRO "TRY:INVOKES-DEBUGGER MGL-PAX:MACRO"
  [1383]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "MACROLET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [162a]: #TRY:MATCH-VALUES%20MGL-PAX:MACRO "TRY:MATCH-VALUES MGL-PAX:MACRO"
  [1720]: #TRY:@EXPLICIT-TRY%20MGL-PAX:SECTION "Explicit `try`"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [18ff]: #TRY:*TRY-DEBUG*%20VARIABLE "TRY:*TRY-DEBUG* VARIABLE"
  [19f3]: #TRY:CAPTURE%20MGL-PAX:MACRO "TRY:CAPTURE MGL-PAX:MACRO"
  [1cdc]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debugg.htm "*DEBUGGER-HOOK* (MGL-PAX:CLHS VARIABLE)"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1d97]: #TRY:WITH-EXPECTED-OUTCOME%20MGL-PAX:MACRO "TRY:WITH-EXPECTED-OUTCOME MGL-PAX:MACRO"
  [1eb3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_1pl_1_.htm "1+ (MGL-PAX:CLHS FUNCTION)"
  [20d8]: #TRY:@EXPLICIT-CAPTURES%20MGL-PAX:SECTION "Explicit Captures"
  [21d9]: #TRY:PASS%20TYPE "TRY:PASS TYPE"
  [231f]: #TRY:RESULT%20CONDITION "TRY:RESULT CONDITION"
  [2337]: #TRY:@REPLAY%20MGL-PAX:SECTION "Reprocessing Trials"
  [2364]: #TRY:@CHECK-RESTARTS%20MGL-PAX:SECTION "Check Restarts"
  [2415]: pax-manual.md "PAX Manual"
  [247c]: #TRY:ACT%20CONDITION "TRY:ACT CONDITION"
  [25f5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_null.htm "NULL (MGL-PAX:CLHS FUNCTION)"
  [2656]: #TRY:OUTCOME%20CONDITION "TRY:OUTCOME CONDITION"
  [269a]: #TRY:SUCCESS%20CONDITION "TRY:SUCCESS CONDITION"
  [279a]: #TRY:@CONCRETE-EVENTS%20MGL-PAX:SECTION "Concrete Events"
  [2eef]: #TRY:@FUNCALLABLE-INSTANCE%20MGL-PAX:GLOSSARY-TERM "funcallable instance"
  [307c]: #TRY:*COLLECT*%20VARIABLE "TRY:*COLLECT* VARIABLE"
  [30c9]: #TRY:EXPECTED-VERDICT-FAILURE%20CONDITION "TRY:EXPECTED-VERDICT-FAILURE CONDITION"
  [31a6]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "SINGLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [351f]: #TRY:CAPTURE-VALUES%20MGL-PAX:MACRO "TRY:CAPTURE-VALUES MGL-PAX:MACRO"
  [35ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_error.htm "ERROR (MGL-PAX:CLHS FUNCTION)"
  [37c1]: #TRY:@MISC-CHECKS%20MGL-PAX:SECTION "Miscellaneous Checks"
  [38e8]: #TRY:*RERUN-CONTEXT*%20VARIABLE "TRY:*RERUN-CONTEXT* VARIABLE"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3ace]: #TRY:BACKTRACE-OF%20%28MGL-PAX:READER%20TRY:UNHANDLED-ERROR%29 "TRY:BACKTRACE-OF (MGL-PAX:READER TRY:UNHANDLED-ERROR)"
  [3bb4]: #TRY:*COUNT*%20VARIABLE "TRY:*COUNT* VARIABLE"
  [3cf4]: #TRY:*PRINT-COMPACTLY*%20VARIABLE "TRY:*PRINT-COMPACTLY* VARIABLE"
  [3d27]: #TRY:@CAPTURES%20MGL-PAX:SECTION "Captures"
  [3e0c]: #TRY:@MIDDLE-LAYER-OF-EVENTS%20MGL-PAX:SECTION "Middle Layer of Events"
  [3eef]: #TRY:@IMPLEMENTATION-NOTES%20MGL-PAX:SECTION "Implementation Notes"
  [3eef7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_ret_fr.htm "RETURN-FROM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4444]: http://www.lispworks.com/documentation/HyperSpec/Body/m_mult_1.htm "MULTIPLE-VALUE-LIST (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [47d5]: #TRY:SUB%20STRUCTURE "TRY:SUB STRUCTURE"
  [4805]: #TRY:VERDICT-ABORT*%20CONDITION "TRY:VERDICT-ABORT* CONDITION"
  [4bec]: #TRY:VERDICT%20%28MGL-PAX:READER%20TRY:TRIAL%29 "TRY:VERDICT (MGL-PAX:READER TRY:TRIAL)"
  [4c86]: #TRY:@EMACS%20MGL-PAX:SECTION "Emacs Integration"
  [4e46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm "FIND (MGL-PAX:CLHS FUNCTION)"
  [4f9f]: #TRY:ABORT-TRIAL%20FUNCTION "TRY:ABORT-TRIAL FUNCTION"
  [4fc4]: #TRY:@EMACS-SETUP%20MGL-PAX:SECTION "Emacs Setup"
  [5065]: #TRY:TEST-BOUND-P%20FUNCTION "TRY:TEST-BOUND-P FUNCTION"
  [5237]: #TRY:@EVENT-GLUE%20MGL-PAX:SECTION "Event Glue"
  [5289]: #TRY:WARN-ON-TESTS-NOT-RUN%20MGL-PAX:MACRO "TRY:WARN-ON-TESTS-NOT-RUN MGL-PAX:MACRO"
  [52e1]: #TRY:VERDICT%20CONDITION "TRY:VERDICT CONDITION"
  [52e5]: #TRY:@COLLECT%20MGL-PAX:SECTION "Collecting Events"
  [5333]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "> (MGL-PAX:CLHS FUNCTION)"
  [5355]: #TRY:@TRIAL-RESTARTS%20MGL-PAX:SECTION "Trial Restarts"
  [5379]: #TRY:@CALLING-TEST-FUNCTIONS%20MGL-PAX:SECTION "Calling Test Functions"
  [55cd]: #TRY:UNEXPECTED-SUCCESS%20TYPE "TRY:UNEXPECTED-SUCCESS TYPE"
  [56ae]: #TRY:@AUTOMATIC-CAPTURES%20MGL-PAX:SECTION "Automatic Captures"
  [5786]: #TRY:VERDICT-SKIP%20CONDITION "TRY:VERDICT-SKIP CONDITION"
  [59c3]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_h.htm#handle "\"handle\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5b0b]: http://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm "RETURN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [5d4a]: #TRY:RUNNINGP%20FUNCTION "TRY:RUNNINGP FUNCTION"
  [5e1a]: #TRY:@TRIAL-VERDICTS%20MGL-PAX:SECTION "Trial Verdicts"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [609c7]: #TRY:EXPECTED-RESULT-SUCCESS%20CONDITION "TRY:EXPECTED-RESULT-SUCCESS CONDITION"
  [628a]: #TRY:@LINKS%20MGL-PAX:SECTION "Links and Systems"
  [6300]: pax-manual.md#MGL-PAX:@TRANSCRIPTS%20MGL-PAX:SECTION "Transcripts"
  [6384]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 (MGL-PAX:CLHS FUNCTION)"
  [63db]: #TRY:*RERUN*%20VARIABLE "TRY:*RERUN* VARIABLE"
  [650d]: http://www.lispworks.com/documentation/HyperSpec/Body/a_lambda.htm "LAMBDA (MGL-PAX:CLHS NIL)"
  [6651]: http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm "DESCRIBE (MGL-PAX:CLHS FUNCTION)"
  [669b]: http://www.lispworks.com/documentation/HyperSpec/Body/a_error.htm "ERROR (MGL-PAX:CLHS NIL)"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [6910]: #TRY:WITH-TESTS-RUN%20MGL-PAX:MACRO "TRY:WITH-TESTS-RUN MGL-PAX:MACRO"
  [69a2]: #TRY:SKIP%20CONDITION "TRY:SKIP CONDITION"
  [6cfa]: #TRY:@FORMAT-SPECIFIER-FORM%20MGL-PAX:SECTION "Format Specifier Form"
  [6d4e]: #TRY:SIGNALS%20MGL-PAX:MACRO "TRY:SIGNALS MGL-PAX:MACRO"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [7230]: #TRY:*PRINTER*%20VARIABLE "TRY:*PRINTER* VARIABLE"
  [7647]: #TRY:*PRINT-BACKTRACE*%20VARIABLE "TRY:*PRINT-BACKTRACE* VARIABLE"
  [790c]: #TRY:%25%20MACROLET "TRY:% MACROLET"
  [7955]: #TRY:FLOAT-~%3D%20FUNCTION "TRY:FLOAT-~= FUNCTION"
  [7ab6]: #TRY:@CANCELLED-NLX%20MGL-PAX:GLOSSARY-TERM "cancelled non-local exit"
  [7af9]: #TRY:SIGNALS-NOT%20MGL-PAX:MACRO "TRY:SIGNALS-NOT MGL-PAX:MACRO"
  [7c3f]: #TRY:RESULT-SKIP%20CONDITION "TRY:RESULT-SKIP CONDITION"
  [7ee9]: #TRY:*PRINT*%20VARIABLE "TRY:*PRINT* VARIABLE"
  [7ef5]: #TRY:@OUTCOME-RESTARTS%20MGL-PAX:SECTION "Outcome Restarts"
  [7f8e]: #TRY:@ERRORS%20MGL-PAX:SECTION "Errors"
  [80d6]: #TRY:IS%20MGL-PAX:MACRO "TRY:IS MGL-PAX:MACRO"
  [826a]: #TRY:ABORT-CHECK%20FUNCTION "TRY:ABORT-CHECK FUNCTION"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [856d]: #TRY:*DEBUG*%20VARIABLE "TRY:*DEBUG* VARIABLE"
  [8620]: #TRY:EXPECTED-FAILURE%20TYPE "TRY:EXPECTED-FAILURE TYPE"
  [87a5]: http://www.lispworks.com/documentation/HyperSpec/Body/a_contin.htm "CONTINUE (MGL-PAX:CLHS NIL)"
  [886e]: #TRY:@COUNT%20MGL-PAX:SECTION "Counting Events"
  [8aea]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_designator "\"function designator\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [8b69]: #TRY:REPLAY-EVENTS%20FUNCTION "TRY:REPLAY-EVENTS FUNCTION"
  [8cf6]: #TRY:RETRY-CHECK%20FUNCTION "TRY:RETRY-CHECK FUNCTION"
  [8ec3]: #TRY:ABORT*%20CONDITION "TRY:ABORT* CONDITION"
  [8f49]: http://www.lispworks.com/documentation/HyperSpec/Body/f_signal.htm "SIGNAL (MGL-PAX:CLHS FUNCTION)"
  [8f5d]: #TRY:WITH-TEST%20MGL-PAX:MACRO "TRY:WITH-TEST MGL-PAX:MACRO"
  [8f78]: #TRY:UNHANDLED-ERROR%20CONDITION "TRY:UNHANDLED-ERROR CONDITION"
  [8f7a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm "*PRINT-LENGTH* (MGL-PAX:CLHS VARIABLE)"
  [8f9f]: #TRY:*N-RECENT-TRIALS*%20VARIABLE "TRY:*N-RECENT-TRIALS* VARIABLE"
  [906a]: #TRY:@CHECK-UTILITIES%20MGL-PAX:SECTION "Check Utilities"
  [92af]: #TRY:%21%20VARIABLE "TRY:! VARIABLE"
  [955d]: #TRY:EVENT%20CONDITION "TRY:EVENT CONDITION"
  [95bb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm "FBOUNDP (MGL-PAX:CLHS FUNCTION)"
  [97ee]: http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm "ASSERT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [99d0]: #TRY:TRIAL%20CLASS "TRY:TRIAL CLASS"
  [9c9c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm "EVAL-WHEN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9fa9]: #TRY:@COMPARING-FLOATS%20MGL-PAX:SECTION "Comparing Floats"
  [a07f]: #TRY:*BEST-MATCHING-CONDITION*%20VARIABLE "TRY:*BEST-MATCHING-CONDITION* VARIABLE"
  [a41d]: #TRY:*GATHER-BACKTRACE*%20VARIABLE "TRY:*GATHER-BACKTRACE* VARIABLE"
  [aa6d]: #TRY:*DESCRIBE*%20VARIABLE "TRY:*DESCRIBE* VARIABLE"
  [aaaa]: #TRY:INVOKES-DEBUGGER-NOT%20MGL-PAX:MACRO "TRY:INVOKES-DEBUGGER-NOT MGL-PAX:MACRO"
  [aaf2]: #TRY:@EVENTS%20MGL-PAX:SECTION "Events"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [afb9]: #TRY:@PRINTING-EVENTS%20MGL-PAX:SECTION "Printing Events"
  [b115]: #TRY:NLX%20CONDITION "TRY:NLX CONDITION"
  [b194]: #TRY:EXPECTED%20CONDITION "TRY:EXPECTED CONDITION"
  [b33f]: #TRY:N-RETRIES%20%28MGL-PAX:READER%20TRY:TRIAL%29 "TRY:N-RETRIES (MGL-PAX:READER TRY:TRIAL)"
  [b36a]: #TRY:TRIAL-EVENT%20CONDITION "TRY:TRIAL-EVENT CONDITION"
  [b3f9]: #TRY:@PRINT%20MGL-PAX:SECTION "Printing Events"
  [b426]: #TRY:LIST-PACKAGE-TESTS%20FUNCTION "TRY:LIST-PACKAGE-TESTS FUNCTION"
  [b5cb]: #TRY:UNEXPECTED-FAILURE%20TYPE "TRY:UNEXPECTED-FAILURE TYPE"
  [b602]: #TRY:TRY%20FUNCTION "TRY:TRY FUNCTION"
  [b664]: #TRY:TRIAL-START%20CONDITION "TRY:TRIAL-START CONDITION"
  [b71e]: #TRY:WITH-SKIP%20MGL-PAX:MACRO "TRY:WITH-SKIP MGL-PAX:MACRO"
  [b72c]: #TRY:UNEXPECTED-RESULT-SUCCESS%20CONDITION "TRY:UNEXPECTED-RESULT-SUCCESS CONDITION"
  [b815]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_n.htm#non-local_exit "\"non-local exit\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [b949]: #TRY:@TUTORIAL%20MGL-PAX:SECTION "Tutorial"
  [b94a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mismat.htm "MISMATCH (MGL-PAX:CLHS FUNCTION)"
  [bb56]: #TRY:@CHECKS%20MGL-PAX:SECTION "Checks"
  [bba7]: http://www.lispworks.com/documentation/HyperSpec/Body/09_adbd.htm "\"9.1.4.2.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [c1f6]: #TRY:%25%25%20MACROLET "TRY:%% MACROLET"
  [c3a0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "< (MGL-PAX:CLHS FUNCTION)"
  [c759]: #TRY:@GLOSSARY%20MGL-PAX:SECTION "Glossary"
  [c864]: #TRY:*TRY-PRINTER*%20VARIABLE "TRY:*TRY-PRINTER* VARIABLE"
  [c8cb]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cir.htm "*PRINT-CIRCLE* (MGL-PAX:CLHS VARIABLE)"
  [c93f]: http://www.lispworks.com/documentation/HyperSpec/Body/s_unwind.htm "UNWIND-PROTECT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c96a]: #TRY:EXPECTED-SUCCESS%20TYPE "TRY:EXPECTED-SUCCESS TYPE"
  [cc23]: #TRY:*PRINT-PARENT*%20VARIABLE "TRY:*PRINT-PARENT* VARIABLE"
  [cdba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_uninte.htm "UNINTERN (MGL-PAX:CLHS FUNCTION)"
  [cdc3]: #TRY:@TESTABLES%20MGL-PAX:SECTION "Testables"
  [ce49]: #TRY:RECORD-EVENT%20FUNCTION "TRY:RECORD-EVENT FUNCTION"
  [cf88]: #TRY:*CONDITION-MATCHED-P*%20VARIABLE "TRY:*CONDITION-MATCHED-P* VARIABLE"
  [cfd3]: #TRY:*RUN-DEFTEST-WHEN*%20VARIABLE "TRY:*RUN-DEFTEST-WHEN* VARIABLE"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2d8]: http://www.lispworks.com/documentation/HyperSpec/Body/s_block.htm "BLOCK (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d4ce]: #TRY:@EVENT-RESTARTS%20MGL-PAX:SECTION "Event Restarts"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d5a9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM (MGL-PAX:CLHS CLASS)"
  [d5ea]: #TRY:FAIL%20TYPE "TRY:FAIL TYPE"
  [d619]: #TRY:EXPECTED-RESULT-FAILURE%20CONDITION "TRY:EXPECTED-RESULT-FAILURE CONDITION"
  [d6ad]: #TRY:UNEXPECTED%20CONDITION "TRY:UNEXPECTED CONDITION"
  [daeb]: #TRY:UNEXPECTED-RESULT-FAILURE%20CONDITION "TRY:UNEXPECTED-RESULT-FAILURE CONDITION"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [dbd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST (MGL-PAX:CLHS FUNCTION)"
  [dc28]: #TRY:@TESTS%20MGL-PAX:SECTION "Tests"
  [de7d]: #TRY:CHILDREN%20%28MGL-PAX:READER%20TRY:TRIAL%29 "TRY:CHILDREN (MGL-PAX:READER TRY:TRIAL)"
  [e186]: #TRY:CURRENT-TRIAL%20FUNCTION "TRY:CURRENT-TRIAL FUNCTION"
  [e2e0]: #TRY:@IS%20MGL-PAX:SECTION "The `is` Macro"
  [e4ac]: #TRY:@RERUN%20MGL-PAX:SECTION "Rerunning Trials"
  [e514]: #TRY:@OUTCOMES%20MGL-PAX:SECTION "Outcomes"
  [e52f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "= (MGL-PAX:CLHS FUNCTION)"
  [e6be]: #TRY:@TRIALS%20MGL-PAX:SECTION "Trials"
  [e760]: http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm "THROW (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [e7ca]: #TRY:DEFTEST%20MGL-PAX:MACRO "TRY:DEFTEST MGL-PAX:MACRO"
  [e80e]: #TRY:FAILS%20MGL-PAX:MACRO "TRY:FAILS MGL-PAX:MACRO"
  [e8d7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_endp.htm "ENDP (MGL-PAX:CLHS FUNCTION)"
  [e949]: #TRY:*CATEGORIES*%20VARIABLE "TRY:*CATEGORIES* VARIABLE"
  [eb5c]: #TRY:ON-VALUES%20MGL-PAX:MACRO "TRY:ON-VALUES MGL-PAX:MACRO"
  [f001]: #TRY:SET-TRY-DEBUG%20FUNCTION "TRY:SET-TRY-DEBUG FUNCTION"
  [f3af]: #TRY:IN-TIME%20MGL-PAX:MACRO "TRY:IN-TIME MGL-PAX:MACRO"
  [f45a]: #TRY:SKIP-TRIAL%20FUNCTION "TRY:SKIP-TRIAL FUNCTION"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f4bf]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*QUERY-IO* (MGL-PAX:CLHS VARIABLE)"
  [f58d]: #TRY:LEAF%20CONDITION "TRY:LEAF CONDITION"
  [f7f7]: #TRY:@CHECK-LIBRARY%20MGL-PAX:SECTION "Check Library"
  [f8bf]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_di.htm "SET-DIFFERENCE (MGL-PAX:CLHS FUNCTION)"
  [f92d]: #TRY:FAILURE%20CONDITION "TRY:FAILURE CONDITION"
  [fae3]: #TRY:RETRY-TRIAL%20FUNCTION "TRY:RETRY-TRIAL FUNCTION"
  [fb0e]: #TRY:SKIP-CHECK%20FUNCTION "TRY:SKIP-CHECK FUNCTION"
  [fb53]: #TRY:*IS-CAPTURES*%20VARIABLE "TRY:*IS-CAPTURES* VARIABLE"
  [fd3c]: http://www.lispworks.com/documentation/HyperSpec/Body/m_handle.htm "HANDLER-BIND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [fdf4]: #TRY:UNEXPECTED-VERDICT-FAILURE%20CONDITION "TRY:UNEXPECTED-VERDICT-FAILURE CONDITION"
  [ff2c]: #TRY:@CHECKING-CONDITIONS%20MGL-PAX:SECTION "Checking Conditions"
  [ffab]: #TRY:RESULT-ABORT*%20CONDITION "TRY:RESULT-ABORT* CONDITION"
