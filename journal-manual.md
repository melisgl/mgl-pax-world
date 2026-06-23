<a id="x-28JOURNAL-3A-40JOURNAL-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-MANUAL%20MGL-PAX:SECTION"></a>

# Journal Manual

## Table of Contents

- [1 Links][aa14]
- [2 Portability][1d0d]
- [3 Background][0114]
- [4 Distinguishing features][9105]
- [5 Basics][f846]
    - [5.1 In-events][186b]
    - [5.2 Out-events][48ef]
    - [5.3 Working with unreadable values][b354]
    - [5.4 Utilities][ba32]
    - [5.5 Pretty-printing][47a7]
    - [5.6 Error handling][bb08]
- [6 Logging][4e53]
    - [6.1 Customizing logs][297c]
    - [6.2 `:log-record`][a6ac]
    - [6.3 Logging with `leaf-event`s][bece]
- [7 Tracing][e03f]
    - [7.1 Slime integration][42a5]
- [8 Replay][041c]
    - [8.1 Journaled for replay][d700]
    - [8.2 Bundles][260d]
    - [8.3 The replay strategy][a8a7]
    - [8.4 Matching in-events][3c21]
        - [8.4.1 Replaying the outcome][7991]
    - [8.5 Matching out-events][7f9d]
    - [8.6 Replay failures][2933]
    - [8.7 Upgrades and replay][750a]
- [9 Testing][7682]
    - [9.1 Testing on multiple levels][9376]
- [10 Persistence][37c4]
    - [10.1 Persistence tutorial][6169]
    - [10.2 Synchronization to storage][046e]
        - [10.2.1 Synchronization strategies][f532]
        - [10.2.2 Synchronization with in-memory journals][12ff]
        - [10.2.3 Synchronization with file journals][674f]
- [11 Safety][7bf3]
- [12 Events reference][faf2]
    - [12.1 Event versions][54c1]
    - [12.2 In-events][f37b]
    - [12.3 Out-events][72cd]
    - [12.4 Leaf-events][86f6]
- [13 Journals reference][fbbb]
    - [13.1 Comparing journals][b7d2]
    - [13.2 In-memory journals][b792]
    - [13.3 File journals][e748]
    - [13.4 Pretty-printing journals][1496]
- [14 Bundles reference][ff8f]
    - [14.1 In-memory bundles][8bc1]
    - [14.2 File bundles][c05a]
- [15 Streamlets reference][f4d5]
    - [15.1 Opening and closing][bfc5]
    - [15.2 Reading from streamlets][adcd]
    - [15.3 Writing to streamlets][14f7]
- [16 Glossary][48f5]

###### \[in package JOURNAL with nicknames JRN\]
<a id="x-28-22journal-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22journal%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"journal"**

    - _Version:_ 0.1.0
    - _Description:_ A library built around explicit execution traces for
        logging, tracing, testing and persistence.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis <mega@retes.hu>
    - _Homepage:_ <http://github.com/melisgl/journal>
    - _Bug tracker:_ <http://github.com/melisgl/journal/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/journal.git)
    - *Depends on:* alexandria, bordeaux-threads, local-time, [mgl-pax][6fdb], osicat(?), sb-posix(?), trivial-features, trivial-garbage

<a id="x-28JOURNAL-3A-40JOURNAL-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-LINKS%20MGL-PAX:SECTION"></a>

## 1 Links

Here is the [official repository](https://github.com/melisgl/journal)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/journal-manual.html)
for the latest version.

<a id="x-28JOURNAL-3A-40JOURNAL-PORTABILITY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-PORTABILITY%20MGL-PAX:SECTION"></a>

## 2 Portability

Tested and supported on ABCL, CCL, CMUCL, ECL, and SBCL.
AllegroCL Express edition runs out of heap while running the tests.
On Lisps that seem to lack support for disabling and enabling of
interrupts, such as ABCL, durability is compromised, and any attempt
to [`sync-journal`][b2ff] (see [Synchronization strategies][f532] and [Safety][7bf3]) will
be a runtime error.

Journal depends on BORDEAUX-THREADS. Consequently, it does not load
on implementations without real thread such as CLISP.

<a id="x-28JOURNAL-3A-40JOURNAL-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-BACKGROUND%20MGL-PAX:SECTION"></a>

## 3 Background

Logging, tracing, testing, and persistence are about what happened
during code execution. Recording machine-readable logs and traces
can be repurposed for white-box testing. More, when the code is
rerun, selected frames may return their recorded values without
executing the code, which could serve as a [mock object][16a9] framework
for writing tests. This ability to isolate external interactions and
to reexecute traces is sufficient to reconstruct the state of a
program, achieving simple persistence not unlike a [journaling filesystem][07c0] or
[event sourcing][7571].

Journal is the library to log, trace, test and persist. It has a
single macro at its heart: [`journaled`][6267], which does pretty much what
was described. It can be thought of as generating two events around
its body: one that records the name and an argument list (as in a
function call), and another that records the return values. In
Lisp-like pseudocode:

```
(defmacro journaled (name args &body body)
  `(progn
     (record-event `(:in ,name :args ,args))
     (let ((,return-values (multiple-value-list (progn ,@body))))
       (record-event `(:out ,name :values ,return-values))
       (values-list ,return-values))))
```

This is basically how recording works. When replaying events from a
previous run, the return values of `body` can be checked against the
recorded ones, or we may return the recorded values without even
running `body`.

In summary, we can produce selective execution traces by wrapping
code in `journaled` and use those traces for various purposes. The
Journal library is this idea taken to its logical conclusion.

<a id="x-28JOURNAL-3A-40JOURNAL-FEATURES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-FEATURES%20MGL-PAX:SECTION"></a>

## 4 Distinguishing features

##### As a logging facility

- Nested contexts and single messages

- Customizable content and format

- Human- or machine-readable output

```
#68200.234: ("some-context")
#68200.234:   Informative log message
#68200.250: => NIL
```

See [Logging][4e53] for a complete example.

##### Compared to `cl:trace`([`0`][10c3] [`1`][548d])

- Ability to handle [non-local exit][b815]s

- Customizable content and format

- Optional timestamps, internal real- and run-time

```
(FOO 2.1)
  (1+ 2.1)
  => 3.1
=E "SIMPLE-ERROR" "The assertion (INTEGERP 3.1) failed."
```

See [Tracing][e03f] for a complete example.

##### As a test framework

- White-box testing based on execution traces

- Isolation of external dependencies

- Record-and-replay testing

```
(define-file-bundle-test (test-user-registration :directory "registration")
  (let ((username (replayed ("ask-username")
                    (format t "Please type your username: ")
                    (read-line))))
    (add-user username)
    (assert (user-exists-p username))))
```

See [Testing][7682] for a complete example.

##### As a solution for persistence

- Event Sourcing: replay interactions with the external world

- Unchanged control flow

- Easy to implement history, undo

```
(defun my-resumable-autosaving-game-with-history ()
  (with-bundle (bundle)
    (play-guess-my-number)))
```

See [Persistence][37c4] for a complete example.

<a id="x-28JOURNAL-3A-40JOURNAL-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-BASICS%20MGL-PAX:SECTION"></a>

## 5 Basics

The [`journaled`][6267] macro does both recording and replaying of events,
possibly at the same time. Recording is easy: events generated by
`journaled` are simply written to a journal, which is a sequence of
events much like a file. What events are generated is described in
`journaled`. [Replay][041c] is much more involved, thus it gets its own
section. The journals used for recording and replaying are specified
by [`with-journaling`][6131] or by [`with-bundle`][12a5].

The [Journals reference][fbbb] is presented later, but for most purposes,
creating them (e.g. with [`make-in-memory-journal`][9955], [`make-file-journal`][f0e7])
and maybe querying their contents with [`list-events`][0c1b] will suffice.
Some common cases of journal creation are handled by the convenience
function [`to-journal`][e8ed].

Built on top of journals, [Bundles][260d] juggle repeated replay-and-record
cycles focussing on persistence.

<a id="x-28JOURNAL-3ATO-JOURNAL-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:TO-JOURNAL%20GENERIC-FUNCTION"></a>

- [generic-function] **to-journal** *designator*

    Return the journal designated by `designator` or
    signal an error. The default implementation
    
    - returns `designator` itself if it is of type [`journal`][5082],
    
    - returns a new [`in-memory-journal`][b668] if `designator` is `t`,
    
    - returns a new [`file-journal`][8428] if `designator` is a `pathname`([`0`][0317] [`1`][6671]).

<a id="x-28JOURNAL-3AWITH-JOURNALING-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:WITH-JOURNALING%20MGL-PAX:MACRO"></a>

- [macro] **with-journaling** *(&key record replay replay-eoj-error-p) &body body*

    Turn recording and/or replaying of events on or off for the
    duration of `body`. Both `record` and `replay` should be a [`journal`][5082]
    designator (in the sense of [`to-journal`][e8ed]) or `nil`.
    
    If `record` designates a `journal`, then events generated by enclosed
    [`journaled`][6267] [block][06a7]s are written to that journal (with exceptions, see
    the `log-record` argument of `journaled`). If `replay` designates a
    `journal`, then the generated events are matched against events from
    that journal according to the rules of [Replay][041c].
    
    A [`journal-error`][0002] is signalled if `record` is a `journal` that has been
    previously recorded to by another `with-journaling` (that is, if its
    [`journal-state`][03de] is not `:new`) or if `replay` is a `journal` that is not a
    complete recording of successful replay (i.e. its `journal-state` is
    not `:completed`). These checks are intended to catch mistakes that
    would render the new or existing records unusable for replay. When
    `with-journaling` finishes, the `record` journal is marked `:completed` or
    `:failed` in its `journal-state`.
    
    `replay-eoj-error-p` controls whether [`end-of-journal`][3cdb] is signalled when
    a new event is being matched to the replay journal from which there
    are no more events to read. If there was a [`journaling-failure`][3956] or a
    [`replay-failure`][2e9b] during execution, then `end-of-journal` is not
    signalled.
    
    If `body` completes successfully, but `replay` has unprocessed events,
    then [`replay-incomplete`][e442] is signalled.
    
    `with-journaling` for different `record` journals can be nested and run
    independently.

<a id="x-28JOURNAL-3A-40BLOCK-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@BLOCK%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **block**

    A journaled block, or simply block, is a number of forms wrapped in
    [`journaled`][6267]. When a block is executed, a [frame][7df7] is created.

<a id="x-28JOURNAL-3A-40FRAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@FRAME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **frame**

    A frame is an [`in-event`][1729], [`out-event`][637d] pair, which are created when a
    [block][06a7] is entered and left, respectively.

<a id="x-28JOURNAL-3ARECORD-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:RECORD-JOURNAL%20FUNCTION"></a>

- [function] **record-journal**

    Return the [`journal`][5082] in which events are currently being
    recorded (see [`with-journaling`][6131] and [`with-bundle`][12a5]) or `nil`.

<a id="x-28JOURNAL-3AREPLAY-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:REPLAY-JOURNAL%20FUNCTION"></a>

- [function] **replay-journal**

    Return the [`journal`][5082] from which events are currently being
    replayed (see [`with-journaling`][6131] and [`with-bundle`][12a5]) or `nil`.

<a id="x-28JOURNAL-3AJOURNALED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:JOURNALED%20MGL-PAX:MACRO"></a>

- [macro] **journaled** *(name &key (log-record :record) version args values condition insertable replay-values replay-condition) &body body*

    `journaled` generates events upon entering and leaving the dynamic
    extent of `body` (also known as the journaled [block][06a7]), which we call
    the [In-events][186b] and [Out-events][48ef]. Between generating the two events,
    `body` is typically executed normally (except for
    [Replaying the outcome][7991]).
    
    Where the generated events are written is determined by the `:record`
    argument of the enclosing [`with-journaling`][6131]. If there is no enclosing
    `with-journaling` and `log-record` is `nil`, then event recording is
    turned off and `journaled` imposes minimal overhead.
    
    - `name` can be of any type except [`null`][663f], not evaluated. For
      names, and for anything that gets written to a journal, a
      non-keyword symbol is a reasonable choice as it can be easily made
      unique. However, it also exposes the package structure, which
      might make reading stuff back more difficult. Keywords and strings
      do not have this problem.
    
    - `args` can be of any type, but is typically a list.
    
    Also see [`:log-record`][a6ac] in the [Logging][4e53] section. For a description of
    `version`, `insertable`, `replay-values` and `replay-condition`, see
    [Journaled for replay][d700].

<a id="x-28JOURNAL-3A-40IN-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@IN-EVENTS%20MGL-PAX:SECTION"></a>

### 5.1 In-events

Upon entering a [block][06a7], [`journaled`][6267] generates an [`in-event`][1729],
which conceptually opens a new [frame][7df7]. These in-events are created
from the `name`, `version` and `args` arguments of `journaled`. For example,

```
(journaled (name :version version :args args) ...)
```

creates an event like this:

```
`(:in ,name :version ,version :args ,args)
```

where `:version` and `:args` may be omitted if they are `nil`. Versions
are used for [Replay][041c].

<a id="x-28JOURNAL-3A-40OUT-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@OUT-EVENTS%20MGL-PAX:SECTION"></a>

### 5.2 Out-events

Upon leaving a [block][06a7], [`journaled`][6267] generates an [`out-event`][637d], closing
the [frame][7df7] opened by the corresponding [`in-event`][1729]. These out-events
are property lists like this:

```
(:out foo :version 1 :values (42))
```

Their `name` and `version` (`foo` and `1` in the example) are the same
as in the in-event: they come from the corresponding arguments of
`journaled`. `exit` and `outcome` are filled in differently depending on
how the block finished its execution.

<a id="x-28JOURNAL-3AEVENT-EXIT-20TYPE-29"></a>
<a id="JOURNAL:EVENT-EXIT%20TYPE"></a>

- [type] **event-exit**

    One of `:values`, `:condition`, `:error` and `:nlx`. Indicates whether a
    journaled [block][06a7]
    
    - returned normally (`:values`, see [values outcome][3ac1]),
    
    - unwound on an expected condition (`:condition`, see [condition outcome][9d9f]),
    
    - unwound on an unexpected condition (`:error`, see [error outcome][560b]),
    
    - unwound by performing a [non-local exit][b815] of some other kind
      such as a throw (`:nlx`, see [nlx outcome][68eb]).
    
    The first two are [expected outcome][4657]s, while the latter two are
    [unexpected outcome][d2c1]s.

<a id="x-28JOURNAL-3A-40VALUES-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@VALUES-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **values outcome**

    If the [`journaled`][6267] [block][06a7] returns normally, [`event-exit`][812a] is
    `:values`, and the outcome is the list of values returned:
    
    ```
    (journaled (foo) (values 7 t))
    ;; generates the out-event
    (:out foo :values (7 t))
    ```
    
    The list of return values of the block is transformed by the `values`
    argument of `journaled`, whose default is `#'identity`. Also see
    [Working with unreadable values][b354]).

<a id="x-28JOURNAL-3A-40CONDITION-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@CONDITION-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **condition outcome**

    If the [block][06a7] unwound due to a condition, and [`journaled`][6267]'s
    `condition` argument (a function whose default is `(constantly nil)`)
    returns non-`nil` when invoked on it, then [`event-exit`][812a] is
    `:condition`, and the outcome is this return value:
    
    ```
    (journaled (foo :condition (lambda (c) (prin1-to-string c)))
      (error "xxx"))
    ;; generates the out-event
    (:out foo :condition "xxx")
    ```
    
    Conditions thus recognized are those that can be considered part of
    normal execution. Just like return values, these expected conditions
    may be required to match what's in the replay journal. Furthermore,
    given a suitable `replay-condition` in `journaled`, they may be replayed
    without running the [block][06a7].

<a id="x-28JOURNAL-3A-40ERROR-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@ERROR-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **error outcome**

    If the [`journaled`][6267] [block][06a7] unwound due to a condition, but
    `journaled`'s `condition` argument returns `nil` when invoked on it, then
    [`event-exit`][812a] is `:error` and the outcome the string
    representations of the type of the condition and the condition
    itself.
    
    ```
    (journaled (foo)
      (error "xxx"))
    ;; generates this out-event:
    ;; (:out foo :error ("simple-error" "xxx"))
    ```
    
    The conversion to string is performed with [`princ`][676d] in
    [`with-standard-io-syntax`][39df]. This scheme is intended to avoid leaking
    random implementation details into the journal, which would make
    [`read`][fe58]ing it back difficult.
    
    In contrast with [condition outcome][9d9f]s, error outcomes are what the
    code is not prepared to handle or replay in a meaningful way.

<a id="x-28JOURNAL-3A-40NLX-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@NLX-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **nlx outcome**

    If the [`journaled`][6267] [block][06a7] performed a [non-local exit][b815] that
    was not due to a condition, then [`event-exit`][812a] is `:nlx` and the
    outcome is `nil`.
    
    ```
    (catch 'xxx
      (journaled (foo)
        (throw 'xxx nil)))
    ;; generates the out-event
    (:out foo :nlx nil)
    ```
    
    Note that [condition outcome][9d9f]s and [error outcome][560b]s are also due to
    [non-local exit][b815]s but are distinct from nlx outcomes.
    
    Currently, nlx outcomes are detected rather heuristically as there
    is no portable way to detect what really caused the unwinding of the
    stack.

There is a further grouping of outcomes into expected and unexpected.

<a id="x-28JOURNAL-3A-40EXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@EXPECTED-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **expected outcome**

    An [`out-event`][637d] is said to have an expected outcome if it had a
    [values outcome][3ac1] or a [condition outcome][9d9f], or equivalently, when its
    [`event-exit`][812a] is `:values` or `:condition`.

<a id="x-28JOURNAL-3A-40UNEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@UNEXPECTED-OUTCOME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **unexpected outcome**

    An [`out-event`][637d] is said to have an unexpected outcome if it had an
    [error outcome][560b] or an [nlx outcome][68eb], or equivalently, when its
    [`event-exit`][812a] is `:error` or `:nlx`.

<a id="x-28JOURNAL-3A-40WORKING-WITH-UNREADABLE-VALUES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@WORKING-WITH-UNREADABLE-VALUES%20MGL-PAX:SECTION"></a>

### 5.3 Working with unreadable values

The events recorded often need to be [readable][768f]. This is always
required with [`file-journal`][8428]s, often with [`in-memory-journal`][b668]s, but
never with [`pprint-journal`][9150]s. By choosing an appropriate identifier or
string representation of the unreadable object to journal, this is
not a problem in practice. [`journaled`][6267] provides the `values`
hook for this purpose.

With [`external-event`][0e53]s, whose outcome is replayed (see
[Replaying the outcome][7991]), we also need to be able to reverse the
transformation of `values`, and this is what the
`replay-values` argument of `journaled` is for.

Let's see a complete example.

```
(defclass user ()
  ((id :initarg :id :reader user-id)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "~S" (slot-value user 'id))))

(defvar *users* (make-hash-table))

(defun find-user (id)
  (gethash id *users*))

(defun add-user (id)
  (setf (gethash id *users*) (make-instance 'user :id id)))

(defvar *user7* (add-user 7))

(defun get-message ()
  (replayed (listen :values (values-> #'user-id)
                    :replay-values (values<- #'find-user))
    (values *user7* "hello")))

(jtrace user-id find-user get-message)

(let ((bundle (make-file-bundle "/tmp/user-example/")))
  (format t "Recording")
  (with-bundle (bundle)
    (get-message))
  (format t "~%Replaying")
  (with-bundle (bundle)
    (get-message)))
.. Recording
.. (GET-MESSAGE)
..   (USER-ID #<USER 7>)
..   => 7
.. => #<USER 7>, "hello"
.. Replaying
.. (GET-MESSAGE)
..   (FIND-USER 7)
..   => #<USER 7>, T
.. => #<USER 7>, "hello"
==> #<USER 7>
=> "hello"
```

To be able to journal the return values of `get-message`, the `user`
object must be transformed to something [readable][768f]. On the
`Recording` run, `(values-> #'user-id)` replaces the user object
with its id in the [`event-outcome`][c290] recorded, but the original user
object is returned.

When `Replaying`, the journaled [`out-event`][637d] is replayed (see
[Replaying the outcome][7991]):

```
(:OUT GET-MESSAGE :VERSION :INFINITY :VALUES (7 "hello"))
```

The user object is looked up according to `:replay-values` and is
returned along with `"hello"`.

<a id="x-28JOURNAL-3AVALUES--3E-20FUNCTION-29"></a>
<a id="JOURNAL:VALUES-%3E%20FUNCTION"></a>

- [function] **values->** *&rest fns*

    A utility to create a function suitable as the `values`
    argument of [`journaled`][6267]. The [`values`][fc69] function is called with the list
    of values returned by the [block][06a7] and returns a transformed set of
    values that may be recorded in a journal. While arbitrary
    transformations are allowed, `values->` handles the common case of
    transforming individual elements of the list independently by
    calling the functions in FN with the values of the list of the same
    position.
    
    ```
    (funcall (values-> #'1+) '(7 :something))
    => (8 :SOMETHING)
    ```
    
    Note how `#'1+` is applied only to the first element of the values
    list. The list of functions is shorter than the values list, so
    `:something` is not transformed. A value can be left explicitly
    untransformed by specifying #'[`identity`][8ae0] or `nil` as the function:
    
    ```
    (funcall (values-> #'1+ nil #'symbol-name)
             '(7 :something :another))
    => (8 :SOMETHING "ANOTHER")
    ```

<a id="x-28JOURNAL-3AVALUES-3C--20FUNCTION-29"></a>
<a id="JOURNAL:VALUES%3C-%20FUNCTION"></a>

- [function] **values\<-** *&rest fns*

    The inverse of [`values->`][7ec9], this returns a function suitable as
    the `replay-values` argument of [`journaled`][6267]. It does pretty much what
    `values->` does, but the function returned returns the transformed
    list as multiple values instead of as a list.
    
    ```
    (funcall (values<- #'1-) '(8 :something))
    => 7
    => :SOMETHING
    ```

<a id="x-28JOURNAL-3A-40JOURNAL-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-UTILITIES%20MGL-PAX:SECTION"></a>

### 5.4 Utilities

<a id="x-28JOURNAL-3ALIST-EVENTS-20FUNCTION-29"></a>
<a id="JOURNAL:LIST-EVENTS%20FUNCTION"></a>

- [function] **list-events** *&optional (journal (record-journal))*

    Return a list of all the events in the journal designated by
    `journal`. Calls [`sync-journal`][b2ff] first to make sure that all writes are
    taken into account.

<a id="x-28JOURNAL-3AEVENTS-TO-FRAMES-20FUNCTION-29"></a>
<a id="JOURNAL:EVENTS-TO-FRAMES%20FUNCTION"></a>

- [function] **events-to-frames** *events*

    Convert a flat list of events, such as those returned by [`list-events`][0c1b],
    to a nested list representing the [frame][7df7]s. Each frame is a list of
    the form `(<in-event> <nested-frames>* <out-event>?)`. Like in
    [`print-events`][f379], `events` may be a [`journal`][5082].
    
    ```
    (events-to-frames '((:in foo :args (1 2))
                        (:in bar :args (7))
                        (:leaf "leaf")
                        (:out bar :values (8))
                        (:out foo :values (2))
                        (:in foo :args (3 4))
                        (:in bar :args (8))))
    => (((:IN FOO :ARGS (1 2))
         ((:IN BAR :ARGS (7))
          (:LEAF "leaf")
          (:OUT BAR :VALUES (8)))
         (:OUT FOO :VALUES (2)))
        ((:IN FOO :ARGS (3 4)) ((:IN BAR :ARGS (8)))))
    ```
    
    Note that, as in the above example, incomplete frames (those without
    an [`out-event`][637d]) are included in the output.

<a id="x-28JOURNAL-3AEXPECTED-TYPE-20FUNCTION-29"></a>
<a id="JOURNAL:EXPECTED-TYPE%20FUNCTION"></a>

- [function] **expected-type** *type*

    Return a function suitable as the `condition` argument of [`journaled`][6267],
    which returns the type of its single argument as a string if it is
    of `type`, else `nil`.

<a id="x-28JOURNAL-3A-40PRETTY-PRINTING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@PRETTY-PRINTING%20MGL-PAX:SECTION"></a>

### 5.5 Pretty-printing

<a id="x-28JOURNAL-3APRINT-EVENTS-20FUNCTION-29"></a>
<a id="JOURNAL:PRINT-EVENTS%20FUNCTION"></a>

- [function] **print-events** *events &key stream*

    Print `events` to `stream` as lists, starting a new line for each
    event and indenting them according to their nesting structure.
    `events` may be a sequence or a [`journal`][5082], in which case [`list-events`][0c1b] is
    called on it first.
    
    ```
    (print-events '((:in log :args ("first arg" 2))
                    (:in versioned :version 1 :args (3))
                    (:out versioned :version 1 :values (42 t))
                    (:out log :condition "a :CONDITION outcome")
                    (:in log-2)
                    (:out log-2 :nlx nil)
                    (:in external :version :infinity)
                    (:out external :version :infinity
                     :error ("ERROR" "an :ERROR outcome"))))
    ..
    .. (:IN LOG :ARGS ("first arg" 2))
    ..   (:IN VERSIONED :VERSION 1 :ARGS (3))
    ..   (:OUT VERSIONED :VERSION 1 :VALUES (42 T))
    .. (:OUT LOG :CONDITION "a :CONDITION outcome")
    .. (:IN LOG-2)
    .. (:OUT LOG-2 :NLX NIL)
    .. (:IN EXTERNAL :VERSION :INFINITY)
    .. (:OUT EXTERNAL :VERSION :INFINITY :ERROR ("ERROR" "an :ERROR outcome"))
    => ; No value
    ```

<a id="x-28JOURNAL-3APPRINT-EVENTS-20FUNCTION-29"></a>
<a id="JOURNAL:PPRINT-EVENTS%20FUNCTION"></a>

- [function] **pprint-events** *events &key stream (prettifier 'prettify-event)*

    Like [`print-events`][f379], but produces terser, more human readable
    output.
    
    ```
    (pprint-events '((:in log :args ("first arg" 2))
                     (:in versioned :version 1 :args (3))
                     (:leaf "This is a leaf, not a frame.")
                     (:out versioned :version 1 :values (42 t))
                     (:out log :condition "a :CONDITION outcome")
                     (:in log-2)
                     (:out log-2 :nlx nil)
                     (:in external :version :infinity)
                     (:out external :version :infinity
                      :error ("ERROR" "an :ERROR outcome"))))
    ..
    .. (LOG "first arg" 2)
    ..   (VERSIONED 3) v1
    ..     This is a leaf, not a frame.
    ..   => 42, T
    .. =C "a :CONDITION outcome"
    .. (LOG-2)
    .. =X
    .. (EXTERNAL) ext
    .. =E "ERROR" "an :ERROR outcome"
    => ; No value
    ```
    
    The function given as the `prettifier` argument formats individual
    events. The above output was produced with [`prettify-event`][11b7]. For a
    description of `prettifier`'s arguments see `prettify-event`.

<a id="x-28JOURNAL-3APRETTIFY-EVENT-20FUNCTION-29"></a>
<a id="JOURNAL:PRETTIFY-EVENT%20FUNCTION"></a>

- [function] **prettify-event** *event depth stream*

    Write `event` to `stream` in a somewhat human-friendly format.
    This is the function [`pprint-journal`][9150], [`pprint-events`][5833], and [Tracing][e03f] use
    by default. In addition to the basic example in `pprint-events`,
    [decoration][1d11] on events is printed before normal, indented output like
    this:
    
    ```
    (pprint-events '((:leaf "About to sleep" :time "19:57:00" :function "FOO")))
    ..
    .. 19:57:00 FOO: About to sleep
    ```
    
    `depth` is the nesting level of the `event`. Top-level events have depth
    0. `prettify-event` prints indents the output after printing the
    decorations by 2 spaces per depth.

Instead of collecting events and then printing them, events can
be pretty-printed to a stream as they generated. This is
accomplished with [Pretty-printing journals][1496], discussed in detail later, in
the following way:

```
(let ((journal (make-pprint-journal)))
  (with-journaling (:record journal)
    (journaled (foo) "Hello")))
..
.. (FOO)
.. => "Hello"
```

Note that [Pretty-printing journals][1496] are not tied to [`with-journaling`][6131] and are
most often used for [Logging][4e53] and [Tracing][e03f].

<a id="x-28JOURNAL-3A-40JOURNAL-ERROR-HANDLING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-ERROR-HANDLING%20MGL-PAX:SECTION"></a>

### 5.6 Error handling

<a id="x-28JOURNAL-3AJOURNALING-FAILURE-20CONDITION-29"></a>
<a id="JOURNAL:JOURNALING-FAILURE%20CONDITION"></a>

- [condition] **journaling-failure** *[serious-condition][af00]*

    Signalled during the dynamic extent of
    [`with-journaling`][6131] when an error threatens to leave the journaling
    mechanism in an inconsistent state. These include I/O errors
    encountered reading or writing journals by `with-journaling`,
    [`journaled`][6267], [`logged`][23c4], [`with-replay-filter`][0cce], [`sync-journal`][b2ff], and also
    [`storage-condition`][ecf9]s, assertion failures, errors calling `journaled`'s
    `values` and `condition` function arguments.
    Crucially, this does not apply to [non-local exit][b815]s from other
    code, such as `journaled` [block][06a7]s, whose error handling is largely
    unaltered (see [Out-events][48ef] and [Replay failures][2933]).
    
    In general, any [non-local exit][b815] from critical parts of the
    code is turned into a `journaling-failure` to protect the integrity of
    the [`record-journal`][3b63]. The condition that caused the unwinding is in
    [`journaling-failure-embedded-condition`][9f90], or `nil` if it was a pure
    [non-local exit][b815] like [`throw`][e760]. This is a [`serious-condition`][af00], not
    to be handled within `with-journaling`.
    
    After a `journaling-failure`, the journaling mechanism cannot be
    trusted anymore. The [`replay-journal`][838b] might have failed a read and be
    out-of-sync. The `record-journal` may have missing events (or even
    half-written events with [`file-journal`][8428]s without `sync`, see
    [Synchronization strategies][f532]), and further writes to it would risk
    replayability, which is equivalent to database corruption. Thus,
    upon signalling `journaling-failure`, [`journal-state`][03de] is set to
    
    - `:completed` if the journal is in state `:recording` or `:logging` and
      the transition to `:recording` was reflected in storage,
    
    - else it is set to `:failed`.
    
    After a `journaling-failure`, any further attempt within the affected
    `with-journaling` to use the critical machinery mentioned
    above (`journaled`, `logged`, etc) resignals the same journal failure
    condition. As a consequence, the record journal cannot be changed,
    and the only way to recover is to leave `with-journaling`. This does
    not affect processing in other threads, which by design cannot write
    to the record journal.
    
    Note that in contrast with `journaling-failure` and [`replay-failure`][2e9b],
    which necessitate leaving `with-journaling` to recover from, the other
    conditions – [`journal-error`][0002], and [`streamlet-error`][e6b2] – are subclasses of
    [`error`][d162] as their handling need not be so heavy-handed.

<a id="x-28JOURNAL-3AJOURNALING-FAILURE-EMBEDDED-CONDITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNALING-FAILURE-29-29"></a>
<a id="JOURNAL:JOURNALING-FAILURE-EMBEDDED-CONDITION%20%28MGL-PAX:READER%20JOURNAL:JOURNALING-FAILURE%29"></a>

- [reader] **journaling-failure-embedded-condition** *[journaling-failure][3956] (:embedded-condition)*

<a id="x-28JOURNAL-3ARECORD-UNEXPECTED-OUTCOME-20CONDITION-29"></a>
<a id="JOURNAL:RECORD-UNEXPECTED-OUTCOME%20CONDITION"></a>

- [condition] **record-unexpected-outcome**

    Signalled (with [`signal`][8f49]: this is not an
    [`error`][d162]) by [`journaled`][6267] when a [`versioned-event`][4c2b] or an
    [`external-event`][0e53] had an UNEXPECTED-OUTCOME while in [`journal-state`][03de]
    `:recording`. Upon signalling this condition, `journal-state` is set to
    `:logging`, thus no more events can be recorded that will affect
    replay of the journal being recorded. The event that triggered this
    condition is recorded in state `:logging`, with its version
    downgraded. Since [Replay][041c] (except [invoked][4212]) is built on the
    assumption that control flow is deterministic, an unexpected outcome
    is significant because it makes this assumption to hold unlikely.
    
    Also see [`replay-unexpected-outcome`][6699].

<a id="x-28JOURNAL-3ADATA-EVENT-LOSSAGE-20CONDITION-29"></a>
<a id="JOURNAL:DATA-EVENT-LOSSAGE%20CONDITION"></a>

- [condition] **data-event-lossage** *[journaling-failure][3956]*

    Signalled when a [data event][c015] is about to be recorded
    in [`journal-state`][03de] `:mismatched` or `:logging`. Since the data event will
    not be replayed that constitutes data loss.

<a id="x-28JOURNAL-3AJOURNAL-ERROR-20CONDITION-29"></a>
<a id="JOURNAL:JOURNAL-ERROR%20CONDITION"></a>

- [condition] **journal-error** *[error][d162]*

    Signalled by [`with-journaling`][6131], [`with-bundle`][12a5] and by
    [`:log-record`][a6ac]. It is also signalled by the low-level streamlet
    interface (see [Streamlets reference][f4d5]).

<a id="x-28JOURNAL-3AEND-OF-JOURNAL-20CONDITION-29"></a>
<a id="JOURNAL:END-OF-JOURNAL%20CONDITION"></a>

- [condition] **end-of-journal** *[journal-error][0002]*

    This might be signalled by the replay mechanism if
    [`with-journaling`][6131]'s `replay-eoj-error-p` is true. Unlike
    [`replay-failure`][2e9b]s, this does not affect [`journal-state`][03de] of
    [`record-journal`][3b63]. At a lower level, it is signalled by [`read-event`][adcf] upon
    reading past the end of the [`journal`][5082] if `eoj-error-p`.

<a id="x-28JOURNAL-3A-40LOGGING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@LOGGING%20MGL-PAX:SECTION"></a>

## 6 Logging

Before we get into the details, here is a self-contained example
that demonstrates typical use.

```
(defvar *communication-log* nil)
(defvar *logic-log* nil)
(defvar *logic-log-level* 0)

(defun call-with-connection (port fn)
  (framed (call-with-connection :log-record *communication-log*
                                :args `(,port))
    (funcall fn)))

(defun fetch-data (key)
  (let ((value 42))
    (logged ((and (<= 1 *logic-log-level*) *logic-log*))
      "The value of ~S is ~S." key value)
    value))

(defun init-logging (&key (logic-log-level 1))
  (let* ((stream (open "/tmp/xxx.log"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append))
         (journal (make-pprint-journal
                   :stream (make-broadcast-stream
                            (make-synonym-stream '*standard-output*)
                            stream))))
    (setq *communication-log* journal)
    (setq *logic-log* journal)
    (setq *logic-log-level* logic-log-level)))

(init-logging)

(call-with-connection 8080 (lambda () (fetch-data :foo)))
..
.. (CALL-WITH-CONNECTION 8080)
..   The value of :FOO is 42.
.. => 42
=> 42

(setq *logic-log-level* 0)
(call-with-connection 8080 (lambda () (fetch-data :foo)))
..
.. (CALL-WITH-CONNECTION 8080)
.. => 42
=> 42

(ignore-errors
  (call-with-connection 8080 (lambda () (error "Something unexpected."))))
..
.. (CALL-WITH-CONNECTION 8080)
.. =E "SIMPLE-ERROR" "Something unexpected."
```

##### Default to muffling

Imagine a utility library called glib.

```
(defvar *glib-log* nil)
(defvar *patience* 1)

(defun sl33p (seconds)
  (logged (*glib-log*) "Sleeping for ~As." seconds)
  (sleep (* *patience* seconds)))
```

Glib follows the recommendation to have a special variable globally
bound to `nil` by default. The value of `*glib-log*` is the journal to
which glib log messages will be routed. Since it's `nil`, the log
messages are muffled, and to record any log message, we need to
change its value.

##### Routing logs to a journal

Let's send the logs to a [`pprint-journal`][9150]:

```
(setq *glib-log* (make-pprint-journal
                  :log-decorator (make-log-decorator :time t)))
(sl33p 0.01)
..
.. 2020-08-31T12:45:23.827172+02:00: Sleeping for 0.01s.
```

That's a bit too wordy. For this tutorial, let's stick to less
verbose output:

```
(setq *glib-log* (make-pprint-journal))
(sl33p 0.01)
..
.. Sleeping for 0.01s.
```

To log to a file:

```
(setq *glib-log* (make-pprint-journal
                  :stream (open "/tmp/glib.log"
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :append)))
```

##### Capturing logs in [`with-journaling`][6131]'s [`record-journal`][3b63]

If we were recording a journal for replay and wanted to include glib
logs in the journal, we would do something like this:

```
(with-journaling (:record t)
  (let ((*glib-log* :record))
    (sl33p 0.01)
    (journaled (non-glib-stuff :version 1)))
  (list-events))
=> ((:LEAF "Sleeping for 0.01s.")
    (:IN NON-GLIB-STUFF :VERSION 1)
    (:OUT NON-GLIB-STUFF :VERSION 1 :VALUES (NIL)))
```

We could even `(setq *glib-log* :record)` to make it so that glib
messages are included by default in the `record-journal`. In this
example, the special `*glib-log*` acts like a log category for all
the log messages of the glib library (currently one).

##### Rerouting a category

Next, we route `*glib-log*` to wherever `*app-log*` is pointing by
binding `*glib-log*` *to the symbol* `*app-log*` (see [`:log-record`][a6ac]).

```
(defvar *app-log* nil)

(let ((*glib-log* '*app-log*))
  (setq *app-log* nil)
  (logged (*glib-log*) "This is not written anywhere.")
  (setq *app-log* (make-pprint-journal :pretty nil))
  (sl33p 0.01))
..
.. (:LEAF "Sleeping for 0.01s.")
```

Note how pretty-printing was turned off, and we see the [`leaf-event`][5cd1]
generated by [`logged`][23c4] in its raw plist form.

##### Conditional routing

Finally, to make routing decisions conditional we need to change
`sl33p`:

```
(defvar *glib-log-level* 1)

(defun sl33p (seconds)
  (logged ((and (<= 2 *glib-log-level*) *glib-log*))
          "Sleeping for ~As." (* *patience* seconds))
  (sleep seconds))

;;; Check that it all works:
(let ((*glib-log-level* 1)
      (*glib-log* (make-pprint-journal)))
  (format t "~%With log-level ~A" *glib-log-level*)
  (sl33p 0.01)
  (setq *glib-log-level* 2)
  (format t "~%With log-level ~A" *glib-log-level*)
  (sl33p 0.01))
..
.. With log-level 1
.. With log-level 2
.. Sleeping for 0.01s.
```

##### Nested log contexts

`logged` is for single messages. [`journaled`][6267], or in this example [`framed`][5d05],
can provide nested context:

```
(defun callv (var value symbol &rest args)
  "Call SYMBOL-FUNCTION of SYMBOL with VAR dynamically bound to VALUE."
  (framed ("glib:callv" :log-record *glib-log*
                        :args `(,var ,value ,symbol ,@args))
    (progv (list var) (list value)
      (apply (symbol-function symbol) args))))

(callv '*print-base* 2 'print 10)
..
.. ("glib:callv" *PRINT-BASE* 2 PRINT 10)
.. 1010 
.. => 10
=> 10

(let ((*glib-log-level* 2))
  (callv '*patience* 7 'sl33p 0.01))
..
.. ("glib:callv" *PATIENCE* 7 SL33P 0.01)
..   Sleeping for 0.07s.
.. => NIL
```


<a id="x-28JOURNAL-3A-40CUSTOMIZING-LOGS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@CUSTOMIZING-LOGS%20MGL-PAX:SECTION"></a>

### 6.1 Customizing logs

Customizing the output format is possible if we don't necessarily
expect to be able to read the logs back programmatically. There is
an example in [Tracing][e03f], which is built on [Pretty-printing journals][1496].

Here, we discuss how to make logs more informative.

<a id="x-28JOURNAL-3A-40DECORATION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@DECORATION%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **decoration**

    [`journal-log-decorator`][8a5b] adds additional data to [`log-event`][51ce]s as they
    are written to the journal. This data is called decoration, and it is
    to capture the context in which the event was triggered. See
    [`make-log-decorator`][e33e] for a typical example. Decorations, since they
    can be on `log-event`s only, do not affect [Replay][041c]. Decorations are
    most often used with [Pretty-printing][47a7].

<a id="x-28JOURNAL-3AJOURNAL-LOG-DECORATOR-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3AJOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-LOG-DECORATOR%20%28MGL-PAX:ACCESSOR%20JOURNAL:JOURNAL%29"></a>

- [accessor] **journal-log-decorator** *[journal][5082] (:log-decorator = nil)*

    If non-`nil`, this is a function to add [decoration][1d11]
    to [`log-event`][51ce]s before they are written to a journal. The only
    allowed transformation is to *append* a plist to the event, which
    is a plist itself. The keys can be anything.

<a id="x-28JOURNAL-3AMAKE-LOG-DECORATOR-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-LOG-DECORATOR%20FUNCTION"></a>

- [function] **make-log-decorator** *&key time real-time run-time thread depth out-name*

    Return a function suitable as [`journal-log-decorator`][8a5b] that may add
    a string timestamp, the internal real-time or run-time (both in
    seconds), the name of the thread, to events, which will be handled
    by [`prettify-event`][11b7]. If `depth`, then `prettify-event` will print the
    nesting level of the event being printed. If `out-name`, the
    `prettify-event` will print the name of [Out-events][48ef].
    
    All arguments are [boolean-valued symbol][62678]s.
    
    ```
    (funcall (make-log-decorator :depth t :out-name t :thread t
                                 :time t :real-time t :run-time t)
             (make-leaf-event :foo))
    => (:LEAF :FOO :DEPTH T :OUT-NAME T :THREAD "worker"
                   :TIME "2023-05-26T12:27:44.172614+01:00"
                   :REAL-TIME 2531.3254 :RUN-TIME 28.972797)
    ```

<a id="x-28JOURNAL-3A-40LOG-RECORD-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@LOG-RECORD%20MGL-PAX:SECTION"></a>

### 6.2 `:log-record`

[`with-journaling`][6131] and [`with-bundle`][12a5] control replaying and recording
within their dynamic extent, which is rather a necessity because
[Replay][041c] needs to read the events in the same order as the [`journaled`][6267]
[block][06a7]s are being executed. However, [`log-event`][51ce]s do not affect
replay, so we can allow more flexibility in routing them.

The `log-record` argument of `journaled` and [`logged`][23c4] controls where
`log-event`s are written both within `with-journaling` and without. The
algorithm to determine the target journal is this:

1. If `log-record` is `:record`, then the [`record-journal`][3b63] is returned.

2. If `log-record` is `nil`, then it is returned.

3. If `log-record` is a [`journal`][5082], then it is returned.

4. If `log-record` is a symbol (other than `nil`), then the [`symbol-value`][cee6]
   of that symbol is assigned to `log-record`, and we go to step 1.

If the return value is `nil`, then the event will not be written
anywhere, else it is written to the journal returned.

This is reminiscent of [`synonym-stream`][1033]s, also in that it is possible
end up in cycles in the resolution. For this reason, the algorithm
stop with a [`journal-error`][0002] after 100 iterations.

##### Interactions

Events may be written to `log-record` even without an enclosing
`with-journaling`, and it does not affect the [`journal-state`][03de]. However,
it is a `journal-error` to write to a `:completed` journal (see
`journal-state`).

When multiple threads log to the same journal, it is guaranteed that
individual events are written atomically, but frames from different
threads do not necessarily nest. To keep the log informative, the
name of thread may be added to the events as [decoration][1d11].

Also, see notes on thread [Safety][7bf3].

<a id="x-28JOURNAL-3A-40LOGGING-WITH-LEAVES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@LOGGING-WITH-LEAVES%20MGL-PAX:SECTION"></a>

### 6.3 Logging with `leaf-event`s

<a id="x-28JOURNAL-3ALOGGED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:LOGGED%20MGL-PAX:MACRO"></a>

- [macro] **logged** *(&optional (log-record :record)) format-control &rest format-args*

    `logged` creates a single [`leaf-event`][5cd1], whose name is the string
    constructed by [`format`][ad78]. For example:
    
    ```
    (with-journaling (:record t)
      (logged () "Hello, ~A." "world")
      (list-events))
    => ((:LEAF "Hello, world."))
    ```
    
    `leaf-event`s are [`log-event`][51ce]s with no separate in- and out-events. They
    have an [`event-name`][9f84] and no other properties. Use `logged` for
    point-in-time textual log messages, and [`journaled`][6267] with `version`
    `nil` (i.e. [`framed`][5d05]) to provide context.
    
    Also, see [`:log-record`][a6ac].

<a id="x-28JOURNAL-3A-40TRACING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@TRACING%20MGL-PAX:SECTION"></a>

## 7 Tracing

[`jtrace`][18be] behaves similarly to `cl:trace`([`0`][10c3] [`1`][548d]) but deals with
[non-local exit][b815]s gracefully.

##### Basic tracing

```common-lisp
(defun foo (x)
  (sleep 0.12)
  (1+ x))

(defun bar (x)
  (foo (+ x 2))
  (error "xxx"))

(jtrace foo bar)

(ignore-errors (bar 1))
..
.. 0: (BAR 1)
..   1: (FOO 3)
..   1: FOO => 4
.. 0: BAR =E "SIMPLE-ERROR" "xxx"
```

##### Log-like output

It can also include the name of the originating thread and
timestamps in the output:

```
(let ((*trace-thread* t)
      (*trace-time* t)
      (*trace-depth* nil)
      (*trace-out-name* nil))
  (ignore-errors (bar 1)))
..
.. 2020-09-02T19:58:19.415204+02:00 worker: (BAR 1)
.. 2020-09-02T19:58:19.415547+02:00 worker:   (FOO 3)
.. 2020-09-02T19:58:19.535766+02:00 worker:   => 4
.. 2020-09-02T19:58:19.535908+02:00 worker: =E "SIMPLE-ERROR" "xxx"
```

##### Profiler-like output

```
(let ((*trace-real-time* t)
      (*trace-run-time* t)
      (*trace-depth* nil)
      (*trace-out-name* nil))
  (ignore-errors (bar 1)))
..
.. #16735.736 !68.368: (BAR 1)
.. #16735.736 !68.369:   (FOO 3)
.. #16735.857 !68.369:   => 4
.. #16735.857 !68.369: =E "SIMPLE-ERROR" "xxx"
```

##### Customizing the content and the format

If these options are insufficient, the content and the format of the
trace can be customized:

```
(let ((*trace-journal*
       (make-pprint-journal :pretty '*trace-pretty*
                     :prettifier (lambda (event depth stream)
                                   (format stream "~%Depth: ~A, event: ~S"
                                           depth event))
                     :stream (make-synonym-stream '*error-output*)
                     :log-decorator (lambda (event)
                                      (append event '(:custom 7))))))
  (ignore-errors (bar 1)))
..
.. Depth: 0, event: (:IN BAR :ARGS (1) :CUSTOM 7)
.. Depth: 1, event: (:IN FOO :ARGS (3) :CUSTOM 7)
.. Depth: 1, event: (:OUT FOO :VALUES (4) :CUSTOM 7)
.. Depth: 0, event: (:OUT BAR :ERROR ("SIMPLE-ERROR" "xxx") :CUSTOM 7)
```

In the above, [`*trace-journal*`][4118] was bound locally to keep the example
from wrecking the global default, but the same effect could be
achieved by [`setf`][a138]ing [`pprint-journal-prettifier`][853d],
[`pprint-journal-stream`][34a8] and [`journal-log-decorator`][8a5b].

<a id="x-28JOURNAL-3AJTRACE-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:JTRACE%20MGL-PAX:MACRO"></a>

- [macro] **jtrace** *&rest names*

    Like `cl:trace`([`0`][10c3] [`1`][548d]), `jtrace` takes a list of symbols. When functions
    denoted by those `names` are invoked, their names, arguments and
    outcomes are printed in human readable form to [`*trace-output*`][2243]. These
    values may not be [readable][768f], `jtrace` does not care.
    
    The format of the output is the same as that of [`pprint-events`][5833].
    Behind the scenes, `jtrace` encapsulates the global functions with
    `names` in wrapper that behaves as if `foo` in the example above was
    defined like this:
    
    ```
    (defun foo (x)
      (framed (foo :args `(,x) :log-record *trace-journal*)
        (1+ x)))
    ```
    
    If `jtrace` is invoked with no arguments, it returns the list of
    symbols currently traced.
    
    On Lisps other than SBCL, where a function encapsulation facility is
    not available or it is not used by Journal, `jtrace` simply sets
    [`symbol-function`][9048]. This solution loses the tracing encapsulation when
    the function is recompiled. On these platforms, `(jtrace)` also
    retraces all functions that should be traced but aren't.
    
    The main advantage of `jtrace` over `cl:trace` is the ability to trace
    errors, not just normal return values. As it is built on [`journaled`][6267],
    it can also detect – somewhat heuristically – [`throw`][e760]s and similar.

<a id="x-28JOURNAL-3AJUNTRACE-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:JUNTRACE%20MGL-PAX:MACRO"></a>

- [macro] **juntrace** *&rest names*

    Like `cl:untrace`([`0`][a370] [`1`][4823]), `juntrace` makes it so that the global functions
    denoted by the symbols `names` are no longer traced by [`jtrace`][18be]. When
    invoked with no arguments, it untraces all traced functions.

<a id="x-28JOURNAL-3A-2ATRACE-PRETTY-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-PRETTY*%20VARIABLE"></a>

- [variable] **\*trace-pretty\*** *t*

    If `*trace-pretty*` is true, then [`jtrace`][18be] produces output like
    [`pprint-events`][5833], else it's like [`print-events`][f379].

<a id="x-28JOURNAL-3A-2ATRACE-DEPTH-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-DEPTH*%20VARIABLE"></a>

- [variable] **\*trace-depth\*** *t*

    Controls whether to decorate the trace with the depth of event.
    See [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-OUT-NAME-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-OUT-NAME*%20VARIABLE"></a>

- [variable] **\*trace-out-name\*** *t*

    Controls whether trace should print the [`event-name`][9f84] of [Out-events][48ef],
    which is redundant with the `event-name` of the corresponding
    [In-events][186b]. See [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-THREAD-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-THREAD*%20VARIABLE"></a>

- [variable] **\*trace-thread\*** *nil*

    Controls whether to decorate the trace with the name of the
    originating thread. See [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-TIME-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-TIME*%20VARIABLE"></a>

- [variable] **\*trace-time\*** *nil*

    Controls whether to decorate the trace with a timestamp. See
    [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-REAL-TIME-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-REAL-TIME*%20VARIABLE"></a>

- [variable] **\*trace-real-time\*** *nil*

    Controls whether to decorate the trace with the internal real-time.
    See [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-RUN-TIME-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-RUN-TIME*%20VARIABLE"></a>

- [variable] **\*trace-run-time\*** *nil*

    Controls whether to decorate the trace with the internal run-time.
    See [`make-log-decorator`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-JOURNAL-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*TRACE-JOURNAL*%20VARIABLE"></a>

- [variable] **\*trace-journal\*** *\#\<pprint-journal :new 1>*

    The [`journal`][5082] where [`jtrace`][18be] writes [`log-event`][51ce]s. By default, it is a
    [`pprint-journal`][9150] that sets up a [`synonym-stream`][1033] to [`*trace-output*`][2243] and
    sends its output there. It pays attention to [`*trace-pretty*`][825c], and its
    log decorator is affected by [`*trace-time*`][2765] and [`*trace-thread*`][9a42].
    However, by changing [`journal-log-decorator`][8a5b] and
    [`pprint-journal-prettifier`][853d], content and output can be customized.

<a id="x-28JOURNAL-3A-40JOURNAL-SLIME-INTEGRATION-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL-SLIME-INTEGRATION%20MGL-PAX:SECTION"></a>

### 7.1 Slime integration

[Slime](https://common-lisp.net/project/slime/), by default,
binds `C-c C-t` to toggling `cl:trace`([`0`][10c3] [`1`][548d]). To integrate [`jtrace`][18be] into
Slime, load `src/mgl-jrn.el` into Emacs.

- If you installed Journal with Quicklisp, the location of
  `mgl-jrn.el` may change with updates, and you may want to copy the
  current version to a stable location:

        (journal:install-journal-elisp "~/quicklisp/")

Then, assuming the Elisp file is in the quicklisp directory, add
this to your `.emacs`:

```elisp
(load "~/quicklisp/mgl-jrn.el")
```

Alternatively, with `use-package`:

```elisp
(use-package mgl-jrn :load-path "~/quicklisp/"
  :after slime
  :demand t)
```

Since `jtrace` lacks some features of `cl:trace`, most notably that of
tracing non-global functions, it is assigned a separate binding,
`C-c C-j`.

<a id="x-28JOURNAL-3AINSTALL-JOURNAL-ELISP-20FUNCTION-29"></a>
<a id="JOURNAL:INSTALL-JOURNAL-ELISP%20FUNCTION"></a>

- [function] **install-journal-elisp** *target-dir*

    Copy `mgl-jrn.el` distributed with this package to `target-dir`.

<a id="x-28JOURNAL-3A-40REPLAY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@REPLAY%20MGL-PAX:SECTION"></a>

## 8 Replay

During replay, code is executed normally with special rules for
[block][06a7]s. There are two modes for dealing with blocks: replaying the
code and replaying the outcome. When code is replayed, upon entering
and leaving a block, the events generated are matched to events read
from the journal being replayed. If the events don't match,
[`replay-failure`][2e9b] is signalled, which marks the record journal as having
failed the replay. This is intended to make sure that the state of
the program during the replay matches the state at the time of
recording. In the other mode, when the outcome is replayed, a block
may not be executed at all, but its recorded outcome is
reproduced (i.e. the recorded return values are simply returned).

Replay can be only be initiated with [`with-journaling`][6131] (or its close
kin [`with-bundle`][12a5]). After the per-event processing described below,
when `with-journaling` finishes, it might signal [`replay-incomplete`][e442] if
there are unprocessed non-log events left in the replay journal.

Replay is deemed successful or failed depending on whether all
events are replayed from the replay journal without a
`replay-failure`. A journal that records events from a successful
replay can be used in place of the journal that was replayed, and so
on. The logic of replacing journals with their successful replays is
automated by [Bundles][260d]. `with-journaling` does not allow replay from
journals that were failed replays themselves. The mechanism, in
terms of which tracking success and failure of replays is
implemented, revolves around [`journal-state`][03de] and
[`event-version`][9ed3]s, which we discuss next.

<a id="x-28JOURNAL-3AJOURNAL-STATE-20TYPE-29"></a>
<a id="JOURNAL:JOURNAL-STATE%20TYPE"></a>

- [type] **journal-state**

    [`journal`][5082]'s state with respect to replay is updated during
    [`with-journaling`][6131]. The possible states are:
    
    - **`:new`**: This journal was just created but never recorded to.
    
    - **`:replaying`**: Replaying events has started, some events may have
      been replayed successfully, but there are more non-log events to
      replay.
    
    - **`:mismatched`**: There was a [`replay-failure`][2e9b]. In this state,
      [`versioned-event`][4c2b]s generated are downgraded to [`log-event`][51ce]s,
      [`external-event`][0e53]s and [invoked][4212] trigger [`data-event-lossage`][4f2b].
    
    - **`:recording`**: All events from the replay journal were
      successfully replayed, and now new events are being recorded
      without being matched to the replay journal.
    
    - **`:logging`**: There was a [`record-unexpected-outcome`][8548]. In this
      state, `versioned-event`s generated are downgraded to `log-event`s,
      `external-event`s and [invoked][4212] trigger `data-event-lossage`.
    
    - **`:failed`**: The journal is to be discarded. It encountered a
      [`journaling-failure`][3956] or a `replay-failure` without completing the
      replay and reaching `:recording`.
    
    - **`:completed`**: All events were successfully replayed and
      `with-journaling` finished or a `journaling-failure` occurred while
      `:recording` or `:logging`.
    
    The state transitions are:
    
        :NEW                -> :REPLAYING  (on entering WITH-JOURNALING)
        :REPLAYING          -> :MISMATCHED (on REPLAY-FAILURE)
        :REPLAYING          -> :FAILED     (on REPLAY-INCOMPLETE)
        :REPLAYING          -> :FAILED     (on JOURNALING-FAILURE)
        :REPLAYING          -> :RECORDING  (on successfully replaying all events)
        :MISMATCHED         -> :FAILED     (on leaving WITH-JOURNALING)
        :RECORDING          -> :LOGGING    (on RECORD-UNEXPECTED-OUTCOME)
        :RECORDING/:LOGGING -> :COMPLETED  (on leaving WITH-JOURNALING)
        :RECORDING/:LOGGING -> :COMPLETED  (on JOURNALING-FAILURE)
    
    `:new` is the starting state. It is a [`journal-error`][0002] to attempt to
    write to journals in `:completed`. Note that once in `:recording`, the
    only possible terminal state is `:completed`.

<a id="x-28JOURNAL-3A-40JOURNALED-FOR-REPLAY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNALED-FOR-REPLAY%20MGL-PAX:SECTION"></a>

### 8.1 Journaled for replay

The following arguments of [`journaled`][6267] control behaviour under replay.

- `version`: see [`event-version`][9ed3] below.

- `insertable` controls whether [`versioned-event`][4c2b]s and [`external-event`][0e53]s
  may be replayed with the *insert* replay strategy (see
  [The replay strategy][a8a7]). Does not affect [`log-event`][51ce]s, which are
  always \_insert\_ed. Note that inserting `external-event`s while
  `:replaying` is often not meaningful (e.g. asking the user for input
  may lead to a [`replay-failure`][2e9b]). See [`peek-replay-event`][eddd] for an
  example on how to properly insert these kinds of `external-event`s.

- `replay-values`, a function or `nil`, may be called with [`event-outcome`][c290]
  when replaying and `:version` `:infinity`. `nil` is equivalent to
  [`values-list`][dbd4]. See [`values<-`][f17d] for an example.

- `replay-condition`, a function or `nil`, may be called with
  `event-outcome` (the return value of the function provided as
  `:condition`) when replaying and `:version` is `:infinity`. `nil` is
  equivalent to the [`error`][35ba] function. Replaying conditions is
  cumbersome and best avoided.


<a id="x-28JOURNAL-3A-2AFORCE-INSERTABLE-2A-20VARIABLE-29"></a>
<a id="JOURNAL:*FORCE-INSERTABLE*%20VARIABLE"></a>

- [variable] **\*force-insertable\*** *nil*

    The default value of the `insertable` argument of [`journaled`][6267] for
    [`versioned-event`][4c2b]s. Binding this to `t` allows en-masse structural
    upgrades in combination with [`with-replay-filter`][0cce]. Does not affect
    [`external-event`][0e53]s. See [Upgrades and replay][750a].

<a id="x-28JOURNAL-3AEVENT-VERSION-20TYPE-29"></a>
<a id="JOURNAL:EVENT-VERSION%20TYPE"></a>

- [type] **event-version**

    An event's version is either `nil`, a positive [`fixnum`][3cde], or `:infinity`,
    which correspond to [`log-event`][51ce]s, [`versioned-event`][4c2b]s, and
    [`external-event`][0e53]s, respectively, and have an increasingly strict
    behaviour with regards to [Replay][041c]. All [`event`][a394]s have versions. The
    versions of the in- and out-events belonging to the same [frame][7df7] are
    the same.

<a id="x-28JOURNAL-3ALOG-EVENT-20TYPE-29"></a>
<a id="JOURNAL:LOG-EVENT%20TYPE"></a>

- [type] **log-event**

    Events with [`event-version`][9ed3] `nil` called log events. During [Replay][041c],
    they are never matched to events from the replay journal, and log
    events in the replay do not affect events being recorded either.
    These properties allow log events to be recorded in arbitrary
    journals with [`journaled`][6267]'s `log-record` argument. The convenience macro
    [`framed`][5d05] is creating frames of log-events, while the [`logged`][23c4] generates
    a log-event that's a [`leaf-event`][5cd1].

<a id="x-28JOURNAL-3AVERSIONED-EVENT-20TYPE-29"></a>
<a id="JOURNAL:VERSIONED-EVENT%20TYPE"></a>

- [type] **versioned-event**

    Events with a positive integer [`event-version`][9ed3] are called
    versioned events. In [Replay][041c], they undergo consistency checks unlike
    [`log-event`][51ce]s, but the rules for them are less strict than for
    [`external-event`][0e53]s. In particular, higher versions are always
    considered compatible with lower versions, they become an *upgrade*
    in terms of the [The replay strategy][a8a7], and versioned events can be
    inserted into the record without a corresponding [replay event][6525] with
    [`journaled`][6267]'s `insertable`.
    
    If a `versioned-event` has an [unexpected outcome][d2c1],
    [`record-unexpected-outcome`][8548] is signalled.

<a id="x-28JOURNAL-3AEXTERNAL-EVENT-20TYPE-29"></a>
<a id="JOURNAL:EXTERNAL-EVENT%20TYPE"></a>

- [type] **external-event**

    Events with [`event-version`][9ed3] `:infinity` are called external events.
    They are like [`versioned-event`][4c2b]s whose version was bumped all the way
    to infinity, which rules out easy, non-matching upgrades. Also, they
    are never inserted to the record without a matching replay
    event (see [The replay strategy][a8a7]).
    
    In return for these restrictions, external events can be replayed
    without running the corresponding [block][06a7] (see
    [Replaying the outcome][7991]). This allows their out-event variety, called
    [data event][c015]s, to be non-deterministic. Data events play a crucial
    role in [Persistence][37c4].
    
    If an `external-event` has an [unexpected outcome][d2c1],
    [`record-unexpected-outcome`][8548] is signalled.

Built on top of [`journaled`][6267], the macros below record a pair of
[In-events][186b] and [Out-events][48ef] but differ in how they are replayed and
the requirements on their [block][06a7]s. The following table names the
type of [`event`][a394] produced (`Event`), how [In-events][186b] are
replayed (`In-e.`), whether the block is always run (`Run`), how
[Out-events][48ef] are replayed (`Out-e.`), whether the block must be
deterministic (`Det`) or side-effect free (`sef`).

    |          | Event     | In-e.  | Run | Out-e. | Det | SEF |
    |----------+-----------+--------+-----+--------+-----+-----|
    | FRAMED   | log       | skip   | y   | skip   | n   | n   |
    | CHECKED  | versioned | match  | y   | match  | y   | n   |
    | REPLAYED | external  | match  | n   | replay | n   | y   |
    | INVOKED  | versioned | replay | y   | match  | y   | n   |

Note that the replay-replay combination is not implemented because
there is nowhere to return values from replay-triggered functions.

<a id="x-28JOURNAL-3AFRAMED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:FRAMED%20MGL-PAX:MACRO"></a>

- [macro] **framed** *(name &key log-record args values condition) &body body*

    A wrapper around [`journaled`][6267] to produce [frame][7df7]s of [`log-event`][51ce]s. That
    is, `version` is always `nil`, and some irrelevant arguments are
    omitted. The related [`logged`][23c4] creates a single [`leaf-event`][5cd1].
    
    With `framed`, `body` is always run and no [`replay-failure`][2e9b]s are
    triggered. `body` is not required to be deterministic, and it may have
    side-effects.

<a id="x-28JOURNAL-3ACHECKED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:CHECKED%20MGL-PAX:MACRO"></a>

- [macro] **checked** *(name &key (version 1) args values condition insertable) &body body*

    A wrapper around [`journaled`][6267] to produce [frame][7df7]s of [`versioned-event`][4c2b]s.
    `version` defaults to 1. `checked` is for ensuring that supposedly
    deterministic processing does not veer off the replay.
    
    With `checked`, `body` – which must be deterministic – is always run and
    [`replay-failure`][2e9b]s are triggered when the events generated do not match
    the events in the replay journal. `body` may have side-effects.
    
    For further discussion of determinism, see [`replayed`][c2b8].

<a id="x-28JOURNAL-3AREPLAYED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:REPLAYED%20MGL-PAX:MACRO"></a>

- [macro] **replayed** *(name &key args values condition insertable replay-values replay-condition) &body body*

    A wrapper around [`journaled`][6267] to produce [frame][7df7]s of [`external-event`][0e53]s.
    `version` is `:infinity`. `replayed` is for primarily for marking and
    isolating non-deterministic processing.
    
    With `replayed`, the [`in-event`][1729] is checked for consistency with the
    replay (as with [`checked`][e95a]), but `body` is not run (assuming it has a
    recorded [expected outcome][4657]), and the outcome in the [`out-event`][637d] is
    reproduced (see [Replaying the outcome][7991]). For this scheme to work,
    `replayed` requires its `body` to be side-effect free, but it may be
    non-deterministic.

<a id="x-28JOURNAL-3A-40INVOKED-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@INVOKED%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **invoked**

    Invoked refers to functions and blocks defined by [`define-invoked`][3153] or
    [`flet-invoked`][6ab5]. Invoked frames may be recorded in response to
    asynchronous events, and at replay the presence of its in-event
    triggers the execution of the function associated with the name of
    the event.
    
    On the one hand, [`framed`][5d05], [`checked`][e95a], [`replayed`][c2b8] or plain [`journaled`][6267] have
    [In-events][186b] that are always predictable from the code and the
    preceding events. The control flow – on the level of recorded frames
    – is deterministic in this sense. On the other hand, Invoked encodes
    in its [`in-event`][1729] what function to call next, introducing
    non-deterministic control flow.
    
    By letting events choose the code to run, Invoked resembles typical
    [event sourcing][7571] frameworks. When Invoked is used exclusively, the
    journal becomes a sequence of events. In contrast, `journaled` and its
    wrappers put code first, and the journal will be a projection of the
    call tree.

<a id="x-28JOURNAL-3ADEFINE-INVOKED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:DEFINE-INVOKED%20MGL-PAX:MACRO"></a>

- [macro] **define-invoked** *function-name args (name &key (version 1) insertable) &body body*

    `define-invoked` is intended for recording asynchronous function
    invocations like event or signal handlers. It defines a function
    that records [`versioned-event`][4c2b]s with `args` set to the actual arguments.
    At replay, it is invoked whenever the recorded [`in-event`][1729] becomes the
    [replay event][6525].
    
    [`defun`][f472] and [`checked`][e95a] rolled into one, `define-invoked` defines a
    top-level function with `function-name` and `args` (only simple
    positional arguments are allowed) and wraps `checked` with `name`, the
    same `args` and `insertable` around `body`. Whenever an `in-event` becomes
    the [replay event][6525], and it has a `define-invoked` defined with the name
    of the event, `function-name` is invoked with [`event-args`][3335].
    
    While `body`'s return values are recorded as usual, the defined
    function returns no values to make it less likely to affect control
    flow in a way that's not possible to reproduce when the function is
    called by the replay mechanism.
    
    ```
    (defvar *state*)
    
    (define-invoked foo (x) ("foo")
      (setq *state* (1+ x)))
    
    (define-invoked bar (x) ("bar")
      (setq *state* (+ 2 x)))
    
    (if (zerop (random 2))
        (foo 0)
        (bar 1))
    ```
    
    The above can be alternatively implemented with [`replayed`][c2b8] explicitly
    encapsulating the non-determinism:
    
    ```
    (let ((x (replayed (choose) (random 2))))
      (if (zerop x)
          (checked (foo :args `(,x))
            (setq *state* (1+ x)))
          (checked (bar :args `(,x))
            (setq *state* (+ 2 x)))))
    ```
    
    [`fmakunbound`][609c] and [`unintern`][cdba] undefine invoked functions. Note that this is thread-safe only on SBCL. Do
    not delete `define-invoked` functions in
    production.

<a id="x-28JOURNAL-3AFLET-INVOKED-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:FLET-INVOKED%20MGL-PAX:MACRO"></a>

- [macro] **flet-invoked** *definitions &body body*

    Like [`define-invoked`][3153], but with [`flet`][091c] instead of [`defun`][f472]. The event
    name and the function are associated in the dynamic extent of `body`.
    [`with-journaling`][6131] does not change the bindings. The example in
    `define-invoked` can be rewritten as:
    
    ```
    (let ((state nil))
      (flet-invoked ((foo (x) ("foo")
                       (setq state (1+ x)))
                     (bar (x) ("bar")
                       (setq state (+ 2 x))))
        (if (zerop (random 2))
          (foo 0)
          (bar 1))))
    ```

<a id="x-28JOURNAL-3A-40BUNDLES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@BUNDLES%20MGL-PAX:SECTION"></a>

### 8.2 Bundles

Consider replaying the same code repeatedly, hoping to make
progress in the processing. Maybe based on the availability of
external input, the code may error out. After each run, one has to
decide whether to keep the journal just recorded or stick with the
replay journal. A typical solution to this would look like this:

```
(let ((record nil))
  (loop
    (setq record (make-in-memory-journal))
    (with-journaling (:record record :replay replay)
      ...)
    (when (and
           ;; RECORD is a valid replay of REPLAY ...
           (eq (journal-state record) :completed)
           ;; ... and is also significantly different from it ...
           (journal-diverged-p record))
      ;; so use it for future replays.
      (setq replay record))))
```

This is pretty much what bundles automate. The above becomes:

```
(let ((bundle (make-in-memory-bundle)))
  (loop
    (with-bundle (bundle)
      ...)))
```

With [`file-journal`][8428]s, the motivating example above would be even more
complicated, but [`file-bundle`][1895]s work the same way as
[`in-memory-bundle`][bacd]s.

<a id="x-28JOURNAL-3AWITH-BUNDLE-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:WITH-BUNDLE%20MGL-PAX:MACRO"></a>

- [macro] **with-bundle** *(bundle) &body body*

    This is like [`with-journaling`][6131] where the [`replay-journal`][838b] is the last
    successfully completed one in `bundle`, and the [`record-journal`][3b63] is a
    new one created in `bundle`. When `with-bundle` finishes, the record
    journal is in [`journal-state`][03de] `:failed` or `:completed`.
    
    To avoid accumulating useless data, the new record is immediately
    deleted when `with-bundle` finishes if it has not diverged from the
    replay journal (see [`journal-divergent-p`][f224]). Because `:failed` journals
    are always divergent in this sense, they are deleted instead based
    on whether there is already a previous failed journal in the bundle
    and the new record is identical to that journal (see
    [`identical-journals-p`][4a00]).
    
    It is a [`journal-error`][0002] to have concurrent or nested `with-bundle`s on
    the same bundle.

<a id="x-28JOURNAL-3A-40THE-REPLAY-STRATEGY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@THE-REPLAY-STRATEGY%20MGL-PAX:SECTION"></a>

### 8.3 The replay strategy

The replay process for both [In-events][186b] and [Out-events][48ef] starts by
determining how the generated event (the *new* event from now on)
shall be replayed. Roughly, the decision is based on the `name` and
`version` of the new event and the [replay event][6525] (the next event to be
read from the replay). There are four possible strategies:

- **match**: A new in-event must match the replay event in its `args`.
  See [Matching in-events][3c21] for details. A new out-event must match
  the replay event's `exit` and `outcome`, see [Matching out-events][7f9d].

- **upgrade**: The new event is not matched to any replay event, but
  an event is consumed from the replay journal. This happens if the
  next new event has the same name as the replay event, but its
  version is higher.

- **insert**: The new event is not matched to any replay event, and
  no events are consumed from the replay journal, which may be
  empty. This is always the case for new [`log-event`][51ce]s and when there
  are no more events to read from the replay journal (unless
  `replay-eoj-error-p`). For [`versioned-event`][4c2b]s, it is affected by
  setting [`journaled`][6267]'s `insertable` to true (see
  [Journaled for replay][d700]).

    The out-event's strategy is always *insert* if the strategy for
    the corresponding in-event was *insert*.

- Also, [`end-of-journal`][3cdb], [`replay-name-mismatch`][6710] and
  [`replay-version-downgrade`][0fdb] may be signalled. See the algorithm below
  details.

The strategy is determined by the following algorithm, invoked
whenever an event is generated by a journaled [block][06a7]:

1. Log events are not matched to the replay. If the new event is a
   log event or a [`replay-failure`][2e9b] has been signalled before (i.e. the
   record journal's [`journal-state`][03de] is `:mismatched`), then **insert**
   is returned.

2. Else, log events to be read in the replay journal are skipped,
   and the next unread, non-log event is peeked at (without
   advancing the replay journal).

    - **end of replay**: If there are no replay events left, then:

        - If `replay-eoj-error-p` is `nil` in [`with-journaling`][6131] (the
          default), **insert** is returned.

        - If `replay-eoj-error-p` is true, then **`end-of-journal`**
          is signalled.

    - **mismatched name**: Else, if the next unread replay event's
      name is not [`equal`][3fb5] to the name of the new event, then:

        - For `versioned-event`s, **`replay-name-mismatch`** is
          signalled if `insertable` is `nil`, else **insert** is
          returned.

        - For [`external-event`][0e53]s, **`replay-name-mismatch`** is
          signalled.

    - **matching name**: Else, if the name of the next unread event
      in the replay journal is `equal` to the name of new event, then
      it is chosen as the *replay* event.

        - If the replay event's version is higher than the new
          event's version, then **`replay-version-downgrade`** is
          signalled.

        - If the two versions are equal, then **match** is returned.

        - If the new event's version is higher, then **upgrade** is
          returned.

        Where `:infinity` is considered higher than any integer and
        equal to itself.

In summary:

     | new event | end-of-replay     | mismatched name   | matching name |
     |-----------+-------------------+-------------------+---------------|
     | Log       | insert            | insert            | insert        |
     | Versioned | insert/eoj-error  | insert/name-error | match-version |
     | External  | insert/eoj-error  | insert/name-error | match-version |

Version matching (`match-version` above) is based on which event has
a higher version:

     | replay event    | =     | new event |
     |-----------------+-------+-----------|
     | downgrade-error | match | upgrade   |


<a id="x-28JOURNAL-3A-40REPLAY-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@REPLAY-EVENT%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **replay event**

    The replay event is the next event to be read from [`replay-journal`][838b]
    which is not to be skipped. There may be no replay event if there
    are no more unread events in the replay journal.
    
    An event in the replay journal is skipped if it is a [`log-event`][51ce] or
    there is a [`with-replay-filter`][0cce] with a matching `:skip`. If `:skip` is in
    effect, the replay event may be indeterminate.
    
    Events from the replay journal are read when they are `:match`ed or
    `:upgrade`d (see [The replay strategy][a8a7]), when nested events are
    echoed while [Replaying the outcome][7991], or when there is an [invoked][4212]
    defined with the same name as the replay event.
    
    The replay event is available via [`peek-replay-event`][eddd].

<a id="x-28JOURNAL-3A-40MATCHING-IN-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@MATCHING-IN-EVENTS%20MGL-PAX:SECTION"></a>

### 8.4 Matching in-events

If the replay strategy is *match*, then, for in-events, the
matching process continues like this:

- If the [`event-args`][3335] are not [`equal`][3fb5], then **[`replay-args-mismatch`][1256]**
  signalled.

- At this point, two things might happen:

    - For [`versioned-event`][4c2b]s, the [block][06a7] will be executed as normal
      and its outcome will be matched to the [replay event][6525] (see
      [Matching out-events][7f9d]).

    - For [`external-event`][0e53]s, the corresponding replay [`out-event`][637d] is
      looked at. If there is one, meaning that the frame finished
      with an [expected outcome][4657], then its outcome will be
      replayed (see [Replaying the outcome][7991]). If the `out-event` is
      missing, then `external-event`s behave like `versioned-event`s,
      and the [block][06a7] is executed.


<a id="x-28JOURNAL-3A-40REPLAYING-THE-OUTCOME-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@REPLAYING-THE-OUTCOME%20MGL-PAX:SECTION"></a>

#### 8.4.1 Replaying the outcome

So, if an in-event is triggered that matches the replay,
`event-version`([`0`][1f5f] [`1`][9ed3]) is `:infinity`, then normal execution is altered in the
following manner:

- The journaled [block][06a7] is not executed.

- To keep execution and the replay journal in sync, events of frames
  nested in the current one are skipped over in the replay journal.

- All events (including [`log-event`][51ce]s) skipped over are echoed to the
  record journal. This serves to keep a trail of what happened
  during the original recording. Note that functions corresponding
  to [invoked][4212] frames are called when their [`in-event`][1729] is skipped over.

- The out-event corresponding to the in-event being processed is
  then read from the replay journal and is recorded again (to allow
  recording to function properly).

To be able to reproduce the outcome in the replay journal, some
assistance may be required from `replay-values` and `replay-condition`:

- If the [replay event][6525] has a normal return (i.e. `event-exit`([`0`][c04d] [`1`][812a]) `:values`),
  then the recorded return values (in [`event-outcome`][c290]) are returned
  immediately as in `(values-list (event-outcome replay-event))`. If
  `replay-values` is specified, it is called instead of [`values-list`][dbd4].
  See [Working with unreadable values][b354] for an example.

- Similarly, if the replay event has unwound with an expected
  condition (has `event-exit` `:condition`), then the recorded
  condition (in `event-outcome`) is signalled as
  IN `(error (event-outcome replay-event))`. If `replay-condition` is
  specified, it is called instead of `error`([`0`][d162] [`1`][35ba]). `replay-condition` must
  not return normally, and it's a [`journal-error`][0002] if it does.

[`with-replay-filter`][0cce]'s `no-replay-outcome` can selectively turn off
replaying the outcome. See [Testing on multiple levels][9376], for an
example.

<a id="x-28JOURNAL-3A-40MATCHING-OUT-EVENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@MATCHING-OUT-EVENTS%20MGL-PAX:SECTION"></a>

### 8.5 Matching out-events

If there were no [Replay failures][2933] during the matching of the
[`in-event`][1729], and the conditions for [Replaying the outcome][7991] were not
met, then the [block][06a7] is executed. When the outcome of the block is
determined, an [`out-event`][637d] is triggered and is matched to the replay
journal. The matching of out-events starts out as in
[The replay strategy][a8a7] with checks for [`event-name`][9f84] and
[`event-version`][1f5f].

If the replay strategy is *insert* or *upgrade*, then the out-event
is written to [`record-journal`][3b63], consuming an event with a matching
name from the [`replay-journal`][838b] in the latter case. If the strategy is
*match*, then:

- If the new event has an [unexpected outcome][d2c1], then
  **[`replay-unexpected-outcome`][6699]** is signalled. Note that the replay
  event always has an [expected outcome][4657] due to the handling of
  [`record-unexpected-outcome`][8548].

- If the new event has an [expected outcome][4657], then unless the new and
  [replay event][6525]'s `event-exit`([`0`][c04d] [`1`][812a])s are [`eq`][5a82] and their [`event-outcome`][c290]s are
  [`equal`][3fb5], **[`replay-outcome-mismatch`][bbef]** is signalled.

- Else, the replay event is consumed and the new event is written to
  the `record-journal`.

Note that [The replay strategy][a8a7] for the in-event and the out-event of
the same [frame][7df7] may differ if the corresponding out-event is not
present in `replay-journal`, which may be the case when the recording
process failed hard without unwinding properly, or when an
[unexpected outcome][d2c1] triggered the transition to [`journal-state`][03de]
`:logging`.

<a id="x-28JOURNAL-3A-40REPLAY-FAILURES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@REPLAY-FAILURES%20MGL-PAX:SECTION"></a>

### 8.6 Replay failures

<a id="x-28JOURNAL-3AREPLAY-FAILURE-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-FAILURE%20CONDITION"></a>

- [condition] **replay-failure** *[serious-condition][af00]*

    A abstract superclass (never itself signalled) for
    all kinds of mismatches between the events produced and the replay
    journal. Signalled only in [`journal-state`][03de] `:replaying` and only once
    per [`with-journaling`][6131]. If a `replay-failure` is signalled for an [`event`][a394],
    then the event will be recorded, but [`record-journal`][3b63] will transition
    to `journal-state` `:mismatched`. Like [`journaling-failure`][3956], this is a
    serious condition because it is to be handled outside the enclosing
    `with-journaling`. If a `replay-failure` were to be handled inside the
    `with-journaling`, keep in mind that in `:mismatched`, replay always
    uses the *insert* replay strategy (see [The replay strategy][a8a7]).

<a id="x-28JOURNAL-3AREPLAY-FAILURE-NEW-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
<a id="JOURNAL:REPLAY-FAILURE-NEW-EVENT%20%28MGL-PAX:READER%20JOURNAL:REPLAY-FAILURE%29"></a>

- [reader] **replay-failure-new-event** *[replay-failure][2e9b] (:new-event)*

<a id="x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
<a id="JOURNAL:REPLAY-FAILURE-REPLAY-EVENT%20%28MGL-PAX:READER%20JOURNAL:REPLAY-FAILURE%29"></a>

- [reader] **replay-failure-replay-event** *[replay-failure][2e9b] (:replay-event)*

<a id="x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-JOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
<a id="JOURNAL:REPLAY-FAILURE-REPLAY-JOURNAL%20%28MGL-PAX:READER%20JOURNAL:REPLAY-FAILURE%29"></a>

- [reader] **replay-failure-replay-journal** *[replay-failure][2e9b] (= '(replay-journal))*

<a id="x-28JOURNAL-3AREPLAY-NAME-MISMATCH-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-NAME-MISMATCH%20CONDITION"></a>

- [condition] **replay-name-mismatch** *[replay-failure][2e9b]*

    Signalled when the new event's and [replay event][6525]'s
    [`event-name`][9f84] are not [`equal`][3fb5]. The [`replay-force-insert`][92aa],
    [`replay-force-upgrade`][10c8] restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-VERSION-DOWNGRADE-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-VERSION-DOWNGRADE%20CONDITION"></a>

- [condition] **replay-version-downgrade** *[replay-failure][2e9b]*

    Signalled when the new event and the [replay event][6525]
    have the same [`event-name`][9f84], but the new event has a lower version. The
    [`replay-force-upgrade`][10c8] restart is provided.

<a id="x-28JOURNAL-3AREPLAY-ARGS-MISMATCH-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-ARGS-MISMATCH%20CONDITION"></a>

- [condition] **replay-args-mismatch** *[replay-failure][2e9b]*

    Signalled when the new event's and [replay event][6525]'s
    [`event-args`][3335] are not [`equal`][3fb5]. The [`replay-force-upgrade`][10c8] restart is
    provided.

<a id="x-28JOURNAL-3AREPLAY-OUTCOME-MISMATCH-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-OUTCOME-MISMATCH%20CONDITION"></a>

- [condition] **replay-outcome-mismatch** *[replay-failure][2e9b]*

    Signalled when the new event's and [replay event][6525]'s
    `event-exit`([`0`][c04d] [`1`][812a]) and/or [`event-outcome`][c290] are not [`equal`][3fb5]. The
    [`replay-force-upgrade`][10c8] restart is provided.

<a id="x-28JOURNAL-3AREPLAY-UNEXPECTED-OUTCOME-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-UNEXPECTED-OUTCOME%20CONDITION"></a>

- [condition] **replay-unexpected-outcome** *[replay-failure][2e9b]*

    Signalled when the new event has an
    [unexpected outcome][d2c1]. Note that the [replay event][6525] always has an
    [expected outcome][4657] due to the logic of [`record-unexpected-outcome`][8548]. No
    restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-INCOMPLETE-20CONDITION-29"></a>
<a id="JOURNAL:REPLAY-INCOMPLETE%20CONDITION"></a>

- [condition] **replay-incomplete** *[replay-failure][2e9b]*

    Signalled if there are unprocessed non-log events in
    [`replay-journal`][838b] when [`with-journaling`][6131] finishes and the body of
    `with-journaling` returned normally, which is to prevent this
    condition to cancel an ongoing unwinding. No restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-FORCE-INSERT-20RESTART-29"></a>
<a id="JOURNAL:REPLAY-FORCE-INSERT%20RESTART"></a>

- [restart] **replay-force-insert**

    This restart forces [The replay strategy][a8a7] to be `:insert`, overriding
    [`replay-name-mismatch`][6710]. This is intended for upgrades, and extreme
    care must be taken not to lose data.

<a id="x-28JOURNAL-3AREPLAY-FORCE-UPGRADE-20RESTART-29"></a>
<a id="JOURNAL:REPLAY-FORCE-UPGRADE%20RESTART"></a>

- [restart] **replay-force-upgrade**

    This restart forces [The replay strategy][a8a7] to be `:upgrade`, overriding
    [`replay-name-mismatch`][6710], [`replay-version-downgrade`][0fdb],
    [`replay-args-mismatch`][1256], [`replay-outcome-mismatch`][bbef]. This is intended for
    upgrades, and extreme care must be taken not to lose data.

<a id="x-28JOURNAL-3A-40UPGRADES-AND-REPLAY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@UPGRADES-AND-REPLAY%20MGL-PAX:SECTION"></a>

### 8.7 Upgrades and replay

The replay mechanism is built on the assumption that the tree of
[frame][7df7]s is the same when the code is replayed as it was when the
replay journal was originally recorded. Thus, non-deterministic
control flow poses a challenge, but non-determinism can be isolated
with [`external-event`][0e53]s. However, when the code changes, we might find
the structure of frames in previous recordings hard to accommodate.
In this case, we might decide to alter the structure, giving up some
of the safety provided by the replay mechanism. There are various
tools at our disposal to control this tradeoff between safety and
flexibility:

- We can insert individual frames with [`journaled`][6267]'s `insertable`,
  upgrade frames by bumping `journaled`'s `version`, and filter frames
  with [`with-replay-filter`][0cce]. This option allows for the most
  consistency checks.

- The [`replay-force-upgrade`][10c8] and [`replay-force-insert`][92aa] restarts allow
  overriding [The replay strategy][a8a7], but their use requires great care
  to be taken.

- Or we may decide to keep the bare minimum of the replay journal
  around and discard everything except for `external-event`s. This
  option is equivalent to

        (let ((*force-insertable* t))
          (with-replay-filter (:skip '((:name nil)))
            42))

- Rerecording the journal without replay might be another option if
  there are no `external-event`s to worry about.

- Finally, we can rewrite the replay journal using the low-level
  interface (see [Streamlets reference][f4d5]). In this case, extreme care
  must be taken not to corrupt the journal (and lose data) as there
  are no consistency checks to save us.

With that, let's see how `with-replay-filter` works.

<a id="x-28JOURNAL-3AWITH-REPLAY-STREAMLET-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:WITH-REPLAY-STREAMLET%20MGL-PAX:MACRO"></a>

- [macro] **with-replay-streamlet** *(var) &body body*

    Open [`replay-journal`][838b] for reading with [`with-open-journal`][6d64], set the
    [`read-position`][6e60] on it to the event next read by the [Replay][041c]
    mechanism (which is never a [`log-event`][51ce]). The low-level
    [Reading from streamlets][adcd] api is then available to inspect the
    contents of the replay. It is an error if `replay-journal` is `nil`.

<a id="x-28JOURNAL-3APEEK-REPLAY-EVENT-20FUNCTION-29"></a>
<a id="JOURNAL:PEEK-REPLAY-EVENT%20FUNCTION"></a>

- [function] **peek-replay-event**

    Return the [replay event][6525] to be read from [`replay-journal`][838b]. This is
    roughly equivalent to
    
    ```
    (when (replay-journal)
      (with-replay-streamlet (streamlet)
        (peek-event streamlet))
    ```
    
    except `peek-replay-event` takes into account [`with-replay-filter`][0cce]
    `:map`, and it may return `(:indeterminate)` if `with-replay-filter`
    `:skip` is in effect and what events are to be skipped cannot be
    decided until the next in-event generated by the code.
    
    Imagine a business process for paying an invoice. In the first
    version of this process, we just pay the invoice:
    
    ```
    (replayed (pay))
    ```
    
    We have left the implementation of PAY blank. In the second version,
    we need to get an approval first:
    
    ```
    (when (replayed (get-approval)
            (= (random 2) 0))
      (replayed (pay)))
    ```
    
    Replaying a journal produced by the first version of the code with
    the second version would run into difficulties because inserting
    [`external-event`][0e53]s is tricky.
    
    We have to first decide how to handle the lack of approval in the
    first version. Here, we just assume the processes started by the
    first version get approval automatically. The implementation is
    based on a dummy `process` block whose version is bumped when the
    payment process changes and is inspected at the start of journaling.
    
    When v1 is replayed with v2, we introduce an `insertable`, versioned
    `get-approval` block that just returns `t`. When replaying the code
    again, still with v2, the `get-approval` block will be upgraded to
    `:infinity`.
    
    ```
    (let ((bundle (make-in-memory-bundle)))
      ;; First version of the payment process. Just pay.
      (with-bundle (bundle)
        (checked (process :version 1))
        (replayed (pay)))
      ;; Second version of the payment process. Only pay if approved.
      (loop repeat 2 do
        (with-bundle (bundle)
          (let ((replay-process-event (peek-replay-event)))
            (checked (process :version 2))
            (when (if (and replay-process-event
                           (< (event-version replay-process-event) 2))
                      ;; This will be upgraded to :INFINITY the second
                      ;; time around the LOOP.
                      (checked (get-approval :insertable t)
                        t)
                      (replayed (get-approval)
                        (= (random 2) 0)))
              (replayed (pay)))))))
    ```

<a id="x-28JOURNAL-3AWITH-REPLAY-FILTER-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:WITH-REPLAY-FILTER%20MGL-PAX:MACRO"></a>

- [macro] **with-replay-filter** *(&key map skip no-replay-outcome) &body body*

    `with-replay-filter` performs journal upgrade during replay by
    allowing events to be transformed as they are read from the replay
    journal or skipped if they match some patterns. For how to add new
    blocks in a code upgrade, see [`journaled`][6267]'s `:insertable` argument. In
    addition, it also allows some control over [Replaying the outcome][7991].
    
    - `map`: A function called with an event read from the replay journal
      which returns a transformed event. See [Events reference][faf2]. `map`
      takes effect before before `skip`.
    
    - `skip`: In addition to filtering out [`log-event`][51ce]s (which always
      happens during replay), filter out all events that belong to
      frames that match any of its `skip` patterns. Filtered out events
      are never seen by `journaled` as it replays events. `skip` patterns
      are of the format `(&key name version<)`, where `version<` is a
      valid [`event-version`][9ed3], and `name` may be `nil`, which acts as a
      wildcard.
    
        `skip` is for when `journaled` [block][06a7]s are removed from the code,
        which would render replaying previously recorded journals
        impossible. Note that, for reasons of safety, it is not possible
        to filter [`external-event`][0e53]s.
    
    - `no-replay-outcome` is a list of [`event-name`][9f84]s. [Replaying the outcome][7991]
      is prevented for frames with [`equal`][3fb5] names. See
      [Testing on multiple levels][9376] for an example.
    
    `with-replay-filter` affects only the immediately enclosing
    [`with-journaling`][6131]. A `with-replay-filter` nested within another in the
    same `with-journaling` inherits the `skip` patterns of its parent, to
    which it adds its own. The [`map`][7206] function is applied to before the
    parent's `map`.
    
    Examples of `skip` patterns:
    
    ```
    ;; Match events with name FOO and version 1, 2, 3 or 4
    (:name foo :version< 5)
    ;; Match events with name BAR and any version
    (:name bar :version< :infinity)
    ;; Same as the previous
    (:name bar)
    ;; Match all names
    (:name nil)
    ;; Same as the previous
    ()
    ```
    
    Skipping can be thought of as removing nodes of the tree of frames,
    connecting its children to its parent. The following example removes
    frames `j1` and `j2` from around `j3`, the `j1` frame from within
    `j3`, and the third `j1` frame.
    
    ```
    (let ((journal (make-in-memory-journal)))
      ;; Record trees J1 -> J2 -> J3 -> J1, and J1.
      (with-journaling (:record journal)
        (checked (j1)
          (checked (j2)
            (checked (j3)
              (checked (j1)
                42))))
        (checked (j1)
          7))
      ;; Filter out all occurrences of VERSIONED-EVENTs named J1 and
      ;; J2 from the replay, leaving only J3 to match.
      (with-journaling (:replay journal :record t :replay-eoj-error-p t)
        (with-replay-filter (:skip '((:name j1) (:name j2)))
          (checked (j3)
            42))))
    ```

<a id="x-28JOURNAL-3A-40TESTING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@TESTING%20MGL-PAX:SECTION"></a>

## 9 Testing

Having discussed the [Replay][041c] mechanism, next are [Testing][7682] and
[Persistence][37c4], which rely heavily on replay. Suppose we want to unit
test user registration. Unfortunately, the code communicates with a
database service and also takes input from the user. A natural
solution is to create [mock object][16a9]s for these external systems to
unshackle the test from the cumbersome database dependency and to
allow it to run without user interaction.

We do this below by wrapping external interaction in [`journaled`][6267] with
`:version` `:infinity` (see [Replaying the outcome][7991]).

```
(defparameter *db* (make-hash-table))

(defun set-key (key value)
  (replayed ("set-key" :args `(,key ,value))
    (format t "Updating db~%")
    (setf (gethash key *db*) value)
    nil))

(defun get-key (key)
  (replayed ("get-key" :args `(,key))
    (format t "Query db~%")
    (gethash key *db*)))

(defun ask-username ()
  (replayed ("ask-username")
    (format t "Please type your username: ")
    (read-line)))

(defun maybe-win-the-grand-prize ()
  (checked ("maybe-win-the-grand-prize")
    (when (= 1000000 (hash-table-count *db*))
      (format t "You are the lucky one!"))))

(defun register-user (username)
  (unless (get-key username)
    (set-key username `(:user-object :username ,username))
    (maybe-win-the-grand-prize)))
```

Now, we write a test that records these interactions in a file when
it's run for the first time.

```
(define-file-bundle-test (test-user-registration
                          :directory (asdf:system-relative-pathname
                                      :journal "test/registration/"))
  (let ((username (ask-username)))
    (register-user username)
    (assert (get-key username))
    (register-user username)
    (assert (get-key username))))

;; Original recording: everything is executed
JRN> (test-user-registration)
Please type your username: joe
Query db
Updating db
Query db
Query db
Query db
=> NIL
```

On reruns, none of the external stuff is executed. The return values
of the external `journaled` blocks are replayed from the journal:

```
;; Replay: all external interactions are mocked.
JRN> (test-user-registration)
=> NIL
```

Should the code change, we might want to upgrade carefully (see
[Upgrades and replay][750a]) or just rerecord from scratch:

```
JRN> (test-user-registration :rerecord t)
Please type your username: joe
Query db
Updating db
Query db
Query db
Query db
=> NIL
```

Thus satisfied that our test runs, we can commit the journal file in
the bundle into version control. Its contents are:

```

(:IN "ask-username" :VERSION :INFINITY)
(:OUT "ask-username" :VERSION :INFINITY :VALUES ("joe" NIL))
(:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
(:OUT "get-key" :VERSION :INFINITY :VALUES (NIL NIL))
(:IN "set-key" :VERSION :INFINITY :ARGS ("joe" (:USER-OBJECT :USERNAME "joe")))
(:OUT "set-key" :VERSION :INFINITY :VALUES (NIL))
(:IN "maybe-win-the-grand-prize" :VERSION 1)
(:OUT "maybe-win-the-grand-prize" :VERSION 1 :VALUES (NIL))
(:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
(:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
(:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
(:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
(:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
(:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
```

Note that when this journal is replayed, new [`versioned-event`][4c2b]s are
required to match the replay. So, after the original recording, we
can check by eyeballing that the record represents a correct
execution. Then on subsequent replays, even though
`maybe-win-the-grand-prize` sits behind `register-user` and is hard
to test with [`assert`][97ee]s, the replay mechanism verifies that it is
called only for new users.

This record-and-replay style of testing is not the only possibility:
direct inspection of a journal with the low-level events api (see
[Events reference][faf2]) can facilitate checking non-local invariants.

<a id="x-28JOURNAL-3ADEFINE-FILE-BUNDLE-TEST-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:DEFINE-FILE-BUNDLE-TEST%20MGL-PAX:MACRO"></a>

- [macro] **define-file-bundle-test** *(name &key directory (equivalentp t)) &body body*

    Define a function with `name` for record-and-replay testing. The
    function's `body` is executed in a [`with-bundle`][12a5] to guarantee
    replayability. The bundle in question is a [`file-bundle`][1895] created in
    `directory`. The function has a single keyword argument, `rerecord`. If
    `rerecord` is true, the bundle is deleted with [`delete-file-bundle`][c438] to
    start afresh.
    
    Furthermore, if `body` returns normally, and it is a replay of a
    previous run, and `equivalentp`, then it is [`assert`][97ee]ed that the record
    and replay journals are [`equivalent-replay-journals-p`][712a]. If this check
    fails, [`record-journal`][3b63] is discarded when the function returns. In
    addition to the replay consistency, this checks that no inserts or
    upgrades were performed (see [The replay strategy][a8a7]).

<a id="x-28JOURNAL-3A-40TESTING-ON-MULTIPLE-LEVELS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@TESTING-ON-MULTIPLE-LEVELS%20MGL-PAX:SECTION"></a>

### 9.1 Testing on multiple levels

Nesting [`replayed`][c2b8]s (that is, [frame][7df7]s of [`external-event`][0e53]s) is not
obviously useful since the outer `replayed` will be replayed by
outcome, and the inner one will be just echoed to the record
journal. However, if we turn off [Replaying the outcome][7991] for the
outer, the inner will be replayed.

This is useful for testing layered communication. For example, we
might have written code that takes input from an external
system ([`read-line`][8a51]) and does some complicated
processing ([`read-from-string`][d813]) before returning the input in a form
suitable for further processing. Suppose we wrap `replayed` around
`read-from-string` for [Persistence][37c4] because putting it around
`read-line` would expose low-level protocol details in the journal,
making protocol changes difficult.

However, upon realizing that `read-from-string` was not the best tool
for the job and switching to [`parse-integer`][3b4e], we want to test by
replaying all previously recorded journals. For this, we prevent the
outer `replayed` from being replayed by outcome with
[`with-replay-filter`][0cce]:

```
(let ((bundle (make-in-memory-bundle)))
  ;; Original with READ-FROM-STRING
  (with-bundle (bundle)
    (replayed ("accept-number")
      (values (read-from-string (replayed ("input-number")
                                  (read-line))))))
  ;; Switch to PARSE-INTEGER and test by replay.
  (with-bundle (bundle)
    (with-replay-filter (:no-replay-outcome '("accept-number"))
      (replayed ("accept-number")
        ;; 1+ is our bug.
        (values (1+ (parse-integer (replayed ("input-number")
                                     (read-line)))))))))
```

The inner `input-number` block is replayed by outcome, and
`parse-integer` is called with the string `read-line` returned in the
original invocation. The outcome of the outer `accept-number` block
checked as if it was a [`versioned-event`][4c2b] and we get a
[`replay-outcome-mismatch`][bbef] due to the bug.

<a id="x-28JOURNAL-3A-40PERSISTENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@PERSISTENCE%20MGL-PAX:SECTION"></a>

## 10 Persistence

<a id="x-28JOURNAL-3A-40PERSISTENCE-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@PERSISTENCE-TUTORIAL%20MGL-PAX:SECTION"></a>

### 10.1 Persistence tutorial

Let's write a simple game.

```
(defun play-guess-my-number ()
  (let ((my-number (replayed (think-of-a-number)
                     (random 10))))
    (format t "~%I thought of a number.~%")
    (loop for i upfrom 0 do
      (write-line "Guess my number:")
      (let ((guess (replayed (read-guess)
                     (values (parse-integer (read-line))))))
        (format t "You guessed ~D.~%" guess)
        (when (= guess my-number)
          (checked (game-won :args `(,(1+ i))))
          (format t "You guessed it in ~D tries!" (1+ i))
          (return))))))

(defparameter *the-evergreen-game* (make-in-memory-bundle))
```

##### Original recording

Unfortunately, the implementation is lacking in the input validation
department. In the transcript below, [`parse-integer`][3b4e] fails with `junk
in string` when the user enters `not a number`:

```
CL-USER> (handler-case
             (with-bundle (*the-evergreen-game*)
               (play-guess-my-number))
           (error (e)
             (format t "Oops. ~A~%" e)))
I thought of a number.
Guess my number:
7 ; real user input
You guessed 7.
Guess my number:
not a number ; real user input
Oops. junk in string "not a number"
```

##### Replay and extension

Instead of fixing this bug, we just restart the game from the
beginning, [Replaying the outcome][7991] of external interactions marked
with [`replayed`][c2b8]:

```
CL-USER> (with-bundle (*the-evergreen-game*)
           (play-guess-my-number))
I thought of a number.
Guess my number:
You guessed 7.
Guess my number: ; New recording starts here
5 ; real user input
You guessed 5.
Guess my number:
4 ; real user input
You guessed 4.
Guess my number:
2 ; real user input
You guessed 2.
You guessed it in 4 tries!
```

##### It's evergreen

We can now replay this game many times without any user interaction:

```
CL-USER> (with-bundle (*the-evergreen-game*)
           (play-guess-my-number))
I thought of a number.
Guess my number:
You guessed 7.
Guess my number:
You guessed 5.
Guess my number:
You guessed 4.
Guess my number:
You guessed 2.
You guessed it in 4 tries!
```

##### The generated events

This simple mechanism allows us to isolate external interactions and
write tests in record-and-replay style based on the events produced:

```
CL-USER> (list-events *the-evergreen-game*)
((:IN THINK-OF-A-NUMBER :VERSION :INFINITY)
 (:OUT THINK-OF-A-NUMBER :VERSION :INFINITY :VALUES (2))
 (:IN READ-GUESS :VERSION :INFINITY)
 (:OUT READ-GUESS :VERSION :INFINITY :VALUES (7))
 (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
 (:OUT READ-GUESS :VERSION :INFINITY :VALUES (5))
 (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
 (:OUT READ-GUESS :VERSION :INFINITY :VALUES (4))
 (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
 (:OUT READ-GUESS :VERSION :INFINITY :VALUES (2))
 (:IN GAME-WON :VERSION 1 :ARGS (4))
 (:OUT GAME-WON :VERSION 1 :VALUES (NIL)))
```

In fact, being able to replay this game at all already checks it
through the `game-won` event that the number of tries calculation is
correct.

In addition, thus being able to reconstruct the internal state of
the program gives us persistence by replay. If instead of a
[`in-memory-bundle`][bacd], we used a [`file-bundle`][1895], the game would have been
saved on disk without having to write any code for saving and
loading the game state.

##### Discussion

Persistence by replay, also known as [event sourcing][7571], is appropriate
when the external interactions are well-defined and stable. Storing
events shines in comparison to persisting state when the control
flow is too complicated to be interrupted and resumed easily.
Resuming execution in deeply nested function calls is fraught with
such peril that it is often easier to flatten the program into a
state machine, which is as pleasant as manually managing
[continuation][77a2]s.

In contrast, the Journal library does not favour certain styles of
control flow and only requires that non-determinism is packaged up
in `replayed`, which allows it to reconstruct the state of the program
from the recorded events at any point during its execution and
resume from there.

<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@SYNCHRONIZATION%20MGL-PAX:SECTION"></a>

### 10.2 Synchronization to storage

In the following, we explore how journals can serve as a
persistence mechanism and the guarantees they offer. The high-level
summary is that journals with `sync` can serve as a durable and
consistent storage medium. The other two
[ACID](https://en.wikipedia.org/wiki/ACID) properties, atomicity and
isolation, do not apply because Journal is single-client and does
not need transactions.

<a id="x-28JOURNAL-3A-40ABORTED-EXECUTION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@ABORTED-EXECUTION%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **aborted execution**

    Aborted execution is when the operating system or the application
    crashes, calls `abort()`, is killed by a `sigkill` signal or there
    is a power outage. Synchronization guarantees are defined in the
    face of aborted execution and do not apply to hardware errors, Lisp
    or OS bugs.

<a id="x-28JOURNAL-3A-40DATA-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@DATA-EVENT%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **data event**

    Data events are the only events that may be non-deterministic. They
    record information that could change if the same code were run
    multiple times. Data events typically correspond to interactions
    with the user, servers or even the random number generator. Due to
    their non-determinism, they are the only parts of the journal not
    reproducible by rerunning the code. In this sense, only the data
    events are not redundant with the code, and whether other events are
    persisted does not affect durability. There are two kinds of data
    events:
    
    - An [`external-event`][0e53] that is also an [`out-event`][637d].
    
    - The [`in-event`][1729] of an [invoked][4212] function, which lies outside the
      normal, deterministic control flow.

<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-STRATEGIES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@SYNCHRONIZATION-STRATEGIES%20MGL-PAX:SECTION"></a>

#### 10.2.1 Synchronization strategies

When a journal or bundle is created (see [`make-in-memory-journal`][9955],
[`make-file-journal`][f0e7], [`make-in-memory-bundle`][8d1e], [`make-file-bundle`][d6af]), the
`sync` option determines when – as a [`record-journal`][3b63] – the recorded
events and [`journal-state`][03de] changes are persisted durably. For
[`file-journal`][8428]s, persisting means calling something like `fsync`,
while for [`in-memory-journal`][b668]s, a user defined function is called to
persist the data.

- `nil`: Never synchronize. A `file-journal`'s file may be corrupted on
  [aborted execution][78fd]. In `in-memory-journal`s, `sync-fn` is never
  called.

- `t`: This is the *no data loss* setting with minimal
  synchronization. It guarantees *consistency* (i.e. no corruption)
  and *durability* up to the most recent [data event][c015] written in
  `journal-state` `:recording` or for the entire record journal in
  states `:failed` and `:completed`. `:failed` or `:completed` is guaranteed
  when leaving [`with-journaling`][6131] at the latest.

- Values other than `nil` and `t` are reserved for future extensions.
  Using them triggers a [`journal-error`][0002].


<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS%20MGL-PAX:SECTION"></a>

#### 10.2.2 Synchronization with in-memory journals

Unlike [`file-journal`][8428]s, [`in-memory-journal`][b668]s do not have any built-in
persistent storage backing them, but with `sync-fn`, persistence can
be tacked on. If non-`nil`, `sync-fn` must be a function of a single
argument, an `in-memory-journal`. `sync-fn` is called according to
[Synchronization strategies][f532], and upon normal return the journal must
be stored durably.

The following example saves the entire journal history when a new
[data event][c015] is recorded. Note how `sync-to-db` is careful to
overwrite `*db*` only if it is called with a journal that has not
failed the replay (as in [Replay failures][2933]) and is sufficiently
different from the replay journal as determined by
[`journal-divergent-p`][f224].

```
(defparameter *db* ())

(defun sync-to-db (journal)
  (when (and (member (journal-state journal)
                     '(:recording :logging :completed))
             (journal-divergent-p journal))
    (setq *db* (journal-events journal))
    (format t "Saved ~S~%New events from position ~S~%" *db*
            (journal-previous-sync-position journal))))

(defun make-db-backed-record-journal ()
  (make-in-memory-journal :sync-fn 'sync-to-db))

(defun make-db-backed-replay-journal ()
  (make-in-memory-journal :events *db*))

(with-journaling (:record (make-db-backed-record-journal)
                  :replay (make-db-backed-replay-journal))
  (replayed (a)
    2)
  (ignore-errors
    (replayed (b)
      (error "Whoops"))))
.. Saved #((:IN A :VERSION :INFINITY)
..         (:OUT A :VERSION :INFINITY :VALUES (2)))
.. New events from position 0
.. Saved #((:IN A :VERSION :INFINITY)
..         (:OUT A :VERSION :INFINITY :VALUES (2))
..         (:IN B :VERSION :INFINITY)
..         (:OUT B :ERROR ("SIMPLE-ERROR" "Whoops")))
.. New events from position 2
..
```

In a real application, external events often involve unreliable or
high-latency communication. In the above example, block `b` signals
an error, say, to simulate some kind of network condition. Now, a
new journal *for replay* is created and initialized with the saved
events, and the whole process is restarted.

```
(defun run-with-db ()
  (with-journaling (:record (make-db-backed-record-journal)
                    :replay (make-db-backed-replay-journal))
    (replayed (a)
      (format t "A~%")
      2)
    (replayed (b)
      (format t "B~%")
      3)))

(run-with-db)
.. B
.. Saved #((:IN A :VERSION :INFINITY)
..         (:OUT A :VERSION :INFINITY :VALUES (2))
..         (:IN B :VERSION :INFINITY)
..         (:OUT B :VERSION :INFINITY :VALUES (3)))
.. New events from position 0
..
=> 3
```

Note that on the rerun, block `a` is not executed because external
events are replayed simply by reproducing their outcome, in this
case returning 2. See [Replaying the outcome][7991]. Block `b`, on the
other hand, was rerun because it had an [unexpected outcome][d2c1] the
first time around. This time it ran without error, a [data event][c015] was
triggered, and `sync-fn` was invoked.

If we were to invoke the now completed `run-with-db` again, it would
simply return 3 without ever invoking `sync-fn`:

```
(run-with-db)
=> 3
```

With [`journal-replay-mismatch`][228c], `sync-fn` can be optimized to to reuse
the sequence of events in the replay journal up until the point of
divergence.

<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-FILE-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@SYNCHRONIZATION-WITH-FILE-JOURNALS%20MGL-PAX:SECTION"></a>

#### 10.2.3 Synchronization with file journals

For [`file-journal`][8428]s, `sync` determines when the events written to the
[`record-journal`][3b63] and its [`journal-state`][03de] will be persisted durably in
the file. Syncing to the file involves two calls to `fsync` and is
not cheap.

Syncing events to files is implemented as follows.

- When the journal file is created, its parent directory is
  immediately fsynced to make sure that the file will not be lost on
  [aborted execution][78fd].

- When an event is about to be written the first time after file
  creation or after a sync, a transaction start marker is written to
  the file.

- Any number of events may be subsequently written until syncing is
  deemed necessary (see [Synchronization strategies][f532]).

- At this point, `fsync` is called to flush all event data and state
  changes to the file, and the transaction start marker is
  *overwritten* with a transaction completed marker and another
  `fsync` is performed.

- When reading back this file (e.g. for replay), an open transaction
  marker is treated as the end of file.

Note that this implementation assumes that after writing the start
transaction marker, a crash cannot leave any kind of garbage bytes
around: it must leave zeros. This is not true for all filesytems.
For example, ext3/ext4 with `data=writeback` [can leave garbage
around][6907].

<a id="x-28JOURNAL-3A-40SAFETY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@SAFETY%20MGL-PAX:SECTION"></a>

## 11 Safety

##### Thread safety

Changes to journals come in two varieties: adding an event and
changing the [`journal-state`][03de]. Both are performed by [`journaled`][6267] only
unless the low-level streamlet interface is used (see
[Streamlets reference][f4d5]). Using `journaled` wrapped in a
[`with-journaling`][6131], [`with-bundle`][12a5], or [`:log-record`][a6ac] without `with-journaling`
is thread-safe.

- Every journal is guaranteed to have at most a single writer active
  at any time. Writers are mainly `with-journaling` and `with-bundle`,
  but any journals directly logged to have a log writer stored in
  the journal object. See [Logging][4e53].

- `with-journaling` and `with-bundle` have dynamic extent as writers,
  but log writers of journals have indefinite extent: once a journal
  is used as a `log-record`, there remains a writer.

- Attempting to create a second writer triggers a [`journal-error`][0002].

- Writing to the same journal via [`:log-record`][a6ac] from multiple threads
  concurrently is possible since this doesn't create multiple
  writers. It is ensured with locking that events are written
  atomically. Frames can be interleaved, but these are [`log-event`][51ce]s,
  so this does not affect replay.

- The juggling of replay and record journals performed by
  `with-bundle` is also thread-safe.

- It is ensured that there is at most one [`file-journal`][8428] object in the
  same Lisp image is backed by the same file.

- Similarly, there is at most [`file-bundle`][1895] object for a directory.

##### Process safety

Currently, there is no protection against multiple OS processes
writing the same `file-journal` or `file-bundle`.

##### Signal safety

Journal is *designed* to be [async-unwind][392c] safe but *not reentrant*.
Interrupts are disabled only for the most critical cleanup forms. If
a thread is killed without unwinding, that constitutes
[aborted execution][78fd], so guarantees about [Synchronization to storage][046e] apply, but
[`journal`][5082] objects written by the thread are not safe to access, and
the Lisp should probably be restarted.

<a id="x-28JOURNAL-3A-40EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@EVENTS-REFERENCE%20MGL-PAX:SECTION"></a>

## 12 Events reference

Events are normally triggered upon entering and leaving the
dynamic extent of a [`journaled`][6267] [block][06a7] (see [In-events][186b] and
[Out-events][48ef]) and also by [`logged`][23c4]. Apart from being part of the
low-level substrate of the Journal library, working with events
directly is sometimes useful when writing tests that inspect
recorded events. Otherwise, skip this entire section.

All [`event`][a394]s have [`event-name`][9f84] and `event-version`([`0`][1f5f] [`1`][9ed3]), which feature
prominently in [The replay strategy][a8a7]. After the examples in
[In-events][186b] and [Out-events][48ef], the following example is a reminder of
how events look in the simplest case.

```
(with-journaling (:record t)
  (journaled (foo :version 1 :args '(1 2))
    (+ 1 2))
  (logged () "Oops")
  (list-events))
=> ((:IN FOO :VERSION 1 :ARGS (1 2))
    (:OUT FOO :VERSION 1 :VALUES (3))
    (:LEAF "Oops"))
```

So, a `journaled` [block][06a7] generates an [`in-event`][1729] and an [`out-event`][637d], which
are simple property lists. The following reference lists these
properties, their semantics and the functions to read them.

<a id="x-28JOURNAL-3AEVENT-20TYPE-29"></a>
<a id="JOURNAL:EVENT%20TYPE"></a>

- [type] **event**

    An event is either an [`in-event`][1729], an [`out-event`][637d] or a [`leaf-event`][5cd1].

<a id="x-28JOURNAL-3AEVENT-3D-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT%3D%20FUNCTION"></a>

- [function] **event=** *event-1 event-2*

    Return whether `event-1` and `event-2` represent the same event.
    In- and out-events belonging to the same [frame][7df7] are *not* the same
    event. [`event-outcome`][c290]s are not compared when `event-exit`([`0`][c04d] [`1`][812a]) is `:error` to
    avoid undue dependence on implementation specific string
    representations. This function is useful in conjunction with
    [`make-in-event`][9ebd] and [`make-out-event`][3439] to write tests.

<a id="x-28JOURNAL-3AEVENT-NAME-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT-NAME%20FUNCTION"></a>

- [function] **event-name** *event*

    The name of an event can be of any type. It is often a symbol or a
    string. When replaying, names are compared with [`equal`][3fb5]. All `event`s
    have names. The names of the in- and out-events belonging to the
    same [frame][7df7] are the same.

<a id="x-28JOURNAL-3A-40EVENT-VERSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@EVENT-VERSIONS%20MGL-PAX:SECTION"></a>

### 12.1 Event versions

<a id="x-28JOURNAL-3AEVENT-VERSION-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT-VERSION%20FUNCTION"></a>

- [function] **event-version** *event*

    Return the version of `event` of type [`event-version`][9ed3].

<a id="x-28JOURNAL-3ALOG-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:LOG-EVENT-P%20FUNCTION"></a>

- [function] **log-event-p** *event*

    See if `event` is a [`log-event`][51ce].

<a id="x-28JOURNAL-3AVERSIONED-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:VERSIONED-EVENT-P%20FUNCTION"></a>

- [function] **versioned-event-p** *event*

    See if `event` is a [`versioned-event`][4c2b].

<a id="x-28JOURNAL-3AEXTERNAL-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:EXTERNAL-EVENT-P%20FUNCTION"></a>

- [function] **external-event-p** *event*

    See if `event` is an [`external-event`][0e53].

<a id="x-28JOURNAL-3A-40IN-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@IN-EVENTS-REFERENCE%20MGL-PAX:SECTION"></a>

### 12.2 In-events

<a id="x-28JOURNAL-3AIN-EVENT-20TYPE-29"></a>
<a id="JOURNAL:IN-EVENT%20TYPE"></a>

- [type] **in-event**

    `in-event`s are triggered upon entering the dynamic extent of a
    [`journaled`][6267] [block][06a7]. `in-event`s have [`event-name`][9f84],
    [`event-version`][1f5f], and [`event-args`][3335]. See [In-events][186b] for a more
    introductory treatment.

<a id="x-28JOURNAL-3AIN-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:IN-EVENT-P%20FUNCTION"></a>

- [function] **in-event-p** *event*

    See if `event` is a [`in-event`][1729].

<a id="x-28JOURNAL-3AMAKE-IN-EVENT-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-IN-EVENT%20FUNCTION"></a>

- [function] **make-in-event** *&key name version args*

    Create an [`in-event`][1729] with `name`, `version` (of type [`event-version`][9ed3]) and
    `args` as its [`event-name`][9f84], [`event-version`][1f5f] and [`event-args`][3335].

<a id="x-28JOURNAL-3AEVENT-ARGS-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT-ARGS%20FUNCTION"></a>

- [function] **event-args** *in-event*

    Return the arguments of `in-event`, normally populated using the `args`
    form in [`journaled`][6267].

<a id="x-28JOURNAL-3A-40OUT-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@OUT-EVENTS-REFERENCE%20MGL-PAX:SECTION"></a>

### 12.3 Out-events

<a id="x-28JOURNAL-3AOUT-EVENT-20TYPE-29"></a>
<a id="JOURNAL:OUT-EVENT%20TYPE"></a>

- [type] **out-event**

    `out-event`s are triggered upon leaving the dynamic extent of the
    [`journaled`][6267] [block][06a7]. `out-event`s have [`event-name`][9f84],
    [`event-version`][1f5f], [`event-exit`][c04d] and [`event-outcome`][c290].
    See [Out-events][48ef] for a more introductory treatment.

<a id="x-28JOURNAL-3AOUT-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:OUT-EVENT-P%20FUNCTION"></a>

- [function] **out-event-p** *event*

    See if `event` is an [`out-event`][637d].

<a id="x-28JOURNAL-3AMAKE-OUT-EVENT-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-OUT-EVENT%20FUNCTION"></a>

- [function] **make-out-event** *&key name version exit outcome*

    Create an [`out-event`][637d] with `name`, `version` (of type [`event-version`][9ed3]),
    `exit` (of type [`event-exit`][812a]), and `outcome` as its [`event-name`][9f84],
    [`event-version`][1f5f], [`event-exit`][c04d] and [`event-outcome`][c290].

<a id="x-28JOURNAL-3AEVENT-EXIT-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT-EXIT%20FUNCTION"></a>

- [function] **event-exit** *out-event*

    Return how the journaled [block][06a7] finished. See [`event-exit`][812a]
    for the possible types.

<a id="x-28JOURNAL-3AEXPECTED-OUTCOME-P-20FUNCTION-29"></a>
<a id="JOURNAL:EXPECTED-OUTCOME-P%20FUNCTION"></a>

- [function] **expected-outcome-p** *out-event*

    See if `out-event` has an [expected outcome][4657].

<a id="x-28JOURNAL-3AUNEXPECTED-OUTCOME-P-20FUNCTION-29"></a>
<a id="JOURNAL:UNEXPECTED-OUTCOME-P%20FUNCTION"></a>

- [function] **unexpected-outcome-p** *out-event*

    See if `out-event` has an [unexpected outcome][d2c1].

<a id="x-28JOURNAL-3AEVENT-OUTCOME-20FUNCTION-29"></a>
<a id="JOURNAL:EVENT-OUTCOME%20FUNCTION"></a>

- [function] **event-outcome** *out-event*

    Return the outcome of the [frame][7df7] (or loosely speaking of a [block][06a7])
    to which `out-event` belongs.

<a id="x-28JOURNAL-3A-40LEAF-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@LEAF-EVENTS-REFERENCE%20MGL-PAX:SECTION"></a>

### 12.4 Leaf-events

<a id="x-28JOURNAL-3ALEAF-EVENT-20TYPE-29"></a>
<a id="JOURNAL:LEAF-EVENT%20TYPE"></a>

- [type] **leaf-event**

    Leaf events are triggered by [`logged`][23c4]. Unlike [`in-event`][1729]s and
    [`out-event`][637d]s, which represent a [frame][7df7], leaf events represent a point
    in execution thus cannot have children. They are also the poorest of
    their kind: they only have an [`event-name`][9f84]. Their `version` is always
    `nil`, which makes them [`log-event`][51ce]s.

<a id="x-28JOURNAL-3ALEAF-EVENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:LEAF-EVENT-P%20FUNCTION"></a>

- [function] **leaf-event-p** *event*

    See if `event` is a [`leaf-event`][5cd1].

<a id="x-28JOURNAL-3AMAKE-LEAF-EVENT-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-LEAF-EVENT%20FUNCTION"></a>

- [function] **make-leaf-event** *name*

    Create a [`leaf-event`][5cd1] with `name`.

<a id="x-28JOURNAL-3A-40JOURNALS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNALS-REFERENCE%20MGL-PAX:SECTION"></a>

## 13 Journals reference

In [Basics][f846], we covered the bare minimum needed to work with
journals. Here, we go into the details.

<a id="x-28JOURNAL-3AJOURNAL-20CLASS-29"></a>
<a id="JOURNAL:JOURNAL%20CLASS"></a>

- [class] **journal**

    `journal` is an abstract base class for a sequence of
    events. In case of [`file-journal`][8428]s, the events are stored in a file,
    while for [`in-memory-journal`][b668]s, they are in a Lisp array. When a
    journal is opened, it is possible to perform I/O on it (see
    [Streamlets reference][f4d5]), which is normally taken care of by
    [`with-journaling`][6131]. For this reason, the user's involvement with
    journals normally only consists of creating and using them in
    `with-journaling`.

<a id="x-28JOURNAL-3AJOURNAL-STATE-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-STATE%20%28MGL-PAX:READER%20JOURNAL:JOURNAL%29"></a>

- [reader] **journal-state** *[journal][5082] (:state)*

    Return the state of [`journal`][5082], which is of type
    [`journal-state`][03de].

<a id="x-28JOURNAL-3AJOURNAL-SYNC-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-SYNC%20%28MGL-PAX:READER%20JOURNAL:JOURNAL%29"></a>

- [reader] **journal-sync** *[journal][5082] (:sync = nil)*

    The `sync` argument specified at instantiation. See
    [Synchronization strategies][f532].

<a id="x-28JOURNAL-3ASYNC-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:SYNC-JOURNAL%20FUNCTION"></a>

- [function] **sync-journal** *&optional (journal (record-journal))*

    Durably persist changes made to `journal` if [`journal-sync`][0752] is `t`.
    The changes that are persisted are 
    
    - [`write-event`][01fd]s and [`journal-state`][03de] changes made in an enclosing
      [`with-journaling`][6131]; and
    
    - `log-record`s from any thread. 
    
    In particular, writes made in a `with-journaling` in another thread
    are not persisted. `sync-journal` is a noop if `journal-sync` is `nil`. It
    is safe to call from any thread.

<a id="x-28JOURNAL-3AJOURNAL-REPLAY-MISMATCH-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-REPLAY-MISMATCH%20%28MGL-PAX:READER%20JOURNAL:JOURNAL%29"></a>

- [reader] **journal-replay-mismatch** *[journal][5082] (= nil)*

    If [`journal-divergent-p`][f224], then this is a list of two
    elements: the [`read-position`][6e60]s in the [`record-journal`][3b63] and
    [`replay-journal`][838b] of the first events that were different (ignoring
    [`log-event`][51ce]s). It is `nil`, otherwise.

<a id="x-28JOURNAL-3AJOURNAL-DIVERGENT-P-20FUNCTION-29"></a>
<a id="JOURNAL:JOURNAL-DIVERGENT-P%20FUNCTION"></a>

- [function] **journal-divergent-p** *journal*

    See if [`with-journaling`][6131] recorded any event so far in this journal
    that was not [`equal`][3fb5] to its [replay event][6525] or it had no corresponding
    replay event. This completely ignores [`log-event`][51ce]s in both journals
    being compared and can be called any time during [Replay][041c]. It plays a
    role in [`with-bundle`][12a5] deciding when a journal is important enough to
    keep and also in [Synchronization with in-memory journals][12ff].
    
    The position of the first mismatch is available via
    [`journal-replay-mismatch`][228c].

<a id="x-28JOURNAL-3A-40COMPARING-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@COMPARING-JOURNALS%20MGL-PAX:SECTION"></a>

### 13.1 Comparing journals

After replay finished (i.e. [`with-journaling`][6131] completed), we can ask
whether there were any changes produced. This is answered in the
strictest sense by [`identical-journals-p`][4a00] and somewhat more
functionally by [`equivalent-replay-journals-p`][712a].

Also see [`journal-divergent-p`][f224].

<a id="x-28JOURNAL-3AIDENTICAL-JOURNALS-P-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:IDENTICAL-JOURNALS-P%20GENERIC-FUNCTION"></a>

- [generic-function] **identical-journals-p** *journal-1 journal-2*

    Compare two journals in a strict sense: whether
    they have the same [`journal-state`][03de] and the lists of their events (as
    in [`list-events`][0c1b]) are [`equal`][3fb5].

<a id="x-28JOURNAL-3AEQUIVALENT-REPLAY-JOURNALS-P-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:EQUIVALENT-REPLAY-JOURNALS-P%20GENERIC-FUNCTION"></a>

- [generic-function] **equivalent-replay-journals-p** *journal-1 journal-2*

    See if two journals are equivalent when used the
    for `replay` in [`with-journaling`][6131]. `equivalent-replay-journals-p` is like
    [`identical-journals-p`][4a00], but it ignores [`log-event`][51ce]s and allows events
    with `event-exit`([`0`][c04d] [`1`][812a]) `:error` to differ in their outcomes, which may very
    well be implementation specific, anyway. Also, it considers two
    groups of states as different `:new`, `:replaying`, `:mismatched`, `:failed`
    vs `:recording`, `:logging`, COMPLETED.

The rest of section is about concrete subclasses of [`journal`][5082].

<a id="x-28JOURNAL-3A-40IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@IN-MEMORY-JOURNALS%20MGL-PAX:SECTION"></a>

### 13.2 In-memory journals

<a id="x-28JOURNAL-3AIN-MEMORY-JOURNAL-20CLASS-29"></a>
<a id="JOURNAL:IN-MEMORY-JOURNAL%20CLASS"></a>

- [class] **in-memory-journal** *[journal][5082]*

    `in-memory-journal`s are backed by a non-persistent
    Lisp array of events. Much quicker than [`file-journal`][8428]s, they are
    ideal for smallish journals persisted manually (see
    [Synchronization with in-memory journals][12ff] for an example).
    
    They are also useful for writing tests based on what events were
    generated. They differ from `file-journal`s in that events written to
    `in-memory-journal`s are not serialized (and deserialized on replay)
    with the following consequences for the objects recorded by
    [`journaled`][6267] (i.e. its `name`, `args` arguments, and also the return `values`([`0`][fc69] [`1`][eef3])
    of the block, or the value returned by [`condition`][83e1]):
    
    - These objects need not be [readable][768f].
    
    - Their identity ([`eq`][5a82]ness) is not lost.
    
    - They must **must not be mutated** in any way.

<a id="x-28JOURNAL-3AMAKE-IN-MEMORY-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-IN-MEMORY-JOURNAL%20FUNCTION"></a>

- [function] **make-in-memory-journal** *&key (events nil eventsp) state (sync nil syncp) sync-fn*

    Create an [`in-memory-journal`][b668].
    
    The returned journal's [`journal-state`][03de] will be set to `state`. If `state`
    is `nil`, then it is replaced by a default value, which is `:completed`
    if the `events` argument is provided, else it is `:new`.
    
    Thus, `(make-in-memory-journal)` creates a journal suitable for
    recording, and to make a replay journal, use `:state` `:completed` with
    some sequence of `events`:
    
    ```
    (make-in-memory-journal :events '((:in foo :version 1)) :state :completed)
    ```
    
    `sync` determines when `sync-fn` will be invoked on the [`record-journal`][3b63].
    `sync` defaults to `t` if `sync-fn`, else to `nil`. For a description of
    possible values, see [Synchronization strategies][f532]. For more
    discussion, see [Synchronization with in-memory journals][12ff].

<a id="x-28JOURNAL-3AJOURNAL-EVENTS-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-EVENTS%20%28MGL-PAX:READER%20JOURNAL:IN-MEMORY-JOURNAL%29"></a>

- [reader] **journal-events** *[in-memory-journal][b668] (:events)*

    A sequence of events in the journal. Not to be
    mutated by client code.

<a id="x-28JOURNAL-3AJOURNAL-PREVIOUS-SYNC-POSITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29"></a>
<a id="JOURNAL:JOURNAL-PREVIOUS-SYNC-POSITION%20%28MGL-PAX:READER%20JOURNAL:IN-MEMORY-JOURNAL%29"></a>

- [reader] **journal-previous-sync-position** *[in-memory-journal][b668] (= 0)*

    The length of [`journal-events`][5e0a] at the time of the
    most recent invocation of `sync-fn`.

<a id="x-28JOURNAL-3A-40FILE-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@FILE-JOURNALS%20MGL-PAX:SECTION"></a>

### 13.3 File journals

<a id="x-28JOURNAL-3AFILE-JOURNAL-20CLASS-29"></a>
<a id="JOURNAL:FILE-JOURNAL%20CLASS"></a>

- [class] **file-journal** *[journal][5082]*

    A `file-journal` is a journal whose contents and
    [`journal-state`][03de] are persisted in a file. This is the [`journal`][5082]
    subclass with out-of-the-box persistence, but see [File bundles][c05a] for
    a more full-featured solution for repeated [Replay][041c]s.
    
    Since serialization in `file-journal`s is built on top of Lisp [`read`][fe58]
    and [`write`][fc0a], everything that [`journaled`][6267] records in events (i.e. its
    `name`, `args` arguments, and also the return `values`([`0`][fc69] [`1`][eef3]) of the block, or
    the value returned by [`condition`][83e1]) must be [readable][768f].
    
    File journals are human-readable and editable by hand with some
    care. When editing, the following needs to be remembered:
    
    - The first character of the file represents its `journal-state`. It
      is a `#\Space` (for state `:new`, `:replaying`, `:mismatched` and
      `:failed`), or a `#\Newline` (for state `:recording`, `:logging` and
      `:completed`).
    
    - If the journal has `sync` (see [Synchronization strategies][f532]), then
      between two events, there may be `#\Del` (also called `#\Rubout`)
      or `#\Ack` characters ([`char-code`][4720] 127 and 6). `#\Del` marks the end
      of the journal contents that may be read back: it's kind of an
      uncommitted-transaction marker for the events that follow it.
      `#\Ack` characters, of which there may be many in the file, mark
      the sequence of events until the next marker of either kind as
      valid (or committed). `#\Ack` characters are ignored when reading
      the journal.
    
    Thus, when editing a file, don't change the first character and
    leave the `#\Del` character, if any, where it is. Also see
    [Synchronization with file journals][674f].

<a id="x-28JOURNAL-3AMAKE-FILE-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-FILE-JOURNAL%20FUNCTION"></a>

- [function] **make-file-journal** *pathname &key sync*

    Return a [`file-journal`][8428] backed by the file with `pathname`. The file is
    created when the journal is opened for writing. For a description of
    `sync`, see [Synchronization strategies][f532].
    
    If there is already an existing `file-journal` backed by the same
    file, then that object is returned. If the existing object has
    different options (e.g. it has `sync` `t` while the `sync` argument is `nil`
    here), then a [`journal-error`][0002] is signalled.
    
    If there is already an existing `file-journal` backed by the same
    file, the [`journal-state`][03de] is not `:new`, but the file doesn't exist,
    then the existing object is **invalidated**: attempts to write will
    fail with `journal-error`. If the existing journal object is being
    written, then invalidation fails with a `journal-error`. After
    invalidation, a new `file-journal` object is created.

<a id="x-28JOURNAL-3APATHNAME-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-JOURNAL-29-29"></a>
<a id="JOURNAL:PATHNAME-OF%20%28MGL-PAX:READER%20JOURNAL:FILE-JOURNAL%29"></a>

- [reader] **pathname-of** *[file-journal][8428] (:pathname)*

    The pathname of the file backing the journal.

<a id="x-28JOURNAL-3A-40PPRINT-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@PPRINT-JOURNALS%20MGL-PAX:SECTION"></a>

### 13.4 Pretty-printing journals

<a id="x-28JOURNAL-3APPRINT-JOURNAL-20CLASS-29"></a>
<a id="JOURNAL:PPRINT-JOURNAL%20CLASS"></a>

- [class] **pprint-journal** *[journal][5082]*

    Events written to a `pprint-journal` have a
    customizable output format. `pprint-journal`s are intended for
    producing prettier output for [Logging][4e53] and [Tracing][e03f], but they do not
    support reads, so they cannot be used as a [`replay-journal`][838b] or in
    [`list-events`][0c1b], for example. On the other hand, events written to
    `pprint-journal`s need not be [readable][768f].

<a id="x-28JOURNAL-3AMAKE-PPRINT-JOURNAL-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-PPRINT-JOURNAL%20FUNCTION"></a>

- [function] **make-pprint-journal** *&key (stream (make-synonym-stream '\*standard-output\*)) (pretty t) (prettifier 'prettify-event) log-decorator*

    Creates a [`pprint-journal`][9150].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-STREAM-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
<a id="JOURNAL:PPRINT-JOURNAL-STREAM%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29"></a>

- [accessor] **pprint-journal-stream** *[pprint-journal][9150] (:stream = \*standard-output\*)*

    The stream where events are dumped. May be set any
    time to another [`stream`][d5a9].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-PRETTY-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
<a id="JOURNAL:PPRINT-JOURNAL-PRETTY%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29"></a>

- [accessor] **pprint-journal-pretty** *[pprint-journal][9150] (:pretty = t)*

    Whether to use [`pprint-journal-prettifier`][853d] or write
    events in as the property lists they are. A
    [boolean-valued symbol][62678].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-PRETTIFIER-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
<a id="JOURNAL:PPRINT-JOURNAL-PRETTIFIER%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29"></a>

- [accessor] **pprint-journal-prettifier** *[pprint-journal][9150] (:prettifier = 'prettify-event)*

    A function like [`prettify-event`][11b7] that writes an
    event to a stream. Only used when [`pprint-journal-pretty`][610f], this is
    the output format customization knob. Also see [decoration][1d11]s.

<a id="x-28JOURNAL-3A-40BUNDLES-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@BUNDLES-REFERENCE%20MGL-PAX:SECTION"></a>

## 14 Bundles reference

In [Bundles][260d], we covered the repeated replay problem that
[`with-bundle`][12a5] automates. Here, we provide a reference for the bundle
classes.

<a id="x-28JOURNAL-3ABUNDLE-20CLASS-29"></a>
<a id="JOURNAL:BUNDLE%20CLASS"></a>

- [class] **bundle**

    A `bundle` consists of a sequence of journals which
    are all reruns of the same code, hopefully making more and more
    progress towards completion. These journals are [Replay][041c]s of the
    previous successful one, extending it with new events. Upon
    replay (see [`with-bundle`][12a5]), the latest journal in the bundle in
    [`journal-state`][03de] `:completed` plays the role of the replay journal, and a
    new journal is added to the bundle for recording. If the replay
    succeeds, this new journal eventually becomes `:completed` and takes
    over the role of the replay journal for future replays until another
    replay succeeds. When the bundle is created and it has no journals
    yet, the replay journal is an empty, completed one.
    
    This is an abstract base class. Direct subclasses are
    [`in-memory-bundle`][bacd] and [`file-bundle`][1895].

<a id="x-28JOURNAL-3AMAX-N-FAILED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29"></a>
<a id="JOURNAL:MAX-N-FAILED%20%28MGL-PAX:ACCESSOR%20JOURNAL:BUNDLE%29"></a>

- [accessor] **max-n-failed** *[bundle][d9b6] (:max-n-failed = 1)*

    If `max-n-failed` is non-`nil`, and the number of
    journals of [`journal-state`][03de] `:failed` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id="x-28JOURNAL-3AMAX-N-COMPLETED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29"></a>
<a id="JOURNAL:MAX-N-COMPLETED%20%28MGL-PAX:ACCESSOR%20JOURNAL:BUNDLE%29"></a>

- [accessor] **max-n-completed** *[bundle][d9b6] (:max-n-completed = 1)*

    If `max-n-completed` is non-`nil`, and the number of
    journals of [`journal-state`][03de] `:completed` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id="x-28JOURNAL-3A-40IN-MEMORY-BUNDLES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@IN-MEMORY-BUNDLES%20MGL-PAX:SECTION"></a>

### 14.1 In-memory bundles

<a id="x-28JOURNAL-3AIN-MEMORY-BUNDLE-20CLASS-29"></a>
<a id="JOURNAL:IN-MEMORY-BUNDLE%20CLASS"></a>

- [class] **in-memory-bundle** *[bundle][d9b6]*

    An `in-memory-bundle` is a [`bundle`][d9b6] that is built on
    [`in-memory-journal`][b668]s. `in-memory-bundle`s have limited utility as a
    persistence mechanism and are provided mainly for reasons of
    symmetry and for testing. See
    [Synchronization with in-memory journals][12ff] for an example of how to
    achieve persistence without bundles.

<a id="x-28JOURNAL-3AMAKE-IN-MEMORY-BUNDLE-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-IN-MEMORY-BUNDLE%20FUNCTION"></a>

- [function] **make-in-memory-bundle** *&key (max-n-failed 1) (max-n-completed 1) sync sync-fn*

    Create a new [`in-memory-bundle`][bacd] with [`max-n-failed`][1dc2] and [`max-n-completed`][8073]. `sync` and `sync-fn`
    are passed on to [`make-in-memory-journal`][9955].

<a id="x-28JOURNAL-3A-40FILE-BUNDLES-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@FILE-BUNDLES%20MGL-PAX:SECTION"></a>

### 14.2 File bundles

<a id="x-28JOURNAL-3AFILE-BUNDLE-20CLASS-29"></a>
<a id="JOURNAL:FILE-BUNDLE%20CLASS"></a>

- [class] **file-bundle** *[bundle][d9b6]*

    A `file-bundle` is a [`bundle`][d9b6] that is built on
    [`file-journal`][8428]s. It provides easy replay-based persistence.

<a id="x-28JOURNAL-3ADIRECTORY-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-BUNDLE-29-29"></a>
<a id="JOURNAL:DIRECTORY-OF%20%28MGL-PAX:READER%20JOURNAL:FILE-BUNDLE%29"></a>

- [reader] **directory-of** *[file-bundle][1895] (:directory)*

    The directory where the files backing the
    [`file-journal`][8428]s in the [`file-bundle`][1895] are kept.

<a id="x-28JOURNAL-3AMAKE-FILE-BUNDLE-20FUNCTION-29"></a>
<a id="JOURNAL:MAKE-FILE-BUNDLE%20FUNCTION"></a>

- [function] **make-file-bundle** *directory &key (max-n-failed 1) (max-n-completed 1) sync*

    Return a [`file-bundle`][1895] object backed by [`file-journal`][8428]s in `directory`.
    See [`max-n-failed`][1dc2] and
    [`max-n-completed`][8073]. For a description of `sync`, see
    [Synchronization strategies][f532].
    
    If there is already a `file-bundle` with the same directory (according
    to [`truename`][ab6d]), that object is returned if it has the same
    `max-n-failed`, `max-n-completed` and `sync` options, else [`journal-error`][0002]
    is signalled.

<a id="x-28JOURNAL-3ADELETE-FILE-BUNDLE-20FUNCTION-29"></a>
<a id="JOURNAL:DELETE-FILE-BUNDLE%20FUNCTION"></a>

- [function] **delete-file-bundle** *directory*

    Delete all journal files (`*.jrn`) from `directory`. Delete the
    directory if empty after the journal files were deleted, else signal
    an error. Existing [`file-bundle`][1895] objects are not updated, so
    [`make-file-journal`][f0e7] with FORCE-RELOAD may be required.

<a id="x-28JOURNAL-3A-40STREAMLETS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@STREAMLETS-REFERENCE%20MGL-PAX:SECTION"></a>

## 15 Streamlets reference

This section is relevant mostly for implementing new kinds of
[`journal`][5082]s in addition to [`file-journal`][8428]s and [`in-memory-journal`][b668]s. In
normal operation, [`streamlet`][7a2f]s are not worked with directly.

<a id="x-28JOURNAL-3A-40OPENING-AND-CLOSING-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@OPENING-AND-CLOSING%20MGL-PAX:SECTION"></a>

### 15.1 Opening and closing

<a id="x-28JOURNAL-3ASTREAMLET-20CLASS-29"></a>
<a id="JOURNAL:STREAMLET%20CLASS"></a>

- [class] **streamlet**

    A `streamlet` is a handle to perform I/O on a
    [`journal`][5082]. The high-level stuff ([`with-journaling`][6131], [`journaled`][6267], etc) is
    built on top of streamlets.

<a id="x-28JOURNAL-3AJOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3ASTREAMLET-29-29"></a>
<a id="JOURNAL:JOURNAL%20%28MGL-PAX:READER%20JOURNAL:STREAMLET%29"></a>

- [reader] **journal** *[streamlet][7a2f] (:journal)*

    The `journal` that was passed to [`open-streamlet`][9d79].
    This is the journal `streamlet` operates on.

<a id="x-28JOURNAL-3AOPEN-STREAMLET-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:OPEN-STREAMLET%20GENERIC-FUNCTION"></a>

- [generic-function] **open-streamlet** *journal &key direction*

    Return a [`streamlet`][7a2f] suitable for performing I/O on
    `journal`. `direction` (defaults to `:input`) is one of `:input`, `:output`,
    `:io`, and it has the same purpose as the similarly named argument of
    [`cl:open`][6547].

<a id="x-28JOURNAL-3ACLOSE-STREAMLET-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:CLOSE-STREAMLET%20GENERIC-FUNCTION"></a>

- [generic-function] **close-streamlet** *streamlet*

    Close `streamlet`, which was returned by
    [`open-streamlet`][9d79]. After closing, `streamlet` may not longer be used for
    IO.

<a id="x-28JOURNAL-3AMAKE-STREAMLET-FINALIZER-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:MAKE-STREAMLET-FINALIZER%20GENERIC-FUNCTION"></a>

- [generic-function] **make-streamlet-finalizer** *streamlet*

    Return `nil` or a function of no arguments suitable
    as a finalizer for `streamlet`. That is, a function that closes
    `streamlet` but holds no reference to it. This is intended for
    streamlets that are not dynamic-extent, so using [`with-open-journal`][6d64]
    is not appropriate.

<a id="x-28JOURNAL-3AOPEN-STREAMLET-P-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:OPEN-STREAMLET-P%20GENERIC-FUNCTION"></a>

- [generic-function] **open-streamlet-p** *streamlet*

    Return true if `streamlet` is open. `streamlet`s are
    open until they have been explicitly closed with [`close-streamlet`][7e9f].

<a id="x-28JOURNAL-3AINPUT-STREAMLET-P-20FUNCTION-29"></a>
<a id="JOURNAL:INPUT-STREAMLET-P%20FUNCTION"></a>

- [function] **input-streamlet-p** *streamlet*

    See if `streamlet` was opened for input (the `direction` argument of
    [`open-streamlet`][9d79] was `:input` or `:io`).

<a id="x-28JOURNAL-3AOUTPUT-STREAMLET-P-20FUNCTION-29"></a>
<a id="JOURNAL:OUTPUT-STREAMLET-P%20FUNCTION"></a>

- [function] **output-streamlet-p** *streamlet*

    See if `streamlet` was opened for input (the `direction` argument of
    [`open-streamlet`][9d79] was `:output` or `:io`).

<a id="x-28JOURNAL-3AWITH-OPEN-JOURNAL-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:WITH-OPEN-JOURNAL%20MGL-PAX:MACRO"></a>

- [macro] **with-open-journal** *(var journal &key (direction :input)) &body body*

    This is like [`with-open-file`][9ac2] but for `journal`s.
    Open the journal designated by `journal` (see [`to-journal`][e8ed]) with
    [`open-streamlet`][9d79], passing `direction` along, and bind `var` to the
    resulting [`streamlet`][7a2f]. Call [`close-streamlet`][7e9f] after `body` finishes. If
    `journal` is `nil`, then `var` is bound to `nil` and no streamlet is
    created.

<a id="x-28JOURNAL-3ASTREAMLET-ERROR-20CONDITION-29"></a>
<a id="JOURNAL:STREAMLET-ERROR%20CONDITION"></a>

- [condition] **streamlet-error** *[error][d162]*

    Like [`cl:stream-error`][ad6d]: failures pertaining to I/O on
    a closed [`streamlet`][7a2f] or of the wrong `direction`. Actual I/O errors are
    *not* encapsulated in `streamlet-error`.

<a id="x-28JOURNAL-3A-40READING-FROM-STREAMLETS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@READING-FROM-STREAMLETS%20MGL-PAX:SECTION"></a>

### 15.2 Reading from streamlets

<a id="x-28JOURNAL-3AREAD-EVENT-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:READ-EVENT%20GENERIC-FUNCTION"></a>

- [generic-function] **read-event** *streamlet &optional eoj-error-p*

    Read the event at the current read position from
    `streamlet`, and move the read position to the event after. If there
    are no more events, signal [`end-of-journal`][3cdb] or return `nil` depending on
    `eoj-error-p`. Signals [`streamlet-error`][e6b2] if `streamlet` is not
    [`input-streamlet-p`][b292] or not [`open-streamlet-p`][5da8].

<a id="x-28JOURNAL-3AREAD-POSITION-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:READ-POSITION%20GENERIC-FUNCTION"></a>

- [generic-function] **read-position** *streamlet*

    Return an integer that identifies the position of
    the next event to be read from `streamlet`. [`setf`][a138]able, see
    [`set-read-position`][f932].

<a id="x-28JOURNAL-3ASET-READ-POSITION-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:SET-READ-POSITION%20GENERIC-FUNCTION"></a>

- [generic-function] **set-read-position** *streamlet position*

    Set the read position of `streamlet` to `position`,
    which must have been acquired from [`read-position`][6e60].

<a id="x-28JOURNAL-3ASAVE-EXCURSION-20MGL-PAX-3AMACRO-29"></a>
<a id="JOURNAL:SAVE-EXCURSION%20MGL-PAX:MACRO"></a>

- [macro] **save-excursion** *(streamlet) &body body*

    Save [`read-position`][6e60] of `streamlet`, execute `body`, and make sure to
    restore the saved read position.

<a id="x-28JOURNAL-3APEEK-EVENT-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:PEEK-EVENT%20GENERIC-FUNCTION"></a>

- [generic-function] **peek-event** *streamlet*

    Read the next event from `streamlet` without changing
    the read position, or return `nil` if there is no event to be read.

<a id="x-28JOURNAL-3APEEK-EVENT-20-28METHOD-20-28JOURNAL-3ASTREAMLET-29-29-29"></a>
<a id="JOURNAL:PEEK-EVENT%20%28METHOD%20%28JOURNAL:STREAMLET%29%29"></a>

- [method] **peek-event** *(streamlet streamlet)*

    This is a slow default implementation, which relies on
    [`save-excursion`][b283] and [`read-event`][adcf].

<a id="x-28JOURNAL-3A-40WRITING-TO-STREAMLETS-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@WRITING-TO-STREAMLETS%20MGL-PAX:SECTION"></a>

### 15.3 Writing to streamlets

<a id="x-28JOURNAL-3AWRITE-EVENT-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:WRITE-EVENT%20GENERIC-FUNCTION"></a>

- [generic-function] **write-event** *event streamlet*

    Write `event` to `streamlet`.
    Writing always happens at the end of `streamlet`'s journal regardless
    of the [`read-position`][6e60], and the read position is not changed. Signals
    [`streamlet-error`][e6b2] if `streamlet` is not [`output-streamlet-p`][956a] or not
    [`open-streamlet-p`][5da8].

<a id="x-28JOURNAL-3AWRITE-EVENT-20-28METHOD-20-28T-20JOURNAL-3AJOURNAL-29-29-29"></a>
<a id="JOURNAL:WRITE-EVENT%20%28METHOD%20%28T%20JOURNAL:JOURNAL%29%29"></a>

- [method] **write-event** *event (journal journal)*

    For convenience, it is possible to write directly to a `journal`,
    in which case the journal's internal output streamlet is used.
    This internal streamlet is opened for `:output` and may be used by
    [`:log-record`][a6ac].

<a id="x-28JOURNAL-3AWRITE-POSITION-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:WRITE-POSITION%20GENERIC-FUNCTION"></a>

- [generic-function] **write-position** *streamlet*

    Return an integer that identifies the position of
    the next event to be written to `streamlet`.

<a id="x-28JOURNAL-3AREQUEST-COMPLETED-ON-ABORT-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:REQUEST-COMPLETED-ON-ABORT%20GENERIC-FUNCTION"></a>

- [generic-function] **request-completed-on-abort** *streamlet*

    Make it so that upon [aborted execution][78fd],
    `streamlet`'s [`journal`][5082] will be in [`journal-state`][03de] `:completed` when loaded
    fresh (e.g. when creating a [`file-journal`][8428] with an existing file). Any
    previously written events must be persisted before making this
    change. Before `request-completed-on-abort` is called, a journal must
    be reloaded in state `:failed`.
    
    It is permissible to defer carrying out this request until the next
    [`sync-streamlet`][c9d6] call. If the request was carried out, return true. If
    it was deferred, return `nil`.

<a id="x-28JOURNAL-3ASYNC-STREAMLET-20GENERIC-FUNCTION-29"></a>
<a id="JOURNAL:SYNC-STREAMLET%20GENERIC-FUNCTION"></a>

- [generic-function] **sync-streamlet** *streamlet*

    Durably persist the effects of all preceding
    [`write-event`][01fd] calls made via `streamlet` to its journal and any deferred
    [`request-completed-on-abort`][068a] in this order.

<a id="x-28JOURNAL-3A-40JOURNAL-2FGLOSSARY-20MGL-PAX-3ASECTION-29"></a>
<a id="JOURNAL:@JOURNAL%2FGLOSSARY%20MGL-PAX:SECTION"></a>

## 16 Glossary

<a id="x-28JOURNAL-3A-40ASYNC-UNWIND-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@ASYNC-UNWIND%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **async-unwind**

    If an asynchronous event, say a `sigint` triggered by `C-c`, is
    delivered to a thread running Lisp or foreign code called from Lisp,
    a Lisp condition is typically signalled. If the handler for this
    condition unwinds the stack, then we have an asynchronous unwind.
    Another example is `bt:interrupt-thread`, which, as it can execute
    arbitrary code, may unwind the stack in the target thread.

<a id="x-28JOURNAL-3A-40BOOLEAN-VALUED-SYMBOL-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@BOOLEAN-VALUED-SYMBOL%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **boolean-valued symbol**

    Imagine writing two [`stream`][d5a9]s with a spaghetti of functions and
    wanting to have pretty-printed output on one of them. Unfortunately,
    binding [`*print-pretty*`][782a] to `t` will affect writes to both streams.
    
    One solution would be to have streams look up their own print-pretty
    flag with `(symbol-value (stream-pretty-print stream))` and have the
    caller specify the dynamic variable they want:
    
    ```
    (defvar *print-pretty-1* nil)
    (setf (stream-print-pretty stream-1) '*print-pretty-1*)
    (let ((*print-pretty-1* t))
      (spaghetti stream-1 stream-2))
    ```
    
    Note that if the default `stream-print-pretty` is `'*print-pretty*`,
    then we have the normal Common Lisp behaviour. Setting
    `stream-print-pretty` to `nil` or `t` also works, because they are
    self-evaluating.
    
    The above hypothetical example demonstrates the concept of
    boolean-valued symbols on `cl:stream`s. In Journal, they are used by
    [`make-log-decorator`][e33e] and [`pprint-journal`][9150]s.

<a id="x-28JOURNAL-3A-40READABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="JOURNAL:@READABLE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **readable**

    In Common Lisp, readable objects are those that can be printed
    [readably][278a]. Anything written to stream-based journals needs to
    be readable.

  [0002]: #JOURNAL:JOURNAL-ERROR%20CONDITION "JOURNAL:JOURNAL-ERROR CONDITION"
  [0114]: #JOURNAL:@JOURNAL-BACKGROUND%20MGL-PAX:SECTION "Background"
  [01fd]: #JOURNAL:WRITE-EVENT%20GENERIC-FUNCTION "JOURNAL:WRITE-EVENT GENERIC-FUNCTION"
  [0317]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME (MGL-PAX:CLHS CLASS)"
  [03de]: #JOURNAL:JOURNAL-STATE%20TYPE "JOURNAL:JOURNAL-STATE TYPE"
  [041c]: #JOURNAL:@REPLAY%20MGL-PAX:SECTION "Replay"
  [046e]: #JOURNAL:@SYNCHRONIZATION%20MGL-PAX:SECTION "Synchronization to storage"
  [068a]: #JOURNAL:REQUEST-COMPLETED-ON-ABORT%20GENERIC-FUNCTION "JOURNAL:REQUEST-COMPLETED-ON-ABORT GENERIC-FUNCTION"
  [06a7]: #JOURNAL:@BLOCK%20MGL-PAX:GLOSSARY-TERM "block"
  [0752]: #JOURNAL:JOURNAL-SYNC%20%28MGL-PAX:READER%20JOURNAL:JOURNAL%29 "JOURNAL:JOURNAL-SYNC (MGL-PAX:READER JOURNAL:JOURNAL)"
  [07c0]: https://en.wikipedia.org/wiki/Journaling_file_system "journaling filesystem"
  [091c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "FLET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [0c1b]: #JOURNAL:LIST-EVENTS%20FUNCTION "JOURNAL:LIST-EVENTS FUNCTION"
  [0cce]: #JOURNAL:WITH-REPLAY-FILTER%20MGL-PAX:MACRO "JOURNAL:WITH-REPLAY-FILTER MGL-PAX:MACRO"
  [0e53]: #JOURNAL:EXTERNAL-EVENT%20TYPE "JOURNAL:EXTERNAL-EVENT TYPE"
  [0fdb]: #JOURNAL:REPLAY-VERSION-DOWNGRADE%20CONDITION "JOURNAL:REPLAY-VERSION-DOWNGRADE CONDITION"
  [1033]: http://www.lispworks.com/documentation/HyperSpec/Body/t_syn_st.htm "SYNONYM-STREAM (MGL-PAX:CLHS CLASS)"
  [10c3]: http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm "TRACE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [10c8]: #JOURNAL:REPLAY-FORCE-UPGRADE%20RESTART "JOURNAL:REPLAY-FORCE-UPGRADE RESTART"
  [11b7]: #JOURNAL:PRETTIFY-EVENT%20FUNCTION "JOURNAL:PRETTIFY-EVENT FUNCTION"
  [1256]: #JOURNAL:REPLAY-ARGS-MISMATCH%20CONDITION "JOURNAL:REPLAY-ARGS-MISMATCH CONDITION"
  [12a5]: #JOURNAL:WITH-BUNDLE%20MGL-PAX:MACRO "JOURNAL:WITH-BUNDLE MGL-PAX:MACRO"
  [12ff]: #JOURNAL:@SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS%20MGL-PAX:SECTION "Synchronization with in-memory journals"
  [1496]: #JOURNAL:@PPRINT-JOURNALS%20MGL-PAX:SECTION "Pretty-printing journals"
  [14f7]: #JOURNAL:@WRITING-TO-STREAMLETS%20MGL-PAX:SECTION "Writing to streamlets"
  [16a9]: https://en.wikipedia.org/wiki/Mock_object "mock object"
  [1729]: #JOURNAL:IN-EVENT%20TYPE "JOURNAL:IN-EVENT TYPE"
  [186b]: #JOURNAL:@IN-EVENTS%20MGL-PAX:SECTION "In-events"
  [1895]: #JOURNAL:FILE-BUNDLE%20CLASS "JOURNAL:FILE-BUNDLE CLASS"
  [18be]: #JOURNAL:JTRACE%20MGL-PAX:MACRO "JOURNAL:JTRACE MGL-PAX:MACRO"
  [1d0d]: #JOURNAL:@JOURNAL-PORTABILITY%20MGL-PAX:SECTION "Portability"
  [1d11]: #JOURNAL:@DECORATION%20MGL-PAX:GLOSSARY-TERM "decoration"
  [1dc2]: #JOURNAL:MAX-N-FAILED%20%28MGL-PAX:ACCESSOR%20JOURNAL:BUNDLE%29 "JOURNAL:MAX-N-FAILED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE)"
  [1f5f]: #JOURNAL:EVENT-VERSION%20FUNCTION "JOURNAL:EVENT-VERSION FUNCTION"
  [2243]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*TRACE-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [228c]: #JOURNAL:JOURNAL-REPLAY-MISMATCH%20%28MGL-PAX:READER%20JOURNAL:JOURNAL%29 "JOURNAL:JOURNAL-REPLAY-MISMATCH (MGL-PAX:READER JOURNAL:JOURNAL)"
  [23c4]: #JOURNAL:LOGGED%20MGL-PAX:MACRO "JOURNAL:LOGGED MGL-PAX:MACRO"
  [260d]: #JOURNAL:@BUNDLES%20MGL-PAX:SECTION "Bundles"
  [2765]: #JOURNAL:*TRACE-TIME*%20VARIABLE "JOURNAL:*TRACE-TIME* VARIABLE"
  [278a]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#readably "\"readably\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [2933]: #JOURNAL:@REPLAY-FAILURES%20MGL-PAX:SECTION "Replay failures"
  [297c]: #JOURNAL:@CUSTOMIZING-LOGS%20MGL-PAX:SECTION "Customizing logs"
  [2e9b]: #JOURNAL:REPLAY-FAILURE%20CONDITION "JOURNAL:REPLAY-FAILURE CONDITION"
  [3153]: #JOURNAL:DEFINE-INVOKED%20MGL-PAX:MACRO "JOURNAL:DEFINE-INVOKED MGL-PAX:MACRO"
  [3335]: #JOURNAL:EVENT-ARGS%20FUNCTION "JOURNAL:EVENT-ARGS FUNCTION"
  [3439]: #JOURNAL:MAKE-OUT-EVENT%20FUNCTION "JOURNAL:MAKE-OUT-EVENT FUNCTION"
  [34a8]: #JOURNAL:PPRINT-JOURNAL-STREAM%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29 "JOURNAL:PPRINT-JOURNAL-STREAM (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [35ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_error.htm "ERROR (MGL-PAX:CLHS FUNCTION)"
  [37c4]: #JOURNAL:@PERSISTENCE%20MGL-PAX:SECTION "Persistence"
  [392c]: #JOURNAL:@ASYNC-UNWIND%20MGL-PAX:GLOSSARY-TERM "async-unwind"
  [3956]: #JOURNAL:JOURNALING-FAILURE%20CONDITION "JOURNAL:JOURNALING-FAILURE CONDITION"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3ac1]: #JOURNAL:@VALUES-OUTCOME%20MGL-PAX:GLOSSARY-TERM "values outcome"
  [3b4e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm "PARSE-INTEGER (MGL-PAX:CLHS FUNCTION)"
  [3b63]: #JOURNAL:RECORD-JOURNAL%20FUNCTION "JOURNAL:RECORD-JOURNAL FUNCTION"
  [3c21]: #JOURNAL:@MATCHING-IN-EVENTS%20MGL-PAX:SECTION "Matching in-events"
  [3cdb]: #JOURNAL:END-OF-JOURNAL%20CONDITION "JOURNAL:END-OF-JOURNAL CONDITION"
  [3cde]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fixnum.htm "FIXNUM (MGL-PAX:CLHS TYPE)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [4118]: #JOURNAL:*TRACE-JOURNAL*%20VARIABLE "JOURNAL:*TRACE-JOURNAL* VARIABLE"
  [4212]: #JOURNAL:@INVOKED%20MGL-PAX:GLOSSARY-TERM "invoked"
  [42a5]: #JOURNAL:@JOURNAL-SLIME-INTEGRATION%20MGL-PAX:SECTION "Slime integration"
  [4657]: #JOURNAL:@EXPECTED-OUTCOME%20MGL-PAX:GLOSSARY-TERM "expected outcome"
  [4720]: http://www.lispworks.com/documentation/HyperSpec/Body/f_char_c.htm "CHAR-CODE (MGL-PAX:CLHS FUNCTION)"
  [47a7]: #JOURNAL:@PRETTY-PRINTING%20MGL-PAX:SECTION "Pretty-printing"
  [4823]: sbcl-manual.md#UNTRACE%20MGL-PAX:MACRO "UNTRACE MGL-PAX:MACRO"
  [48ef]: #JOURNAL:@OUT-EVENTS%20MGL-PAX:SECTION "Out-events"
  [48f5]: #JOURNAL:@JOURNAL%2FGLOSSARY%20MGL-PAX:SECTION "Glossary"
  [4a00]: #JOURNAL:IDENTICAL-JOURNALS-P%20GENERIC-FUNCTION "JOURNAL:IDENTICAL-JOURNALS-P GENERIC-FUNCTION"
  [4c2b]: #JOURNAL:VERSIONED-EVENT%20TYPE "JOURNAL:VERSIONED-EVENT TYPE"
  [4e53]: #JOURNAL:@LOGGING%20MGL-PAX:SECTION "Logging"
  [4f2b]: #JOURNAL:DATA-EVENT-LOSSAGE%20CONDITION "JOURNAL:DATA-EVENT-LOSSAGE CONDITION"
  [5082]: #JOURNAL:JOURNAL%20CLASS "JOURNAL:JOURNAL CLASS"
  [51ce]: #JOURNAL:LOG-EVENT%20TYPE "JOURNAL:LOG-EVENT TYPE"
  [548d]: sbcl-manual.md#TRACE%20MGL-PAX:MACRO "TRACE MGL-PAX:MACRO"
  [54c1]: #JOURNAL:@EVENT-VERSIONS%20MGL-PAX:SECTION "Event versions"
  [560b]: #JOURNAL:@ERROR-OUTCOME%20MGL-PAX:GLOSSARY-TERM "error outcome"
  [5833]: #JOURNAL:PPRINT-EVENTS%20FUNCTION "JOURNAL:PPRINT-EVENTS FUNCTION"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5cd1]: #JOURNAL:LEAF-EVENT%20TYPE "JOURNAL:LEAF-EVENT TYPE"
  [5d05]: #JOURNAL:FRAMED%20MGL-PAX:MACRO "JOURNAL:FRAMED MGL-PAX:MACRO"
  [5da8]: #JOURNAL:OPEN-STREAMLET-P%20GENERIC-FUNCTION "JOURNAL:OPEN-STREAMLET-P GENERIC-FUNCTION"
  [5e0a]: #JOURNAL:JOURNAL-EVENTS%20%28MGL-PAX:READER%20JOURNAL:IN-MEMORY-JOURNAL%29 "JOURNAL:JOURNAL-EVENTS (MGL-PAX:READER JOURNAL:IN-MEMORY-JOURNAL)"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [610f]: #JOURNAL:PPRINT-JOURNAL-PRETTY%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29 "JOURNAL:PPRINT-JOURNAL-PRETTY (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [6131]: #JOURNAL:WITH-JOURNALING%20MGL-PAX:MACRO "JOURNAL:WITH-JOURNALING MGL-PAX:MACRO"
  [6169]: #JOURNAL:@PERSISTENCE-TUTORIAL%20MGL-PAX:SECTION "Persistence tutorial"
  [6267]: #JOURNAL:JOURNALED%20MGL-PAX:MACRO "JOURNAL:JOURNALED MGL-PAX:MACRO"
  [62678]: #JOURNAL:@BOOLEAN-VALUED-SYMBOL%20MGL-PAX:GLOSSARY-TERM "boolean-valued symbol"
  [637d]: #JOURNAL:OUT-EVENT%20TYPE "JOURNAL:OUT-EVENT TYPE"
  [6525]: #JOURNAL:@REPLAY-EVENT%20MGL-PAX:GLOSSARY-TERM "replay event"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [663f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_null.htm "NULL (MGL-PAX:CLHS CLASS)"
  [6671]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pn.htm "PATHNAME (MGL-PAX:CLHS FUNCTION)"
  [6699]: #JOURNAL:REPLAY-UNEXPECTED-OUTCOME%20CONDITION "JOURNAL:REPLAY-UNEXPECTED-OUTCOME CONDITION"
  [6710]: #JOURNAL:REPLAY-NAME-MISMATCH%20CONDITION "JOURNAL:REPLAY-NAME-MISMATCH CONDITION"
  [674f]: #JOURNAL:@SYNCHRONIZATION-WITH-FILE-JOURNALS%20MGL-PAX:SECTION "Synchronization with file journals"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [68eb]: #JOURNAL:@NLX-OUTCOME%20MGL-PAX:GLOSSARY-TERM "nlx outcome"
  [6907]: https://ext4.wiki.kernel.org/index.php/Ext3_Data=Ordered_vs_Data=Writeback_mode "Ext4 writeback"
  [6ab5]: #JOURNAL:FLET-INVOKED%20MGL-PAX:MACRO "JOURNAL:FLET-INVOKED MGL-PAX:MACRO"
  [6d64]: #JOURNAL:WITH-OPEN-JOURNAL%20MGL-PAX:MACRO "JOURNAL:WITH-OPEN-JOURNAL MGL-PAX:MACRO"
  [6e60]: #JOURNAL:READ-POSITION%20GENERIC-FUNCTION "JOURNAL:READ-POSITION GENERIC-FUNCTION"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [712a]: #JOURNAL:EQUIVALENT-REPLAY-JOURNALS-P%20GENERIC-FUNCTION "JOURNAL:EQUIVALENT-REPLAY-JOURNALS-P GENERIC-FUNCTION"
  [7206]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm "MAP (MGL-PAX:CLHS FUNCTION)"
  [72cd]: #JOURNAL:@OUT-EVENTS-REFERENCE%20MGL-PAX:SECTION "Out-events"
  [750a]: #JOURNAL:@UPGRADES-AND-REPLAY%20MGL-PAX:SECTION "Upgrades and replay"
  [7571]: https://martinfowler.com/eaaDev/EventSourcing.html "event sourcing"
  [7682]: #JOURNAL:@TESTING%20MGL-PAX:SECTION "Testing"
  [768f]: #JOURNAL:@READABLE%20MGL-PAX:GLOSSARY-TERM "readable"
  [77a2]: https://en.wikipedia.org/wiki/Continuation "continuation"
  [782a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_pre.htm "*PRINT-PRETTY* (MGL-PAX:CLHS VARIABLE)"
  [78fd]: #JOURNAL:@ABORTED-EXECUTION%20MGL-PAX:GLOSSARY-TERM "aborted execution"
  [7991]: #JOURNAL:@REPLAYING-THE-OUTCOME%20MGL-PAX:SECTION "Replaying the outcome"
  [7a2f]: #JOURNAL:STREAMLET%20CLASS "JOURNAL:STREAMLET CLASS"
  [7bf3]: #JOURNAL:@SAFETY%20MGL-PAX:SECTION "Safety"
  [7df7]: #JOURNAL:@FRAME%20MGL-PAX:GLOSSARY-TERM "frame"
  [7e9f]: #JOURNAL:CLOSE-STREAMLET%20GENERIC-FUNCTION "JOURNAL:CLOSE-STREAMLET GENERIC-FUNCTION"
  [7ec9]: #JOURNAL:VALUES-%3E%20FUNCTION "JOURNAL:VALUES-> FUNCTION"
  [7f9d]: #JOURNAL:@MATCHING-OUT-EVENTS%20MGL-PAX:SECTION "Matching out-events"
  [8073]: #JOURNAL:MAX-N-COMPLETED%20%28MGL-PAX:ACCESSOR%20JOURNAL:BUNDLE%29 "JOURNAL:MAX-N-COMPLETED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE)"
  [812a]: #JOURNAL:EVENT-EXIT%20TYPE "JOURNAL:EVENT-EXIT TYPE"
  [825c]: #JOURNAL:*TRACE-PRETTY*%20VARIABLE "JOURNAL:*TRACE-PRETTY* VARIABLE"
  [838b]: #JOURNAL:REPLAY-JOURNAL%20FUNCTION "JOURNAL:REPLAY-JOURNAL FUNCTION"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [8428]: #JOURNAL:FILE-JOURNAL%20CLASS "JOURNAL:FILE-JOURNAL CLASS"
  [853d]: #JOURNAL:PPRINT-JOURNAL-PRETTIFIER%20%28MGL-PAX:ACCESSOR%20JOURNAL:PPRINT-JOURNAL%29 "JOURNAL:PPRINT-JOURNAL-PRETTIFIER (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [8548]: #JOURNAL:RECORD-UNEXPECTED-OUTCOME%20CONDITION "JOURNAL:RECORD-UNEXPECTED-OUTCOME CONDITION"
  [86f6]: #JOURNAL:@LEAF-EVENTS-REFERENCE%20MGL-PAX:SECTION "Leaf-events"
  [8a51]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm "READ-LINE (MGL-PAX:CLHS FUNCTION)"
  [8a5b]: #JOURNAL:JOURNAL-LOG-DECORATOR%20%28MGL-PAX:ACCESSOR%20JOURNAL:JOURNAL%29 "JOURNAL:JOURNAL-LOG-DECORATOR (MGL-PAX:ACCESSOR JOURNAL:JOURNAL)"
  [8ae0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY (MGL-PAX:CLHS FUNCTION)"
  [8bc1]: #JOURNAL:@IN-MEMORY-BUNDLES%20MGL-PAX:SECTION "In-memory bundles"
  [8d1e]: #JOURNAL:MAKE-IN-MEMORY-BUNDLE%20FUNCTION "JOURNAL:MAKE-IN-MEMORY-BUNDLE FUNCTION"
  [8f49]: http://www.lispworks.com/documentation/HyperSpec/Body/f_signal.htm "SIGNAL (MGL-PAX:CLHS FUNCTION)"
  [9048]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm "SYMBOL-FUNCTION (MGL-PAX:CLHS FUNCTION)"
  [9105]: #JOURNAL:@JOURNAL-FEATURES%20MGL-PAX:SECTION "Distinguishing features"
  [9150]: #JOURNAL:PPRINT-JOURNAL%20CLASS "JOURNAL:PPRINT-JOURNAL CLASS"
  [92aa]: #JOURNAL:REPLAY-FORCE-INSERT%20RESTART "JOURNAL:REPLAY-FORCE-INSERT RESTART"
  [9376]: #JOURNAL:@TESTING-ON-MULTIPLE-LEVELS%20MGL-PAX:SECTION "Testing on multiple levels"
  [956a]: #JOURNAL:OUTPUT-STREAMLET-P%20FUNCTION "JOURNAL:OUTPUT-STREAMLET-P FUNCTION"
  [97ee]: http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm "ASSERT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9955]: #JOURNAL:MAKE-IN-MEMORY-JOURNAL%20FUNCTION "JOURNAL:MAKE-IN-MEMORY-JOURNAL FUNCTION"
  [9a42]: #JOURNAL:*TRACE-THREAD*%20VARIABLE "JOURNAL:*TRACE-THREAD* VARIABLE"
  [9ac2]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm "WITH-OPEN-FILE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9d79]: #JOURNAL:OPEN-STREAMLET%20GENERIC-FUNCTION "JOURNAL:OPEN-STREAMLET GENERIC-FUNCTION"
  [9d9f]: #JOURNAL:@CONDITION-OUTCOME%20MGL-PAX:GLOSSARY-TERM "condition outcome"
  [9ebd]: #JOURNAL:MAKE-IN-EVENT%20FUNCTION "JOURNAL:MAKE-IN-EVENT FUNCTION"
  [9ed3]: #JOURNAL:EVENT-VERSION%20TYPE "JOURNAL:EVENT-VERSION TYPE"
  [9f84]: #JOURNAL:EVENT-NAME%20FUNCTION "JOURNAL:EVENT-NAME FUNCTION"
  [9f90]: #JOURNAL:JOURNALING-FAILURE-EMBEDDED-CONDITION%20%28MGL-PAX:READER%20JOURNAL:JOURNALING-FAILURE%29 "JOURNAL:JOURNALING-FAILURE-EMBEDDED-CONDITION (MGL-PAX:READER JOURNAL:JOURNALING-FAILURE)"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a370]: http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm "UNTRACE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a394]: #JOURNAL:EVENT%20TYPE "JOURNAL:EVENT TYPE"
  [a6ac]: #JOURNAL:@LOG-RECORD%20MGL-PAX:SECTION "`:log-record`"
  [a8a7]: #JOURNAL:@THE-REPLAY-STRATEGY%20MGL-PAX:SECTION "The replay strategy"
  [aa14]: #JOURNAL:@JOURNAL-LINKS%20MGL-PAX:SECTION "Links"
  [ab6d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tn.htm "TRUENAME (MGL-PAX:CLHS FUNCTION)"
  [ad6d]: http://www.lispworks.com/documentation/HyperSpec/Body/e_stm_er.htm "STREAM-ERROR (MGL-PAX:CLHS CONDITION)"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [adcd]: #JOURNAL:@READING-FROM-STREAMLETS%20MGL-PAX:SECTION "Reading from streamlets"
  [adcf]: #JOURNAL:READ-EVENT%20GENERIC-FUNCTION "JOURNAL:READ-EVENT GENERIC-FUNCTION"
  [af00]: http://www.lispworks.com/documentation/HyperSpec/Body/e_seriou.htm "SERIOUS-CONDITION (MGL-PAX:CLHS CONDITION)"
  [b283]: #JOURNAL:SAVE-EXCURSION%20MGL-PAX:MACRO "JOURNAL:SAVE-EXCURSION MGL-PAX:MACRO"
  [b292]: #JOURNAL:INPUT-STREAMLET-P%20FUNCTION "JOURNAL:INPUT-STREAMLET-P FUNCTION"
  [b2ff]: #JOURNAL:SYNC-JOURNAL%20FUNCTION "JOURNAL:SYNC-JOURNAL FUNCTION"
  [b354]: #JOURNAL:@WORKING-WITH-UNREADABLE-VALUES%20MGL-PAX:SECTION "Working with unreadable values"
  [b668]: #JOURNAL:IN-MEMORY-JOURNAL%20CLASS "JOURNAL:IN-MEMORY-JOURNAL CLASS"
  [b792]: #JOURNAL:@IN-MEMORY-JOURNALS%20MGL-PAX:SECTION "In-memory journals"
  [b7d2]: #JOURNAL:@COMPARING-JOURNALS%20MGL-PAX:SECTION "Comparing journals"
  [b815]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_n.htm#non-local_exit "\"non-local exit\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [ba32]: #JOURNAL:@JOURNAL-UTILITIES%20MGL-PAX:SECTION "Utilities"
  [bacd]: #JOURNAL:IN-MEMORY-BUNDLE%20CLASS "JOURNAL:IN-MEMORY-BUNDLE CLASS"
  [bb08]: #JOURNAL:@JOURNAL-ERROR-HANDLING%20MGL-PAX:SECTION "Error handling"
  [bbef]: #JOURNAL:REPLAY-OUTCOME-MISMATCH%20CONDITION "JOURNAL:REPLAY-OUTCOME-MISMATCH CONDITION"
  [bece]: #JOURNAL:@LOGGING-WITH-LEAVES%20MGL-PAX:SECTION "Logging with `leaf-event`s"
  [bfc5]: #JOURNAL:@OPENING-AND-CLOSING%20MGL-PAX:SECTION "Opening and closing"
  [c015]: #JOURNAL:@DATA-EVENT%20MGL-PAX:GLOSSARY-TERM "data event"
  [c04d]: #JOURNAL:EVENT-EXIT%20FUNCTION "JOURNAL:EVENT-EXIT FUNCTION"
  [c05a]: #JOURNAL:@FILE-BUNDLES%20MGL-PAX:SECTION "File bundles"
  [c290]: #JOURNAL:EVENT-OUTCOME%20FUNCTION "JOURNAL:EVENT-OUTCOME FUNCTION"
  [c2b8]: #JOURNAL:REPLAYED%20MGL-PAX:MACRO "JOURNAL:REPLAYED MGL-PAX:MACRO"
  [c438]: #JOURNAL:DELETE-FILE-BUNDLE%20FUNCTION "JOURNAL:DELETE-FILE-BUNDLE FUNCTION"
  [c9d6]: #JOURNAL:SYNC-STREAMLET%20GENERIC-FUNCTION "JOURNAL:SYNC-STREAMLET GENERIC-FUNCTION"
  [cdba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_uninte.htm "UNINTERN (MGL-PAX:CLHS FUNCTION)"
  [cee6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm "SYMBOL-VALUE (MGL-PAX:CLHS FUNCTION)"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2c1]: #JOURNAL:@UNEXPECTED-OUTCOME%20MGL-PAX:GLOSSARY-TERM "unexpected outcome"
  [d5a9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM (MGL-PAX:CLHS CLASS)"
  [d6af]: #JOURNAL:MAKE-FILE-BUNDLE%20FUNCTION "JOURNAL:MAKE-FILE-BUNDLE FUNCTION"
  [d700]: #JOURNAL:@JOURNALED-FOR-REPLAY%20MGL-PAX:SECTION "Journaled for replay"
  [d813]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm "READ-FROM-STRING (MGL-PAX:CLHS FUNCTION)"
  [d9b6]: #JOURNAL:BUNDLE%20CLASS "JOURNAL:BUNDLE CLASS"
  [dbd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST (MGL-PAX:CLHS FUNCTION)"
  [e03f]: #JOURNAL:@TRACING%20MGL-PAX:SECTION "Tracing"
  [e33e]: #JOURNAL:MAKE-LOG-DECORATOR%20FUNCTION "JOURNAL:MAKE-LOG-DECORATOR FUNCTION"
  [e442]: #JOURNAL:REPLAY-INCOMPLETE%20CONDITION "JOURNAL:REPLAY-INCOMPLETE CONDITION"
  [e6b2]: #JOURNAL:STREAMLET-ERROR%20CONDITION "JOURNAL:STREAMLET-ERROR CONDITION"
  [e748]: #JOURNAL:@FILE-JOURNALS%20MGL-PAX:SECTION "File journals"
  [e760]: http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm "THROW (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [e8ed]: #JOURNAL:TO-JOURNAL%20GENERIC-FUNCTION "JOURNAL:TO-JOURNAL GENERIC-FUNCTION"
  [e95a]: #JOURNAL:CHECKED%20MGL-PAX:MACRO "JOURNAL:CHECKED MGL-PAX:MACRO"
  [ecf9]: http://www.lispworks.com/documentation/HyperSpec/Body/e_storag.htm "STORAGE-CONDITION (MGL-PAX:CLHS CONDITION)"
  [eddd]: #JOURNAL:PEEK-REPLAY-EVENT%20FUNCTION "JOURNAL:PEEK-REPLAY-EVENT FUNCTION"
  [eef3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_values.htm "VALUES (MGL-PAX:CLHS TYPE)"
  [f0e7]: #JOURNAL:MAKE-FILE-JOURNAL%20FUNCTION "JOURNAL:MAKE-FILE-JOURNAL FUNCTION"
  [f17d]: #JOURNAL:VALUES%3C-%20FUNCTION "JOURNAL:VALUES<- FUNCTION"
  [f224]: #JOURNAL:JOURNAL-DIVERGENT-P%20FUNCTION "JOURNAL:JOURNAL-DIVERGENT-P FUNCTION"
  [f379]: #JOURNAL:PRINT-EVENTS%20FUNCTION "JOURNAL:PRINT-EVENTS FUNCTION"
  [f37b]: #JOURNAL:@IN-EVENTS-REFERENCE%20MGL-PAX:SECTION "In-events"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f4d5]: #JOURNAL:@STREAMLETS-REFERENCE%20MGL-PAX:SECTION "Streamlets reference"
  [f532]: #JOURNAL:@SYNCHRONIZATION-STRATEGIES%20MGL-PAX:SECTION "Synchronization strategies"
  [f846]: #JOURNAL:@JOURNAL-BASICS%20MGL-PAX:SECTION "Basics"
  [f932]: #JOURNAL:SET-READ-POSITION%20GENERIC-FUNCTION "JOURNAL:SET-READ-POSITION GENERIC-FUNCTION"
  [faf2]: #JOURNAL:@EVENTS-REFERENCE%20MGL-PAX:SECTION "Events reference"
  [fbbb]: #JOURNAL:@JOURNALS-REFERENCE%20MGL-PAX:SECTION "Journals reference"
  [fc0a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "WRITE (MGL-PAX:CLHS FUNCTION)"
  [fc69]: http://www.lispworks.com/documentation/HyperSpec/Body/f_values.htm "VALUES (MGL-PAX:CLHS FUNCTION)"
  [fe58]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ (MGL-PAX:CLHS FUNCTION)"
  [ff8f]: #JOURNAL:@BUNDLES-REFERENCE%20MGL-PAX:SECTION "Bundles reference"
