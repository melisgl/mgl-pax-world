<a id="x-28LMDB-3A-40LMDB-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB-MANUAL%20MGL-PAX:SECTION"></a>

# LMDB Manual

## Table of Contents

- [1 Links][0090]

- [2 Introduction][3bc3]

- [3 Design and implementation][c362]

    - [3.1 Safety][af06]

    - [3.2 Deviations from the C lmdb API][84b1]

- [4 Library versions][1d55]

- [5 Environments][d0ca]

    - [5.1 Environments reference][bd3d]

    - [5.2 Opening and closing environments][82fb]

    - [5.3 Miscellaneous environment functions][d66b]

- [6 Transactions][bae2]

    - [6.1 Nesting transactions][6ecf]

- [7 Databases][97e3]

    - [7.1 The unnamed database][a14f]

    - [7.2 `dupsort`][186b]

    - [7.3 Database API][67cb]

- [8 Encoding and decoding data][03b4]

    - [8.1 Overriding encodings][60cd]

- [9 Basic operations][f63b]

- [10 Cursors][eb36]

    - [10.1 Positioning cursors][471b]

    - [10.2 Basic cursor operations][ea6c]

    - [10.3 Miscellaneous cursor operations][328c]

- [11 Conditions][edfb]

    - [11.1 Conditions for C lmdb error codes][58a4]

    - [11.2 Additional conditions][5538]

###### \[in package LMDB\]

<a id="x-28-22lmdb-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22lmdb%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- \[system\] **"lmdb"**

    - *Version:* 0.1

    - *Description:* Bindings to `lmdb`, the Lightning Memory-mapped Database.

    - *Licence:* MIT, see COPYING.

    - *Author:* Fernando Borretti <eudoxiahp@gmail.com>, James Anderson \<james.anderson@setf.de>, Gábor Melis <mega@retes.hu>

    - *Maintainer:* Gábor Melis <mega@retes.hu>

    - *Homepage:* <https://github.com/melisgl/lmdb>

    - *Bug tracker:* <https://github.com/melisgl/lmdb/issues>

    - *Source control:* [GIT](https://github.com/melisgl/lmdb.git)

    - *Depends on:* alexandria, bordeaux-threads, cl-reexport, [mgl-pax][6fdb], osicat, trivial-features, trivial-garbage, [trivial-utf-8][d9f2]

<a id="x-28LMDB-3A-40LMDB-2FLINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FLINKS%20MGL-PAX:SECTION"></a>

## 1 Links

The official repository is <https://github.com/melisgl/lmdb>, and
this document in available in various formats on
<https://fixnum.com> for the latest version.

<a id="x-28LMDB-3A-40LMDB-2FINTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FINTRODUCTION%20MGL-PAX:SECTION"></a>

## 2 Introduction

[LMDB](http://www.lmdb.tech/doc/), the Lightning Memory-mapped
Database, is an [ACID](https://en.wikipedia.org/wiki/ACID) key-value
database with
[MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control).
It is a small C library ("C lmdb" from now on), around which `lmdb` is
a Common Lisp wrapper. `lmdb` covers most of C lmdb's functionality,
has a simplified API, much needed [Safety][af06] checks, and
comprehensive documentation.

Compared to other key-value stores, `lmdb`'s distuingishing features
are:

- Transactions span multiple keys.

- Embedded. It has no server but can be used concurrently not only
  by multiple threads but by multiple OS processes, too.

- Extremely high read performance: millions of transactions per
  second.

- Very low maintenance.

Other notable things:

- With its default - the most durable - settings, it has average
  write performance, which is bottlenecked by `fsync()`.

- Readers don't block readers or writers, but there is at most one
  writer at a time.

- Extremely simple, crash-proof design.

- The entire database (called *environment*) is backed by a single
  memory-mapped file, with a
  [copy-on-write](https://en.wikipedia.org/wiki/Copy-on-write)
  [B+ tree](https://en.wikipedia.org/wiki/B%2B_tree).

- No transaction log.

- It is very much like [Berkeley
  DB](https://en.wikipedia.org/wiki/Berkeley_DB) done right,
  without the fluff and much improved administration.

Do read the [Caveats](http://www.lmdb.tech/doc/), though. On the
Lisp side, this library **will not work with virtual threads**
because `lmdb`'s write locking is tied to native threads.

Using `lmdb` is easy:

```
(with-temporary-env (*env*)
  (let ((db (get-db "test")))
    (with-txn (:write t)
      (put db "k1" #(2 3))
      (print (g3t db "k1")) ; => #(2 3)
      (del db "k1"))))
```

More typically, the environment and databases are opened once so
that multiple threads and transactions can access them:

```
(defvar *test-db*)

(unless *env*
  (setq *env* (open-env "/tmp/lmdb-test-env/" :if-does-not-exist :create))
  (setq *test-db* (get-db "test" :value-encoding :utf-8)))

(with-txn (:write t)
  (put *test-db* 1 "hello")
  (print (g3t *test-db* 1)) ; => "hello"
  (del *test-db* 1))
```

Note how `:value-encoding` sneaked in above. This was so to make [`g3t`][13e8]
return a string instead of an octet vector.

`lmdb` treats keys and values as opaque byte arrays to be hung on a B+
tree, and only requires a comparison function to be defined over
keys. `lmdb` knows how to serialize the types `(unsigned-byte 64)` and
[`string`][b93c] (which are often used as keys so sorting must work as
expected). Serialization of the rest of the datatypes is left to the
client. See [Encoding and decoding data][03b4] for more.

<a id="x-28LMDB-3A-40LMDB-2FDESIGN-AND-IMPLEMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FDESIGN-AND-IMPLEMENTATION%20MGL-PAX:SECTION"></a>

## 3 Design and implementation

<a id="x-28LMDB-3A-40LMDB-2FSAFETY-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FSAFETY%20MGL-PAX:SECTION"></a>

### 3.1 Safety

The lmdb C API trusts client code to respect its rules. Being C,
managing object lifetimes is the biggest burden. There are also
rules that are documented, but not enforced. This Lisp wrapper tries
to enforce these rules itself and manage object lifetimes in a safe
way to avoid data corruption. How and what it does is described in
the following.

##### Environments

- [`open-env`][bea1] checks that the same path is not used in multiple open
  environments to prevent locking issues documented in
  [Caveats](http://www.lmdb.tech/doc/).

- [`close-env`][0b9a] waits until all [active transaction][00c7]s are finished before
  actually closing the environment. Alternatively, if `open-env` was
  called with `:synchronized` `nil`, to avoid the overhead of
  synchronization, the environment is closed only when garbage
  collected.

##### Transactions

- Checks are made to detect illegal operations on parent
  transactions (see [`lmdb-illegal-access-to-parent-txn-error`][3935]).

- Access to closed transactions is reliably detected.

- C `lmdb` allows read transactions to be used in multiple threads.
  The synchronization cost of performing this safely (i.e. without
  risking access to closed and freed transaction objects) is
  significant so this is not supported.

##### Databases

- [mdb\_dbi\_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a)
  is wrapped by [`get-db`][f306] in a transaction and is protected by a mutex
  to comply with C lmdb's requirements:

         A transaction that opens a database must finish (either
         commit or abort) before another transaction may open it.
         Multiple concurrent transactions cannot open the same
         database.

- [mdb\_dbi\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5)
  is too dangerous to be exposed as explained in the `get-db`
  documentation.

- For similar reasons, [`drop-db`][4437] is wrapped in [`with-env`][3c69].

- [mdb\_env\_set\_mapsize()](http://www.lmdb.tech/doc/group__mdb.html#gaa2506ec8dab3d969b0e609cd82e619e5),
  [mdb\_env\_set\_max\_readers()](http://www.lmdb.tech/doc/group__mdb.html#gae687966c24b790630be2a41573fe40e2),
  and [mdb\_env\_set\_maxdbs()](http://www.lmdb.tech/doc/group__mdb.html#gaa2fc2f1f37cb1115e733b62cab2fcdbc)
  are only available through `open-env` because they either require that
  there are no write transactions or do not work on open environments.

##### Cursors

- As even read transactions are restricted to a single thread, so
  are cursors. Using a cursor from a thread other than the one in
  which it was created (i.e. the thread of its transaction) raises
  [`lmdb-cursor-thread-error`][2b7a]. In return for this restriction, access
  to cursors belonging to closed transactions is reliably detected.

##### Signal handling

The C lmdb library handles system calls being interrupted (`eintr`
and `eagain`), but unwinding the stack from interrupts in the middle
of `lmdb` calls can leave the in-memory data structures such as
transactions inconsistent. If this happens, their further use risks
data corruption. For this reason, calls to `lmdb` are performed with
interrupts disabled. For SBCL, this means `sb-sys:without-interrupts`.
It is an error when compiling `lmdb` if an equivalent facility is not
found in the Lisp implementation. A warning is signalled if no
substitute is found for `sb-sys:with-interrupts` because this makes
the body of `with-env`, [`with-txn`][fdc6], [`with-cursor`][b1c7] and similar
uninterruptible.

Operations that do not modify the database ([`g3t`][13e8], [`cursor-first`][fa3d],
[`cursor-value`][5902], etc) are async unwind safe, and for performance they
are called without the above provisions.

Note that the library is not reentrant, so don't call `lmdb` from
signal handlers.

<a id="x-28LMDB-3A-40LMDB-2FDEVIATIONS-FROM-THE-LMDB-API-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FDEVIATIONS-FROM-THE-LMDB-API%20MGL-PAX:SECTION"></a>

### 3.2 Deviations from the C lmdb API

The following are the most prominent deviations and omissions from
the C lmdb API in addition to those listed in [Safety][af06].

##### Environments

- [mdb\_reader\_list()](http://www.lmdb.tech/doc/group__mdb.html#ga8550000cd0501a44f57ee6dff0188744)
  is not implemented.

- [mdb\_env\_copy()](http://www.lmdb.tech/doc/group__mdb.html#ga5d51d6130325f7353db0955dbedbc378)
  and its close kin are not yet implemented.

##### Transactions

- Read-only [`with-txn`][fdc6]s are turned into noops when "nested" (unless
  `ignore-parent`).

##### Databases

- [mdb\_set\_compare()](http://www.lmdb.tech/doc/group__mdb.html#ga68e47ffcf72eceec553c72b1784ee0fe)
  and [mdb\_set\_dupsort()](http://www.lmdb.tech/doc/group__mdb.html#gacef4ec3dab0bbd9bc978b73c19c879ae)
  are not exposed. If they are needed, implement a foreign comparison
  function and call `liblmdb:set-compare` or `liblmdb:set-dupsort`
  directly or perhaps change the encoding of the data.

- Working with multiple contiguous values with `dupfixed` is not yet
  implemented. This functionality would belong in [`put`][edfe], [`cursor-put`][a56d],
  [`cursor-next`][d4f9] and [`cursor-value`][5902].

- `put`, `cursor-put` do not support the
  [`RESERVE`](http://www.lmdb.tech/doc/group__mdb__put.html#gac0545c6aea719991e3eae6ccc686efcc)
  flag.

<a id="x-28LMDB-3A-40LMDB-2FVERSION-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FVERSION%20MGL-PAX:SECTION"></a>

## 4 Library versions

<a id="x-28LMDB-3ALMDB-FOREIGN-VERSION-20FUNCTION-29"></a>
<a id="LMDB:LMDB-FOREIGN-VERSION%20FUNCTION"></a>

- \[function\] **lmdb-foreign-version**

    Return the version of the C lmdb library as a string like `0.9.26`.

    Wraps [mdb\_version()](http://www.lmdb.tech/doc/group__mdb.html#ga0e5d7298fc39b3c187fffbe30264c968).

<a id="x-28LMDB-3ALMDB-BINDING-VERSION-20FUNCTION-29"></a>
<a id="LMDB:LMDB-BINDING-VERSION%20FUNCTION"></a>

- \[function\] **lmdb-binding-version**

    Return a string representing the version of C lmdb based on which
    the CFFI bindings were created. The version string has the same
    format as [`lmdb-foreign-version`][af7c].

<a id="x-28LMDB-3A-40LMDB-2FENVIRONMENTS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FENVIRONMENTS%20MGL-PAX:SECTION"></a>

## 5 Environments

An environment (class [`env`][412f]) is basically a single memory-mapped file
holding all the data, plus some flags determining how we interact
it. An environment can have multiple databases (class [`db`][3a5d]), each of
which is a B+ tree within the same file. An environment is like a
database in a relational db, and the databases in it are like tables
and indices. The terminology comes from [Berkeley
`db`](https://docs.oracle.com/cd/E17276_01/html/programmer_reference/env.html).

<a id="x-28LMDB-3A-40LMDB-2FENV-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FENV-REFERENCE%20MGL-PAX:SECTION"></a>

### 5.1 Environments reference

<a id="x-28LMDB-3AENV-20CLASS-29"></a>
<a id="LMDB:ENV%20CLASS"></a>

- \[class\] **env**

    An environment object through which a memory-mapped
    data file can be accessed. Always to be created by [`open-env`][bea1].

<a id="x-28LMDB-3AENV-PATH-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-PATH%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-path** *[env][412f] (:path)*

    The location of the memory-mapped file and the
    environment lock file.

<a id="x-28LMDB-3AENV-MAX-DBS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-MAX-DBS%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-max-dbs** *[env][412f] (:max-dbs)*

    The maximum number of named databases in the
    environment. Currently a moderate number is cheap, but a huge
    number gets expensive: 7-120 words per transaction, and every
    [`get-db`][f306] does a linear search of the opened database.

<a id="x-28LMDB-3AENV-MAX-READERS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-MAX-READERS%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-max-readers** *[env][412f] (:max-readers)*

    The maximum number of threads/reader slots. See
    the documentation of the [reader lock
    table](http://lmdb.tech/doc/group__readers.html) for more.

<a id="x-28LMDB-3AENV-MAP-SIZE-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-MAP-SIZE%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-map-size** *[env][412f] (:map-size)*

    Specifies the size of the data file in bytes.

<a id="x-28LMDB-3AENV-MODE-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-MODE%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-mode** *[env][412f] (:mode)*

<a id="x-28LMDB-3AENV-FLAGS-20-28MGL-PAX-3AREADER-20LMDB-3AENV-29-29"></a>
<a id="LMDB:ENV-FLAGS%20%28MGL-PAX:READER%20LMDB:ENV%29"></a>

- \[reader\] **env-flags** *[env][412f] (:flags)*

    A plist of the options as captured by [`open-env`][bea1].
    For example, `(:fixed-map nil :subdir t ...)`.

<a id="x-28LMDB-3A-40LMDB-2FOPENING-AND-CLOSING-ENV-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FOPENING-AND-CLOSING-ENV%20MGL-PAX:SECTION"></a>

### 5.2 Opening and closing environments

<a id="x-28LMDB-3A-2AENV-CLASS-2A-20VARIABLE-29"></a>
<a id="LMDB:*ENV-CLASS*%20VARIABLE"></a>

- \[variable\] **\*env-class\*** *env*

    The default class [`open-env`][bea1] instaniates. Must be a subclass of [`env`][412f].
    This provides a way to associate application specific data with `env`
    objects.

<a id="x-28LMDB-3AOPEN-ENV-20FUNCTION-29"></a>
<a id="LMDB:OPEN-ENV%20FUNCTION"></a>

- \[function\] **open-env** *path \&key (class \*env-class\*) (if-does-not-exist :error) (synchronized t) (max-dbs 1) (max-readers 126) (map-size (\* 1024 1024)) (mode 436) (subdir t) (sync t) (meta-sync t) read-only (tls t) (read-ahead t) (lock t) (mem-init t) fixed-map write-map map-async*

    Create an [`env`][412f] object through which the `lmdb` environment can be
    accessed and open it. To prevent corruption, an error is signalled
    if the same data file is opened multiple times. However, the checks
    performed do not work on remote filesystems (see [`env-path`][6ed7]).

    [`lmdb-error`][b3a1] is signalled if opening the environment fails for any
    other reason.

    Unless explicitly noted, none of the arguments persist (i.e. they
    are not saved in the data file).

    `path` is the filesystem location of the environment files (see `subdir`
    below for more). Do not use `lmdb` data files on remote filesystems,
    even between processes on the same host. This breaks `flock()` on
    some OSes, possibly memory map sync, and certainly sync between
    programs on different hosts.

    `if-does-not-exist` determines what happens if `env-path` does not
    exists:

    - `:error`: An error is signalled.

    - `:create`: A new memory-mapped file is created ensuring that all
      containing directories exist.

    - `nil`: Return `nil` without doing anything.

    See [`close-env`][0b9a] for the description of `synchronized`.

    - `max-dbs`: The maximum number of named databases in the environment.
      Currently a moderate number is cheap, but a huge number gets
      expensive: 7-120 words per transaction, and every [`get-db`][f306] does a
      linear search of the opened database.

    - `map-size`: Specifies the size of the data file in bytes. The new
      size takes effect immediately for the current process, but will
      not be persisted to any others until a write transaction has been
      committed by the current process. Also, only map size increases
      are persisted into the environment. If the map size is increased
      by another process, and data has grown beyond the range of the
      current mapsize, starting a new transaction (see [`with-txn`][fdc6]) will
      signal [`lmdb-map-resized-error`][e93f]. If zero is specified for `map-size`,
      then the persisted size is used from the data file. Also see
      [`lmdb-map-full-error`][f710].

    - `mode`: Unix file mode for files created. The default is `#o664`.
      Has no effect when opening an existing environment.

    The rest of the arguments correspond to `lmdb` environment flags and
    are available in the plist [`env-flags`][5601].

    - `subdir`: If `subdir`, then the path is a directory which holds the
      `data.mdb` and the `lock.mdb` files. If `subdir` is `nil`, the path
      is the filename of the data file and the lock file has the same
      name plus a `-lock` suffix.

    - `sync`: If `nil`, don't `fsync` after commit. This optimization means
      a system crash can corrupt the database or lose the last
      transactions if buffers are not yet flushed to disk. The risk is
      governed by how often the system flushes dirty buffers to disk and
      how often [`sync-env`][c5fa] is called. However, if the filesystem preserves
      write order (very few do) and the `write-map` (currently
      unsupported) flag is not used, transactions exhibit
      ACI (atomicity, consistency, isolation) properties and only lose
      D (durability). I.e. database integrity is maintained, but a
      system crash may undo the final transactions.

    - `meta-sync`: If `nil`, flush system buffers to disk only once per
      transaction, but omit the metadata flush. Defer that until the
      system flushes files to disk, the next commit of a non-read-only
      transaction or `sync-env`. This optimization maintains database
      integrity, but a system crash may undo the last committed
      transaction. I.e. it preserves the ACI (atomicity, consistency,
      isolation) but not D (durability) database property.

    - `read-only`: Map the data file in read-only mode. It is an error to
      try to modify anything in it.

    - `tls`: Setting it to `nil` allows each OS thread to have multiple
      read-only transactions (see `with-txn`'s `ignore-parent` argument). It
      also allows and transactions not to be tied to a single thread,
      but that's quite dangerous, see [Safety][af06].

    - `read-ahead`: Turn off readahead as in `madvise(MADV_RANDOM)`. Most
      operating systems perform read-ahead on read requests by default.
      This option turns it off if the OS supports it. Turning it off may
      help random read performance when the [`db`][3a5d] is larger than RAM and
      system RAM is full. This option is not implemented on Windows.

    - `lock`: Data corruption lurks here. If `nil`, don't do any locking. If
      concurrent access is anticipated, the caller must manage all
      concurrency itself. For proper operation the caller must enforce
      single-writer semantics, and must ensure that no readers are using
      old transactions while a writer is active. The simplest approach
      is to use an exclusive lock so that no readers may be active at
      all when a writer begins.

    - `mem-init`: If `nil`, don't initialize `malloc`ed memory before
      writing to unused spaces in the data file. By default, memory for
      pages written to the data file is obtained using `malloc`. While
      these pages may be reused in subsequent transactions, freshly
      `malloc`ed pages will be initialized to zeroes before use. This
      avoids persisting leftover data from other code (that used the
      heap and subsequently freed the memory) into the data file. Note
      that many other system libraries may allocate and free memory from
      the heap for arbitrary uses. E.g., stdio may use the heap for file
      I/O buffers. This initialization step has a modest performance
      cost so some applications may want to disable it using this flag.
      This option can be a problem for applications which handle
      sensitive data like passwords, and it makes memory checkers like
      Valgrind noisy. This flag is not needed with `write-map`, which
      writes directly to the mmap instead of using malloc for pages.

    - `fixed-map` (experimental): This flag must be specified when
      creating the environment and is stored persistently in the data
      file. If successful, the memory map will always reside at the same
      virtual address and pointers used to reference data items in the
      database will be constant across multiple invocations. This option
      may not always work, depending on how the operating system has
      allocated memory to shared libraries and other uses.

    Unsupported flags (an error is signalled if they are changed from
    their default values):

    - `write-map`: Use a writable memory map unless `read-only` is set. This
      is faster and uses fewer mallocs, but loses protection from
      application bugs like wild pointer writes and other bad updates
      into the database. Incompatible with nested transactions. This may
      be slightly faster for `db`s that fit entirely in RAM, but is slower
      for `db`s larger than RAM. Do not mix processes with and without
      `write-map` on the same environment. This can defeat
      durability (`sync-env`, etc).

    - `map-async`: When using `write-map`, use asynchronous flushes to disk.
      As with `sync` `nil`, a system crash can then corrupt the database or
      lose the last transactions. Calling #sync ensures on-disk database
      integrity until next commit.

    Open environments have a finalizer attached to them that takes care
    of freeing foreign resources. Thus, the common idiom:

    ```
    (setq *env* (open-env "some-path"))
    ```

    is okay for development, too. No need to always do [`with-env`][3c69],
    which does not mesh with threads anyway.

    Wraps [mdb\_env\_create()](http://www.lmdb.tech/doc/group__mdb.html#gaad6be3d8dcd4ea01f8df436f41d158d4)
    and [mdb\_env\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga32a193c6bf4d7d5c5d579e71f22e9340).

<a id="x-28LMDB-3ACLOSE-ENV-20FUNCTION-29"></a>
<a id="LMDB:CLOSE-ENV%20FUNCTION"></a>

- \[function\] **close-env** *env \&key force*

    Close `env` and free the memory. Closing an already closed `env` has no effect.

    Since accessing [Transactions][bae2], [Databases][97e3] and
    [Cursors][eb36] after closing their environment would risk database
    curruption, `close-env` makes sure that they are not in use. There are
    two ways this can happen:

    - If `env` was opened `:synchronized` (see [`open-env`][bea1]), then `close-env`
      waits until there are no [active transaction][00c7]s in `env` before
      closing it. This requires synchronization and introduces some
      overhead, which might be noticable for workloads involving lots of
      quick read transactions. It is an [`lmdb-error`][b3a1] to attempt to close
      an environment in a [`with-txn`][fdc6] to avoid deadlocks.

    - On the other hand, if `synchronized` was `nil`, then - unless `force` is
      true - calling `close-env` signals an `lmdb-error` to avoid the
      [Safety][af06] issues involved in closing the environment.
      Environments opened with `:synchronized` `nil` are only closed when
      they are garbage collected and their finalizer is run. Still, for
      production it might be worth it to gain the last bit of
      performance.

    Wraps [mdb\_env\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga4366c43ada8874588b6a62fbda2d1e95).

<a id="x-28LMDB-3A-2AENV-2A-20VARIABLE-29"></a>
<a id="LMDB:*ENV*%20VARIABLE"></a>

- \[variable\] **\*env\*** *nil*

    The default [`env`][412f] for macros and function that take an environment
    argument.

<a id="x-28LMDB-3AWITH-ENV-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-ENV%20MGL-PAX:MACRO"></a>

- \[macro\] **with-env** *(env path \&rest open-env-args) \&body body*

    Bind the variable `env` to a new enviroment returned by [`open-env`][bea1]
    called with `path` and `open-env-args`, execute `body`, and [`close-env`][0b9a]. The
    following example binds the default environment:

    ```
    (with-env (*env* "/tmp/lmdb-test" :if-does-not-exist :create)
      ...)
    ```

<a id="x-28LMDB-3AOPEN-ENV-P-20FUNCTION-29"></a>
<a id="LMDB:OPEN-ENV-P%20FUNCTION"></a>

- \[function\] **open-env-p** *env*

    See if `env` is open, i.e. [`open-env`][bea1] has been called on it without a
    corresponding [`close-env`][0b9a].

<a id="x-28LMDB-3A-40LMDB-2FMISC-ENV-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FMISC-ENV%20MGL-PAX:SECTION"></a>

### 5.3 Miscellaneous environment functions

<a id="x-28LMDB-3ACHECK-FOR-STALE-READERS-20FUNCTION-29"></a>
<a id="LMDB:CHECK-FOR-STALE-READERS%20FUNCTION"></a>

- \[function\] **check-for-stale-readers** *\&optional (env \*env\*)*

    Check for stale entries in the reader lock table. See
    [Caveats](http://www.lmdb.tech/doc/). This function is called
    automatically by [`open-env`][bea1]. If other OS processes or threads
    accessing `env` abort without closing read transactions, call this
    function periodically to get rid off them. Alternatively, close all
    environments accessing the data file.

    Wraps [mdb\_reader\_check()](http://www.lmdb.tech/doc/group__mdb.html#ga366923d08bb384b3d9580a98edf5d668).

<a id="x-28LMDB-3AENV-STATISTICS-20FUNCTION-29"></a>
<a id="LMDB:ENV-STATISTICS%20FUNCTION"></a>

- \[function\] **env-statistics** *\&optional (env \*env\*)*

    Return statistics about `env` as a plist.

    - `:page-size`: The size of a database page in bytes.

    - `:depth`: The height of the B-tree.

    - `:branch-pages`: The number of internal (non-leaf) pages.

    - `:leaf-pages`: The number of leaf pages.

    - `:overflow-pages`: The number of overflow pages.

    - `:entries`: The number of data items.

    Wraps [mdb\_env\_stat()](http://www.lmdb.tech/doc/group__mdb.html#gaf881dca452050efbd434cd16e4bae255).

<a id="x-28LMDB-3AENV-INFO-20FUNCTION-29"></a>
<a id="LMDB:ENV-INFO%20FUNCTION"></a>

- \[function\] **env-info** *\&optional (env \*env\*)*

    Return information about `env` as a plist.

    - `:map-address`: Address of memory map, if fixed (see [`open-env`][bea1]'s
      `fixed-map`).

    - `:map-size`: Size of the memory map in bytes.

    - `:last-page-number`: Id of the last used page.

    - `:last-txn-id`: Id of the last committed transaction.

    - `:maximum-readers`: The number of reader slots.

    - `:n-readers`: The number of reader slots current used.

    Wraps [mdb\_env\_info()](http://www.lmdb.tech/doc/group__mdb.html#ga18769362c7e7d6cf91889a028a5c5947).

<a id="x-28LMDB-3ASYNC-ENV-20FUNCTION-29"></a>
<a id="LMDB:SYNC-ENV%20FUNCTION"></a>

- \[function\] **sync-env** *\&optional (env \*env\*)*

    Flush the data buffers to disk as in calling `fsync()`. When `env`
    had been opened with `:sync` `nil` or `:meta-sync` `nil`, this may be handy
    to force flushing the OS buffers to disk, which avoids potential
    durability and integrity issues.

    Wraps [mdb\_env\_sync()](http://www.lmdb.tech/doc/group__mdb.html#ga85e61f05aa68b520cc6c3b981dba5037).

<a id="x-28LMDB-3AENV-MAX-KEY-SIZE-20FUNCTION-29"></a>
<a id="LMDB:ENV-MAX-KEY-SIZE%20FUNCTION"></a>

- \[function\] **env-max-key-size** *\&optional (env \*env\*)*

    Return the maximum size of keys and [`dupsort`][186b] data in bytes. Depends
    on the compile-time constant `mdb_maxkeysize` in the C library. The
    default is 511. If this limit is exceeded [`lmdb-bad-valsize-error`][be19] is
    signalled.

    Wraps [mdb\_env\_get\_maxkeysize()](http://www.lmdb.tech/doc/group__mdb.html#gaaf0be004f33828bf2fb09d77eb3cef94).

<a id="x-28LMDB-3AWITH-TEMPORARY-ENV-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-TEMPORARY-ENV%20MGL-PAX:MACRO"></a>

- \[macro\] **with-temporary-env** *(env \&rest open-env-args) \&body body*

    Run `body` with an open temporary environment bound to `env`. In more
    detail, create an environment in a fresh temporary directory in an
    OS specific location. `open-env-args` is a list of keyword arguments
    and values for [`open-env`][bea1]. This macro is intended for testing and
    examples.

    ```
    (with-temporary-env (*env*)
      (let ((db (get-db "test")))
        (with-txn (:write t)
          (put db "k1" #(2 3))
          (print (g3t db "k1")) ; => #(2 3)
          (del db "k1"))))
    ```

    Since data corruption in temporary environments is not a concern,
    unlike [`with-env`][3c69], `with-temporary-env` closes the environment even if
    it was opened with `:synchronized` `nil` (see `open-env` and
    [`close-env`][0b9a]).

<a id="x-28LMDB-3A-40LMDB-2FTRANSACTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FTRANSACTIONS%20MGL-PAX:SECTION"></a>

## 6 Transactions

The `lmdb` environment supports transactional reads and writes. By
default, these provide the standard ACID (atomicity, consistency,
isolation, durability) guarantees. Writes from a transaction are not
immediately visible to other transactions. When the transaction is
committed, all its writes become visible atomically for future
transactions even if Lisp crashes or there is power failure. If the
transaction is aborted, its writes are discarded.

Transactions span the entire environment (see [`env`][412f]). All the updates
made in the course of an update transaction - writing records across
all databases, creating databases, and destroying databases - are
either completed atomically or rolled back.

Write transactions can be nested. Child transactions see the
uncommitted writes of their parent. The child transaction can commit
or abort, at which point its writes become visible to the parent
transaction or are discarded. If the parent aborts, all of the
writes performed in the context of the parent, including those from
committed child transactions, are discarded.

<a id="x-28LMDB-3AWITH-TXN-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-TXN%20MGL-PAX:MACRO"></a>

- \[macro\] **with-txn** *(\&key (env '\*env\*) write ignore-parent (sync t) (meta-sync t)) \&body body*

    Start a transaction in `env`, execute `body`. Then, if the transaction
    is open (see [`open-txn-p`][8edb]) and `body` returned normally, attempt to
    commit the transaction. Next, if `body` performed a non-local exit or
    committing failed, but the transaction is still open, then abort it.
    It is explicitly allowed to call [`commit-txn`][b473] or [`abort-txn`][f466] within
    `with-txn`.

    Transactions provide ACID guarantees (with `sync` and `meta-sync` both
    on). They span the entire environment, they are not specific to
    individual [`db`][3a5d].

    - If `write` is `nil`, the transaction is read-only and no writes (e.g.
      [`put`][edfe]) may be performed in the transaction. On the flipside, many
      read-only transactions can run concurrently (see [`env-max-readers`][d062]),
      while write transactions are mutually exclusive. Furthermore, the
      single write transaction can also run concurrently with read
      transactions, just keep in mind that read transactions hold on to
      the state of the environment at the time of their creation and
      thus prevent pages since replaced from being reused.

    - If `ignore-parent` is true, then in an enclosing `with-txn`, instead
      of creating a child transaction, start an independent transaction.

    - If `sync` is `nil`, then no flushing of buffers will take place after
      a commit as if the environment had been opened with `:sync` `nil`.

    - Likewise, `meta-sync` is the per-transaction equivalent of the
      [`open-env`][bea1]'s `meta-sync`.

    Also see [Nesting transactions][6ecf].

    Wraps [mdb\_txn\_begin()](http://www.lmdb.tech/doc/group__mdb.html#gad7ea55da06b77513609efebd44b26920).

<a id="x-28LMDB-3A-40ACTIVE-TRANSACTION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="LMDB:@ACTIVE-TRANSACTION%20MGL-PAX:GLOSSARY-TERM"></a>

- \[glossary-term\] **active transaction**

    The active transaction in some environment and thread is the
    transaction of the innermost [`with-txn`][fdc6] being executed in the thread
    that belongs to the environment. In most cases, this is simply the
    enclosing `with-txn`, but if `with-txn`s with different `:env` arguments
    are nested, then it may not be:

    ```
    (with-temporary-env (env)
      (let ((db (get-db "db" :env env)))
        (with-temporary-env (inner-env)
          (with-txn (:env env :write t)
            (with-txn (:env inner-env)
              (put db #(1) #(2)))))))
    ```

    In the above example, [`db`][3a5d] is known to belong to [`env`][412f] so although the
    immediately enclosing transaction belongs to INNER-ENV, [`put`][edfe] is
    executed in context of the outer, write transaction because that's
    the innermost in `env`.

    Operations that require a transaction always attempt to use the
    active transaction even if it is not open (see [`open-txn-p`][8edb]).

<a id="x-28LMDB-3AOPEN-TXN-P-20FUNCTION-29"></a>
<a id="LMDB:OPEN-TXN-P%20FUNCTION"></a>

- \[function\] **open-txn-p** *\&optional env*

    See if there is an active transaction and it is open, i.e.
    [`commit-txn`][b473] or [`abort-txn`][f466] have not been called on it. Also, [`reset-txn`][ad52]
    without a corresponding [`renew-txn`][6817] closes the transaction.

<a id="x-28LMDB-3ATXN-ID-20FUNCTION-29"></a>
<a id="LMDB:TXN-ID%20FUNCTION"></a>

- \[function\] **txn-id**

    The ID of `txn`. IDs are integers incrementing from 1. For a
    read-only transaction, this corresponds to the snapshot being read;
    concurrent readers will frequently have the same transaction ID.
    Only committed write transactions increment the ID. If a transaction
    aborts, the ID may be re-used by the next writer.

<a id="x-28LMDB-3ACOMMIT-TXN-20FUNCTION-29"></a>
<a id="LMDB:COMMIT-TXN%20FUNCTION"></a>

- \[function\] **commit-txn** *\&optional env*

    Commit the innermost enclosig transaction (or [active transaction][00c7]
    belonging to `env` if `env` is specified) or signal an error if it is
    not open. If `txn` is not nested in another transaction, committing
    makes updates performed visible to future transactions. If `txn` is a
    child transaction, then committing makes updates visible to its
    parent only. For read-only transactions, committing releases the
    reference to a historical version environment, allowing reuse of
    pages replaced since.

    Wraps [mdb\_txn\_commit()](http://www.lmdb.tech/doc/group__mdb.html#ga846fbd6f46105617ac9f4d76476f6597).

<a id="x-28LMDB-3AABORT-TXN-20FUNCTION-29"></a>
<a id="LMDB:ABORT-TXN%20FUNCTION"></a>

- \[function\] **abort-txn** *\&optional env*

    Close `txn` by discarding all updates performed, which will then not
    be visible to either parent or future transactions. Aborting an
    already closed transaction is a noop. Always succeeds.

    Wraps [mdb\_txn\_abort()](http://www.lmdb.tech/doc/group__mdb.html#ga73a5938ae4c3239ee11efa07eb22b882).

<a id="x-28LMDB-3ARENEW-TXN-20FUNCTION-29"></a>
<a id="LMDB:RENEW-TXN%20FUNCTION"></a>

- \[function\] **renew-txn** *\&optional env*

    Renew `txn` that was reset by [`reset-txn`][ad52]. This acquires a new reader
    lock that had been released by `reset-txn`. After renewal, it is as if
    `txn` had just been started.

    Wraps [mdb\_txn\_renew()](http://www.lmdb.tech/doc/group__mdb.html#ga6c6f917959517ede1c504cf7c720ce6d).

<a id="x-28LMDB-3ARESET-TXN-20FUNCTION-29"></a>
<a id="LMDB:RESET-TXN%20FUNCTION"></a>

- \[function\] **reset-txn** *\&optional env*

    Abort the open, read-only `txn`, release the reference to the
    historical version of the environment, but make it faster to start
    another read-only transaction with [`renew-txn`][6817]. This is accomplished
    by not deallocating some data structures, and keeping the slot in
    the reader table. Cursors opened within the transaction must not be
    used again, except if renewed (see RENEW-CURSOR). If `txn` is an open,
    read-only transaction, this function always succeeds.

    Wraps [mdb\_txn\_reset()](http://www.lmdb.tech/doc/group__mdb.html#ga02b06706f8a66249769503c4e88c56cd).

<a id="x-28LMDB-3A-40LMDB-2FNESTING-TRANSACTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FNESTING-TRANSACTIONS%20MGL-PAX:SECTION"></a>

### 6.1 Nesting transactions

When [`with-txn`][fdc6]s are nested (i.e. one is executed in the dynamic
extent of another), we speak of nested transactions. Transaction can
be nested to arbitrary levels. Child transactions may be committed
or aborted independently from their parent transaction (the
immediately enclosing `with-txn`). Committing a child transaction only
makes the updates made by it visible to the parent. If the parent
then aborts, the child's updates are aborted too. If the parent
commits, all child transactions that were not aborted are committed,
too.

Actually, the C lmdb library only supports nesting write
transactions. To simplify usage, the Lisp side turns read-only
`with-txn`s nested in another `with-txn`s into noops.

```
(with-temporary-env (*env*)
  (let ((db (get-db "test" :value-encoding :uint64)))
    ;; Create a top-level write transaction.
    (with-txn (:write t)
      (put db "p" 0)
      ;; First child transaction
      (with-txn (:write t)
        ;; Writes of the parent are visible in children.
        (assert (= (g3t db "p") 0))
        (put db "c1" 1))
      ;; Parent sees what the child committed (but it's not visible to
      ;; unrelated transactions).
      (assert (= (g3t db "c1") 1))
      ;; Second child transaction
      (with-txn (:write t)
        ;; Sees writes from the parent that came from the first child.
        (assert (= (g3t db "c1") 1))
        (put db "c1" 2)
        (put db "c2" 2)
        (abort-txn)))
    ;; Create a top-level read transaction to check what was committed.
    (with-txn ()
      ;; Since the second child aborted, its writes are discarded.
      (assert (= (g3t db "p") 0))
      (assert (= (g3t db "c1") 1))
      (assert (null (g3t db "c2"))))))
```

[`commit-txn`][b473], [`abort-txn`][f466], and [`reset-txn`][ad52] all close the
[active transaction][00c7] (see [`open-txn-p`][8edb]). When the active transaction is
not open, database operations such as [`g3t`][13e8], [`put`][edfe], [`del`][6237] signal
[`lmdb-bad-txn-error`][749e]. Furthermore, any [Cursors][eb36] created in the
context of the transaction will no longer be valid (but see
[`cursor-renew`][5578]).

An `lmdb` parent transaction and its cursors must not issue operations
other than `commit-txn` and `abort-txn` while there are active child
transactions. As the Lisp side does not expose transaction objects
directly, performing [Basic operations][f63b] in the parent
transaction is not possible, but it is possible with [Cursors][eb36]
as they are tied to the transaction in which they were created.

`ignore-parent` true overrides the default nesting semantics of
`with-txn` and creates a new top-level transaction, which is not a
child of the enclosing `with-txn`.

- Since `lmdb` is single-writer, on nesting an `ignore-parent` write
  transaction in another write transaction, `lmdb-bad-txn-error` is
  signalled to avoid the deadlock.

- Nesting a read-only `with-txn` with `ignore-parent` in another
  read-only `with-txn` is [`lmdb-bad-rslot-error`][4880] error with the `tls`
  option because it would create two read-only transactions in the
  same thread.

Nesting a read transaction in another transaction would be an
`lmdb-bad-rslot-error` according to the C lmdb library, but a
read-only `with-txn` with `ignore-parent` `nil` nested in another `with-txn`
is turned into a noop so this edge case is papered over.

<a id="x-28LMDB-3A-40LMDB-2FDATABASES-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FDATABASES%20MGL-PAX:SECTION"></a>

## 7 Databases

<a id="x-28LMDB-3A-40LMDB-2FTHE-UNNAMED-DATABASE-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FTHE-UNNAMED-DATABASE%20MGL-PAX:SECTION"></a>

### 7.1 The unnamed database

`lmdb` has a default, unnamed database backed by a B+ tree. This db
can hold normal key-value pairs and named databases. The unnamed
database can be accessed by passing `nil` as the database name to
[`get-db`][f306]. There are some restrictions on the flags of the unnamed
database, see [`lmdb-incompatible-error`][12a5].

<a id="x-28LMDB-3A-40DUPSORT-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@DUPSORT%20MGL-PAX:SECTION"></a>

### 7.2 `dupsort`

A prominent feature of `lmdb` is the ability to associate multiple
sorted values with keys, which is enabled by the `dupsort` argument of
[`get-db`][f306]. Just as a named database is a B+ tree associated with a
key (its name) in the B+ tree of the unnamed database, so do these
sorted duplicates form a B+ tree under a key in a named or the
unnamed database. Among the [Basic operations][f63b], [`put`][edfe] and [`del`][6237] are
equipped to deal with duplicate values, but [`g3t`][13e8] is too limited, and
[Cursors][eb36] are needed to make full use of `dupsort`.

When using this feature the limit on the maximum key size applies to
duplicate data, as well. See [`env-max-key-size`][47fd].

<a id="x-28LMDB-3A-40LMDB-2FDATABASE-API-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FDATABASE-API%20MGL-PAX:SECTION"></a>

### 7.3 Database API

<a id="x-28LMDB-3A-2ADB-CLASS-2A-20VARIABLE-29"></a>
<a id="LMDB:*DB-CLASS*%20VARIABLE"></a>

- \[variable\] **\*db-class\*** *db*

    The default class that [`get-db`][f306] instantiates. Must a subclass of [`db`][3a5d].
    This provides a way to associate application specific data with `db`
    objects.

<a id="x-28LMDB-3AGET-DB-20FUNCTION-29"></a>
<a id="LMDB:GET-DB%20FUNCTION"></a>

- \[function\] **get-db** *name \&key (class \*db-class\*) (env \*env\*) (if-does-not-exist :create) key-encoding value-encoding integer-key reverse-key dupsort integer-dup reverse-dup dupfixed*

    Open the database with `name` in the open environment `env`, and return
    a [`db`][3a5d] object. If `name` is `nil`, then the [The unnamed database][a14f] is
    opened.

    If `get-db` is called with the same name multiple times, the returned
    `db` objects will be associated with the same database (although they
    may not be [`eq`][5a82]). The first time `get-db` is called with any given name
    and environment, it must not be from an open transaction. This is
    because `get-db` starts a transaction itself to comply with C lmdb's
    requirements on
    [mdb\_dbi\_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a) (see
    [Safety][af06]). Since dbi handles are cached within `env`, subsequent
    calls do not involve `mdb_dbi_open()` and are thus permissible
    within transactions.

    `class` designates the class which will instantiated. See [`*db-class*`][2462].

    If `if-does-not-exist` is `:create`, then a new named database is
    created. If `if-does-not-exist` is `:error`, then an error is signalled
    if the database does not exists.

    `key-encoding` and `value-encoding` are both one of `nil`, `:uint64`,
    `:octets` or `:utf-8`. `key-encoding` is set to `:uint64` when `integer-key`
    is true. `value-encoding` is set to `:uint64` when `integer-dup` is true.
    Note that changing the encoding does *not* reencode already existing
    data. See [Encoding and decoding data][03b4] for the full semantics.

    `get-db` may be called more than once with the same `name` and `env`, and
    the returned `db` objects will have the same underlying C lmdb
    database, but they may have different `key-encoding` and
    `value-encoding`.

    The following flags are for database creation, they do not have any
    effect in subsequent calls (except for the
    [The unnamed database][a14f]).

    - `integer-key`: Keys in the database are C `unsigned` or `size_t`
      integers encoded in native byte order. Keys must all be either
      `unsigned` or `size_t`, they cannot be mixed in a single database.

    - `reverse-key`: Keys are strings to be compared in reverse order,
      from the end of the strings to the beginning. By default, keys are
      treated as strings and compared from beginning to end.

    - `dupsort`: Duplicate keys may be used in the database (or, from
      another perspective, keys may have multiple values, stored in
      sorted order). By default, keys must be unique and may have only a
      single value. Also, see [`dupsort`][186b].

    - `integer-dup`: This option specifies that duplicate data items are
      binary integers, similarly to `integer-key`. Only matters if
      `dupsort`.

    - `reverse-dup`: This option specifies that duplicate data items
      should be compared as strings in reverse order. Only matters if
      `dupsort`.

    - `dupfixed`: This flag may only be used in combination `dupsort`. When
      true, data items for this database must all be the same size,
      which allows further optimizations in storage and retrieval.
      Currently, the wrapper functions that could take advantage of
      this (e.g. [`put`][edfe], [`cursor-put`][a56d], [`cursor-next`][d4f9] and [`cursor-value`][5902]), do not.

    No function to close a database (an equivalent to
    [mdb\_dbi\_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5))
    is provided due to subtle races and corruption it could cause when
    an `MDB_dbi` (unsigned integer, similar to an fd) is assigned by a
    subsequent open to another named database.

    Wraps [mdb\_dbi\_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a).

<a id="x-28LMDB-3ADB-20CLASS-29"></a>
<a id="LMDB:DB%20CLASS"></a>

- \[class\] **db**

    A database in an environment (class [`env`][412f]). Always to
    be created by [`get-db`][f306].

<a id="x-28LMDB-3ADB-NAME-20-28MGL-PAX-3AREADER-20LMDB-3ADB-29-29"></a>
<a id="LMDB:DB-NAME%20%28MGL-PAX:READER%20LMDB:DB%29"></a>

- \[reader\] **db-name** *[db][3a5d] (:name)*

    The name of the database.

<a id="x-28LMDB-3ADB-KEY-ENCODING-20-28MGL-PAX-3AREADER-20LMDB-3ADB-29-29"></a>
<a id="LMDB:DB-KEY-ENCODING%20%28MGL-PAX:READER%20LMDB:DB%29"></a>

- \[reader\] **db-key-encoding** *[db][3a5d] (:key-encoding)*

    The [`encoding`][5488] that was passed as `key-encoding` to
    [`get-db`][f306].

<a id="x-28LMDB-3ADB-VALUE-ENCODING-20-28MGL-PAX-3AREADER-20LMDB-3ADB-29-29"></a>
<a id="LMDB:DB-VALUE-ENCODING%20%28MGL-PAX:READER%20LMDB:DB%29"></a>

- \[reader\] **db-value-encoding** *[db][3a5d] (:value-encoding)*

    The [`encoding`][5488] that was passed as `value-encoding`
    to [`get-db`][f306].

<a id="x-28LMDB-3ADROP-DB-20FUNCTION-29"></a>
<a id="LMDB:DROP-DB%20FUNCTION"></a>

- \[function\] **drop-db** *name path \&key open-env-args (delete t)*

    Empty the database with `name` in the environment denoted by `path`. If
    `delete`, then delete the database. Since closing a database is
    dangerous (see [`get-db`][f306]), `drop-db` opens and closes the environment
    itself.

    Wraps [mdb\_drop()](http://www.lmdb.tech/doc/group__mdb.html#gab966fab3840fc54a6571dfb32b00f2db).

<a id="x-28LMDB-3ADB-STATISTICS-20FUNCTION-29"></a>
<a id="LMDB:DB-STATISTICS%20FUNCTION"></a>

- \[function\] **db-statistics** *db*

    Return statistics about the database.

    Wraps [mdb\_stat()](http://www.lmdb.tech/doc/group__mdb.html#gae6c1069febe94299769dbdd032fadef6).

<a id="x-28LMDB-3A-40LMDB-2FENCODINGS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FENCODINGS%20MGL-PAX:SECTION"></a>

## 8 Encoding and decoding data

In the C lmdb library, keys and values are opaque byte vectors
only ever inspected internally to maintain the sort order (of keys
and also duplicate values if [`dupsort`][186b]). The client is given the
freedom and the responsibility to choose how to perform conversion
to and from byte vectors.

`lmdb` exposes this full flexibility while at the same time providing
reasonable defaults for the common cases. In particular, with the
`key-encoding` and `value-encoding` arguments of [`get-db`][f306], the
data (meaning the key or value here) encoding can be declared
explicitly.

Even if the encoding is undeclared, it is recommended to use a
single type for keys (and duplicate values) to avoid unexpected
conflicts that could arise, for example, when the UTF-8 encoding of
a string and the `:uint64` encoding of an integer coincide. The same
consideration doubly applies to named databases, which share the key
space with normal key-value pairs in the default database (see
[The unnamed database][a14f]).

Together, `:uint64` and `:utf-8` cover the common cases for keys. They
trade off dynamic typing for easy sortability (using the default C
lmdb behaviour). On the other hand, when sorting is not
concern (either for keys and values), serialization may be done more
freely. For this purpose, using an encoding of `:octets` or `nil` with
[cl-conspack](https://github.com/conspack/cl-conspack) is
recommended because it works with complex objects, it encodes object
types, it is fast and space-efficient, has a stable specification
and an alternative implementation in C. For example:

```
(with-temporary-env (*env*)
  (let ((db (get-db "test")))
    (with-txn (:write t)
      (put db "key1" (cpk:encode (list :some "stuff" 42)))
      (cpk:decode (g3t db "key1")))))
=> (:SOME "stuff" 42)
```

Note that multiple [`db`][3a5d] objects with different encodings can be
associated with the same C lmdb database, which declutters the code:

```
(defvar *cpk-encoding*
  (cons #'cpk:encode (alexandria:compose #'cpk:decode #'mdb-val-to-octets)))

(with-temporary-env (*env*)
  (let ((next-id-db (get-db "test" :key-encoding *cpk-encoding*
                                   :value-encoding :uint64))
        (db (get-db "test" :key-encoding *cpk-encoding*
                           :value-encoding *cpk-encoding*)))
    (with-txn (:write t)
      (let ((id (or (g3t next-id-db :next-id) 0)))
        (put next-id-db :next-id (1+ id))
        (put db id (list :some "stuff" 42))
        (g3t db id)))))
=> (:SOME "stuff" 42)
=> T
```

<a id="x-28LMDB-3AENCODING-20TYPE-29"></a>
<a id="LMDB:ENCODING%20TYPE"></a>

- \[type\] **encoding**

    The following values are supported:

    - `:uint64`: Data to be encoded must be of type `(unsigned-byte 64)`,
      which is then encoded as an 8 byte array in *native* byte order
      with [`uint64-to-octets`][7c81]. The reverse transformation takes place when
      returning values. This is the encoding used for `integer-key` and
      `integer-dup` [`db`][3a5d]s.

    - `:octets`: Note the plural. Data to be encoded (e.g. `key` argument of
      [`g3t`][13e8]) must be a 1D byte array. If its element type
      is `(unsigned-byte 8)`, then the data can be passed to the foreign
      code more efficiently, but declaring the element type is not
      required. For example, [`vector`][6098]s can be used as long as the
      actual elements are of type `(unsigned-byte 8)`. Foreign byte
      arrays to be decoded (e.g. the value returned by `g3t`) are returned
      as [`octets`][b9c5].

    - `:utf-8`: Data to be encoded must be a string, which is converted to
      octets by TRIVIAL-UTF-8. Null-terminated. Foreign byte arrays are
      decoded the same way.

    - `nil`: Data is encoded using the default encoding according to its
      Lisp type: strings as `:utf-8`, vectors as `:octets`, `(unsigned-byte
      64)` as `:uint64`. Decoding is always performed as `:octets`.

    - A [`cons`][a237]: Data is encoded by the function in the [`car`][d5a2] of the
      cons and decoded by the function in the [`cdr`][e012]. For example, `:uint64`
      is equivalent to `(cons #'uint64-to-octets #'mdb-val-to-uint64)`.

<a id="x-28LMDB-3AWITH-MDB-VAL-SLOTS-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-MDB-VAL-SLOTS%20MGL-PAX:MACRO"></a>

- \[macro\] **with-mdb-val-slots** *(%bytes size mdb-val) \&body body*

    Bind `%bytes` and `size` locally to the corresponding slots of `mdb-val`.
    `mdb-val` is an opaque handle for a foreign `MDB_val` struct, that
    holds the pointer to a byte array and the number of bytes in the
    array. This macro is needed to access the foreign data in a function
    used as [`*key-decoder*`][d4a7] or [`*value-decoder*`][ac19]. `mdb-val` is dynamic extent,
    so don't hold on to it. Also, the pointer to which `%bytes` is bound
    is valid only within the context of current top-level transaction.

<a id="x-28LMDB-3AOCTETS-20TYPE-29"></a>
<a id="LMDB:OCTETS%20TYPE"></a>

- \[type\] **octets** *\&optional (size '\*)*

    A 1D [`simple-array`][451a] of `(unsigned-byte 8)`.

<a id="x-28LMDB-3AMDB-VAL-TO-OCTETS-20FUNCTION-29"></a>
<a id="LMDB:MDB-VAL-TO-OCTETS%20FUNCTION"></a>

- \[function\] **mdb-val-to-octets** *mdb-val*

    A utility function provided for writing [`*key-decoder*`][d4a7] and
    [`*value-decoder*`][ac19] functions. It returns a Lisp octet vector that holds
    the same bytes as `mdb-val`.

<a id="x-28LMDB-3AUINT64-TO-OCTETS-20FUNCTION-29"></a>
<a id="LMDB:UINT64-TO-OCTETS%20FUNCTION"></a>

- \[function\] **uint64-to-octets** *n*

    Convert an `(unsigned-byte 64)` to [`octets`][b9c5] of length 8 taking the
    native byte order representation of `n`. Suitable as a [`*key-encoder*`][13f2]
    or [`*value-encoder*`][19d5].

<a id="x-28LMDB-3AOCTETS-TO-UINT64-20FUNCTION-29"></a>
<a id="LMDB:OCTETS-TO-UINT64%20FUNCTION"></a>

- \[function\] **octets-to-uint64** *octets*

    The inverse of [`uint64-to-octets`][7c81]. Use [`mdb-val-to-uint64`][f3ff] as a
    [`*key-decoder*`][d4a7] or [`*value-decoder*`][ac19].

<a id="x-28LMDB-3AMDB-VAL-TO-UINT64-20FUNCTION-29"></a>
<a id="LMDB:MDB-VAL-TO-UINT64%20FUNCTION"></a>

- \[function\] **mdb-val-to-uint64** *mdb-val*

    Like [`octets-to-uint64`][5d07], but suitable for [`*key-decoder*`][d4a7] or
    [`*value-decoder*`][ac19] that decodes unsigned 64 bit integers in native byte
    order. This function is called automatically when the encoding is
    known to require it (see [`get-db`][f306]'s `integer-key`, `:value-encoding`,
    etc).

<a id="x-28LMDB-3ASTRING-TO-OCTETS-20FUNCTION-29"></a>
<a id="LMDB:STRING-TO-OCTETS%20FUNCTION"></a>

- \[function\] **string-to-octets** *string*

    Convert `string` to [`octets`][b9c5] by encoding it as UTF-8 with null
    termination. Suitable as a [`*key-encoder*`][13f2] or [`*value-encoder*`][19d5].

<a id="x-28LMDB-3AOCTETS-TO-STRING-20FUNCTION-29"></a>
<a id="LMDB:OCTETS-TO-STRING%20FUNCTION"></a>

- \[function\] **octets-to-string** *octets*

    The inverse of [`string-to-octets`][3582]. Use [`mdb-val-to-string`][2a35] as a
    [`*key-decoder*`][d4a7] or [`*value-decoder*`][ac19].

<a id="x-28LMDB-3AMDB-VAL-TO-STRING-20FUNCTION-29"></a>
<a id="LMDB:MDB-VAL-TO-STRING%20FUNCTION"></a>

- \[function\] **mdb-val-to-string** *mdb-val*

    Like [`octets-to-string`][f6fc], but suitable as a [`*key-decoder*`][d4a7] or
    [`*value-decoder*`][ac19].

<a id="x-28LMDB-3A-40LMDB-2FOVERRIDING-ENCODINGS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FOVERRIDING-ENCODINGS%20MGL-PAX:SECTION"></a>

### 8.1 Overriding encodings

Using multiple [`db`][3a5d] objects with different encodings is the
recommended practice (see the example in [Encoding and decoding data][03b4]), but when
that is inconvenient, one can override the encodings with the
following variables.

<a id="x-28LMDB-3A-2AKEY-ENCODER-2A-20VARIABLE-29"></a>
<a id="LMDB:*KEY-ENCODER*%20VARIABLE"></a>

- \[variable\] **\*key-encoder\*** *nil*

    A function designator, `nil` or an [`encoding`][5488]. If non-`nil`, it overrides
    the encoding method determined by `key-encoding` (see [`get-db`][f306]). It is
    called with a single argument, the key, when it is to be converted
    to an octet vector.

<a id="x-28LMDB-3A-2AKEY-DECODER-2A-20VARIABLE-29"></a>
<a id="LMDB:*KEY-DECODER*%20VARIABLE"></a>

- \[variable\] **\*key-decoder\*** *nil*

    A function designator, `nil` or an [`encoding`][5488]. If non-`nil`, it is
    called with a single `mdb-val` argument (see [`with-mdb-val-slots`][e84c]), that
    holds a pointer to data to be decoded and its size. This function is
    called whenever a key is to be decoded and overrides the
    `key-encoding` argument of [`get-db`][f306].

    For example, if we are only interested in the length of the value
    and want to avoid creating a lisp vector on the heap, we can do
    this:

    ```
    (with-temporary-env (*env*)
      (let ((db (get-db "test")))
        (with-txn (:write t)
          (put db "key1" "abc")
          (let ((*value-decoder* (lambda (mdb-val)
                                   (with-mdb-val-slots (%bytes size mdb-val)
                                     (declare (ignore %bytes))
                                     ;; Take null termination into account.
                                     (1- size)))))
            (g3t db "key1")))))
    => 3
    => T
    ```

<a id="x-28LMDB-3A-2AVALUE-ENCODER-2A-20VARIABLE-29"></a>
<a id="LMDB:*VALUE-ENCODER*%20VARIABLE"></a>

- \[variable\] **\*value-encoder\*** *nil*

    Like [`*key-encoder*`][13f2], but for values.

<a id="x-28LMDB-3A-2AVALUE-DECODER-2A-20VARIABLE-29"></a>
<a id="LMDB:*VALUE-DECODER*%20VARIABLE"></a>

- \[variable\] **\*value-decoder\*** *nil*

    Like [`*key-decoder*`][d4a7], but for values.

    Apart from performing actual decoding, the main purpose of
    `*value-decoder*`, one can also pass the foreign data on to other
    foreign functions such as `write()` directly from the decoder
    function and returning a constant such as `t` to avoid consing.

<a id="x-28LMDB-3A-40LMDB-2FBASIC-OPERATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FBASIC-OPERATIONS%20MGL-PAX:SECTION"></a>

## 9 Basic operations

<a id="x-28LMDB-3AG3T-20FUNCTION-29"></a>
<a id="LMDB:G3T%20FUNCTION"></a>

- \[function\] **g3t** *db key*

    Return the value from `db` associated with `key` and `t` as the second
    value. If `key` is not found in `db`, then `nil` is returned. If `db`
    supports [`dupsort`][186b], then the first value for `key` will be returned.
    Retrieval of other values requires the use of [Cursors][eb36].

    This function is called `g3t` instead of `get` to avoid
    having to shadow `cl:get` when importing the `lmdb` package. On the
    other hand, importing the LMDB+ package, which has `lmdb::get`
    exported, requires some shadowing.

    The LMDB+ package is like the `lmdb` package, but it has `#'lmdb:g3t`
    fbound to `lmdb+:g3t` so it probably needs shadowing to avoid conflict
    with `cl:get`:

    ```
    (defpackage lmdb/test
      (:shadow #:get)
      (:use #:cl #:lmdb+))
    ```

    Wraps [mdb\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga8bf10cd91d3f3a83a34d04ce6b07992d).

<a id="x-28LMDB-3APUT-20FUNCTION-29"></a>
<a id="LMDB:PUT%20FUNCTION"></a>

- \[function\] **put** *db key value \&key (overwrite t) (dupdata t) append append-dup (key-exists-error-p t)*

    Add a `key`, `value` pair to `db` within `txn` (which must support writes).
    Returns `t` on success.

    - `overwrite`: If `nil`, signal [`lmdb-key-exists-error`][8720] if `key` already
      appears in `db`.

    - `dupdata`: If `nil`, signal `lmdb-key-exists-error` if the `key`, `value`
      pair already appears in `db`. Has no effect if `db` doesn't have
      `dupsort`.

    - `append`: Append the `key`, `value` pair to the end of `db` instead of
      finding `key`'s location in the B+ tree by performing comparisons.
      The client effectively promises that keys are inserted in sort
      order, which allows for fast bulk loading. If the promise is
      broken, a `lmdb-key-exists-error` is signalled.

    - `append-dup`: The client promises that duplicate values are inserted
      in sort order. If the promise is broken, a `lmdb-key-exists-error`
      is signalled.

    - If `key-exists-error-p` is `nil`, then instead of signalling
      `lmdb-key-exists-error` return `nil`.

    May signal [`lmdb-map-full-error`][f710], [`lmdb-txn-full-error`][2070],
    [`lmdb-txn-read-only-error`][e274].

    Wraps [mdb\_put()](http://www.lmdb.tech/doc/group__mdb.html#ga4fa8573d9236d54687c61827ebf8cac0).

<a id="x-28LMDB-3ADEL-20FUNCTION-29"></a>
<a id="LMDB:DEL%20FUNCTION"></a>

- \[function\] **del** *db key \&key value*

    Delete `key` from `db`. Returns `t` if data was deleted, `nil` otherwise.
    If `db` supports sorted duplicates ([`dupsort`][186b]), then `value` is taken
    into account: if it's `nil`, then all duplicate values for `key` are
    deleted, if it's not `nil`, then only the matching value. May signal
    [`lmdb-txn-read-only-error`][e274].

    Wraps [mdb\_del()](http://www.lmdb.tech/doc/group__mdb.html#gab8182f9360ea69ac0afd4a4eaab1ddb0).

<a id="x-28LMDB-3A-40LMDB-2FCURSORS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FCURSORS%20MGL-PAX:SECTION"></a>

## 10 Cursors

<a id="x-28LMDB-3AWITH-CURSOR-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-CURSOR%20MGL-PAX:MACRO"></a>

- \[macro\] **with-cursor** *(var db) \&body body*

    Bind `var` to a fresh [`cursor`][1306] on `db`. Execute `body`, then close the
    cursor. Within the dynamic extent of `body`, this will be the
    [default cursor][d997]. The cursor is tied to the [active transaction][00c7].

    [`lmdb-cursor-thread-error`][2b7a] is signalled if the cursor is accessed from
    threads other than the one in which it was created.

    Wraps [mdb\_cursor\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
    and [mdb\_cursor\_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188).

<a id="x-28LMDB-3AWITH-IMPLICIT-CURSOR-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:WITH-IMPLICIT-CURSOR%20MGL-PAX:MACRO"></a>

- \[macro\] **with-implicit-cursor** *(db) \&body body*

    Like [`with-cursor`][b1c7] but the cursor object is not accessible directly,
    only through the [default cursor][d997] mechanism. The cursor is
    stack-allocated, which eliminates the consing of `with-cursor`. Note
    that stack allocation of cursors in `with-cursor` would risk data
    corruption if the cursor were accessed beyond its dynamic extent.

    Use `with-implicit-cursor` instead of `with-cursor` if a single cursor
    at a time will suffice. Conversely, use `with-cursor` if a second
    cursor is needed. That is, use

    ```
    (with-implicit-cursor (db)
      (cursor-set-key 1))
    ```

    but when two cursors iterate in an interleaved manner, use
    `with-cursor`:

    ```
    (with-cursor (c1 db)
      (with-cursor (c2 db)
        (cursor-first c1)
        (cursor-last c2)
        (if (some-pred (cursor-value c1) (cursor-value c2))
            (cursor-next c1)
            (cursor-prev c2))
        ...))
    ```

    Wraps [mdb\_cursor\_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
    and [mdb\_cursor\_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188).

<a id="x-28LMDB-3ACURSOR-20STRUCTURE-29"></a>
<a id="LMDB:CURSOR%20STRUCTURE"></a>

- \[structure\] **cursor**

<a id="x-28LMDB-3ACURSOR-DB-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20LMDB-3ACURSOR-29-29"></a>
<a id="LMDB:CURSOR-DB%20%28MGL-PAX:STRUCTURE-ACCESSOR%20LMDB:CURSOR%29"></a>

- \[structure-accessor\] **cursor-db** *cursor*

<a id="x-28LMDB-3A-40DEFAULT-CURSOR-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="LMDB:@DEFAULT-CURSOR%20MGL-PAX:GLOSSARY-TERM"></a>

- \[glossary-term\] **default cursor**

    All operations, described below, that take cursor arguments accept
    `nil` instead of a [`cursor`][1306] object, in which case the cursor from the
    immediately enclosing [`with-cursor`][b1c7] or [`with-implicit-cursor`][7def] is used.
    This cursor is referred to as the *default cursor*.

    To reduce syntactic clutter, some operations thus make cursor
    arguments [`&optional`][4336]. When this is undesirable - because there are
    keyword arguments as well - the cursor may be a required argument as
    in [`cursor-put`][a56d]. Still `nil` can be passed explicitly.

<a id="x-28LMDB-3A-40LMDB-2FPOSITIONING-CURSORS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FPOSITIONING-CURSORS%20MGL-PAX:SECTION"></a>

### 10.1 Positioning cursors

The following functions *position* or *initialize* a cursor while
returning the value (*a* value with [`dupsort`][186b]) associated with a key,
or both the key and the value. Initialization is successful if there
is the cursor points to a key-value pair, which is indicated by the
last return value being `t`.

<a id="x-28LMDB-3ACURSOR-FIRST-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-FIRST%20FUNCTION"></a>

- \[function\] **cursor-first** *\&optional cursor*

    Move `cursor` to the first key of its database. Return the key, the
    value and `t`, or `nil` if the database is empty. If [`dupsort`][186b], position
    `cursor` on the first value of the first key.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_FIRST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-FIRST-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-FIRST-DUP%20FUNCTION"></a>

- \[function\] **cursor-first-dup** *\&optional cursor*

    Move `cursor` to the first duplicate value of the current key. Return
    the value and `t`. Return `nil` if `cursor` is not positioned.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_FIRST\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-LAST-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-LAST%20FUNCTION"></a>

- \[function\] **cursor-last** *\&optional cursor*

    Move `cursor` to the last key of its database. Return the key, the
    value and `t`, or `nil` if the database is empty. If [`dupsort`][186b], position
    `cursor` on the last value of the last key.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_LAST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-LAST-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-LAST-DUP%20FUNCTION"></a>

- \[function\] **cursor-last-dup** *\&optional cursor*

    Move `cursor` to the last duplicate value of the current key. Return
    the value and `t`. Return `nil` if `cursor` is not positioned.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_LAST\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-NEXT-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-NEXT%20FUNCTION"></a>

- \[function\] **cursor-next** *\&optional cursor*

    Move `cursor` to the next key-value pair of its database and return
    the key, the value, and `t`. Return `nil` if there is no next item. If
    [`dupsort`][186b], position `cursor` on the next value of the current key if
    exists, else the first value of next key. If `cursor` is
    uninitialized, then [`cursor-first`][fa3d] is called on it first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-NEXT-NODUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-NEXT-NODUP%20FUNCTION"></a>

- \[function\] **cursor-next-nodup** *\&optional cursor*

    Move `cursor` to the first value of next key pair of its
    database (skipping over duplicate values of the current key). Return
    the key, the value and `t`. Return `nil` if there is no next item. If
    `cursor` is uninitialized, then [`cursor-first`][fa3d] is called on it first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT\_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-NEXT-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-NEXT-DUP%20FUNCTION"></a>

- \[function\] **cursor-next-dup** *\&optional cursor*

    Move `cursor` to the next value of current key pair of its database.
    Return the value and `t`. Return `nil` if there is no next value. If
    `cursor` is uninitialized, then [`cursor-first`][fa3d] is called on it first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_NEXT\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-PREV-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-PREV%20FUNCTION"></a>

- \[function\] **cursor-prev** *\&optional cursor*

    Move `cursor` to the previous key-value pair of its database.
    Return the key, the value and `t`. Return `nil` if there is no previous
    item. If [`dupsort`][186b], position `cursor` on the previous value of the
    current key if exists, else the last value of previous key. If
    `cursor` is uninitialized, then [`cursor-last`][7269] is called on it first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-PREV-NODUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-PREV-NODUP%20FUNCTION"></a>

- \[function\] **cursor-prev-nodup** *\&optional cursor*

    Move `cursor` to the last value of previous key pair of its
    database (skipping over duplicate values of the current and the
    previous key). Return the key, the value, and `t`. Return `nil` if
    there is no prev item. If `cursor` is uninitialized, then [`cursor-last`][7269]
    is called on it first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV\_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-PREV-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-PREV-DUP%20FUNCTION"></a>

- \[function\] **cursor-prev-dup** *\&optional cursor*

    Move `cursor` to the previous duplicate value of current key pair of
    its database. Return the value and `t`. Return `nil` if there is no prev
    value. If `cursor` is uninitialized, then [`cursor-last`][7269] is called on it
    first.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_PREV\_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-SET-KEY-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-SET-KEY%20FUNCTION"></a>

- \[function\] **cursor-set-key** *key \&optional cursor*

    Move `cursor` to `key` of its database. Return the corresponding value
    and `t`. Return `nil` if `key` was not found. If [`dupsort`][186b], position `cursor`
    on the first value of `key`.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_SET\_KEY](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-SET-KEY-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-SET-KEY-DUP%20FUNCTION"></a>

- \[function\] **cursor-set-key-dup** *key value \&optional cursor*

    Move `cursor` to the `key`, `value` pair of its database and return `t` on
    success. Return `nil` if the pair was not found.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_BOTH](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-SET-RANGE-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-SET-RANGE%20FUNCTION"></a>

- \[function\] **cursor-set-range** *key \&optional cursor*

    Position `cursor` on the first key equal to or greater than `key`.
    Return the found key, the value and `t`. Return `nil` if `key` was not
    found. If [`dupsort`][186b], position `cursor` on the first value of the found
    key.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_SET\_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-SET-RANGE-DUP-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-SET-RANGE-DUP%20FUNCTION"></a>

- \[function\] **cursor-set-range-dup** *key value \&optional cursor*

    Position `cursor` exactly at `key` on the first value greater than or
    equal to `value`. Return the value at the position and `t` on success,
    or `nil` if there is no such value associated with `key`.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_BOTH\_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3A-40LMDB-2FBASIC-CURSOR-OPERATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FBASIC-CURSOR-OPERATIONS%20MGL-PAX:SECTION"></a>

### 10.2 Basic cursor operations

The following operations are similar to [`g3t`][13e8], [`put`][edfe], [`del`][6237] (the
[Basic operations][f63b]), but `g3t` has three variants
([`cursor-key-value`][29e1], [`cursor-key`][2a09], and [`cursor-value`][5902]). All of them
require the cursor to be positioned (see
[Positioning cursors][471b]).

<a id="x-28LMDB-3ACURSOR-KEY-VALUE-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-KEY-VALUE%20FUNCTION"></a>

- \[function\] **cursor-key-value** *\&optional cursor*

    Return the key and value `cursor` is positioned at and `t`. Return `nil`
    if `cursor` is uninitialized.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-KEY-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-KEY%20FUNCTION"></a>

- \[function\] **cursor-key** *\&optional cursor*

    Return the key `cursor` is positioned at and `t`. Return `nil` if `cursor`
    is uninitialized.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-VALUE-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-VALUE%20FUNCTION"></a>

- \[function\] **cursor-value** *\&optional cursor*

    Return the value `cursor` is positioned at and `t`. Return `nil` if
    `cursor` is uninitialized.

    Wraps [mdb\_cursor\_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
    with [MDB\_GET\_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127).

<a id="x-28LMDB-3ACURSOR-PUT-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-PUT%20FUNCTION"></a>

- \[function\] **cursor-put** *key value cursor \&key current (overwrite t) (dupdata t) append append-dup*

    Like [`put`][edfe], store key-value pairs into `cursor`'s database.
    `cursor` is positioned at the new item, or on failure usually near it.
    Return `value`.

    - `current`: Replace the item at the current cursor position. `key` must
      still be provided, and must match it. If using sorted
      duplicates ([`dupsort`][186b]), `value` must still sort into the same place.
      This is intended to be used when the new data is the same size as
      the old. Otherwise it will simply perform a delete of the old
      record followed by an insert.

    - `overwrite`: If `nil`, signal [`lmdb-key-exists-error`][8720] if `key` already
      appears in [`cursor-db`][8887].

    - `dupdata`: If `nil`, signal `lmdb-key-exists-error` if the `key`, `value`
      pair already appears in [`db`][3a5d]. Has no effect if `cursor-db` doesn't
      have [`dupsort`][186b].

    - `append`: Append the `key`, `value` pair to the end of `cursor-db` instead
      of finding `key`'s location in the B+ tree by performing
      comparisons. The client effectively promises that keys are
      inserted in sort order, which allows for fast bulk loading. If the
      promise is broken, `lmdb-key-exists-error` is signalled.

    - `append-dup`: The client promises that duplicate values are inserted
      in sort order. If the promise is broken, `lmdb-key-exists-error` is
      signalled.

    May signal [`lmdb-map-full-error`][f710], [`lmdb-txn-full-error`][2070],
    [`lmdb-txn-read-only-error`][e274].

    Wraps [mdb\_cursor\_put()](http://www.lmdb.tech/doc/group__mdb.html#ga1f83ccb40011837ff37cc32be01ad91e).

<a id="x-28LMDB-3ACURSOR-DEL-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-DEL%20FUNCTION"></a>

- \[function\] **cursor-del** *cursor \&key delete-dups*

    Delete the key-value pair `cursor` is positioned at. This does not
    make the cursor uninitialized, so operations such as [`cursor-next`][d4f9] can
    still be used on it. Both `cursor-next` and [`cursor-key-value`][29e1] will
    return the same record after this operation. If `cursor` is not
    initialized, [`lmdb-cursor-uninitialized-error`][d4f4] is signalled. Returns
    no values.

    If `delete-dups`, delete all duplicate values that belong to the
    current key. With `delete-dups`, [`cursor-db`][8887] must have [`dupsort`][186b], else
    [`lmdb-incompatible-error`][12a5] is signalled.

    May signal `lmdb-cursor-uninitialized-error`,
    [`lmdb-txn-read-only-error`][e274].

    Wraps [mdb\_cursor\_del()](http://www.lmdb.tech/doc/group__mdb.html#ga26a52d3efcfd72e5bf6bd6960bf75f95).

<a id="x-28LMDB-3A-40LMDB-2FMISC-CURSOR-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FMISC-CURSOR%20MGL-PAX:SECTION"></a>

### 10.3 Miscellaneous cursor operations

<a id="x-28LMDB-3ACURSOR-RENEW-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-RENEW%20FUNCTION"></a>

- \[function\] **cursor-renew** *\&optional cursor*

    Associate `cursor` with the [active transaction][00c7] (which must be
    read-only) as if it had been created with that transaction to begin
    with to avoid allocation overhead. [`cursor-db`][8887] stays the same. This
    may be done whether the previous transaction is open or closed (see
    [`open-txn-p`][8edb]). No values are returned.

    Wraps [mdb\_cursor\_renew()](http://www.lmdb.tech/doc/group__mdb.html#gac8b57befb68793070c85ea813df481af).

<a id="x-28LMDB-3ACURSOR-COUNT-20FUNCTION-29"></a>
<a id="LMDB:CURSOR-COUNT%20FUNCTION"></a>

- \[function\] **cursor-count** *\&optional cursor*

    Return the number of duplicate values for the current key of
    `cursor`. If [`cursor-db`][8887] doesn't have [`dupsort`][186b], [`lmdb-incompatible-error`][12a5]
    is signalled. If `cursor` is not initialized,
    [`lmdb-cursor-uninitialized-error`][d4f4] is signalled.

    Wraps [mdb\_cursor\_count()](http://www.lmdb.tech/doc/group__mdb.html#ga4041fd1e1862c6b7d5f10590b86ffbe2).

<a id="x-28LMDB-3ADO-CURSOR-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:DO-CURSOR%20MGL-PAX:MACRO"></a>

- \[macro\] **do-cursor** *(key-var value-var cursor \&key from-end nodup) \&body body*

    Iterate over key-value pairs starting from the position of `cursor`.
    If `cursor` is not positioned then no key-value pairs will be seen. If
    `from-end`, then iterate with [`cursor-prev`][6440] instead of [`cursor-next`][d4f9]. If
    `nodup`, then make that [`cursor-prev-nodup`][a5c6] and [`cursor-next-nodup`][24f5].

    If `cursor` is `nil`, the [default cursor][d997] is used.

    If `nodup` and not `from-end`, then the first duplicate of each key will
    be seen. If `nodup` and `from-end`, then the last duplicate of each key
    will be seen.

    To iterate over all key-value pairs with keys >= 7:

    ```
    (with-cursor (cursor db)
      (cursor-set-key 7 cursor)
      (do-cursor (key value cursor)
        (print (cons key value))))
    ```

<a id="x-28LMDB-3ADO-CURSOR-DUP-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:DO-CURSOR-DUP%20MGL-PAX:MACRO"></a>

- \[macro\] **do-cursor-dup** *(value-var cursor \&key from-end) \&body body*

    Iterate over duplicate values with starting from the position of
    `cursor`. If `cursor` is not positioned then no values will be seen. If
    `from-end`, then iterate with [`cursor-prev-dup`][5fe3] instead of
    [`cursor-next-dup`][aad5].

    If `cursor` is `nil`, the [default cursor][d997] is used.

    To iterate over all values that not smaller than #(3 4 5),
    associated with the key 7:

    ```
    (with-cursor (cursor db)
      (cursor-set-key-dup cursor 7 #(3 4 5))
      (do-cursor-dup (value cursor)
        (print value)))
    ```

<a id="x-28LMDB-3ADO-DB-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:DO-DB%20MGL-PAX:MACRO"></a>

- \[macro\] **do-db** *(key-var value-var db \&key from-end nodup) \&body body*

    Iterate over all keys and values in `db`. If `nodup`, then all but the
    first (or last if `from-end`) value for each key are skipped. If
    `from-end`, then iterate in reverse order.

    To iterate over all values in `db`:

    ```
    (do-db (key value db)
      (print (cons key value)))
    ```

    This macro establishes a [default cursor][d997].

<a id="x-28LMDB-3ADO-DB-DUP-20MGL-PAX-3AMACRO-29"></a>
<a id="LMDB:DO-DB-DUP%20MGL-PAX:MACRO"></a>

- \[macro\] **do-db-dup** *(value-var db key \&key from-end) \&body body*

    Iterate over all values associated with `key` in `db`. If `from-end`,
    then iteration starts at the largest value.

    To iterate over all values associated with the key 7:

    ```
    (do-db-dup (value db 7)
      (print value))
    ```

    This macro establishes a [default cursor][d997].

<a id="x-28LMDB-3ALIST-DUPS-20FUNCTION-29"></a>
<a id="LMDB:LIST-DUPS%20FUNCTION"></a>

- \[function\] **list-dups** *db key \&key from-end*

    A thin wrapper around [`do-db-dup`][8a15], this function returns all values
    associated with `key` in `db` as a list. If `from-end`, then the first
    element of the list is the largest value.

<a id="x-28LMDB-3A-40LMDB-2FCONDITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FCONDITIONS%20MGL-PAX:SECTION"></a>

## 11 Conditions

<a id="x-28LMDB-3ALMDB-SERIOUS-CONDITION-20CONDITION-29"></a>
<a id="LMDB:LMDB-SERIOUS-CONDITION%20CONDITION"></a>

- \[condition\] **lmdb-serious-condition** *[serious-condition][af00]*

    The base class of all `lmdb` conditions. Conditions
    that are `lmdb-serious-condition`s, but not [`lmdb-error`][b3a1]s are corruption
    and internal errors, which are hard to recover from.

<a id="x-28LMDB-3ALMDB-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-error** *[lmdb-serious-condition][4ce7] [error][d162]*

    Base class for normal, recoverable `lmdb` errors.

<a id="x-28LMDB-3A-40LMDB-2FERROR-CODE-CONDITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FERROR-CODE-CONDITIONS%20MGL-PAX:SECTION"></a>

### 11.1 Conditions for C lmdb error codes

The following conditions correspond to [C lmdb error
codes](http://www.lmdb.tech/doc/group__errors.html).

<a id="x-28LMDB-3ALMDB-KEY-EXISTS-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-KEY-EXISTS-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-key-exists-error** *[lmdb-error][b3a1]*

    Key-value pair already exists. Signalled by [`put`][edfe]
    and [`cursor-put`][a56d].

<a id="x-28LMDB-3ALMDB-NOT-FOUND-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-NOT-FOUND-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-not-found-error** *[lmdb-error][b3a1]*

    Key-value pair does not exist. All functions ([`g3t`][13e8],
    [`cursor-next`][d4f9], ...) should return `nil` instead of signalling this
    error. If it is signalled, that's a bug.

<a id="x-28LMDB-3ALMDB-PAGE-NOT-FOUND-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-PAGE-NOT-FOUND-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-page-not-found-error** *[lmdb-serious-condition][4ce7]*

    Requested page not found - this usually indicates
    corruption.

<a id="x-28LMDB-3ALMDB-CORRUPTED-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-CORRUPTED-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-corrupted-error** *[lmdb-serious-condition][4ce7]*

    Located page was wrong type.

<a id="x-28LMDB-3ALMDB-PANIC-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-PANIC-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-panic-error** *[lmdb-serious-condition][4ce7]*

    Update of meta page failed or environment had fatal
    error.

<a id="x-28LMDB-3ALMDB-VERSION-MISMATCH-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-VERSION-MISMATCH-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-version-mismatch-error** *[lmdb-error][b3a1]*

    Environment version mismatch.

<a id="x-28LMDB-3ALMDB-INVALID-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-INVALID-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-invalid-error** *[lmdb-serious-condition][4ce7]*

    File is not a valid `lmdb` file.

<a id="x-28LMDB-3ALMDB-MAP-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-MAP-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-map-full-error** *[lmdb-error][b3a1]*

    [`env-map-size`][69da] reached. Reopen the environment with a
    larger `:map-size`.

<a id="x-28LMDB-3ALMDB-DBS-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-DBS-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-dbs-full-error** *[lmdb-error][b3a1]*

    [`env-max-dbs`][7388] reached. Reopen the environment with a
    higher `:max-dbs`.

<a id="x-28LMDB-3ALMDB-READERS-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-READERS-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-readers-full-error** *[lmdb-error][b3a1]*

    [`env-max-readers`][d062] reached. Reopen the environment
    with a higher `:max-readers`.

<a id="x-28LMDB-3ALMDB-TXN-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-TXN-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-txn-full-error** *[lmdb-error][b3a1]*

    `txn` has too many dirty pages. This condition is
    expected to occur only when using nested read-write transactions or
    operations multiple items (currently not supported by this
    wrapper).

<a id="x-28LMDB-3ALMDB-CURSOR-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-CURSOR-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-cursor-full-error** *[lmdb-serious-condition][4ce7]*

    Cursor stack too deep - internal error.

<a id="x-28LMDB-3ALMDB-PAGE-FULL-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-PAGE-FULL-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-page-full-error** *[lmdb-serious-condition][4ce7]*

    Page has not enough space - internal error.

<a id="x-28LMDB-3ALMDB-MAP-RESIZED-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-MAP-RESIZED-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-map-resized-error** *[lmdb-error][b3a1]*

    Data file contents grew beyond [`env-map-size`][69da]. This
    can happen if another OS process using the same environment path set
    a larger map size than this process did.

<a id="x-28LMDB-3ALMDB-INCOMPATIBLE-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-INCOMPATIBLE-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-incompatible-error** *[lmdb-error][b3a1]*

    Operation and [`db`][3a5d] incompatible, or [`db`][3a5d] type changed.
    This can mean:

    - The operation expects a [`dupsort`][186b] or `dupfixed` database.

    - Opening a named `db` when the unnamed `db` has `dupsort` or `integer-key`.

    - Accessing a data record as a database, or vice versa.

    - The database was dropped and recreated with different flags.

<a id="x-28LMDB-3ALMDB-BAD-RSLOT-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-BAD-RSLOT-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-bad-rslot-error** *[lmdb-error][b3a1]*

    Invalid reuse of reader locktable slot. May be
    signalled by [`with-txn`][fdc6].

<a id="x-28LMDB-3ALMDB-BAD-TXN-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-BAD-TXN-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-bad-txn-error** *[lmdb-error][b3a1]*

    Transaction must abort, has a child, or is invalid.
    Signalled, for example, when a read-only transaction is nested in a
    read-write transaction, or when a cursor is used whose transaction
    has been closed (committed, aborted, or reset).

<a id="x-28LMDB-3ALMDB-BAD-VALSIZE-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-BAD-VALSIZE-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-bad-valsize-error** *[lmdb-error][b3a1]*

    Unsupported size of key/[`db`][3a5d] name/data, or wrong
    `dupfixed`, `integer-key` or `integer-dup`. See [`env-max-key-size`][47fd].

<a id="x-28LMDB-3ALMDB-BAD-DBI-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-BAD-DBI-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-bad-dbi-error** *[lmdb-error][b3a1]*

    The specified `dbi` was changed unexpectedly.

<a id="x-28LMDB-3A-40LMDB-2FADDITIONAL-CONDITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="LMDB:@LMDB%2FADDITIONAL-CONDITIONS%20MGL-PAX:SECTION"></a>

### 11.2 Additional conditions

The following conditions do not have a dedicated C lmdb error
code.

<a id="x-28LMDB-3ALMDB-CURSOR-UNINITIALIZED-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-CURSOR-UNINITIALIZED-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-cursor-uninitialized-error** *[lmdb-error][b3a1]*

    Cursor was not initialized. Position the cursor at
    a key-value pair with a function like [`cursor-first`][fa3d] or
    [`cursor-set-key`][8615]. Signalled when some functions return the C error
    code `einval`.

<a id="x-28LMDB-3ALMDB-CURSOR-THREAD-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-CURSOR-THREAD-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-cursor-thread-error** *[lmdb-error][b3a1]*

    Cursor was accessed from a thread other than the
    one in which it was created. Since the foreign cursor object's
    lifetime is tied to the dynamic extent of its [`with-cursor`][b1c7], this
    might mean accessing garbage in foreign memory with unpredictable
    consequences.

<a id="x-28LMDB-3ALMDB-TXN-READ-ONLY-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-TXN-READ-ONLY-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-txn-read-only-error** *[lmdb-error][b3a1]*

    Attempt was made to write in a read-only
    transaction. Signalled when some functions return the C error code
    `eaccess`.

<a id="x-28LMDB-3ALMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR-20CONDITION-29"></a>
<a id="LMDB:LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR%20CONDITION"></a>

- \[condition\] **lmdb-illegal-access-to-parent-txn-error** *[lmdb-error][b3a1]*

    A parent transaction and its cursors may not
    issue any other operations than [`commit-txn`][b473] and [`abort-txn`][f466] while it
    has active child transactions. In `lmdb`, [Basic operations][f63b] are
    always executed in the [active transaction][00c7], but [Cursors][eb36] can
    refer to the parent transaction:

    ```
    (with-temporary-env (*env*)
      (let ((db (get-db "db")))
        (with-txn (:write t)
          (put db #(1) #(1))
          (with-cursor (cursor db)
            (with-txn (:write t)
              (assert-error lmdb-illegal-access-to-parent-txn-error
                (cursor-set-key #(1) cursor)))))))
    ```

[0090]: #LMDB:@LMDB%2FLINKS%20MGL-PAX:SECTION "Links"

[00c7]: #LMDB:@ACTIVE-TRANSACTION%20MGL-PAX:GLOSSARY-TERM "active transaction"

[03b4]: #LMDB:@LMDB%2FENCODINGS%20MGL-PAX:SECTION "Encoding and decoding data"

[0b9a]: #LMDB:CLOSE-ENV%20FUNCTION "LMDB:CLOSE-ENV FUNCTION"

[12a5]: #LMDB:LMDB-INCOMPATIBLE-ERROR%20CONDITION "LMDB:LMDB-INCOMPATIBLE-ERROR CONDITION"

[1306]: #LMDB:CURSOR%20STRUCTURE "LMDB:CURSOR STRUCTURE"

[13e8]: #LMDB:G3T%20FUNCTION "LMDB:G3T FUNCTION"

[13f2]: #LMDB:*KEY-ENCODER*%20VARIABLE "LMDB:*KEY-ENCODER* VARIABLE"

[186b]: #LMDB:@DUPSORT%20MGL-PAX:SECTION "`dupsort`"

[19d5]: #LMDB:*VALUE-ENCODER*%20VARIABLE "LMDB:*VALUE-ENCODER* VARIABLE"

[1d55]: #LMDB:@LMDB%2FVERSION%20MGL-PAX:SECTION "Library versions"

[2070]: #LMDB:LMDB-TXN-FULL-ERROR%20CONDITION "LMDB:LMDB-TXN-FULL-ERROR CONDITION"

[2462]: #LMDB:*DB-CLASS*%20VARIABLE "LMDB:*DB-CLASS* VARIABLE"

[24f5]: #LMDB:CURSOR-NEXT-NODUP%20FUNCTION "LMDB:CURSOR-NEXT-NODUP FUNCTION"

[29e1]: #LMDB:CURSOR-KEY-VALUE%20FUNCTION "LMDB:CURSOR-KEY-VALUE FUNCTION"

[2a09]: #LMDB:CURSOR-KEY%20FUNCTION "LMDB:CURSOR-KEY FUNCTION"

[2a35]: #LMDB:MDB-VAL-TO-STRING%20FUNCTION "LMDB:MDB-VAL-TO-STRING FUNCTION"

[2b7a]: #LMDB:LMDB-CURSOR-THREAD-ERROR%20CONDITION "LMDB:LMDB-CURSOR-THREAD-ERROR CONDITION"

[328c]: #LMDB:@LMDB%2FMISC-CURSOR%20MGL-PAX:SECTION "Miscellaneous cursor operations"

[3582]: #LMDB:STRING-TO-OCTETS%20FUNCTION "LMDB:STRING-TO-OCTETS FUNCTION"

[3935]: #LMDB:LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR%20CONDITION "LMDB:LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR CONDITION"

[3a5d]: #LMDB:DB%20CLASS "LMDB:DB CLASS"

[3bc3]: #LMDB:@LMDB%2FINTRODUCTION%20MGL-PAX:SECTION "Introduction"

[3c69]: #LMDB:WITH-ENV%20MGL-PAX:MACRO "LMDB:WITH-ENV MGL-PAX:MACRO"

[412f]: #LMDB:ENV%20CLASS "LMDB:ENV CLASS"

[4336]: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm "\"3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"

[4437]: #LMDB:DROP-DB%20FUNCTION "LMDB:DROP-DB FUNCTION"

[451a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm "SIMPLE-ARRAY (MGL-PAX:CLHS TYPE)"

[471b]: #LMDB:@LMDB%2FPOSITIONING-CURSORS%20MGL-PAX:SECTION "Positioning cursors"

[47fd]: #LMDB:ENV-MAX-KEY-SIZE%20FUNCTION "LMDB:ENV-MAX-KEY-SIZE FUNCTION"

[4880]: #LMDB:LMDB-BAD-RSLOT-ERROR%20CONDITION "LMDB:LMDB-BAD-RSLOT-ERROR CONDITION"

[4ce7]: #LMDB:LMDB-SERIOUS-CONDITION%20CONDITION "LMDB:LMDB-SERIOUS-CONDITION CONDITION"

[5488]: #LMDB:ENCODING%20TYPE "LMDB:ENCODING TYPE"

[5538]: #LMDB:@LMDB%2FADDITIONAL-CONDITIONS%20MGL-PAX:SECTION "Additional conditions"

[5578]: #LMDB:CURSOR-RENEW%20FUNCTION "LMDB:CURSOR-RENEW FUNCTION"

[5601]: #LMDB:ENV-FLAGS%20%28MGL-PAX:READER%20LMDB:ENV%29 "LMDB:ENV-FLAGS (MGL-PAX:READER LMDB:ENV)"

[58a4]: #LMDB:@LMDB%2FERROR-CODE-CONDITIONS%20MGL-PAX:SECTION "Conditions for C lmdb error codes"

[5902]: #LMDB:CURSOR-VALUE%20FUNCTION "LMDB:CURSOR-VALUE FUNCTION"

[5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"

[5d07]: #LMDB:OCTETS-TO-UINT64%20FUNCTION "LMDB:OCTETS-TO-UINT64 FUNCTION"

[5fe3]: #LMDB:CURSOR-PREV-DUP%20FUNCTION "LMDB:CURSOR-PREV-DUP FUNCTION"

[6098]: http://www.lispworks.com/documentation/HyperSpec/Body/t_vector.htm "VECTOR (MGL-PAX:CLHS CLASS)"

[60cd]: #LMDB:@LMDB%2FOVERRIDING-ENCODINGS%20MGL-PAX:SECTION "Overriding encodings"

[6237]: #LMDB:DEL%20FUNCTION "LMDB:DEL FUNCTION"

[6440]: #LMDB:CURSOR-PREV%20FUNCTION "LMDB:CURSOR-PREV FUNCTION"

[67cb]: #LMDB:@LMDB%2FDATABASE-API%20MGL-PAX:SECTION "Database API"

[6817]: #LMDB:RENEW-TXN%20FUNCTION "LMDB:RENEW-TXN FUNCTION"

[69da]: #LMDB:ENV-MAP-SIZE%20%28MGL-PAX:READER%20LMDB:ENV%29 "LMDB:ENV-MAP-SIZE (MGL-PAX:READER LMDB:ENV)"

[6ecf]: #LMDB:@LMDB%2FNESTING-TRANSACTIONS%20MGL-PAX:SECTION "Nesting transactions"

[6ed7]: #LMDB:ENV-PATH%20%28MGL-PAX:READER%20LMDB:ENV%29 "LMDB:ENV-PATH (MGL-PAX:READER LMDB:ENV)"

[6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"

[7269]: #LMDB:CURSOR-LAST%20FUNCTION "LMDB:CURSOR-LAST FUNCTION"

[7388]: #LMDB:ENV-MAX-DBS%20%28MGL-PAX:READER%20LMDB:ENV%29 "LMDB:ENV-MAX-DBS (MGL-PAX:READER LMDB:ENV)"

[749e]: #LMDB:LMDB-BAD-TXN-ERROR%20CONDITION "LMDB:LMDB-BAD-TXN-ERROR CONDITION"

[7c81]: #LMDB:UINT64-TO-OCTETS%20FUNCTION "LMDB:UINT64-TO-OCTETS FUNCTION"

[7def]: #LMDB:WITH-IMPLICIT-CURSOR%20MGL-PAX:MACRO "LMDB:WITH-IMPLICIT-CURSOR MGL-PAX:MACRO"

[82fb]: #LMDB:@LMDB%2FOPENING-AND-CLOSING-ENV%20MGL-PAX:SECTION "Opening and closing environments"

[84b1]: #LMDB:@LMDB%2FDEVIATIONS-FROM-THE-LMDB-API%20MGL-PAX:SECTION "Deviations from the C lmdb API"

[8615]: #LMDB:CURSOR-SET-KEY%20FUNCTION "LMDB:CURSOR-SET-KEY FUNCTION"

[8720]: #LMDB:LMDB-KEY-EXISTS-ERROR%20CONDITION "LMDB:LMDB-KEY-EXISTS-ERROR CONDITION"

[8887]: #LMDB:CURSOR-DB%20%28MGL-PAX:STRUCTURE-ACCESSOR%20LMDB:CURSOR%29 "LMDB:CURSOR-DB (MGL-PAX:STRUCTURE-ACCESSOR LMDB:CURSOR)"

[8a15]: #LMDB:DO-DB-DUP%20MGL-PAX:MACRO "LMDB:DO-DB-DUP MGL-PAX:MACRO"

[8edb]: #LMDB:OPEN-TXN-P%20FUNCTION "LMDB:OPEN-TXN-P FUNCTION"

[97e3]: #LMDB:@LMDB%2FDATABASES%20MGL-PAX:SECTION "Databases"

[a14f]: #LMDB:@LMDB%2FTHE-UNNAMED-DATABASE%20MGL-PAX:SECTION "The unnamed database"

[a237]: http://www.lispworks.com/documentation/HyperSpec/Body/t_cons.htm "CONS (MGL-PAX:CLHS CLASS)"

[a56d]: #LMDB:CURSOR-PUT%20FUNCTION "LMDB:CURSOR-PUT FUNCTION"

[a5c6]: #LMDB:CURSOR-PREV-NODUP%20FUNCTION "LMDB:CURSOR-PREV-NODUP FUNCTION"

[aad5]: #LMDB:CURSOR-NEXT-DUP%20FUNCTION "LMDB:CURSOR-NEXT-DUP FUNCTION"

[ac19]: #LMDB:*VALUE-DECODER*%20VARIABLE "LMDB:*VALUE-DECODER* VARIABLE"

[ad52]: #LMDB:RESET-TXN%20FUNCTION "LMDB:RESET-TXN FUNCTION"

[af00]: http://www.lispworks.com/documentation/HyperSpec/Body/e_seriou.htm "SERIOUS-CONDITION (MGL-PAX:CLHS CONDITION)"

[af06]: #LMDB:@LMDB%2FSAFETY%20MGL-PAX:SECTION "Safety"

[af7c]: #LMDB:LMDB-FOREIGN-VERSION%20FUNCTION "LMDB:LMDB-FOREIGN-VERSION FUNCTION"

[b1c7]: #LMDB:WITH-CURSOR%20MGL-PAX:MACRO "LMDB:WITH-CURSOR MGL-PAX:MACRO"

[b3a1]: #LMDB:LMDB-ERROR%20CONDITION "LMDB:LMDB-ERROR CONDITION"

[b473]: #LMDB:COMMIT-TXN%20FUNCTION "LMDB:COMMIT-TXN FUNCTION"

[b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"

[b9c5]: #LMDB:OCTETS%20TYPE "LMDB:OCTETS TYPE"

[bae2]: #LMDB:@LMDB%2FTRANSACTIONS%20MGL-PAX:SECTION "Transactions"

[bd3d]: #LMDB:@LMDB%2FENV-REFERENCE%20MGL-PAX:SECTION "Environments reference"

[be19]: #LMDB:LMDB-BAD-VALSIZE-ERROR%20CONDITION "LMDB:LMDB-BAD-VALSIZE-ERROR CONDITION"

[bea1]: #LMDB:OPEN-ENV%20FUNCTION "LMDB:OPEN-ENV FUNCTION"

[c362]: #LMDB:@LMDB%2FDESIGN-AND-IMPLEMENTATION%20MGL-PAX:SECTION "Design and implementation"

[c5fa]: #LMDB:SYNC-ENV%20FUNCTION "LMDB:SYNC-ENV FUNCTION"

[d062]: #LMDB:ENV-MAX-READERS%20%28MGL-PAX:READER%20LMDB:ENV%29 "LMDB:ENV-MAX-READERS (MGL-PAX:READER LMDB:ENV)"

[d0ca]: #LMDB:@LMDB%2FENVIRONMENTS%20MGL-PAX:SECTION "Environments"

[d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"

[d4a7]: #LMDB:*KEY-DECODER*%20VARIABLE "LMDB:*KEY-DECODER* VARIABLE"

[d4f4]: #LMDB:LMDB-CURSOR-UNINITIALIZED-ERROR%20CONDITION "LMDB:LMDB-CURSOR-UNINITIALIZED-ERROR CONDITION"

[d4f9]: #LMDB:CURSOR-NEXT%20FUNCTION "LMDB:CURSOR-NEXT FUNCTION"

[d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"

[d66b]: #LMDB:@LMDB%2FMISC-ENV%20MGL-PAX:SECTION "Miscellaneous environment functions"

[d997]: #LMDB:@DEFAULT-CURSOR%20MGL-PAX:GLOSSARY-TERM "default cursor"

[d9f2]: trivial-utf-8-manual.md#%22trivial-utf-8%22%20ASDF%2FSYSTEM:SYSTEM "\"trivial-utf-8\" ASDF/SYSTEM:SYSTEM"

[e012]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CDR (MGL-PAX:CLHS FUNCTION)"

[e274]: #LMDB:LMDB-TXN-READ-ONLY-ERROR%20CONDITION "LMDB:LMDB-TXN-READ-ONLY-ERROR CONDITION"

[e84c]: #LMDB:WITH-MDB-VAL-SLOTS%20MGL-PAX:MACRO "LMDB:WITH-MDB-VAL-SLOTS MGL-PAX:MACRO"

[e93f]: #LMDB:LMDB-MAP-RESIZED-ERROR%20CONDITION "LMDB:LMDB-MAP-RESIZED-ERROR CONDITION"

[ea6c]: #LMDB:@LMDB%2FBASIC-CURSOR-OPERATIONS%20MGL-PAX:SECTION "Basic cursor operations"

[eb36]: #LMDB:@LMDB%2FCURSORS%20MGL-PAX:SECTION "Cursors"

[edfb]: #LMDB:@LMDB%2FCONDITIONS%20MGL-PAX:SECTION "Conditions"

[edfe]: #LMDB:PUT%20FUNCTION "LMDB:PUT FUNCTION"

[f306]: #LMDB:GET-DB%20FUNCTION "LMDB:GET-DB FUNCTION"

[f3ff]: #LMDB:MDB-VAL-TO-UINT64%20FUNCTION "LMDB:MDB-VAL-TO-UINT64 FUNCTION"

[f466]: #LMDB:ABORT-TXN%20FUNCTION "LMDB:ABORT-TXN FUNCTION"

[f63b]: #LMDB:@LMDB%2FBASIC-OPERATIONS%20MGL-PAX:SECTION "Basic operations"

[f6fc]: #LMDB:OCTETS-TO-STRING%20FUNCTION "LMDB:OCTETS-TO-STRING FUNCTION"

[f710]: #LMDB:LMDB-MAP-FULL-ERROR%20CONDITION "LMDB:LMDB-MAP-FULL-ERROR CONDITION"

[fa3d]: #LMDB:CURSOR-FIRST%20FUNCTION "LMDB:CURSOR-FIRST FUNCTION"

[fdc6]: #LMDB:WITH-TXN%20MGL-PAX:MACRO "LMDB:WITH-TXN MGL-PAX:MACRO"
