<a id="x-28MGL-MAT-3A-40MAT-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-MANUAL%20MGL-PAX:SECTION"></a>

# MAT Manual

## Table of Contents

- [1 Links][54ea]
- [2 Introduction][7e60]
    - [2.1 What's MGL-MAT?][ff55]
    - [2.2 What kind of matrices are supported?][2ea4]
    - [2.3 Where to Get it?][c96c]
- [3 Tutorial][1b99]
- [4 Basics][086b]
- [5 Element types][e798]
- [6 Printing][43e9]
- [7 Shaping][7bf8]
    - [7.1 Comparison to Lisp Arrays][2cbb]
    - [7.2 Functional Shaping][aa5b]
    - [7.3 Destructive Shaping][b6a7]
- [8 Assembling][46e6]
- [9 Caching][e678]
- [10 BLAS Operations][4958]
- [11 Destructive API][545d]
- [12 Non-destructive API][3e36]
- [13 Mappings][1600]
- [14 Random numbers][c669]
- [15 I/O][7feb]
- [16 Debugging][ac8d]
- [17 Facet API][8eda]
    - [17.1 Facets][7ecb]
    - [17.2 Foreign arrays][36b5]
    - [17.3 CUDA][5212]
        - [17.3.1 CUDA Memory Management][4e65]
- [18 Writing Extensions][8f0e]
    - [18.1 Lisp Extensions][d3b8]
    - [18.2 CUDA Extensions][93e1]
        - [18.2.1 CUBLAS][b116]
        - [18.2.2 CURAND][3993]

###### \[in package MGL-MAT\]
<a id="x-28-22mgl-mat-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-mat%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl-mat"**

    - _Version:_ 0.1.0
    - _Description:_ [`mat`][6d14] is library for working with multi-dimensional
        arrays which supports efficient interfacing to foreign and CUDA
        code. BLAS and CUBLAS bindings are available.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis <mega@retes.hu>
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <http://melisgl.github.io/mgl-mat>
    - _Bug tracker:_ <https://github.com/melisgl/mgl-mat/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/mgl-mat.git)
    - *Depends on:* alexandria, bordeaux-threads, cffi, cffi-grovel, cl-cuda, flexi-streams, ieee-floats, lla, [mgl-pax][6fdb], num-utils, static-vectors, trivial-garbage

<a id="x-28MGL-MAT-3A-40MAT-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-LINKS%20MGL-PAX:SECTION"></a>

## 1 Links

Here is the [official
repository](https://github.com/melisgl/mgl-mat) and the [HTML
documentation](http://melisgl.github.io/mgl-mat/mat-manual.html)
for the latest version.

<a id="x-28MGL-MAT-3A-40MAT-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-INTRODUCTION%20MGL-PAX:SECTION"></a>

## 2 Introduction

<a id="x-28MGL-MAT-3A-40MAT-WHAT-IS-IT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-WHAT-IS-IT%20MGL-PAX:SECTION"></a>

### 2.1 What's MGL-MAT?

MGL-MAT is library for working with multi-dimensional arrays
which supports efficient interfacing to foreign and CUDA code with
automatic translations between cuda, foreign and lisp storage. BLAS
and CUBLAS bindings are available.

<a id="x-28MGL-MAT-3A-40MAT-WHAT-KIND-OF-MATRICES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-WHAT-KIND-OF-MATRICES%20MGL-PAX:SECTION"></a>

### 2.2 What kind of matrices are supported?

Currently only row-major single and double float matrices are
supported, but it would be easy to add single and double precision
complex types too. Other numeric types, such as byte and native
integer, can be added too, but they are not supported by CUBLAS.
There are no restrictions on the number of dimensions, and reshaping
is possible. All functions operate on the visible portion of the
matrix (which is subject to displacement and shaping), invisible
elements are not affected.

<a id="x-28MGL-MAT-3A-40MAT-INSTALLATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-INSTALLATION%20MGL-PAX:SECTION"></a>

### 2.3 Where to Get it?

All dependencies are in quicklisp except for
[CL-CUDA](https://github.com/takagi/cl-cuda) that needs to be
fetched from github. Just clone CL-CUDA and MGL-MAT into
`quicklisp/local-projects/` and you are set. MGL-MAT itself lives
[at github](https://github.com/melisgl/mgl-mat), too.

Prettier-than-markdown HTML documentation cross-linked with other
libraries is
[available](http://melisgl.github.io/mgl-pax-world/mat-manual.html)
as part of [PAX World](http://melisgl.github.io/mgl-pax-world/).

<a id="x-28MGL-MAT-3A-40MAT-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-TUTORIAL%20MGL-PAX:SECTION"></a>

## 3 Tutorial

We are going to see how to create matrices, access their contents.

Creating matrices is just like creating lisp arrays:

```commonlisp
(make-mat '6)
==> #<MAT 6 A #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)>

(make-mat '(2 3) :ctype :float :initial-contents '((1 2 3) (4 5 6)))
==> #<MAT 2x3 AB #2A((1.0 2.0 3.0) (4.0 5.0 6.0))>

(make-mat '(2 3 4) :initial-element 1)
==> #<MAT 2x3x4 A #3A(((1.0d0 1.0d0 1.0d0 1.0d0)
-->                    (1.0d0 1.0d0 1.0d0 1.0d0)
-->                    (1.0d0 1.0d0 1.0d0 1.0d0))
-->                   ((1.0d0 1.0d0 1.0d0 1.0d0)
-->                    (1.0d0 1.0d0 1.0d0 1.0d0)
-->                    (1.0d0 1.0d0 1.0d0 1.0d0)))>
```

The most prominent difference from lisp arrays is that [`mat`][6d14]s are
always numeric and their element type (called [`ctype`][1354] here) defaults
to `:double`.

Individual elements can be accessed or set with [`mref`][eb95]:

```commonlisp
(let ((m (make-mat '(2 3))))
  (setf (mref m 0 0) 1)
  (setf (mref m 0 1) (* 2 (mref m 0 0)))
  (incf (mref m 0 2) 4)
  m)
==> #<MAT 2x3 AB #2A((1.0d0 2.0d0 4.0d0) (0.0d0 0.0d0 0.0d0))>
```

Compared to [`aref`][e22b] `mref` is a very expensive operation and it's best
used sparingly. Instead, typical code relies much more on matrix
level operations:

```commonlisp
(princ (scal! 2 (fill! 3 (make-mat 4))))
.. #<MAT 4 BF #(6.0d0 6.0d0 6.0d0 6.0d0)>
==> #<MAT 4 ABF #(6.0d0 6.0d0 6.0d0 6.0d0)>
```

The content of a matrix can be accessed in various representations
called *facets*. MGL-MAT takes care of synchronizing these facets
by copying data around lazily, but doing its best to share storage
for facets that allow it.

Notice the `abf` in the printed results. It illustrates that behind
the scenes [`fill!`][510c] worked on the [`backing-array`][1fda]
facet (hence the `b`) that's basically a 1d lisp array. [`scal!`][3f21] on the
other hand made a foreign call to the BLAS `dscal` function for
which it needed the [`foreign-array`][4f59] facet (`f`).
Finally, the `a` stands for the [`array`][cb94] facet that was
created when the array was printed. All facets are up-to-date (else
some of the characters would be lowercase). This is possible because
these three facets actually share storage which is never the case
for the [`cuda-array`][7a7a] facet. Now if we have a
CUDA-capable GPU, CUDA can be enabled with [`with-cuda*`][3db3]:

```commonlisp
(with-cuda* ()
  (princ (scal! 2 (fill! 3 (make-mat 4)))))
.. #<MAT 4 C #(6.0d0 6.0d0 6.0d0 6.0d0)>
==> #<MAT 4 A #(6.0d0 6.0d0 6.0d0 6.0d0)>
```

Note the lonely `c` showing that only the [`cuda-array`][7a7a]
facet was used for both `fill!` and `scal!`. When `with-cuda*` exits and
destroys the CUDA context, it destroys all CUDA facets, moving their
data to the [`array`][cb94] facet, so the returned `mat` only has
that facet.

When there is no high-level operation that does what we want, we may
need to add new operations. This is usually best accomplished by
accessing one of the facets directly, as in the following example:

```commonlisp
(defun logdet (mat)
  "Logarithm of the determinant of MAT. Return -1, 1 or 0 (or
  equivalent) to correct for the sign, as the second value."
  (with-facets ((array (mat 'array :direction :input)))
    (lla:logdet array)))

```

Notice that [`logdet`][e1aa] doesn't know about CUDA at all. [`with-facets`][ddd4]
gives it the content of the matrix as a normal multidimensional lisp
array, copying the data from the GPU or elsewhere if necessary. This
allows new representations ([`facet`][36d6]s) to be added easily and it also
avoids copying if the facet is already up-to-date. Of course, adding
CUDA support to `logdet` could make it more efficient.

Adding support for matrices that, for instance, live on a remote
machine is thus possible with a new facet type and existing code
would continue to work (albeit possibly slowly). Then one could
optimize the bottleneck operations by sending commands over the
network instead of copying data.

It is a bad idea to conflate resource management policy and
algorithms. MGL-MAT does its best to keep them separate.

<a id="x-28MGL-MAT-3A-40MAT-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-BASICS%20MGL-PAX:SECTION"></a>

## 4 Basics

<a id="x-28MGL-MAT-3AMAT-20CLASS-29"></a>
<a id="MGL-MAT:MAT%20CLASS"></a>

- [class] **mat** *[cube][1659]*

    A `mat` is a data [`cube`][1659] that is much like a lisp
    array, it supports `displacement`, arbitrary `dimensions` and
    `initial-element` with the usual semantics. However, a `mat` supports
    different representations of the same data. See [Tutorial][1b99] for
    an introduction.

<a id="x-28MGL-MAT-3AMAT-CTYPE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-CTYPE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-ctype** *[mat][6d14] (:ctype = \*default-mat-ctype\*)*

    One of [`*supported-ctypes*`][fc7d]. The matrix can hold
    only values of this type.

<a id="x-28MGL-MAT-3AMAT-DISPLACEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-DISPLACEMENT%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-displacement** *[mat][6d14] (:displacement = 0)*

    A value in the `[0,max-size]` interval. This is
    like the DISPLACED-INDEX-OFFSET of a lisp array, but displacement
    is relative to the start of the underlying storage vector.

<a id="x-28MGL-MAT-3AMAT-DIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-DIMENSIONS%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-dimensions** *[mat][6d14] (:dimensions)*

    Like [`array-dimensions`][b315]. It holds a list of
    dimensions, but it is allowed to pass in scalars too.

<a id="x-28MGL-MAT-3AMAT-DIMENSION-20FUNCTION-29"></a>
<a id="MGL-MAT:MAT-DIMENSION%20FUNCTION"></a>

- [function] **mat-dimension** *mat axis-number*

    Return the dimension along `axis-number`. Similar to
    [`array-dimension`][6c28].

<a id="x-28MGL-MAT-3AMAT-INITIAL-ELEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-INITIAL-ELEMENT%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-initial-element** *[mat][6d14] (:initial-element = 0)*

    If non-nil, then when a facet is created, it is
    filled with `initial-element` coerced to the appropriate numeric
    type. If `nil`, then no initialization is performed.

<a id="x-28MGL-MAT-3AMAT-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-SIZE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-size** *[mat][6d14]*

    The number of elements in the visible portion of
    the array. This is always the product of the elements
    [`mat-dimensions`][73f6] and is similar to [`array-total-size`][4821].

<a id="x-28MGL-MAT-3AMAT-MAX-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:MAT-MAX-SIZE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29"></a>

- [reader] **mat-max-size** *[mat][6d14] (:max-size)*

    The number of elements for which storage may be
    allocated. This is `displacement` + [`mat-size`][3573] + `slack` where `slack`
    is the number of trailing invisible elements.

<a id="x-28MGL-MAT-3AMAKE-MAT-20FUNCTION-29"></a>
<a id="MGL-MAT:MAKE-MAT%20FUNCTION"></a>

- [function] **make-mat** *dimensions &rest args &key (ctype \*default-mat-ctype\*) (displacement 0) max-size initial-element initial-contents (synchronization \*default-synchronization\*) displaced-to (cuda-enabled \*default-mat-cuda-enabled\*)*

    Return a new [`mat`][6d14] object. If `initial-contents` is given then the
    matrix contents are initialized with [`replace!`][0ecb]. See class [`mat`][6d14] for the
    description of the rest of the parameters. This is exactly
    what ([`make-instance`][dddd] '`mat` ...) does except `dimensions` is not a
    keyword argument so that `make-mat` looks more like [`make-array`][92ab]. The
    semantics of `synchronization` are desribed in the
    [Synchronization][a5a6] section.
    
    If specified, `displaced-to` must be a `mat` object large enough (in the
    sense of its [`mat-size`][3573]), to hold `displacement` plus `(reduce #'*
    dimensions)` elements. Just like with `make-array`, `initial-element`
    and `initial-contents` must not be supplied together with
    `displaced-to`. See [Shaping][7bf8] for more.

<a id="x-28MGL-MAT-3AARRAY-TO-MAT-20FUNCTION-29"></a>
<a id="MGL-MAT:ARRAY-TO-MAT%20FUNCTION"></a>

- [function] **array-to-mat** *array &key ctype (synchronization \*default-synchronization\*)*

    Create a [`mat`][6d14] that's equivalent to `array`. Displacement of the
    created array will be 0 and the size will be equal to
    [`array-total-size`][4821]. If `ctype` is non-nil, then it will be the ctype of
    the new matrix. Else `array`'s type is converted to a ctype. If there
    is no corresponding ctype, then [`*default-mat-ctype*`][3046] is used.
    Elements of `array` are coerced to `ctype`.
    
    Also see [Synchronization][a5a6].

<a id="x-28MGL-MAT-3AMAT-TO-ARRAY-20FUNCTION-29"></a>
<a id="MGL-MAT:MAT-TO-ARRAY%20FUNCTION"></a>

- [function] **mat-to-array** *mat*

<a id="x-28MGL-MAT-3AREPLACE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:REPLACE%21%20FUNCTION"></a>

- [function] **replace!** *mat seq-of-seqs*

    Replace the contents of `mat` with the elements of `seq-of-seqs`.
    `seq-of-seqs` is a nested sequence of sequences similar to the
    `initial-contents` argument of [`make-array`][92ab]. The total number of
    elements must match the size of `mat`. Returns `mat`.
    
    `seq-of-seqs` may contain multi-dimensional arrays as *leafs*, so the
    following is legal:
    
    ```common-lisp
    (replace! (make-mat '(1 2 3)) '(#2A((1 2 3) (4 5 6))))
    ==> #<MAT 1x2x3 AB #3A(((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0)))>
    ```

<a id="x-28MGL-MAT-3AMREF-20FUNCTION-29"></a>
<a id="MGL-MAT:MREF%20FUNCTION"></a>

- [function] **mref** *mat &rest indices*

    Like [`aref`][e22b] for arrays. Don't use this if you care about performance
    at all. [`setf`][a138]able. When set, the value is coerced to the ctype of `mat`
    with [`coerce-to-ctype`][6ead]. Note that currently `mref` always operates on
    the [`backing-array`][1fda] facet so it can trigger copying of facets. When
    it's `setf`'ed, however, it will update the [`cuda-array`][7a7a] if cuda is
    enabled and it is up-to-date or there are no facets at all.

<a id="x-28MGL-MAT-3AROW-MAJOR-MREF-20FUNCTION-29"></a>
<a id="MGL-MAT:ROW-MAJOR-MREF%20FUNCTION"></a>

- [function] **row-major-mref** *mat index*

    Like [`row-major-aref`][2053] for arrays. Don't use this if you care about
    performance at all. [`setf`][a138]able. When set, the value is coerced to the
    ctype of `mat` with [`coerce-to-ctype`][6ead]. Note that currently
    `row-major-mref` always operates on the [`backing-array`][1fda] facet so it can
    trigger copying of facets. When it's `setf`'ed, however, it will
    update the [`cuda-array`][7a7a] if cuda is enabled and it is up-to-date or
    there are no facets at all.

<a id="x-28MGL-MAT-3AMAT-ROW-MAJOR-INDEX-20FUNCTION-29"></a>
<a id="MGL-MAT:MAT-ROW-MAJOR-INDEX%20FUNCTION"></a>

- [function] **mat-row-major-index** *mat &rest subscripts*

    Like [`array-row-major-index`][72ba] for arrays.

<a id="x-28MGL-MAT-3A-40MAT-CTYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CTYPES%20MGL-PAX:SECTION"></a>

## 5 Element types

<a id="x-28MGL-MAT-3A-2ASUPPORTED-CTYPES-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*SUPPORTED-CTYPES*%20VARIABLE"></a>

- [variable] **\*supported-ctypes\*** *(:float :double)*

<a id="x-28MGL-MAT-3ACTYPE-20TYPE-29"></a>
<a id="MGL-MAT:CTYPE%20TYPE"></a>

- [type] **ctype**

    This is basically `(member :float :double)`.

<a id="x-28MGL-MAT-3A-2ADEFAULT-MAT-CTYPE-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*DEFAULT-MAT-CTYPE*%20VARIABLE"></a>

- [variable] **\*default-mat-ctype\*** *:double*

    By default [`mat`][6d14]s are created with this ctype. One of `:float`
    or `:double`.

<a id="x-28MGL-MAT-3ACOERCE-TO-CTYPE-20FUNCTION-29"></a>
<a id="MGL-MAT:COERCE-TO-CTYPE%20FUNCTION"></a>

- [function] **coerce-to-ctype** *x &key (ctype \*default-mat-ctype\*)*

    Coerce the scalar `x` to the lisp type corresponding to `ctype`.

<a id="x-28MGL-MAT-3A-40MAT-PRINTING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-PRINTING%20MGL-PAX:SECTION"></a>

## 6 Printing

<a id="x-28MGL-MAT-3A-2APRINT-MAT-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*PRINT-MAT*%20VARIABLE"></a>

- [variable] **\*print-mat\*** *t*

    Controls whether the contents of a [`mat`][6d14] object are printed as an
    array (subject to the standard printer control variables).

<a id="x-28MGL-MAT-3A-2APRINT-MAT-FACETS-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*PRINT-MAT-FACETS*%20VARIABLE"></a>

- [variable] **\*print-mat-facets\*** *t*

    Controls whether a summary of existing and up-to-date facets is
    printed when a [`mat`][6d14] object is printed. The summary that looks like
    `ABcfh` indicates that all five facets ([`array`][cb94],
    [`backing-array`][1fda], [`cuda-array`][7a7a],
    [`foreign-array`][4f59], [`cuda-host-array`][0789]) are
    present and the first two are up-to-date. A summary of a single #-
    indicates that there are no facets.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-SHAPING%20MGL-PAX:SECTION"></a>

## 7 Shaping

We are going to discuss various ways to change the visible portion
and dimensions of matrices. Conceptually a matrix has an *underlying
non-displaced storage vector*. For `(make-mat 10 :displacement
7 :max-size 21)` this underlying vector looks like this:

    displacement | visible elements  | slack
    . . . . . . . 0 0 0 0 0 0 0 0 0 0 . . . .

Whenever a matrix is reshaped (or *displaced to* in lisp
terminology), its displacement and dimensions change but the
underlying vector does not.

The rules for accessing displaced matrices is the same as always:
multiple readers can run in parallel, but attempts to write will
result in an error if there are either readers or writers on any of
the matrices that share the same underlying vector.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-COMPARISON-TO-LISP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-SHAPING-COMPARISON-TO-LISP%20MGL-PAX:SECTION"></a>

### 7.1 Comparison to Lisp Arrays

One way to reshape and displace [`mat`][6d14] objects is with [`make-mat`][e3c3] and
its `displaced-to` argument whose semantics are similar to that of
[`make-array`][92ab] in that the displacement is *relative* to the
displacement of `displaced-to`.

```commonlisp
(let* ((base (make-mat 10 :initial-element 5 :displacement 1))
       (mat (make-mat 6 :displaced-to base :displacement 2)))
  (fill! 1 mat)
  (values base mat))
==> #<MAT 1+10+0 A #(5.0d0 5.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 5.0d0
-->                  5.0d0)>
==> #<MAT 3+6+2 AB #(1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0)>
```

There are important semantic differences compared to lisp arrays all
which follow from the fact that displacement operates on the
underlying conceptual non-displaced vector.

- Matrices can be displaced and have slack even without `displaced-to`
  just like `base` in the above example.

- It's legal to alias invisible elements of `displaced-to` as long as
  the new matrix fits into the underlying storage.

- Negative displacements are allowed with `displaced-to` as long as
  the adjusted displacement is non-negative.

- Further shaping operations can make invisible portions of the
  `displaced-to` matrix visible by changing the displacement.

- In contrast to [`array-displacement`][bd19], [`mat-displacement`][f5be] only returns
  an offset into the underlying storage vector.


<a id="x-28MGL-MAT-3A-40MAT-SHAPING-FUNCTIONAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-SHAPING-FUNCTIONAL%20MGL-PAX:SECTION"></a>

### 7.2 Functional Shaping

The following functions are collectively called the functional
shaping operations, since they don't alter their arguments in any
way. Still, since storage is aliased modification to the returned
matrix will affect the original.

<a id="x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-20FUNCTION-29"></a>
<a id="MGL-MAT:RESHAPE-AND-DISPLACE%20FUNCTION"></a>

- [function] **reshape-and-displace** *mat dimensions displacement*

    Return a new matrix of `dimensions` that aliases `mat`'s storage at
    offset `displacement`. `displacement` 0 is equivalent to the start of
    the storage of `mat` regardless of `mat`'s displacement.

<a id="x-28MGL-MAT-3ARESHAPE-20FUNCTION-29"></a>
<a id="MGL-MAT:RESHAPE%20FUNCTION"></a>

- [function] **reshape** *mat dimensions*

    Return a new matrix of `dimensions` whose displacement is the same as
    the displacement of `mat`.

<a id="x-28MGL-MAT-3ADISPLACE-20FUNCTION-29"></a>
<a id="MGL-MAT:DISPLACE%20FUNCTION"></a>

- [function] **displace** *mat displacement*

    Return a new matrix that aliases `mat`'s storage at offset
    `displacement`. `displacement` 0 is equivalent to the start of the
    storage of `mat` regardless of `mat`'s displacement. The returned matrix
    has the same dimensions as `mat`.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-DESTRUCTIVE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-SHAPING-DESTRUCTIVE%20MGL-PAX:SECTION"></a>

### 7.3 Destructive Shaping

The following destructive operations don't alter the contents of
the matrix, but change what is visible. [`adjust!`][8100] is the odd one out,
it may create a new [`mat`][6d14].

<a id="x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:RESHAPE-AND-DISPLACE%21%20FUNCTION"></a>

- [function] **reshape-and-displace!** *mat dimensions displacement*

    Change the visible (or active) portion of `mat` by altering its
    displacement offset and dimensions. Future operations will only
    affect this visible portion as if the rest of the elements were not
    there. Return `mat`.
    
    `displacement` + the new size must not exceed [`mat-max-size`][c4f8].
    Furthermore, there must be no facets being viewed (with [`with-facets`][ddd4])
    when calling this function as the identity of the facets is not
    stable.

<a id="x-28MGL-MAT-3ARESHAPE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:RESHAPE%21%20FUNCTION"></a>

- [function] **reshape!** *mat dimensions*

    Like [`reshape-and-displace!`][896c] but only alters the dimensions.

<a id="x-28MGL-MAT-3ADISPLACE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:DISPLACE%21%20FUNCTION"></a>

- [function] **displace!** *mat displacement*

    Like [`reshape-and-displace!`][896c] but only alters the displacement.

<a id="x-28MGL-MAT-3ARESHAPE-TO-ROW-MATRIX-21-20FUNCTION-29"></a>
<a id="MGL-MAT:RESHAPE-TO-ROW-MATRIX%21%20FUNCTION"></a>

- [function] **reshape-to-row-matrix!** *mat row*

    Reshape the 2d `mat` to make only a single `row` visible. This is made
    possible by the row-major layout, hence no column counterpart.
    Return `mat`.

<a id="x-28MGL-MAT-3AWITH-SHAPE-AND-DISPLACEMENT-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-SHAPE-AND-DISPLACEMENT%20MGL-PAX:MACRO"></a>

- [macro] **with-shape-and-displacement** *(mat &optional dimensions displacement) &body body*

    Reshape and displace `mat` if `dimensions` and/or `displacement` is given
    and restore the original shape and displacement after `body` is
    executed. If neither is specificed, then nothing will be changed,
    but `body` is still allowed to alter the shape and displacement.

<a id="x-28MGL-MAT-3AADJUST-21-20FUNCTION-29"></a>
<a id="MGL-MAT:ADJUST%21%20FUNCTION"></a>

- [function] **adjust!** *mat dimensions displacement &key (destroy-old-p t)*

    Like [`reshape-and-displace!`][896c] but creates a new matrix if `mat` isn't
    large enough. If a new matrix is created, the contents are not
    copied over and the old matrix is destroyed with [`destroy-cube`][dbac] if
    `destroy-old-p`.

<a id="x-28MGL-MAT-3A-40MAT-ASSEMBLING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-ASSEMBLING%20MGL-PAX:SECTION"></a>

## 8 Assembling

The functions here assemble a single [`mat`][6d14] from a number of
[`mat`][6d14]s.

<a id="x-28MGL-MAT-3ASTACK-21-20FUNCTION-29"></a>
<a id="MGL-MAT:STACK%21%20FUNCTION"></a>

- [function] **stack!** *axis mats mat*

    Stack `mats` along `axis` into `mat` and return `mat`. If `axis` is 0, place
    `mats` into `mat` below each other starting from the top. If `axis` is 1,
    place `mats` side by side starting from the left. Higher `axis` are also
    supported. All dimensions except for `axis` must be the same for all
    `mats`.

<a id="x-28MGL-MAT-3ASTACK-20FUNCTION-29"></a>
<a id="MGL-MAT:STACK%20FUNCTION"></a>

- [function] **stack** *axis mats &key (ctype \*default-mat-ctype\*)*

    Like [`stack!`][cd9e] but return a new [`mat`][6d14] of `ctype`.
    
    ```commonlisp
    (stack 1 (list (make-mat '(3 2) :initial-element 0)
                   (make-mat '(3 1) :initial-element 1)))
    ==> #<MAT 3x3 B #2A((0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0))>
    ```

<a id="x-28MGL-MAT-3A-40MAT-CACHING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CACHING%20MGL-PAX:SECTION"></a>

## 9 Caching

Allocating and initializing a [`mat`][6d14] object and its necessary facets
can be expensive. The following macros remember the previous value
of a binding in the same thread and /place/. Only weak references
are constructed so the cached objects can be garbage collected.

While the cache is global, thread safety is guaranteed by having
separate subcaches per thread. Each subcache is keyed by a /place/
object that's either explicitly specified or else is unique to each
invocation of the caching macro, so different occurrences of caching
macros in the source never share data. Still, recursion could lead
to data sharing between different invocations of the same function.
To prevent this, the cached object is removed from the cache while
it is used so other invocations will create a fresh one which isn't
particularly efficient but at least it's safe.

<a id="x-28MGL-MAT-3AWITH-THREAD-CACHED-MAT-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-THREAD-CACHED-MAT%20MGL-PAX:MACRO"></a>

- [macro] **with-thread-cached-mat** *(var dimensions &rest args &key (place :scratch) (ctype '\*default-mat-ctype\*) (displacement 0) max-size (initial-element 0) initial-contents) &body body*

    Bind `var` to a matrix of `dimensions`, `ctype`, etc. Cache this matrix,
    and possibly reuse it later by reshaping it. When `body` exits the
    cached object is updated with the binding of `var` which `body` may
    change.
    
    There is a separate cache for each thread and each `place` (under
    [`eq`][5a82]). Since every cache holds exactly one [`mat`][6d14] per `ctype`, nested
    `with-thread-cached-mat` often want to use different `place`s. By
    convention, these places are called `:scratch-1`, `:scratch-2`,
    etc.

<a id="x-28MGL-MAT-3AWITH-THREAD-CACHED-MATS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-THREAD-CACHED-MATS%20MGL-PAX:MACRO"></a>

- [macro] **with-thread-cached-mats** *specs &body body*

    A shorthand for writing nested [`with-thread-cached-mat`][aa72] calls.
    
    ```
    (with-thread-cached-mat (a ...)
      (with-thread-cached-mat (b ...)
        ...))
    ```
    
    is equivalent to:
    
    ```
    (with-thread-cached-mat ((a ...)
                             (b ...))
      ...)
    ```

<a id="x-28MGL-MAT-3AWITH-ONES-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-ONES%20MGL-PAX:MACRO"></a>

- [macro] **with-ones** *(var dimensions &key (ctype '\*default-mat-ctype\*) (place :ones)) &body body*

    Bind `var` to a matrix of `dimensions` whose every element is 1. The
    matrix is cached for efficiency.

<a id="x-28MGL-MAT-3A-40MAT-BLAS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-BLAS%20MGL-PAX:SECTION"></a>

## 10 BLAS Operations

Only some BLAS functions are implemented, but it should be easy to
add more as needed. All of them default to using CUDA, if it is
initialized and enabled (see [`use-cuda-p`][c442]).

Level 1 BLAS operations

<a id="x-28MGL-MAT-3AASUM-20FUNCTION-29"></a>
<a id="MGL-MAT:ASUM%20FUNCTION"></a>

- [function] **asum** *x &key (n (mat-size x)) (incx 1)*

    Return the l1 norm of `x`, that is, sum of the absolute values of its
    elements.

<a id="x-28MGL-MAT-3AAXPY-21-20FUNCTION-29"></a>
<a id="MGL-MAT:AXPY%21%20FUNCTION"></a>

- [function] **axpy!** *alpha x y &key (n (mat-size x)) (incx 1) (incy 1)*

    Set `y` to `alpha` \* `x` + `y`. Return `y`.

<a id="x-28MGL-MAT-3ACOPY-21-20FUNCTION-29"></a>
<a id="MGL-MAT:COPY%21%20FUNCTION"></a>

- [function] **copy!** *x y &key (n (mat-size x)) (incx 1) (incy 1)*

    Copy `x` into `y`. Return `y`.

<a id="x-28CL-CUDA-2ELANG-2EBUILT-IN-3ADOT-20FUNCTION-29"></a>
<a id="CL-CUDA.LANG.BUILT-IN:DOT%20FUNCTION"></a>

- [function] **dot** *x y &key (n (mat-size x)) (incx 1) (incy 1)*

    Return the dot product of `x` and `y`.

<a id="x-28MGL-MAT-3ANRM2-20FUNCTION-29"></a>
<a id="MGL-MAT:NRM2%20FUNCTION"></a>

- [function] **nrm2** *x &key (n (mat-size x)) (incx 1)*

    Return the l2 norm of `x`, which is the square root of the sum of the
    squares of its elements.

<a id="x-28MGL-MAT-3ASCAL-21-20FUNCTION-29"></a>
<a id="MGL-MAT:SCAL%21%20FUNCTION"></a>

- [function] **scal!** *alpha x &key (n (mat-size x)) (incx 1)*

    Set `x` to `alpha` \* `x`. Return `x`.

Level 3 BLAS operations

<a id="x-28MGL-MAT-3AGEMM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:GEMM%21%20FUNCTION"></a>

- [function] **gemm!** *alpha a b beta c &key transpose-a? transpose-b? m n k lda ldb ldc*

    Basically `c` = `alpha` \* `a`' \* `b`' + `beta` \* `c`. `a`' is `a` or its transpose
    depending on `transpose-a?`. `b`' is `b` or its transpose depending on
    `transpose-b?`. Returns `c`.
    
    `a`' is an MxK matrix. `b`' is a KxN matrix. `c` is an MxN matrix.
    
    `lda` is the width of the matrix `a` (not of `a`'). If `a` is not transposed,
    then `k` \<= `lda`, if it's transposed then `m` \<= `lda`.
    
    `ldb` is the width of the matrix `b` (not of `b`'). If `b` is not transposed,
    then `n` \<= `ldb`, if it's transposed then `k` \<= `ldb`.
    
    In the example below M=3, N=2, K=5, LDA=6, LDB=3, LDC=4. The cells
    marked with + do not feature in the calculation.
    
                   N
                  --+
                  --+
                K -B+
                  --+
                  --+
                  +++
            K
          -----+  --++
        M --A--+  -C++
          -----+  --++
          ++++++  ++++

<a id="x-28MGL-MAT-3A-40MAT-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-DESTRUCTIVE-API%20MGL-PAX:SECTION"></a>

## 11 Destructive API

<a id="x-28MGL-MAT-3A-2ESQUARE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.SQUARE%21%20FUNCTION"></a>

- [function] **.square!** *x &key (n (mat-size x))*

    Set `x` to its elementwise square. Return `x`.

<a id="x-28MGL-MAT-3A-2ESQRT-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.SQRT%21%20FUNCTION"></a>

- [function] **.sqrt!** *x &key (n (mat-size x))*

    Set `x` to its elementwise square root. Return `x`.

<a id="x-28MGL-MAT-3A-2ELOG-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.LOG%21%20FUNCTION"></a>

- [function] **.log!** *x &key (n (mat-size x))*

    Set `x` to its elementwise natural logarithm. Return `x`.

<a id="x-28MGL-MAT-3A-2EEXP-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.EXP%21%20FUNCTION"></a>

- [function] **.exp!** *x &key (n (mat-size x))*

    Apply [`exp`][bc8c] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2EEXPT-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.EXPT%21%20FUNCTION"></a>

- [function] **.expt!** *x power*

    Raise matrix `x` to `power` in an elementwise manner. Return `x`. Note
    that CUDA and non-CUDA implementations may disagree on the treatment
    of NaNs, infinities and complex results. In particular, the lisp
    implementation always computes the [`realpart`][64f7] of the results while
    CUDA's pow() returns NaNs instead of complex numbers.

<a id="x-28MGL-MAT-3A-2EINV-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.INV%21%20FUNCTION"></a>

- [function] **.inv!** *x &key (n (mat-size x))*

    Set `x` to its elementwise inverse `(/ 1 x)`. Return `x`.

<a id="x-28MGL-MAT-3A-2ELOGISTIC-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.LOGISTIC%21%20FUNCTION"></a>

- [function] **.logistic!** *x &key (n (mat-size x))*

    Destructively apply the logistic function to `x` in an elementwise
    manner. Return `x`.

<a id="x-28MGL-MAT-3A-2E-2B-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.+%21%20FUNCTION"></a>

- [function] **.+!** *alpha x*

    Add the scalar `alpha` to each element of `x` destructively modifying
    `x`. Return `x`.

<a id="x-28MGL-MAT-3A-2E-2A-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.*%21%20FUNCTION"></a>

- [function] **.\*!** *x y*

<a id="x-28MGL-MAT-3AGEEM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:GEEM%21%20FUNCTION"></a>

- [function] **geem!** *alpha a b beta c*

    Like [`gemm!`][e95b], but multiplication is elementwise. This is not a
    standard BLAS routine.

<a id="x-28MGL-MAT-3AGEERV-21-20FUNCTION-29"></a>
<a id="MGL-MAT:GEERV%21%20FUNCTION"></a>

- [function] **geerv!** *alpha a x beta b*

    GEneric Elementwise Row - Vector multiplication. `B = beta * B +
    alpha a .* X*` where `x*` is a matrix of the same shape as `a` whose
    every row is `x`. Perform elementwise multiplication on each row of `a`
    with the vector `x` and add the scaled result to the corresponding row
    of `b`. Return `b`. This is not a standard BLAS routine.

<a id="x-28MGL-MAT-3A-2E-3C-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.%3C%21%20FUNCTION"></a>

- [function] **.\<!** *x y*

    For each element of `x` and `y` set `y` to 1 if the element in `y` is
    greater than the element in `x`, and to 0 otherwise. Return `y`.

<a id="x-28MGL-MAT-3A-2EMIN-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.MIN%21%20FUNCTION"></a>

- [function] **.min!** *alpha x*

    Set each element of `x` to `alpha` if it's greater than `alpha`. Return
    `x`.

<a id="x-28MGL-MAT-3A-2EMAX-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.MAX%21%20FUNCTION"></a>

- [function] **.max!** *alpha x*

    Set each element of `x` to `alpha` if it's less than `alpha`. Return `x`.

<a id="x-28MGL-MAT-3AADD-SIGN-21-20FUNCTION-29"></a>
<a id="MGL-MAT:ADD-SIGN%21%20FUNCTION"></a>

- [function] **add-sign!** *alpha a beta b*

    Add the elementwise sign (-1, 0 or 1 for negative, zero and
    positive numbers respectively) of `a` times `alpha` to `beta` \* `b`. Return
    `b`.

<a id="x-28MGL-MAT-3AFILL-21-20FUNCTION-29"></a>
<a id="MGL-MAT:FILL%21%20FUNCTION"></a>

- [function] **fill!** *alpha x &key (n (mat-size x))*

    Fill matrix `x` with `alpha`. Return `x`.

<a id="x-28MGL-MAT-3ASUM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:SUM%21%20FUNCTION"></a>

- [function] **sum!** *x y &key axis (alpha 1) (beta 0)*

    Sum matrix `x` along `axis` and add `alpha` \* `sum`s to `beta` \* `y`
    destructively modifying `y`. Return `y`. On a 2d matrix (nothing else is
    supported currently), if `axis` is 0, then columns are summed, if `axis`
    is 1 then rows are summed.

<a id="x-28MGL-MAT-3ASCALE-ROWS-21-20FUNCTION-29"></a>
<a id="MGL-MAT:SCALE-ROWS%21%20FUNCTION"></a>

- [function] **scale-rows!** *scales a &key (result a)*

    Set `result` to `diag(scales)*a` and return it. `a` is an `MxN`
    matrix, `scales` is treated as a length `m` vector.

<a id="x-28MGL-MAT-3ASCALE-COLUMNS-21-20FUNCTION-29"></a>
<a id="MGL-MAT:SCALE-COLUMNS%21%20FUNCTION"></a>

- [function] **scale-columns!** *scales a &key (result a)*

    Set `result` to `a*diag(scales)` and return it. `a` is an `MxN`
    matrix, `scales` is treated as a length `n` vector.

<a id="x-28MGL-MAT-3A-2ESIN-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.SIN%21%20FUNCTION"></a>

- [function] **.sin!** *x &key (n (mat-size x))*

    Apply [`sin`][ece2] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2ECOS-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.COS%21%20FUNCTION"></a>

- [function] **.cos!** *x &key (n (mat-size x))*

    Apply [`cos`][c4a3] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2ETAN-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.TAN%21%20FUNCTION"></a>

- [function] **.tan!** *x &key (n (mat-size x))*

    Apply [`tan`][524a] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2ESINH-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.SINH%21%20FUNCTION"></a>

- [function] **.sinh!** *x &key (n (mat-size x))*

    Apply [`sinh`][0771] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2ECOSH-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.COSH%21%20FUNCTION"></a>

- [function] **.cosh!** *x &key (n (mat-size x))*

    Apply [`cosh`][fda1] elementwise to `x` in a destructive manner. Return `x`.

<a id="x-28MGL-MAT-3A-2ETANH-21-20FUNCTION-29"></a>
<a id="MGL-MAT:.TANH%21%20FUNCTION"></a>

- [function] **.tanh!** *x &key (n (mat-size x))*

    Apply [`tanh`][993b] elementwise to `x` in a destructive manner. Return `x`.

Finally, some neural network operations.

<a id="x-28MGL-MAT-3ACONVOLVE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:CONVOLVE%21%20FUNCTION"></a>

- [function] **convolve!** *x w y &key start stride anchor batched*

    `y` = `y` + conv(`x`, `w`) and return `y`. If `batched`, then the first
    dimension of `x` and `y` is the number of elements in the batch (B),
    else B is assumed to be 1. The rest of the dimensions encode the
    input (`x`) and output (`y`) N dimensional feature maps. `start`, `stride`
    and `anchor` are lists of length N. `start` is the multi-dimensional
    index of the first element of the input feature map (for each
    element in the batch) for which the convolution must be computed.
    Then ([`elt`][1023] `stride` (- N 1)) is added to the last element of `start` and
    so on until ([`array-dimension`][6c28] `x` 1) is reached. Then the last element
    of `start` is reset, (`elt` `stride` (- N 2)) is added to the first but
    last element of `start` and we scan the last dimension again. Take a
    2d example, `start` is (0 0), `stride` is (1 2), and `x` is a B\*2x7
    matrix.
    
    `w` is:
    
        1 2 1
        2 4 2
        1 2 1
    
    and `anchor` is (1 1) which refers to the element of `w` whose value is
    4. This anchor point of `w` is placed over elements of `x` whose multi
    dimensional index is in numbers in this figure (only one element in
    the batch is shown):
    
        0,0 . 0,2 . 0,4 . 0,6
        1,0 . 1,2 . 1,4 . 1,6
    
    When applying `w` at position P of `x`, the convolution is the sum of
    the products of overlapping elements of `x` and `w` when `w`'s `anchor` is
    placed at P. Elements of `w` over the edges of `x` are multiplied with 0
    so are effectively ignored. The order of application of `w` to
    positions defined by `start`, `stride` and `anchor` is undefined.
    
    `y` must be a B\*2x4 (or 2x4 if not `batched`) matrix in this example,
    just large enough to hold the results of the convolutions.

<a id="x-28MGL-MAT-3ADERIVE-CONVOLVE-21-20FUNCTION-29"></a>
<a id="MGL-MAT:DERIVE-CONVOLVE%21%20FUNCTION"></a>

- [function] **derive-convolve!** *x xd w wd yd &key start stride anchor batched*

    Add the dF/dX to `xd` and and dF/dW to `wd` where `yd` is dF/dY for some
    function F where Y is the result of convolution with the same
    arguments. 

<a id="x-28MGL-MAT-3AMAX-POOL-21-20FUNCTION-29"></a>
<a id="MGL-MAT:MAX-POOL%21%20FUNCTION"></a>

- [function] **max-pool!** *x y &key start stride anchor batched pool-dimensions*

<a id="x-28MGL-MAT-3ADERIVE-MAX-POOL-21-20FUNCTION-29"></a>
<a id="MGL-MAT:DERIVE-MAX-POOL%21%20FUNCTION"></a>

- [function] **derive-max-pool!** *x xd y yd &key start stride anchor batched pool-dimensions*

    Add the dF/dX to `xd` and and dF/dW to WD where `yd` is dF/dY for some
    function F where `y` is the result of [`max-pool!`][e17e] with the same
    arguments. 

<a id="x-28MGL-MAT-3A-40MAT-NON-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-NON-DESTRUCTIVE-API%20MGL-PAX:SECTION"></a>

## 12 Non-destructive API

<a id="x-28MGL-MAT-3ACOPY-MAT-20FUNCTION-29"></a>
<a id="MGL-MAT:COPY-MAT%20FUNCTION"></a>

- [function] **copy-mat** *a*

    Return a copy of the active portion with regards to displacement
    and shape of `a`. 

<a id="x-28MGL-MAT-3ACOPY-ROW-20FUNCTION-29"></a>
<a id="MGL-MAT:COPY-ROW%20FUNCTION"></a>

- [function] **copy-row** *a row*

    Return `row` of `a` as a new 1d matrix.

<a id="x-28MGL-MAT-3ACOPY-COLUMN-20FUNCTION-29"></a>
<a id="MGL-MAT:COPY-COLUMN%20FUNCTION"></a>

- [function] **copy-column** *a column*

    Return `column` of `a` as a new 1d matrix.

<a id="x-28MGL-MAT-3AMAT-AS-SCALAR-20FUNCTION-29"></a>
<a id="MGL-MAT:MAT-AS-SCALAR%20FUNCTION"></a>

- [function] **mat-as-scalar** *a*

    Return the first element of `a`. `a` must be of size 1.

<a id="x-28MGL-MAT-3ASCALAR-AS-MAT-20FUNCTION-29"></a>
<a id="MGL-MAT:SCALAR-AS-MAT%20FUNCTION"></a>

- [function] **scalar-as-mat** *x &key (ctype (lisp->ctype (type-of x)))*

    Return a matrix of one dimension and one element: `x`. `ctype`, the
    type of the matrix, defaults to the ctype corresponding to the type
    of `x`.

<a id="x-28MGL-MAT-3AM-3D-20FUNCTION-29"></a>
<a id="MGL-MAT:M%3D%20FUNCTION"></a>

- [function] **m=** *a b*

    Check whether `a` and `b`, which must be matrices of the same size, are
    elementwise equal.

<a id="x-28MGL-MAT-3ATRANSPOSE-20FUNCTION-29"></a>
<a id="MGL-MAT:TRANSPOSE%20FUNCTION"></a>

- [function] **transpose** *a*

    Return the transpose of `a`.

<a id="x-28MGL-MAT-3AM-2A-20FUNCTION-29"></a>
<a id="MGL-MAT:M*%20FUNCTION"></a>

- [function] **m\*** *a b &key transpose-a? transpose-b?*

    Compute op(`a`) \* op(`b`). Where op is either the identity or the
    transpose operation depending on `transpose-a?` and `transpose-b?`.

<a id="x-28MGL-MAT-3AMM-2A-20FUNCTION-29"></a>
<a id="MGL-MAT:MM*%20FUNCTION"></a>

- [function] **mm\*** *m &rest args*

    Convenience function to multiply several matrices. 
    
    (mm\* a b c) => a \* b \* c

<a id="x-28MGL-MAT-3AM--20FUNCTION-29"></a>
<a id="MGL-MAT:M-%20FUNCTION"></a>

- [function] **m-** *a b*

    Return `a` - `b`.

<a id="x-28MGL-MAT-3AM-2B-20FUNCTION-29"></a>
<a id="MGL-MAT:M+%20FUNCTION"></a>

- [function] **m+** *a b*

    Return `a` + `b`.

<a id="x-28MGL-MAT-3AINVERT-20FUNCTION-29"></a>
<a id="MGL-MAT:INVERT%20FUNCTION"></a>

- [function] **invert** *a*

    Return the inverse of `a`.

<a id="x-28MGL-MAT-3ALOGDET-20FUNCTION-29"></a>
<a id="MGL-MAT:LOGDET%20FUNCTION"></a>

- [function] **logdet** *mat*

    Logarithm of the determinant of `mat`. Return -1, 1 or 0 (or
    equivalent) to correct for the sign, as the second value.

<a id="x-28MGL-MAT-3A-40MAT-MAPPINGS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-MAPPINGS%20MGL-PAX:SECTION"></a>

## 13 Mappings

<a id="x-28MGL-MAT-3AMAP-CONCAT-20FUNCTION-29"></a>
<a id="MGL-MAT:MAP-CONCAT%20FUNCTION"></a>

- [function] **map-concat** *fn mats mat &key key pass-raw-p*

    Call `fn` with each element of `mats` and `mat` temporarily reshaped to
    the dimensions of the current element of `mats` and return `mat`. For
    the next element the displacement is increased so that there is no
    overlap.
    
    `mats` is keyed by `key` just like the CL sequence functions. Normally,
    `fn` is called with the matrix returned by `key`. However, if
    `pass-raw-p`, then the matrix returned by `key` is only used to
    calculate dimensions and the element of `mats` that was passed to `key`
    is passed to `fn`, too.
    
    ```
    (map-concat #'copy! (list (make-mat 2) (make-mat 4 :initial-element 1))
                (make-mat '(2 3)))
    ==> #<MAT 2x3 AB #2A((0.0d0 0.0d0 1.0d0) (1.0d0 1.0d0 1.0d0))>
    ```

<a id="x-28MGL-MAT-3AMAP-DISPLACEMENTS-20FUNCTION-29"></a>
<a id="MGL-MAT:MAP-DISPLACEMENTS%20FUNCTION"></a>

- [function] **map-displacements** *fn mat dimensions &key (displacement-start 0) displacement-step*

    Call `fn` with `mat` reshaped to `dimensions`, first displaced by
    `displacement-start` that's incremented by `displacement-step` each
    iteration while there are enough elements left for `dimensions` at the
    current displacement. Returns `mat`.
    
    ```commonlisp
    (let ((mat (make-mat 14 :initial-contents '(-1 0 1 2 3
                                                4 5 6 7
                                                8 9 10 11 12))))
      (reshape-and-displace! mat '(4 3) 1)
      (map-displacements #'print mat 4))
    ..
    .. #<MAT 1+4+9 B #(0.0d0 1.0d0 2.0d0 3.0d0)> 
    .. #<MAT 5+4+5 B #(4.0d0 5.0d0 6.0d0 7.0d0)> 
    .. #<MAT 9+4+1 B #(8.0d0 9.0d0 10.0d0 11.0d0)> 
    ```

<a id="x-28MGL-MAT-3AMAP-MATS-INTO-20FUNCTION-29"></a>
<a id="MGL-MAT:MAP-MATS-INTO%20FUNCTION"></a>

- [function] **map-mats-into** *result-mat fn &rest mats*

    Like [`cl:map-into`][a78a] but for [`mat`][6d14] objects. Destructively modifies
    `result-mat` to contain the results of applying `fn` to each element in
    the argument `mats` in turn.

<a id="x-28MGL-MAT-3A-40MAT-RANDOM-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-RANDOM%20MGL-PAX:SECTION"></a>

## 14 Random numbers

Unless noted these work efficiently with CUDA.

<a id="x-28MGL-MAT-3ACOPY-RANDOM-STATE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-MAT:COPY-RANDOM-STATE%20GENERIC-FUNCTION"></a>

- [generic-function] **copy-random-state** *state*

    Return a copy of `state` be it a lisp or cuda random
    state.

<a id="x-28MGL-MAT-3AUNIFORM-RANDOM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:UNIFORM-RANDOM%21%20FUNCTION"></a>

- [function] **uniform-random!** *mat &key (limit 1)*

    Fill `mat` with random numbers sampled uniformly from the \[0,LIMIT)
    interval of `mat`'s type.

<a id="x-28MGL-MAT-3AGAUSSIAN-RANDOM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:GAUSSIAN-RANDOM%21%20FUNCTION"></a>

- [function] **gaussian-random!** *mat &key (mean 0) (stddev 1)*

    Fill `mat` with independent normally distributed random numbers with
    `mean` and `stddev`.

<a id="x-28MGL-MAT-3AMV-GAUSSIAN-RANDOM-20FUNCTION-29"></a>
<a id="MGL-MAT:MV-GAUSSIAN-RANDOM%20FUNCTION"></a>

- [function] **mv-gaussian-random** *&key means covariances*

    Return a column vector of samples from the multivariate normal
    distribution defined by `means` (Nx1) and `covariances` (NxN). No CUDA
    implementation.

<a id="x-28MGL-MAT-3AORTHOGONAL-RANDOM-21-20FUNCTION-29"></a>
<a id="MGL-MAT:ORTHOGONAL-RANDOM%21%20FUNCTION"></a>

- [function] **orthogonal-random!** *m &key (scale 1)*

    Fill the matrix `m` with random values in such a way that `m^t * m`
    is the identity matrix (or something close if `m` is wide). Return `m`.

<a id="x-28MGL-MAT-3A-40MAT-IO-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-IO%20MGL-PAX:SECTION"></a>

## 15 I/O

<a id="x-28MGL-MAT-3A-2AMAT-HEADERS-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*MAT-HEADERS*%20VARIABLE"></a>

- [variable] **\*mat-headers\*** *t*

    If true, a header with [`mat-ctype`][62f5] and [`mat-size`][3573] is written by
    [`write-mat`][f0db] before the contents and [`read-mat`][420d] checks that these match
    the matrix into which it is reading.

<a id="x-28MGL-MAT-3AWRITE-MAT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-MAT:WRITE-MAT%20GENERIC-FUNCTION"></a>

- [generic-function] **write-mat** *mat stream*

    Write `mat` to binary `stream` in portable binary
    format. Return `mat`. Displacement and size are taken into account,
    only visible elements are written. Also see [`*mat-headers*`][a9da].

<a id="x-28MGL-MAT-3AREAD-MAT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-MAT:READ-MAT%20GENERIC-FUNCTION"></a>

- [generic-function] **read-mat** *mat stream*

    Destructively modify the visible portion (with
    regards to displacement and shape) of `mat` by reading [`mat-size`][3573] number
    of elements from binary `stream`. Return `mat`. Also see
    [`*mat-headers*`][a9da].

<a id="x-28MGL-MAT-3A-40MAT-DEBUGGING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-DEBUGGING%20MGL-PAX:SECTION"></a>

## 16 Debugging

The largest class of bugs has to do with synchronization of facets
being broken. This is almost always caused by an operation that
mispecifies the `direction` argument of [`with-facet`][0706]. For example, the
matrix argument of [`scal!`][3f21] should be accessed with direciton `:io`. But
if it's `:input` instead, then subsequent access to the `array`([`0`][1f99] [`1`][cb94]) facet
will not see the changes made by [`axpy!`][7408], and if it's `:output`, then
any changes made to the `array` facet since the last update of the
[`cuda-array`][7a7a] facet will not be copied and from the wrong input `scal!`
will compute the wrong result.

Using the SLIME inspector or trying to access the
[`cuda-array`][7a7a] facet from threads other than the one in
which the corresponding CUDA context was initialized will fail. For
now, the easy way out is to debug the code with CUDA disabled (see
[`*cuda-enabled*`][aba0]).

Another thing that tends to come up is figuring out where memory is
used.

<a id="x-28MGL-MAT-3AMAT-ROOM-20FUNCTION-29"></a>
<a id="MGL-MAT:MAT-ROOM%20FUNCTION"></a>

- [function] **mat-room** *&key (stream \*standard-output\*) (verbose t)*

    Calls [`foreign-room`][d5f8] and [`cuda-room`][010e].

<a id="x-28MGL-MAT-3AWITH-MAT-COUNTERS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-MAT-COUNTERS%20MGL-PAX:MACRO"></a>

- [macro] **with-mat-counters** *(&key count n-bytes) &body body*

    Count all [`mat`][6d14] allocations and also the number of bytes they may
    require. *May require* here really means an upper bound,
    because `(make-mat (expt 2 60))` doesn't actually uses memory until
    one of its facets is accessed (don't simply evaluate it though,
    printing the result will access the [`array`][cb94] facet if
    [`*print-mat*`][1f37]). Also, while facets today all require the same number
    of bytes, this may change in the future. This is a debugging tool,
    don't use it in production.
    
    ```common-lisp
    (with-mat-counters (:count count :n-bytes n-bytes)
      (assert (= count 0))
      (assert (= n-bytes 0))
      (make-mat '(2 3) :ctype :double)
      (assert (= count 1))
      (assert (= n-bytes (* 2 3 8)))
      (with-mat-counters (:n-bytes n-bytes-1 :count count-1)
        (make-mat '7 :ctype :float)
        (assert (= count-1 1))
        (assert (= n-bytes-1 (* 7 4))))
      (assert (= n-bytes (+ (* 2 3 8) (* 7 4))))
      (assert (= count 2)))
    ```

<a id="x-28MGL-MAT-3A-40MAT-FACET-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-FACET-API%20MGL-PAX:SECTION"></a>

## 17 Facet API



<a id="x-28MGL-MAT-3A-40MAT-FACETS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-FACETS%20MGL-PAX:SECTION"></a>

### 17.1 Facets

A [`mat`][6d14] is a [`cube`][1659] (see [Cube Manual][713f]) whose facets are
different representations of numeric arrays. These facets can be
accessed with [`with-facets`][ddd4] with one of the following
[`facet-name`][799d] locatives:

<a id="x-28MGL-MAT-3ABACKING-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
<a id="MGL-MAT:BACKING-ARRAY%20MGL-CUBE:FACET-NAME"></a>

- [facet-name] **backing-array**

    The corresponding facet's value is a one dimensional lisp array or
    a static vector that also looks exactly like a lisp array but is
    allocated in foreign memory. See [`*foreign-array-strategy*`][7400].

<a id="x-28ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
<a id="ARRAY%20MGL-CUBE:FACET-NAME"></a>

- [facet-name] **array**

    Same as [`backing-array`][1fda] if the matrix is one-dimensional, all
    elements are visible (see [Shaping][7bf8]), else it's a lisp array
    displaced to the backing array.

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
<a id="MGL-MAT:FOREIGN-ARRAY%20MGL-CUBE:FACET-NAME"></a>

- [facet-name] **foreign-array**

    The facet's value is a [`foreign-array`][cdc3] which is an
    `offset-pointer` wrapping a CFFI pointer. See
    [`*foreign-array-strategy*`][7400].

<a id="x-28MGL-MAT-3ACUDA-HOST-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
<a id="MGL-MAT:CUDA-HOST-ARRAY%20MGL-CUBE:FACET-NAME"></a>

- [facet-name] **cuda-host-array**

    This facet's value is a basically the same as that of
    [`foreign-array`][4f59]. In fact, they share storage. The
    difference is that accessing [`cuda-host-array`][0789] ensures
    that the foreign memory region is page-locked and registered with
    the CUDA Driver API function cuMemHostRegister(). Copying between
    GPU memory ([`cuda-array`][7a7a]) and registered memory is
    significantly faster than with non-registered memory and also allows
    overlapping copying with computation. See
    [`with-syncing-cuda-facets`][9fff].

<a id="x-28MGL-MAT-3ACUDA-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
<a id="MGL-MAT:CUDA-ARRAY%20MGL-CUBE:FACET-NAME"></a>

- [facet-name] **cuda-array**

    The facet's value is a `cuda-array`, which is an `offset-pointer`
    wrapping a `cl-cuda.driver-api:cu-device-ptr`, allocated with
    `cl-cuda.driver-api:cu-mem-alloc` and freed automatically.

Facets bound by with [`with-facets`][ddd4] are to be treated as dynamic
extent: it is not allowed to keep a reference to them beyond the
dynamic scope of `with-facets`.

For example, to implement the [`fill!`][510c] operation using only the
[`backing-array`][1fda], one could do this:

```commonlisp
(let ((displacement (mat-displacement x))
      (size (mat-size x)))
 (with-facets ((x* (x 'backing-array :direction :output)))
   (fill x* 1 :start displacement :end (+ displacement size))))
```

[`direction`][69de] is `:output` because we clobber all values in `x`. Armed
with this knowledge about the direction, `with-facets` will not copy
data from another facet if the backing array is not up-to-date.

To transpose a 2d matrix with the [`array`][cb94] facet:

```commonlisp
(destructuring-bind (n-rows n-columns) (mat-dimensions x)
  (with-facets ((x* (x 'array :direction :io)))
    (dotimes (row n-rows)
      (dotimes (column n-columns)
        (setf (aref x* row column) (aref x* column row))))))
```

Note that `direction` is `:io`, because we need the data in this facet
to be up-to-date (that's the input part) and we are invalidating all
other facets by changing values (that's the output part).

To sum the values of a matrix using the [`foreign-array`][4f59]
facet:

```commonlisp
(let ((sum 0))
  (with-facets ((x* (x 'foreign-array :direction :input)))
    (let ((pointer (offset-pointer x*)))
      (loop for index below (mat-size x)
            do (incf sum (cffi:mem-aref pointer (mat-ctype x) index)))))
  sum)
```

See `direction` for a complete description of `:input`, `:output` and `:io`.
For [`mat`][6d14] objects, that needs to be refined. If a `mat` is reshaped
and/or displaced in a way that not all elements are visible then
those elements are always kept intact and copied around. This is
accomplished by turning `:output` into `:io` automatically on such `mat`s.

We have finished our introduction to the various facets. It must be
said though that one can do anything without ever accessing a facet
directly or even being aware of them as most operations on `mat`s
take care of choosing the most appropriate facet behind the scenes.
In particular, most operations automatically use CUDA, if available
and initialized. See [`with-cuda*`][3db3] for detail.

<a id="x-28MGL-MAT-3A-40MAT-FOREIGN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-FOREIGN%20MGL-PAX:SECTION"></a>

### 17.2 Foreign arrays

One facet of [`mat`][6d14] objects is [`foreign-array`][4f59] which is
backed by a memory area that can be a pinned lisp array or is
allocated in foreign memory depending on [`*foreign-array-strategy*`][7400].

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-20CLASS-29"></a>
<a id="MGL-MAT:FOREIGN-ARRAY%20CLASS"></a>

- [class] **foreign-array**

    [`foreign-array`][cdc3] wraps a foreign pointer (in
    the sense of `cffi:pointerp`). That is, both `offset-pointer` and
    `base-pointer` return a foreign pointer. There are no other public
    operations that work with [`foreign-array`][cdc3] objects, their sole
    purpose is represent facets of [`mat`][6d14] objects.

<a id="x-28MGL-MAT-3A-2AFOREIGN-ARRAY-STRATEGY-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*FOREIGN-ARRAY-STRATEGY*%20VARIABLE"></a>

- [variable] **\*foreign-array-strategy\*** *"-see below-"*

    One of `:pinned`, `:static` and `:cuda-host` (see type
    [`foreign-array-strategy`][4495]). This variable controls how foreign arrays
    are handled, and it can be changed at any time.
    
    If it's `:pinned` (only supported if [`pinning-supported-p`][fae1]), then no
    separate storage is allocated for the foreign array. Instead, it
    aliases the lisp array (via the [`backing-array`][1fda] facet).
    
    If it's `:static`, then the lisp backing arrays are allocated
    statically via the static-vectors library. On some implementations,
    explicit freeing of static vectors is necessary, this is taken care
    of by finalizers or can be controlled with [`with-facet-barrier`][c44d].
    [`destroy-cube`][dbac] and [`destroy-facet`][39a0] may also be of help.
    
    `:cuda-host` is the same as `:static`, but any copies to/from the
    GPU (i.e. the [`cuda-array`][7a7a] facet) will be done via the
    [`cuda-host-array`][0789] facet whose memory pages will also be
    locked and registered with `cuMemHostRegister` which allows quicker
    and asynchronous copying to and from CUDA land.
    
    The default is `:pinned` if available, because it's the most
    efficient. If pinning is not available, then it's `:static`.

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-STRATEGY-20TYPE-29"></a>
<a id="MGL-MAT:FOREIGN-ARRAY-STRATEGY%20TYPE"></a>

- [type] **foreign-array-strategy**

    One of `:pinned`, `:static` and `:cuda-host`. See
    [`*foreign-array-strategy*`][7400] for their semantics.

<a id="x-28MGL-MAT-3APINNING-SUPPORTED-P-20FUNCTION-29"></a>
<a id="MGL-MAT:PINNING-SUPPORTED-P%20FUNCTION"></a>

- [function] **pinning-supported-p**

    Return true iff the lisp implementation efficiently supports
    pinning lisp arrays. Pinning ensures that the garbage collector
    doesn't move the array in memory. Currently this is only supported on
    SBCL gencgc platforms.

<a id="x-28MGL-MAT-3AFOREIGN-ROOM-20FUNCTION-29"></a>
<a id="MGL-MAT:FOREIGN-ROOM%20FUNCTION"></a>

- [function] **foreign-room** *&key (stream \*standard-output\*) (verbose t)*

    Print a summary of foreign memory usage to `stream`. If `verbose`, make
    the output human easily readable, else try to present it in a very
    concise way. Sample output with `verbose`:
    
    ```
    Foreign memory usage:
    foreign arrays: 450 (used bytes: 3,386,295,808)
    ```
    
    The same data presented with `verbose` false:
    
    ```
    f: 450 (3,386,295,808)
    ```

<a id="x-28MGL-MAT-3A-40MAT-CUDA-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CUDA%20MGL-PAX:SECTION"></a>

### 17.3 CUDA

<a id="x-28MGL-MAT-3ACUDA-AVAILABLE-P-20FUNCTION-29"></a>
<a id="MGL-MAT:CUDA-AVAILABLE-P%20FUNCTION"></a>

- [function] **cuda-available-p** *&key (device-id 0)*

    Check that a cuda context is already in initialized in the current
    thread or a device with `device-id` is available.

<a id="x-28MGL-MAT-3AWITH-CUDA-2A-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-CUDA*%20MGL-PAX:MACRO"></a>

- [macro] **with-cuda\*** *(&key (enabled '\*cuda-enabled\*) (device-id '\*cuda-default-device-id\*) (random-seed '\*cuda-default-random-seed\*) (n-random-states '\*cuda-default-n-random-states\*) n-pool-bytes) &body body*

    Initializes CUDA with with all bells and whistles before `body` and
    deinitializes it after. Simply wrapping `with-cuda*` around a piece
    code is enough to make use of the first available CUDA device or
    fall back on blas and lisp kernels if there is none.
    
    If CUDA is already initialized, then it sets up a facet barrier
    which destroys [`cuda-array`][7a7a] and [`cuda-host-array`][0789] facets after ensuring
    that the [`array`][cb94] facet is up-to-date.
    
    Else, if CUDA is available and `enabled`, then in addition to the
    facet barrier, a CUDA context is set up, [`*n-memcpy-host-to-device*`][9d0b],
    [`*n-memcpy-device-to-host*`][7332] are bound to zero, a cublas handle
    created, and [`*curand-state*`][8673] is bound to a [`curand-xorwow-state`][ffae] with
    `n-random-states`, seeded with `random-seed`, and allocation of device
    memory is limited to `n-pool-bytes` (`nil` means no limit, see
    [CUDA Memory Management][4e65]).
    
    Else - that is, if CUDA is not available, `body` is simply executed.

<a id="x-28MGL-MAT-3ACALL-WITH-CUDA-20FUNCTION-29"></a>
<a id="MGL-MAT:CALL-WITH-CUDA%20FUNCTION"></a>

- [function] **call-with-cuda** *fn &key ((:enabled \*cuda-enabled\*) \*cuda-enabled\*) (device-id \*cuda-default-device-id\*) (random-seed \*cuda-default-random-seed\*) (n-random-states \*cuda-default-n-random-states\*) n-pool-bytes*

    Like [`with-cuda*`][3db3], but takes a no argument function instead of the
    macro's `body`.

<a id="x-28MGL-MAT-3A-2ACUDA-ENABLED-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CUDA-ENABLED*%20VARIABLE"></a>

- [variable] **\*cuda-enabled\*** *t*

    Set or bind this to false to disable all use of cuda. If this is
    done from within `with-cuda`*, then cuda becomes temporarily disabled.
    If this is done from outside `with-cuda`*, then it changes the default
    values of the `enabled` argument of any future [`with-cuda*`][3db3]s which
    turns off cuda initialization entirely.

<a id="x-28MGL-MAT-3ACUDA-ENABLED-20-28MGL-PAX-3AACCESSOR-20MGL-MAT-3AMAT-29-29"></a>
<a id="MGL-MAT:CUDA-ENABLED%20%28MGL-PAX:ACCESSOR%20MGL-MAT:MAT%29"></a>

- [accessor] **cuda-enabled** *[mat][6d14] (:cuda-enabled = \*default-mat-cuda-enabled\*)*

    The control provided by [`*cuda-enabled*`][aba0] can be too
    coarse. This flag provides a per-object mechanism to turn cuda
    off. If it is set to `nil`, then any operation that pays attention
    to this flag will not create or access the [`cuda-array`][7a7a] facet.
    Implementationally speaking, this is easily accomplished by using
    [`use-cuda-p`][c442].

<a id="x-28MGL-MAT-3A-2ADEFAULT-MAT-CUDA-ENABLED-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*DEFAULT-MAT-CUDA-ENABLED*%20VARIABLE"></a>

- [variable] **\*default-mat-cuda-enabled\*** *t*

    The default for [`cuda-enabled`][47eb].

<a id="x-28MGL-MAT-3A-2AN-MEMCPY-HOST-TO-DEVICE-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*N-MEMCPY-HOST-TO-DEVICE*%20VARIABLE"></a>

- [variable] **\*n-memcpy-host-to-device\*** *0*

    Incremented each time a host to device copy is performed. Bound to
    0 by [`with-cuda*`][3db3]. Useful for tracking down performance problems.

<a id="x-28MGL-MAT-3A-2AN-MEMCPY-DEVICE-TO-HOST-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*N-MEMCPY-DEVICE-TO-HOST*%20VARIABLE"></a>

- [variable] **\*n-memcpy-device-to-host\*** *0*

    Incremented each time a device to host copy is performed. Bound to
    0 by [`with-cuda*`][3db3]. Useful for tracking down performance problems.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-DEVICE-ID-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CUDA-DEFAULT-DEVICE-ID*%20VARIABLE"></a>

- [variable] **\*cuda-default-device-id\*** *0*

    The default value of [`with-cuda*`][3db3]'s `:device-id` argument.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-RANDOM-SEED-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CUDA-DEFAULT-RANDOM-SEED*%20VARIABLE"></a>

- [variable] **\*cuda-default-random-seed\*** *1234*

    The default value of [`with-cuda*`][3db3]'s `:random-seed` argument.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-N-RANDOM-STATES-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CUDA-DEFAULT-N-RANDOM-STATES*%20VARIABLE"></a>

- [variable] **\*cuda-default-n-random-states\*** *4096*

    The default value of [`with-cuda*`][3db3]'s `:n-random-states` argument.

<a id="x-28MGL-MAT-3A-40MAT-CUDA-MEMORY-MANAGEMENT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CUDA-MEMORY-MANAGEMENT%20MGL-PAX:SECTION"></a>

#### 17.3.1 CUDA Memory Management

The GPU (called *device* in CUDA terminology) has its own memory
and it can only perform computation on data in this *device memory*
so there is some copying involved to and from main memory. Efficient
algorithms often allocate device memory up front and minimize the
amount of copying that has to be done by computing as much as
possible on the GPU.

MGL-MAT reduces the cost of device of memory allocations by
maintaining a cache of currently unused allocations from which it
first tries to satisfy allocation requests. The total size of all
the allocated device memory regions (be they in use or currently
unused but cached) is never more than `n-pool-bytes` as specified in
[`with-cuda*`][3db3]. `n-pool-bytes` being `nil` means no limit.

<a id="x-28MGL-MAT-3ACUDA-OUT-OF-MEMORY-20CONDITION-29"></a>
<a id="MGL-MAT:CUDA-OUT-OF-MEMORY%20CONDITION"></a>

- [condition] **cuda-out-of-memory** *[storage-condition][ecf9]*

    If an allocation request cannot be
    satisfied (either because of `n-pool-bytes` or physical device memory
    limits being reached), then `cuda-out-of-memory` is signalled.

<a id="x-28MGL-MAT-3ACUDA-ROOM-20FUNCTION-29"></a>
<a id="MGL-MAT:CUDA-ROOM%20FUNCTION"></a>

- [function] **cuda-room** *&key (stream \*standard-output\*) (verbose t)*

    When CUDA is in use (see [`use-cuda-p`][c442]), print a summary of memory
    usage in the current CUDA context to `stream`. If `verbose`, make the
    output human easily readable, else try to present it in a very
    concise way. Sample output with `verbose`:
    
    ```
    CUDA memory usage:
    device arrays: 450 (used bytes: 3,386,295,808, pooled bytes: 1,816,657,920)
    host arrays: 14640 (used bytes: 17,380,147,200)
    host->device copies: 154,102,488, device->host copies: 117,136,434
    ```
    
    The same data presented with `verbose` false:
    
    ```
    d: 450 (3,386,295,808 + 1,816,657,920), h: 14640 (17,380,147,200)
    h->d: 154,102,488, d->h: 117,136,434
    ```

That's it about reducing the cost allocations. The other important
performance consideration, minimizing the amount copying done, is
very hard to do if the data doesn't fit in device memory which is
often a very limited resource. In this case the next best thing is
to do the copying concurrently with computation.

<a id="x-28MGL-MAT-3AWITH-SYNCING-CUDA-FACETS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-SYNCING-CUDA-FACETS%20MGL-PAX:MACRO"></a>

- [macro] **with-syncing-cuda-facets** *(mats-to-cuda mats-to-cuda-host &key (safep '\*syncing-cuda-facets-safe-p\*)) &body body*

    Update CUDA facets in a possibly asynchronous way while `body`
    executes. Behind the scenes, a separate CUDA stream is used to copy
    between registered host memory and device memory. When
    `with-syncing-cuda-facets` finishes either by returning normally or by
    a performing a non-local-exit the following are true:
    
    - All [`mat`][6d14]s in `mats-to-cuda` have an up-to-date
      [`cuda-array`][7a7a] facet.
    
    - All `mat`s in `mats-to-cuda-host` have an up-to-date
      [`cuda-host-array`][0789] facet and no
      [`cuda-array`][7a7a].
    
    It is an error if the same matrix appears in both `mats-to-cuda` and
    `mats-to-cuda-host`, but the same matrix may appear any number of
    times in one of them.
    
    If `safep` is true, then the all matrices in either of the two lists
    are effectively locked for output until `with-syncing-cuda-facets`
    finishes. With SAFE `nil`, unsafe accesses to facets of these matrices
    are not detected, but the whole operation has a bit less overhead.

<a id="x-28MGL-MAT-3A-2ASYNCING-CUDA-FACETS-SAFE-P-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*SYNCING-CUDA-FACETS-SAFE-P*%20VARIABLE"></a>

- [variable] **\*syncing-cuda-facets-safe-p\*** *t*

    The default value of the `safep` argument of
    [`with-syncing-cuda-facets`][9fff].

Also note that often the easiest thing to do is to prevent the use
of CUDA (and consequently the creation of [`cuda-array`][7a7a]
facets, and allocations). This can be done either by binding
[`*cuda-enabled*`][aba0] to `nil` or by setting [`cuda-enabled`][47eb] to `nil` on specific
matrices.

<a id="x-28MGL-MAT-3A-40MAT-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-EXTENSIONS%20MGL-PAX:SECTION"></a>

## 18 Writing Extensions

New operations are usually implemented in lisp, CUDA, or by calling
a foreign function in, for instance, BLAS, CUBLAS, CURAND.

<a id="x-28MGL-MAT-3A-40MAT-LISP-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-LISP-EXTENSIONS%20MGL-PAX:SECTION"></a>

### 18.1 Lisp Extensions

<a id="x-28MGL-MAT-3ADEFINE-LISP-KERNEL-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:DEFINE-LISP-KERNEL%20MGL-PAX:MACRO"></a>

- [macro] **define-lisp-kernel** *(name &key (ctypes '(:float :double))) (&rest params) &body body*

    This is very much like [`define-cuda-kernel`][4074] but for normal lisp code.
    It knows how to deal with [`mat`][6d14] objects and can define the same
    function for multiple `ctypes`. Example:
    
    ```commonlisp
    (define-lisp-kernel (lisp-.+!)
        ((alpha single-float) (x :mat :input) (start-x index) (n index))
      (loop for xi of-type index upfrom start-x
              below (the! index (+ start-x n))
            do (incf (aref x xi) alpha)))
    ```
    
    Parameters are either of the form `(<name> <lisp-type)`
    or `(<name> :mat <direction>)`. In the latter case, the appropriate
    CFFI pointer is passed to the kernel. `<direction>` is passed on to
    the [`with-facet`][0706] that's used to acquire the foreign array. Note that
    the return type is not declared.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `ctypes` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this means that one needs to write only one
    kernel for [`single-float`][31a6] and [`double-float`][0d57]. All such functions get the
    declaration from [`*default-lisp-kernel-declarations*`][6a5e].
    
    Finally, a dispatcher function with `name` is defined which determines
    the ctype of the `mat` objects passed for `:mat` typed parameters. It's
    an error if they are not of the same type. Scalars declared
    `single-float` are coerced to that type and the appropriate kernel is
    called.

<a id="x-28MGL-MAT-3A-2ADEFAULT-LISP-KERNEL-DECLARATIONS-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*DEFAULT-LISP-KERNEL-DECLARATIONS*%20VARIABLE"></a>

- [variable] **\*default-lisp-kernel-declarations\*** *((optimize speed (sb-c:insert-array-bounds-checks 0)))*

    These declarations are added automatically to kernel functions.

<a id="x-28MGL-MAT-3A-40MAT-CUDA-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CUDA-EXTENSIONS%20MGL-PAX:SECTION"></a>

### 18.2 CUDA Extensions

<a id="x-28MGL-MAT-3AUSE-CUDA-P-20FUNCTION-29"></a>
<a id="MGL-MAT:USE-CUDA-P%20FUNCTION"></a>

- [function] **use-cuda-p** *&rest mats*

    Return true if cuda is enabled ([`*cuda-enabled*`][aba0]), it's initialized
    and all `mats` have [`cuda-enabled`][47eb]. Operations of
    matrices use this to decide whether to go for the CUDA
    implementation or BLAS/Lisp. It's provided for implementing new
    operations.

<a id="x-28MGL-MAT-3ACHOOSE-1D-BLOCK-AND-GRID-20FUNCTION-29"></a>
<a id="MGL-MAT:CHOOSE-1D-BLOCK-AND-GRID%20FUNCTION"></a>

- [function] **choose-1d-block-and-grid** *n max-n-warps-per-block*

    Return two values, one suitable as the `:block-dim`, the other as
    the `:grid-dim` argument for a cuda kernel call where both are
    one-dimensional (only the first element may be different from 1).
    
    The number of threads in a block is a multiple of `*cuda-warp-size*`.
    The number of blocks is between 1 and and `*cuda-max-n-blocks*`. This
    means that the kernel must be able handle any number of elements in
    each thread. For example, a strided kernel that adds a constant to
    each element of a length `n` vector looks like this:
    
    ```
    (let ((stride (* block-dim-x grid-dim-x)))
      (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
              (+ i stride)))
          ((>= i n))
        (set (aref x i) (+ (aref x i) alpha))))
    ```
    
    It is often the most efficient to have `max-n-warps-per-block` around
    4. Note that the maximum number of threads per block is limited by
    hardware (512 for compute capability \< 2.0, 1024 for later
    versions), so `*cuda-max-n-blocks*` times `max-n-warps-per-block` must
    not exceed that limit.

<a id="x-28MGL-MAT-3ACHOOSE-2D-BLOCK-AND-GRID-20FUNCTION-29"></a>
<a id="MGL-MAT:CHOOSE-2D-BLOCK-AND-GRID%20FUNCTION"></a>

- [function] **choose-2d-block-and-grid** *dimensions max-n-warps-per-block*

    Return two values, one suitable as the `:block-dim`, the other as
    the `:grid-dim` argument for a cuda kernel call where both are
    two-dimensional (only the first two elements may be different from
    1).
    
    The number of threads in a block is a multiple of `*cuda-warp-size*`.
    The number of blocks is between 1 and and `*cuda-max-n-blocks*`.
    Currently - but this may change - the `block-dim-x` is always
    `*cuda-warp-size*` and `grid-dim-x` is always 1.
    
    This means that the kernel must be able handle any number of
    elements in each thread. For example, a strided kernel that adds a
    constant to each element of a HEIGHT\*WIDTH matrix looks like this:
    
    ```
    (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
          (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
          (stride-x (* block-dim-x grid-dim-x))
          (stride-y (* block-dim-y grid-dim-y)))
      (do ((row id-y (+ row stride-y)))
          ((>= row height))
        (let ((i (* row width)))
          (do ((column id-x (+ column stride-x)))
              ((>= column width))
            (set (aref x i) (+ (aref x i) alpha))
            (incf i stride-x)))))
    ```

<a id="x-28MGL-MAT-3ACHOOSE-3D-BLOCK-AND-GRID-20FUNCTION-29"></a>
<a id="MGL-MAT:CHOOSE-3D-BLOCK-AND-GRID%20FUNCTION"></a>

- [function] **choose-3d-block-and-grid** *dimensions max-n-warps-per-block*

    Return two values, one suitable as the `:block-dim`, the other as
    the `:grid-dim` argument for a cuda kernel call where both are
    two-dimensional (only the first two elements may be different from
    1).
    
    The number of threads in a block is a multiple of `*cuda-warp-size*`.
    The number of blocks is between 1 and and `*cuda-max-n-blocks*`.
    Currently - but this may change - the `block-dim-x` is always
    `*cuda-warp-size*` and `grid-dim-x` is always 1.
    
    This means that the kernel must be able handle any number of
    elements in each thread. For example, a strided kernel that adds a
    constant to each element of a `thickness` \* `height` \* `width` 3d array
    looks like this:
    
    ```
    (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
          (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
          (id-z (+ (* block-dim-z block-idx-z) thread-idx-z))
          (stride-x (* block-dim-x grid-dim-x))
          (stride-y (* block-dim-y grid-dim-y))
          (stride-z (* block-dim-z grid-dim-z)))
      (do ((plane id-z (+ plane stride-z)))
          ((>= plane thickness))
        (do ((row id-y (+ row stride-y)))
            ((>= row height))
          (let ((i (* (+ (* plane height) row)
                      width)))
            (do ((column id-x (+ column stride-x)))
                ((>= column width))
              (set (aref x i) (+ (aref x i) alpha))
              (incf i stride-x))))))
    ```

<a id="x-28MGL-MAT-3ADEFINE-CUDA-KERNEL-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:DEFINE-CUDA-KERNEL%20MGL-PAX:MACRO"></a>

- [macro] **define-cuda-kernel** *(name &key (ctypes '(:float :double))) (return-type params) &body body*

    This is an extended `cl-cuda:defkernel` macro. It knows how to deal
    with [`mat`][6d14] objects and can define the same function for multiple
    `ctypes`. Example:
    
    ```commonlisp
    (define-cuda-kernel (cuda-.+!)
        (void ((alpha float) (x :mat :input) (n int)))
      (let ((stride (* block-dim-x grid-dim-x)))
        (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
                (+ i stride)))
            ((>= i n))
          (set (aref x i) (+ (aref x i) alpha)))))
    ```
    
    The signature looks pretty much like in `cl-cuda:defkernel`, but
    parameters can take the form of `(<name> :mat <direction>)` too, in
    which case the appropriate `cl-cuda.driver-api:cu-device-ptr` is
    passed to the kernel. `<direction>` is passed on to the [`with-facet`][0706]
    that's used to acquire the cuda array.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `ctypes` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this means that one needs to write only one
    kernel for `float` and `double`.
    
    Finally, a dispatcher function with `name` is defined which determines
    the ctype of the `mat` objects passed for `:mat` typed parameters. It's
    an error if they are not of the same type. Scalars declared
    `float` are coerced to that type and the appropriate
    kernel is called.

<a id="x-28MGL-MAT-3A-40MAT-CUBLAS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CUBLAS%20MGL-PAX:SECTION"></a>

#### 18.2.1 CUBLAS

In a [`with-cuda*`][3db3] [BLAS Operations][4958] will automatically use CUBLAS. No need to
use these at all.

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-20CONDITION-29"></a>
<a id="MGL-MAT:CUBLAS-ERROR%20CONDITION"></a>

- [condition] **cublas-error** *[error][d162]*

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-FUNCTION-NAME-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29"></a>
<a id="MGL-MAT:CUBLAS-ERROR-FUNCTION-NAME%20%28MGL-PAX:READER%20MGL-MAT:CUBLAS-ERROR%29"></a>

- [reader] **cublas-error-function-name** *[cublas-error][db71] (:function-name)*

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-STATUS-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29"></a>
<a id="MGL-MAT:CUBLAS-ERROR-STATUS%20%28MGL-PAX:READER%20MGL-MAT:CUBLAS-ERROR%29"></a>

- [reader] **cublas-error-status** *[cublas-error][db71] (:status)*

<a id="x-28MGL-MAT-3A-2ACUBLAS-HANDLE-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CUBLAS-HANDLE*%20VARIABLE"></a>

- [variable] **\*cublas-handle\***

<a id="x-28MGL-MAT-3ACUBLAS-CREATE-20FUNCTION-29"></a>
<a id="MGL-MAT:CUBLAS-CREATE%20FUNCTION"></a>

- [function] **cublas-create** *handle*

<a id="x-28MGL-MAT-3ACUBLAS-DESTROY-20FUNCTION-29"></a>
<a id="MGL-MAT:CUBLAS-DESTROY%20FUNCTION"></a>

- [function] **cublas-destroy** *&key (handle \*cublas-handle\*)*

<a id="x-28MGL-MAT-3AWITH-CUBLAS-HANDLE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-CUBLAS-HANDLE%20MGL-PAX:MACRO"></a>

- [macro] **with-cublas-handle** *nil &body body*

<a id="x-28MGL-MAT-3ACUBLAS-GET-VERSION-20FUNCTION-29"></a>
<a id="MGL-MAT:CUBLAS-GET-VERSION%20FUNCTION"></a>

- [function] **cublas-get-version** *version &key (handle \*cublas-handle\*)*

<a id="x-28MGL-MAT-3A-40MAT-CURAND-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-MAT:@MAT-CURAND%20MGL-PAX:SECTION"></a>

#### 18.2.2 CURAND

This the low level CURAND API. You probably want [Random numbers][c669]
instead.

<a id="x-28MGL-MAT-3AWITH-CURAND-STATE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-MAT:WITH-CURAND-STATE%20MGL-PAX:MACRO"></a>

- [macro] **with-curand-state** *(state) &body body*

<a id="x-28MGL-MAT-3A-2ACURAND-STATE-2A-20VARIABLE-29"></a>
<a id="MGL-MAT:*CURAND-STATE*%20VARIABLE"></a>

- [variable] **\*curand-state\***

<a id="x-28MGL-MAT-3ACURAND-XORWOW-STATE-20CLASS-29"></a>
<a id="MGL-MAT:CURAND-XORWOW-STATE%20CLASS"></a>

- [class] **curand-xorwow-state**

<a id="x-28MGL-MAT-3AN-STATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29"></a>
<a id="MGL-MAT:N-STATES%20%28MGL-PAX:READER%20MGL-MAT:CURAND-XORWOW-STATE%29"></a>

- [reader] **n-states** *[curand-xorwow-state][ffae] (:n-states)*

<a id="x-28MGL-MAT-3ASTATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29"></a>
<a id="MGL-MAT:STATES%20%28MGL-PAX:READER%20MGL-MAT:CURAND-XORWOW-STATE%29"></a>

- [reader] **states** *[curand-xorwow-state][ffae] (:states)*

  [010e]: #MGL-MAT:CUDA-ROOM%20FUNCTION "MGL-MAT:CUDA-ROOM FUNCTION"
  [0706]: cube-manual.md#MGL-CUBE:WITH-FACET%20MGL-PAX:MACRO "MGL-CUBE:WITH-FACET MGL-PAX:MACRO"
  [0771]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "SINH (MGL-PAX:CLHS FUNCTION)"
  [0789]: #MGL-MAT:CUDA-HOST-ARRAY%20MGL-CUBE:FACET-NAME "MGL-MAT:CUDA-HOST-ARRAY MGL-CUBE:FACET-NAME"
  [086b]: #MGL-MAT:@MAT-BASICS%20MGL-PAX:SECTION "Basics"
  [0d57]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "DOUBLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [0ecb]: #MGL-MAT:REPLACE%21%20FUNCTION "MGL-MAT:REPLACE! FUNCTION"
  [1023]: http://www.lispworks.com/documentation/HyperSpec/Body/f_elt.htm "ELT (MGL-PAX:CLHS FUNCTION)"
  [1354]: #MGL-MAT:CTYPE%20TYPE "MGL-MAT:CTYPE TYPE"
  [1600]: #MGL-MAT:@MAT-MAPPINGS%20MGL-PAX:SECTION "Mappings"
  [1659]: cube-manual.md#MGL-CUBE:CUBE%20CLASS "MGL-CUBE:CUBE CLASS"
  [1b99]: #MGL-MAT:@MAT-TUTORIAL%20MGL-PAX:SECTION "Tutorial"
  [1f37]: #MGL-MAT:*PRINT-MAT*%20VARIABLE "MGL-MAT:*PRINT-MAT* VARIABLE"
  [1f99]: http://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm "ARRAY (MGL-PAX:CLHS CLASS)"
  [1fda]: #MGL-MAT:BACKING-ARRAY%20MGL-CUBE:FACET-NAME "MGL-MAT:BACKING-ARRAY MGL-CUBE:FACET-NAME"
  [2053]: http://www.lispworks.com/documentation/HyperSpec/Body/f_row_ma.htm "ROW-MAJOR-AREF (MGL-PAX:CLHS FUNCTION)"
  [2cbb]: #MGL-MAT:@MAT-SHAPING-COMPARISON-TO-LISP%20MGL-PAX:SECTION "Comparison to Lisp Arrays"
  [2ea4]: #MGL-MAT:@MAT-WHAT-KIND-OF-MATRICES%20MGL-PAX:SECTION "What kind of matrices are supported?"
  [3046]: #MGL-MAT:*DEFAULT-MAT-CTYPE*%20VARIABLE "MGL-MAT:*DEFAULT-MAT-CTYPE* VARIABLE"
  [31a6]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "SINGLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [3573]: #MGL-MAT:MAT-SIZE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29 "MGL-MAT:MAT-SIZE (MGL-PAX:READER MGL-MAT:MAT)"
  [36b5]: #MGL-MAT:@MAT-FOREIGN%20MGL-PAX:SECTION "Foreign arrays"
  [36d6]: cube-manual.md#MGL-CUBE:FACET%20STRUCTURE "MGL-CUBE:FACET STRUCTURE"
  [3993]: #MGL-MAT:@MAT-CURAND%20MGL-PAX:SECTION "CURAND"
  [39a0]: cube-manual.md#MGL-CUBE:DESTROY-FACET%20FUNCTION "MGL-CUBE:DESTROY-FACET FUNCTION"
  [3db3]: #MGL-MAT:WITH-CUDA*%20MGL-PAX:MACRO "MGL-MAT:WITH-CUDA* MGL-PAX:MACRO"
  [3e36]: #MGL-MAT:@MAT-NON-DESTRUCTIVE-API%20MGL-PAX:SECTION "Non-destructive API"
  [3f21]: #MGL-MAT:SCAL%21%20FUNCTION "MGL-MAT:SCAL! FUNCTION"
  [4074]: #MGL-MAT:DEFINE-CUDA-KERNEL%20MGL-PAX:MACRO "MGL-MAT:DEFINE-CUDA-KERNEL MGL-PAX:MACRO"
  [420d]: #MGL-MAT:READ-MAT%20GENERIC-FUNCTION "MGL-MAT:READ-MAT GENERIC-FUNCTION"
  [43e9]: #MGL-MAT:@MAT-PRINTING%20MGL-PAX:SECTION "Printing"
  [4495]: #MGL-MAT:FOREIGN-ARRAY-STRATEGY%20TYPE "MGL-MAT:FOREIGN-ARRAY-STRATEGY TYPE"
  [46e6]: #MGL-MAT:@MAT-ASSEMBLING%20MGL-PAX:SECTION "Assembling"
  [47eb]: #MGL-MAT:CUDA-ENABLED%20%28MGL-PAX:ACCESSOR%20MGL-MAT:MAT%29 "MGL-MAT:CUDA-ENABLED (MGL-PAX:ACCESSOR MGL-MAT:MAT)"
  [4821]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_tot.htm "ARRAY-TOTAL-SIZE (MGL-PAX:CLHS FUNCTION)"
  [4958]: #MGL-MAT:@MAT-BLAS%20MGL-PAX:SECTION "BLAS Operations"
  [4e65]: #MGL-MAT:@MAT-CUDA-MEMORY-MANAGEMENT%20MGL-PAX:SECTION "CUDA Memory Management"
  [4f59]: #MGL-MAT:FOREIGN-ARRAY%20MGL-CUBE:FACET-NAME "MGL-MAT:FOREIGN-ARRAY MGL-CUBE:FACET-NAME"
  [510c]: #MGL-MAT:FILL%21%20FUNCTION "MGL-MAT:FILL! FUNCTION"
  [5212]: #MGL-MAT:@MAT-CUDA%20MGL-PAX:SECTION "CUDA"
  [524a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "TAN (MGL-PAX:CLHS FUNCTION)"
  [545d]: #MGL-MAT:@MAT-DESTRUCTIVE-API%20MGL-PAX:SECTION "Destructive API"
  [54ea]: #MGL-MAT:@MAT-LINKS%20MGL-PAX:SECTION "Links"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [62f5]: #MGL-MAT:MAT-CTYPE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29 "MGL-MAT:MAT-CTYPE (MGL-PAX:READER MGL-MAT:MAT)"
  [64f7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_realpa.htm "REALPART (MGL-PAX:CLHS FUNCTION)"
  [69de]: cube-manual.md#MGL-CUBE:DIRECTION%20TYPE "MGL-CUBE:DIRECTION TYPE"
  [6a5e]: #MGL-MAT:*DEFAULT-LISP-KERNEL-DECLARATIONS*%20VARIABLE "MGL-MAT:*DEFAULT-LISP-KERNEL-DECLARATIONS* VARIABLE"
  [6c28]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dim.htm "ARRAY-DIMENSION (MGL-PAX:CLHS FUNCTION)"
  [6d14]: #MGL-MAT:MAT%20CLASS "MGL-MAT:MAT CLASS"
  [6ead]: #MGL-MAT:COERCE-TO-CTYPE%20FUNCTION "MGL-MAT:COERCE-TO-CTYPE FUNCTION"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [713f]: cube-manual.md "Cube Manual"
  [72ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_row.htm "ARRAY-ROW-MAJOR-INDEX (MGL-PAX:CLHS FUNCTION)"
  [7332]: #MGL-MAT:*N-MEMCPY-DEVICE-TO-HOST*%20VARIABLE "MGL-MAT:*N-MEMCPY-DEVICE-TO-HOST* VARIABLE"
  [73f6]: #MGL-MAT:MAT-DIMENSIONS%20%28MGL-PAX:READER%20MGL-MAT:MAT%29 "MGL-MAT:MAT-DIMENSIONS (MGL-PAX:READER MGL-MAT:MAT)"
  [7400]: #MGL-MAT:*FOREIGN-ARRAY-STRATEGY*%20VARIABLE "MGL-MAT:*FOREIGN-ARRAY-STRATEGY* VARIABLE"
  [7408]: #MGL-MAT:AXPY%21%20FUNCTION "MGL-MAT:AXPY! FUNCTION"
  [799d]: cube-manual.md#MGL-CUBE:FACET-NAME%20MGL-PAX:LOCATIVE "MGL-CUBE:FACET-NAME MGL-PAX:LOCATIVE"
  [7a7a]: #MGL-MAT:CUDA-ARRAY%20MGL-CUBE:FACET-NAME "MGL-MAT:CUDA-ARRAY MGL-CUBE:FACET-NAME"
  [7bf8]: #MGL-MAT:@MAT-SHAPING%20MGL-PAX:SECTION "Shaping"
  [7e60]: #MGL-MAT:@MAT-INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [7ecb]: #MGL-MAT:@MAT-FACETS%20MGL-PAX:SECTION "Facets"
  [7feb]: #MGL-MAT:@MAT-IO%20MGL-PAX:SECTION "I/O"
  [8100]: #MGL-MAT:ADJUST%21%20FUNCTION "MGL-MAT:ADJUST! FUNCTION"
  [8673]: #MGL-MAT:*CURAND-STATE*%20VARIABLE "MGL-MAT:*CURAND-STATE* VARIABLE"
  [896c]: #MGL-MAT:RESHAPE-AND-DISPLACE%21%20FUNCTION "MGL-MAT:RESHAPE-AND-DISPLACE! FUNCTION"
  [8eda]: #MGL-MAT:@MAT-FACET-API%20MGL-PAX:SECTION "Facet API"
  [8f0e]: #MGL-MAT:@MAT-EXTENSIONS%20MGL-PAX:SECTION "Writing Extensions"
  [92ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm "MAKE-ARRAY (MGL-PAX:CLHS FUNCTION)"
  [93e1]: #MGL-MAT:@MAT-CUDA-EXTENSIONS%20MGL-PAX:SECTION "CUDA Extensions"
  [993b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "TANH (MGL-PAX:CLHS FUNCTION)"
  [9d0b]: #MGL-MAT:*N-MEMCPY-HOST-TO-DEVICE*%20VARIABLE "MGL-MAT:*N-MEMCPY-HOST-TO-DEVICE* VARIABLE"
  [9fff]: #MGL-MAT:WITH-SYNCING-CUDA-FACETS%20MGL-PAX:MACRO "MGL-MAT:WITH-SYNCING-CUDA-FACETS MGL-PAX:MACRO"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a5a6]: cube-manual.md#MGL-CUBE:@CUBE-SYNCHRONIZATION%20MGL-PAX:SECTION "Synchronization"
  [a78a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm "MAP-INTO (MGL-PAX:CLHS FUNCTION)"
  [a9da]: #MGL-MAT:*MAT-HEADERS*%20VARIABLE "MGL-MAT:*MAT-HEADERS* VARIABLE"
  [aa5b]: #MGL-MAT:@MAT-SHAPING-FUNCTIONAL%20MGL-PAX:SECTION "Functional Shaping"
  [aa72]: #MGL-MAT:WITH-THREAD-CACHED-MAT%20MGL-PAX:MACRO "MGL-MAT:WITH-THREAD-CACHED-MAT MGL-PAX:MACRO"
  [aba0]: #MGL-MAT:*CUDA-ENABLED*%20VARIABLE "MGL-MAT:*CUDA-ENABLED* VARIABLE"
  [ac8d]: #MGL-MAT:@MAT-DEBUGGING%20MGL-PAX:SECTION "Debugging"
  [b116]: #MGL-MAT:@MAT-CUBLAS%20MGL-PAX:SECTION "CUBLAS"
  [b315]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_d_1.htm "ARRAY-DIMENSIONS (MGL-PAX:CLHS FUNCTION)"
  [b6a7]: #MGL-MAT:@MAT-SHAPING-DESTRUCTIVE%20MGL-PAX:SECTION "Destructive Shaping"
  [bc8c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_exp_e.htm "EXP (MGL-PAX:CLHS FUNCTION)"
  [bd19]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dis.htm "ARRAY-DISPLACEMENT (MGL-PAX:CLHS FUNCTION)"
  [c442]: #MGL-MAT:USE-CUDA-P%20FUNCTION "MGL-MAT:USE-CUDA-P FUNCTION"
  [c44d]: cube-manual.md#MGL-CUBE:WITH-FACET-BARRIER%20MGL-PAX:MACRO "MGL-CUBE:WITH-FACET-BARRIER MGL-PAX:MACRO"
  [c4a3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "COS (MGL-PAX:CLHS FUNCTION)"
  [c4f8]: #MGL-MAT:MAT-MAX-SIZE%20%28MGL-PAX:READER%20MGL-MAT:MAT%29 "MGL-MAT:MAT-MAX-SIZE (MGL-PAX:READER MGL-MAT:MAT)"
  [c669]: #MGL-MAT:@MAT-RANDOM%20MGL-PAX:SECTION "Random numbers"
  [c96c]: #MGL-MAT:@MAT-INSTALLATION%20MGL-PAX:SECTION "Where to Get it?"
  [cb94]: #ARRAY%20MGL-CUBE:FACET-NAME "ARRAY MGL-CUBE:FACET-NAME"
  [cd9e]: #MGL-MAT:STACK%21%20FUNCTION "MGL-MAT:STACK! FUNCTION"
  [cdc3]: #MGL-MAT:FOREIGN-ARRAY%20CLASS "MGL-MAT:FOREIGN-ARRAY CLASS"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d3b8]: #MGL-MAT:@MAT-LISP-EXTENSIONS%20MGL-PAX:SECTION "Lisp Extensions"
  [d5f8]: #MGL-MAT:FOREIGN-ROOM%20FUNCTION "MGL-MAT:FOREIGN-ROOM FUNCTION"
  [db71]: #MGL-MAT:CUBLAS-ERROR%20CONDITION "MGL-MAT:CUBLAS-ERROR CONDITION"
  [dbac]: cube-manual.md#MGL-CUBE:DESTROY-CUBE%20FUNCTION "MGL-CUBE:DESTROY-CUBE FUNCTION"
  [ddd4]: cube-manual.md#MGL-CUBE:WITH-FACETS%20MGL-PAX:MACRO "MGL-CUBE:WITH-FACETS MGL-PAX:MACRO"
  [dddd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm "MAKE-INSTANCE (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [e17e]: #MGL-MAT:MAX-POOL%21%20FUNCTION "MGL-MAT:MAX-POOL! FUNCTION"
  [e1aa]: #MGL-MAT:LOGDET%20FUNCTION "MGL-MAT:LOGDET FUNCTION"
  [e22b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_aref.htm "AREF (MGL-PAX:CLHS FUNCTION)"
  [e3c3]: #MGL-MAT:MAKE-MAT%20FUNCTION "MGL-MAT:MAKE-MAT FUNCTION"
  [e678]: #MGL-MAT:@MAT-CACHING%20MGL-PAX:SECTION "Caching"
  [e798]: #MGL-MAT:@MAT-CTYPES%20MGL-PAX:SECTION "Element types"
  [e95b]: #MGL-MAT:GEMM%21%20FUNCTION "MGL-MAT:GEMM! FUNCTION"
  [eb95]: #MGL-MAT:MREF%20FUNCTION "MGL-MAT:MREF FUNCTION"
  [ece2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "SIN (MGL-PAX:CLHS FUNCTION)"
  [ecf9]: http://www.lispworks.com/documentation/HyperSpec/Body/e_storag.htm "STORAGE-CONDITION (MGL-PAX:CLHS CONDITION)"
  [f0db]: #MGL-MAT:WRITE-MAT%20GENERIC-FUNCTION "MGL-MAT:WRITE-MAT GENERIC-FUNCTION"
  [f5be]: #MGL-MAT:MAT-DISPLACEMENT%20%28MGL-PAX:READER%20MGL-MAT:MAT%29 "MGL-MAT:MAT-DISPLACEMENT (MGL-PAX:READER MGL-MAT:MAT)"
  [fae1]: #MGL-MAT:PINNING-SUPPORTED-P%20FUNCTION "MGL-MAT:PINNING-SUPPORTED-P FUNCTION"
  [fc7d]: #MGL-MAT:*SUPPORTED-CTYPES*%20VARIABLE "MGL-MAT:*SUPPORTED-CTYPES* VARIABLE"
  [fda1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "COSH (MGL-PAX:CLHS FUNCTION)"
  [ff55]: #MGL-MAT:@MAT-WHAT-IS-IT%20MGL-PAX:SECTION "What's MGL-MAT?"
  [ffae]: #MGL-MAT:CURAND-XORWOW-STATE%20CLASS "MGL-MAT:CURAND-XORWOW-STATE CLASS"
