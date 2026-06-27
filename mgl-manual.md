<a id="x-28MGL-3A-40MGL-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-MANUAL%20MGL-PAX:SECTION"></a>

# MGL Manual

## Table of Contents

- [1 Introduction][f7aa]
    - [1.1 Overview][9192]
    - [1.2 Links][00ee]
    - [1.3 Dependencies][e7ea]
    - [1.4 Code Organization][443c]
    - [1.5 Glossary][4a8e]
- [2 Common Stuff][e198]
- [3 Datasets][109e]
    - [3.1 Samplers][7bc3]
        - [3.1.1 Function Sampler][be8d]
- [4 Resampling][a39b]
    - [4.1 Shuffling][8611]
    - [4.2 Partitions][f790]
    - [4.3 Cross-validation][f17b]
    - [4.4 Bagging][b647]
    - [4.5 CV Bagging][3f9f]
    - [4.6 Miscellaneous Operations][59c2]
- [5 Core][f257]
    - [5.1 Persistence][29a1]
    - [5.2 Batch Processing][ff82]
    - [5.3 Executors][4476]
        - [5.3.1 Parameterized Executor Cache][ada2]
- [6 Monitoring][e668]
    - [6.1 Monitors][c701]
    - [6.2 Measurers][cd3b]
    - [6.3 Counters][be95]
        - [6.3.1 Attributes][6da5]
        - [6.3.2 Counter classes][7ee3]
- [7 Classification][60e3]
    - [7.1 Classification Monitors][c573]
    - [7.2 Classification Measurers][0ba7]
    - [7.3 Classification Counters][6598]
        - [7.3.1 Confusion Matrices][07c7]
- [8 Features][c8db]
    - [8.1 Feature Selection][1b5e]
    - [8.2 Feature Encoding][24aa]
- [9 Gradient Based Optimization][c74a]
    - [9.1 Iterative Optimizer][779d]
    - [9.2 Cost Function][e746]
    - [9.3 Gradient Descent][10e7]
        - [9.3.1 Batch Based Optimizers][2c39]
        - [9.3.2 Segmented GD Optimizer][989a]
        - [9.3.3 Per-weight Optimization][a884]
        - [9.3.4 Utilities][c40e]
    - [9.4 Conjugate Gradient][83e6]
    - [9.5 Extension API][6a6f]
        - [9.5.1 Implementing Optimizers][5748]
        - [9.5.2 Implementing Gradient Sources][c58b]
        - [9.5.3 Implementing Gradient Sinks][a210]
- [10 Differentiable Functions][2981]
- [11 Backpropagation Neural Networks][8788]
    - [11.1 Backprop Overview][56b2]
    - [11.2 Clump API][7a28]
    - [11.3 `bpn`s][d1e0]
        - [11.3.1 Training][0d82]
        - [11.3.2 Monitoring][4f0e]
        - [11.3.3 Feed-Forward Nets][1355]
        - [11.3.4 Recurrent Neural Nets][871e]
    - [11.4 Lumps][9641]
        - [11.4.1 Lump Base Class][3045]
        - [11.4.2 Inputs][207b]
        - [11.4.3 Weight Lump][6872]
        - [11.4.4 Activations][9105]
        - [11.4.5 Activation Functions][5d86]
        - [11.4.6 Losses][93a7]
        - [11.4.7 Stochasticity][aa2e]
        - [11.4.8 Arithmetic][2fe9]
        - [11.4.9 Operations for `rnn`s][51f7]
    - [11.5 Utilities][91f3]
- [12 Boltzmann Machines][332e]
- [13 Gaussian Processes][60b3]
- [14 Natural Language Processing][0d6a]
    - [14.1 Bag of Words][0784]
- [15 Logging][3f42]

###### \[in package MGL\]
<a id="x-28-22mgl-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"mgl"**

    - _Version:_ 0.1.0
    - _Description:_ `mgl` is a machine learning library for backpropagation
        neural networks, boltzmann machines, gaussian processes and more.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis <mega@retes.hu>
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ <http://melisgl.github.io/mgl>
    - _Bug tracker:_ <https://github.com/melisgl/mgl/issues>
    - _Source control:_ [GIT](https://github.com/melisgl/mgl.git)
    - *Depends on:* alexandria, array-operations, cl-reexport, closer-mop, lla, mgl-gnuplot, [mgl-mat][caca], [mgl-pax][6fdb], [named-readtables][718a], num-utils, pythonic-string-reader, swank(?)

<a id="x-28MGL-3A-40MGL-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-INTRODUCTION%20MGL-PAX:SECTION"></a>

## 1 Introduction

<a id="x-28MGL-3A-40MGL-OVERVIEW-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-OVERVIEW%20MGL-PAX:SECTION"></a>

### 1.1 Overview

MGL is a Common Lisp machine learning library by [Gábor
Melis](http://quotenil.com) with some parts originally contributed
by Ravenpack International. It mainly concentrates on various forms
of neural networks (boltzmann machines, feed-forward and recurrent
backprop nets). Most of MGL is built on top of
[MGL-MAT][f470] so it has BLAS and CUDA support.

In general, the focus is on power and performance not on ease of
use. Perhaps one day there will be a cookie cutter interface with
restricted functionality if a reasonable compromise is found between
power and utility.

<a id="x-28MGL-3A-40MGL-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-LINKS%20MGL-PAX:SECTION"></a>

### 1.2 Links

Here is the [official repository](https://github.com/melisgl/mgl)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-manual.html)
for the latest version.

<a id="x-28MGL-3A-40MGL-DEPENDENCIES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-DEPENDENCIES%20MGL-PAX:SECTION"></a>

### 1.3 Dependencies

MGL used to rely on [LLA](https://github.com/tpapp/lla) to
interface to BLAS and LAPACK. That's mostly history by now, but
configuration of foreign libraries is still done via LLA. See the
README in LLA on how to set things up. Note that these days OpenBLAS
is easier to set up and just as fast as ATLAS.

[CL-CUDA](https://github.com/takagi/cl-cuda) and
[MGL-MAT](https://github.com/melisgl/mgl) are the two main
dependencies and also the ones not yet in quicklisp, so just drop
them into `quicklisp/local-projects/`. If there is no suitable GPU
on the system or the CUDA SDK is not installed, MGL will simply
fall back on using BLAS and Lisp code. Wrapping code in
[`mgl-mat:with-cuda*`][3db3] is basically all that's needed to run on the GPU,
and with [`mgl-mat:cuda-available-p`][b057] one can check whether the GPU is
really being used.

<a id="x-28MGL-3A-40MGL-CODE-ORGANIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-CODE-ORGANIZATION%20MGL-PAX:SECTION"></a>

### 1.4 Code Organization

MGL consists of several packages dedicated to different tasks.
For example, package `mgl-resample` is about
[Resampling][a39b] and `mgl-gd` is about [Gradient Descent][10e7]
and so on. On one hand, having many packages makes it easier to
cleanly separate API and implementation and also to explore into a
specific task. At other times, they can be a hassle, so the `mgl`
package itself reexports every external symbol found in all the
other packages that make up MGL and MGL-MAT (see
[MAT Manual][f470]) on which it heavily relies.

One exception to this rule is the bundled, but independent
MGL-GNUPLOT library.

The built in tests can be run with:

    (ASDF:OOS 'ASDF:TEST-OP '#:MGL)

Note, that most of the tests are rather stochastic and can fail once
in a while.

<a id="x-28MGL-3A-40MGL-GLOSSARY-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-GLOSSARY%20MGL-PAX:SECTION"></a>

### 1.5 Glossary

Ultimately machine learning is about creating **models** of some
domain. The observations in the modelled domain are called
**instances** (also known as examples or samples). Sets of instances
are called **datasets**. Datasets are used when fitting a model or
when making **predictions**. Sometimes the word predictions is too
specific, and the results obtained from applying a model to some
instances are simply called **results**.

<a id="x-28MGL-COMMON-3A-40MGL-COMMON-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-COMMON:@MGL-COMMON%20MGL-PAX:SECTION"></a>

## 2 Common Stuff

###### \[in package MGL-COMMON\]
<a id="x-28MGL-COMMON-3ANAME-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:NAME%20GENERIC-FUNCTION"></a>

- [generic-function] **name** *object*

<a id="x-28MGL-COMMON-3ANAME-3D-20FUNCTION-29"></a>
<a id="MGL-COMMON:NAME%3D%20FUNCTION"></a>

- [function] **name=** *x y*

    Return `t` if X and Y are [`eql`][38a2] or if they are structured components whose
    elements are [`equal`][3fb5]. Strings and bit-vectors are `equal` if they are the same
    length and have identical components. Other arrays must be [`eq`][5a82] to be `equal`.

<a id="x-28MGL-COMMON-3ASIZE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:SIZE%20GENERIC-FUNCTION"></a>

- [generic-function] **size** *object*

<a id="x-28MGL-COMMON-3ANODES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:NODES%20GENERIC-FUNCTION"></a>

- [generic-function] **nodes** *object*

    Returns a [`mgl-mat:mat`][6d14] object representing the state
    or result of `object`. The first dimension of the returned matrix is
    equal to the number of stripes.

<a id="x-28MGL-COMMON-3ADEFAULT-VALUE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:DEFAULT-VALUE%20GENERIC-FUNCTION"></a>

- [generic-function] **default-value** *object*

<a id="x-28MGL-COMMON-3AGROUP-SIZE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:GROUP-SIZE%20GENERIC-FUNCTION"></a>

- [generic-function] **group-size** *object*

<a id="x-28MGL-COMMON-3ABATCH-SIZE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:BATCH-SIZE%20GENERIC-FUNCTION"></a>

- [generic-function] **batch-size** *object*

<a id="x-28MGL-COMMON-3AWEIGHTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:WEIGHTS%20GENERIC-FUNCTION"></a>

- [generic-function] **weights** *object*

<a id="x-28MGL-COMMON-3ASCALE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:SCALE%20GENERIC-FUNCTION"></a>

- [generic-function] **scale** *object*

<a id="x-28MGL-DATASET-3A-40MGL-DATASET-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-DATASET:@MGL-DATASET%20MGL-PAX:SECTION"></a>

## 3 Datasets

###### \[in package MGL-DATASET\]
An instance can often be any kind of object of the user's choice.
It is typically represented by a set of numbers which is called a
feature vector or by a structure holding the feature vector, the
label, etc. A dataset is a [`sequence`][ae23] of such instances or a
[Samplers][7bc3] object that produces instances.

<a id="x-28MGL-DATASET-3AMAP-DATASET-20FUNCTION-29"></a>
<a id="MGL-DATASET:MAP-DATASET%20FUNCTION"></a>

- [function] **map-dataset** *fn dataset*

    Call `fn` with each instance in `dataset`. This is basically equivalent
    to iterating over the elements of a sequence or a sampler (see
    [Samplers][7bc3]).

<a id="x-28MGL-DATASET-3AMAP-DATASETS-20FUNCTION-29"></a>
<a id="MGL-DATASET:MAP-DATASETS%20FUNCTION"></a>

- [function] **map-datasets** *fn datasets &key impute*

    Call `fn` with a list of instances, one from each dataset in
    `datasets`. Return nothing. If `impute` is specified then iterate until
    the largest dataset is consumed imputing `impute` for missing values.
    If `impute` is not specified then iterate until the smallest dataset
    runs out.
    
    ```common-lisp
    (map-datasets #'prin1 '((0 1 2) (:a :b)))
    .. (0 :A)(1 :B)
    
    (map-datasets #'prin1 '((0 1 2) (:a :b)) :impute nil)
    .. (0 :A)(1 :B)(2 NIL)
    ```
    
    It is of course allowed to mix sequences with samplers:
    
    ```common-lisp
    (map-datasets #'prin1
                  (list '(0 1 2)
                        (make-sequence-sampler '(:a :b) :max-n-samples 2)))
    .. (0 :A)(1 :B)
    ```

<a id="x-28MGL-DATASET-3A-40MGL-SAMPLER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-DATASET:@MGL-SAMPLER%20MGL-PAX:SECTION"></a>

### 3.1 Samplers

Some algorithms do not need random access to the entire dataset and
can work with a stream observations. Samplers are simple generators
providing two functions: [`sample`][f956] and [`finishedp`][401f].

<a id="x-28MGL-DATASET-3ASAMPLE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-DATASET:SAMPLE%20GENERIC-FUNCTION"></a>

- [generic-function] **sample** *sampler*

    If `sampler` has not run out of data (see [`finishedp`][401f])
    `sample` returns an object that represents a sample from the world to
    be experienced or, in other words, simply something the can be used
    as input for training or prediction. It is not allowed to call
    `sample` if `sampler` is `finishedp`.

<a id="x-28MGL-DATASET-3AFINISHEDP-20GENERIC-FUNCTION-29"></a>
<a id="MGL-DATASET:FINISHEDP%20GENERIC-FUNCTION"></a>

- [generic-function] **finishedp** *sampler*

    See if `sampler` has run out of examples.

<a id="x-28MGL-DATASET-3ALIST-SAMPLES-20FUNCTION-29"></a>
<a id="MGL-DATASET:LIST-SAMPLES%20FUNCTION"></a>

- [function] **list-samples** *sampler max-size*

    Return a list of samples of length at most `max-size` or less if
    `sampler` runs out.

<a id="x-28MGL-DATASET-3AMAKE-SEQUENCE-SAMPLER-20FUNCTION-29"></a>
<a id="MGL-DATASET:MAKE-SEQUENCE-SAMPLER%20FUNCTION"></a>

- [function] **make-sequence-sampler** *seq &key max-n-samples*

    Create a sampler that returns elements of `seq` in their original
    order. If `max-n-samples` is non-nil, then at most `max-n-samples` are
    sampled.

<a id="x-28MGL-DATASET-3AMAKE-RANDOM-SAMPLER-20FUNCTION-29"></a>
<a id="MGL-DATASET:MAKE-RANDOM-SAMPLER%20FUNCTION"></a>

- [function] **make-random-sampler** *seq &key max-n-samples (reorder \#'mgl-resample:shuffle)*

    Create a sampler that returns elements of `seq` in random order. If
    `max-n-samples` is non-nil, then at most `max-n-samples` are sampled.
    The first pass over a shuffled copy of `seq`, and this copy is
    reshuffled whenever the sampler reaches the end of it. Shuffling is
    performed by calling the `reorder` function.

<a id="x-28MGL-DATASET-3A-2AINFINITELY-EMPTY-DATASET-2A-20VARIABLE-29"></a>
<a id="MGL-DATASET:*INFINITELY-EMPTY-DATASET*%20VARIABLE"></a>

- [variable] **\*infinitely-empty-dataset\*** *\#\<function-sampler "infinitely empty" >*

    This is the default dataset for [`mgl-opt:minimize`][46a4]. It's an infinite
    stream of `nil`s.

<a id="x-28MGL-DATASET-3A-40MGL-SAMPLER-FUNCTION-SAMPLER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-DATASET:@MGL-SAMPLER-FUNCTION-SAMPLER%20MGL-PAX:SECTION"></a>

#### 3.1.1 Function Sampler

<a id="x-28MGL-DATASET-3AFUNCTION-SAMPLER-20CLASS-29"></a>
<a id="MGL-DATASET:FUNCTION-SAMPLER%20CLASS"></a>

- [class] **function-sampler**

    A sampler with a function in its [`generator`][08ac] that
    produces a stream of samples which may or may not be finite
    depending on [`max-n-samples`][1cab]. [`finishedp`][401f] returns `t` iff `max-n-samples` is
    non-nil, and it's not greater than the number of samples
    generated ([`n-samples`][bdf9]).
    
        (list-samples (make-instance 'function-sampler
                                     :generator (lambda ()
                                                  (random 10))
                                     :max-n-samples 5)
                      10)
        => (3 5 2 3 3)

<a id="x-28MGL-DATASET-3AGENERATOR-20-28MGL-PAX-3AREADER-20MGL-DATASET-3AFUNCTION-SAMPLER-29-29"></a>
<a id="MGL-DATASET:GENERATOR%20%28MGL-PAX:READER%20MGL-DATASET:FUNCTION-SAMPLER%29"></a>

- [reader] **generator** *[function-sampler][715c] (:generator)*

    A generator function of no arguments that returns
    the next sample.

<a id="x-28MGL-DATASET-3AMAX-N-SAMPLES-20-28MGL-PAX-3AACCESSOR-20MGL-DATASET-3AFUNCTION-SAMPLER-29-29"></a>
<a id="MGL-DATASET:MAX-N-SAMPLES%20%28MGL-PAX:ACCESSOR%20MGL-DATASET:FUNCTION-SAMPLER%29"></a>

- [accessor] **max-n-samples** *[function-sampler][715c] (:max-n-samples = nil)*

<a id="x-28MGL-COMMON-3ANAME-20-28MGL-PAX-3AREADER-20MGL-DATASET-3AFUNCTION-SAMPLER-29-29"></a>
<a id="MGL-COMMON:NAME%20%28MGL-PAX:READER%20MGL-DATASET:FUNCTION-SAMPLER%29"></a>

- [reader] **name** *[function-sampler][715c] (:name = nil)*

    An arbitrary object naming the sampler. Only used
    for printing the sampler object.

<a id="x-28MGL-DATASET-3AN-SAMPLES-20-28MGL-PAX-3AREADER-20MGL-DATASET-3AFUNCTION-SAMPLER-29-29"></a>
<a id="MGL-DATASET:N-SAMPLES%20%28MGL-PAX:READER%20MGL-DATASET:FUNCTION-SAMPLER%29"></a>

- [reader] **n-samples** *[function-sampler][715c] (:n-samples = 0)*

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE%20MGL-PAX:SECTION"></a>

## 4 Resampling

###### \[in package MGL-RESAMPLE\]
The focus of this package is on resampling methods such as
cross-validation and bagging which can be used for model evaluation,
model selection, and also as a simple form of ensembling. Data
partitioning and sampling functions are also provided because they
tend to be used together with resampling.

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-SHUFFLING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-SHUFFLING%20MGL-PAX:SECTION"></a>

### 4.1 Shuffling

<a id="x-28MGL-RESAMPLE-3ASHUFFLE-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SHUFFLE%20FUNCTION"></a>

- [function] **shuffle** *seq*

    Copy of `seq` and shuffle it using Fisher-Yates algorithm.

<a id="x-28MGL-RESAMPLE-3ASHUFFLE-21-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SHUFFLE%21%20FUNCTION"></a>

- [function] **shuffle!** *seq*

    Shuffle `seq` using Fisher-Yates algorithm.

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-PARTITIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-PARTITIONS%20MGL-PAX:SECTION"></a>

### 4.2 Partitions

The following functions partition a dataset (currently only
[`sequence`][ae23]s are supported) into a number of partitions. For each
element in the original dataset there is exactly one partition that
contains it.

<a id="x-28MGL-RESAMPLE-3AFRACTURE-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:FRACTURE%20FUNCTION"></a>

- [function] **fracture** *fractions seq &key weight*

    Partition `seq` into a number of subsequences. `fractions` is either a
    positive integer or a list of non-negative real numbers. `weight` is
    `nil` or a function that returns a non-negative real number when
    called with an element from `seq`. If `fractions` is a positive integer
    then return a list of that many subsequences with equal sum of
    weights bar rounding errors, else partition `seq` into subsequences,
    where the sum of weights of subsequence I is proportional to element
    I of `fractions`. If `weight` is `nil`, then it's element is assumed to
    have the same weight.
    
    To split into 5 sequences:
    
    ```common-lisp
    (fracture 5 '(0 1 2 3 4 5 6 7 8 9))
    => ((0 1) (2 3) (4 5) (6 7) (8 9))
    ```
    
    To split into two sequences whose lengths are proportional to 2 and
    3:
    
    ```common-lisp
    (fracture '(2 3) '(0 1 2 3 4 5 6 7 8 9))
    => ((0 1 2 3) (4 5 6 7 8 9))
    ```

<a id="x-28MGL-RESAMPLE-3ASTRATIFY-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:STRATIFY%20FUNCTION"></a>

- [function] **stratify** *seq &key (key \#'identity) (test \#'eql)*

    Return the list of strata of `seq`. `seq` is a sequence of elements for
    which the function `key` returns the class they belong to. Such
    classes are opaque objects compared for equality with `test`. A
    stratum is a sequence of elements with the same (under `test`) `key`.
    
    ```common-lisp
    (stratify '(0 1 2 3 4 5 6 7 8 9) :key #'evenp)
    => ((0 2 4 6 8) (1 3 5 7 9))
    ```

<a id="x-28MGL-RESAMPLE-3AFRACTURE-STRATIFIED-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:FRACTURE-STRATIFIED%20FUNCTION"></a>

- [function] **fracture-stratified** *fractions seq &key (key \#'identity) (test \#'eql) weight*

    Similar to [`fracture`][6f82], but also makes sure that keys are evenly
    distributed among the partitions (see [`stratify`][ba91]). It can be useful
    for classification tasks to partition the data set while keeping the
    distribution of classes the same.
    
    Note that the sets returned are not in random order. In fact, they
    are sorted internally by `key`.
    
    For example, to make two splits with approximately the same number
    of even and odd numbers:
    
    ```common-lisp
    (fracture-stratified 2 '(0 1 2 3 4 5 6 7 8 9) :key #'evenp)
    => ((0 2 1 3) (4 6 8 5 7 9))
    ```

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-CROSS-VALIDATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-CROSS-VALIDATION%20MGL-PAX:SECTION"></a>

### 4.3 Cross-validation

<a id="x-28MGL-RESAMPLE-3ACROSS-VALIDATE-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:CROSS-VALIDATE%20FUNCTION"></a>

- [function] **cross-validate** *data fn &key (n-folds 5) (folds (alexandria:iota n-folds)) (split-fn \#'split-fold/mod) pass-fold*

    Map `fn` over the `folds` of `data` split with `split-fn` and collect the
    results in a list. The simplest demonstration is:
    
    ```common-lisp
    (cross-validate '(0 1 2 3 4)
                    (lambda (test training)
                     (list test training))
                    :n-folds 5)
    => (((0) (1 2 3 4))
        ((1) (0 2 3 4))
        ((2) (0 1 3 4))
        ((3) (0 1 2 4))
        ((4) (0 1 2 3)))
    ```
    
    Of course, in practice one would typically train a model and return
    the trained model and/or its score on `test`. Also, sometimes one may
    want to do only some of the folds and remember which ones they were:
    
    ```common-lisp
    (cross-validate '(0 1 2 3 4)
                    (lambda (fold test training)
                     (list :fold fold test training))
                    :folds '(2 3)
                    :pass-fold t)
    => ((:fold 2 (2) (0 1 3 4))
        (:fold 3 (3) (0 1 2 4)))
    ```
    
    Finally, the way the data is split can be customized. By default
    [`split-fold/mod`][5ded] is called with the arguments `data`, the fold (from
    among `folds`) and `n-folds`. `split-fold/mod` returns two values which
    are then passed on to `fn`. One can use [`split-fold/cont`][5293] or
    [`split-stratified`][8cb8] or any other function that works with these
    arguments. The only real constraint is that `fn` has to take as many
    arguments (plus the fold argument if `pass-fold`) as `split-fn`
    returns.

<a id="x-28MGL-RESAMPLE-3ASPLIT-FOLD-2FMOD-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SPLIT-FOLD%2FMOD%20FUNCTION"></a>

- [function] **split-fold/mod** *seq fold n-folds*

    Partition `seq` into two sequences: one with elements of `seq` with
    indices whose remainder is `fold` when divided with `n-folds`, and a
    second one with the rest. The second one is the larger set. The
    order of elements remains stable. This function is suitable as the
    `split-fn` argument of [`cross-validate`][9524].

<a id="x-28MGL-RESAMPLE-3ASPLIT-FOLD-2FCONT-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SPLIT-FOLD%2FCONT%20FUNCTION"></a>

- [function] **split-fold/cont** *seq fold n-folds*

    Imagine dividing `seq` into `n-folds` subsequences of the same
    size (bar rounding). Return the subsequence of index `fold` as the
    first value and the all the other subsequences concatenated into one
    as the second value. The order of elements remains stable. This
    function is suitable as the `split-fn` argument of [`cross-validate`][9524].

<a id="x-28MGL-RESAMPLE-3ASPLIT-STRATIFIED-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SPLIT-STRATIFIED%20FUNCTION"></a>

- [function] **split-stratified** *seq fold n-folds &key (key \#'identity) (test \#'eql) weight*

    Split `seq` into `n-folds` partitions (as in [`fracture-stratified`][627a]).
    Return the partition of index `fold` as the first value, and the
    concatenation of the rest as the second value. This function is
    suitable as the `split-fn` argument of [`cross-validate`][9524] (mostly likely
    as a closure with `key`, `test`, `weight` bound).

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-BAGGING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-BAGGING%20MGL-PAX:SECTION"></a>

### 4.4 Bagging

<a id="x-28MGL-RESAMPLE-3ABAG-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:BAG%20FUNCTION"></a>

- [function] **bag** *seq fn &key (ratio 1) n weight (replacement t) key (test \#'eql) (random-state \*random-state\*)*

    Sample from `seq` with [`sample-from`][86fd] (passing `ratio`, `weight`,
    `replacement`), or [`sample-stratified`][aee6] if `key` is not `nil`. Call `fn` with
    the sample. If `n` is `nil` then keep repeating this until `fn` performs a
    non-local exit. Else `n` must be a non-negative integer, `n` iterations
    will be performed, the primary values returned by `fn` collected into
    a list and returned. See `sample-from` and `sample-stratified` for
    examples.

<a id="x-28MGL-RESAMPLE-3ASAMPLE-FROM-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SAMPLE-FROM%20FUNCTION"></a>

- [function] **sample-from** *ratio seq &key weight replacement (random-state \*random-state\*)*

    Return a sequence constructed by sampling with or without
    `replacement` from `seq`. The sum of weights in the result sequence will
    approximately be the sum of weights of `seq` times `ratio`. If `weight` is
    `nil` then elements are assumed to have equal weights, else `weight`
    should return a non-negative real number when called with an element
    of `seq`.
    
    To randomly select half of the elements:
    
    ```common-lisp
    (sample-from 1/2 '(0 1 2 3 4 5))
    => (5 3 2)
    ```
    
    To randomly select some elements such that the sum of their weights
    constitute about half of the sum of weights across the whole
    sequence:
    
    ```common-lisp
    (sample-from 1/2 '(0 1 2 3 4 5 6 7 8 9) :weight #'identity)
    => ;; sums to 28 that's near 45/2
       (9 4 1 6 8)
    ```
    
    To sample with replacement (that is, allowing the element to be
    sampled multiple times):
    
    ```common-lisp
    (sample-from 1 '(0 1 2 3 4 5) :replacement t)
    => (1 1 5 1 4 4)
    ```

<a id="x-28MGL-RESAMPLE-3ASAMPLE-STRATIFIED-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SAMPLE-STRATIFIED%20FUNCTION"></a>

- [function] **sample-stratified** *ratio seq &key weight replacement (key \#'identity) (test \#'eql) (random-state \*random-state\*)*

    Like [`sample-from`][86fd] but makes sure that the weighted proportion of
    classes in the result is approximately the same as the proportion in
    `seq`. See [`stratify`][ba91] for the description of `key` and `test`.

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-CV-BAGGING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-CV-BAGGING%20MGL-PAX:SECTION"></a>

### 4.5 CV Bagging

<a id="x-28MGL-RESAMPLE-3ABAG-CV-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:BAG-CV%20FUNCTION"></a>

- [function] **bag-cv** *data fn &key n (n-folds 5) (folds (alexandria:iota n-folds)) (split-fn \#'split-fold/mod) pass-fold (random-state \*random-state\*)*

    Perform cross-validation on different shuffles of `data` `n` times and
    collect the results. Since [`cross-validate`][9524] collects the return values
    of `fn`, the return value of this function is a list of lists of `fn`
    results. If `n` is `nil`, don't collect anything just keep doing
    repeated CVs until `fn` performs a non-local exit.
    
    The following example simply collects the test and training sets for
    2-fold CV repeated 3 times with shuffled data:
    
    ```commonlisp
    ;;; This is non-deterministic.
    (bag-cv '(0 1 2 3 4) #'list :n 3 :n-folds 2)
    => ((((2 3 4) (1 0))
         ((1 0) (2 3 4)))
        (((2 1 0) (4 3))
         ((4 3) (2 1 0)))
        (((1 0 3) (2 4))
         ((2 4) (1 0 3))))
    ```
    
    CV bagging is useful when a single CV is not producing stable
    results. As an ensemble method, CV bagging has the advantage over
    bagging that each example will occur the same number of times and
    after the first CV is complete there is a complete but less reliable
    estimate for each example which gets refined by further CVs.

<a id="x-28MGL-RESAMPLE-3A-40MGL-RESAMPLE-MISC-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-RESAMPLE:@MGL-RESAMPLE-MISC%20MGL-PAX:SECTION"></a>

### 4.6 Miscellaneous Operations

<a id="x-28MGL-RESAMPLE-3ASPREAD-STRATA-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:SPREAD-STRATA%20FUNCTION"></a>

- [function] **spread-strata** *seq &key (key \#'identity) (test \#'eql)*

    Return a sequence that's a reordering of `seq` such that elements
    belonging to different strata (under `key` and `test`, see [`stratify`][ba91]) are
    distributed evenly. The order of elements belonging to the same
    stratum is unchanged.
    
    For example, to make sure that even and odd numbers are distributed
    evenly:
    
    ```common-lisp
    (spread-strata '(0 2 4 6 8 1 3 5 7 9) :key #'evenp)
    => (0 1 2 3 4 5 6 7 8 9)
    ```
    
    Same thing with unbalanced classes:
    
    ```common-lisp
    (spread-strata (vector 0 2 3 5 6 1 4)
                   :key (lambda (x)
                          (if (member x '(1 4))
                              t
                              nil)))
    => #(0 1 2 3 4 5 6)
    ```

<a id="x-28MGL-RESAMPLE-3AZIP-EVENLY-20FUNCTION-29"></a>
<a id="MGL-RESAMPLE:ZIP-EVENLY%20FUNCTION"></a>

- [function] **zip-evenly** *seqs &key result-type*

    Make a single sequence out of the sequences in `seqs` so that in the
    returned sequence indices of elements belonging to the same source
    sequence are spread evenly across the whole range. The result is a
    list is `result-type` is [`list`][9271], it's a vector if `result-type` is [`vector`][64b3].
    If `result-type` is `nil`, then it's determined by the type of the first
    sequence in `seqs`.
    
    ```common-lisp
    (zip-evenly '((0 2 4) (1 3)))
    => (0 1 2 3 4)
    ```

<a id="x-28MGL-CORE-3A-40MGL-CORE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CORE%20MGL-PAX:SECTION"></a>

## 5 Core

###### \[in package MGL-CORE\]
<a id="x-28MGL-CORE-3A-40MGL-PERSISTENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-PERSISTENCE%20MGL-PAX:SECTION"></a>

### 5.1 Persistence

<a id="x-28MGL-CORE-3ALOAD-STATE-20FUNCTION-29"></a>
<a id="MGL-CORE:LOAD-STATE%20FUNCTION"></a>

- [function] **load-state** *filename object*

    Load weights of `object` from `filename`. Return `object`.

<a id="x-28MGL-CORE-3ASAVE-STATE-20FUNCTION-29"></a>
<a id="MGL-CORE:SAVE-STATE%20FUNCTION"></a>

- [function] **save-state** *filename object &key (if-exists :error) (ensure t)*

    Save weights of `object` to `filename`. If `ensure`, then
    [`ensure-directories-exist`][876d] is called on `filename`. `if-exists` is passed
    on to [`open`][6547]. Return `object`.

<a id="x-28MGL-CORE-3AREAD-STATE-20FUNCTION-29"></a>
<a id="MGL-CORE:READ-STATE%20FUNCTION"></a>

- [function] **read-state** *object stream*

    Read the weights of `object` from the bivalent `stream` where weights
    mean the learnt parameters. There is currently no sanity checking of
    data which will most certainly change in the future together with
    the serialization format. Return `object`.

<a id="x-28MGL-CORE-3AWRITE-STATE-20FUNCTION-29"></a>
<a id="MGL-CORE:WRITE-STATE%20FUNCTION"></a>

- [function] **write-state** *object stream*

    Write weight of `object` to the bivalent `stream`. Return `object`.

<a id="x-28MGL-CORE-3AREAD-STATE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:READ-STATE*%20GENERIC-FUNCTION"></a>

- [generic-function] **read-state\*** *object stream context*

    This is the extension point for [`read-state`][8148]. It is
    guaranteed that primary `read-state*` methods will be called only once
    for each `object` (under [`eq`][5a82]). `context` is an opaque object and must be
    passed on to any recursive `read-state*` calls.

<a id="x-28MGL-CORE-3AWRITE-STATE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:WRITE-STATE*%20GENERIC-FUNCTION"></a>

- [generic-function] **write-state\*** *object stream context*

    This is the extension point for [`write-state`][95fe]. It is
    guaranteed that primary `write-state*` methods will be called only
    once for each `object` (under [`eq`][5a82]). `context` is an opaque object and must
    be passed on to any recursive `write-state*` calls.

<a id="x-28MGL-CORE-3A-40MGL-MODEL-STRIPE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-MODEL-STRIPE%20MGL-PAX:SECTION"></a>

### 5.2 Batch Processing

Processing instances one by one during training or prediction can
be slow. The models that support batch processing for greater
efficiency are said to be *striped*.

Typically, during or after creating a model, one sets [`max-n-stripes`][16c4]
on it a positive integer. When a batch of instances is to be fed to
the model it is first broken into subbatches of length that's at
most `max-n-stripes`. For each subbatch, [`set-input`][0c9e] (FIXDOC) is called
and a before method takes care of setting [`n-stripes`][8dd7] to the actual
number of instances in the subbatch. When `max-n-stripes` is set
internal data structures may be resized which is an expensive
operation. Setting `n-stripes` is a comparatively cheap operation,
often implemented as matrix reshaping.

Note that for models made of different parts (for example,
[`mgl-bp:bpn`][5187] consists of [`mgl-bp:lump`][c1ac]s) , setting these
values affects the constituent parts, but one should never change
the number stripes of the parts directly because that would lead to
an internal inconsistency in the model.

<a id="x-28MGL-CORE-3AMAX-N-STRIPES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAX-N-STRIPES%20GENERIC-FUNCTION"></a>

- [generic-function] **max-n-stripes** *object*

    The number of stripes with which the `object` is
    capable of dealing simultaneously. 

<a id="x-28MGL-CORE-3ASET-MAX-N-STRIPES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:SET-MAX-N-STRIPES%20GENERIC-FUNCTION"></a>

- [generic-function] **set-max-n-stripes** *max-n-stripes object*

    Allocate the necessary stuff to allow for
    `max-n-stripes` number of stripes to be worked with simultaneously in
    `object`. This is called when `max-n-stripes` is [`setf`][a138]'ed.

<a id="x-28MGL-CORE-3AN-STRIPES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:N-STRIPES%20GENERIC-FUNCTION"></a>

- [generic-function] **n-stripes** *object*

    The number of stripes currently present in `object`.
    This is at most [`max-n-stripes`][16c4].

<a id="x-28MGL-CORE-3ASET-N-STRIPES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:SET-N-STRIPES%20GENERIC-FUNCTION"></a>

- [generic-function] **set-n-stripes** *n-stripes object*

    Set the number of stripes (out of [`max-n-stripes`][16c4])
    that are in use in `object`. This is called when `n-stripes` is
    [`setf`][a138]'ed.

<a id="x-28MGL-CORE-3AWITH-STRIPES-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CORE:WITH-STRIPES%20MGL-PAX:MACRO"></a>

- [macro] **with-stripes** *specs &body body*

    Bind start and optionally end indices belonging to stripes in
    striped objects.
    
        (WITH-STRIPES ((STRIPE1 OBJECT1 START1 END1)
                       (STRIPE2 OBJECT2 START2)
                       ...)
         ...)
    
    This is how one's supposed to find the index range corresponding to
    the Nth input in an input lump of a bpn:
    
         (with-stripes ((n input-lump start end))
           (loop for i upfrom start below end
                 do (setf (mref (nodes input-lump) i) 0d0)))
    
    Note how the input lump is striped, but the matrix into which we are
    indexing ([`nodes`][cc1c]) is not known to `with-stripes`. In fact, for lumps
    the same stripe indices work with `nodes` and [`mgl-bp:derivatives`][a81b].

<a id="x-28MGL-CORE-3ASTRIPE-START-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:STRIPE-START%20GENERIC-FUNCTION"></a>

- [generic-function] **stripe-start** *stripe object*

    Return the start index of `stripe` in some array or
    matrix of `object`.

<a id="x-28MGL-CORE-3ASTRIPE-END-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:STRIPE-END%20GENERIC-FUNCTION"></a>

- [generic-function] **stripe-end** *stripe object*

    Return the end index (exclusive) of `stripe` in some
    array or matrix of `object`.

<a id="x-28MGL-CORE-3ASET-INPUT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:SET-INPUT%20GENERIC-FUNCTION"></a>

- [generic-function] **set-input** *instances model*

    Set `instances` as inputs in `model`. `instances` is
    always a [`sequence`][ae23] of instances even for models not capable of batch
    operation. It sets [`n-stripes`][8dd7] to ([`length`][2f78] `instances`) in a `:before`
    method.

<a id="x-28MGL-CORE-3AMAP-BATCHES-FOR-MODEL-20FUNCTION-29"></a>
<a id="MGL-CORE:MAP-BATCHES-FOR-MODEL%20FUNCTION"></a>

- [function] **map-batches-for-model** *fn dataset model*

    Call `fn` with batches of instances from `dataset` suitable for `model`.
    The number of instances in a batch is [`max-n-stripes`][16c4] of `model` or less
    if there are no more instances left.

<a id="x-28MGL-CORE-3ADO-BATCHES-FOR-MODEL-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CORE:DO-BATCHES-FOR-MODEL%20MGL-PAX:MACRO"></a>

- [macro] **do-batches-for-model** *(batch (dataset model)) &body body*

    Convenience macro over [`map-batches-for-model`][5fdc].

<a id="x-28MGL-CORE-3A-40MGL-EXECUTORS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-EXECUTORS%20MGL-PAX:SECTION"></a>

### 5.3 Executors

<a id="x-28MGL-CORE-3AMAP-OVER-EXECUTORS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAP-OVER-EXECUTORS%20GENERIC-FUNCTION"></a>

- [generic-function] **map-over-executors** *fn instances prototype-executor*

    Divide `instances` between executors that perform the
    same function as `prototype-executor` and call `fn` with the instances
    and the executor for which the instances are.
    
    Some objects conflate function and call: the forward pass of a
    [`mgl-bp:bpn`][5187] computes output from inputs so it is like a
    function but it also doubles as a function call in the sense that
    the bpn (function) object changes state during the computation of
    the output. Hence not even the forward pass of a bpn is thread safe.
    There is also the restriction that all inputs must be of the same
    size.
    
    For example, if we have a function that builds bpn a for an input of
    a certain size, then we can create a factory that creates bpns for a
    particular call. The factory probably wants to keep the weights the
    same though. In [Parameterized Executor Cache][ada2],
    [`make-executor-with-parameters`][331b] is this factory.
    
    Parallelization of execution is another possibility
    `map-over-executors` allows, but there is no prebuilt solution for it,
    yet.
    
    The default implementation simply calls `fn` with `instances` and
    `prototype-executor`.

<a id="x-28MGL-CORE-3ADO-EXECUTORS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CORE:DO-EXECUTORS%20MGL-PAX:MACRO"></a>

- [macro] **do-executors** *(instances object) &body body*

    Convenience macro on top of [`map-over-executors`][b01b].

<a id="x-28MGL-CORE-3A-40MGL-PARAMETERIZED-EXECUTOR-CACHE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-PARAMETERIZED-EXECUTOR-CACHE%20MGL-PAX:SECTION"></a>

#### 5.3.1 Parameterized Executor Cache

<a id="x-28MGL-CORE-3APARAMETERIZED-EXECUTOR-CACHE-MIXIN-20CLASS-29"></a>
<a id="MGL-CORE:PARAMETERIZED-EXECUTOR-CACHE-MIXIN%20CLASS"></a>

- [class] **parameterized-executor-cache-mixin**

    Mix this into a model, implement
    [`instance-to-executor-parameters`][0078] and [`make-executor-with-parameters`][331b]
    and [`do-executors`][f98e] will be to able build executors suitable for
    different instances. The canonical example is using a BPN to compute
    the means and convariances of a gaussian process. Since each
    instance is made of a variable number of observations, the size of
    the input is not constant, thus we have a bpn (an executor) for each
    input dimension (the parameters).

<a id="x-28MGL-CORE-3AMAKE-EXECUTOR-WITH-PARAMETERS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-EXECUTOR-WITH-PARAMETERS%20GENERIC-FUNCTION"></a>

- [generic-function] **make-executor-with-parameters** *parameters cache*

    Create a new executor for `parameters`. `cache` is a
    [`parameterized-executor-cache-mixin`][d3b2]. In the BPN gaussian process
    example, `parameters` would be a list of input dimensions.

<a id="x-28MGL-CORE-3AINSTANCE-TO-EXECUTOR-PARAMETERS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:INSTANCE-TO-EXECUTOR-PARAMETERS%20GENERIC-FUNCTION"></a>

- [generic-function] **instance-to-executor-parameters** *instance cache*

    Return the parameters for an executor able to
    handle `instance`. Called by [`map-over-executors`][b01b] on `cache` (that's a
    [`parameterized-executor-cache-mixin`][d3b2]). The returned parameters are
    keys in an [`equal`][3fb5] parameters->executor hash table.

<a id="x-28MGL-CORE-3A-40MGL-MONITORING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-MONITORING%20MGL-PAX:SECTION"></a>

## 6 Monitoring

###### \[in package MGL-CORE\]
When training or applying a model, one often wants to track various
statistics. For example, in the case of training a neural network
with cross-entropy loss, these statistics could be the average
cross-entropy loss itself, classification accuracy, or even the
entire confusion matrix and sparsity levels in hidden layers. Also,
there is the question of what to do with the measured values (log
and forget, add to some counter or a list).

So there may be several phases of operation when we want to keep an
eye on. Let's call these **events**. There can also be many fairly
independent things to do in response to an event. Let's call these
**monitors**. Some monitors are a composition of two operations: one
that extracts some measurements and another that aggregates those
measurements. Let's call these two **measurers** and **counters**,
respectively.

For example, consider training a backpropagation neural network. We
want to look at the state of of network just after the backward
pass. [`mgl-bp:bp-learner`][00a0] has a [`monitors`][6202] event hook corresponding to the moment after
backpropagating the gradients. Suppose we are interested in how the
training cost evolves:

    (push (make-instance 'monitor
                         :measurer (lambda (instances bpn)
                                     (declare (ignore instances))
                                     (mgl-bp:cost bpn))
                         :counter (make-instance 'basic-counter))
          (monitors learner))

During training, this monitor will track the cost of training
examples behind the scenes. If we want to print and reset this
monitor periodically we can put another monitor on
[`mgl-opt:iterative-optimizer`][8da0]'s [`mgl-opt:on-n-instances-changed`][4f0b]
accessor:

    (push (lambda (optimizer gradient-source n-instances)
            (declare (ignore optimizer))
            (when (zerop (mod n-instances 1000))
              (format t "n-instances: ~S~%" n-instances)
              (dolist (monitor (monitors gradient-source))
                (when (counter monitor)
                  (format t "~A~%" (counter monitor))
                  (reset-counter (counter monitor)))))
          (mgl-opt:on-n-instances-changed optimizer))

Note that the monitor we push can be anything as long as
[`apply-monitor`][bbdf] is implemented on it with the appropriate signature.
Also note that the [`zerop`][ec8b] + [`mod`][e363] logic is fragile, so you will likely
want to use [`mgl-opt:monitor-optimization-periodically`][4528] instead of
doing the above.

So that's the general idea. Concrete events are documented where
they are signalled. Often there are task specific utilities that
create a reasonable set of default monitors (see
[Classification Monitors][c573]).

<a id="x-28MGL-CORE-3AAPPLY-MONITORS-20FUNCTION-29"></a>
<a id="MGL-CORE:APPLY-MONITORS%20FUNCTION"></a>

- [function] **apply-monitors** *monitors &rest arguments*

    Call [`apply-monitor`][bbdf] on each monitor in `monitors` and `arguments`. This
    is how an event is fired.

<a id="x-28MGL-CORE-3AAPPLY-MONITOR-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:APPLY-MONITOR%20GENERIC-FUNCTION"></a>

- [generic-function] **apply-monitor** *monitor &rest arguments*

    Apply `monitor` to `arguments`. This sound fairly
    generic, because it is. `monitor` can be anything, even a simple
    function or symbol, in which case this is just [`cl:apply`][d811]. See
    [Monitors][c701] for more.

<a id="x-28MGL-CORE-3ACOUNTER-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:COUNTER%20GENERIC-FUNCTION"></a>

- [generic-function] **counter** *monitor*

    Return an object representing the state of `monitor`
    or `nil`, if it doesn't have any (say because it's a simple logging
    function). Most monitors have counters into which they accumulate
    results until they are printed and reset. See [Counters][be95] for
    more.

<a id="x-28MGL-CORE-3AMONITOR-MODEL-RESULTS-20FUNCTION-29"></a>
<a id="MGL-CORE:MONITOR-MODEL-RESULTS%20FUNCTION"></a>

- [function] **monitor-model-results** *fn dataset model monitors*

    Call `fn` with batches of instances from `dataset` until it runs
    out (as in [`do-batches-for-model`][faaa]). `fn` is supposed to apply `model` to
    the batch and return some kind of result (for neural networks, the
    result is the model state itself). Apply `monitors` to each batch and
    the result returned by `fn` for that batch. Finally, return the list
    of counters of `monitors`.
    
    The purpose of this function is to collect various results and
    statistics (such as error measures) efficiently by applying the
    model only once, leaving extraction of quantities of interest from
    the model's results to `monitors`.
    
    See the model specific versions of this functions such as
    [`mgl-bp:monitor-bpn-results`][0933].

<a id="x-28MGL-CORE-3AMONITORS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MONITORS%20GENERIC-FUNCTION"></a>

- [generic-function] **monitors** *object*

    Return monitors associated with `object`. See various
    methods such as [`monitors`][6202] for more
    documentation.

<a id="x-28MGL-CORE-3A-40MGL-MONITOR-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-MONITOR%20MGL-PAX:SECTION"></a>

### 6.1 Monitors

<a id="x-28MGL-CORE-3AMONITOR-20CLASS-29"></a>
<a id="MGL-CORE:MONITOR%20CLASS"></a>

- [class] **monitor**

    A monitor that has another monitor called [`measurer`][eb05]
    embedded in it. When this monitor is applied, it applies the
    measurer and passes the returned values to [`add-to-counter`][62de] called on
    its [`counter`][a077] slot. One may further specialize [`apply-monitor`][bbdf] to change
    that.
    
    This class is useful when the same event monitor is applied
    repeatedly over a period and its results must be aggregated such as
    when training statistics are being tracked or when predictions are
    begin made. Note that the monitor must be compatible with the event
    it handles. That is, the embedded `measurer` must be prepared to take
    the arguments that are documented to come with the event.

<a id="x-28MGL-CORE-3AMEASURER-20-28MGL-PAX-3AREADER-20MGL-CORE-3AMONITOR-29-29"></a>
<a id="MGL-CORE:MEASURER%20%28MGL-PAX:READER%20MGL-CORE:MONITOR%29"></a>

- [reader] **measurer** *[monitor][7068] (:measurer)*

    This must be a monitor itself which only means
    that [`apply-monitor`][bbdf] is defined on it (but see [Monitoring][e668]). The
    returned values are aggregated by [`counter`][5752]. See
    [Measurers][cd3b] for a library of measurers.

<a id="x-28MGL-CORE-3ACOUNTER-20-28MGL-PAX-3AREADER-20MGL-CORE-3AMONITOR-29-29"></a>
<a id="MGL-CORE:COUNTER%20%28MGL-PAX:READER%20MGL-CORE:MONITOR%29"></a>

- [reader] **counter** *[monitor][7068] (:counter)*

    The `counter` of a monitor carries out the
    aggregation of results returned by [`measurer`][eb05]. The See [Counters][be95]
    for a library of counters.

<a id="x-28MGL-CORE-3A-40MGL-MEASURER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-MEASURER%20MGL-PAX:SECTION"></a>

### 6.2 Measurers

[`measurer`][eb05] is a part of [`monitor`][7068] objects, an embedded monitor that
computes a specific quantity (e.g. classification accuracy) from the
arguments of event it is applied to (e.g. the model results).
Measurers are often implemented by combining some kind of model
specific extractor with a generic measurer function.

All generic measurer functions return their results as multiple
values matching the arguments of [`add-to-counter`][62de] for a counter of a
certain type (see [Counters][be95]) so as to make them easily used in a
`monitor`:

    (multiple-value-call #'add-to-counter <some-counter>
                         <call-to-some-measurer>)

The counter class compatible with the measurer this way is noted for
each function.

For a list of measurer functions see [Classification Measurers][0ba7].

<a id="x-28MGL-CORE-3A-40MGL-COUNTER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-COUNTER%20MGL-PAX:SECTION"></a>

### 6.3 Counters

<a id="x-28MGL-CORE-3AADD-TO-COUNTER-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:ADD-TO-COUNTER%20GENERIC-FUNCTION"></a>

- [generic-function] **add-to-counter** *counter &rest args*

    Add `args` to `counter` in some way. See specialized
    methods for type specific documentation. The kind of arguments to be
    supported is the what the measurer functions (see [Measurers][cd3b])
    intended to be paired with the counter return as multiple values.

<a id="x-28MGL-CORE-3ACOUNTER-VALUES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:COUNTER-VALUES%20GENERIC-FUNCTION"></a>

- [generic-function] **counter-values** *counter*

    Return any number of values representing the state
    of `counter`. See specialized methods for type specific
    documentation.

<a id="x-28MGL-CORE-3ACOUNTER-RAW-VALUES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:COUNTER-RAW-VALUES%20GENERIC-FUNCTION"></a>

- [generic-function] **counter-raw-values** *counter*

    Return any number of values representing the state
    of `counter` in such a way that passing the returned values as
    arguments [`add-to-counter`][62de] on a fresh instance of the same type
    recreates the original state.

<a id="x-28MGL-CORE-3ARESET-COUNTER-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:RESET-COUNTER%20GENERIC-FUNCTION"></a>

- [generic-function] **reset-counter** *counter*

    Restore state of `counter` to what it was just after
    creation.

<a id="x-28MGL-CORE-3A-40MGL-ATTRIBUTES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-ATTRIBUTES%20MGL-PAX:SECTION"></a>

#### 6.3.1 Attributes

<a id="x-28MGL-CORE-3AATTRIBUTED-20CLASS-29"></a>
<a id="MGL-CORE:ATTRIBUTED%20CLASS"></a>

- [class] **attributed**

    This is a utility class that all counters subclass.
    The [`attributes`][cc37] plist can hold basically anything. Currently the
    attributes are only used when printing and they can be specified by
    the user. The monitor maker functions such as those in
    [Classification Monitors][c573] also add attributes of their own to the
    counters they create.
    
    With the `:prepend-attributes` initarg when can easily add new
    attributes without clobbering the those in the `:initform`, (`:type`
    "rmse") in this case.
    
        (princ (make-instance 'rmse-counter
                              :prepend-attributes '(:event "pred."
                                                    :dataset "test")))
        ;; pred. test rmse: 0.000e+0 (0)
        => #<RMSE-COUNTER pred. test rmse: 0.000e+0 (0)>

<a id="x-28MGL-CORE-3AATTRIBUTES-20-28MGL-PAX-3AACCESSOR-20MGL-CORE-3AATTRIBUTED-29-29"></a>
<a id="MGL-CORE:ATTRIBUTES%20%28MGL-PAX:ACCESSOR%20MGL-CORE:ATTRIBUTED%29"></a>

- [accessor] **attributes** *[attributed][9715] (:attributes = nil)*

    A plist of attribute keys and values.

<a id="x-28MGL-COMMON-3ANAME-20-28METHOD-20-28MGL-CORE-3AATTRIBUTED-29-29-29"></a>
<a id="MGL-COMMON:NAME%20%28METHOD%20%28MGL-CORE:ATTRIBUTED%29%29"></a>

- [method] **name** *(attributed attributed)*

    Return a string assembled from the values of the [`attributes`][cc37] of
    `attributed`. If there are multiple entries with the same key, then
    they are printed near together.
    
    Values may be padded according to an enclosing
    [`with-padded-attribute-printing`][2e8b].

<a id="x-28MGL-CORE-3AWITH-PADDED-ATTRIBUTE-PRINTING-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-CORE:WITH-PADDED-ATTRIBUTE-PRINTING%20MGL-PAX:MACRO"></a>

- [macro] **with-padded-attribute-printing** *(attributeds) &body body*

    Note the width of values for each attribute key which is the number
    of characters in the value's [`princ-to-string`][a541]'ed representation. In
    `body`, if attributes with they same key are printed they are forced
    to be at least this wide. This allows for nice, table-like output:
    
        (let ((attributeds
                (list (make-instance 'basic-counter
                                     :attributes '(:a 1 :b 23 :c 456))
                      (make-instance 'basic-counter
                                     :attributes '(:a 123 :b 45 :c 6)))))
          (with-padded-attribute-printing (attributeds)
            (map nil (lambda (attributed)
                       (format t "~A~%" attributed))
                 attributeds)))
        ;; 1   23 456: 0.000e+0 (0)
        ;; 123 45 6  : 0.000e+0 (0)

<a id="x-28MGL-CORE-3ALOG-PADDED-20FUNCTION-29"></a>
<a id="MGL-CORE:LOG-PADDED%20FUNCTION"></a>

- [function] **log-padded** *attributeds*

    Log (see [`log-msg`][f85e]) `attributeds` non-escaped (as in [`princ`][676d] or ~A) with
    the output being as table-like as possible.

<a id="x-28MGL-CORE-3A-40MGL-COUNTER-CLASSES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-COUNTER-CLASSES%20MGL-PAX:SECTION"></a>

#### 6.3.2 Counter classes

In addition to the really basic ones here, also see
[Classification Counters][6598].

<a id="x-28MGL-CORE-3ABASIC-COUNTER-20CLASS-29"></a>
<a id="MGL-CORE:BASIC-COUNTER%20CLASS"></a>

- [class] **basic-counter** *[attributed][9715]*

    A simple counter whose [`add-to-counter`][62de] takes two
    additional parameters: an increment to the internal sums of called
    the [`numerator`][8af5] and [`denominator`][5cd8]. [`counter-values`][20e8] returns two
    values:
    
    - `numerator` divided by `denominator` (or 0 if `denominator` is 0) and
    
    - `denominator`
    
    Here is an example the compute the mean of 5 things received in two
    batches:
    
         (let ((counter (make-instance 'basic-counter)))
           (add-to-counter counter 6.5 3)
           (add-to-counter counter 3.5 2)
           counter)
         => #<BASIC-COUNTER 2.00000e+0 (5)>

<a id="x-28MGL-CORE-3ARMSE-COUNTER-20CLASS-29"></a>
<a id="MGL-CORE:RMSE-COUNTER%20CLASS"></a>

- [class] **rmse-counter** *[basic-counter][5979]*

    A [`basic-counter`][5979] with whose nominator accumulates
    the square of some statistics. It has the attribute `:type` "rmse".
    [`counter-values`][20e8] returns the square root of what `basic-counter`'s
    `counter-values` would return.
    
        (let ((counter (make-instance 'rmse-counter)))
          (add-to-counter counter (+ (* 3 3) (* 4 4)) 2)
          counter)
        => #<RMSE-COUNTER rmse: 3.53553e+0 (2)>

<a id="x-28MGL-CORE-3ACONCAT-COUNTER-20CLASS-29"></a>
<a id="MGL-CORE:CONCAT-COUNTER%20CLASS"></a>

- [class] **concat-counter** *[attributed][9715]*

    A counter that simply concatenates
    sequences.
    
    ```common-lisp
    (let ((counter (make-instance 'concat-counter)))
      (add-to-counter counter '(1 2 3) #(4 5))
      (add-to-counter counter '(6 7))
      (counter-values counter))
    => (1 2 3 4 5 6 7)
    ```

<a id="x-28MGL-CORE-3ACONCATENATION-TYPE-20-28MGL-PAX-3AREADER-20MGL-CORE-3ACONCAT-COUNTER-29-29"></a>
<a id="MGL-CORE:CONCATENATION-TYPE%20%28MGL-PAX:READER%20MGL-CORE:CONCAT-COUNTER%29"></a>

- [reader] **concatenation-type** *[concat-counter][0f83] (:concatenation-type = 'list)*

    A type designator suitable as the RESULT-TYPE
    argument to [`concatenate`][2ecb].

<a id="x-28MGL-CORE-3A-40MGL-CLASSIFICATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CLASSIFICATION%20MGL-PAX:SECTION"></a>

## 7 Classification

###### \[in package MGL-CORE\]
To be able to measure classification related quantities, we need to
define what the label of an instance is. Customization is possible
by implementing a method for a specific type of instance, but these
functions only ever appear as defaults that can be overridden.

<a id="x-28MGL-CORE-3ALABEL-INDEX-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:LABEL-INDEX%20GENERIC-FUNCTION"></a>

- [generic-function] **label-index** *instance*

    Return the label of `instance` as a non-negative
    integer.

<a id="x-28MGL-CORE-3ALABEL-INDEX-DISTRIBUTION-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:LABEL-INDEX-DISTRIBUTION%20GENERIC-FUNCTION"></a>

- [generic-function] **label-index-distribution** *instance*

    Return a one dimensional array of probabilities
    representing the distribution of labels. The probability of the
    label with [`label-index`][cc80] `i` is element at index `i` of the returned
    arrray.

The following two functions are basically the same as the previous
two, but in batch mode: they return a sequence of label indices or
distributions. These are called on results produced by models.
Implement these for a model and the monitor maker functions below
will automatically work. See FIXDOC: for bpn and boltzmann.

<a id="x-28MGL-CORE-3ALABEL-INDICES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:LABEL-INDICES%20GENERIC-FUNCTION"></a>

- [generic-function] **label-indices** *results*

    Return a sequence of label indices for `results`
    produced by some model for a batch of instances. This is akin to
    [`label-index`][cc80].

<a id="x-28MGL-CORE-3ALABEL-INDEX-DISTRIBUTIONS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:LABEL-INDEX-DISTRIBUTIONS%20GENERIC-FUNCTION"></a>

- [generic-function] **label-index-distributions** *result*

    Return a sequence of label index distributions for
    `results` produced by some model for a batch of instances. This is
    akin to [`label-index-distribution`][caec].

<a id="x-28MGL-CORE-3A-40MGL-CLASSIFICATION-MONITOR-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CLASSIFICATION-MONITOR%20MGL-PAX:SECTION"></a>

### 7.1 Classification Monitors

The following functions return a list monitors. The monitors are
for events of signature (`instances` `model`) such as those produced by
[`monitor-model-results`][e50c] and its various model specific variations.
They are model-agnostic functions, extensible to new classifier
types. 

<a id="x-28MGL-CORE-3AMAKE-CLASSIFICATION-ACCURACY-MONITORS-20FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS%20FUNCTION"></a>

- [function] **make-classification-accuracy-monitors** *model &key operation-mode attributes (label-index-fn \#'label-index)*

    Return a list of [`monitor`][7068] objects associated with
    [`classification-accuracy-counter`][430d]s. `label-index-fn` is a function
    like [`label-index`][cc80]. See that function for more.
    
    Implemented in terms of [`make-classification-accuracy-monitors*`][2aa3].

<a id="x-28MGL-CORE-3AMAKE-CROSS-ENTROPY-MONITORS-20FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS%20FUNCTION"></a>

- [function] **make-cross-entropy-monitors** *model &key operation-mode attributes (label-index-distribution-fn \#'label-index-distribution)*

    Return a list of [`monitor`][7068] objects associated with
    [`cross-entropy-counter`][b186]s. `label-index-distribution-fn` is a
    function like [`label-index-distribution`][caec]. See that function for more.
    
    Implemented in terms of [`make-cross-entropy-monitors*`][e46f].

<a id="x-28MGL-CORE-3AMAKE-LABEL-MONITORS-20FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-LABEL-MONITORS%20FUNCTION"></a>

- [function] **make-label-monitors** *model &key operation-mode attributes (label-index-fn \#'label-index) (label-index-distribution-fn \#'label-index-distribution)*

    Return classification accuracy and cross-entropy monitors. See
    [`make-classification-accuracy-monitors`][911c] and
    [`make-cross-entropy-monitors`][6004] for a description of paramters.

The monitor makers above can be extended to support new classifier
types via the following generic functions.

<a id="x-28MGL-CORE-3AMAKE-CLASSIFICATION-ACCURACY-MONITORS-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS*%20GENERIC-FUNCTION"></a>

- [generic-function] **make-classification-accuracy-monitors\*** *model operation-mode label-index-fn attributes*

    Identical to [`make-classification-accuracy-monitors`][911c]
    bar the keywords arguments. Specialize this to add to support for
    new model types. The default implementation also allows for some
    extensibility: if [`label-indices`][31ed] is defined on `model`, then it will be
    used to extract label indices from model results.

<a id="x-28MGL-CORE-3AMAKE-CROSS-ENTROPY-MONITORS-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS*%20GENERIC-FUNCTION"></a>

- [generic-function] **make-cross-entropy-monitors\*** *model operation-mode label-index-distribution-fn attributes*

    Identical to [`make-cross-entropy-monitors`][6004] bar the
    keywords arguments. Specialize this to add to support for new model
    types. The default implementation also allows for some
    extensibility: if [`label-index-distributions`][9385] is defined on `model`,
    then it will be used to extract label distributions from model
    results.

<a id="x-28MGL-CORE-3A-40MGL-CLASSIFICATION-MEASURER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CLASSIFICATION-MEASURER%20MGL-PAX:SECTION"></a>

### 7.2 Classification Measurers

The functions here compare some known good solution (also known as
*ground truth* or *target*) to a prediction or approximation and
return some measure of their \[dis\]similarity. They are model
independent, hence one has to extract the ground truths and
predictions first. Rarely used directly, they are mostly hidden
behind [Classification Monitors][c573].

<a id="x-28MGL-CORE-3AMEASURE-CLASSIFICATION-ACCURACY-20FUNCTION-29"></a>
<a id="MGL-CORE:MEASURE-CLASSIFICATION-ACCURACY%20FUNCTION"></a>

- [function] **measure-classification-accuracy** *truths predictions &key (test \#'eql) truth-key prediction-key weight*

    Return the number of correct classifications and as the second
    value the number of instances (equal to length of `truths` in the
    non-weighted case). `truths` (keyed by `truth-key`) is a sequence of
    opaque class labels compared with `test` to another sequence of
    classes labels in `predictions` (keyed by `prediction-key`). If `weight`
    is non-nil, then it is a function that returns the weight of an
    element of `truths`. Weighted cases add their weight to both
    counts (returned as the first and second values) instead of 1 as in
    the non-weighted case.
    
    Note how the returned values are suitable for [`multiple-value-call`][e4dd]
    with #'[`add-to-counter`][62de] and a [`classification-accuracy-counter`][430d].

<a id="x-28MGL-CORE-3AMEASURE-CROSS-ENTROPY-20FUNCTION-29"></a>
<a id="MGL-CORE:MEASURE-CROSS-ENTROPY%20FUNCTION"></a>

- [function] **measure-cross-entropy** *truths predictions &key truth-key prediction-key (min-prediction-pr 1.0d-15)*

    Return the sum of the cross-entropy between pairs of elements with
    the same index of `truths` and `predictions`. `truth-key` is a function
    that's when applied to an element of `truths` returns a sequence
    representing some kind of discrete target distribution (P in the
    definition below). `truth-key` may be `nil` which is equivalent to the
    [`identity`][8ae0] function. `prediction-key` is the same kind of key for
    `predictions`, but the sequence it returns represents a distribution
    that approximates (Q below) the true one.
    
    Cross-entropy of the true and approximating distributions is defined
    as:
    
        cross-entropy(p,q) = - sum_i p(i) * log(q(i))
    
    of which this function returns the sum over the pairs of elements of
    `truths` and `predictions` keyed by `truth-key` and `prediction-key`.
    
    Due to the logarithm, if q(i) is close to zero, we run into
    numerical problems. To prevent this, all q(i) that are less than
    `min-prediction-pr` are treated as if they were `min-prediction-pr`.
    
    The second value returned is the sum of p(i) over all `truths` and all
    `i`. This is normally equal to `(length truths)`, since elements of
    `truths` represent a probability distribution, but this is not
    enforced which allows relative importance of elements to be
    controlled.
    
    The third value returned is a plist that maps each index occurring
    in the distribution sequences to a list of two elements:
    
         sum_j p_j(i) * log(q_j(i))
    
    and
    
        sum_j p_j(i)
    
    where `j` indexes into `truths` and `predictions`.
    
        (measure-cross-entropy '((0 1 0)) '((0.1 0.7 0.2)))
        => 0.35667497
           1
           (2 (0.0 0)
            1 (0.35667497 1)
            0 (0.0 0))
    
    Note how the returned values are suitable for [`multiple-value-call`][e4dd]
    with #'[`add-to-counter`][62de] and a [`cross-entropy-counter`][b186].

<a id="x-28MGL-CORE-3AMEASURE-ROC-AUC-20FUNCTION-29"></a>
<a id="MGL-CORE:MEASURE-ROC-AUC%20FUNCTION"></a>

- [function] **measure-roc-auc** *predictions pred &key (key \#'identity) weight*

    Return the area under the ROC curve for `predictions` representing
    predictions for a binary classification problem. `pred` is a predicate
    function for deciding whether a prediction belongs to the so called
    positive class. `key` returns a number for each element which is the
    predictor's idea of how much that element is likely to belong to the
    class, although it's not necessarily a probability.
    
    If `weight` is `nil`, then all elements of `predictions` count as 1
    towards the unnormalized sum within AUC. Else `weight` must be a
    function like `key`, but it should return the importance (a positive
    real number) of elements. If the weight of an prediction is 2 then
    it's as if there were another identical copy of that prediction in
    `predictions`.
    
    The algorithm is based on algorithm 2 in the paper 'An introduction
    to ROC analysis' by Tom Fawcett.
    
    ROC AUC is equal to the probability of a randomly chosen positive
    having higher `key` (score) than a randomly chosen negative element.
    With equal scores in mind, a more precise version is: AUC is the
    expectation of the above probability over all possible sequences
    sorted by scores.

<a id="x-28MGL-CORE-3AMEASURE-CONFUSION-20FUNCTION-29"></a>
<a id="MGL-CORE:MEASURE-CONFUSION%20FUNCTION"></a>

- [function] **measure-confusion** *truths predictions &key (test \#'eql) truth-key prediction-key weight*

    Create a [`confusion-matrix`][60d2] from `truths` and `predictions`.
    `truths` (keyed by `truth-key`) is a sequence of class labels compared
    with `test` to another sequence of class labels in `predictions` (keyed
    by `prediction-key`). If `weight` is non-nil, then it is a function that
    returns the weight of an element of `truths`. Weighted cases add their
    weight to both counts (returned as the first and second values).
    
    Note how the returned confusion matrix can be added to another with
    [`add-to-counter`][62de].

<a id="x-28MGL-CORE-3A-40MGL-CLASSIFICATION-COUNTER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CLASSIFICATION-COUNTER%20MGL-PAX:SECTION"></a>

### 7.3 Classification Counters

<a id="x-28MGL-CORE-3ACLASSIFICATION-ACCURACY-COUNTER-20CLASS-29"></a>
<a id="MGL-CORE:CLASSIFICATION-ACCURACY-COUNTER%20CLASS"></a>

- [class] **classification-accuracy-counter** *[basic-counter][5979]*

    A [`basic-counter`][5979] with "acc." as its `:type`
    attribute and a [`print-object`][3f2e] method that prints percentages.

<a id="x-28MGL-CORE-3ACROSS-ENTROPY-COUNTER-20CLASS-29"></a>
<a id="MGL-CORE:CROSS-ENTROPY-COUNTER%20CLASS"></a>

- [class] **cross-entropy-counter** *[basic-counter][5979]*

    A [`basic-counter`][5979] with "xent" as its `:type`
    attribute.

<a id="x-28MGL-CORE-3A-40MGL-CONFUSION-MATRIX-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-CONFUSION-MATRIX%20MGL-PAX:SECTION"></a>

#### 7.3.1 Confusion Matrices

<a id="x-28MGL-CORE-3ACONFUSION-MATRIX-20CLASS-29"></a>
<a id="MGL-CORE:CONFUSION-MATRIX%20CLASS"></a>

- [class] **confusion-matrix**

    A confusion matrix keeps count of classification
    results. The correct class is called `target' and the output of the
    classifier is called`prediction'.

<a id="x-28MGL-CORE-3AMAKE-CONFUSION-MATRIX-20FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-CONFUSION-MATRIX%20FUNCTION"></a>

- [function] **make-confusion-matrix** *&key (test \#'eql)*

    Classes are compared with `test`.

<a id="x-28MGL-CORE-3ASORT-CONFUSION-CLASSES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:SORT-CONFUSION-CLASSES%20GENERIC-FUNCTION"></a>

- [generic-function] **sort-confusion-classes** *matrix classes*

    Return a list of `classes` sorted for presentation
    purposes.

<a id="x-28MGL-CORE-3ACONFUSION-CLASS-NAME-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-CLASS-NAME%20GENERIC-FUNCTION"></a>

- [generic-function] **confusion-class-name** *matrix class*

    Name of `class` for presentation purposes.

<a id="x-28MGL-CORE-3ACONFUSION-COUNT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-COUNT%20GENERIC-FUNCTION"></a>

- [generic-function] **confusion-count** *matrix target prediction*

<a id="x-28MGL-CORE-3AMAP-CONFUSION-MATRIX-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:MAP-CONFUSION-MATRIX%20GENERIC-FUNCTION"></a>

- [generic-function] **map-confusion-matrix** *fn matrix*

    Call `fn` with `target`, `prediction`,
    [`count`][3155] paramaters for each cell in the confusion matrix. Cells with a
    zero count may be ommitted.

<a id="x-28MGL-CORE-3ACONFUSION-MATRIX-CLASSES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-MATRIX-CLASSES%20GENERIC-FUNCTION"></a>

- [generic-function] **confusion-matrix-classes** *matrix*

    A list of all classes. The default is to collect
    classes from the counts. This can be overridden if, for instance,
    some classes are not present in the results.

<a id="x-28MGL-CORE-3ACONFUSION-MATRIX-ACCURACY-20FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-MATRIX-ACCURACY%20FUNCTION"></a>

- [function] **confusion-matrix-accuracy** *matrix &key filter*

    Return the overall accuracy of the results in `matrix`. It's computed
    as the number of correctly classified cases (hits) divided by the
    name of cases. Return the number of hits and the number of cases as
    the second and third value. If `filter` function is given, then call
    it with the target and the prediction of the cell. Disregard cell
    for which `filter` returns `nil`.
    
    Precision and recall can be easily computed by giving the right
    filter, although those are provided in separate convenience
    functions.

<a id="x-28MGL-CORE-3ACONFUSION-MATRIX-PRECISION-20FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-MATRIX-PRECISION%20FUNCTION"></a>

- [function] **confusion-matrix-precision** *matrix prediction*

    Return the accuracy over the cases when the classifier said
    `prediction`.

<a id="x-28MGL-CORE-3ACONFUSION-MATRIX-RECALL-20FUNCTION-29"></a>
<a id="MGL-CORE:CONFUSION-MATRIX-RECALL%20FUNCTION"></a>

- [function] **confusion-matrix-recall** *matrix target*

    Return the accuracy over the cases when the correct class is
    `target`.

<a id="x-28MGL-CORE-3AADD-CONFUSION-MATRIX-20FUNCTION-29"></a>
<a id="MGL-CORE:ADD-CONFUSION-MATRIX%20FUNCTION"></a>

- [function] **add-confusion-matrix** *matrix result-matrix*

    Add `matrix` into `result-matrix`.

<a id="x-28MGL-CORE-3A-40MGL-FEATURES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-FEATURES%20MGL-PAX:SECTION"></a>

## 8 Features

###### \[in package MGL-CORE\]
<a id="x-28MGL-CORE-3A-40MGL-FEATURE-SELECTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-FEATURE-SELECTION%20MGL-PAX:SECTION"></a>

### 8.1 Feature Selection

The following *scoring functions* all return an [`equal`][3fb5] hash table
that maps features to scores.

<a id="x-28MGL-CORE-3ACOUNT-FEATURES-20FUNCTION-29"></a>
<a id="MGL-CORE:COUNT-FEATURES%20FUNCTION"></a>

- [function] **count-features** *documents mapper &key (key \#'identity)*

    Return scored features as an [`equal`][3fb5] hash table whose keys are
    features of `documents` and values are counts of occurrences of
    features. `mapper` takes a function and a document and calls function
    with features of the document.
    
    ```common-lisp
    (sort (alexandria:hash-table-alist
           (count-features '(("hello" "world")
                             ("this" "is" "our" "world"))
                           (lambda (fn document)
                             (map nil fn document))))
          #'string< :key #'car)
    => (("hello" . 1) ("is" . 1) ("our" . 1) ("this" . 1) ("world" . 2))
    ```

<a id="x-28MGL-CORE-3AFEATURE-LLRS-20FUNCTION-29"></a>
<a id="MGL-CORE:FEATURE-LLRS%20FUNCTION"></a>

- [function] **feature-llrs** *documents mapper class-fn &key (classes (all-document-classes documents class-fn))*

    Return scored features as an [`equal`][3fb5] hash table whose keys are
    features of `documents` and values are their log likelihood ratios.
    `mapper` takes a function and a document and calls function with
    features of the document.
    
    ```common-lisp
    (sort (alexandria:hash-table-alist
           (feature-llrs '((:a "hello" "world")
                           (:b "this" "is" "our" "world"))
                         (lambda (fn document)
                           (map nil fn (rest document)))
                         #'first))
          #'string< :key #'car)
    => (("hello" . 2.6032386) ("is" . 2.6032386) ("our" . 2.6032386)
        ("this" . 2.6032386) ("world" . 4.8428774e-8))
    ```

<a id="x-28MGL-CORE-3AFEATURE-DISAMBIGUITIES-20FUNCTION-29"></a>
<a id="MGL-CORE:FEATURE-DISAMBIGUITIES%20FUNCTION"></a>

- [function] **feature-disambiguities** *documents mapper class-fn &key (classes (all-document-classes documents class-fn))*

    Return scored features as an [`equal`][3fb5] hash table whose keys are
    features of `documents` and values are their *disambiguities*. `mapper`
    takes a function and a document and calls function with features of
    the document.
    
    From the paper 'Using Ambiguity Measure Feature Selection Algorithm
    for Support Vector Machine Classifier'.

<a id="x-28MGL-CORE-3A-40MGL-FEATURE-ENCODING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CORE:@MGL-FEATURE-ENCODING%20MGL-PAX:SECTION"></a>

### 8.2 Feature Encoding

Features can rarely be fed directly to algorithms as is, they need
to be transformed in some way. Suppose we have a simple language
model that takes a single word as input and predicts the next word.
However, both input and output is to be encoded as float vectors of
length 1000. What we do is find the top 1000 words by some
measure (see [Feature Selection][1b5e]) and associate these words with
the integers in \[0..999\] (this is [`encode`][fedd]ing). By using for
example [one-hot](http://en.wikipedia.org/wiki/One-hot) encoding, we
translate a word into a float vector when passing in the input. When
the model outputs the probability distribution of the next word, we
find the index of the max and find the word associated with it (this
is [`decode`][1339]ing)

<a id="x-28MGL-CORE-3AENCODE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:ENCODE%20GENERIC-FUNCTION"></a>

- [generic-function] **encode** *encoder decoded*

    Encode `decoded` with `encoder`. This interface is
    generic enough to be almost meaningless. See [`encoder/decoder`][1beb] for a
    simple, [`mgl-nlp:bag-of-words-encoder`][cbb4] for a slightly more involved
    example.
    
    If `encoder` is a function designator, then it's simply [`funcall`][03c7]ed
    with `decoded`.

<a id="x-28MGL-CORE-3ADECODE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CORE:DECODE%20GENERIC-FUNCTION"></a>

- [generic-function] **decode** *decoder encoded*

    Decode `encoded` with `encoder`. For an `decoder` /
    `encoder` pair, `(decode decoder (encode encoder object))` must be
    equal in some sense to `object`.
    
    If `decoder` is a function designator, then it's simply [`funcall`][03c7]ed
    with `encoded`.

<a id="x-28MGL-CORE-3AENCODER-2FDECODER-20CLASS-29"></a>
<a id="MGL-CORE:ENCODER%2FDECODER%20CLASS"></a>

- [class] **encoder/decoder**

    Implements O(1) [`encode`][fedd] and [`decode`][1339] by having an
    internal decoded-to-encoded and an encoded-to-decoded [`equal`][3fb5] hash
    table. `encoder/decoder` objects can be saved and loaded (see
    [Persistence][29a1]) as long as the elements in the hash tables have
    read/write consitency.
    
    ```common-lisp
    (let ((indexer
            (make-indexer
             (alexandria:alist-hash-table '(("I" . 3) ("me" . 2) ("mine" . 1)))
             2)))
      (values (encode indexer "I")
              (encode indexer "me")
              (encode indexer "mine")
              (decode indexer 0)
              (decode indexer 1)
              (decode indexer 2)))
    => 0
    => 1
    => NIL
    => "I"
    => "me"
    => NIL
    ```

<a id="x-28MGL-CORE-3AMAKE-INDEXER-20FUNCTION-29"></a>
<a id="MGL-CORE:MAKE-INDEXER%20FUNCTION"></a>

- [function] **make-indexer** *scored-features n &key (start 0) (class 'encoder/decoder)*

    Take the top `n` features from `scored-features` (see
    [Feature Selection][1b5e]), assign indices to them starting from `start`.
    Return an [`encoder/decoder`][1beb] (or another `class`) that converts between
    objects and indices.

Also see [Bag of Words][0784].

<a id="x-28MGL-OPT-3A-40MGL-OPT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT%20MGL-PAX:SECTION"></a>

## 9 Gradient Based Optimization

###### \[in package MGL-OPT\]
We have a real valued, differentiable function F and the task is to
find the parameters that minimize its value. Optimization starts
from a single point in the parameter space of F, and this single
point is updated iteratively based on the gradient and value of F at
or around the current point.

Note that while the stated problem is that of global optimization,
for non-convex functions, most algorithms will tend to converge to a
local optimum.

Currently, there are two optimization algorithms:
[Gradient Descent][10e7] (with several variants) and [Conjugate Gradient][83e6] both of
which are first order methods (they do not need second order
gradients) but more can be added with the [Extension API][6a6f].

<a id="x-28MGL-OPT-3AMINIMIZE-20FUNCTION-29"></a>
<a id="MGL-OPT:MINIMIZE%20FUNCTION"></a>

- [function] **minimize** *optimizer gradient-source &key (weights (list-segments gradient-source)) (dataset \*infinitely-empty-dataset\*)*

    Minimize the value of the real valued function represented by
    `gradient-source` by updating some of its parameters in `weights` (a [`mat`][6d14]
    or a sequence of `mat`s). Return `weights`. `dataset` (see
    [Datasets][109e]) is a set of unoptimized parameters of the
    same function. For example, `weights` may be the weights of a neural
    network while `dataset` is the training set consisting of inputs
    suitable for [`set-input`][0c9e]. The default
    `dataset`, ([`*infinitely-empty-dataset*`][ad8f]) is suitable for when all
    parameters are optimized, so there is nothing left to come from the
    environment.
    
    Optimization terminates if `dataset` is a sampler and it runs out or
    when some other condition met (see [`termination`][9006], for example). If
    `dataset` is a [`sequence`][ae23], then it is reused over and over again.
    
    Examples for various optimizers are provided in [Gradient Descent][10e7] and
    [Conjugate Gradient][83e6].

<a id="x-28MGL-OPT-3A-40MGL-OPT-ITERATIVE-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-ITERATIVE-OPTIMIZER%20MGL-PAX:SECTION"></a>

### 9.1 Iterative Optimizer

<a id="x-28MGL-OPT-3AITERATIVE-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-OPT:ITERATIVE-OPTIMIZER%20CLASS"></a>

- [class] **iterative-optimizer**

    An abstract base class of [Gradient Descent][10e7] and
    [Conjugate Gradient][83e6] based optimizers that iterate over instances until a
    termination condition is met.

<a id="x-28MGL-OPT-3AN-INSTANCES-20-28MGL-PAX-3AREADER-20MGL-OPT-3AITERATIVE-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:N-INSTANCES%20%28MGL-PAX:READER%20MGL-OPT:ITERATIVE-OPTIMIZER%29"></a>

- [reader] **n-instances** *[iterative-optimizer][8da0] (:n-instances = 0)*

    The number of instances this optimizer has seen so
    far. Incremented automatically during optimization.

<a id="x-28MGL-OPT-3ATERMINATION-20-28MGL-PAX-3AACCESSOR-20MGL-OPT-3AITERATIVE-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:TERMINATION%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29"></a>

- [accessor] **termination** *[iterative-optimizer][8da0] (:termination = nil)*

    If a number, it's the number of instances to train
    on in the sense of [`n-instances`][4c73]. If `n-instances` is equal or greater
    than this value optimization stops. If `termination` is `nil`, then
    optimization will continue. If it is `t`, then optimization will
    stop. If it is a function of no arguments, then its return value
    is processed as if it was returned by `termination`.

<a id="x-28MGL-OPT-3AON-OPTIMIZATION-STARTED-20-28MGL-PAX-3AACCESSOR-20MGL-OPT-3AITERATIVE-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:ON-OPTIMIZATION-STARTED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29"></a>

- [accessor] **on-optimization-started** *[iterative-optimizer][8da0] (:on-optimization-started = nil)*

    An event hook with parameters `(optimizer
    gradient-source n-instances)`. Called after initializations are
    performed (INITIALIZE-OPTIMIZER*, INITIALIZE-GRADIENT-SOURCE*) but
    before optimization is started.

<a id="x-28MGL-OPT-3AON-OPTIMIZATION-FINISHED-20-28MGL-PAX-3AACCESSOR-20MGL-OPT-3AITERATIVE-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:ON-OPTIMIZATION-FINISHED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29"></a>

- [accessor] **on-optimization-finished** *[iterative-optimizer][8da0] (:on-optimization-finished = nil)*

    An event hook with parameters `(optimizer
    gradient-source n-instances)`. Called when optimization has
    finished.

<a id="x-28MGL-OPT-3AON-N-INSTANCES-CHANGED-20-28MGL-PAX-3AACCESSOR-20MGL-OPT-3AITERATIVE-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:ON-N-INSTANCES-CHANGED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29"></a>

- [accessor] **on-n-instances-changed** *[iterative-optimizer][8da0] (:on-n-instances-changed = nil)*

    An event hook with parameters `(optimizer
    gradient-source n-instances)`. Called when optimization of a batch
    of instances is done and [`n-instances`][4c73] is incremented.

Now let's discuss a few handy utilities.

<a id="x-28MGL-OPT-3AMONITOR-OPTIMIZATION-PERIODICALLY-20FUNCTION-29"></a>
<a id="MGL-OPT:MONITOR-OPTIMIZATION-PERIODICALLY%20FUNCTION"></a>

- [function] **monitor-optimization-periodically** *optimizer periodic-fns*

    For each periodic function in the list of `periodic-fns`, add a
    monitor to `optimizer`'s [`on-optimization-started`][ebd4],
    [`on-optimization-finished`][0072] and [`on-n-instances-changed`][4f0b] hooks. The
    monitors are simple functions that just call each periodic function
    with the event parameters (`optimizer` `gradient-source` [`n-instances`][4c73]).
    Return `optimizer`.
    
    To log and reset the monitors of the gradient source after every
    1000 instances seen by `optimizer`:
    
        (monitor-optimization-periodically optimizer
                                           '((:fn log-my-test-error
                                              :period 2000)
                                             (:fn reset-optimization-monitors
                                              :period 1000
                                              :last-eval 0)))
    
    Note how we don't pass it's allowed to just pass the initargs for a
    `periodic-fn` instead of `periodic-fn` itself. The `:last-eval` 0 bit
    prevents [`reset-optimization-monitors`][ca09] from being called at the start
    of the optimization when the monitors are empty anyway.

<a id="x-28MGL-OPT-3ARESET-OPTIMIZATION-MONITORS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:RESET-OPTIMIZATION-MONITORS%20GENERIC-FUNCTION"></a>

- [generic-function] **reset-optimization-monitors** *optimizer gradient-source*

    Report the state of [`monitors`][8f37] of
    `optimizer` and `gradient-source` and reset their counters. See
    [`monitor-optimization-periodically`][4528] for an example of how this is
    used.

<a id="x-28MGL-OPT-3ARESET-OPTIMIZATION-MONITORS-20-28METHOD-20-28MGL-OPT-3AITERATIVE-OPTIMIZER-20T-29-29-29"></a>
<a id="MGL-OPT:RESET-OPTIMIZATION-MONITORS%20%28METHOD%20%28MGL-OPT:ITERATIVE-OPTIMIZER%20T%29%29"></a>

- [method] **reset-optimization-monitors** *(optimizer iterative-optimizer) gradient-source*

    Log the counters of the monitors of `optimizer` and `gradient-source`
    and reset them.

<a id="x-28MGL-OPT-3AREPORT-OPTIMIZATION-PARAMETERS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:REPORT-OPTIMIZATION-PARAMETERS%20GENERIC-FUNCTION"></a>

- [generic-function] **report-optimization-parameters** *optimizer gradient-source*

    A utility that's often called at the start of
    optimization (from [`on-optimization-started`][ebd4]). The default
    implementation logs the description of `gradient-source` (as in
    [`describe`][6651]) and `optimizer` and calls [`log-mat-room`][ea7d].

<a id="x-28MGL-OPT-3A-40MGL-OPT-COST-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-COST%20MGL-PAX:SECTION"></a>

### 9.2 Cost Function

The function being minimized is often called the *cost* or the
*loss* function.

<a id="x-28MGL-COMMON-3ACOST-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:COST%20GENERIC-FUNCTION"></a>

- [generic-function] **cost** *model*

    Return the value of the cost function being
    minimized. Calling this only makes sense in the context of an
    ongoing optimization (see [`minimize`][46a4]). The cost is that of a batch of
    instances.

<a id="x-28MGL-OPT-3AMAKE-COST-MONITORS-20FUNCTION-29"></a>
<a id="MGL-OPT:MAKE-COST-MONITORS%20FUNCTION"></a>

- [function] **make-cost-monitors** *model &key operation-mode attributes*

    Return a list of [`monitor`][7068] objects, each associated with one
    [`basic-counter`][5979] with attribute `:type` "cost". Implemented in terms of
    [`make-cost-monitors*`][3815].

<a id="x-28MGL-OPT-3AMAKE-COST-MONITORS-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:MAKE-COST-MONITORS*%20GENERIC-FUNCTION"></a>

- [generic-function] **make-cost-monitors\*** *model operation-mode attributes*

    Identical to [`make-cost-monitors`][46c2] bar the keywords
    arguments. Specialize this to add to support for new model types.

<a id="x-28MGL-GD-3A-40MGL-GD-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD%20MGL-PAX:SECTION"></a>

### 9.3 Gradient Descent

###### \[in package MGL-GD\]
Gradient descent is a first-order optimization algorithm. Relying
completely on first derivatives, it does not even evaluate the
function to be minimized. Let's see how to minimize a numerical lisp
function with respect to some of its parameters.

```commonlisp
(cl:defpackage :mgl-example-sgd
  (:use #:common-lisp #:mgl))

(in-package :mgl-example-sgd)

;;; Create an object representing the sine function.
(defparameter *diff-fn-1*
  (make-instance 'mgl-diffun:diffun
                 :fn #'sin
                 ;; We are going to optimize its only parameter.
                 :weight-indices '(0)))

;;; Minimize SIN. Note that there is no dataset involved because all
;;; parameters are being optimized.
(minimize (make-instance 'sgd-optimizer :termination 1000)
          *diff-fn-1*
          :weights (make-mat 1))
;;; => A MAT with a single value of about -pi/2.

;;; Create a differentiable function for f(x,y)=(x-y)^2. X is a
;;; parameter whose values come from the DATASET argument passed to
;;; MINIMIZE. Y is a parameter to be optimized (a 'weight').
(defparameter *diff-fn-2*
  (make-instance 'mgl-diffun:diffun
                 :fn (lambda (x y)
                       (expt (- x y) 2))
                 :parameter-indices '(0)
                 :weight-indices '(1)))

;;; Find the Y that minimizes the distance from the instances
;;; generated by the sampler.
(minimize (make-instance 'sgd-optimizer :batch-size 10)
          *diff-fn-2*
          :weights (make-mat 1)
          :dataset (make-instance 'function-sampler
                                  :generator (lambda ()
                                               (list (+ 10
                                                        (gaussian-random-1))))
                                  :max-n-samples 1000))
;;; => A MAT with a single value of about 10, the expected value of
;;; the instances in the dataset.

;;; The dataset can be a SEQUENCE in which case we'd better set
;;; TERMINATION else optimization would never finish.
(minimize (make-instance 'sgd-optimizer :termination 1000)
          *diff-fn-2*
          :weights (make-mat 1)
          :dataset '((0) (1) (2) (3) (4) (5)))
;;; => A MAT with a single value of about 2.5.
```

We are going to see a number of accessors for optimizer paramaters.
In general, it's allowed to [`setf`][a138] real slot accessors (as opposed to
readers and writers) at any time during optimization and so is
defining a method on an optimizer subclass that computes the value
in any way. For example, to decay the learning rate on a per
mini-batch basis:

```commonlisp
(defmethod learning-rate ((optimizer my-sgd-optimizer))
  (* (slot-value optimizer 'learning-rate)
     (expt 0.998
           (/ (n-instances optimizer) 60000))))
```


<a id="x-28MGL-GD-3A-40MGL-GD-BATCH-GD-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-BATCH-GD-OPTIMIZER%20MGL-PAX:SECTION"></a>

#### 9.3.1 Batch Based Optimizers

First let's see everything common to all batch based optimizers,
then discuss [SGD Optimizer][25fd], [Adam Optimizer][bd13] and
[Normalized Batch Optimizer][0c91]. All batch based optimizers
are [`iterative-optimizer`][8da0]s, so see
[Iterative Optimizer][779d] too.

<a id="x-28MGL-GD-3ABATCH-GD-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:BATCH-GD-OPTIMIZER%20CLASS"></a>

- [class] **batch-gd-optimizer** *[iterative-optimizer][8da0]*

    Another abstract base class for gradient based
    optimizers tath updates all weights simultaneously after chewing
    through [`batch-size`][fa6d] inputs. See subclasses [`sgd-optimizer`][2a2f],
    [`adam-optimizer`][e0e6] and [`normalized-batch-gd-optimizer`][f6ae].
    
    [`per-weight-batch-gd-optimizer`][5a43] may be a better choice when some
    weights can go unused for instance due to missing input values.

<a id="x-28MGL-COMMON-3ABATCH-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-COMMON:BATCH-SIZE%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **batch-size** *gd-optimizer (:batch-size = 1)*

    After having gone through `batch-size` number of
    inputs, weights are updated. With `batch-size` 1, one gets
    Stochastics Gradient Descent. With `batch-size` equal to the number
    of instances in the dataset, one gets standard, 'batch' gradient
    descent. With `batch-size` between these two extremes, one gets the
    most practical 'mini-batch' compromise.

<a id="x-28MGL-GD-3ALEARNING-RATE-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:LEARNING-RATE%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **learning-rate** *gd-optimizer (:learning-rate = 0.1)*

    This is the step size along the gradient. Decrease
    it if optimization diverges, increase it if it doesn't make
    progress.

<a id="x-28MGL-GD-3AMOMENTUM-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:MOMENTUM%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **momentum** *gd-optimizer (:momentum = 0)*

    A value in the \[0, 1) interval. `momentum` times the
    previous weight change is added to the gradient. 0 means no
    momentum.

<a id="x-28MGL-GD-3AMOMENTUM-TYPE-20-28MGL-PAX-3AREADER-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:MOMENTUM-TYPE%20%28MGL-PAX:READER%20MGL-GD::GD-OPTIMIZER%29"></a>

- [reader] **momentum-type** *gd-optimizer (:momentum-type = :normal)*

    One of `:normal`, `:nesterov` or `:none`. For pure
    optimization Nesterov's momentum may be better, but it may also
    increases chances of overfitting. Using `:none` is equivalent to 0
    momentum, but it also uses less memory. Note that with `:none`,
    [`momentum`][af05] is ignored even it it is non-zero.

<a id="x-28MGL-GD-3AWEIGHT-DECAY-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:WEIGHT-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **weight-decay** *gd-optimizer (:weight-decay = 0)*

    An L2 penalty. It discourages large weights, much
    like a zero mean gaussian prior. `weight-decay` \* WEIGHT is added to
    the gradient to penalize large weights. It's as if the function
    whose minimum is sought had `weight-decay`\*sum\_i{0.5 \* WEIGHT\_i^2}
    added to it.

<a id="x-28MGL-GD-3AWEIGHT-PENALTY-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:WEIGHT-PENALTY%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **weight-penalty** *gd-optimizer (:weight-penalty = 0)*

    An L1 penalty. It encourages sparsity.
    `sign`(WEIGHT) \* `weight-penalty` is added to the gradient pushing the
    weight towards negative infinity. It's as if the function whose
    minima is sought had `weight-penalty`\*sum\_i{abs(WEIGHT\_i)} added to
    it. Putting it on feature biases consitutes a sparsity constraint
    on the features.

<a id="x-28MGL-GD-3AUSE-SEGMENT-DERIVATIVES-P-20-28MGL-PAX-3AREADER-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:USE-SEGMENT-DERIVATIVES-P%20%28MGL-PAX:READER%20MGL-GD::GD-OPTIMIZER%29"></a>

- [reader] **use-segment-derivatives-p** *gd-optimizer (:use-segment-derivatives-p = nil)*

    Save memory if both the gradient source (the model
    being optimized) and the optimizer support this feature. It works
    like this: the accumulator into which the gradient source is asked
    to place the derivatives of a segment will be [`segment-derivatives`][9a5b]
    of the segment. This allows the optimizer not to allocate an
    accumulator matrix into which the derivatives are summed.

<a id="x-28MGL-GD-3AAFTER-UPDATE-HOOK-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3A-3AGD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:AFTER-UPDATE-HOOK%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29"></a>

- [accessor] **after-update-hook** *gd-optimizer (:after-update-hook = nil)*

    A list of functions with no arguments called after
    each weight update.

<a id="x-28MGL-GD-3ABEFORE-UPDATE-HOOK-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3ABATCH-GD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:BEFORE-UPDATE-HOOK%20%28MGL-PAX:ACCESSOR%20MGL-GD:BATCH-GD-OPTIMIZER%29"></a>

- [accessor] **before-update-hook** *[batch-gd-optimizer][d94e] (:before-update-hook = nil)*

    A list of functions of no parameters. Each
    function is called just before a weight update takes place (after
    accumulated gradients have been divided the length of the batch).
    Convenient to hang some additional gradient accumulating code
    on.

<a id="x-28MGL-GD-3A-40MGL-GD-SGD-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-SGD-OPTIMIZER%20MGL-PAX:SECTION"></a>

##### SGD Optimizer

<a id="x-28MGL-GD-3ASGD-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:SGD-OPTIMIZER%20CLASS"></a>

- [class] **sgd-optimizer** *[batch-gd-optimizer][d94e]*

    With [`batch-size`][fa6d] 1 this is Stochastic Gradient
    Descent. With higher batch sizes, one gets mini-batch and Batch
    Gradient Descent.
    
    Assuming that `accumulator` has the sum of gradients for a mini-batch,
    the weight update looks like this:
    
    $$
    \Delta_w^{t+1} = momentum * \Delta_w^t
      + \frac{accumulator}{batchsize}
      + l_2 w + l_1 sign(w)
    $$
    
    $$
    w^{t+1} = w^{t} - learningrate * \Delta_w,
    $$
    
    which is the same as the more traditional formulation:
    
    $$
    \Delta_w^{t+1} = momentum * \Delta_w^{t}
      + learningrate * \left(\frac{\frac{df}{dw}}{batchsize}
                           + l_2 w + l_1 sign(w)\right)
    $$
    
    $$
    w^{t+1} = w^{t} - \Delta_w,
    $$
    
    but the former works better when batch size, momentum or learning
    rate change during the course of optimization. The above is with
    normal momentum, Nesterov's momentum (see [`momentum-type`][5611]) momentum is
    also available.
    
    See [Batch Based Optimizers][2c39] for the description of the various
    options common to all batch based optimizers.

<a id="x-28MGL-GD-3A-40MGL-GD-ADAM-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-ADAM-OPTIMIZER%20MGL-PAX:SECTION"></a>

##### Adam Optimizer

<a id="x-28MGL-GD-3AADAM-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:ADAM-OPTIMIZER%20CLASS"></a>

- [class] **adam-optimizer** *[batch-gd-optimizer][d94e]*

    Adam is a first-order stochasistic gradient descent
    optimizer. It maintains an internal estimation for the mean and raw
    variance of each derivative as exponential moving averages. The step
    it takes is basically `M/(sqrt(V)+E)` where `m` is the estimated
    mean, `v` is the estimated variance, and `e` is a small adjustment
    factor to prevent the gradient from blowing up. See version 5 of the
    [paper](http://arxiv.org/abs/1412.6980) for more.
    
    Note that using momentum is not supported with Adam. In fact, an
    error is signalled if it's not `:none`.
    
    See [Batch Based Optimizers][2c39] for the description of the various
    options common to all batch based optimizers.

<a id="x-28MGL-GD-3ALEARNING-RATE-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3AADAM-OPTIMIZER-29-29"></a>
<a id="MGL-GD:LEARNING-RATE%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29"></a>

- [accessor] **learning-rate** *[adam-optimizer][e0e6] (= 2.0e-4)*

    Same thing as [`learning-rate`][09ed] but with the default suggested by the Adam paper.

<a id="x-28MGL-GD-3AMEAN-DECAY-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3AADAM-OPTIMIZER-29-29"></a>
<a id="MGL-GD:MEAN-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29"></a>

- [accessor] **mean-decay** *[adam-optimizer][e0e6] (:mean-decay = 0.9)*

    A number between 0 and 1 that determines how fast
    the estimated mean of derivatives is updated. 0 basically gives
    you RMSPROP (if [`variance-decay`][0900] is not too large) or AdaGrad (if
    `variance-decay` is close to 1 and the learning rate is annealed.
    This is $\beta_1$ in the paper.

<a id="x-28MGL-GD-3AMEAN-DECAY-DECAY-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3AADAM-OPTIMIZER-29-29"></a>
<a id="MGL-GD:MEAN-DECAY-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29"></a>

- [accessor] **mean-decay-decay** *[adam-optimizer][e0e6] (:mean-decay-decay = (- 1 1.0d-7))*

    A value that should be close to 1. [`mean-decay`][011d] is
    multiplied by this value after each update. This is $\lambda$ in
    the paper.

<a id="x-28MGL-GD-3AVARIANCE-DECAY-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3AADAM-OPTIMIZER-29-29"></a>
<a id="MGL-GD:VARIANCE-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29"></a>

- [accessor] **variance-decay** *[adam-optimizer][e0e6] (:variance-decay = 0.999)*

    A number between 0 and 1 that determines how fast
    the estimated variance of derivatives is updated. This is
    $\beta_2$ in the paper.

<a id="x-28MGL-GD-3AVARIANCE-ADJUSTMENT-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3AADAM-OPTIMIZER-29-29"></a>
<a id="MGL-GD:VARIANCE-ADJUSTMENT%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29"></a>

- [accessor] **variance-adjustment** *[adam-optimizer][e0e6] (:variance-adjustment = 1.0d-7)*

    Within the bowels of adam, the estimated mean is
    divided by the square root of the estimated variance (per weight)
    which can lead to numerical problems if the denominator is near
    zero. To avoid this, `variance-adjustment`, which should be a small
    positive number, is added to the denominator. This is `epsilon` in
    the paper.

<a id="x-28MGL-GD-3A-40MGL-GD-NORMALIZED-BATCH-GD-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-NORMALIZED-BATCH-GD-OPTIMIZER%20MGL-PAX:SECTION"></a>

##### Normalized Batch Optimizer

<a id="x-28MGL-GD-3ANORMALIZED-BATCH-GD-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:NORMALIZED-BATCH-GD-OPTIMIZER%20CLASS"></a>

- [class] **normalized-batch-gd-optimizer** *[batch-gd-optimizer][d94e]*

    Like [`batch-gd-optimizer`][d94e] but keeps count of how many
    times each weight was used in the batch and divides the accumulated
    gradient by this count instead of dividing by `n-instances-in-batch`.
    This only makes a difference if there are missing values in the
    learner that's being trained. The main feature that distuinguishes
    this class from [`per-weight-batch-gd-optimizer`][5a43] is that batches end at
    same time for all weights.

<a id="x-28MGL-GD-3AN-WEIGHT-USES-IN-BATCH-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3ANORMALIZED-BATCH-GD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:N-WEIGHT-USES-IN-BATCH%20%28MGL-PAX:ACCESSOR%20MGL-GD:NORMALIZED-BATCH-GD-OPTIMIZER%29"></a>

- [accessor] **n-weight-uses-in-batch** *[normalized-batch-gd-optimizer][f6ae]*

    Number of uses of the weight in its current batch.

<a id="x-28MGL-GD-3A-40MGL-GD-SEGMENTED-GD-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-SEGMENTED-GD-OPTIMIZER%20MGL-PAX:SECTION"></a>

#### 9.3.2 Segmented GD Optimizer

<a id="x-28MGL-GD-3ASEGMENTED-GD-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:SEGMENTED-GD-OPTIMIZER%20CLASS"></a>

- [class] **segmented-gd-optimizer** *[iterative-optimizer][8da0]*

    An optimizer that delegates training of segments to
    other optimizers. Useful to delegate training of different segments
    to different optimizers (capable of working with segmentables) or
    simply to not train all segments.

<a id="x-28MGL-GD-3ASEGMENTER-20-28MGL-PAX-3AREADER-20MGL-GD-3ASEGMENTED-GD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:SEGMENTER%20%28MGL-PAX:READER%20MGL-GD:SEGMENTED-GD-OPTIMIZER%29"></a>

- [reader] **segmenter** *[segmented-gd-optimizer][3ce0] (:segmenter)*

    When this optimizer is initialized it loops over
    the segment of the learner with [`map-segments`][2312]. `segmenter` is a
    function that is called with each segment and returns an optimizer
    or `nil`. Several segments may be mapped to the same optimizer.
    After the segment->optimizer mappings are collected, each
    optimizer is initialized by INITIALIZE-OPTIMIZER with the list of
    segments mapped to it.

<a id="x-28MGL-OPT-3ASEGMENTS-20-28MGL-PAX-3AREADER-20MGL-GD-3ASEGMENTED-GD-OPTIMIZER-29-29"></a>
<a id="MGL-OPT:SEGMENTS%20%28MGL-PAX:READER%20MGL-GD:SEGMENTED-GD-OPTIMIZER%29"></a>

- [reader] **segments** *[segmented-gd-optimizer][3ce0]*

[`segmented-gd-optimizer`][3ce0] inherits from [`iterative-optimizer`][8da0], so see
[Iterative Optimizer][779d] too.

<a id="x-28MGL-GD-3A-40MGL-GD-PER-WEIGHT-OPTIMIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-PER-WEIGHT-OPTIMIZATION%20MGL-PAX:SECTION"></a>

#### 9.3.3 Per-weight Optimization

<a id="x-28MGL-GD-3APER-WEIGHT-BATCH-GD-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-GD:PER-WEIGHT-BATCH-GD-OPTIMIZER%20CLASS"></a>

- [class] **per-weight-batch-gd-optimizer** *[iterative-optimizer][8da0]*

    This is much like [Batch Based Optimizers][2c39] but it
    is more clever about when to update weights. Basically every weight
    has its own batch independent from the batches of others. This has
    desirable properties. One can for example put two neural networks
    together without adding any connections between them and the
    learning will produce results equivalent to the separated case.
    Also, adding inputs with only missing values does not change
    anything.
    
    Due to its very non-batch nature, there is no CUDA implementation of
    this optimizer.

<a id="x-28MGL-GD-3AN-WEIGHT-USES-IN-BATCH-20-28MGL-PAX-3AACCESSOR-20MGL-GD-3APER-WEIGHT-BATCH-GD-OPTIMIZER-29-29"></a>
<a id="MGL-GD:N-WEIGHT-USES-IN-BATCH%20%28MGL-PAX:ACCESSOR%20MGL-GD:PER-WEIGHT-BATCH-GD-OPTIMIZER%29"></a>

- [accessor] **n-weight-uses-in-batch** *[per-weight-batch-gd-optimizer][5a43]*

    Number of uses of the weight in its current batch.

<a id="x-28MGL-GD-3A-40MGL-GD-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GD:@MGL-GD-UTILITIES%20MGL-PAX:SECTION"></a>

#### 9.3.4 Utilities

<a id="x-28MGL-GD-3ACLIP-L2-NORM-20FUNCTION-29"></a>
<a id="MGL-GD:CLIP-L2-NORM%20FUNCTION"></a>

- [function] **clip-l2-norm** *mats l2-upper-bound &key callback*

    Scale `mats` so that their $L_2$ norm does not exceed `l2-upper-bound`.
    
    Compute the norm of of `mats` as if they were a single vector. If the
    norm is greater than `l2-upper-bound`, then scale each matrix
    destructively by the norm divided by `l2-upper-bound` and if non-`nil`
    call the function `callback` with the scaling factor.

<a id="x-28MGL-GD-3AARRANGE-FOR-CLIPPING-GRADIENTS-20FUNCTION-29"></a>
<a id="MGL-GD:ARRANGE-FOR-CLIPPING-GRADIENTS%20FUNCTION"></a>

- [function] **arrange-for-clipping-gradients** *batch-gd-optimizer l2-upper-bound &key callback*

    Make it so that the norm of the batch normalized gradients
    accumulated by `batch-gd-optimizer` is clipped to `l2-upper-bound`
    before every update. See [`clip-l2-norm`][af6b].

<a id="x-28MGL-CG-3A-40MGL-CG-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-CG:@MGL-CG%20MGL-PAX:SECTION"></a>

### 9.4 Conjugate Gradient

###### \[in package MGL-CG\]
Conjugate gradient is a first-order optimization algorithm. It's
more advanced than gradient descent as it does line searches which
unfortunately also makes it unsuitable for non-deterministic
functions. Let's see how to minimize a numerical lisp function with
respect to some of its parameters.

```
;;; Create an object representing the sine function.
(defparameter *diff-fn-1*
  (make-instance 'mgl-diffun:diffun
                 :fn #'sin
                 ;; We are going to optimize its only parameter.
                 :weight-indices '(0)))

;;; Minimize SIN. Note that there is no dataset involved because all
;;; parameters are being optimized.
(minimize (make-instance 'cg-optimizer
                         :batch-size 1
                         :termination 1)
          *diff-fn-1*
          :weights (make-mat 1))
;;; => A MAT with a single value of about -pi/2.

;;; Create a differentiable function for f(x,y)=(x-y)^2. X is a
;;; parameter whose values come from the DATASET argument passed to
;;; MINIMIZE. Y is a parameter to be optimized (a 'weight').
(defparameter *diff-fn-2*
  (make-instance 'mgl-diffun:diffun
                 :fn (lambda (x y)
                       (expt (- x y) 2))
                 :parameter-indices '(0)
                 :weight-indices '(1)))

;;; Find the Y that minimizes the distance from the instances
;;; generated by the sampler.
(minimize (make-instance 'cg-optimizer :batch-size 10)
          *diff-fn-2*
          :weights (make-mat 1)
          :dataset (make-instance 'function-sampler
                                  :generator (lambda ()
                                               (list (+ 10
                                                        (gaussian-random-1))))
                                  :max-n-samples 1000))
;;; => A MAT with a single value of about 10, the expected value of
;;; the instances in the dataset.

;;; The dataset can be a SEQUENCE in which case we'd better set
;;; TERMINATION else optimization would never finish. Note how a
;;; single epoch suffices.
(minimize (make-instance 'cg-optimizer :termination 6)
          *diff-fn-2*
          :weights (make-mat 1)
          :dataset '((0) (1) (2) (3) (4) (5)))
;;; => A MAT with a single value of about 2.5.
```


<a id="x-28MGL-CG-3ACG-20FUNCTION-29"></a>
<a id="MGL-CG:CG%20FUNCTION"></a>

- [function] **cg** *fn w &key (max-n-line-searches \*default-max-n-line-searches\*) (max-n-evaluations-per-line-search \*default-max-n-evaluations-per-line-search\*) (max-n-evaluations \*default-max-n-evaluations\*) (sig \*default-sig\*) (rho \*default-rho\*) (int \*default-int\*) (ext \*default-ext\*) (ratio \*default-ratio\*) spare-vectors*

    [`cg-optimizer`][ee97] passes each batch of data to this function with its
    [`cg-args`][9749] passed on.
    
    Minimize a differentiable multivariate function with conjugate
    gradient. The Polak-Ribiere flavour of conjugate gradients is used
    to compute search directions, and a line search using quadratic and
    cubic polynomial approximations and the Wolfe-Powell stopping
    criteria is used together with the slope ratio method for guessing
    initial step sizes. Additionally a bunch of checks are made to make
    sure that exploration is taking place and that extrapolation will
    not be unboundedly large.
    
    `fn` is a function of two parameters: [`weights`][ab3c] and `derivatives`. `weights`
    is a [`mat`][6d14] of the same size as `w` that is where the search start from.
    `derivatives` is also a `mat` of that size and it is where `fn` shall
    place the partial derivatives. `fn` returns the value of the function
    that is being minimized.
    
    `cg` performs a number of line searches and invokes `fn` at each step. A
    line search invokes `fn` at most `max-n-evaluations-per-line-search`
    number of times and can succeed in improving the minimum by the
    sufficient margin or it can fail. Note, the even a failed line
    search may improve further and hence change the weights it's just
    that the improvement was deemed too small. `cg` stops when either:
    
    - two line searches fail in a row
    
    - `max-n-line-searches` is reached
    
    - `max-n-evaluations` is reached
    
    `cg` returns a `mat` that contains the best weights, the minimum, the
    number of line searches performed, the number of succesful line
    searches and the number of evaluations.
    
    When using `max-n-evaluations` remember that there is an extra
    evaluation of `fn` before the first line search.
    
    `spare-vectors` is a list of preallocated `mat`s of the same size as `w`.
    Passing 6 of them covers the current need of the algorithm and it
    will not cons up vectors of size `w` at all.
    
    [`note`][e2ae]: If the function terminates within a few iterations, it could
    be an indication that the function values and derivatives are not
    consistent (ie, there may be a bug in the implementation of `fn`
    function).
    
    `sig` and `rho` are the constants controlling the Wolfe-Powell
    conditions. `sig` is the maximum allowed absolute ratio between
    previous and new slopes (derivatives in the search direction), thus
    setting `sig` to low (positive) values forces higher precision in the
    line-searches. `rho` is the minimum allowed fraction of the
    expected (from the slope at the initial point in the linesearch).
    Constants must satisfy 0 \< `rho` \< `sig` \< 1. Tuning of `sig` (depending
    on the nature of the function to be optimized) may speed up the
    minimization; it is probably not worth playing much with `rho`.

<a id="x-28MGL-CG-3A-2ADEFAULT-INT-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-INT*%20VARIABLE"></a>

- [variable] **\*default-int\*** *0.1*

    Don't reevaluate within `int` of the limit of the current bracket.

<a id="x-28MGL-CG-3A-2ADEFAULT-EXT-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-EXT*%20VARIABLE"></a>

- [variable] **\*default-ext\*** *3*

    Extrapolate maximum `ext` times the current step-size.

<a id="x-28MGL-CG-3A-2ADEFAULT-SIG-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-SIG*%20VARIABLE"></a>

- [variable] **\*default-sig\*** *0.1*

    `sig` and `rho` are the constants controlling the Wolfe-Powell
    conditions. `sig` is the maximum allowed absolute ratio between
    previous and new slopes (derivatives in the search direction), thus
    setting `sig` to low (positive) values forces higher precision in the
    line-searches.

<a id="x-28MGL-CG-3A-2ADEFAULT-RHO-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-RHO*%20VARIABLE"></a>

- [variable] **\*default-rho\*** *0.05*

    `rho` is the minimum allowed fraction of the expected (from the slope
    at the initial point in the linesearch). Constants must satisfy 0 \<
    `rho` \< `sig` \< 1.

<a id="x-28MGL-CG-3A-2ADEFAULT-RATIO-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-RATIO*%20VARIABLE"></a>

- [variable] **\*default-ratio\*** *10*

    Maximum allowed slope ratio.

<a id="x-28MGL-CG-3A-2ADEFAULT-MAX-N-LINE-SEARCHES-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-MAX-N-LINE-SEARCHES*%20VARIABLE"></a>

- [variable] **\*default-max-n-line-searches\*** *nil*

<a id="x-28MGL-CG-3A-2ADEFAULT-MAX-N-EVALUATIONS-PER-LINE-SEARCH-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-MAX-N-EVALUATIONS-PER-LINE-SEARCH*%20VARIABLE"></a>

- [variable] **\*default-max-n-evaluations-per-line-search\*** *20*

<a id="x-28MGL-CG-3A-2ADEFAULT-MAX-N-EVALUATIONS-2A-20VARIABLE-29"></a>
<a id="MGL-CG:*DEFAULT-MAX-N-EVALUATIONS*%20VARIABLE"></a>

- [variable] **\*default-max-n-evaluations\*** *nil*

<a id="x-28MGL-CG-3ACG-OPTIMIZER-20CLASS-29"></a>
<a id="MGL-CG:CG-OPTIMIZER%20CLASS"></a>

- [class] **cg-optimizer** *[iterative-optimizer][8da0]*

    Updates all weights simultaneously after chewing
    through [`batch-size`][fa6d] inputs.

<a id="x-28MGL-COMMON-3ABATCH-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-CG-3ACG-OPTIMIZER-29-29"></a>
<a id="MGL-COMMON:BATCH-SIZE%20%28MGL-PAX:ACCESSOR%20MGL-CG:CG-OPTIMIZER%29"></a>

- [accessor] **batch-size** *[cg-optimizer][ee97] (:batch-size)*

    After having gone through `batch-size` number of
    instances, weights are updated. Normally, [`cg`][4ffb] operates on all
    available data, but it may be useful to introduce some noise into
    the optimization to reduce overfitting by using smaller batch
    sizes. If `batch-size` is not set, it is initialized to the size of
    the dataset at the start of optimization.

<a id="x-28MGL-CG-3ACG-ARGS-20-28MGL-PAX-3AACCESSOR-20MGL-CG-3ACG-OPTIMIZER-29-29"></a>
<a id="MGL-CG:CG-ARGS%20%28MGL-PAX:ACCESSOR%20MGL-CG:CG-OPTIMIZER%29"></a>

- [accessor] **cg-args** *[cg-optimizer][ee97] (:cg-args = 'nil)*

<a id="x-28MGL-CG-3AON-CG-BATCH-DONE-20-28MGL-PAX-3AACCESSOR-20MGL-CG-3ACG-OPTIMIZER-29-29"></a>
<a id="MGL-CG:ON-CG-BATCH-DONE%20%28MGL-PAX:ACCESSOR%20MGL-CG:CG-OPTIMIZER%29"></a>

- [accessor] **on-cg-batch-done** *[cg-optimizer][ee97] (:on-cg-batch-done = nil)*

    An event hook called when processing a conjugate
    gradient batch is done. The handlers on the hook are called with 8
    arguments:
    
        (optimizer gradient-source instances
         best-w best-f n-line-searches
         n-succesful-line-searches n-evaluations)
    
    The latter 5 of which are the return values of the [`cg`][4ffb] function.

<a id="x-28MGL-CG-3ALOG-CG-BATCH-DONE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-CG:LOG-CG-BATCH-DONE%20GENERIC-FUNCTION"></a>

- [generic-function] **log-cg-batch-done** *optimizer gradient-source instances best-w best-f n-line-searches n-succesful-line-searches n-evaluations*

    This is a function can be added to
    [`on-cg-batch-done`][d10a]. The default implementation simply logs the event
    arguments.

<a id="x-28MGL-CG-3ASEGMENT-FILTER-20-28MGL-PAX-3AREADER-20MGL-CG-3ACG-OPTIMIZER-29-29"></a>
<a id="MGL-CG:SEGMENT-FILTER%20%28MGL-PAX:READER%20MGL-CG:CG-OPTIMIZER%29"></a>

- [reader] **segment-filter** *[cg-optimizer][ee97] (:segment-filter = (constantly t))*

    A predicate function on segments that filters out
    uninteresting segments. Called from [`initialize-optimizer*`][7c2f].

<a id="x-28MGL-OPT-3A-40MGL-OPT-EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-EXTENSION-API%20MGL-PAX:SECTION"></a>

### 9.5 Extension API

<a id="x-28MGL-OPT-3A-40MGL-OPT-OPTIMIZER-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-OPTIMIZER%20MGL-PAX:SECTION"></a>

#### 9.5.1 Implementing Optimizers

The following generic functions must be specialized for new
optimizer types.

<a id="x-28MGL-OPT-3AMINIMIZE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:MINIMIZE*%20GENERIC-FUNCTION"></a>

- [generic-function] **minimize\*** *optimizer gradient-source weights dataset*

    Called by [`minimize`][46a4] after [`initialize-optimizer*`][7c2f] and
    [`initialize-gradient-source*`][dd95], this generic function is the main
    extension point for writing optimizers.

<a id="x-28MGL-OPT-3AINITIALIZE-OPTIMIZER-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:INITIALIZE-OPTIMIZER*%20GENERIC-FUNCTION"></a>

- [generic-function] **initialize-optimizer\*** *optimizer gradient-source weights dataset*

    Called automatically before training starts, this
    function sets up `optimizer` to be suitable for optimizing
    `gradient-source`. It typically creates appropriately sized
    accumulators for the gradients.

<a id="x-28MGL-OPT-3ASEGMENTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:SEGMENTS%20GENERIC-FUNCTION"></a>

- [generic-function] **segments** *optimizer*

    Several weight matrices known as *segments* can be
    optimized by a single optimizer. This function returns them as a
    list.

The rest are just useful for utilities for implementing
optimizers.

<a id="x-28MGL-OPT-3ATERMINATE-OPTIMIZATION-P-20FUNCTION-29"></a>
<a id="MGL-OPT:TERMINATE-OPTIMIZATION-P%20FUNCTION"></a>

- [function] **terminate-optimization-p** *n-instances termination*

    Utility function for subclasses of [`iterative-optimizer`][8da0]. It returns
    whether optimization is to be terminated based on `n-instances` and
    `termination` that are values of the respective accessors of
    `iterative-optimizer`.

<a id="x-28MGL-OPT-3ASET-N-INSTANCES-20FUNCTION-29"></a>
<a id="MGL-OPT:SET-N-INSTANCES%20FUNCTION"></a>

- [function] **set-n-instances** *optimizer gradient-source n-instances*

    Set [`n-instances`][4c73] of `optimizer` and
    fire [`on-n-instances-changed`][4f0b]. [`iterative-optimizer`][8da0] subclasses must
    call this to increment [`n-instances`][4c73].

<a id="x-28MGL-OPT-3ASEGMENT-SET-20CLASS-29"></a>
<a id="MGL-OPT:SEGMENT-SET%20CLASS"></a>

- [class] **segment-set**

    This is a utility class for optimizers that have a
    list of [`segments`][f00d] and (the weights being optimized) is able to copy
    back and forth between those segments and a single [`mat`][6d14] (the
    accumulator).

<a id="x-28MGL-OPT-3ASEGMENTS-20-28MGL-PAX-3AREADER-20MGL-OPT-3ASEGMENT-SET-29-29"></a>
<a id="MGL-OPT:SEGMENTS%20%28MGL-PAX:READER%20MGL-OPT:SEGMENT-SET%29"></a>

- [reader] **segments** *[segment-set][418a] (:segments)*

    A list of weight matrices.

<a id="x-28MGL-COMMON-3ASIZE-20-28MGL-PAX-3AREADER-20MGL-OPT-3ASEGMENT-SET-29-29"></a>
<a id="MGL-COMMON:SIZE%20%28MGL-PAX:READER%20MGL-OPT:SEGMENT-SET%29"></a>

- [reader] **size** *[segment-set][418a]*

    The sum of the sizes of the weight matrices of
    [`segments`][f00d].

<a id="x-28MGL-OPT-3ADO-SEGMENT-SET-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-OPT:DO-SEGMENT-SET%20MGL-PAX:MACRO"></a>

- [macro] **do-segment-set** *(segment &optional start) segment-set &body body*

    Iterate over [`segments`][f00d] in `segment-set`. If `start` is specified, the it
    is bound to the start index of `segment` within `segment-set`. The start
    index is the sum of the sizes of previous segments.

<a id="x-28MGL-OPT-3ASEGMENT-SET-3C-MAT-20FUNCTION-29"></a>
<a id="MGL-OPT:SEGMENT-SET%3C-MAT%20FUNCTION"></a>

- [function] **segment-set\<-mat** *segment-set mat*

    Copy the values of `mat` to the weight matrices of `segment-set` as if
    they were concatenated into a single `mat`.

<a id="x-28MGL-OPT-3ASEGMENT-SET--3EMAT-20FUNCTION-29"></a>
<a id="MGL-OPT:SEGMENT-SET-%3EMAT%20FUNCTION"></a>

- [function] **segment-set->mat** *segment-set mat*

    Copy the values of `segment-set` to `mat` as if they were concatenated
    into a single `mat`.

<a id="x-28MGL-OPT-3A-40MGL-OPT-GRADIENT-SOURCE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-GRADIENT-SOURCE%20MGL-PAX:SECTION"></a>

#### 9.5.2 Implementing Gradient Sources

Weights can be stored in a multitude of ways. Optimizers need to
update weights, so it is assumed that weights are stored in any
number of [`mat`][6d14] objects called segments.

The generic functions in this section must all be specialized for
new gradient sources except where noted.

<a id="x-28MGL-OPT-3AMAP-SEGMENTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:MAP-SEGMENTS%20GENERIC-FUNCTION"></a>

- [generic-function] **map-segments** *fn gradient-source*

    Apply `fn` to each segment of `gradient-source`.

<a id="x-28MGL-OPT-3AMAP-SEGMENT-RUNS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:MAP-SEGMENT-RUNS%20GENERIC-FUNCTION"></a>

- [generic-function] **map-segment-runs** *fn segment*

    Call `fn` with start and end of intervals of
    consecutive indices that are not missing in `segment`. Called by
    optimizers that support partial updates. The default implementation
    assumes that all weights are present. This only needs to be
    specialized if one plans to use an optimizer that knows how to deal
    unused/missing weights such as [`mgl-gd:normalized-batch-gd-optimizer`][f6ae]
    and `optimizer` [`mgl-gd:per-weight-batch-gd-optimizer`][5a43].

<a id="x-28MGL-OPT-3ASEGMENT-WEIGHTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:SEGMENT-WEIGHTS%20GENERIC-FUNCTION"></a>

- [generic-function] **segment-weights** *segment*

    Return the weight matrix of `segment`. A segment
    doesn't need to be a [`mat`][6d14] object itself. For example, it may be a
    `mgl-bm:chunk` of a `mgl-bm:bm` or a [`mgl-bp:lump`][c1ac] of a
    [`mgl-bp:bpn`][5187] whose [`nodes`][cc1c] slot holds the weights.

<a id="x-28MGL-OPT-3ASEGMENT-WEIGHTS-20-28METHOD-20-28MGL-MAT-3AMAT-29-29-29"></a>
<a id="MGL-OPT:SEGMENT-WEIGHTS%20%28METHOD%20%28MGL-MAT:MAT%29%29"></a>

- [method] **segment-weights** *(mat mat)*

    When the segment is really a `mat`, then just return it.

<a id="x-28MGL-OPT-3ASEGMENT-DERIVATIVES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:SEGMENT-DERIVATIVES%20GENERIC-FUNCTION"></a>

- [generic-function] **segment-derivatives** *segment*

    Return the derivatives matrix of `segment`. A segment
    doesn't need to be a [`mat`][6d14] object itself. For example, it may be a
    `mgl-bm:chunk` of a `mgl-bm:bm` or a [`mgl-bp:lump`][c1ac] of a
    [`mgl-bp:bpn`][5187] whose DERIVATIVES slot holds the gradient.

<a id="x-28MGL-OPT-3ALIST-SEGMENTS-20FUNCTION-29"></a>
<a id="MGL-OPT:LIST-SEGMENTS%20FUNCTION"></a>

- [function] **list-segments** *gradient-source*

    A utility function that returns the list of segments from
    [`map-segments`][2312] on `gradient-source`.

<a id="x-28MGL-OPT-3AINITIALIZE-GRADIENT-SOURCE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:INITIALIZE-GRADIENT-SOURCE*%20GENERIC-FUNCTION"></a>

- [generic-function] **initialize-gradient-source\*** *optimizer gradient-source weights dataset*

    Called automatically before [`minimize*`][ae3d] is called,
    this function may be specialized if `gradient-source` needs some kind
    of setup.

<a id="x-28MGL-OPT-3AINITIALIZE-GRADIENT-SOURCE-2A-20-28METHOD-20-28T-20T-20T-20T-29-29-29"></a>
<a id="MGL-OPT:INITIALIZE-GRADIENT-SOURCE*%20%28METHOD%20%28T%20T%20T%20T%29%29"></a>

- [method] **initialize-gradient-source\*** *optimizer gradient-source weights dataset*

    The default method does nothing.

<a id="x-28MGL-OPT-3AACCUMULATE-GRADIENTS-2A-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:ACCUMULATE-GRADIENTS*%20GENERIC-FUNCTION"></a>

- [generic-function] **accumulate-gradients\*** *gradient-source sink batch multiplier valuep*

    Add `multiplier` times the sum of first-order
    gradients to accumulators of `sink` (normally accessed with
    [`do-gradient-sink`][20ca]) and if `valuep`, return the sum of values of the
    function being optimized for a `batch` of instances. `gradient-source`
    is the object representing the function being optimized, `sink` is
    gradient sink.
    
    Note the number of instances in `batch` may be larger than what
    `gradient-source` process in one go (in the sense of say,
    [`max-n-stripes`][16c4]), so [`do-batches-for-model`][faaa] or something like (`group`
    `batch` `max-n-stripes`) can be handy.

<a id="x-28MGL-OPT-3A-40MGL-OPT-GRADIENT-SINK-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-OPT:@MGL-OPT-GRADIENT-SINK%20MGL-PAX:SECTION"></a>

#### 9.5.3 Implementing Gradient Sinks

Optimizers call [`accumulate-gradients*`][4bf1] on gradient sources. One
parameter of `accumulate-gradients*` is the `sink`. A gradient sink
knows what accumulator matrix (if any) belongs to a segment. Sinks
are defined entirely by [`map-gradient-sink`][aabd].

<a id="x-28MGL-OPT-3AMAP-GRADIENT-SINK-20GENERIC-FUNCTION-29"></a>
<a id="MGL-OPT:MAP-GRADIENT-SINK%20GENERIC-FUNCTION"></a>

- [generic-function] **map-gradient-sink** *fn sink*

    Call `fn` of lambda list (`segment` `accumulator`) on
    each segment and their corresponding accumulator [`mat`][6d14] in `sink`.

<a id="x-28MGL-OPT-3ADO-GRADIENT-SINK-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-OPT:DO-GRADIENT-SINK%20MGL-PAX:MACRO"></a>

- [macro] **do-gradient-sink** *((segment accumulator) sink) &body body*

    A convenience macro on top of [`map-gradient-sink`][aabd].

<a id="x-28MGL-DIFFUN-3A-40MGL-DIFFUN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-DIFFUN:@MGL-DIFFUN%20MGL-PAX:SECTION"></a>

## 10 Differentiable Functions

###### \[in package MGL-DIFFUN\]
<a id="x-28MGL-DIFFUN-3ADIFFUN-20CLASS-29"></a>
<a id="MGL-DIFFUN:DIFFUN%20CLASS"></a>

- [class] **diffun**

    `diffun` dresses a lisp function (in its [`fn`][f491] slot) as
    a gradient source (see [Implementing Gradient Sources][c58b]), which
    allows it to be used in [`minimize`][46a4]. See the examples in
    [Gradient Descent][10e7] and [Conjugate Gradient][83e6].

<a id="x-28MGL-COMMON-3AFN-20-28MGL-PAX-3AREADER-20MGL-DIFFUN-3ADIFFUN-29-29"></a>
<a id="MGL-COMMON:FN%20%28MGL-PAX:READER%20MGL-DIFFUN:DIFFUN%29"></a>

- [reader] **fn** *[diffun][1a61] (:fn)*

    A real valued lisp function. It may have any
    number of parameters.

<a id="x-28MGL-DIFFUN-3APARAMETER-INDICES-20-28MGL-PAX-3AREADER-20MGL-DIFFUN-3ADIFFUN-29-29"></a>
<a id="MGL-DIFFUN:PARAMETER-INDICES%20%28MGL-PAX:READER%20MGL-DIFFUN:DIFFUN%29"></a>

- [reader] **parameter-indices** *[diffun][1a61] (:parameter-indices = nil)*

    The list of indices of parameters that we don't
    optimize. Values for these will come from the DATASET argument of
    [`minimize`][46a4].

<a id="x-28MGL-DIFFUN-3AWEIGHT-INDICES-20-28MGL-PAX-3AREADER-20MGL-DIFFUN-3ADIFFUN-29-29"></a>
<a id="MGL-DIFFUN:WEIGHT-INDICES%20%28MGL-PAX:READER%20MGL-DIFFUN:DIFFUN%29"></a>

- [reader] **weight-indices** *[diffun][1a61] (:weight-indices = nil)*

    The list of indices of parameters to be optimized,
    the values of which will come from the `weights`
    argument of [`minimize`][46a4].

<a id="x-28MGL-BP-3A-40MGL-BP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP%20MGL-PAX:SECTION"></a>

## 11 Backpropagation Neural Networks

###### \[in package MGL-BP\]
<a id="x-28MGL-BP-3A-40MGL-BP-OVERVIEW-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-OVERVIEW%20MGL-PAX:SECTION"></a>

### 11.1 Backprop Overview

Backpropagation Neural Networks are just functions with lots of
parameters called *weights* and a layered structure when presented
as a [computational
graph](http://en.wikipedia.org/wiki/Automatic_differentiation). The
network is trained to [`minimize`][46a4] some kind of *loss function* whose
value the network computes.

In this implementation, a [`bpn`][5187] is assembled from several
[`lump`][c1ac]s (roughly corresponding to layers). Both feed-forward and
recurrent neural nets are supported ([`fnn`][9de4] and [`rnn`][b0f3], respectively).
`bpn`s can contain not only `lump`s but other `bpn`s, too. As we
see, networks are composite objects and the abstract base class for
composite and simple parts is called [`clump`][a4fe].

<a id="x-28MGL-BP-3ACLUMP-20CLASS-29"></a>
<a id="MGL-BP:CLUMP%20CLASS"></a>

- [class] **clump**

    A `clump` is a [`lump`][c1ac] or a [`bpn`][5187]. It represents
    a differentiable function. Arguments of clumps are given during
    instantiation. Some arguments are clumps themselves so they get
    permenantly wired together like this:
    
    ```commonlisp
    (->v*m (->input :size 10 :name 'input)
           (->weight :dimensions '(10 20) :name 'weight)
           :name 'activation)
    ```
    
    The above creates three clumps: the vector-matrix multiplication
    clumps called `activation` which has a reference to its operands:
    `input` and `weight`. Note that the example just defines a function, no
    actual computation has taken place, yet.
    
    This wiring of `clump`s is how one builds feed-forward nets ([`fnn`][9de4]) or
    recurrent neural networks ([`rnn`][b0f3]) that are `clump`s themselves so one
    can build nets in a hiearchical style if desired. Non-composite
    `clump`s are called `lump` (note the loss of `c` that stands for
    composite). The various `lump` subtypes correspond to different layer
    types ([`->sigmoid`][83f9], [`->dropout`][441b], [`->relu`][9d3a], [`->tanh`][5309], etc).

At this point, you may want to jump ahead to get a feel for how
things work by reading the [`fnn` Tutorial][6b38].

<a id="x-28MGL-BP-3A-40MGL-BP-EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-EXTENSION-API%20MGL-PAX:SECTION"></a>

### 11.2 Clump API

These are mostly for extension purposes. About the only thing
needed from here for normal operation is [`nodes`][cc1c] when clamping inputs
or extracting predictions.

<a id="x-28MGL-BP-3ASTRIPEDP-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:STRIPEDP%20GENERIC-FUNCTION"></a>

- [generic-function] **stripedp** *clump*

    For efficiency, forward and backprop phases do
    their stuff in batch mode: passing a number of instances through the
    network in batches. Thus clumps must be able to store values of and
    gradients for each of these instances. However, some clumps produce
    the same result for each instance in a batch. These clumps are the
    weights, the parameters of the network. `stripedp` returns true iff
    `clump` does not represent weights (i.e. it's not a [`->weight`][b76f]).
    
    For striped clumps, their [`nodes`][cc1c] and [`derivatives`][a81b] are [`mat`][6d14] objects with
    a leading dimension (number of rows in the 2d case) equal to the
    number of instances in the batch. Non-striped clumps have no
    restriction on their shape apart from what their usage dictates.

<a id="x-28MGL-COMMON-3ANODES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-COMMON:NODES%20GENERIC-FUNCTION"></a>

- [generic-function] **nodes** *object*

    Returns a [`mgl-mat:mat`][6d14] object representing the state
    or result of `object`. The first dimension of the returned matrix is
    equal to the number of stripes.

[`clump`][a4fe]s' [`nodes`][cc1c] holds the result computed by the most recent
[`forward`][c1ae]. For [`->input`][f54e] lumps, this is where input values shall be
placed (see [`set-input`][0c9e]). Currently, the matrix is always two
dimensional but this restriction may go away in the future.

<a id="x-28MGL-BP-3ADERIVATIVES-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:DERIVATIVES%20GENERIC-FUNCTION"></a>

- [generic-function] **derivatives** *clump*

    Return the [`mat`][6d14] object representing the partial
    derivatives of the function `clump` computes. The returned partial
    derivatives were accumulated by previous [`backward`][5bd4] calls.
    
    This matrix is shaped like the matrix returned by [`nodes`][cc1c].

<a id="x-28MGL-BP-3AFORWARD-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:FORWARD%20GENERIC-FUNCTION"></a>

- [generic-function] **forward** *clump*

    Compute the values of the function represented by
    `clump` for all stripes and place the results into [`nodes`][cc1c] of `clump`.

<a id="x-28MGL-BP-3ABACKWARD-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:BACKWARD%20GENERIC-FUNCTION"></a>

- [generic-function] **backward** *clump*

    Compute the partial derivatives of the function
    represented by `clump` and add them to [`derivatives`][a81b] of the
    corresponding argument clumps. The `derivatives` of `clump` contains the
    sum of partial derivatives of all clumps by the corresponding
    output. This function is intended to be called after a [`forward`][c1ae] pass.
    
    Take the [`->sigmoid`][83f9] clump for example when the network is being
    applied to a batch of two instances `x1` and `x2`. `x1` and `x2` are
    set in the [`->input`][f54e] lump X. The sigmoid computes `1/(1+exp(-x))`
    where `x` is its only argument clump.
    
        f(x) = 1/(1+exp(-x))
    
    When `backward` is called on the sigmoid lump, its `derivatives` is a
    2x1 [`mat`][6d14] object that contains the partial derivatives of the loss
    function:
    
        dL(x1)/df
        dL(x2)/df
    
    Now the `backward` method of the sigmoid needs to add `dL(x1)/dx1` and
    `dL(x2)/dx2` to `derivatives` of `x`. Now, `dL(x1)/dx1 = dL(x1)/df *
    df(x1)/dx1` and the first term is what we have in `derivatives` of the
    sigmoid so it only needs to calculate the second term.

In addition to the above, clumps also have to support [`size`][019f],
[`n-stripes`][8dd7], [`max-n-stripes`][16c4] (and the [`setf`][a138] methods of the latter two)
which can be accomplished just by inheriting from [`bpn`][5187], [`fnn`][9de4], [`rnn`][b0f3], or
a [`lump`][c1ac].

<a id="x-28MGL-BP-3A-40MGL-BPN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BPN%20MGL-PAX:SECTION"></a>

### 11.3 `bpn`s

<a id="x-28MGL-BP-3ABPN-20CLASS-29"></a>
<a id="MGL-BP:BPN%20CLASS"></a>

- [class] **bpn** *[clump][a4fe]*

    Abstract base class for [`fnn`][9de4] and [`rnn`][b0f3].

<a id="x-28MGL-CORE-3AN-STRIPES-20-28MGL-PAX-3AREADER-20MGL-BP-3ABPN-29-29"></a>
<a id="MGL-CORE:N-STRIPES%20%28MGL-PAX:READER%20MGL-BP:BPN%29"></a>

- [reader] **n-stripes** *[bpn][5187] (:n-stripes = 1)*

    The current number of instances the network has.
    This is automatically set to the number of instances passed to
    [`set-input`][0c9e], so it rarely has to be manipulated directly although it
    can be set. When set `n-stripes` of all [`clumps`][f7c1] get set to the same
    value.

<a id="x-28MGL-CORE-3AMAX-N-STRIPES-20-28MGL-PAX-3AREADER-20MGL-BP-3ABPN-29-29"></a>
<a id="MGL-CORE:MAX-N-STRIPES%20%28MGL-PAX:READER%20MGL-BP:BPN%29"></a>

- [reader] **max-n-stripes** *[bpn][5187] (:max-n-stripes = nil)*

    The maximum number of instances the network can
    operate on in parallel. Within [`build-fnn`][606c] or [`build-rnn`][764b], it defaults
    to `max-n-stripes` of that parent network, else it defaults to 1.
    When set `max-n-stripes` of all [`clumps`][f7c1] get set to the same value.

<a id="x-28MGL-BP-3ACLUMPS-20-28MGL-PAX-3AREADER-20MGL-BP-3ABPN-29-29"></a>
<a id="MGL-BP:CLUMPS%20%28MGL-PAX:READER%20MGL-BP:BPN%29"></a>

- [reader] **clumps** *[bpn][5187] (:clumps = (make-array 0 :element-type 'clump :adjustable t :fill-pointer t))*

    A topological sorted adjustable array with a fill
    pointer that holds the clumps that make up the network. Clumps are
    added to it by [`add-clump`][82d8] or, more often, automatically when within
    a [`build-fnn`][606c] or [`build-rnn`][764b]. Rarely needed, [`find-clump`][175f] takes care of
    most uses.

<a id="x-28MGL-BP-3AFIND-CLUMP-20FUNCTION-29"></a>
<a id="MGL-BP:FIND-CLUMP%20FUNCTION"></a>

- [function] **find-clump** *name bpn &key (errorp t)*

    Find the clump with `name` among [`clumps`][f7c1] of `bpn`. As always, names are
    compared with [`equal`][3fb5]. If not found, then return `nil` or signal and
    error depending on `errorp`.

<a id="x-28MGL-BP-3AADD-CLUMP-20FUNCTION-29"></a>
<a id="MGL-BP:ADD-CLUMP%20FUNCTION"></a>

- [function] **add-clump** *clump bpn*

    Add `clump` to `bpn`. [`max-n-stripes`][16c4] of `clump` gets set to that of `bpn`.
    It is an error to add a clump with a name already used by one of the
    [`clumps`][f7c1] of `bpn`.

<a id="x-28MGL-BP-3A-40MGL-BP-TRAINING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-TRAINING%20MGL-PAX:SECTION"></a>

#### 11.3.1 Training

[`bpn`][5187]s are trained to minimize the loss function they compute.
Before a `bpn` is passed to [`minimize`][46a4] (as its `gradient-source`
argument), it must be wrapped in a [`bp-learner`][00a0] object. `bp-learner` has
[`monitors`][6202] slot which is used for example by
[`reset-optimization-monitors`][d479].

Without the bells an whistles, the basic shape of training is this:

```commonlisp
(minimize optimizer (make-instance 'bp-learner :bpn bpn)
          :dataset dataset)
```


<a id="x-28MGL-BP-3ABP-LEARNER-20CLASS-29"></a>
<a id="MGL-BP:BP-LEARNER%20CLASS"></a>

- [class] **bp-learner**

<a id="x-28MGL-BP-3ABPN-20-28MGL-PAX-3AREADER-20MGL-BP-3ABP-LEARNER-29-29"></a>
<a id="MGL-BP:BPN%20%28MGL-PAX:READER%20MGL-BP:BP-LEARNER%29"></a>

- [reader] **bpn** *[bp-learner][00a0] (:bpn)*

    The `bpn` for which this [`bp-learner`][00a0] provides the
    gradients.

<a id="x-28MGL-CORE-3AMONITORS-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3ABP-LEARNER-29-29"></a>
<a id="MGL-CORE:MONITORS%20%28MGL-PAX:ACCESSOR%20MGL-BP:BP-LEARNER%29"></a>

- [accessor] **monitors** *[bp-learner][00a0] (:monitors = nil)*

    A list of [`monitor`][7068]s.

<a id="x-28MGL-BP-3A-40MGL-BP-MONITORING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-MONITORING%20MGL-PAX:SECTION"></a>

#### 11.3.2 Monitoring

<a id="x-28MGL-BP-3AMONITOR-BPN-RESULTS-20FUNCTION-29"></a>
<a id="MGL-BP:MONITOR-BPN-RESULTS%20FUNCTION"></a>

- [function] **monitor-bpn-results** *dataset bpn monitors*

    For every batch (of size [`max-n-stripes`][16c4] of `bpn`) of instances in
    `dataset`, set the batch as the next input with [`set-input`][0c9e], perform a
    [`forward`][c1ae] pass and apply `monitors` to the `bpn` (with [`apply-monitors`][989c]).
    Finally, return the counters of `monitors`. This is built on top of
    [`monitor-model-results`][e50c].

<a id="x-28MGL-BP-3AMAKE-STEP-MONITOR-MONITORS-20FUNCTION-29"></a>
<a id="MGL-BP:MAKE-STEP-MONITOR-MONITORS%20FUNCTION"></a>

- [function] **make-step-monitor-monitors** *rnn &key (counter-values-fn \#'counter-raw-values) (make-counter \#'make-step-monitor-monitor-counter)*

    Return a list of monitors, one for every monitor in [`step-monitors`][71f9]
    of `rnn`. These monitors extract the results from their warp
    counterpairs with `counter-values-fn` and add them to their own
    counter that's created by `make-counter`. Wow. Ew. The idea is that
    one does something like this do monitor warped prediction:
    
    ```commonlisp
    (let ((*warp-time* t))
      (setf (step-monitors rnn)
            (make-cost-monitors rnn :attributes '(:event "warped pred.")))
      (monitor-bpn-results dataset rnn
                           ;; Just collect and reset the warp
                           ;; monitors after each batch of
                           ;; instances.
                           (make-step-monitor-monitors rnn)))
    ```

<a id="x-28MGL-BP-3AMAKE-STEP-MONITOR-MONITOR-COUNTER-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:MAKE-STEP-MONITOR-MONITOR-COUNTER%20GENERIC-FUNCTION"></a>

- [generic-function] **make-step-monitor-monitor-counter** *step-counter*

    In an [`rnn`][b0f3], `step-counter` aggregates results of all
    the time steps during the processing of instances in the current
    batch. Return a new counter into which results from `step-counter` can
    be accumulated when the processing of the batch is finished. The
    default implementation creates a copy of `step-counter`.

<a id="x-28MGL-BP-3A-40MGL-FNN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-FNN%20MGL-PAX:SECTION"></a>

#### 11.3.3 Feed-Forward Nets

[`fnn`][9de4] and [`rnn`][b0f3] have a lot in common (see their common superclass, [`bpn`][5187]).
There is very limited functionality that's specific to `fnn`s so let's
get them out of they way before we study a full example.

<a id="x-28MGL-BP-3AFNN-20CLASS-29"></a>
<a id="MGL-BP:FNN%20CLASS"></a>

- [class] **fnn** *[bpn][5187]*

    A feed-forward neural net (as opposed to a
    recurrent one, see [`rnn`][b0f3]).

<a id="x-28MGL-BP-3ABUILD-FNN-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-BP:BUILD-FNN%20MGL-PAX:MACRO"></a>

- [macro] **build-fnn** *(&key fnn (class ''fnn) initargs max-n-stripes name) &body clumps*

    Syntactic sugar to assemble `fnn`s from [`clump`][a4fe]s. Like [`let*`][49f5], it is a
    sequence of bindings (of symbols to `clump`s). The names of the clumps
    created default to the symbol of the binding. In case a clump is not
    bound to a symbol (because it was created in a nested expression),
    the local function `clump` can be used to find the clump with the
    given name in the fnn being built. Example:
    
        (build-fnn ()
          (features (->input :size n-features))
          (biases (->weight :size n-features))
          (weights (->weight :size (* n-hiddens n-features)))
          (activations0 (->v*m :weights weights :x (clump 'features)))
          (activations (->+ :args (list biases activations0)))
          (output (->sigmoid :x activations)))

<a id="x-28MGL-BP-3A-40MGL-FNN-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-FNN-TUTORIAL%20MGL-PAX:SECTION"></a>

##### `fnn` Tutorial

Hopefully this example from `example/digit-fnn.lisp` illustrates
the concepts involved. If it's too dense despite the comments, then
read up on [Datasets][109e], [Gradient Based Optimization][c74a] and come back.

```commonlisp
(cl:defpackage :mgl-example-digit-fnn
  (:use #:common-lisp #:mgl))

(in-package :mgl-example-digit-fnn)

;;; There are 10 possible digits used as inputs ...
(defparameter *n-inputs* 10)
;;; and we want to learn the rule that maps the input digit D to (MOD
;;; (1+ D) 3).
(defparameter *n-outputs* 3)

;;; We define a feed-forward net to be able to specialize how inputs
;;; are translated by adding a SET-INPUT method later.
(defclass digit-fnn (fnn)
  ())

;;; Build a DIGIT-FNN with a single hidden layer of rectified linear
;;; units and a softmax output.
(defun make-digit-fnn (&key (n-hiddens 5))
  (build-fnn (:class 'digit-fnn)
    (input (->input :size *n-inputs*))
    (hidden-activation (->activation input :size n-hiddens))
    (hidden (->relu hidden-activation))
    (output-activation (->activation hidden :size *n-outputs*))
    (output (->softmax-xe-loss output-activation))))

;;; This method is called with batches of 'instances' (input digits in
;;; this case) by MINIMIZE and also by MONITOR-BPN-RESULTS before
;;; performing a forward pass (i.e. computing the value of the
;;; function represented by the network). Its job is to encode the
;;; inputs by populating rows of the NODES matrix of the INPUT clump.
;;;
;;; Each input is encoded as a row of zeros with a single 1 at index
;;; determined by the input digit. This is called one-hot encoding.
;;; The TARGET could be encoded the same way, but instead we use the
;;; sparse option supported by TARGET of ->SOFTMAX-XE-LOSS.
(defmethod set-input (digits (fnn digit-fnn))
  (let* ((input (nodes (find-clump 'input fnn)))
         (output-lump (find-clump 'output fnn)))
    (fill! 0 input)
    (loop for i upfrom 0
          for digit in digits
          do (setf (mref input i digit) 1))
    (setf (target output-lump)
          (mapcar (lambda (digit)
                    (mod (1+ digit) *n-outputs*))
                  digits))))

;;; Train the network by minimizing the loss (cross-entropy here) with
;;; stochastic gradient descent.
(defun train-digit-fnn ()
  (let ((optimizer
          ;; First create the optimizer for MINIMIZE.
          (make-instance 'segmented-gd-optimizer
                         :segmenter
                         ;; We train each weight lump with the same
                         ;; parameters and, in fact, the same
                         ;; optimizer. But it need not be so, in
                         ;; general.
                         (constantly
                          (make-instance 'sgd-optimizer
                                         :learning-rate 1
                                         :momentum 0.9
                                         :batch-size 100))))
        (fnn (make-digit-fnn)))
    ;; The number of instances the FNN can work with in parallel. It's
    ;; usually equal to the batch size or is a its divisor.
    (setf (max-n-stripes fnn) 50)
    ;; Initialize all weights randomly.
    (map-segments (lambda (weights)
                    (gaussian-random! (nodes weights) :stddev 0.01))
                  fnn)
    ;; Arrange for training and test error to be logged.
    (monitor-optimization-periodically
     optimizer '((:fn log-test-error :period 10000)
                 (:fn reset-optimization-monitors :period 1000)))
    ;; Finally, start the optimization.
    (minimize optimizer
              ;; Dress FNN in a BP-LEARNER and attach monitors for the
              ;; cost to it. These monitors are going to be logged and
              ;; reset after every 100 training instance by
              ;; RESET-OPTIMIZATION-MONITORS above.
              (make-instance 'bp-learner
                             :bpn fnn
                             :monitors (make-cost-monitors
                                        fnn :attributes `(:event "train")))
              ;; Training stops when the sampler runs out (after 10000
              ;; instances).
              :dataset (make-sampler 10000))))

;;; Return a sampler object that produces MAX-N-SAMPLES number of
;;; random inputs (numbers between 0 and 9).
(defun make-sampler (max-n-samples)
  (make-instance 'function-sampler :max-n-samples max-n-samples
                 :generator (lambda () (random *n-inputs*))))

;;; Log the test error. Also, describe the optimizer and the bpn at
;;; the beginning of training. Called periodically during training
;;; (see above).
(defun log-test-error (optimizer learner)
  (when (zerop (n-instances optimizer))
    (describe optimizer)
    (describe (bpn learner)))
  (log-padded
   (monitor-bpn-results (make-sampler 1000) (bpn learner)
                        (make-cost-monitors
                         (bpn learner) :attributes `(:event "pred.")))))

#|

;;; Transcript follows:
(repeatably ()
  (let ((*log-time* nil))
    (train-digit-fnn)))
.. training at n-instances: 0
.. train cost: 0.000e+0 (0)
.. #<SEGMENTED-GD-OPTIMIZER {100E112E93}>
..  SEGMENTED-GD-OPTIMIZER description:
..    N-INSTANCES = 0
..    OPTIMIZERS = (#<SGD-OPTIMIZER
..                    #<SEGMENT-SET
..                      (#<->WEIGHT # :SIZE 15 1/1 :NORM 0.04473>
..                       #<->WEIGHT # :SIZE 3 1/1 :NORM 0.01850>
..                       #<->WEIGHT # :SIZE 50 1/1 :NORM 0.07159>
..                       #<->WEIGHT # :SIZE 5 1/1 :NORM 0.03056>)
..                      {100E335B73}>
..                    {100E06DF83}>)
..    SEGMENTS = (#<->WEIGHT (HIDDEN OUTPUT-ACTIVATION) :SIZE
..                  15 1/1 :NORM 0.04473>
..                #<->WEIGHT (:BIAS OUTPUT-ACTIVATION) :SIZE
..                  3 1/1 :NORM 0.01850>
..                #<->WEIGHT (INPUT HIDDEN-ACTIVATION) :SIZE
..                  50 1/1 :NORM 0.07159>
..                #<->WEIGHT (:BIAS HIDDEN-ACTIVATION) :SIZE
..                  5 1/1 :NORM 0.03056>)
..  
.. #<SGD-OPTIMIZER {100E06DF83}>
..  GD-OPTIMIZER description:
..    N-INSTANCES = 0
..    SEGMENT-SET = #<SEGMENT-SET
..                    (#<->WEIGHT (HIDDEN OUTPUT-ACTIVATION) :SIZE
..                       15 1/1 :NORM 0.04473>
..                     #<->WEIGHT (:BIAS OUTPUT-ACTIVATION) :SIZE
..                       3 1/1 :NORM 0.01850>
..                     #<->WEIGHT (INPUT HIDDEN-ACTIVATION) :SIZE
..                       50 1/1 :NORM 0.07159>
..                     #<->WEIGHT (:BIAS HIDDEN-ACTIVATION) :SIZE
..                       5 1/1 :NORM 0.03056>)
..                    {100E335B73}>
..    LEARNING-RATE = 1.00000e+0
..    MOMENTUM = 9.00000e-1
..    MOMENTUM-TYPE = :NORMAL
..    WEIGHT-DECAY = 0.00000e+0
..    WEIGHT-PENALTY = 0.00000e+0
..    N-AFTER-UPATE-HOOK = 0
..    BATCH-SIZE = 100
..  
..  BATCH-GD-OPTIMIZER description:
..    N-BEFORE-UPATE-HOOK = 0
..  #<DIGIT-FNN {100E11A423}>
..   BPN description:
..     CLUMPS = #(#<->INPUT INPUT :SIZE 10 1/50 :NORM 0.00000>
..                #<->ACTIVATION
..                  (HIDDEN-ACTIVATION :ACTIVATION) :STRIPES 1/50
..                  :CLUMPS 4>
..                #<->RELU HIDDEN :SIZE 5 1/50 :NORM 0.00000>
..                #<->ACTIVATION
..                  (OUTPUT-ACTIVATION :ACTIVATION) :STRIPES 1/50
..                  :CLUMPS 4>
..                #<->SOFTMAX-XE-LOSS OUTPUT :SIZE 3 1/50 :NORM 0.00000>)
..     N-STRIPES = 1
..     MAX-N-STRIPES = 50
..   pred. cost: 1.100d+0 (1000.00)
.. training at n-instances: 1000
.. train cost: 1.093d+0 (1000.00)
.. training at n-instances: 2000
.. train cost: 5.886d-1 (1000.00)
.. training at n-instances: 3000
.. train cost: 3.574d-3 (1000.00)
.. training at n-instances: 4000
.. train cost: 1.601d-7 (1000.00)
.. training at n-instances: 5000
.. train cost: 1.973d-9 (1000.00)
.. training at n-instances: 6000
.. train cost: 4.882d-10 (1000.00)
.. training at n-instances: 7000
.. train cost: 2.771d-10 (1000.00)
.. training at n-instances: 8000
.. train cost: 2.283d-10 (1000.00)
.. training at n-instances: 9000
.. train cost: 2.123d-10 (1000.00)
.. training at n-instances: 10000
.. train cost: 2.263d-10 (1000.00)
.. pred. cost: 2.210d-10 (1000.00)
..
==> (#<->WEIGHT (:BIAS HIDDEN-ACTIVATION) :SIZE 5 1/1 :NORM 2.94294>
-->  #<->WEIGHT (INPUT HIDDEN-ACTIVATION) :SIZE 50 1/1 :NORM 11.48995>
-->  #<->WEIGHT (:BIAS OUTPUT-ACTIVATION) :SIZE 3 1/1 :NORM 3.39103>
-->  #<->WEIGHT (HIDDEN OUTPUT-ACTIVATION) :SIZE 15 1/1 :NORM 11.39339>)

|#
```

<a id="x-28MGL-BP-3A-40MGL-RNN-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-RNN%20MGL-PAX:SECTION"></a>

#### 11.3.4 Recurrent Neural Nets

<a id="x-28MGL-BP-3A-40MGL-RNN-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-RNN-TUTORIAL%20MGL-PAX:SECTION"></a>

##### `rnn` Tutorial

Hopefully this example from `example/sum-sign-fnn.lisp` illustrates
the concepts involved. Make sure you are comfortable with
[`fnn` Tutorial][6b38] before reading this.

```commonlisp
(cl:defpackage :mgl-example-sum-sign-rnn
  (:use #:common-lisp #:mgl))

(in-package :mgl-example-sum-sign-rnn)

;;; There is a single input at each time step...
(defparameter *n-inputs* 1)
;;; and we want to learn the rule that outputs the sign of the sum of
;;; inputs so far in the sequence.
(defparameter *n-outputs* 3)

;;; Generate a training example that's a sequence of random length
;;; between 1 and LENGTH. Elements of the sequence are lists of two
;;; elements:
;;;
;;; 1. The input for the network (a single random number).
;;;
;;; 2. The sign of the sum of inputs so far encoded as 0, 1, 2 (for
;;;    negative, zero and positive values). To add a twist, the sum is
;;;    reset whenever a negative input is seen.
(defun make-sum-sign-instance (&key (length 10))
  (let ((length (max 1 (random length)))
        (sum 0))
    (loop for i below length
          collect (let ((x (1- (* 2 (random 2)))))
                    (incf sum x)
                    (when (< x 0)
                      (setq sum x))
                    (list x (cond ((minusp sum) 0)
                                  ((zerop sum) 1)
                                  (t 2)))))))

;;; Build an RNN with a single lstm hidden layer and softmax output.
;;; For each time step, a SUM-SIGN-FNN will be instantiated.
(defun make-sum-sign-rnn (&key (n-hiddens 1))
  (build-rnn ()
    (build-fnn (:class 'sum-sign-fnn)
      (input (->input :size 1))
      (h (->lstm input :name 'h :size n-hiddens))
      (prediction (->softmax-xe-loss (->activation h :name 'prediction
                                                   :size *n-outputs*))))))

;;; We define this class to be able to specialize how inputs are
;;; translated by adding a SET-INPUT method later.
(defclass sum-sign-fnn (fnn)
  ())

;;; We have a batch of instances from MAKE-SUM-SIGN-INSTANCE for the
;;; RNN. This function is invoked with elements of these instances
;;; belonging to the same time step (i.e. at the same index) and sets
;;; the input and target up.
(defmethod set-input (instances (fnn sum-sign-fnn))
  (let ((input-nodes (nodes (find-clump 'input fnn))))
    (setf (target (find-clump 'prediction fnn))
          (loop for stripe upfrom 0
                for instance in instances
                collect
                ;; Sequences in the batch are not of equal length. The
                ;; RNN sends a NIL our way if a sequence has run out.
                (when instance
                  (destructuring-bind (input target) instance
                    (setf (mref input-nodes stripe 0) input)
                    target))))))

;;; Train the network by minimizing the loss (cross-entropy here) with
;;; the Adam optimizer.
(defun train-sum-sign-rnn ()
  (let ((rnn (make-sum-sign-rnn)))
    (setf (max-n-stripes rnn) 50)
    ;; Initialize the weights in the usual sqrt(1 / fan-in) style.
    (map-segments (lambda (weights)
                    (let* ((fan-in (mat-dimension (nodes weights) 0))
                           (limit (sqrt (/ 6 fan-in))))
                      (uniform-random! (nodes weights)
                                       :limit (* 2 limit))
                      (.+! (- limit) (nodes weights))))
                  rnn)
    (minimize (monitor-optimization-periodically
               (make-instance 'adam-optimizer
                              :learning-rate 0.2
                              :mean-decay 0.9
                              :mean-decay-decay 0.9
                              :variance-decay 0.9
                              :batch-size 100)
               '((:fn log-test-error :period 30000)
                 (:fn reset-optimization-monitors :period 3000)))
              (make-instance 'bp-learner
                             :bpn rnn
                             :monitors (make-cost-monitors rnn))
              :dataset (make-sampler 30000))))

;;; Return a sampler object that produces MAX-N-SAMPLES number of
;;; random inputs.
(defun make-sampler (max-n-samples &key (length 10))
  (make-instance 'function-sampler :max-n-samples max-n-samples
                 :generator (lambda ()
                              (make-sum-sign-instance :length length))))

;;; Log the test error. Also, describe the optimizer and the bpn at
;;; the beginning of training. Called periodically during training
;;; (see above).
(defun log-test-error (optimizer learner)
  (when (zerop (n-instances optimizer))
    (describe optimizer)
    (describe (bpn learner)))
  (let ((rnn (bpn learner)))
    (log-padded
     (append
      (monitor-bpn-results (make-sampler 1000) rnn
                           (make-cost-monitors
                            rnn :attributes '(:event "pred.")))
      ;; Same result in a different way: monitor predictions for
      ;; sequences up to length 20, but don't unfold the RNN
      ;; unnecessarily to save memory.
      (let ((*warp-time* t))
        (monitor-bpn-results (make-sampler 1000 :length 20) rnn
                             ;; Just collect and reset the warp
                             ;; monitors after each batch of
                             ;; instances.
                             (make-cost-monitors
                              rnn :attributes '(:event "warped pred."))))))
    ;; Verify that no further unfoldings took place.
    (assert (<= (length (clumps rnn)) 10)))
  (log-mat-room))

#|

;;; Transcript follows:
(let (;; Backprop nets do not need double float. Using single floats
      ;; is faster and needs less memory.
      (*default-mat-ctype* :float)
      ;; Enable moving data in and out of GPU memory so that the RNN
      ;; can work with sequences so long that the unfolded network
      ;; wouldn't otherwise fit in the GPU.
      (*cuda-window-start-time* 1)
      (*log-time* nil))
  ;; Seed the random number generators.
  (repeatably ()
    ;; Enable CUDA if available.
    (with-cuda* ()
      (train-sum-sign-rnn))))
.. training at n-instances: 0
.. cost: 0.000e+0 (0)
.. #<ADAM-OPTIMIZER {1006CD5663}>
..  GD-OPTIMIZER description:
..    N-INSTANCES = 0
..    SEGMENT-SET = #<SEGMENT-SET
..                    (#<->WEIGHT (H #) :SIZE 1 1/1 :NORM 1.73685>
..                     #<->WEIGHT (H #) :SIZE 1 1/1 :NORM 0.31893>
..                     #<->WEIGHT (#1=# #2=# :PEEPHOLE) :SIZE
..                       1 1/1 :NORM 1.81610>
..                     #<->WEIGHT (H #2#) :SIZE 1 1/1 :NORM 0.21965>
..                     #<->WEIGHT (#1# #3=# :PEEPHOLE) :SIZE
..                       1 1/1 :NORM 1.74939>
..                     #<->WEIGHT (H #3#) :SIZE 1 1/1 :NORM 0.40377>
..                     #<->WEIGHT (H PREDICTION) :SIZE
..                       3 1/1 :NORM 2.15898>
..                     #<->WEIGHT (:BIAS PREDICTION) :SIZE
..                       3 1/1 :NORM 2.94470>
..                     #<->WEIGHT (#1# #4=# :PEEPHOLE) :SIZE
..                       1 1/1 :NORM 0.97601>
..                     #<->WEIGHT (INPUT #4#) :SIZE 1 1/1 :NORM 0.65261>
..                     #<->WEIGHT (:BIAS #4#) :SIZE 1 1/1 :NORM 0.37653>
..                     #<->WEIGHT (INPUT #1#) :SIZE 1 1/1 :NORM 0.92334>
..                     #<->WEIGHT (:BIAS #1#) :SIZE 1 1/1 :NORM 0.01609>
..                     #<->WEIGHT (INPUT #5=#) :SIZE 1 1/1 :NORM 1.09995>
..                     #<->WEIGHT (:BIAS #5#) :SIZE 1 1/1 :NORM 1.41244>
..                     #<->WEIGHT (INPUT #6=#) :SIZE 1 1/1 :NORM 0.40475>
..                     #<->WEIGHT (:BIAS #6#) :SIZE 1 1/1 :NORM 1.75358>)
..                    {1006CD8753}>
..    LEARNING-RATE = 2.00000e-1
..    MOMENTUM = NONE
..    MOMENTUM-TYPE = :NONE
..    WEIGHT-DECAY = 0.00000e+0
..    WEIGHT-PENALTY = 0.00000e+0
..    N-AFTER-UPATE-HOOK = 0
..    BATCH-SIZE = 100
..  
..  BATCH-GD-OPTIMIZER description:
..    N-BEFORE-UPATE-HOOK = 0
..  
..  ADAM-OPTIMIZER description:
..    MEAN-DECAY-RATE = 1.00000e-1
..    MEAN-DECAY-RATE-DECAY = 9.00000e-1
..    VARIANCE-DECAY-RATE = 1.00000e-1
..    VARIANCE-ADJUSTMENT = 1.00000d-7
..  #<RNN {10047C77E3}>
..   BPN description:
..     CLUMPS = #(#<SUM-SIGN-FNN :STRIPES 1/50 :CLUMPS 4>
..                #<SUM-SIGN-FNN :STRIPES 1/50 :CLUMPS 4>)
..     N-STRIPES = 1
..     MAX-N-STRIPES = 50
..   
..   RNN description:
..     MAX-LAG = 1
..   pred.        cost: 1.223e+0 (4455.00)
.. warped pred. cost: 1.228e+0 (9476.00)
.. Foreign memory usage:
.. foreign arrays: 162 (used bytes: 39,600)
.. CUDA memory usage:
.. device arrays: 114 (used bytes: 220,892, pooled bytes: 19,200)
.. host arrays: 162 (used bytes: 39,600)
.. host->device copies: 6,164, device->host copies: 4,490
.. training at n-instances: 3000
.. cost: 3.323e-1 (13726.00)
.. training at n-instances: 6000
.. cost: 3.735e-2 (13890.00)
.. training at n-instances: 9000
.. cost: 1.012e-2 (13872.00)
.. training at n-instances: 12000
.. cost: 3.026e-3 (13953.00)
.. training at n-instances: 15000
.. cost: 9.267e-4 (13948.00)
.. training at n-instances: 18000
.. cost: 2.865e-4 (13849.00)
.. training at n-instances: 21000
.. cost: 8.893e-5 (13758.00)
.. training at n-instances: 24000
.. cost: 2.770e-5 (13908.00)
.. training at n-instances: 27000
.. cost: 8.514e-6 (13570.00)
.. training at n-instances: 30000
.. cost: 2.705e-6 (13721.00)
.. pred.        cost: 1.426e-6 (4593.00)
.. warped pred. cost: 1.406e-6 (9717.00)
.. Foreign memory usage:
.. foreign arrays: 216 (used bytes: 52,800)
.. CUDA memory usage:
.. device arrays: 148 (used bytes: 224,428, pooled bytes: 19,200)
.. host arrays: 216 (used bytes: 52,800)
.. host->device copies: 465,818, device->host copies: 371,990
..
==> (#<->WEIGHT (H (H :OUTPUT)) :SIZE 1 1/1 :NORM 0.10624>
-->  #<->WEIGHT (H (H :CELL)) :SIZE 1 1/1 :NORM 0.94460>
-->  #<->WEIGHT ((H :CELL) (H :FORGET) :PEEPHOLE) :SIZE 1 1/1 :NORM 0.61312>
-->  #<->WEIGHT (H (H :FORGET)) :SIZE 1 1/1 :NORM 0.38093>
-->  #<->WEIGHT ((H :CELL) (H :INPUT) :PEEPHOLE) :SIZE 1 1/1 :NORM 1.17956>
-->  #<->WEIGHT (H (H :INPUT)) :SIZE 1 1/1 :NORM 0.88011>
-->  #<->WEIGHT (H PREDICTION) :SIZE 3 1/1 :NORM 49.93808>
-->  #<->WEIGHT (:BIAS PREDICTION) :SIZE 3 1/1 :NORM 10.98112>
-->  #<->WEIGHT ((H :CELL) (H :OUTPUT) :PEEPHOLE) :SIZE 1 1/1 :NORM 0.67996>
-->  #<->WEIGHT (INPUT (H :OUTPUT)) :SIZE 1 1/1 :NORM 0.65251>
-->  #<->WEIGHT (:BIAS (H :OUTPUT)) :SIZE 1 1/1 :NORM 10.23003>
-->  #<->WEIGHT (INPUT (H :CELL)) :SIZE 1 1/1 :NORM 5.98116>
-->  #<->WEIGHT (:BIAS (H :CELL)) :SIZE 1 1/1 :NORM 0.10681>
-->  #<->WEIGHT (INPUT (H :FORGET)) :SIZE 1 1/1 :NORM 4.46301>
-->  #<->WEIGHT (:BIAS (H :FORGET)) :SIZE 1 1/1 :NORM 1.57195>
-->  #<->WEIGHT (INPUT (H :INPUT)) :SIZE 1 1/1 :NORM 0.36401>
-->  #<->WEIGHT (:BIAS (H :INPUT)) :SIZE 1 1/1 :NORM 8.63833>)

|#
```

<a id="x-28MGL-BP-3ARNN-20CLASS-29"></a>
<a id="MGL-BP:RNN%20CLASS"></a>

- [class] **rnn** *[bpn][5187]*

    A recurrent neural net (as opposed to a
    feed-forward one. It is typically built with [`build-rnn`][764b] that's no
    more than a shallow convenience macro.
    
    An `rnn` takes instances as inputs that are sequences of variable
    length. At each time step, the next unprocessed elements of these
    sequences are set as input until all input sequences in the batch
    run out. To be able to perform backpropagation, all intermediate
    [`lump`][c1ac]s must be kept around, so the recursive connections are
    transformed out by
    [unfolding](http://en.wikipedia.org/wiki/Backpropagation_through_time)
    the network. Just how many lumps this means depends on the length of
    the sequences.
    
    When an `rnn` is created, `max-lag + 1` [`bpn`][5187]s are instantiated so
    that all weights are present and one can start training it.

<a id="x-28MGL-BP-3AUNFOLDER-20-28MGL-PAX-3AREADER-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:UNFOLDER%20%28MGL-PAX:READER%20MGL-BP:RNN%29"></a>

- [reader] **unfolder** *[rnn][b0f3] (:unfolder)*

    The `unfolder` of an [`rnn`][b0f3] is function of no arguments
    that builds and returns a [`bpn`][5187]. The unfolder is allowed to create
    networks with arbitrary topology even different ones for different
    [`time-step`][6e96]s with the help of [`lag`][ff5a], or nested `rnn`s. Weights of
    the same name are shared between the folds. That is, if a [`->weight`][b76f]
    lump were to be created and a weight lump of the same name already
    exists, then the existing lump will be added to the `bpn` created by
    `unfolder`.

<a id="x-28MGL-BP-3AMAX-LAG-20-28MGL-PAX-3AREADER-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:MAX-LAG%20%28MGL-PAX:READER%20MGL-BP:RNN%29"></a>

- [reader] **max-lag** *[rnn][b0f3] (:max-lag = 1)*

    The networks built by [`unfolder`][8e53] may contain new
    weights up to time step `max-lag`. Beyond that point, all weight
    lumps must be reappearances of weight lumps with the same name at
    previous time steps. Most recurrent networks reference only the
    state of lumps at the previous time step (with the function [`lag`][ff5a]),
    hence the default of 1. But it is possible to have connections to
    arbitrary time steps. The maximum connection lag must be specified
    when creating the [`rnn`][b0f3].

<a id="x-28MGL-BP-3ACUDA-WINDOW-START-TIME-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:CUDA-WINDOW-START-TIME%20%28MGL-PAX:ACCESSOR%20MGL-BP:RNN%29"></a>

- [accessor] **cuda-window-start-time** *[rnn][b0f3] (:cuda-window-start-time = \*cuda-window-start-time\*)*

    Due to unfolding, the memory footprint of an [`rnn`][b0f3]
    is almost linear in the number of time steps (i.e. the max
    sequence length). For prediction, this is addressed by
    [Time Warp][d0e3]. For training, we cannot discard results of
    previous time steps because they are needed for backpropagation,
    but we can at least move them out of GPU memory if they are not
    going to be used for a while and copy them back before they are
    needed. Obviously, this is only relevant if CUDA is being used.
    
    If `cuda-window-start-time` is `nil`, then this feature is turned off.
    Else, during training, at `cuda-window-start-time` or later time
    steps, matrices belonging to non-weight lumps may be forced out of
    GPU memory and later brought back as neeeded.
    
    This feature is implemented in terms of
    [`mgl-mat:with-syncing-cuda-facets`][9fff] that uses CUDA host memory (also
    known as *page-locked* or *pinned memory*) to do asynchronous
    copies concurrently with normal computation. The consequence of
    this is that it is now main memory usage that's unbounded which
    toghether with page-locking makes it a potent weapon to bring a
    machine to a halt. You were warned.

<a id="x-28MGL-BP-3A-2ACUDA-WINDOW-START-TIME-2A-20VARIABLE-29"></a>
<a id="MGL-BP:*CUDA-WINDOW-START-TIME*%20VARIABLE"></a>

- [variable] **\*cuda-window-start-time\*** *nil*

    The default for [`cuda-window-start-time`][f573].

<a id="x-28MGL-BP-3ABUILD-RNN-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-BP:BUILD-RNN%20MGL-PAX:MACRO"></a>

- [macro] **build-rnn** *(&key rnn (class ''rnn) name initargs max-n-stripes (max-lag 1)) &body body*

    Create an `rnn` with `max-n-stripes` and `max-lag` whose [`unfolder`][8e53] is `body`
    wrapped in a lambda. Bind symbol given as the `rnn` argument to the
    `rnn` object so that `body` can see it.

<a id="x-28MGL-BP-3ALAG-20FUNCTION-29"></a>
<a id="MGL-BP:LAG%20FUNCTION"></a>

- [function] **lag** *name &key (lag 1) rnn path*

    In `rnn` or if it's `nil` the `rnn` being extended with another
    [`bpn`][5187] (called *unfolding*), look up the [`clump`][a4fe] with `name` in the `bpn`
    that's `lag` number of time steps before the `bpn` being added. If this
    function is called from [`unfolder`][8e53] of an `rnn` (which is what happens
    behind the scene in the body of [`build-rnn`][764b]), then it returns an
    opaque object representing a lagged connection to a clump, else it
    returns the `clump` itself.
    
    FIXDOC: `path`

<a id="x-28MGL-BP-3ATIME-STEP-20FUNCTION-29"></a>
<a id="MGL-BP:TIME-STEP%20FUNCTION"></a>

- [function] **time-step** *&key (rnn \*rnn\*)*

    Return the time step `rnn` is currently executing or being unfolded for.
    It is 0 when the `rnn` is being unfolded for the first time.

<a id="x-28MGL-CORE-3ASET-INPUT-20-28METHOD-20-28T-20MGL-BP-3ARNN-29-29-29"></a>
<a id="MGL-CORE:SET-INPUT%20%28METHOD%20%28T%20MGL-BP:RNN%29%29"></a>

- [method] **set-input** *instances (rnn rnn)*

    `rnn`s operate on batches of instances just like [`fnn`][9de4]s. But the
    instances here are like datasets: sequences or samplers and they are
    turned into sequences of batches of instances with
    [`map-datasets`][765c] `:impute` `nil`. The batch of instances at index 2 is
    clamped onto the [`bpn`][5187] at time step 2 with `set-input`.
    
    When the input sequences in the batch are not of the same length,
    already exhausted sequences will produce `nil` (due to `:impute` `nil`)
    above. When such a `nil` is clamped with `set-input` on a `bpn` of the
    `rnn`, `set-input` must set the [`importance`][038e] of the ->ERROR lumps to 0
    else training would operate on the noise left there by previous
    invocations.

<a id="x-28MGL-BP-3A-40MGL-RNN-TIME-WARP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-RNN-TIME-WARP%20MGL-PAX:SECTION"></a>

##### Time Warp

The unbounded memory usage of [`rnn`][b0f3]s with one [`bpn`][5187] allocated per
time step can become a problem. For training, where the gradients
often have to be backpropagated from the last time step to the very
beginning, this is hard to solve but with [`cuda-window-start-time`][f573] the
limit is no longer GPU memory.

For prediction on the other hand, one doesn't need to keep old steps
around indefinitely: they can be discarded when future time steps
will never reference them again.

<a id="x-28MGL-BP-3A-2AWARP-TIME-2A-20VARIABLE-29"></a>
<a id="MGL-BP:*WARP-TIME*%20VARIABLE"></a>

- [variable] **\*warp-time\*** *nil*

    Controls whether warping is enabled (see [Time Warp][d0e3]). Don't
    enable it for training, as it would make backprop impossible.

<a id="x-28MGL-BP-3AWARPED-TIME-20FUNCTION-29"></a>
<a id="MGL-BP:WARPED-TIME%20FUNCTION"></a>

- [function] **warped-time** *&key (rnn \*rnn\*) (time (time-step :rnn rnn)) (lag 0)*

    Return the index of the [`bpn`][5187] in [`clumps`][f7c1] of `rnn` whose task it is to
    execute computation at `(- (time-step rnn) lag)`. This is normally
    the same as [`time-step`][6e96] (disregarding `lag`). That is, `clumps` can be
    indexed by `time-step` to get the `bpn`. However, when [`*warp-time*`][ed4f] is
    true, execution proceeds in a cycle as the structure of the network
    allows.
    
    Suppose we have a typical `rnn` that only ever references the previous
    time step so its [`max-lag`][084d] is 1. Its [`unfolder`][8e53] returns `bpn`s of
    identical structure bar a shift in their time lagged connections
    except for the very first, so [`warp-start`][d6e0] and [`warp-length`][51d5] are both 1.
    If `*warp-time*` is `nil`, then the mapping from `time-step` to the `bpn` in
    `clumps` is straightforward:
    
        time:   |  0 |  1 |  2 |  3 |  4 |  5
        --------+----+----+----+----+----+----
        warped: |  0 |  1 |  2 |  3 |  4 |  5
        --------+----+----+----+----+----+----
        bpn:    | b0 | b1 | b2 | b3 | b4 | b5
    
    When `*warp-time*` is true, we reuse the `b1` - `b2` bpns in a loop:
    
        time:   |  0 |  1 |  2 |  3 |  4 |  5
        --------+----+----+----+----+----+----
        warped: |  0 |  1 |  2 |  1 |  2 |  1
        --------+----+----+----+----+----+----
        bpn:    | b0 | b1 | b2 | b1*| b2 | b1*
    
    `b1*` is the same `bpn` as `b1`, but its connections created by `lag` go
    through warped time and end up referencing `b2`. This way, memory
    consumption is independent of the number time steps needed to
    process a sequence or make predictions.
    
    To be able to pull this trick off `warp-start` and `warp-length` must be
    specified when the `rnn` is instantiated. In general, with
    `*warp-time*` `(+ warp-start (max 2 warp-length))` bpns are needed.
    The 2 comes from the fact that with cycle length 1 a bpn would need
    to takes its input from itself which is problematic because it has
    [`nodes`][cc1c] for only one set of values.

<a id="x-28MGL-BP-3AWARP-START-20-28MGL-PAX-3AREADER-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:WARP-START%20%28MGL-PAX:READER%20MGL-BP:RNN%29"></a>

- [reader] **warp-start** *[rnn][b0f3] (:warp-start = 1)*

    The [`time-step`][6e96] from which [`unfolder`][8e53] will create
    [`bpn`][5187]s that essentially repeat every [`warp-length`][51d5] steps.

<a id="x-28MGL-BP-3AWARP-LENGTH-20-28MGL-PAX-3AREADER-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:WARP-LENGTH%20%28MGL-PAX:READER%20MGL-BP:RNN%29"></a>

- [reader] **warp-length** *[rnn][b0f3] (:warp-length = 1)*

    An integer such that the [`bpn`][5187] [`unfolder`][8e53] creates at
    time step `i` (where `(<= warp-start i)`) is identical to the `bpn`
    created at time step `(+ warp-start (mod (- i warp-start)
    warp-length))` except for a shift in its time lagged
    connections.

<a id="x-28MGL-BP-3ASTEP-MONITORS-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3ARNN-29-29"></a>
<a id="MGL-BP:STEP-MONITORS%20%28MGL-PAX:ACCESSOR%20MGL-BP:RNN%29"></a>

- [accessor] **step-monitors** *[rnn][b0f3] (:step-monitors = nil)*

    During training, unfolded [`bpn`][5187]s corresponding to
    previous time steps may be expensive to get at because they are no
    longer in GPU memory. This consideration also applies to making
    prediction with the additional caveat that with [`*warp-time*`][ed4f] true,
    previous states are discarded so it's not possible to gather
    statistics after [`forward`][c1ae] finished.
    
    Add monitor objects to this slot and they will be automatically
    applied to the [`rnn`][b0f3] after each step when `forward`ing the `rnn`
    during training or prediction. To be able to easily switch between
    sets of monitors, in addition to a list of monitors this can be a
    symbol or a function, too. If it's a symbol, then its a designator
    for its [`symbol-value`][cee6]. If it's a function, then it must have no
    arguments and it's a designator for its return value.

<a id="x-28MGL-BP-3A-40MGL-BP-LUMPS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-LUMPS%20MGL-PAX:SECTION"></a>

### 11.4 Lumps

<a id="x-28MGL-BP-3A-40MGL-BP-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-LUMP%20MGL-PAX:SECTION"></a>

#### 11.4.1 Lump Base Class

<a id="x-28MGL-BP-3ALUMP-20CLASS-29"></a>
<a id="MGL-BP:LUMP%20CLASS"></a>

- [class] **lump** *[clump][a4fe]*

    A `lump` is a simple, layerlike component of a neural
    network. There are many kinds of lumps, each of which performs a
    specific operation or just stores inputs and weights. By convention,
    the names of lumps start with the prefix `->`. Defined as classes,
    they also have a function of the same name as the class to create
    them easily. These maker functions typically have keyword arguments
    corresponding to initargs of the class, with some (mainly the input
    lumps) turned into normal positional arguments. So instead of having
    to do
    
        (make-instance '->tanh :x some-input :name 'my-tanh)
    
    one can simply write
    
        (->tanh some-input :name 'my-tanh)
    
    Lumps instantiated in any way within a [`build-fnn`][606c] or [`build-rnn`][764b] are
    automatically added to the network being built.
    
    A lump has its own [`nodes`][cc1c] and [`derivatives`][a81b] matrices allocated for it
    in which the results of the forward and backward passes are stored.
    This is in contrast to a [`bpn`][5187] whose `nodes` and `derivatives`
    are those of its last constituent [`clump`][a4fe].
    
    Since lumps almost always live within a `bpn`, their
    [`n-stripes`][07fb] and [`max-n-stripes`][91a3] are
    handled automagically behind the scenes.

<a id="x-28MGL-COMMON-3ASIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3ALUMP-29-29"></a>
<a id="MGL-COMMON:SIZE%20%28MGL-PAX:READER%20MGL-BP:LUMP%29"></a>

- [reader] **size** *[lump][c1ac] (:size)*

    The number of values in a single stripe.

<a id="x-28MGL-COMMON-3ADEFAULT-VALUE-20-28MGL-PAX-3AREADER-20MGL-BP-3ALUMP-29-29"></a>
<a id="MGL-COMMON:DEFAULT-VALUE%20%28MGL-PAX:READER%20MGL-BP:LUMP%29"></a>

- [reader] **default-value** *[lump][c1ac] (:default-value = 0)*

    Upon creation or resize the lump's nodes get
    filled with this value.

<a id="x-28MGL-BP-3ADEFAULT-SIZE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-BP:DEFAULT-SIZE%20GENERIC-FUNCTION"></a>

- [generic-function] **default-size** *lump*

    Return a default for the [`size`][85d3] of
    `lump` if one is not supplied at instantiation. The value is often
    computed based on the sizes of the inputs. This function is for
    implementing new lump types.

<a id="x-28MGL-COMMON-3ANODES-20-28MGL-PAX-3AREADER-20MGL-BP-3ALUMP-29-29"></a>
<a id="MGL-COMMON:NODES%20%28MGL-PAX:READER%20MGL-BP:LUMP%29"></a>

- [reader] **nodes** *[lump][c1ac] (= nil)*

    The values computed by the lump in the forward
    pass are stored here. It is an `n-stripes * size` matrix that has
    storage allocated for `max-n-stripes * size` elements for
    non-weight lumps. [`->weight`][b76f] lumps have no stripes nor restrictions
    on their shape.

<a id="x-28MGL-BP-3ADERIVATIVES-20-28MGL-PAX-3AREADER-20MGL-BP-3ALUMP-29-29"></a>
<a id="MGL-BP:DERIVATIVES%20%28MGL-PAX:READER%20MGL-BP:LUMP%29"></a>

- [reader] **derivatives** *[lump][c1ac]*

    The derivatives computed in the backward pass are
    stored here. This matrix is very much like [`nodes`][d699]
    in shape and size.

<a id="x-28MGL-BP-3A-40MGL-BP-INPUTS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-INPUTS%20MGL-PAX:SECTION"></a>

#### 11.4.2 Inputs

<a id="x-28MGL-BP-3A-40MGL-BP-INPUT-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-INPUT-LUMP%20MGL-PAX:SECTION"></a>

##### Input Lump

<a id="x-28MGL-BP-3A--3EINPUT-20CLASS-29"></a>
<a id="MGL-BP:-%3EINPUT%20CLASS"></a>

- [class] **->input** *[->dropout][441b]*

    A lump that has no input lumps, does not change its
    values in the forward pass (except when [`dropout`][e7f6] is non-zero), and does not compute derivatives. *Clamp*
    inputs on [`nodes`][cc1c] of input lumps in [`set-input`][0c9e].
    
    For convenience, `->input` can perform dropout itself although it
    defaults to no dropout.
    
    ```common-lisp
    (->input :size 10 :name 'some-input)
    ==> #<->INPUT SOME-INPUT :SIZE 10 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-BP-3ADROPOUT-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EINPUT-29-29"></a>
<a id="MGL-BP:DROPOUT%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EINPUT%29"></a>

- [accessor] **dropout** *[->input][f54e] (= nil)*

    See [`dropout`][2481].

<a id="x-28MGL-BP-3A-40MGL-BP-EMBEDDING-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-EMBEDDING-LUMP%20MGL-PAX:SECTION"></a>

##### Embedding Lump

This lump is like an input and a simple activation molded together
in the name of efficiency.

<a id="x-28MGL-BP-3A--3EEMBEDDING-20CLASS-29"></a>
<a id="MGL-BP:-%3EEMBEDDING%20CLASS"></a>

- [class] **->embedding** *[lump][c1ac]*

    Select rows of [`weights`][ab3c], one row for each index in
    [`input-row-indices`][1a52]. This lump is equivalent to adding an [`->input`][f54e] lump
    with a one hot encoding scheme and a [`->v*m`][dbc4] lump on top of it, but it
    is more efficient in execution and in memory usage, because it works
    with a sparse representation of the input.
    
    The [`size`][019f] of this lump is the number of columns of `weights` which is
    determined automatically.
    
    ```common-lisp
    (->embedding :weights (->weight :name 'embedding-weights
                                    :dimensions '(3 5))
                 :name 'embeddings)
    ==> #<->EMBEDDING EMBEDDINGS :SIZE 5 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-COMMON-3AWEIGHTS-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EEMBEDDING-29-29"></a>
<a id="MGL-COMMON:WEIGHTS%20%28MGL-PAX:READER%20MGL-BP:-%3EEMBEDDING%29"></a>

- [reader] **weights** *[->embedding][f1c1] (:weights)*

    A weight lump whose rows indexed by
    [`input-row-indices`][1a52] are copied to the output of this lump.

<a id="x-28MGL-BP-3AINPUT-ROW-INDICES-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EEMBEDDING-29-29"></a>
<a id="MGL-BP:INPUT-ROW-INDICES%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EEMBEDDING%29"></a>

- [accessor] **input-row-indices** *[->embedding][f1c1] (:input-row-indices)*

    A sequence of batch size length of row indices. To
    be set in [`set-input`][0c9e].

<a id="x-28MGL-BP-3A-40MGL-BP-WEIGHT-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-WEIGHT-LUMP%20MGL-PAX:SECTION"></a>

#### 11.4.3 Weight Lump

<a id="x-28MGL-BP-3A--3EWEIGHT-20CLASS-29"></a>
<a id="MGL-BP:-%3EWEIGHT%20CLASS"></a>

- [class] **->weight** *[lump][c1ac]*

    A set of optimizable parameters of some kind. When
    a [`bpn`][5187] is is trained (see [Training][0d82]) the [`nodes`][cc1c] of weight lumps
    will be changed. Weight lumps perform no computation.
    
    Weights can be created by specifying the total size or the
    dimensions:
    
    ```common-lisp
    (dimensions (->weight :size 10 :name 'w))
    => (1 10)
    (dimensions (->weight :dimensions '(5 10) :name 'w))
    => (5 10)
    ```

<a id="x-28MGL-BP-3ADIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EWEIGHT-29-29"></a>
<a id="MGL-BP:DIMENSIONS%20%28MGL-PAX:READER%20MGL-BP:-%3EWEIGHT%29"></a>

- [reader] **dimensions** *[->weight][b76f] (:dimensions)*

    [`nodes`][cc1c] and [`derivatives`][a81b] of this lump will be
    allocated with these dimensions.

<a id="x-28MGL-BP-3AWITH-WEIGHTS-COPIED-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-BP:WITH-WEIGHTS-COPIED%20MGL-PAX:MACRO"></a>

- [macro] **with-weights-copied** *(from-bpn) &body body*

    In `body` [`->weight`][b76f] will first look up if a weight lump of the same
    name exists in `from-bpn` and return that, or else create a weight
    lump normally. If `from-bpn` is `nil`, then no weights are copied.

<a id="x-28MGL-BP-3A-40MGL-BP-ACTIVATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-ACTIVATIONS%20MGL-PAX:SECTION"></a>

#### 11.4.4 Activations

<a id="x-28MGL-BP-3A-40MGL-BP-ACTIVATION-SUBNET-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-ACTIVATION-SUBNET%20MGL-PAX:SECTION"></a>

##### Activation Subnet

So we have some inputs. Usually the next step is to multiply the
input vector with a weight matrix and add biases. This can be done
directly with ->+, [`->v*m`][dbc4] and [`->weight`][b76f], but it's more convenient to
use activation subnets to reduce the clutter.

<a id="x-28MGL-BP-3A--3EACTIVATION-20CLASS-29"></a>
<a id="MGL-BP:-%3EACTIVATION%20CLASS"></a>

- [class] **->activation** *[bpn][5187]*

    Activation subnetworks are built by the function
    [`->activation`][b602] and they have a number of lumps hidden inside them.
    Ultimately, this subnetwork computes a sum like `sum_i x_i * W_i +
    sum_j y_j .* V_j + biases` where `x_i` are input lumps, `W_i` are
    dense matrices representing connections, while `V_j` are peephole
    connection vectors that are mulitplied in an elementwise manner with
    their corresponding input `y_j`.

<a id="x-28MGL-BP-3A--3EACTIVATION-20FUNCTION-29"></a>
<a id="MGL-BP:-%3EACTIVATION%20FUNCTION"></a>

- [function] **->activation** *inputs &key (name (gensym)) size peepholes (add-bias-p t)*

    Create a subnetwork of class [`->activation`][7162] that computes the over
    activation from dense connection from lumps in `inputs`, and
    elementwise connection from lumps in `peepholes`. Create new [`->weight`][b76f]
    lumps as necessary. `inputs` and `peepholes` can be a single lump or a
    list of lumps. Finally, if `add-bias-p`, then add an elementwise bias
    too. `size` must be specified explicitly, because it is not possible
    to determine it unless there are peephole connections.
    
    ```common-lisp
    (->activation (->input :size 10 :name 'input) :name 'h1 :size 4)
    ==> #<->ACTIVATION (H1 :ACTIVATION) :STRIPES 1/1 :CLUMPS 4>
    ```
    
    This is the basic workhorse of neural networks which takes care of
    the linear transformation whose results and then fed to some
    non-linearity ([`->sigmoid`][83f9], [`->tanh`][5309], etc).
    
    The name of the subnetwork clump is `(,name :activation)`. The bias
    weight lump (if any) is named `(:bias ,name)`. Dense connection
    weight lumps are named are named after the input and `name`: `(,(name
    input) ,name)`, while peepholes weight lumps are named `(,(name
    input) ,name :peephole)`. This is useful to know if, for example,
    they are to be initialized differently.

<a id="x-28MGL-BP-3A-40MGL-BP-BATCH-NORMALIZATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-BATCH-NORMALIZATION%20MGL-PAX:SECTION"></a>

##### Batch-Normalization

<a id="x-28MGL-BP-3A--3EBATCH-NORMALIZED-20CLASS-29"></a>
<a id="MGL-BP:-%3EBATCH-NORMALIZED%20CLASS"></a>

- [class] **->batch-normalized** *[lump][c1ac]*

    This is an implementation of v3 of the [Batch
    Normalization paper](http://arxiv.org/abs/1502.03167). The output of
    `->batch-normalized` is its input normalized so that for all elements
    the mean across stripes is zero and the variance is 1. That is, the
    mean of the batch is subtracted from the inputs and they are
    rescaled by their sample stddev. Actually, after the normalization
    step the values are rescaled and shifted (but this time with learnt
    parameters) in order to keep the representational power of the model
    the same. The primary purpose of this lump is to speed up learning,
    but it also acts as a regularizer. See the paper for the details.
    
    To normalize the output of `lump` without no additional
    regularizer effect:
    
    ```commonlisp
    (->batch-normalized lump :batch-size :use-population)
    ```
    
    The above uses an exponential moving average to estimate the mean
    and variance of batches and these estimations are used at both
    training and test time. In contrast to this, the published version
    uses the sample mean and variance of the current batch at training
    time which injects noise into the process. The noise is higher for
    lower batch sizes and has a regularizing effect. This is the default
    behavior (equivalent to `:batch-size nil`):
    
    ```commonlisp
    (->batch-normalized lump)
    ```
    
    For performance reasons one may wish to process a higher number of
    instances in a batch (in the sense of [`n-stripes`][8dd7]) and get the
    regularization effect associated with a lower batch size. This is
    possible by setting `:batch-size` to a divisor of the the number of
    stripes. Say, the number of stripes is 128, but we want as much
    regularization as we would get with 32:
    
    ```commonlisp
    (->batch-normalized lump :batch-size 32)
    ```
    
    The primary input of `->batch-normalized` is often an `->activation`([`0`][7162] [`1`][b602]) and
    its output is fed into an activation function (see
    [Activation Functions][5d86]).

<a id="x-28MGL-BP-3ABATCH-NORMALIZATION-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZED-29-29"></a>
<a id="MGL-BP:BATCH-NORMALIZATION%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZED%29"></a>

- [reader] **batch-normalization** *[->batch-normalized][9da9] (:normalization)*

    The [`->batch-normalization`][c469] of this lump. May be
    shared between multiple [`->batch-normalized`][9da9] lumps.
    
    Batch normalization is special in that it has state apart from the
    computed results ([`nodes`][cc1c]) and its derivatives ([`derivatives`][a81b]). This
    state is the estimated mean and variance of its inputs and they
    are encapsulated by `->batch-normalization`.
    
    If `normalization` is not given at instantiation, then a new
    `->batch-normalization` object will be created automatically,
    passing `:batch-size`, `:variance-adjustment`, and `:population-decay`
    arguments on to `->batch-normalization`. See [`batch-size`][c918], [`variance-adjustment`][aa86] and [`population-decay`][46c4]. New scale and shift weight lumps will be
    created with names:
    
        `(,name :scale)
        `(,name :shift)
    
    where `name` is the [`name`][5842] of this lump.
    
    This default behavior covers the use-case where the statistics
    kept by `->batch-normalization` are to be shared only between time
    steps of an [`rnn`][b0f3].

<a id="x-28MGL-BP-3A--3EBATCH-NORMALIZATION-20CLASS-29"></a>
<a id="MGL-BP:-%3EBATCH-NORMALIZATION%20CLASS"></a>

- [class] **->batch-normalization** *[->weight][b76f]*

    The primary purpose of this class is to hold the
    estimated mean and variance of the inputs to be normalized and allow
    them to be shared between multiple [`->batch-normalized`][9da9] lumps that
    carry out the computation. These estimations are saved and loaded by
    [`save-state`][c102] and [`load-state`][6bd7].
    
    ```commonlisp
    (->batch-normalization (->weight :name '(h1 :scale) :size 10)
                           (->weight :name '(h1 :shift) :size 10)
                           :name '(h1 :batch-normalization))
    ```

<a id="x-28MGL-COMMON-3ASCALE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZATION-29-29"></a>
<a id="MGL-COMMON:SCALE%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29"></a>

- [reader] **scale** *[->batch-normalization][c469] (:scale)*

    A weight lump of the same size as [`shift`][7960]. This is
    $\gamma$ in the paper.

<a id="x-28MGL-BP-3ASHIFT-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZATION-29-29"></a>
<a id="MGL-BP:SHIFT%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29"></a>

- [reader] **shift** *[->batch-normalization][c469] (:shift)*

    A weight lump of the same size as [`scale`][8970]. This is
    $\beta$ in the paper.

<a id="x-28MGL-COMMON-3ABATCH-SIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZATION-29-29"></a>
<a id="MGL-COMMON:BATCH-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29"></a>

- [reader] **batch-size** *[->batch-normalization][c469] (:batch-size = nil)*

    Normally all stripes participate in the batch.
    Lowering the number of stripes may increase the regularization
    effect, but it also makes the computation less efficient. By
    setting `batch-size` to a divisor of [`n-stripes`][8dd7] one can decouple the
    concern of efficiency from that of regularization. The default
    value, `nil`, is equivalent to `n-stripes`. `batch-size` only affects
    training.
    
    With the special value `:use-population`, instead of the mean and
    the variance of the current batch, use the population statistics
    for normalization. This effectively cancels the regularization
    effect, leaving only the faster learning.

<a id="x-28MGL-GD-3AVARIANCE-ADJUSTMENT-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZATION-29-29"></a>
<a id="MGL-GD:VARIANCE-ADJUSTMENT%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29"></a>

- [reader] **variance-adjustment** *[->batch-normalization][c469] (:variance-adjustment = 1.0e-4)*

    A small positive real number that's added to the
    sample variance. This is $\epsilon$ in the paper.

<a id="x-28MGL-BP-3APOPULATION-DECAY-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EBATCH-NORMALIZATION-29-29"></a>
<a id="MGL-BP:POPULATION-DECAY%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29"></a>

- [reader] **population-decay** *[->batch-normalization][c469] (:population-decay = 0.99)*

    While training, an exponential moving average of
    batch means and standard deviances (termed *population
    statistics*) is updated. When making predictions, normalization is
    performed using these statistics. These population statistics are
    persisted by [`save-state`][c102].

<a id="x-28MGL-BP-3A--3EBATCH-NORMALIZED-ACTIVATION-20FUNCTION-29"></a>
<a id="MGL-BP:-%3EBATCH-NORMALIZED-ACTIVATION%20FUNCTION"></a>

- [function] **->batch-normalized-activation** *inputs &key (name (gensym)) size peepholes batch-size variance-adjustment population-decay*

    A utility functions that creates and wraps an `->activation`([`0`][7162] [`1`][b602]) in
    [`->batch-normalized`][9da9] and with its [`batch-normalization`][eaf1] the two weight
    lumps for the scale and shift
    parameters. `(->batch-normalized-activation inputs :name 'h1 :size
    10)` is equivalent to:
    
    ```commonlisp
    (->batch-normalized (->activation inputs :name 'h1 :size 10 :add-bias-p nil)
                        :name '(h1 :batch-normalized-activation))
    ```
    
    Note how biases are turned off since normalization will cancel them
    anyway (but a shift is added which amounts to the same effect).

<a id="x-28MGL-BP-3A-40MGL-BP-ACTIVATION-FUNCTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-ACTIVATION-FUNCTIONS%20MGL-PAX:SECTION"></a>

#### 11.4.5 Activation Functions

Now we are moving on to the most important non-linearities to which
activations are fed.

<a id="x-28MGL-BP-3A-40MGL-BP-SIGMOID-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SIGMOID-LUMP%20MGL-PAX:SECTION"></a>

##### Sigmoid Lump

<a id="x-28MGL-BP-3A--3ESIGMOID-20CLASS-29"></a>
<a id="MGL-BP:-%3ESIGMOID%20CLASS"></a>

- [class] **->sigmoid** *[->dropout][441b] [lump][c1ac]*

    Applies the `1/(1 + e^{-x})` function elementwise
    to its inputs. This is one of the classic non-linearities for neural
    networks.
    
    For convenience, `->sigmoid` can perform dropout itself although it
    defaults to no dropout.
    
    ```common-lisp
    (->sigmoid (->activation (->input :size 10) :size 5) :name 'this)
    ==> #<->SIGMOID THIS :SIZE 5 1/1 :NORM 0.00000>
    ```
    
    The [`size`][019f] of this lump is the size of its input which is determined
    automatically.

<a id="x-28MGL-BP-3ADROPOUT-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3ESIGMOID-29-29"></a>
<a id="MGL-BP:DROPOUT%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ESIGMOID%29"></a>

- [accessor] **dropout** *[->sigmoid][83f9] (= nil)*

    See [`dropout`][2481].

<a id="x-28MGL-BP-3A-40MGL-BP-TANH-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-TANH-LUMP%20MGL-PAX:SECTION"></a>

##### Tanh Lump

<a id="x-28MGL-BP-3A--3ETANH-20CLASS-29"></a>
<a id="MGL-BP:-%3ETANH%20CLASS"></a>

- [class] **->tanh** *[lump][c1ac]*

    Applies the [`tanh`][993b] function to its input in an
    elementwise manner. The [`size`][019f] of this lump is the size of its input
    which is determined automatically.

<a id="x-28MGL-BP-3A-40MGL-BP-SCALED-TANH-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SCALED-TANH-LUMP%20MGL-PAX:SECTION"></a>

##### Scaled Tanh Lump

<a id="x-28MGL-BP-3A--3ESCALED-TANH-20CLASS-29"></a>
<a id="MGL-BP:-%3ESCALED-TANH%20CLASS"></a>

- [class] **->scaled-tanh** *[lump][c1ac]*

    Pretty much like [`tanh`][993b] but its input and output is
    scaled in such a way that the variance of its output is close to 1
    if the variance of its input is close to 1 which is a nice property
    to combat vanishing gradients. The actual function is `1.7159 *
    tanh(2/3 * x)`. The [`size`][019f] of this lump is the size of its input which
    is determined automatically.

<a id="x-28MGL-BP-3A-40MGL-BP-RELU-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-RELU-LUMP%20MGL-PAX:SECTION"></a>

##### Relu Lump

We are somewhere around year 2007 by now.

<a id="x-28MGL-BP-3A--3ERELU-20CLASS-29"></a>
<a id="MGL-BP:-%3ERELU%20CLASS"></a>

- [class] **->relu** *[lump][c1ac]*

    `max(0,x)` activation function. Be careful, relu
    units can get stuck in the off state: if they move to far to
    negative territory it can be very difficult to get out of it. The
    [`size`][019f] of this lump is the size of its input which is determined
    automatically.

<a id="x-28MGL-BP-3A-40MGL-BP-MAX-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-MAX-LUMP%20MGL-PAX:SECTION"></a>

##### Max Lump

We are in about year 2011.

<a id="x-28MGL-BP-3A--3EMAX-20CLASS-29"></a>
<a id="MGL-BP:-%3EMAX%20CLASS"></a>

- [class] **->max** *[lump][c1ac]*

    This is basically maxout without dropout (see
    http://arxiv.org/abs/1302.4389). It groups its inputs by
    [`group-size`][59dd], and outputs the maximum of each group.
    The [`size`][019f] of the output is automatically calculated, it is the size
    of the input divided by [`group-size`][59dd].
    
    ```common-lisp
    (->max (->input :size 120) :group-size 3 :name 'my-max)
    ==> #<->MAX MY-MAX :SIZE 40 1/1 :NORM 0.00000 :GROUP-SIZE 3>
    ```
    
    The advantage of `->max` over [`->relu`][9d3a] is that flow gradient is never
    stopped so there is no problem of units getting stuck in off
    state.

<a id="x-28MGL-COMMON-3AGROUP-SIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EMAX-29-29"></a>
<a id="MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EMAX%29"></a>

- [reader] **group-size** *[->max][f652] (:group-size)*

    The number of inputs in each group.

<a id="x-28MGL-BP-3A-40MGL-BP-MIN-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-MIN-LUMP%20MGL-PAX:SECTION"></a>

##### Min Lump

<a id="x-28MGL-BP-3A--3EMIN-20CLASS-29"></a>
<a id="MGL-BP:-%3EMIN%20CLASS"></a>

- [class] **->min** *[lump][c1ac]*

    Same as [`->max`][f652], but it computes the [`min`][115e] of groups.
    Rarely useful.

<a id="x-28MGL-COMMON-3AGROUP-SIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EMIN-29-29"></a>
<a id="MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EMIN%29"></a>

- [reader] **group-size** *[->min][9a84] (:group-size)*

    The number of inputs in each group.

<a id="x-28MGL-BP-3A-40MGL-BP-MAX-CHANNEL-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-MAX-CHANNEL-LUMP%20MGL-PAX:SECTION"></a>

##### Max-Channel Lump

<a id="x-28MGL-BP-3A--3EMAX-CHANNEL-20CLASS-29"></a>
<a id="MGL-BP:-%3EMAX-CHANNEL%20CLASS"></a>

- [class] **->max-channel** *[lump][c1ac]*

    Called LWTA (Local Winner Take All) or
    Channel-Out (see http://arxiv.org/abs/1312.1909) in the literature
    it is basically [`->max`][f652], but instead of producing one output per
    group, it just produces zeros for all unit but the one with the
    maximum value in the group. This allows the next layer to get some
    information about the path along which information flowed. The [`size`][019f]
    of this lump is the size of its input which is determined
    automatically.

<a id="x-28MGL-COMMON-3AGROUP-SIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EMAX-CHANNEL-29-29"></a>
<a id="MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EMAX-CHANNEL%29"></a>

- [reader] **group-size** *[->max-channel][6021] (:group-size)*

    The number of inputs in each group.

<a id="x-28MGL-BP-3A-40MGL-BP-LOSSES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-LOSSES%20MGL-PAX:SECTION"></a>

#### 11.4.6 Losses

Ultimately, we need to tell the network what to learn which means
that the loss function to be minimized needs to be constructed as
part of the network.

<a id="x-28MGL-BP-3A-40MGL-BP-LOSS-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-LOSS-LUMP%20MGL-PAX:SECTION"></a>

##### Loss Lump

<a id="x-28MGL-BP-3A--3ELOSS-20CLASS-29"></a>
<a id="MGL-BP:-%3ELOSS%20CLASS"></a>

- [class] **->loss** *[->sum][edcf]*

    Calculate the loss for the instances in the batch.
    The main purpose of this lump is to provide a training signal.
    
    An error lump is usually a leaf in the graph of lumps (i.e. there
    are no other lumps whose input is this one). The special thing about
    error lumps is that 1 (but see [`importance`][038e]) is added automatically to
    their derivatives. Error lumps have exactly one node (per stripe)
    whose value is computed as the sum of nodes in their input lump.

<a id="x-28MGL-BP-3AIMPORTANCE-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3ELOSS-29-29"></a>
<a id="MGL-BP:IMPORTANCE%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ELOSS%29"></a>

- [accessor] **importance** *[->loss][2171] (:importance = nil)*

    This is to support weighted instances. That is
    when not all training instances are equally important. If non-`nil`,
    a 1d [`mat`][6d14] with the importances of stripes of the batch. When
    `importance` is given (typically in [`set-input`][0c9e]), then instead of
    adding 1 to the derivatives of all stripes, `importance` is added
    elemtwise.

<a id="x-28MGL-BP-3A-40MGL-BP-SQUARED-DIFFERENCE-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SQUARED-DIFFERENCE-LUMP%20MGL-PAX:SECTION"></a>

##### Squared Difference Lump

In regression, the squared error loss is most common. The squared
error loss can be constructed by combining [`->squared-difference`][e8d2] with
a [`->loss`][2171].

<a id="x-28MGL-BP-3A--3ESQUARED-DIFFERENCE-20CLASS-29"></a>
<a id="MGL-BP:-%3ESQUARED-DIFFERENCE%20CLASS"></a>

- [class] **->squared-difference** *[lump][c1ac]*

    This lump takes two input lumps and calculates
    their squared difference `(x - y)^2` in an elementwise manner. The
    [`size`][019f] of this lump is automatically determined from the size of its
    inputs. This lump is often fed into [`->loss`][2171] that sums the squared
    differences and makes it part of the function to be minimized.
    
    ```common-lisp
    (->loss (->squared-difference (->activation (->input :size 100)
                                                :size 10)
                                  (->input :name 'target :size 10))
            :name 'squared-error)
    ==> #<->LOSS SQUARED-ERROR :SIZE 1 1/1 :NORM 0.00000>
    ```
    
    Currently this lump is not CUDAized, but it will copy data from the
    GPU if it needs to.

<a id="x-28MGL-BP-3A-40MGL-BP-SOFTMAX-XE-LOSS-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SOFTMAX-XE-LOSS-LUMP%20MGL-PAX:SECTION"></a>

##### Softmax Cross-Entropy Loss Lump

<a id="x-28MGL-BP-3A--3ESOFTMAX-XE-LOSS-20CLASS-29"></a>
<a id="MGL-BP:-%3ESOFTMAX-XE-LOSS%20CLASS"></a>

- [class] **->softmax-xe-loss** *[lump][c1ac]*

    A specialized lump that computes the softmax of its
    input in the forward pass and backpropagates a cross-entropy loss.
    The advantage of doing these together is numerical stability. The
    total cross-entropy is the sum of cross-entropies per group of
    [`group-size`][a437] elements:
    
    $$
    XE(x) = - \sum_{i=1,g} t_i \ln(s_i),
    $$
    
    where `g` is the number of classes ([`group-size`][a437]), `t_i` are the targets (i.e. the true
    probabilities of the class, often all zero but one), `s_i` is the
    output of softmax calculated from input `x`:
    
    $$
    s_i = {softmax}(x_1, x_2, ..., x_g) =
      \frac{e^x_i}{\sum_{j=1,g} e^x_j}
    $$
    
    In other words, in the forward phase this lump takes input `x`,
    computes its elementwise [`exp`][bc8c], normalizes each group of
    [`group-size`][a437] elements to sum to 1 to get
    the softmax which is the result that goes into [`nodes`][cc1c]. In the
    backward phase, there are two sources of gradients: the lumps that
    use the output of this lump as their input (currently not
    implemented and would result in an error) and an implicit
    cross-entropy loss.
    
    One can get the cross-entropy calculated in the most recent forward
    pass by calling [`cost`][410c] on this lump.
    
    This is the most common loss function for classification. In fact,
    it is nearly ubiquitous. See the [`fnn` Tutorial][6b38] and the
    [`rnn` Tutorial][9700] for how this loss and [`set-input`][0c9e] work together.

<a id="x-28MGL-COMMON-3AGROUP-SIZE-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3ESOFTMAX-XE-LOSS-29-29"></a>
<a id="MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3ESOFTMAX-XE-LOSS%29"></a>

- [reader] **group-size** *[->softmax-xe-loss][85d34] (:group-size)*

    The number of elements in a softmax group. This is
    the number of classes for classification. Often `group-size` is
    equal to [`size`][019f] (it is the default), but in general the only
    constraint is that `size` is a multiple of `group-size`.

<a id="x-28MGL-COMMON-3ATARGET-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3ESOFTMAX-XE-LOSS-29-29"></a>
<a id="MGL-COMMON:TARGET%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ESOFTMAX-XE-LOSS%29"></a>

- [accessor] **target** *[->softmax-xe-loss][85d34] (:target = nil)*

    Set in [`set-input`][0c9e], this is either a [`mat`][6d14] of the same
    size as the input lump `x` or if the target is very sparse, this
    can also be a sequence of batch size length that contains the
    index value pairs of non-zero entries:
    
        (;; first instance in batch has two non-zero targets
         (;; class 10 has 30% expected probability
          (10 . 0.3)
          ;; class 2 has 70% expected probability
          (2 .  0.7))
         ;; second instance in batch puts 100% on class 7
         7
         ;; more instances in the batch follow
         ...)
    
    Actually, in the rare case where [`group-size`][a437] is not [`size`][019f] (i.e. there are several softmax
    normalization groups for every example), the length of the above
    target sequence is [`batch-size`][fa6d] \* N-GROUPS. Indices are always
    relative to the start of the group.
    
    If [`group-size`][a437] is large (for example,
    in neural language models with a huge number of words), using
    sparse targets can make things go much faster, because calculation
    of the derivative is no longer quadratic.
    
    Giving different weights to training instances is implicitly
    supported. While target values in a group should sum to 1,
    multiplying all target values with a weight `w` is equivalent to
    training that `w` times on the same example.

<a id="x-28MGL-BP-3AENSURE-SOFTMAX-TARGET-MATRIX-20FUNCTION-29"></a>
<a id="MGL-BP:ENSURE-SOFTMAX-TARGET-MATRIX%20FUNCTION"></a>

- [function] **ensure-softmax-target-matrix** *softmax-xe-loss n*

    Set [`target`][b5c7] of `softmax-xe-loss` to a [`mat`][6d14] capable of holding the dense
    target values for `n` stripes.

<a id="x-28MGL-BP-3A-40MGL-BP-STOCHASTICITY-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-STOCHASTICITY%20MGL-PAX:SECTION"></a>

#### 11.4.7 Stochasticity

<a id="x-28MGL-BP-3A-40MGL-BP-DROPOUT-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-DROPOUT-LUMP%20MGL-PAX:SECTION"></a>

##### Dropout Lump

<a id="x-28MGL-BP-3A--3EDROPOUT-20CLASS-29"></a>
<a id="MGL-BP:-%3EDROPOUT%20CLASS"></a>

- [class] **->dropout** *[lump][c1ac]*

    The output of this lump is identical to its input,
    except it randomly zeroes out some of them during training which act
    as a very strong regularizer. See Geoffrey Hinton's 'Improving
    neural networks by preventing co-adaptation of feature
    detectors'.
    
    The [`size`][019f] of this lump is the size of its input which is determined
    automatically.

<a id="x-28MGL-BP-3ADROPOUT-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EDROPOUT-29-29"></a>
<a id="MGL-BP:DROPOUT%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EDROPOUT%29"></a>

- [accessor] **dropout** *[->dropout][441b] (:dropout = 0.5)*

    If non-`nil`, then in the forward pass zero out each
    node in this chunk with `dropout` probability.

<a id="x-28MGL-BP-3A-40MGL-BP-GAUSSIAN-RANDOM-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-GAUSSIAN-RANDOM-LUMP%20MGL-PAX:SECTION"></a>

##### Gaussian Random Lump

<a id="x-28MGL-BP-3A--3EGAUSSIAN-RANDOM-20CLASS-29"></a>
<a id="MGL-BP:-%3EGAUSSIAN-RANDOM%20CLASS"></a>

- [class] **->gaussian-random** *[lump][c1ac]*

    This lump has no input, it produces normally
    distributed independent random numbers with [`mean`][d96a] and [`variance`][404c] (or
    [`variance-for-prediction`][80e2]). This is useful building block for noise
    based regularization methods.
    
    ```common-lisp
    (->gaussian-random :size 10 :name 'normal :mean 1 :variance 2)
    ==> #<->GAUSSIAN-RANDOM NORMAL :SIZE 10 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-BP-3AMEAN-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EGAUSSIAN-RANDOM-29-29"></a>
<a id="MGL-BP:MEAN%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29"></a>

- [accessor] **mean** *[->gaussian-random][feaa] (:mean = 0)*

    The mean of the normal distribution.

<a id="x-28MGL-BP-3AVARIANCE-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EGAUSSIAN-RANDOM-29-29"></a>
<a id="MGL-BP:VARIANCE%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29"></a>

- [accessor] **variance** *[->gaussian-random][feaa] (:variance = 1)*

    The variance of the normal distribution.

<a id="x-28MGL-BP-3AVARIANCE-FOR-PREDICTION-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3EGAUSSIAN-RANDOM-29-29"></a>
<a id="MGL-BP:VARIANCE-FOR-PREDICTION%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29"></a>

- [accessor] **variance-for-prediction** *[->gaussian-random][feaa] (:variance-for-prediction = 0)*

    If not `nil`, then this value overrides [`variance`][404c]
    when not in training (i.e. when making predictions).

<a id="x-28MGL-BP-3A-40MGL-BP-SAMPLE-BINARY-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SAMPLE-BINARY-LUMP%20MGL-PAX:SECTION"></a>

##### Binary Sampling Lump

<a id="x-28MGL-BP-3A--3ESAMPLE-BINARY-20CLASS-29"></a>
<a id="MGL-BP:-%3ESAMPLE-BINARY%20CLASS"></a>

- [class] **->sample-binary** *[lump][c1ac]*

    Treating values of its input as probabilities,
    sample independent binomials. Turn true into 1 and false into 0. The
    [`size`][019f] of this lump is determined automatically from the size of its
    input.
    
    ```common-lisp
    (->sample-binary (->input :size 10) :name 'binarized-input)
    ==> #<->SAMPLE-BINARY BINARIZED-INPUT :SIZE 10 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-BP-3A-40MGL-BP-ARITHMETIC-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-ARITHMETIC%20MGL-PAX:SECTION"></a>

#### 11.4.8 Arithmetic

<a id="x-28MGL-BP-3A-40MGL-BP-SUM-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SUM-LUMP%20MGL-PAX:SECTION"></a>

##### Sum Lump

<a id="x-28MGL-BP-3A--3ESUM-20CLASS-29"></a>
<a id="MGL-BP:-%3ESUM%20CLASS"></a>

- [class] **->sum** *[lump][c1ac]*

    Computes the sum of all nodes of its input per
    stripe. This [`size`][019f] of this lump is always 1.

<a id="x-28MGL-BP-3A-40MGL-BP-V-2AM-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-V*M-LUMP%20MGL-PAX:SECTION"></a>

##### Vector-Matrix Multiplication Lump

<a id="x-28MGL-BP-3A--3EV-2AM-20CLASS-29"></a>
<a id="MGL-BP:-%3EV*M%20CLASS"></a>

- [class] **->v\*m** *[lump][c1ac]*

    Perform `x * weights` where `x` (the input) is of
    size `m` and [`weights`][ab3c] is a [`->weight`][b76f] whose single stripe is taken to
    be of dimensions `M x N` stored in row major order. `n` is the size
    of this lump. If [`transpose-weights-p`][533e] then `weights` is `N x M` and `x
    * weights'` is computed.

<a id="x-28MGL-COMMON-3AWEIGHTS-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EV-2AM-29-29"></a>
<a id="MGL-COMMON:WEIGHTS%20%28MGL-PAX:READER%20MGL-BP:-%3EV*M%29"></a>

- [reader] **weights** *[->v\*m][dbc4] (:weights)*

    A [`->weight`][b76f] lump.

<a id="x-28MGL-BP-3ATRANSPOSE-WEIGHTS-P-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3EV-2AM-29-29"></a>
<a id="MGL-BP:TRANSPOSE-WEIGHTS-P%20%28MGL-PAX:READER%20MGL-BP:-%3EV*M%29"></a>

- [reader] **transpose-weights-p** *[->v\*m][dbc4] (:transpose-weights-p = nil)*

    Determines whether the input is multiplied by
    [`weights`][ab3c] or its transpose.

<a id="x-28MGL-BP-3A-40MGL-BP--2B-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-+-LUMP%20MGL-PAX:SECTION"></a>

##### Elementwise Addition Lump

<a id="x-28MGL-BP-3A--3E-2B-20CLASS-29"></a>
<a id="MGL-BP:-%3E+%20CLASS"></a>

- [class] **->+** *[lump][c1ac]*

    Performs elementwise addition on its input lumps.
    The [`size`][019f] of this lump is automatically determined from the size of
    its inputs if there is at least one. If one of the inputs is a
    [`->weight`][b76f] lump, then it is added to every stripe.
    
    ```common-lisp
    (->+ (list (->input :size 10) (->weight :size 10 :name 'bias))
         :name 'plus)
    ==> #<->+ PLUS :SIZE 10 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-BP-3A-40MGL-BP--2A-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-*-LUMP%20MGL-PAX:SECTION"></a>

##### Elementwise Multiplication Lump

<a id="x-28MGL-BP-3A--3E-2A-20CLASS-29"></a>
<a id="MGL-BP:-%3E*%20CLASS"></a>

- [class] **->\*** *[lump][c1ac]*

    Performs elementwise multiplication on its two
    input lumps. The [`size`][019f] of this lump is automatically determined from
    the size of its inputs. Either input can be a [`->weight`][b76f] lump.
    
    ```common-lisp
    (->* (->input :size 10) (->weight :size 10 :name 'scale)
         :name 'mult)
    ==> #<->* MULT :SIZE 10 1/1 :NORM 0.00000>
    ```

<a id="x-28MGL-BP-3A-40MGL-BP-ABS-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-ABS-LUMP%20MGL-PAX:SECTION"></a>

##### Abs Lump

<a id="x-28MGL-BP-3A--3EABS-20CLASS-29"></a>
<a id="MGL-BP:-%3EABS%20CLASS"></a>

- [class] **->abs** *[lump][c1ac]*

<a id="x-28MGL-BP-3A-40MGL-BP-EXP-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-EXP-LUMP%20MGL-PAX:SECTION"></a>

##### Exp Lump

<a id="x-28MGL-BP-3A--3EEXP-20CLASS-29"></a>
<a id="MGL-BP:-%3EEXP%20CLASS"></a>

- [class] **->exp** *[lump][c1ac]*

<a id="x-28MGL-BP-3A-40MGL-BP-NORMALIZED-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-NORMALIZED-LUMP%20MGL-PAX:SECTION"></a>

##### Normalized Lump

<a id="x-28MGL-BP-3A--3ENORMALIZED-20CLASS-29"></a>
<a id="MGL-BP:-%3ENORMALIZED%20CLASS"></a>

- [class] **->normalized** *[lump][c1ac]*

<a id="x-28MGL-BP-3A-40MGL-BP-SINE-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SINE-LUMP%20MGL-PAX:SECTION"></a>

##### Sine Lump

<a id="x-28MGL-BP-3A--3ESIN-20CLASS-29"></a>
<a id="MGL-BP:-%3ESIN%20CLASS"></a>

- [class] **->sin** *[lump][c1ac]*

    Applies the [`sin`][ece2] function to its input in an
    elementwise manner. The [`size`][019f] of this lump is the size of its input
    which is determined automatically.

<a id="x-28MGL-BP-3A-40MGL-BP-RNN-OPERATIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-RNN-OPERATIONS%20MGL-PAX:SECTION"></a>

#### 11.4.9 Operations for `rnn`s

<a id="x-28MGL-BP-3A-40MGL-BP-LSTM-SUBNET-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-LSTM-SUBNET%20MGL-PAX:SECTION"></a>

##### LSTM Subnet

<a id="x-28MGL-BP-3A--3ELSTM-20CLASS-29"></a>
<a id="MGL-BP:-%3ELSTM%20CLASS"></a>

- [class] **->lstm** *[bpn][5187]*

    Long-Short Term Memory subnetworks are built by the
    function [`->lstm`][2823] and they have many lumps hidden inside them. These
    lumps are packaged into a subnetwork to reduce clutter.

<a id="x-28MGL-BP-3A--3ELSTM-20FUNCTION-29"></a>
<a id="MGL-BP:-%3ELSTM%20FUNCTION"></a>

- [function] **->lstm** *inputs &key name cell-init output-init size (activation-fn '->activation) (gate-fn '->sigmoid) (input-fn '->tanh) (output-fn '->tanh) (peepholes t)*

    Create an LSTM layer consisting of input, forget, output gates with
    which input, cell state and output are scaled. Lots of lumps are
    created, the final one representing to output of the LSTM has `name`.
    The rest of the lumps are named automatically based on `name`. This
    function returns only the output lump (`m`), but all created lumps
    are added automatically to the [`bpn`][5187] being built.
    
    There are many papers and tutorials on LSTMs. This version is well
    described in "Long Short-Term Memory Recurrent Neural Network
    Architectures for Large Scale Acoustic Modeling" (2014, Hasim Sak,
    Andrew Senior, Francoise Beaufays). Using the notation from that
    paper:
    
    $$
    i_t = s(W\_{ix} x\_t + W\_{im} m\_{t-1} + W\_{ic} \odot
    c\_{t-1} + b\_i)
    $$
    
    $$
    f\_t = s(W\_{fx} x\_t + W\_{fm} m\_{t-1} + W\_{fc} \odot
    c\_{t-1} + b\_f)
    $$
    
    $$
    c\_t = f\_t \odot c\_{t-1} + i\_t \odot g(W\_{cx} x\_t +
    W\_{cm} m\_{t-1} + b\_c)
    $$
    
    $$
    o\_t = s(W\_{ox} x\_t + W\_{om} m\_{t-1} + W\_{oc} \odot
    c\_t + b\_o)
    $$
    
    $$
    m\_t = o\_t \odot h(c\_t),
    $$
    
    where `i`, `f`, and `o` are the input, forget and output gates. `c`
    is the cell state and `m` is the actual output.
    
    Weight matrices for connections from `c` (`W_ic`, `W_fc` and `W_oc`)
    are diagonal and represented by just the vector of diagonal values.
    These connections are only added if `peepholes` is true.
    
    A notable difference from the paper is that in addition to being a
    single lump, `x_t` (`inputs`) can also be a list of lumps. Whenever
    some activation is to be calculated based on `x_t`, it is going to
    be the sum of individual activations. For example, `W_ix * x_t` is
    really `sum_j W_ijx * inputs_j`.
    
    If `cell-init` is non-`nil`, then it must be a [`clump`][a4fe] of `size` form which
    stands for the initial state of the value cell (`c_{-1}`). `cell-init`
    being `nil` is equivalent to the state of all zeros.
    
    `activation-fn` defaults to `->activation`([`0`][7162] [`1`][b602]), but it can be for example
    [`->batch-normalized-activation`][0f0f]. In general, functions like the
    aforementioned two with signature like (`inputs` [`&key`][4336] `name` `size`
    `peepholes`) can be passed as `activation-fn`.

<a id="x-28MGL-BP-3A-40MGL-BP-SEQ-BARRIER-LUMP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-SEQ-BARRIER-LUMP%20MGL-PAX:SECTION"></a>

##### Sequence Barrier Lump

<a id="x-28MGL-BP-3A--3ESEQ-BARRIER-20CLASS-29"></a>
<a id="MGL-BP:-%3ESEQ-BARRIER%20CLASS"></a>

- [class] **->seq-barrier** *[lump][c1ac]*

    In an [`rnn`][b0f3], processing of stripes (instances in the
    batch) may require different number of time step so the final state
    for stripe 0 is in stripe 0 of some lump L at time step 7, while for
    stripe 1 it is in stripe 1 of sump lump L at time step 42.
    
    This lump copies the per-stripe states from different lumps into a
    single lump so that further processing can take place (typically
    when the `rnn` is embedded in another network).
    
    The [`size`][019f] of this lump is automatically set to the size of the lump
    returned by `(funcall seq-elt-fn 0)`.

<a id="x-28MGL-BP-3ASEQ-ELT-FN-20-28MGL-PAX-3AREADER-20MGL-BP-3A--3ESEQ-BARRIER-29-29"></a>
<a id="MGL-BP:SEQ-ELT-FN%20%28MGL-PAX:READER%20MGL-BP:-%3ESEQ-BARRIER%29"></a>

- [reader] **seq-elt-fn** *[->seq-barrier][4e91] (:seq-elt-fn)*

    A function of an `index` argument that returns the
    lump with that index in some sequence.

<a id="x-28MGL-BP-3ASEQ-INDICES-20-28MGL-PAX-3AACCESSOR-20MGL-BP-3A--3ESEQ-BARRIER-29-29"></a>
<a id="MGL-BP:SEQ-INDICES%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ESEQ-BARRIER%29"></a>

- [accessor] **seq-indices** *[->seq-barrier][4e91]*

    A sequence of length batch size of indices. The
    element at index `i` is the index to be passed to [`seq-elt-fn`][29c0] to
    find the lump whose stripe `i` is copied to stripe `i` of this
    this lump.

<a id="x-28MGL-BP-3A-40MGL-BP-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-BP:@MGL-BP-UTILITIES%20MGL-PAX:SECTION"></a>

### 11.5 Utilities

<a id="x-28MGL-BP-3ARENORMALIZE-ACTIVATIONS-20FUNCTION-29"></a>
<a id="MGL-BP:RENORMALIZE-ACTIVATIONS%20FUNCTION"></a>

- [function] **renormalize-activations** *->v\*m-lumps l2-upper-bound*

    If the l2 norm of the incoming weight vector of the a unit is
    larger than `l2-upper-bound` then renormalize it to `l2-upper-bound`.
    The list of `->v*m-lumps` is assumed to be eventually fed to the same
    lump.
    
    To use it, group the activation clumps into the same GD-OPTIMIZER
    and hang this function on [`after-update-hook`][124f], that latter of which is
    done for you [`arrange-for-renormalizing-activations`][8b55].
    
    See "Improving neural networks by preventing co-adaptation of
    feature detectors (Hinton, 2012)",
    <http://arxiv.org/pdf/1207.0580.pdf>.

<a id="x-28MGL-BP-3AARRANGE-FOR-RENORMALIZING-ACTIVATIONS-20FUNCTION-29"></a>
<a id="MGL-BP:ARRANGE-FOR-RENORMALIZING-ACTIVATIONS%20FUNCTION"></a>

- [function] **arrange-for-renormalizing-activations** *bpn optimizer l2-upper-bound*

    By pushing a lambda to [`after-update-hook`][124f] of `optimizer` arrange for
    all weights beings trained by `optimizer` to be renormalized (as in
    [`renormalize-activations`][c7fa] with `l2-upper-bound`).
    
    It is assumed that if the weights either belong to an activation
    lump or are simply added to the activations (i.e. they are biases).

<a id="x-28MGL-3A-40MGL-BM-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-BM%20MGL-PAX:SECTION"></a>

## 12 Boltzmann Machines


<a id="x-28MGL-3A-40MGL-GP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL:@MGL-GP%20MGL-PAX:SECTION"></a>

## 13 Gaussian Processes


<a id="x-28MGL-NLP-3A-40MGL-NLP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-NLP:@MGL-NLP%20MGL-PAX:SECTION"></a>

## 14 Natural Language Processing

###### \[in package MGL-NLP\]
This in nothing more then a couple of utilities for now which may
grow into a more serious toolset for NLP eventually.

<a id="x-28MGL-NLP-3AMAKE-N-GRAM-MAPPEE-20FUNCTION-29"></a>
<a id="MGL-NLP:MAKE-N-GRAM-MAPPEE%20FUNCTION"></a>

- [function] **make-n-gram-mappee** *function n*

    Make a function of a single argument that's suitable as the
    function argument to a mapper function. It calls `function` with every
    `n` element.
    
    ```common-lisp
    (map nil (make-n-gram-mappee #'print 3) '(a b c d e))
    ..
    .. (A B C) 
    .. (B C D) 
    .. (C D E) 
    ```

<a id="x-28MGL-NLP-3ABLEU-20FUNCTION-29"></a>
<a id="MGL-NLP:BLEU%20FUNCTION"></a>

- [function] **bleu** *candidates references &key candidate-key reference-key (n 4)*

    Compute the [BLEU score](http://en.wikipedia.org/wiki/BLEU) for
    bilingual CORPUS. BLEU measures how good a translation is compared
    to human reference translations.
    
    `candidates` (keyed by `candidate-key`) and `references` (keyed by
    `reference-key`) are sequences of sentences. A sentence is a sequence
    of words. Words are compared with [`equal`][3fb5], and may be any kind of
    object (not necessarily strings).
    
    Currently there is no support for multiple reference translations. `n`
    determines the largest n-grams to consider.
    
    The first return value is the `bleu` score (between 0 and 1, not as a
    percentage). The second value is the sum of the lengths of
    `candidates` divided by the sum of the lengths of `references` (or `nil`,
    if the denominator is 0). The third is a list of n-gram
    precisions (also between 0 and 1 or `nil`), one for each element in
    \[1..`n`\].
    
    This is basically a reimplementation of
    [multi-bleu.perl](https://github.com/moses-smt/mosesdecoder/blob/master/scripts/generic/multi-bleu.perl).
    
    ```common-lisp
    (bleu '((1 2 3 4) (a b))
          '((1 2 3 4) (1 2)))
    => 0.8408964
    => 1
    => (;; 1-gram precision: 4/6
        2/3
        ;; 2-gram precision: 3/4
        3/4
        ;; 3-gram precision: 2/2
        1
        ;; 4-gram precision: 1/1
        1)
    ```

<a id="x-28MGL-NLP-3A-40MGL-NLP-BAG-OF-WORDS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-NLP:@MGL-NLP-BAG-OF-WORDS%20MGL-PAX:SECTION"></a>

### 14.1 Bag of Words

<a id="x-28MGL-NLP-3ABAG-OF-WORDS-ENCODER-20CLASS-29"></a>
<a id="MGL-NLP:BAG-OF-WORDS-ENCODER%20CLASS"></a>

- [class] **bag-of-words-encoder**

    [`encode`][fedd] all features of a document with a sparse
    vector. Get the features of document from `mapper`, encode each
    feature with [`feature-encoder`][96d0]. `feature-encoder` may return `nil` if the
    feature is not used. The result is a vector of encoded-feature/value
    conses. encoded-features are unique (under [`encoded-feature-test`][21ca])
    within the vector but are in no particular order.
    
    Depending on `kind`, value is calculated in various ways:
    
    - For `:frequency` it is the number of times the corresponding feature
    was found in [`document`][432c].
    
    - For `:binary` it is always 1.
    
    - For `:normalized-frequency` and `:normalized-binary` are like the
      unnormalized counterparts except that as the final step values in
      the assembled sparse vector are normalized to sum to 1.
    
    - Finally, `:compacted-binary` is like `:binary` but the return values
      is not a vector of conses, but a vector of element-type
      [`encoded-feature-type`][d443].
    
    ```common-lisp
    (let* ((feature-indexer
             (make-indexer
              (alexandria:alist-hash-table '(("I" . 3) ("me" . 2) ("mine" . 1)))
              2))
           (bag-of-words-encoder
             (make-instance 'bag-of-words-encoder
                            :feature-encoder feature-indexer
                            :feature-mapper (lambda (fn document)
                                              (map nil fn document))
                            :kind :frequency)))
      (encode bag-of-words-encoder '("All" "through" "day" "I" "me" "mine"
                                     "I" "me" "mine" "I" "me" "mine")))
    => #((0 . 3.0d0) (1 . 3.0d0))
    ```

<a id="x-28MGL-NLP-3AFEATURE-ENCODER-20-28MGL-PAX-3AREADER-20MGL-NLP-3ABAG-OF-WORDS-ENCODER-29-29"></a>
<a id="MGL-NLP:FEATURE-ENCODER%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29"></a>

- [reader] **feature-encoder** *[bag-of-words-encoder][cbb4] (:feature-encoder)*

<a id="x-28MGL-NLP-3AFEATURE-MAPPER-20-28MGL-PAX-3AREADER-20MGL-NLP-3ABAG-OF-WORDS-ENCODER-29-29"></a>
<a id="MGL-NLP:FEATURE-MAPPER%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29"></a>

- [reader] **feature-mapper** *[bag-of-words-encoder][cbb4] (:feature-mapper)*

<a id="x-28MGL-NLP-3AENCODED-FEATURE-TEST-20-28MGL-PAX-3AREADER-20MGL-NLP-3ABAG-OF-WORDS-ENCODER-29-29"></a>
<a id="MGL-NLP:ENCODED-FEATURE-TEST%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29"></a>

- [reader] **encoded-feature-test** *[bag-of-words-encoder][cbb4] (:encoded-feature-test = #'eql)*

<a id="x-28MGL-NLP-3AENCODED-FEATURE-TYPE-20-28MGL-PAX-3AREADER-20MGL-NLP-3ABAG-OF-WORDS-ENCODER-29-29"></a>
<a id="MGL-NLP:ENCODED-FEATURE-TYPE%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29"></a>

- [reader] **encoded-feature-type** *[bag-of-words-encoder][cbb4] (:encoded-feature-type = t)*

<a id="x-28MGL-NLP-3ABAG-OF-WORDS-KIND-20-28MGL-PAX-3AREADER-20MGL-NLP-3ABAG-OF-WORDS-ENCODER-29-29"></a>
<a id="MGL-NLP:BAG-OF-WORDS-KIND%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29"></a>

- [reader] **bag-of-words-kind** *[bag-of-words-encoder][cbb4] (:kind = :binary)*

<a id="x-28MGL-LOG-3A-40MGL-LOG-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-LOG:@MGL-LOG%20MGL-PAX:SECTION"></a>

## 15 Logging

###### \[in package MGL-LOG\]
<a id="x-28MGL-LOG-3ALOG-MSG-20FUNCTION-29"></a>
<a id="MGL-LOG:LOG-MSG%20FUNCTION"></a>

- [function] **log-msg** *format &rest args*

<a id="x-28MGL-LOG-3AWITH-LOGGING-ENTRY-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-LOG:WITH-LOGGING-ENTRY%20MGL-PAX:MACRO"></a>

- [macro] **with-logging-entry** *(stream) &body body*

<a id="x-28MGL-LOG-3A-2ALOG-FILE-2A-20VARIABLE-29"></a>
<a id="MGL-LOG:*LOG-FILE*%20VARIABLE"></a>

- [variable] **\*log-file\*** *nil*

<a id="x-28MGL-LOG-3A-2ALOG-TIME-2A-20VARIABLE-29"></a>
<a id="MGL-LOG:*LOG-TIME*%20VARIABLE"></a>

- [variable] **\*log-time\*** *t*

<a id="x-28MGL-LOG-3ALOG-MAT-ROOM-20FUNCTION-29"></a>
<a id="MGL-LOG:LOG-MAT-ROOM%20FUNCTION"></a>

- [function] **log-mat-room** *&key (verbose t)*

  [0072]: #MGL-OPT:ON-OPTIMIZATION-FINISHED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29 "MGL-OPT:ON-OPTIMIZATION-FINISHED (MGL-PAX:ACCESSOR MGL-OPT:ITERATIVE-OPTIMIZER)"
  [0078]: #MGL-CORE:INSTANCE-TO-EXECUTOR-PARAMETERS%20GENERIC-FUNCTION "MGL-CORE:INSTANCE-TO-EXECUTOR-PARAMETERS GENERIC-FUNCTION"
  [00a0]: #MGL-BP:BP-LEARNER%20CLASS "MGL-BP:BP-LEARNER CLASS"
  [00ee]: #MGL:@MGL-LINKS%20MGL-PAX:SECTION "Links"
  [011d]: #MGL-GD:MEAN-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29 "MGL-GD:MEAN-DECAY (MGL-PAX:ACCESSOR MGL-GD:ADAM-OPTIMIZER)"
  [019f]: #MGL-COMMON:SIZE%20GENERIC-FUNCTION "MGL-COMMON:SIZE GENERIC-FUNCTION"
  [038e]: #MGL-BP:IMPORTANCE%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ELOSS%29 "MGL-BP:IMPORTANCE (MGL-PAX:ACCESSOR MGL-BP:->LOSS)"
  [03c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL (MGL-PAX:CLHS FUNCTION)"
  [0784]: #MGL-NLP:@MGL-NLP-BAG-OF-WORDS%20MGL-PAX:SECTION "Bag of Words"
  [07c7]: #MGL-CORE:@MGL-CONFUSION-MATRIX%20MGL-PAX:SECTION "Confusion Matrices"
  [07fb]: #MGL-CORE:N-STRIPES%20%28MGL-PAX:READER%20MGL-BP:BPN%29 "MGL-CORE:N-STRIPES (MGL-PAX:READER MGL-BP:BPN)"
  [084d]: #MGL-BP:MAX-LAG%20%28MGL-PAX:READER%20MGL-BP:RNN%29 "MGL-BP:MAX-LAG (MGL-PAX:READER MGL-BP:RNN)"
  [08ac]: #MGL-DATASET:GENERATOR%20%28MGL-PAX:READER%20MGL-DATASET:FUNCTION-SAMPLER%29 "MGL-DATASET:GENERATOR (MGL-PAX:READER MGL-DATASET:FUNCTION-SAMPLER)"
  [0900]: #MGL-GD:VARIANCE-DECAY%20%28MGL-PAX:ACCESSOR%20MGL-GD:ADAM-OPTIMIZER%29 "MGL-GD:VARIANCE-DECAY (MGL-PAX:ACCESSOR MGL-GD:ADAM-OPTIMIZER)"
  [0933]: #MGL-BP:MONITOR-BPN-RESULTS%20FUNCTION "MGL-BP:MONITOR-BPN-RESULTS FUNCTION"
  [09ed]: #MGL-GD:LEARNING-RATE%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29 "MGL-GD:LEARNING-RATE (MGL-PAX:ACCESSOR MGL-GD::GD-OPTIMIZER)"
  [0ba7]: #MGL-CORE:@MGL-CLASSIFICATION-MEASURER%20MGL-PAX:SECTION "Classification Measurers"
  [0c91]: #MGL-GD:@MGL-GD-NORMALIZED-BATCH-GD-OPTIMIZER%20MGL-PAX:SECTION "Normalized Batch Optimizer"
  [0c9e]: #MGL-CORE:SET-INPUT%20GENERIC-FUNCTION "MGL-CORE:SET-INPUT GENERIC-FUNCTION"
  [0d6a]: #MGL-NLP:@MGL-NLP%20MGL-PAX:SECTION "Natural Language Processing"
  [0d82]: #MGL-BP:@MGL-BP-TRAINING%20MGL-PAX:SECTION "Training"
  [0f0f]: #MGL-BP:-%3EBATCH-NORMALIZED-ACTIVATION%20FUNCTION "MGL-BP:->BATCH-NORMALIZED-ACTIVATION FUNCTION"
  [0f83]: #MGL-CORE:CONCAT-COUNTER%20CLASS "MGL-CORE:CONCAT-COUNTER CLASS"
  [109e]: #MGL-DATASET:@MGL-DATASET%20MGL-PAX:SECTION "Datasets"
  [10e7]: #MGL-GD:@MGL-GD%20MGL-PAX:SECTION "Gradient Descent"
  [115e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_max_m.htm "MIN (MGL-PAX:CLHS FUNCTION)"
  [124f]: #MGL-GD:AFTER-UPDATE-HOOK%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29 "MGL-GD:AFTER-UPDATE-HOOK (MGL-PAX:ACCESSOR MGL-GD::GD-OPTIMIZER)"
  [1339]: #MGL-CORE:DECODE%20GENERIC-FUNCTION "MGL-CORE:DECODE GENERIC-FUNCTION"
  [1355]: #MGL-BP:@MGL-FNN%20MGL-PAX:SECTION "Feed-Forward Nets"
  [16c4]: #MGL-CORE:MAX-N-STRIPES%20GENERIC-FUNCTION "MGL-CORE:MAX-N-STRIPES GENERIC-FUNCTION"
  [175f]: #MGL-BP:FIND-CLUMP%20FUNCTION "MGL-BP:FIND-CLUMP FUNCTION"
  [1a52]: #MGL-BP:INPUT-ROW-INDICES%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EEMBEDDING%29 "MGL-BP:INPUT-ROW-INDICES (MGL-PAX:ACCESSOR MGL-BP:->EMBEDDING)"
  [1a61]: #MGL-DIFFUN:DIFFUN%20CLASS "MGL-DIFFUN:DIFFUN CLASS"
  [1b5e]: #MGL-CORE:@MGL-FEATURE-SELECTION%20MGL-PAX:SECTION "Feature Selection"
  [1beb]: #MGL-CORE:ENCODER%2FDECODER%20CLASS "MGL-CORE:ENCODER/DECODER CLASS"
  [1cab]: #MGL-DATASET:MAX-N-SAMPLES%20%28MGL-PAX:ACCESSOR%20MGL-DATASET:FUNCTION-SAMPLER%29 "MGL-DATASET:MAX-N-SAMPLES (MGL-PAX:ACCESSOR MGL-DATASET:FUNCTION-SAMPLER)"
  [207b]: #MGL-BP:@MGL-BP-INPUTS%20MGL-PAX:SECTION "Inputs"
  [20ca]: #MGL-OPT:DO-GRADIENT-SINK%20MGL-PAX:MACRO "MGL-OPT:DO-GRADIENT-SINK MGL-PAX:MACRO"
  [20e8]: #MGL-CORE:COUNTER-VALUES%20GENERIC-FUNCTION "MGL-CORE:COUNTER-VALUES GENERIC-FUNCTION"
  [2171]: #MGL-BP:-%3ELOSS%20CLASS "MGL-BP:->LOSS CLASS"
  [21ca]: #MGL-NLP:ENCODED-FEATURE-TEST%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29 "MGL-NLP:ENCODED-FEATURE-TEST (MGL-PAX:READER MGL-NLP:BAG-OF-WORDS-ENCODER)"
  [2312]: #MGL-OPT:MAP-SEGMENTS%20GENERIC-FUNCTION "MGL-OPT:MAP-SEGMENTS GENERIC-FUNCTION"
  [2481]: #MGL-BP:DROPOUT%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EDROPOUT%29 "MGL-BP:DROPOUT (MGL-PAX:ACCESSOR MGL-BP:->DROPOUT)"
  [24aa]: #MGL-CORE:@MGL-FEATURE-ENCODING%20MGL-PAX:SECTION "Feature Encoding"
  [25fd]: #MGL-GD:@MGL-GD-SGD-OPTIMIZER%20MGL-PAX:SECTION "SGD Optimizer"
  [2823]: #MGL-BP:-%3ELSTM%20FUNCTION "MGL-BP:->LSTM FUNCTION"
  [2981]: #MGL-DIFFUN:@MGL-DIFFUN%20MGL-PAX:SECTION "Differentiable Functions"
  [29a1]: #MGL-CORE:@MGL-PERSISTENCE%20MGL-PAX:SECTION "Persistence"
  [29c0]: #MGL-BP:SEQ-ELT-FN%20%28MGL-PAX:READER%20MGL-BP:-%3ESEQ-BARRIER%29 "MGL-BP:SEQ-ELT-FN (MGL-PAX:READER MGL-BP:->SEQ-BARRIER)"
  [2a2f]: #MGL-GD:SGD-OPTIMIZER%20CLASS "MGL-GD:SGD-OPTIMIZER CLASS"
  [2aa3]: #MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS*%20GENERIC-FUNCTION "MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS* GENERIC-FUNCTION"
  [2c39]: #MGL-GD:@MGL-GD-BATCH-GD-OPTIMIZER%20MGL-PAX:SECTION "Batch Based Optimizers"
  [2e8b]: #MGL-CORE:WITH-PADDED-ATTRIBUTE-PRINTING%20MGL-PAX:MACRO "MGL-CORE:WITH-PADDED-ATTRIBUTE-PRINTING MGL-PAX:MACRO"
  [2ecb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_concat.htm "CONCATENATE (MGL-PAX:CLHS FUNCTION)"
  [2f78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_length.htm "LENGTH (MGL-PAX:CLHS FUNCTION)"
  [2fe9]: #MGL-BP:@MGL-BP-ARITHMETIC%20MGL-PAX:SECTION "Arithmetic"
  [3045]: #MGL-BP:@MGL-BP-LUMP%20MGL-PAX:SECTION "Lump Base Class"
  [3155]: http://www.lispworks.com/documentation/HyperSpec/Body/f_countc.htm "COUNT (MGL-PAX:CLHS FUNCTION)"
  [31ed]: #MGL-CORE:LABEL-INDICES%20GENERIC-FUNCTION "MGL-CORE:LABEL-INDICES GENERIC-FUNCTION"
  [331b]: #MGL-CORE:MAKE-EXECUTOR-WITH-PARAMETERS%20GENERIC-FUNCTION "MGL-CORE:MAKE-EXECUTOR-WITH-PARAMETERS GENERIC-FUNCTION"
  [332e]: #MGL:@MGL-BM%20MGL-PAX:SECTION "Boltzmann Machines"
  [3815]: #MGL-OPT:MAKE-COST-MONITORS*%20GENERIC-FUNCTION "MGL-OPT:MAKE-COST-MONITORS* GENERIC-FUNCTION"
  [38a2]: http://www.lispworks.com/documentation/HyperSpec/Body/a_eql.htm "EQL (MGL-PAX:CLHS NIL)"
  [3ce0]: #MGL-GD:SEGMENTED-GD-OPTIMIZER%20CLASS "MGL-GD:SEGMENTED-GD-OPTIMIZER CLASS"
  [3db3]: mat-manual.md#MGL-MAT:WITH-CUDA*%20MGL-PAX:MACRO "MGL-MAT:WITH-CUDA* MGL-PAX:MACRO"
  [3f2e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [3f42]: #MGL-LOG:@MGL-LOG%20MGL-PAX:SECTION "Logging"
  [3f9f]: #MGL-RESAMPLE:@MGL-RESAMPLE-CV-BAGGING%20MGL-PAX:SECTION "CV Bagging"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [401f]: #MGL-DATASET:FINISHEDP%20GENERIC-FUNCTION "MGL-DATASET:FINISHEDP GENERIC-FUNCTION"
  [404c]: #MGL-BP:VARIANCE%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29 "MGL-BP:VARIANCE (MGL-PAX:ACCESSOR MGL-BP:->GAUSSIAN-RANDOM)"
  [410c]: #MGL-COMMON:COST%20GENERIC-FUNCTION "MGL-COMMON:COST GENERIC-FUNCTION"
  [418a]: #MGL-OPT:SEGMENT-SET%20CLASS "MGL-OPT:SEGMENT-SET CLASS"
  [430d]: #MGL-CORE:CLASSIFICATION-ACCURACY-COUNTER%20CLASS "MGL-CORE:CLASSIFICATION-ACCURACY-COUNTER CLASS"
  [432c]: pax-manual.md#MGL-PAX:DOCUMENT%20FUNCTION "MGL-PAX:DOCUMENT FUNCTION"
  [4336]: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm "\"3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [441b]: #MGL-BP:-%3EDROPOUT%20CLASS "MGL-BP:->DROPOUT CLASS"
  [443c]: #MGL:@MGL-CODE-ORGANIZATION%20MGL-PAX:SECTION "Code Organization"
  [4476]: #MGL-CORE:@MGL-EXECUTORS%20MGL-PAX:SECTION "Executors"
  [4528]: #MGL-OPT:MONITOR-OPTIMIZATION-PERIODICALLY%20FUNCTION "MGL-OPT:MONITOR-OPTIMIZATION-PERIODICALLY FUNCTION"
  [46a4]: #MGL-OPT:MINIMIZE%20FUNCTION "MGL-OPT:MINIMIZE FUNCTION"
  [46c2]: #MGL-OPT:MAKE-COST-MONITORS%20FUNCTION "MGL-OPT:MAKE-COST-MONITORS FUNCTION"
  [46c4]: #MGL-BP:POPULATION-DECAY%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29 "MGL-BP:POPULATION-DECAY (MGL-PAX:READER MGL-BP:->BATCH-NORMALIZATION)"
  [49f5]: http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm "LET* (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4a8e]: #MGL:@MGL-GLOSSARY%20MGL-PAX:SECTION "Glossary"
  [4bf1]: #MGL-OPT:ACCUMULATE-GRADIENTS*%20GENERIC-FUNCTION "MGL-OPT:ACCUMULATE-GRADIENTS* GENERIC-FUNCTION"
  [4c73]: #MGL-OPT:N-INSTANCES%20%28MGL-PAX:READER%20MGL-OPT:ITERATIVE-OPTIMIZER%29 "MGL-OPT:N-INSTANCES (MGL-PAX:READER MGL-OPT:ITERATIVE-OPTIMIZER)"
  [4e91]: #MGL-BP:-%3ESEQ-BARRIER%20CLASS "MGL-BP:->SEQ-BARRIER CLASS"
  [4f0b]: #MGL-OPT:ON-N-INSTANCES-CHANGED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29 "MGL-OPT:ON-N-INSTANCES-CHANGED (MGL-PAX:ACCESSOR MGL-OPT:ITERATIVE-OPTIMIZER)"
  [4f0e]: #MGL-BP:@MGL-BP-MONITORING%20MGL-PAX:SECTION "Monitoring"
  [4ffb]: #MGL-CG:CG%20FUNCTION "MGL-CG:CG FUNCTION"
  [5187]: #MGL-BP:BPN%20CLASS "MGL-BP:BPN CLASS"
  [51d5]: #MGL-BP:WARP-LENGTH%20%28MGL-PAX:READER%20MGL-BP:RNN%29 "MGL-BP:WARP-LENGTH (MGL-PAX:READER MGL-BP:RNN)"
  [51f7]: #MGL-BP:@MGL-BP-RNN-OPERATIONS%20MGL-PAX:SECTION "Operations for `rnn`s"
  [5293]: #MGL-RESAMPLE:SPLIT-FOLD%2FCONT%20FUNCTION "MGL-RESAMPLE:SPLIT-FOLD/CONT FUNCTION"
  [5309]: #MGL-BP:-%3ETANH%20CLASS "MGL-BP:->TANH CLASS"
  [533e]: #MGL-BP:TRANSPOSE-WEIGHTS-P%20%28MGL-PAX:READER%20MGL-BP:-%3EV*M%29 "MGL-BP:TRANSPOSE-WEIGHTS-P (MGL-PAX:READER MGL-BP:->V*M)"
  [5611]: #MGL-GD:MOMENTUM-TYPE%20%28MGL-PAX:READER%20MGL-GD::GD-OPTIMIZER%29 "MGL-GD:MOMENTUM-TYPE (MGL-PAX:READER MGL-GD::GD-OPTIMIZER)"
  [56b2]: #MGL-BP:@MGL-BP-OVERVIEW%20MGL-PAX:SECTION "Backprop Overview"
  [5748]: #MGL-OPT:@MGL-OPT-OPTIMIZER%20MGL-PAX:SECTION "Implementing Optimizers"
  [5752]: #MGL-CORE:COUNTER%20%28MGL-PAX:READER%20MGL-CORE:MONITOR%29 "MGL-CORE:COUNTER (MGL-PAX:READER MGL-CORE:MONITOR)"
  [5842]: #MGL-COMMON:NAME%20GENERIC-FUNCTION "MGL-COMMON:NAME GENERIC-FUNCTION"
  [5979]: #MGL-CORE:BASIC-COUNTER%20CLASS "MGL-CORE:BASIC-COUNTER CLASS"
  [59c2]: #MGL-RESAMPLE:@MGL-RESAMPLE-MISC%20MGL-PAX:SECTION "Miscellaneous Operations"
  [59dd]: #MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EMAX%29 "MGL-COMMON:GROUP-SIZE (MGL-PAX:READER MGL-BP:->MAX)"
  [5a43]: #MGL-GD:PER-WEIGHT-BATCH-GD-OPTIMIZER%20CLASS "MGL-GD:PER-WEIGHT-BATCH-GD-OPTIMIZER CLASS"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5bd4]: #MGL-BP:BACKWARD%20GENERIC-FUNCTION "MGL-BP:BACKWARD GENERIC-FUNCTION"
  [5cd8]: http://www.lispworks.com/documentation/HyperSpec/Body/f_numera.htm "DENOMINATOR (MGL-PAX:CLHS FUNCTION)"
  [5d86]: #MGL-BP:@MGL-BP-ACTIVATION-FUNCTIONS%20MGL-PAX:SECTION "Activation Functions"
  [5ded]: #MGL-RESAMPLE:SPLIT-FOLD%2FMOD%20FUNCTION "MGL-RESAMPLE:SPLIT-FOLD/MOD FUNCTION"
  [5fdc]: #MGL-CORE:MAP-BATCHES-FOR-MODEL%20FUNCTION "MGL-CORE:MAP-BATCHES-FOR-MODEL FUNCTION"
  [6004]: #MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS%20FUNCTION "MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS FUNCTION"
  [6021]: #MGL-BP:-%3EMAX-CHANNEL%20CLASS "MGL-BP:->MAX-CHANNEL CLASS"
  [606c]: #MGL-BP:BUILD-FNN%20MGL-PAX:MACRO "MGL-BP:BUILD-FNN MGL-PAX:MACRO"
  [60b3]: #MGL:@MGL-GP%20MGL-PAX:SECTION "Gaussian Processes"
  [60d2]: #MGL-CORE:CONFUSION-MATRIX%20CLASS "MGL-CORE:CONFUSION-MATRIX CLASS"
  [60e3]: #MGL-CORE:@MGL-CLASSIFICATION%20MGL-PAX:SECTION "Classification"
  [6202]: #MGL-CORE:MONITORS%20%28MGL-PAX:ACCESSOR%20MGL-BP:BP-LEARNER%29 "MGL-CORE:MONITORS (MGL-PAX:ACCESSOR MGL-BP:BP-LEARNER)"
  [627a]: #MGL-RESAMPLE:FRACTURE-STRATIFIED%20FUNCTION "MGL-RESAMPLE:FRACTURE-STRATIFIED FUNCTION"
  [62de]: #MGL-CORE:ADD-TO-COUNTER%20GENERIC-FUNCTION "MGL-CORE:ADD-TO-COUNTER GENERIC-FUNCTION"
  [64b3]: http://www.lispworks.com/documentation/HyperSpec/Body/a_vector.htm "VECTOR (MGL-PAX:CLHS NIL)"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [6598]: #MGL-CORE:@MGL-CLASSIFICATION-COUNTER%20MGL-PAX:SECTION "Classification Counters"
  [6651]: http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm "DESCRIBE (MGL-PAX:CLHS FUNCTION)"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [6872]: #MGL-BP:@MGL-BP-WEIGHT-LUMP%20MGL-PAX:SECTION "Weight Lump"
  [6a6f]: #MGL-OPT:@MGL-OPT-EXTENSION-API%20MGL-PAX:SECTION "Extension API"
  [6b38]: #MGL-BP:@MGL-FNN-TUTORIAL%20MGL-PAX:SECTION "`fnn` Tutorial"
  [6bd7]: #MGL-CORE:LOAD-STATE%20FUNCTION "MGL-CORE:LOAD-STATE FUNCTION"
  [6d14]: mat-manual.md#MGL-MAT:MAT%20CLASS "MGL-MAT:MAT CLASS"
  [6da5]: #MGL-CORE:@MGL-ATTRIBUTES%20MGL-PAX:SECTION "Attributes"
  [6e96]: #MGL-BP:TIME-STEP%20FUNCTION "MGL-BP:TIME-STEP FUNCTION"
  [6f82]: #MGL-RESAMPLE:FRACTURE%20FUNCTION "MGL-RESAMPLE:FRACTURE FUNCTION"
  [6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [7068]: #MGL-CORE:MONITOR%20CLASS "MGL-CORE:MONITOR CLASS"
  [715c]: #MGL-DATASET:FUNCTION-SAMPLER%20CLASS "MGL-DATASET:FUNCTION-SAMPLER CLASS"
  [7162]: #MGL-BP:-%3EACTIVATION%20CLASS "MGL-BP:->ACTIVATION CLASS"
  [718a]: named-readtables-manual.md#%22named-readtables%22%20ASDF%2FSYSTEM:SYSTEM "\"named-readtables\" ASDF/SYSTEM:SYSTEM"
  [71f9]: #MGL-BP:STEP-MONITORS%20%28MGL-PAX:ACCESSOR%20MGL-BP:RNN%29 "MGL-BP:STEP-MONITORS (MGL-PAX:ACCESSOR MGL-BP:RNN)"
  [764b]: #MGL-BP:BUILD-RNN%20MGL-PAX:MACRO "MGL-BP:BUILD-RNN MGL-PAX:MACRO"
  [765c]: #MGL-DATASET:MAP-DATASETS%20FUNCTION "MGL-DATASET:MAP-DATASETS FUNCTION"
  [779d]: #MGL-OPT:@MGL-OPT-ITERATIVE-OPTIMIZER%20MGL-PAX:SECTION "Iterative Optimizer"
  [7960]: #MGL-BP:SHIFT%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29 "MGL-BP:SHIFT (MGL-PAX:READER MGL-BP:->BATCH-NORMALIZATION)"
  [7a28]: #MGL-BP:@MGL-BP-EXTENSION-API%20MGL-PAX:SECTION "Clump API"
  [7bc3]: #MGL-DATASET:@MGL-SAMPLER%20MGL-PAX:SECTION "Samplers"
  [7c2f]: #MGL-OPT:INITIALIZE-OPTIMIZER*%20GENERIC-FUNCTION "MGL-OPT:INITIALIZE-OPTIMIZER* GENERIC-FUNCTION"
  [7ee3]: #MGL-CORE:@MGL-COUNTER-CLASSES%20MGL-PAX:SECTION "Counter classes"
  [80e2]: #MGL-BP:VARIANCE-FOR-PREDICTION%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29 "MGL-BP:VARIANCE-FOR-PREDICTION (MGL-PAX:ACCESSOR MGL-BP:->GAUSSIAN-RANDOM)"
  [8148]: #MGL-CORE:READ-STATE%20FUNCTION "MGL-CORE:READ-STATE FUNCTION"
  [82d8]: #MGL-BP:ADD-CLUMP%20FUNCTION "MGL-BP:ADD-CLUMP FUNCTION"
  [83e6]: #MGL-CG:@MGL-CG%20MGL-PAX:SECTION "Conjugate Gradient"
  [83f9]: #MGL-BP:-%3ESIGMOID%20CLASS "MGL-BP:->SIGMOID CLASS"
  [85d3]: #MGL-COMMON:SIZE%20%28MGL-PAX:READER%20MGL-BP:LUMP%29 "MGL-COMMON:SIZE (MGL-PAX:READER MGL-BP:LUMP)"
  [85d34]: #MGL-BP:-%3ESOFTMAX-XE-LOSS%20CLASS "MGL-BP:->SOFTMAX-XE-LOSS CLASS"
  [8611]: #MGL-RESAMPLE:@MGL-RESAMPLE-SHUFFLING%20MGL-PAX:SECTION "Shuffling"
  [86fd]: #MGL-RESAMPLE:SAMPLE-FROM%20FUNCTION "MGL-RESAMPLE:SAMPLE-FROM FUNCTION"
  [871e]: #MGL-BP:@MGL-RNN%20MGL-PAX:SECTION "Recurrent Neural Nets"
  [876d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST (MGL-PAX:CLHS FUNCTION)"
  [8788]: #MGL-BP:@MGL-BP%20MGL-PAX:SECTION "Backpropagation Neural Networks"
  [8970]: #MGL-COMMON:SCALE%20GENERIC-FUNCTION "MGL-COMMON:SCALE GENERIC-FUNCTION"
  [8ae0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY (MGL-PAX:CLHS FUNCTION)"
  [8af5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_numera.htm "NUMERATOR (MGL-PAX:CLHS FUNCTION)"
  [8b55]: #MGL-BP:ARRANGE-FOR-RENORMALIZING-ACTIVATIONS%20FUNCTION "MGL-BP:ARRANGE-FOR-RENORMALIZING-ACTIVATIONS FUNCTION"
  [8cb8]: #MGL-RESAMPLE:SPLIT-STRATIFIED%20FUNCTION "MGL-RESAMPLE:SPLIT-STRATIFIED FUNCTION"
  [8da0]: #MGL-OPT:ITERATIVE-OPTIMIZER%20CLASS "MGL-OPT:ITERATIVE-OPTIMIZER CLASS"
  [8dd7]: #MGL-CORE:N-STRIPES%20GENERIC-FUNCTION "MGL-CORE:N-STRIPES GENERIC-FUNCTION"
  [8e53]: #MGL-BP:UNFOLDER%20%28MGL-PAX:READER%20MGL-BP:RNN%29 "MGL-BP:UNFOLDER (MGL-PAX:READER MGL-BP:RNN)"
  [8f37]: #MGL-CORE:MONITORS%20GENERIC-FUNCTION "MGL-CORE:MONITORS GENERIC-FUNCTION"
  [9006]: #MGL-OPT:TERMINATION%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29 "MGL-OPT:TERMINATION (MGL-PAX:ACCESSOR MGL-OPT:ITERATIVE-OPTIMIZER)"
  [9105]: #MGL-BP:@MGL-BP-ACTIVATIONS%20MGL-PAX:SECTION "Activations"
  [911c]: #MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS%20FUNCTION "MGL-CORE:MAKE-CLASSIFICATION-ACCURACY-MONITORS FUNCTION"
  [9192]: #MGL:@MGL-OVERVIEW%20MGL-PAX:SECTION "Overview"
  [91a3]: #MGL-CORE:MAX-N-STRIPES%20%28MGL-PAX:READER%20MGL-BP:BPN%29 "MGL-CORE:MAX-N-STRIPES (MGL-PAX:READER MGL-BP:BPN)"
  [91f3]: #MGL-BP:@MGL-BP-UTILITIES%20MGL-PAX:SECTION "Utilities"
  [9271]: http://www.lispworks.com/documentation/HyperSpec/Body/a_list.htm "LIST (MGL-PAX:CLHS NIL)"
  [9385]: #MGL-CORE:LABEL-INDEX-DISTRIBUTIONS%20GENERIC-FUNCTION "MGL-CORE:LABEL-INDEX-DISTRIBUTIONS GENERIC-FUNCTION"
  [93a7]: #MGL-BP:@MGL-BP-LOSSES%20MGL-PAX:SECTION "Losses"
  [9524]: #MGL-RESAMPLE:CROSS-VALIDATE%20FUNCTION "MGL-RESAMPLE:CROSS-VALIDATE FUNCTION"
  [95fe]: #MGL-CORE:WRITE-STATE%20FUNCTION "MGL-CORE:WRITE-STATE FUNCTION"
  [9641]: #MGL-BP:@MGL-BP-LUMPS%20MGL-PAX:SECTION "Lumps"
  [96d0]: #MGL-NLP:FEATURE-ENCODER%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29 "MGL-NLP:FEATURE-ENCODER (MGL-PAX:READER MGL-NLP:BAG-OF-WORDS-ENCODER)"
  [9700]: #MGL-BP:@MGL-RNN-TUTORIAL%20MGL-PAX:SECTION "`rnn` Tutorial"
  [9715]: #MGL-CORE:ATTRIBUTED%20CLASS "MGL-CORE:ATTRIBUTED CLASS"
  [9749]: #MGL-CG:CG-ARGS%20%28MGL-PAX:ACCESSOR%20MGL-CG:CG-OPTIMIZER%29 "MGL-CG:CG-ARGS (MGL-PAX:ACCESSOR MGL-CG:CG-OPTIMIZER)"
  [989a]: #MGL-GD:@MGL-GD-SEGMENTED-GD-OPTIMIZER%20MGL-PAX:SECTION "Segmented GD Optimizer"
  [989c]: #MGL-CORE:APPLY-MONITORS%20FUNCTION "MGL-CORE:APPLY-MONITORS FUNCTION"
  [993b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "TANH (MGL-PAX:CLHS FUNCTION)"
  [9a5b]: #MGL-OPT:SEGMENT-DERIVATIVES%20GENERIC-FUNCTION "MGL-OPT:SEGMENT-DERIVATIVES GENERIC-FUNCTION"
  [9a84]: #MGL-BP:-%3EMIN%20CLASS "MGL-BP:->MIN CLASS"
  [9d3a]: #MGL-BP:-%3ERELU%20CLASS "MGL-BP:->RELU CLASS"
  [9da9]: #MGL-BP:-%3EBATCH-NORMALIZED%20CLASS "MGL-BP:->BATCH-NORMALIZED CLASS"
  [9de4]: #MGL-BP:FNN%20CLASS "MGL-BP:FNN CLASS"
  [9fff]: mat-manual.md#MGL-MAT:WITH-SYNCING-CUDA-FACETS%20MGL-PAX:MACRO "MGL-MAT:WITH-SYNCING-CUDA-FACETS MGL-PAX:MACRO"
  [a077]: #MGL-CORE:COUNTER%20GENERIC-FUNCTION "MGL-CORE:COUNTER GENERIC-FUNCTION"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a210]: #MGL-OPT:@MGL-OPT-GRADIENT-SINK%20MGL-PAX:SECTION "Implementing Gradient Sinks"
  [a39b]: #MGL-RESAMPLE:@MGL-RESAMPLE%20MGL-PAX:SECTION "Resampling"
  [a437]: #MGL-COMMON:GROUP-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3ESOFTMAX-XE-LOSS%29 "MGL-COMMON:GROUP-SIZE (MGL-PAX:READER MGL-BP:->SOFTMAX-XE-LOSS)"
  [a4fe]: #MGL-BP:CLUMP%20CLASS "MGL-BP:CLUMP CLASS"
  [a541]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRINC-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [a81b]: #MGL-BP:DERIVATIVES%20GENERIC-FUNCTION "MGL-BP:DERIVATIVES GENERIC-FUNCTION"
  [a884]: #MGL-GD:@MGL-GD-PER-WEIGHT-OPTIMIZATION%20MGL-PAX:SECTION "Per-weight Optimization"
  [aa2e]: #MGL-BP:@MGL-BP-STOCHASTICITY%20MGL-PAX:SECTION "Stochasticity"
  [aa86]: #MGL-GD:VARIANCE-ADJUSTMENT%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29 "MGL-GD:VARIANCE-ADJUSTMENT (MGL-PAX:READER MGL-BP:->BATCH-NORMALIZATION)"
  [aabd]: #MGL-OPT:MAP-GRADIENT-SINK%20GENERIC-FUNCTION "MGL-OPT:MAP-GRADIENT-SINK GENERIC-FUNCTION"
  [ab3c]: #MGL-COMMON:WEIGHTS%20GENERIC-FUNCTION "MGL-COMMON:WEIGHTS GENERIC-FUNCTION"
  [ad8f]: #MGL-DATASET:*INFINITELY-EMPTY-DATASET*%20VARIABLE "MGL-DATASET:*INFINITELY-EMPTY-DATASET* VARIABLE"
  [ada2]: #MGL-CORE:@MGL-PARAMETERIZED-EXECUTOR-CACHE%20MGL-PAX:SECTION "Parameterized Executor Cache"
  [ae23]: http://www.lispworks.com/documentation/HyperSpec/Body/t_seq.htm "SEQUENCE (MGL-PAX:CLHS CLASS)"
  [ae3d]: #MGL-OPT:MINIMIZE*%20GENERIC-FUNCTION "MGL-OPT:MINIMIZE* GENERIC-FUNCTION"
  [aee6]: #MGL-RESAMPLE:SAMPLE-STRATIFIED%20FUNCTION "MGL-RESAMPLE:SAMPLE-STRATIFIED FUNCTION"
  [af05]: #MGL-GD:MOMENTUM%20%28MGL-PAX:ACCESSOR%20MGL-GD::GD-OPTIMIZER%29 "MGL-GD:MOMENTUM (MGL-PAX:ACCESSOR MGL-GD::GD-OPTIMIZER)"
  [af6b]: #MGL-GD:CLIP-L2-NORM%20FUNCTION "MGL-GD:CLIP-L2-NORM FUNCTION"
  [b01b]: #MGL-CORE:MAP-OVER-EXECUTORS%20GENERIC-FUNCTION "MGL-CORE:MAP-OVER-EXECUTORS GENERIC-FUNCTION"
  [b057]: mat-manual.md#MGL-MAT:CUDA-AVAILABLE-P%20FUNCTION "MGL-MAT:CUDA-AVAILABLE-P FUNCTION"
  [b0f3]: #MGL-BP:RNN%20CLASS "MGL-BP:RNN CLASS"
  [b186]: #MGL-CORE:CROSS-ENTROPY-COUNTER%20CLASS "MGL-CORE:CROSS-ENTROPY-COUNTER CLASS"
  [b5c7]: #MGL-COMMON:TARGET%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3ESOFTMAX-XE-LOSS%29 "MGL-COMMON:TARGET (MGL-PAX:ACCESSOR MGL-BP:->SOFTMAX-XE-LOSS)"
  [b602]: #MGL-BP:-%3EACTIVATION%20FUNCTION "MGL-BP:->ACTIVATION FUNCTION"
  [b647]: #MGL-RESAMPLE:@MGL-RESAMPLE-BAGGING%20MGL-PAX:SECTION "Bagging"
  [b76f]: #MGL-BP:-%3EWEIGHT%20CLASS "MGL-BP:->WEIGHT CLASS"
  [ba91]: #MGL-RESAMPLE:STRATIFY%20FUNCTION "MGL-RESAMPLE:STRATIFY FUNCTION"
  [bbdf]: #MGL-CORE:APPLY-MONITOR%20GENERIC-FUNCTION "MGL-CORE:APPLY-MONITOR GENERIC-FUNCTION"
  [bc8c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_exp_e.htm "EXP (MGL-PAX:CLHS FUNCTION)"
  [bd13]: #MGL-GD:@MGL-GD-ADAM-OPTIMIZER%20MGL-PAX:SECTION "Adam Optimizer"
  [bdf9]: #MGL-DATASET:N-SAMPLES%20%28MGL-PAX:READER%20MGL-DATASET:FUNCTION-SAMPLER%29 "MGL-DATASET:N-SAMPLES (MGL-PAX:READER MGL-DATASET:FUNCTION-SAMPLER)"
  [be8d]: #MGL-DATASET:@MGL-SAMPLER-FUNCTION-SAMPLER%20MGL-PAX:SECTION "Function Sampler"
  [be95]: #MGL-CORE:@MGL-COUNTER%20MGL-PAX:SECTION "Counters"
  [c102]: #MGL-CORE:SAVE-STATE%20FUNCTION "MGL-CORE:SAVE-STATE FUNCTION"
  [c1ac]: #MGL-BP:LUMP%20CLASS "MGL-BP:LUMP CLASS"
  [c1ae]: #MGL-BP:FORWARD%20GENERIC-FUNCTION "MGL-BP:FORWARD GENERIC-FUNCTION"
  [c40e]: #MGL-GD:@MGL-GD-UTILITIES%20MGL-PAX:SECTION "Utilities"
  [c469]: #MGL-BP:-%3EBATCH-NORMALIZATION%20CLASS "MGL-BP:->BATCH-NORMALIZATION CLASS"
  [c573]: #MGL-CORE:@MGL-CLASSIFICATION-MONITOR%20MGL-PAX:SECTION "Classification Monitors"
  [c58b]: #MGL-OPT:@MGL-OPT-GRADIENT-SOURCE%20MGL-PAX:SECTION "Implementing Gradient Sources"
  [c701]: #MGL-CORE:@MGL-MONITOR%20MGL-PAX:SECTION "Monitors"
  [c74a]: #MGL-OPT:@MGL-OPT%20MGL-PAX:SECTION "Gradient Based Optimization"
  [c7fa]: #MGL-BP:RENORMALIZE-ACTIVATIONS%20FUNCTION "MGL-BP:RENORMALIZE-ACTIVATIONS FUNCTION"
  [c8db]: #MGL-CORE:@MGL-FEATURES%20MGL-PAX:SECTION "Features"
  [c918]: #MGL-COMMON:BATCH-SIZE%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZATION%29 "MGL-COMMON:BATCH-SIZE (MGL-PAX:READER MGL-BP:->BATCH-NORMALIZATION)"
  [ca09]: #MGL-OPT:RESET-OPTIMIZATION-MONITORS%20GENERIC-FUNCTION "MGL-OPT:RESET-OPTIMIZATION-MONITORS GENERIC-FUNCTION"
  [caca]: mat-manual.md#%22mgl-mat%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-mat\" ASDF/SYSTEM:SYSTEM"
  [caec]: #MGL-CORE:LABEL-INDEX-DISTRIBUTION%20GENERIC-FUNCTION "MGL-CORE:LABEL-INDEX-DISTRIBUTION GENERIC-FUNCTION"
  [cbb4]: #MGL-NLP:BAG-OF-WORDS-ENCODER%20CLASS "MGL-NLP:BAG-OF-WORDS-ENCODER CLASS"
  [cc1c]: #MGL-COMMON:NODES%20GENERIC-FUNCTION "MGL-COMMON:NODES GENERIC-FUNCTION"
  [cc37]: #MGL-CORE:ATTRIBUTES%20%28MGL-PAX:ACCESSOR%20MGL-CORE:ATTRIBUTED%29 "MGL-CORE:ATTRIBUTES (MGL-PAX:ACCESSOR MGL-CORE:ATTRIBUTED)"
  [cc80]: #MGL-CORE:LABEL-INDEX%20GENERIC-FUNCTION "MGL-CORE:LABEL-INDEX GENERIC-FUNCTION"
  [cd3b]: #MGL-CORE:@MGL-MEASURER%20MGL-PAX:SECTION "Measurers"
  [cee6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm "SYMBOL-VALUE (MGL-PAX:CLHS FUNCTION)"
  [d0e3]: #MGL-BP:@MGL-RNN-TIME-WARP%20MGL-PAX:SECTION "Time Warp"
  [d10a]: #MGL-CG:ON-CG-BATCH-DONE%20%28MGL-PAX:ACCESSOR%20MGL-CG:CG-OPTIMIZER%29 "MGL-CG:ON-CG-BATCH-DONE (MGL-PAX:ACCESSOR MGL-CG:CG-OPTIMIZER)"
  [d1e0]: #MGL-BP:@MGL-BPN%20MGL-PAX:SECTION "`bpn`s"
  [d3b2]: #MGL-CORE:PARAMETERIZED-EXECUTOR-CACHE-MIXIN%20CLASS "MGL-CORE:PARAMETERIZED-EXECUTOR-CACHE-MIXIN CLASS"
  [d443]: #MGL-NLP:ENCODED-FEATURE-TYPE%20%28MGL-PAX:READER%20MGL-NLP:BAG-OF-WORDS-ENCODER%29 "MGL-NLP:ENCODED-FEATURE-TYPE (MGL-PAX:READER MGL-NLP:BAG-OF-WORDS-ENCODER)"
  [d479]: #MGL-OPT:RESET-OPTIMIZATION-MONITORS%20%28METHOD%20%28MGL-OPT:ITERATIVE-OPTIMIZER%20T%29%29 "MGL-OPT:RESET-OPTIMIZATION-MONITORS (METHOD (MGL-OPT:ITERATIVE-OPTIMIZER T))"
  [d699]: #MGL-COMMON:NODES%20%28MGL-PAX:READER%20MGL-BP:LUMP%29 "MGL-COMMON:NODES (MGL-PAX:READER MGL-BP:LUMP)"
  [d6e0]: #MGL-BP:WARP-START%20%28MGL-PAX:READER%20MGL-BP:RNN%29 "MGL-BP:WARP-START (MGL-PAX:READER MGL-BP:RNN)"
  [d811]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apply.htm "APPLY (MGL-PAX:CLHS FUNCTION)"
  [d94e]: #MGL-GD:BATCH-GD-OPTIMIZER%20CLASS "MGL-GD:BATCH-GD-OPTIMIZER CLASS"
  [d96a]: #MGL-BP:MEAN%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EGAUSSIAN-RANDOM%29 "MGL-BP:MEAN (MGL-PAX:ACCESSOR MGL-BP:->GAUSSIAN-RANDOM)"
  [dbc4]: #MGL-BP:-%3EV*M%20CLASS "MGL-BP:->V*M CLASS"
  [dd95]: #MGL-OPT:INITIALIZE-GRADIENT-SOURCE*%20GENERIC-FUNCTION "MGL-OPT:INITIALIZE-GRADIENT-SOURCE* GENERIC-FUNCTION"
  [e0e6]: #MGL-GD:ADAM-OPTIMIZER%20CLASS "MGL-GD:ADAM-OPTIMIZER CLASS"
  [e198]: #MGL-COMMON:@MGL-COMMON%20MGL-PAX:SECTION "Common Stuff"
  [e2ae]: pax-manual.md#MGL-PAX:NOTE%20MGL-PAX:MACRO "MGL-PAX:NOTE MGL-PAX:MACRO"
  [e363]: http://www.lispworks.com/documentation/HyperSpec/Body/a_mod.htm "MOD (MGL-PAX:CLHS NIL)"
  [e46f]: #MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS*%20GENERIC-FUNCTION "MGL-CORE:MAKE-CROSS-ENTROPY-MONITORS* GENERIC-FUNCTION"
  [e4dd]: http://www.lispworks.com/documentation/HyperSpec/Body/s_multip.htm "MULTIPLE-VALUE-CALL (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [e50c]: #MGL-CORE:MONITOR-MODEL-RESULTS%20FUNCTION "MGL-CORE:MONITOR-MODEL-RESULTS FUNCTION"
  [e668]: #MGL-CORE:@MGL-MONITORING%20MGL-PAX:SECTION "Monitoring"
  [e746]: #MGL-OPT:@MGL-OPT-COST%20MGL-PAX:SECTION "Cost Function"
  [e7ea]: #MGL:@MGL-DEPENDENCIES%20MGL-PAX:SECTION "Dependencies"
  [e7f6]: #MGL-BP:DROPOUT%20%28MGL-PAX:ACCESSOR%20MGL-BP:-%3EINPUT%29 "MGL-BP:DROPOUT (MGL-PAX:ACCESSOR MGL-BP:->INPUT)"
  [e8d2]: #MGL-BP:-%3ESQUARED-DIFFERENCE%20CLASS "MGL-BP:->SQUARED-DIFFERENCE CLASS"
  [ea7d]: #MGL-LOG:LOG-MAT-ROOM%20FUNCTION "MGL-LOG:LOG-MAT-ROOM FUNCTION"
  [eaf1]: #MGL-BP:BATCH-NORMALIZATION%20%28MGL-PAX:READER%20MGL-BP:-%3EBATCH-NORMALIZED%29 "MGL-BP:BATCH-NORMALIZATION (MGL-PAX:READER MGL-BP:->BATCH-NORMALIZED)"
  [eb05]: #MGL-CORE:MEASURER%20%28MGL-PAX:READER%20MGL-CORE:MONITOR%29 "MGL-CORE:MEASURER (MGL-PAX:READER MGL-CORE:MONITOR)"
  [ebd4]: #MGL-OPT:ON-OPTIMIZATION-STARTED%20%28MGL-PAX:ACCESSOR%20MGL-OPT:ITERATIVE-OPTIMIZER%29 "MGL-OPT:ON-OPTIMIZATION-STARTED (MGL-PAX:ACCESSOR MGL-OPT:ITERATIVE-OPTIMIZER)"
  [ec8b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_zerop.htm "ZEROP (MGL-PAX:CLHS FUNCTION)"
  [ece2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "SIN (MGL-PAX:CLHS FUNCTION)"
  [ed4f]: #MGL-BP:*WARP-TIME*%20VARIABLE "MGL-BP:*WARP-TIME* VARIABLE"
  [edcf]: #MGL-BP:-%3ESUM%20CLASS "MGL-BP:->SUM CLASS"
  [ee97]: #MGL-CG:CG-OPTIMIZER%20CLASS "MGL-CG:CG-OPTIMIZER CLASS"
  [f00d]: #MGL-OPT:SEGMENTS%20GENERIC-FUNCTION "MGL-OPT:SEGMENTS GENERIC-FUNCTION"
  [f17b]: #MGL-RESAMPLE:@MGL-RESAMPLE-CROSS-VALIDATION%20MGL-PAX:SECTION "Cross-validation"
  [f1c1]: #MGL-BP:-%3EEMBEDDING%20CLASS "MGL-BP:->EMBEDDING CLASS"
  [f257]: #MGL-CORE:@MGL-CORE%20MGL-PAX:SECTION "Core"
  [f470]: mat-manual.md "MAT Manual"
  [f491]: #MGL-COMMON:FN%20%28MGL-PAX:READER%20MGL-DIFFUN:DIFFUN%29 "MGL-COMMON:FN (MGL-PAX:READER MGL-DIFFUN:DIFFUN)"
  [f54e]: #MGL-BP:-%3EINPUT%20CLASS "MGL-BP:->INPUT CLASS"
  [f573]: #MGL-BP:CUDA-WINDOW-START-TIME%20%28MGL-PAX:ACCESSOR%20MGL-BP:RNN%29 "MGL-BP:CUDA-WINDOW-START-TIME (MGL-PAX:ACCESSOR MGL-BP:RNN)"
  [f652]: #MGL-BP:-%3EMAX%20CLASS "MGL-BP:->MAX CLASS"
  [f6ae]: #MGL-GD:NORMALIZED-BATCH-GD-OPTIMIZER%20CLASS "MGL-GD:NORMALIZED-BATCH-GD-OPTIMIZER CLASS"
  [f790]: #MGL-RESAMPLE:@MGL-RESAMPLE-PARTITIONS%20MGL-PAX:SECTION "Partitions"
  [f7aa]: #MGL:@MGL-INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [f7c1]: #MGL-BP:CLUMPS%20%28MGL-PAX:READER%20MGL-BP:BPN%29 "MGL-BP:CLUMPS (MGL-PAX:READER MGL-BP:BPN)"
  [f85e]: #MGL-LOG:LOG-MSG%20FUNCTION "MGL-LOG:LOG-MSG FUNCTION"
  [f956]: #MGL-DATASET:SAMPLE%20GENERIC-FUNCTION "MGL-DATASET:SAMPLE GENERIC-FUNCTION"
  [f98e]: #MGL-CORE:DO-EXECUTORS%20MGL-PAX:MACRO "MGL-CORE:DO-EXECUTORS MGL-PAX:MACRO"
  [fa6d]: #MGL-COMMON:BATCH-SIZE%20GENERIC-FUNCTION "MGL-COMMON:BATCH-SIZE GENERIC-FUNCTION"
  [faaa]: #MGL-CORE:DO-BATCHES-FOR-MODEL%20MGL-PAX:MACRO "MGL-CORE:DO-BATCHES-FOR-MODEL MGL-PAX:MACRO"
  [feaa]: #MGL-BP:-%3EGAUSSIAN-RANDOM%20CLASS "MGL-BP:->GAUSSIAN-RANDOM CLASS"
  [fedd]: #MGL-CORE:ENCODE%20GENERIC-FUNCTION "MGL-CORE:ENCODE GENERIC-FUNCTION"
  [ff5a]: #MGL-BP:LAG%20FUNCTION "MGL-BP:LAG FUNCTION"
  [ff82]: #MGL-CORE:@MGL-MODEL-STRIPE%20MGL-PAX:SECTION "Batch Processing"
