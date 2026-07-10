<a id="x-28MGL-GPR-3A-40GPR-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-MANUAL%20MGL-PAX:SECTION"></a>

# GPR Manual

## Table of Contents

- [1 Links][6e4d]

- [2 Background][2e8a]

- [3 Evolutionary Algorithms][057a]

    - [3.1 Populations][9cba]

    - [3.2 Evaluation][b60a]

    - [3.3 Training][e87f]

- [4 Genetic Programming][0d2f]

    - [4.1 Background][c392]

    - [4.2 Tutorial][075b]

    - [4.3 Expressions][f9a4]

    - [4.4 Basics][e084]

    - [4.5 Search Space][9085]

    - [4.6 Reproduction][5d57]

    - [4.7 Environment][084d]

- [5 Differential Evolution][db20]

    - [5.1 SANSDE][40a0]

###### \[in package MGL-GPR\]

<a id="x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-gpr%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- \[system\] **"mgl-gpr"**

    - *Version:* 0.0.1

    - *Description:* `mgl-gpr` is a library of evolutionary algorithms such
    as Genetic Programming (evolving typed expressions from a set of
    operators and constants) and Differential Evolution.

    - *Licence:* MIT, see COPYING.

    - *Author:* Gábor Melis <mega@retes.hu>

    - *Mailto:* [mega@retes.hu](mailto:mega@retes.hu)

    - *Homepage:* <http://melisgl.github.io/mgl-gpr>

    - *Bug tracker:* <https://github.com/melisgl/mgl-gpr/issues>

    - *Source control:* [GIT](https://github.com/melisgl/mgl-gpr.git)

    - *Depends on:* alexandria, [mgl-pax][6fdb]

<a id="x-28MGL-GPR-3A-40GPR-GP-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-LINKS%20MGL-PAX:SECTION"></a>

## 1 Links

The official repository is <https://github.com/melisgl/mgl-gpr>, and
this document in available in various formats on
<https://fixnum.com> for the latest version.

<a id="x-28MGL-GPR-3A-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-BACKGROUND%20MGL-PAX:SECTION"></a>

## 2 Background

Evolutionary algorithms are optimization tools that assume little
of the task at hand. Often they are population based, that is, there
is a set of individuals that each represent a candidate solution.
Individuals are combined and changed with crossover and mutationlike
operators to produce the next generation. Individuals with lower
fitness have a lower probability to survive than those with higher
fitness. In this way, the fitness function defines the optimization
task.

Typically, EAs are quick to get up and running, can produce
reasonable results across a wild variety of domains, but they may
need a bit of fiddling to perform well and domain specific
approaches will almost always have better results. All in all, EA
can be very useful to cut down on the tedium of human trial and
error. However, they have serious problems scaling to higher number
of variables.

This library grew from the Genetic Programming implementation I
wrote while working for Ravenpack who agreed to release it under an
MIT licence. Several years later I cleaned it up, and documented it.
Enjoy.

<a id="x-28MGL-GPR-3A-40GPR-EA-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-EA%20MGL-PAX:SECTION"></a>

## 3 Evolutionary Algorithms

Evolutionary algorithm is an umbrella term. In this section we
first discuss the concepts common to conrete evolutionary algorithms
[Genetic Programming][0d2f] and [Differential Evolution][db20].

<a id="x-28MGL-GPR-3AEVOLUTIONARY-ALGORITHM-20CLASS-29"></a>
<a id="MGL-GPR:EVOLUTIONARY-ALGORITHM%20CLASS"></a>

- \[class\] **evolutionary-algorithm**

    The `evolutionary-algorithm` is an abstract base
    class for generational, population based optimization algorithms.

<a id="x-28MGL-GPR-3A-40GPR-EA-POPULATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-EA-POPULATION%20MGL-PAX:SECTION"></a>

### 3.1 Populations

The currenly implemented EAs are generational. That is, they
maintain a population of candidate solutions (also known as
individuals) which they replace with the next generation of
individuals.

<a id="x-28MGL-GPR-3APOPULATION-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:POPULATION-SIZE%20%28MGL-PAX:ACCESSOR%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[accessor\] **population-size** *[evolutionary-algorithm][f5ee] (:population-size)*

    The number of individuals in a generation. This is
    a very important parameter. Too low and there won't be enough
    diversity in the population, too high and convergence will be
    slow.

<a id="x-28MGL-GPR-3APOPULATION-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:POPULATION%20%28MGL-PAX:ACCESSOR%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[accessor\] **population** *[evolutionary-algorithm][f5ee] (= (make-array 0 :adjustable 0 :fill-pointer t))*

    An adjustable array with a fill-pointer that holds
    the individuals that make up the population.

<a id="x-28MGL-GPR-3AGENERATION-COUNTER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:GENERATION-COUNTER%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[reader\] **generation-counter** *[evolutionary-algorithm][f5ee] (= 0)*

    A counter that starts from 0 and is incremented by
    [`advance`][be13]. All accessors of [`evolutionary-algorithm`][f5ee] are allowed to be
    specialized on a subclass which allows them to be functions of
    `generation-counter`.

<a id="x-28MGL-GPR-3AADD-INDIVIDUAL-20FUNCTION-29"></a>
<a id="MGL-GPR:ADD-INDIVIDUAL%20FUNCTION"></a>

- \[function\] **add-individual** *ea individual*

    Adds `individual` to [`population`][1602] of `ea`. Usually called when
    initializing the `ea`.

<a id="x-28MGL-GPR-3A-40GPR-EA-EVALUATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-EA-EVALUATION%20MGL-PAX:SECTION"></a>

### 3.2 Evaluation

<a id="x-28MGL-GPR-3AEVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:EVALUATOR%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[reader\] **evaluator** *[evolutionary-algorithm][f5ee] (:evaluator)*

    A function of two arguments: the
    [`evolutionary-algorithm`][f5ee] object and an individual. It must return
    the fitness of the individual. For [Genetic Programming][0d2f], the evaluator often
    simply calls [`eval`][0d6e], or [`compile`][bc41] + [`funcall`][03c7], and compares the result
    to some gold standard. It is also typical to slightly penalize
    solutions with too many nodes to control complexity and evaluation
    cost (see [`count-nodes`][d237]). For [Differential Evolution][db20], individuals are
    conceptually (and often implemented as) vectors of numbers so the
    fitness function may include an L1 or L2 penalty term.

    Alternatively, one can specify [`mass-evaluator`][ef7a] instead.

<a id="x-28MGL-GPR-3AMASS-EVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:MASS-EVALUATOR%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[reader\] **mass-evaluator** *[evolutionary-algorithm][f5ee] (:mass-evaluator = nil)*

    `nil` or a function of three arguments: the
    [`evolutionary-algorithm`][f5ee] object, the population vector and the
    fitness vector into which the fitnesses of the individuals in the
    population vector shall be written. By specifying `mass-evaluator`
    instead of an [`evaluator`][9a7e], one can, for example, distribute costly
    evaluations over multiple threads. `mass-evaluator` has precedence
    over `evaluator`.

<a id="x-28MGL-GPR-3AFITNESS-KEY-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:FITNESS-KEY%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[reader\] **fitness-key** *[evolutionary-algorithm][f5ee] (:fitness-key = #'identity)*

    A function that returns a real number for an
    object returned by [`evaluator`][9a7e]. It is called when two fitness are to
    be compared. The default value is #'[`identity`][8ae0] which is sufficient
    when `evaluator` returns real numbers. However, sometimes the
    evaluator returns more information about the solution (such as
    fitness in various situations) and `fitness-key` key be used to
    select the fitness value.

<a id="x-28MGL-GPR-3A-40GPR-EA-TRAINING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-EA-TRAINING%20MGL-PAX:SECTION"></a>

### 3.3 Training

Training is easy: one creates an object of a subclass of
[`evolutionary-algorithm`][f5ee] such as [`genetic-programming`][4643] or
[`differential-evolution`][a4e6], creates the initial population by adding
individuals to it (see [`add-individual`][4a1e]) and calls [`advance`][be13] in a loop
to move on to the next generation until a certain number of
generations or until the [`fittest`][9631] individual is good enough.

<a id="x-28MGL-GPR-3AADVANCE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-GPR:ADVANCE%20GENERIC-FUNCTION"></a>

- \[generic-function\] **advance** *ea*

    Create the next generation and place it in
    [`population`][1602] of `ea`.

<a id="x-28MGL-GPR-3AFITTEST-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:FITTEST%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[reader\] **fittest** *[evolutionary-algorithm][f5ee] (= nil)*

    The fittest individual ever to be seen and its
    fittness as a cons cell.

<a id="x-28MGL-GPR-3AFITTEST-CHANGED-FN-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
<a id="MGL-GPR:FITTEST-CHANGED-FN%20%28MGL-PAX:ACCESSOR%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29"></a>

- \[accessor\] **fittest-changed-fn** *[evolutionary-algorithm][f5ee] (:fittest-changed-fn = nil)*

    If non-`nil`, a function that's called when [`fittest`][9631]
    is updated with three arguments: the [`evolutionary-algorithm`][f5ee]
    object, the fittest individual and its fitness. Useful for
    tracking progress.

<a id="x-28MGL-GPR-3A-40GPR-GP-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP%20MGL-PAX:SECTION"></a>

## 4 Genetic Programming

<a id="x-28MGL-GPR-3A-40GPR-GP-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-BACKGROUND%20MGL-PAX:SECTION"></a>

### 4.1 Background

What is Genetic Programming? This is what Wikipedia has to say:

> In artificial intelligence, genetic programming (GP) is an
> evolutionary algorithm-based methodology inspired by biological
> evolution to find computer programs that perform a user-defined
> task. Essentially GP is a set of instructions and a fitness
> function to measure how well a computer has performed a task. It
> is a specialization of genetic algorithms (GA) where each
> individual is a computer program. It is a machine learning
> technique used to optimize a population of computer programs
> according to a fitness landscape determined by a program's ability
> to perform a given computational task.

Lisp has a long history of Genetic Programming because GP involves
manipulation of expressions which is of course particularly easy
with sexps.

<a id="x-28MGL-GPR-3A-40GPR-GP-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-TUTORIAL%20MGL-PAX:SECTION"></a>

### 4.2 Tutorial

GPR works with typed expressions. Mutation and crossover never
produce expressions that fail with a type error. Let's define a
couple of operators that work with real numbers and also return a
real:

    (defparameter *operators* (list (operator (+ real real) real)
                                    (operator (- real real) real)
                                    (operator (* real real) real)
                                    (operator (sin real) real)))

One cannot build an expression out of these operators because they
all have at least one argument. Let's define some literal classes
too. The first is produces random numbers, the second always returns
the symbol `*x*`:

    (defparameter *literals* (list (literal (real)
                                     (- (random 32.0) 16.0))
                                   (literal (real)
                                     '*x*)))

Armed with `*operators*` and `*literals*`, one can already build
random expressions with [`random-expression`][7df2], but we also need to
define how good a certain expression is which is called *fitness*.

In this example, we are going to perform symbolic regression, that
is, try to find an expression that approximates some target
expression well:

    (defparameter *target-expr* '(+ 7 (sin (expt (* *x* 2 pi) 2))))

Think of `*target-expr*` as a function of `*x*`. The evaluator
function will bind the special `*x*` to the input and simply [`eval`][0d6e]
the expression to be evaluated.

    (defvar *x*)

The evaluator function calculates the average difference between
`expr` and `target-expr`, penalizes large expressions and returns
the fitness of `expr`. Expressions with higher fitness have higher
chance to produce offsprings.

    (defun evaluate (gp expr target-expr)
      (declare (ignore gp))
      (/ 1
         (1+
          ;; Calculate average difference from target.
          (/ (loop for x from 0d0 to 10d0 by 0.5d0
                   summing (let ((*x* x))
                             (abs (- (eval expr)
                                     (eval target-expr)))))
             21))
         ;; Penalize large expressions.
         (let ((min-penalized-size 40)
               (size (count-nodes expr)))
           (if (< size min-penalized-size)
               1
               (exp (min 120 (/ (- size min-penalized-size) 10d0)))))))

When an expression is to undergo mutation, a randomizer function is
called. Here we change literal numbers slightly, or produce an
entirely new random expression that will be substituted for `expr`:

    (defun randomize (gp type expr)
      (if (and (numberp expr)
               (< (random 1.0) 0.5))
          (+ expr (random 1.0) -0.5)
          (random-gp-expression gp (lambda (level)
                                     (<= 3 level))
                                :type type)))

That's about it. Now we create a GP instance hooking everything up,
set up the initial population and just call [`advance`][be13] a couple of
times to create new generations of expressions.

    (defun run ()
      (let ((*print-length* nil)
            (*print-level* nil)
            (gp (make-instance
                 'gp
                 :toplevel-type 'real
                 :operators *operators*
                 :literals *literals*
                 :population-size 1000
                 :copy-chance 0.0
                 :mutation-chance 0.5
                 :evaluator (lambda (gp expr)
                              (evaluate gp expr *target-expr*))
                 :randomizer 'randomize
                 :selector (lambda (gp fitnesses)
                             (declare (ignore gp))
                             (hold-tournament fitnesses :n-contestants 2))
                 :fittest-changed-fn
                 (lambda (gp fittest fitness)
                   (format t "Best fitness until generation ~S: ~S for~%  ~S~%"
                           (generation-counter gp) fitness fittest)))))
        (loop repeat (population-size gp) do
          (add-individual gp (random-gp-expression gp (lambda (level)
                                                        (<= 5 level)))))
        (loop repeat 1000 do
          (when (zerop (mod (generation-counter gp) 20))
            (format t "Generation ~S~%" (generation-counter gp)))
          (advance gp))
        (destructuring-bind (fittest . fitness) (fittest gp)
          (format t "Best fitness: ~S for~%  ~S~%" fitness fittest))))

Note that this example can be found in
example/symbolic-regression.lisp.

<a id="x-28MGL-GPR-3A-40GPR-GP-EXPRESSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-EXPRESSIONS%20MGL-PAX:SECTION"></a>

### 4.3 Expressions

Genetic programming works with a population of individuals. The
individuals are sexps that may be evaluated directly by [`eval`][0d6e] or by
other means. The internal nodes and the leafs of the sexp as a tree
represent the application of operators and literal objects,
respectively. Note that currently there is no way to represent
literal lists.

<a id="x-28MGL-GPR-3AEXPRESSION-CLASS-20CLASS-29"></a>
<a id="MGL-GPR:EXPRESSION-CLASS%20CLASS"></a>

- \[class\] **expression-class**

    An object of `expression-class` defines two things:
    how to build a random expression that belongs to that expression
    class and what lisp type those expressions evaluate to.

<a id="x-28MGL-GPR-3ARESULT-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29"></a>
<a id="MGL-GPR:RESULT-TYPE%20%28MGL-PAX:READER%20MGL-GPR:EXPRESSION-CLASS%29"></a>

- \[reader\] **result-type** *[expression-class][95f7] (:result-type)*

    Expressions belonging to this expression class
    must evaluate to a value of this lisp type.

<a id="x-28MGL-GPR-3AWEIGHT-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29"></a>
<a id="MGL-GPR:WEIGHT%20%28MGL-PAX:READER%20MGL-GPR:EXPRESSION-CLASS%29"></a>

- \[reader\] **weight** *[expression-class][95f7] (:weight = 1)*

    The probability of an expression class to be
    selected from a set of candidates is proportional to its
    weight.

<a id="x-28MGL-GPR-3AOPERATOR-20CLASS-29"></a>
<a id="MGL-GPR:OPERATOR%20CLASS"></a>

- \[class\] **operator** *[expression-class][95f7]*

    Defines how the symbol [`name`][12f5] in the function
    position of a list can be combined arguments: how many and of what
    types. The following defines `+` as an operator that
    adds two [`float`][eee2]s:

        (make-instance 'operator 
                       :name '+
                       :result-type float
                       :argument-types '(float float))

    See the macro [`operator`][5b15] for a shorthand for the above.

    Currently no lambda list keywords are supported and there is no way
    to define how an expression with a particular operator is to be
    built. See [`random-expression`][7df2].

<a id="x-28MGL-GPR-3ANAME-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29"></a>
<a id="MGL-GPR:NAME%20%28MGL-PAX:READER%20MGL-GPR:OPERATOR%29"></a>

- \[reader\] **name** *[operator][b75b] (:name)*

    A symbol that's the name of the operator.

<a id="x-28MGL-GPR-3AARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29"></a>
<a id="MGL-GPR:ARGUMENT-TYPES%20%28MGL-PAX:READER%20MGL-GPR:OPERATOR%29"></a>

- \[reader\] **argument-types** *[operator][b75b] (:argument-types)*

    A list of lisp types. One for each argument of
    this operator.

<a id="x-28MGL-GPR-3AOPERATOR-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-GPR:OPERATOR%20MGL-PAX:MACRO"></a>

- \[macro\] **operator** *(name \&rest arg-types) result-type \&key (weight 1)*

    Syntactic sugar for instantiating operators. The example given for
    [`operator`][b75b] could be written as:

        (operator (+ float float) float)

    See [`weight`][be2e] for what `weight` means.

<a id="x-28MGL-GPR-3ALITERAL-20CLASS-29"></a>
<a id="MGL-GPR:LITERAL%20CLASS"></a>

- \[class\] **literal** *[expression-class][95f7]*

    This is slightly misnamed. An object belonging to
    the [`literal`][32b7] class is not a literal itself, it's a factory for
    literals via its [`builder`][0cb2] function. For example, the following
    literal builds bytes:

        (make-instance 'literal
                       :result-type '(unsigned-byte 8)
                       :builder (lambda () (random 256)))

    In practice, one rarely writes it out like that, because the [`literal`][8a46]
    macro provides a more convenient shorthand.

<a id="x-28MGL-GPR-3ABUILDER-20-28MGL-PAX-3AREADER-20MGL-GPR-3ALITERAL-29-29"></a>
<a id="MGL-GPR:BUILDER%20%28MGL-PAX:READER%20MGL-GPR:LITERAL%29"></a>

- \[reader\] **builder** *[literal][32b7] (:builder)*

    A function of no arguments that returns a random
    literal that belongs to its literal class.

<a id="x-28MGL-GPR-3ALITERAL-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-GPR:LITERAL%20MGL-PAX:MACRO"></a>

- \[macro\] **literal** *(result-type \&key (weight 1)) \&body body*

    Syntactic sugar for defining literal classes. The example given for
    [`literal`][32b7] could be written as:

        (literal ((unsigned-byte 8))
          (random 256))

    See [`weight`][be2e] for what `weight` means.

<a id="x-28MGL-GPR-3ARANDOM-EXPRESSION-20FUNCTION-29"></a>
<a id="MGL-GPR:RANDOM-EXPRESSION%20FUNCTION"></a>

- \[function\] **random-expression** *operators literals type terminate-fn*

    Return an expression built from `operators` and `literals` that
    evaluates to values of `type`. `terminate-fn` is a function of one
    argument: the level of the root of the subexpression to be generated
    in the context of the entire expression. If it returns `t` then a
    [`literal`][32b7] will be inserted (by calling its `builder` function),
    else an [`operator`][b75b] with all its necessary arguments.

    The algorithm recursively generates the expression starting from
    level 0 where only operators and literals with a [`result-type`][4e03] that's
    a subtype of `type` are considered and one is selected with the
    unnormalized probability given by its [`weight`][be2e]. On lower levels, the
    [`argument-types`][a92c] specification of operators is similarly satisfied and
    the resulting expression should evaluate without without a type
    error.

    The building of expressions cannot backtrack. If it finds itself in
    a situation where no literals or operators of the right type are
    available then it will fail with an error.

<a id="x-28MGL-GPR-3A-40GPR-GP-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-BASICS%20MGL-PAX:SECTION"></a>

### 4.4 Basics

To start the evolutionary process one creates a GP object,
adds to it the individuals (see [`add-individual`][4a1e]) that make up the
initial population and calls [`advance`][be13] in a loop to move on to the
next generation.

<a id="x-28MGL-GPR-3AGENETIC-PROGRAMMING-20CLASS-29"></a>
<a id="MGL-GPR:GENETIC-PROGRAMMING%20CLASS"></a>

- \[class\] **genetic-programming** *[evolutionary-algorithm][f5ee]*

    The [`genetic-programming`][4643] class defines the search
    space, how mutation and recombination occur, and hold various
    parameters of the evolutionary process and the individuals
    themselves.

<a id="x-28MGL-GPR-3ARANDOM-GP-EXPRESSION-20FUNCTION-29"></a>
<a id="MGL-GPR:RANDOM-GP-EXPRESSION%20FUNCTION"></a>

- \[function\] **random-gp-expression** *gp terminate-fn \&key (type (toplevel-type gp))*

    Creating the initial population by hand is tedious. This
    convenience function calls [`random-expression`][7df2] to create a random
    individual that produces `gp`'s [`toplevel-type`][5460]. By passing in another
    `type` one can create expressions that fit somewhere else in a larger
    expression, which is useful in a [`randomizer`][612d] function.

<a id="x-28MGL-GPR-3A-40GPR-GP-SEARCH-SPACE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-SEARCH-SPACE%20MGL-PAX:SECTION"></a>

### 4.5 Search Space

The search space of the GP is defined by the available operators,
literals and the type of the final result produced. The evaluator
function acts as the guiding light.

<a id="x-28MGL-GPR-3AOPERATORS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:OPERATORS%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[reader\] **operators** *[genetic-programming][4643] (:operators)*

    The set of [`operator`][b75b]s from which (together
    with [`literal`][32b7]s) individuals are built.

<a id="x-28MGL-GPR-3ALITERALS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:LITERALS%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[reader\] **literals** *[genetic-programming][4643] (:literals)*

    The set of [`literal`][32b7]s from which (together
    with [`operator`][b75b]s) individuals are built.

<a id="x-28MGL-GPR-3ATOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:TOPLEVEL-TYPE%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[reader\] **toplevel-type** *[genetic-programming][4643] (:toplevel-type = t)*

    The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol [`real`][c4ce]. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.

<a id="x-28MGL-GPR-3ACOUNT-NODES-20FUNCTION-29"></a>
<a id="MGL-GPR:COUNT-NODES%20FUNCTION"></a>

- \[function\] **count-nodes** *tree \&key internal*

    Count the nodes in the sexp `tree`. If `internal` then don't count the
    leaves.

<a id="x-28MGL-GPR-3A-40GPR-GP-REPRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-REPRODUCTION%20MGL-PAX:SECTION"></a>

### 4.6 Reproduction

The [`randomizer`][612d] and [`selector`][049c] functions define how mutation and
recombination occur.

<a id="x-28MGL-GPR-3ARANDOMIZER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:RANDOMIZER%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[reader\] **randomizer** *[genetic-programming][4643] (:randomizer)*

    Used for mutations, this is a function of three
    arguments: the GP object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.

<a id="x-28MGL-GPR-3ASELECTOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:SELECTOR%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[reader\] **selector** *[genetic-programming][4643] (:selector)*

    A function of two arguments: the GP object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it copying, mutation or crossover.
    Typically, this defers to [`hold-tournament`][d40e].

<a id="x-28MGL-GPR-3AHOLD-TOURNAMENT-20FUNCTION-29"></a>
<a id="MGL-GPR:HOLD-TOURNAMENT%20FUNCTION"></a>

- \[function\] **hold-tournament** *fitnesses \&key select-contestant-fn n-contestants key*

    Select `n-contestants` (all different) for the tournament randomly,
    represented by indices into `fitnesses` and return the one with the
    highest fitness. If `select-contestant-fn` is `nil` then contestants are
    selected randomly with uniform probability. If `select-contestant-fn`
    is a function, then it's called with `fitnesses` to return an
    index (that may or may not be already selected for the tournament).
    Specifying `select-contestant-fn` allows one to conduct 'local'
    tournaments biased towards a particular region of the index range.
    `key` is `nil` or a function that select the real fitness value from
    elements of `fitnesses`.

<a id="x-28MGL-GPR-3A-40GPR-GP-ENVIRONMENT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-GP-ENVIRONMENT%20MGL-PAX:SECTION"></a>

### 4.7 Environment

The new generation is created by applying a reproduction operator
until [`population-size`][ce25] is reached in the new generation. At each
step, a reproduction operator is randomly chosen.

<a id="x-28MGL-GPR-3ACOPY-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:COPY-CHANCE%20%28MGL-PAX:ACCESSOR%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[accessor\] **copy-chance** *[genetic-programming][4643] (:copy-chance = 0)*

    The probability of the copying reproduction
    operator being chosen. Copying simply creates an exact copy of a
    single individual.

<a id="x-28MGL-GPR-3AMUTATION-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:MUTATION-CHANCE%20%28MGL-PAX:ACCESSOR%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[accessor\] **mutation-chance** *[genetic-programming][4643] (:mutation-chance = 0)*

    The probability of the mutation reproduction
    operator being chosen. Mutation creates a randomly altered copy of
    an individual. See [`randomizer`][612d].

If neither copying nor mutation were chosen, then a crossover will
take place.

<a id="x-28MGL-GPR-3AKEEP-FITTEST-P-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
<a id="MGL-GPR:KEEP-FITTEST-P%20%28MGL-PAX:ACCESSOR%20MGL-GPR:GENETIC-PROGRAMMING%29"></a>

- \[accessor\] **keep-fittest-p** *[genetic-programming][4643] (:keep-fittest-p = t)*

    If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings.

<a id="x-28MGL-GPR-3A-40GPR-DE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-DE%20MGL-PAX:SECTION"></a>

## 5 Differential Evolution

The concepts in this section are covered by [Differential
Evolution: A Survey of the State-of-the-Art][1].

[1]: http://107.167.189.191/~piak/teaching/ec/ec2012/das-de-sota-2011.pdf

<a id="x-28MGL-GPR-3ADIFFERENTIAL-EVOLUTION-20CLASS-29"></a>
<a id="MGL-GPR:DIFFERENTIAL-EVOLUTION%20CLASS"></a>

- \[class\] **differential-evolution** *[evolutionary-algorithm][f5ee]*

    Differential evolution (DE) is an evolutionary
    algorithm in which individuals are represented by vectors of
    numbers. New individuals are created by taking linear combinations
    or by randomly swapping some of these numbers between two
    individuals.

<a id="x-28MGL-GPR-3AMAP-WEIGHTS-INTO-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
<a id="MGL-GPR:MAP-WEIGHTS-INTO-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29"></a>

- \[reader\] **map-weights-into-fn** *[differential-evolution][a4e6] (:map-weights-into-fn = #'map-into)*

    The vector of numbers (the 'weights') are most
    often stored in some kind of array. All individuals must have the
    same number of weights, but the actual representation can be
    anything as long as the function in this slot mimics the semantics
    of [`map-into`][a78a] that's the default.

<a id="x-28MGL-GPR-3ACREATE-INDIVIDUAL-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
<a id="MGL-GPR:CREATE-INDIVIDUAL-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29"></a>

- \[reader\] **create-individual-fn** *[differential-evolution][a4e6] (:create-individual-fn)*

    Holds a function of one argument, the DE, that
    returns a new individual that needs not be initialized in any way.
    Typically this just calls [`make-array`][92ab].

<a id="x-28MGL-GPR-3AMUTATE-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
<a id="MGL-GPR:MUTATE-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29"></a>

- \[reader\] **mutate-fn** *[differential-evolution][a4e6] (:mutate-fn)*

    One of the supplied mutation functions:
    [`mutate/rand/1`][720f] [`mutate/best/1`][2fdd] [`mutate/current-to-best/2`][f19b].

<a id="x-28MGL-GPR-3ACROSSOVER-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
<a id="MGL-GPR:CROSSOVER-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29"></a>

- \[reader\] **crossover-fn** *[differential-evolution][a4e6] (:crossover-fn = #'crossover/binary)*

    A function of three arguments, the DE and two
    individuals, that destructively modifies the second individual by
    using some parts of the first one. Currently, the implemented
    crossover function is [`crossover/binary`][e67a].

<a id="x-28MGL-GPR-3AMUTATE-2FRAND-2F1-20FUNCTION-29"></a>
<a id="MGL-GPR:MUTATE%2FRAND%2F1%20FUNCTION"></a>

- \[function\] **mutate/rand/1** *de current best population nursery \&key (f 0.5)*

<a id="x-28MGL-GPR-3AMUTATE-2FBEST-2F1-20FUNCTION-29"></a>
<a id="MGL-GPR:MUTATE%2FBEST%2F1%20FUNCTION"></a>

- \[function\] **mutate/best/1** *de current best population nursery \&key (f 0.5)*

<a id="x-28MGL-GPR-3AMUTATE-2FCURRENT-TO-BEST-2F2-20FUNCTION-29"></a>
<a id="MGL-GPR:MUTATE%2FCURRENT-TO-BEST%2F2%20FUNCTION"></a>

- \[function\] **mutate/current-to-best/2** *de current best population nursery \&key (f 0.5)*

<a id="x-28MGL-GPR-3ACROSSOVER-2FBINARY-20FUNCTION-29"></a>
<a id="MGL-GPR:CROSSOVER%2FBINARY%20FUNCTION"></a>

- \[function\] **crossover/binary** *de individual-1 individual-2 \&key (crossover-rate 0.5)*

    Destructively modify `individual-2` by replacement each element with
    a probability of 1 - `crossover-rate` with the corresponding element
    in `individual-1`. At least one, element is changed. Return
    `individual-2`.

<a id="x-28MGL-GPR-3ASELECT-DISTINCT-RANDOM-NUMBERS-20FUNCTION-29"></a>
<a id="MGL-GPR:SELECT-DISTINCT-RANDOM-NUMBERS%20FUNCTION"></a>

- \[function\] **select-distinct-random-numbers** *taboos n limit*

<a id="x-28MGL-GPR-3A-40GPR-DE-SANSDE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-GPR:@GPR-DE-SANSDE%20MGL-PAX:SECTION"></a>

### 5.1 SANSDE

See the paper [Self-adaptive Differential Evolution with
Neighborhood Search][1].

[1]: http://staff.ustc.edu.cn/~ketang/papers/YangTangYao_CEC08a.pdf

<a id="x-28MGL-GPR-3ASANSDE-20CLASS-29"></a>
<a id="MGL-GPR:SANSDE%20CLASS"></a>

- \[class\] **sansde** *[differential-evolution][a4e6]*

    SaNSDE is a special DE that dynamically adjust the
    crossover and mutation are performed. The only parameters are the
    generic EA ones: [`population-size`][ce25], [`evaluator`][9a7e], etc. One also has to
    specify [`map-weights-into-fn`][7ca1] and [`create-individual-fn`][1c33].

[03c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL (MGL-PAX:CLHS FUNCTION)"

[049c]: #MGL-GPR:SELECTOR%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29 "MGL-GPR:SELECTOR (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"

[057a]: #MGL-GPR:@GPR-EA%20MGL-PAX:SECTION "Evolutionary Algorithms"

[075b]: #MGL-GPR:@GPR-GP-TUTORIAL%20MGL-PAX:SECTION "Tutorial"

[084d]: #MGL-GPR:@GPR-GP-ENVIRONMENT%20MGL-PAX:SECTION "Environment"

[0cb2]: #MGL-GPR:BUILDER%20%28MGL-PAX:READER%20MGL-GPR:LITERAL%29 "MGL-GPR:BUILDER (MGL-PAX:READER MGL-GPR:LITERAL)"

[0d2f]: #MGL-GPR:@GPR-GP%20MGL-PAX:SECTION "Genetic Programming"

[0d6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL (MGL-PAX:CLHS FUNCTION)"

[12f5]: #MGL-GPR:NAME%20%28MGL-PAX:READER%20MGL-GPR:OPERATOR%29 "MGL-GPR:NAME (MGL-PAX:READER MGL-GPR:OPERATOR)"

[1602]: #MGL-GPR:POPULATION%20%28MGL-PAX:ACCESSOR%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29 "MGL-GPR:POPULATION (MGL-PAX:ACCESSOR MGL-GPR:EVOLUTIONARY-ALGORITHM)"

[1c33]: #MGL-GPR:CREATE-INDIVIDUAL-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29 "MGL-GPR:CREATE-INDIVIDUAL-FN (MGL-PAX:READER MGL-GPR:DIFFERENTIAL-EVOLUTION)"

[2e8a]: #MGL-GPR:@GPR-BACKGROUND%20MGL-PAX:SECTION "Background"

[2fdd]: #MGL-GPR:MUTATE%2FBEST%2F1%20FUNCTION "MGL-GPR:MUTATE/BEST/1 FUNCTION"

[32b7]: #MGL-GPR:LITERAL%20CLASS "MGL-GPR:LITERAL CLASS"

[40a0]: #MGL-GPR:@GPR-DE-SANSDE%20MGL-PAX:SECTION "SANSDE"

[4643]: #MGL-GPR:GENETIC-PROGRAMMING%20CLASS "MGL-GPR:GENETIC-PROGRAMMING CLASS"

[4a1e]: #MGL-GPR:ADD-INDIVIDUAL%20FUNCTION "MGL-GPR:ADD-INDIVIDUAL FUNCTION"

[4e03]: #MGL-GPR:RESULT-TYPE%20%28MGL-PAX:READER%20MGL-GPR:EXPRESSION-CLASS%29 "MGL-GPR:RESULT-TYPE (MGL-PAX:READER MGL-GPR:EXPRESSION-CLASS)"

[5460]: #MGL-GPR:TOPLEVEL-TYPE%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29 "MGL-GPR:TOPLEVEL-TYPE (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"

[5b15]: #MGL-GPR:OPERATOR%20MGL-PAX:MACRO "MGL-GPR:OPERATOR MGL-PAX:MACRO"

[5d57]: #MGL-GPR:@GPR-GP-REPRODUCTION%20MGL-PAX:SECTION "Reproduction"

[612d]: #MGL-GPR:RANDOMIZER%20%28MGL-PAX:READER%20MGL-GPR:GENETIC-PROGRAMMING%29 "MGL-GPR:RANDOMIZER (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"

[6e4d]: #MGL-GPR:@GPR-GP-LINKS%20MGL-PAX:SECTION "Links"

[6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"

[720f]: #MGL-GPR:MUTATE%2FRAND%2F1%20FUNCTION "MGL-GPR:MUTATE/RAND/1 FUNCTION"

[7ca1]: #MGL-GPR:MAP-WEIGHTS-INTO-FN%20%28MGL-PAX:READER%20MGL-GPR:DIFFERENTIAL-EVOLUTION%29 "MGL-GPR:MAP-WEIGHTS-INTO-FN (MGL-PAX:READER MGL-GPR:DIFFERENTIAL-EVOLUTION)"

[7df2]: #MGL-GPR:RANDOM-EXPRESSION%20FUNCTION "MGL-GPR:RANDOM-EXPRESSION FUNCTION"

[8a46]: #MGL-GPR:LITERAL%20MGL-PAX:MACRO "MGL-GPR:LITERAL MGL-PAX:MACRO"

[8ae0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY (MGL-PAX:CLHS FUNCTION)"

[9085]: #MGL-GPR:@GPR-GP-SEARCH-SPACE%20MGL-PAX:SECTION "Search Space"

[92ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm "MAKE-ARRAY (MGL-PAX:CLHS FUNCTION)"

[95f7]: #MGL-GPR:EXPRESSION-CLASS%20CLASS "MGL-GPR:EXPRESSION-CLASS CLASS"

[9631]: #MGL-GPR:FITTEST%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29 "MGL-GPR:FITTEST (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"

[9a7e]: #MGL-GPR:EVALUATOR%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29 "MGL-GPR:EVALUATOR (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"

[9cba]: #MGL-GPR:@GPR-EA-POPULATION%20MGL-PAX:SECTION "Populations"

[a4e6]: #MGL-GPR:DIFFERENTIAL-EVOLUTION%20CLASS "MGL-GPR:DIFFERENTIAL-EVOLUTION CLASS"

[a78a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm "MAP-INTO (MGL-PAX:CLHS FUNCTION)"

[a92c]: #MGL-GPR:ARGUMENT-TYPES%20%28MGL-PAX:READER%20MGL-GPR:OPERATOR%29 "MGL-GPR:ARGUMENT-TYPES (MGL-PAX:READER MGL-GPR:OPERATOR)"

[b60a]: #MGL-GPR:@GPR-EA-EVALUATION%20MGL-PAX:SECTION "Evaluation"

[b75b]: #MGL-GPR:OPERATOR%20CLASS "MGL-GPR:OPERATOR CLASS"

[bc41]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp.htm "COMPILE (MGL-PAX:CLHS FUNCTION)"

[be13]: #MGL-GPR:ADVANCE%20GENERIC-FUNCTION "MGL-GPR:ADVANCE GENERIC-FUNCTION"

[be2e]: #MGL-GPR:WEIGHT%20%28MGL-PAX:READER%20MGL-GPR:EXPRESSION-CLASS%29 "MGL-GPR:WEIGHT (MGL-PAX:READER MGL-GPR:EXPRESSION-CLASS)"

[c392]: #MGL-GPR:@GPR-GP-BACKGROUND%20MGL-PAX:SECTION "Background"

[c4ce]: http://www.lispworks.com/documentation/HyperSpec/Body/t_real.htm "REAL (MGL-PAX:CLHS CLASS)"

[ce25]: #MGL-GPR:POPULATION-SIZE%20%28MGL-PAX:ACCESSOR%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29 "MGL-GPR:POPULATION-SIZE (MGL-PAX:ACCESSOR MGL-GPR:EVOLUTIONARY-ALGORITHM)"

[d237]: #MGL-GPR:COUNT-NODES%20FUNCTION "MGL-GPR:COUNT-NODES FUNCTION"

[d40e]: #MGL-GPR:HOLD-TOURNAMENT%20FUNCTION "MGL-GPR:HOLD-TOURNAMENT FUNCTION"

[db20]: #MGL-GPR:@GPR-DE%20MGL-PAX:SECTION "Differential Evolution"

[e084]: #MGL-GPR:@GPR-GP-BASICS%20MGL-PAX:SECTION "Basics"

[e67a]: #MGL-GPR:CROSSOVER%2FBINARY%20FUNCTION "MGL-GPR:CROSSOVER/BINARY FUNCTION"

[e87f]: #MGL-GPR:@GPR-EA-TRAINING%20MGL-PAX:SECTION "Training"

[eee2]: http://www.lispworks.com/documentation/HyperSpec/Body/a_float.htm "FLOAT (MGL-PAX:CLHS NIL)"

[ef7a]: #MGL-GPR:MASS-EVALUATOR%20%28MGL-PAX:READER%20MGL-GPR:EVOLUTIONARY-ALGORITHM%29 "MGL-GPR:MASS-EVALUATOR (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"

[f19b]: #MGL-GPR:MUTATE%2FCURRENT-TO-BEST%2F2%20FUNCTION "MGL-GPR:MUTATE/CURRENT-TO-BEST/2 FUNCTION"

[f5ee]: #MGL-GPR:EVOLUTIONARY-ALGORITHM%20CLASS "MGL-GPR:EVOLUTIONARY-ALGORITHM CLASS"

[f9a4]: #MGL-GPR:@GPR-GP-EXPRESSIONS%20MGL-PAX:SECTION "Expressions"
