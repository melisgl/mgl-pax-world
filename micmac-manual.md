<a id="x-28MICMAC-3A-40MICMAC-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC:@MICMAC-MANUAL%20MGL-PAX:SECTION"></a>

# Micmac Manual

## Table of Contents

- [1 Introduction][5825]

    - [1.1 Overview][ac40]

    - [1.2 Links][3bf2]

- [2 Graph Search][2c8a]

    - [2.1 UCT][7169]

- [3 Metropolis Hastings][d1c6]

- [4 Game Theory][a80e]

###### \[in package MICMAC\]

<a id="x-28-22micmac-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22micmac%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- \[system\] **"micmac"**

    - *Version:* 0.0.2

    - *Description:* Micmac is mainly a library of graph search algorithms
    such as alpha-beta, UCT and beam search, but it also has some MCMC
    and other slightly unrelated stuff.

    - *Licence:* MIT, see COPYING.

    - *Author:* Gábor Melis <mega@retes.hu>

    - *Mailto:* [mega@retes.hu](mailto:mega@retes.hu)

    - *Homepage:* <http://melisgl.github.io/mgl-gpr>

    - *Bug tracker:* <https://github.com/melisgl/mgl-gpr/issues>

    - *Source control:* [GIT](https://github.com/melisgl/mgl-gpr.git)

    - *Depends on:* alexandria, [mgl-pax][6fdb]

<a id="x-28MICMAC-3A-40MICMAC-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC:@MICMAC-INTRODUCTION%20MGL-PAX:SECTION"></a>

## 1 Introduction

<a id="x-28MICMAC-3A-40MICMAC-OVERVIEW-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC:@MICMAC-OVERVIEW%20MGL-PAX:SECTION"></a>

### 1.1 Overview

MICMAC is a Common Lisp library by [Gábor
Melis](http://quotenil.com) focusing on [graph
search](http://en.wikipedia.org/wiki/Graph_traversal) algorithms.

<a id="x-28MICMAC-3A-40MICMAC-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC:@MICMAC-LINKS%20MGL-PAX:SECTION"></a>

### 1.2 Links

The official repository is <https://github.com/melisgl/micmac>, and
this document in available in various formats on
<https://fixnum.com> for the latest version.

<a id="x-28MICMAC-3A-40MICMAC-GRAPH-SEARCH-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC:@MICMAC-GRAPH-SEARCH%20MGL-PAX:SECTION"></a>

## 2 Graph Search

<a id="x-28MICMAC-3AALPHA-BETA-20FUNCTION-29"></a>
<a id="MICMAC:ALPHA-BETA%20FUNCTION"></a>

- \[function\] **alpha-beta** *state \&key (depth 0) alpha beta call-with-action maybe-evaluate-state list-actions record-best*

    Alpha-beta pruning for two player, zero-sum maximax (like minimax
    but both players maximize and the score is negated when passed
    between depths). Return the score of the game `state` from the point
    of view of the player to move at `depth` and as the second value the
    list of actions of the principal variant.

    `call-with-action` is a function of `(state depth action fn)`. It
    carries out `action` (returned by `list-actions` or `nil`) to get the
    state corresponding to `depth` and calls `fn` with that state. It may
    destructively modify `state` provided it undoes the damage after FN
    returns. `call-with-action` is called with `nil` as `action` for the
    root of the tree, in this case `state` need not be changed. `fn`
    returns the same kinds of values as `alpha-beta`. They may be useful
    for logging.

    `maybe-evaluate-state` is a function of `(state depth)`. If `state` at
    `depth` is a terminal node then it returns the score from the point of
    view of the player to move and as the second value a list of actions
    that lead from `state` to the position that was evaluated. The list of
    actions is typically empty. If we are not at a terminal node then
    `maybe-evaluate-state` returns `nil`.

    `list-actions` is a function of (`state` `depth`) and returns a non-empty
    list of legal candidate moves for non-terminal nodes. Actions are
    tried in the order `list-actions` returns them: stronger moves

    `call-with-action`, `maybe-evaluate-state` and `list-actions` are
    mandatory.

    `record-best`, if non-`nil`, is a function of `(depth score actions)`.
    It is called when at `depth` a new best action is found. `actions` is
    a list of all the actions in the principle variant corresonding to
    the newly found best score. `record-best` is useful for graceful
    degradation in case of timeout.

    `alpha` and `beta` are typically `nil` (equivalent to -infinity,
    +infinity) but any real number is allowed if the range of scores can
    be boxed.

    See `test/test-alpha-beta.lisp` for an example.

<a id="x-28MICMAC-3ABEAM-SEARCH-20FUNCTION-29"></a>
<a id="MICMAC:BEAM-SEARCH%20FUNCTION"></a>

- \[function\] **beam-search** *start-nodes \&key max-depth (n-solutions 1) (beam-width (length start-nodes)) expand-node-fn expand-beam-fn score-fn upper-bound-fn solutionp-fn (finishedp-fn solutionp-fn)*

    In a graph, search for nodes that with the best scores with [beam
    search](http://en.wikipedia.org/wiki/Beam_search). That is, starting
    from `start-nodes` perform a breadth-first search but at each depth
    only keep `beam-width` number of nodes with the best scores. Keep the
    best `n-solutions` (at most) complete solutions. Discard nodes known
    to be unable to get into the best `n-solutions` (due to
    `upper-bound-fn`). Finally, return the solutions and the active
    nodes (the *beam*) as adjustable arrays sorted by score in
    descending order.

    `start-nodes` (a sequence of elements of arbitrary type). `score-fn`,
    `upper-bound-fn`, `solutionp-fn`, `finishedp-fn` are all functions of one
    argument: the node. `solutionp-fn` checks whether a node represents a
    complete solution (i.e. some goal is reached). `score-fn` returns a
    real number that's to be maximized, it's only called for node for
    which `solutionp-fn` returned true. `upper-bound-fn` (if not `nil`)
    returns a real number that equal or greater than the score of all
    solutions reachable from that node. `finishedp-fn` returns true iff
    there is nowhere to go from the node.

    `expand-node-fn` is also a function of a single node argument. It
    returns a sequence of nodes to 'one step away' from its argument
    node. `expand-beam-fn` is similar, but it takes a vector of nodes and
    returns all nodes one step away from any of them. It's enough
    provide either `expand-node-fn` or `expand-beam-fn`. The purpose of
    `expand-beam-fn`. is to allow more efficient, batch-like operations.

    See `test/test-beam-search.lisp` for an example.

<a id="x-28MICMAC-3APARALLEL-BEAM-SEARCH-20FUNCTION-29"></a>
<a id="MICMAC:PARALLEL-BEAM-SEARCH%20FUNCTION"></a>

- \[function\] **parallel-beam-search** *start-node-seqs \&key max-depth (n-solutions 1) beam-width expand-node-fn expand-beams-fn score-fn upper-bound-fn solutionp-fn (finishedp-fn solutionp-fn)*

    This is very much like [`beam-search`][aab6] except it solves a number of
    instances of the same search problem starting from different sets of
    nodes. The sole purpose of `parallel-beam-search` is to amortize the
    cost `expand-beam-fn` if possible.

    `expand-beams-fn` is called with sequence of beams (i.e. it's a
    sequence of sequence of nodes) and it must return another sequence
    of sequences of nodes. Each element of the returned sequence is the
    reachable nodes of the nodes in the corresponding element of its
    argument sequence.

    `parallel-beam-search` returns a sequence of solutions sequences, and
    a sequence of active node sequences.

    See `test/test-beam-search.lisp` for an example.

<a id="x-28MICMAC-2EUCT-3A-40MICMAC-UCT-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC.UCT:@MICMAC-UCT%20MGL-PAX:SECTION"></a>

### 2.1 UCT

###### \[in package MICMAC.UCT\]

[`uct`][e061] Monte Carlo tree search. This is what makes current Go programs
tick. And Hex programs as well, for that matter. This is a cleanup
and generalization of code originally created in course of the
Google AI Challenge 2010.

For now, the documentation is just a reference. See
`test/test-uct.lisp` for an example.

<a id="x-28MICMAC-2EUCT-3AUCT-NODE-20CLASS-29"></a>
<a id="MICMAC.UCT:UCT-NODE%20CLASS"></a>

- \[class\] **uct-node**

    A node in the [`uct`][e061] tree. Roughly translates to a
    state in the search space. Note that the state itself is not stored
    explicity, but it can be recovered by \`replaying' the actions from
    the starting state or by customizing [`make-uct-node`][496c].

<a id="x-28MICMAC-2EUCT-3ADEPTH-20-28MGL-PAX-3AREADER-20MICMAC-2EUCT-3AUCT-NODE-29-29"></a>
<a id="MICMAC.UCT:DEPTH%20%28MGL-PAX:READER%20MICMAC.UCT:UCT-NODE%29"></a>

- \[reader\] **depth** *[uct-node][5b77] (:depth = 0)*

<a id="x-28MICMAC-2EUCT-3AEDGES-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-NODE-29-29"></a>
<a id="MICMAC.UCT:EDGES%20%28MGL-PAX:ACCESSOR%20MICMAC.UCT:UCT-NODE%29"></a>

- \[accessor\] **edges** *[uct-node][5b77]*

    Outgoing edges.

<a id="x-28MICMAC-2EUCT-3AAVERAGE-REWARD-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-NODE-29-29"></a>
<a id="MICMAC.UCT:AVERAGE-REWARD%20%28MGL-PAX:ACCESSOR%20MICMAC.UCT:UCT-NODE%29"></a>

- \[accessor\] **average-reward** *[uct-node][5b77] (:average-reward = 0)*

    Average reward over random playouts started from
    below this node. See [`update-uct-statistics`][76b0] and REWARD.

<a id="x-28MICMAC-2EUCT-3AUCT-EDGE-20CLASS-29"></a>
<a id="MICMAC.UCT:UCT-EDGE%20CLASS"></a>

- \[class\] **uct-edge**

    An edge in the [`uct`][e061] tree. Represents an action taken
    from a state. The value of an action is the value of its target
    state which is not quite as generic as it could be; feel free to
    specialize [`average-reward`][7c34] for the edges if that's not the case.

<a id="x-28MICMAC-2EUCT-3AACTION-20-28MGL-PAX-3AREADER-20MICMAC-2EUCT-3AUCT-EDGE-29-29"></a>
<a id="MICMAC.UCT:ACTION%20%28MGL-PAX:READER%20MICMAC.UCT:UCT-EDGE%29"></a>

- \[reader\] **action** *[uct-edge][86c7] (:action)*

    The action represented by the edge.

<a id="x-28MICMAC-2EUCT-3AFROM-NODE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-EDGE-29-29"></a>
<a id="MICMAC.UCT:FROM-NODE%20%28MGL-PAX:ACCESSOR%20MICMAC.UCT:UCT-EDGE%29"></a>

- \[accessor\] **from-node** *[uct-edge][86c7] (:from-node)*

    The node this edge starts from.

<a id="x-28MICMAC-2EUCT-3ATO-NODE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EUCT-3AUCT-EDGE-29-29"></a>
<a id="MICMAC.UCT:TO-NODE%20%28MGL-PAX:ACCESSOR%20MICMAC.UCT:UCT-EDGE%29"></a>

- \[accessor\] **to-node** *[uct-edge][86c7] (= nil)*

    The node this edge points to if the edge has been
    visited or `nil`.

<a id="x-28MICMAC-2EUCT-3AVISITED-EDGES-20FUNCTION-29"></a>
<a id="MICMAC.UCT:VISITED-EDGES%20FUNCTION"></a>

- \[function\] **visited-edges** *node*

<a id="x-28MICMAC-2EUCT-3AUNVISITED-EDGES-20FUNCTION-29"></a>
<a id="MICMAC.UCT:UNVISITED-EDGES%20FUNCTION"></a>

- \[function\] **unvisited-edges** *node*

<a id="x-28MICMAC-2EUCT-3AEDGE-SCORE-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:EDGE-SCORE%20GENERIC-FUNCTION"></a>

- \[generic-function\] **edge-score** *node edge exploration-bias*

<a id="x-28MICMAC-2EUCT-3ASELECT-EDGE-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:SELECT-EDGE%20GENERIC-FUNCTION"></a>

- \[generic-function\] **select-edge** *node exploration-bias*

    Choose an action to take from a state, in other
    words an edge to follow from `node` in the tree. The default
    implementation chooses randomly from the yet unvisited edges or if
    there is none moves down the edge with the maximum [`edge-score`][ba95]. If
    you are thinking of customizing this, for example to make it choose
    the minimum at odd depths, the you may want to consider specializing
    REWARD or [`update-uct-statistics`][76b0] instead.

<a id="x-28MICMAC-2EUCT-3AOUTCOME--3EREWARD-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:OUTCOME-%3EREWARD%20GENERIC-FUNCTION"></a>

- \[generic-function\] **outcome->reward** *node outcome*

    Compute the reward for a node in the tree from
    `outcome` that is the result of a playout. This is called by the
    default implementation of [`update-uct-statistics`][76b0]. This is where one
    typically negates depending on the parity of [`depth`][7698] in two player
    games.

<a id="x-28MICMAC-2EUCT-3AUPDATE-UCT-STATISTICS-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:UPDATE-UCT-STATISTICS%20GENERIC-FUNCTION"></a>

- \[generic-function\] **update-uct-statistics** *root path outcome*

    Increment the number of visits and update the
    average reward in nodes and edges of `path`. By default, edges simply
    get their visit counter incremented while nodes also get an update
    to [`average-reward`][7c34] based on what [`outcome->reward`][532e] returns.

<a id="x-28MICMAC-2EUCT-3AMAKE-UCT-NODE-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:MAKE-UCT-NODE%20GENERIC-FUNCTION"></a>

- \[generic-function\] **make-uct-node** *parent edge parent-state*

    Create a node representing the state that `edge`
    leads to (from `parent`). Specialize this if you want to keep track of
    the state, which is not done by default as it can be expensive,
    especially in light of TAKE-ACTION mutating it. The default
    implementation simply creates an instance of the class of `parent` so
    that one can start from a subclass of [`uct-node`][5b77] and be sure that that
    class is going to be used for nodes below it.

<a id="x-28MICMAC-2EUCT-3ASTATE-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:STATE%20GENERIC-FUNCTION"></a>

- \[generic-function\] **state** *node parent edge parent-state*

    Return the state that corresponds to `node`. This is
    not a straightforward accessor unless `node` is customized to store
    it. The rest of the parameters are provided so that one can
    reconstruct the state by taking the action of `edge` in the
    `parent-state` of `parent`. It's allowed to mutate `parent-state` and
    return it. This function must be specialized.

<a id="x-28MICMAC-2EUCT-3ALIST-EDGES-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:LIST-EDGES%20GENERIC-FUNCTION"></a>

- \[generic-function\] **list-edges** *node state*

    Return a list of edges representing the possible
    actions from `node` with `state`. This function must be customized.

<a id="x-28MICMAC-2EUCT-3APLAY-OUT-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.UCT:PLAY-OUT%20GENERIC-FUNCTION"></a>

- \[generic-function\] **play-out** *node state reverse-path*

    Play a random game from `node` with `state` and return
    the outcome that's fed into [`update-uct-statistics`][76b0]. The way the
    random game is played is referred to as \`default policy' and that's
    what makes or breaks [`uct`][e061] search. This function must be
    customized.

<a id="x-28MICMAC-2EUCT-3AUCT-20FUNCTION-29"></a>
<a id="MICMAC.UCT:UCT%20FUNCTION"></a>

- \[function\] **uct** *\&key root fresh-root-state exploration-bias max-n-playouts*

    Starting from the `root` node, search the tree expanding it one node
    for each playout. Finally return the mutated `root`. `root` may be the
    root node of any tree, need not be a single node with no edges.
    `fresh-root-state` is a function that returns a fresh state
    corresponding to `root`. This state will be destroyed unless special
    care is taken in [`state`][a04f].

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3A-40MICMAC-METROPOLIS-HASTINGS-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:@MICMAC-METROPOLIS-HASTINGS%20MGL-PAX:SECTION"></a>

## 3 Metropolis Hastings

###### \[in package MICMAC.METROPOLIS-HASTINGS with nicknames MICMAC.MH\]

Generic interface for the Metropolis-Hastings algorithm, also
Metropolis Coupled MCMC.

References:

- http://en.wikipedia.org/wiki/Metropolis–Hastings\_algorithm

- Markov Chain Monte Carlo and Gibbs Sampling
  Lecture Notes for EEB 581, version 26 April 2004 c B. Walsh 2004
  http://web.mit.edu/~wingated/www/introductions/mcmc-gibbs-intro.pdf

- Geyer, C.J. (1991) Markov chain Monte Carlo maximum likelihood

For now, the documentation is just a reference. See
`test/test-metropolis-hastings.lisp` for an example.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-20CLASS-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:MC-CHAIN%20CLASS"></a>

- \[class\] **mc-chain**

    A simple markov chain for Metropolis Hastings. With
    temperature it is suitable for `mc3`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ATEMPERATURE-20-28MGL-PAX-3AACCESSOR-20MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-29-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:TEMPERATURE%20%28MGL-PAX:ACCESSOR%20MICMAC.METROPOLIS-HASTINGS:MC-CHAIN%29"></a>

- \[accessor\] **temperature** *[mc-chain][ee90] (:temperature = 1.0d0)*

    The PROBABILITY-RATIO of samples is raised to the
    power of 1 / `temperature` before calculating the acceptance
    probability. This effectively flattens the peaks if `temperature` >
    1 which makes it easier for the chain to traverse deep valleys.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ASTATE-20-28MGL-PAX-3AREADER-20MICMAC-2EMETROPOLIS-HASTINGS-3AMC-CHAIN-29-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:STATE%20%28MGL-PAX:READER%20MICMAC.METROPOLIS-HASTINGS:MC-CHAIN%29"></a>

- \[reader\] **state** *[mc-chain][ee90] (:state)*

    This is the current sample where the chain is.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-TO-SAMPLE-20FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:JUMP-TO-SAMPLE%20FUNCTION"></a>

- \[function\] **jump-to-sample** *chain jump \&key (result-sample (state chain))*

    From the current state of `chain` make `jump` (from the current
    distribution of `chain`) and return the sample where we landed. Reuse
    `result-sample` when possible. 

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-TO-SAMPLE-2A-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:JUMP-TO-SAMPLE*%20GENERIC-FUNCTION"></a>

- \[generic-function\] **jump-to-sample\*** *chain jump result-sample*

    This function is called by [`jump-to-sample`][90e6]. It is
    where `jump-to-sample` behaviour shall be customized.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3APREPARE-JUMP-DISTRIBUTION-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:PREPARE-JUMP-DISTRIBUTION%20GENERIC-FUNCTION"></a>

- \[generic-function\] **prepare-jump-distribution** *chain*

    Prepare for sampling from the F(X) = Q(SAMPLE->X)
    distribution. Called by [`random-jump`][0676]. The around method ensures that
    nothing is done unless there was a state change.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ARANDOM-JUMP-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:RANDOM-JUMP%20GENERIC-FUNCTION"></a>

- \[generic-function\] **random-jump** *chain*

    Sample a jump from the current distribution of
    jumps that was computed by [`prepare-jump-distribution`][28a5].

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-PROBABILITY-RATIO-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:LOG-PROBABILITY-RATIO%20GENERIC-FUNCTION"></a>

- \[generic-function\] **log-probability-ratio** *chain sample1 sample2*

    Return P(`sample1`)/P(`sample2`). It's in the log
    domain to avoid overflows and the ratio part is because that it may
    allow computational shortcuts as opposed to calculating unnormalized
    probabilities separately.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-PROBABILITY-RATIO-TO-JUMP-TARGET-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:LOG-PROBABILITY-RATIO-TO-JUMP-TARGET%20GENERIC-FUNCTION"></a>

- \[generic-function\] **log-probability-ratio-to-jump-target** *chain jump target*

    Return P(`target`)/P([`state`][bd48]) where `jump` is from the
    current state of `chain` to `target` sample. This can be specialized for
    speed. The default implementation just falls back on
    [`log-probability-ratio`][5e10].

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ALOG-JUMP-PROBABILITY-RATIO-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:LOG-JUMP-PROBABILITY-RATIO%20GENERIC-FUNCTION"></a>

- \[generic-function\] **log-jump-probability-ratio** *chain jump target*

    Return Q(TARGET->STATE) / Q(STATE->TARGET) where Q
    is the jump distribution and `jump` is from the current [`state`][bd48] of `chain`
    to `target` sample.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPTANCE-PROBABILITY-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:ACCEPTANCE-PROBABILITY%20GENERIC-FUNCTION"></a>

- \[generic-function\] **acceptance-probability** *chain jump candidate*

    Calculate the acceptance probability of `candidate`
    to which `jump` leads from the current [`state`][bd48] of `chain`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPT-JUMP-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:ACCEPT-JUMP%20GENERIC-FUNCTION"></a>

- \[generic-function\] **accept-jump** *chain jump candidate*

    Called when `chain` accepts `jump` to `candidate`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AREJECT-JUMP-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:REJECT-JUMP%20GENERIC-FUNCTION"></a>

- \[generic-function\] **reject-jump** *chain jump candidate*

    Called when `chain` rejects `jump` to `candidate`. It
    does nothing by default, it's just a convenience for debugging.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMAYBE-JUMP-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:MAYBE-JUMP%20GENERIC-FUNCTION"></a>

- \[generic-function\] **maybe-jump** *chain jump candidate acceptance-probability*

    Randomly accept or reject `jump` to `candidate` from
    the current state of `chain` with `acceptance-probability`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:JUMP%20GENERIC-FUNCTION"></a>

- \[generic-function\] **jump** *chain*

    Take a step on the markov chain. Return a boolean
    indicating whether the proposed jump was accepted.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMC3-CHAIN-20CLASS-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:MC3-CHAIN%20CLASS"></a>

- \[class\] **mc3-chain** *[mc-chain][ee90]*

    High probability island separated by low valley
    make the chain poorly mixing. `mc3-chain` has a number of `hot-chains`
    that have state probabilities similar to that of the main chain but
    less jagged. Often it suffices to set the temperatures of the
    `hot-chains` higher use the very same base probability
    distribution.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AACCEPT-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:ACCEPT-SWAP-CHAIN-STATES%20GENERIC-FUNCTION"></a>

- \[generic-function\] **accept-swap-chain-states** *mc3 chain1 chain2*

    Swap the states of `chain1` and `chain2` of `mc3`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AREJECT-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:REJECT-SWAP-CHAIN-STATES%20GENERIC-FUNCTION"></a>

- \[generic-function\] **reject-swap-chain-states** *mc3 chain1 chain2*

    Called when the swap of states of `chain1` and `chain2`
    is rejected. It does nothing by default, it's just a convenience for
    debugging.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AMAYBE-SWAP-CHAIN-STATES-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:MAYBE-SWAP-CHAIN-STATES%20GENERIC-FUNCTION"></a>

- \[generic-function\] **maybe-swap-chain-states** *mc3 chain1 chain2 acceptance-probability*

    Swap of states of `chain1` and `chain2` of `mc3` with
    `acceptance-probability`.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AJUMP-BETWEEN-CHAINS-20GENERIC-FUNCTION-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:JUMP-BETWEEN-CHAINS%20GENERIC-FUNCTION"></a>

- \[generic-function\] **jump-between-chains** *mc3*

    Choose two chains randomly and swap their states
    with `mc3` acceptance probability.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3AENUMERATING-CHAIN-20CLASS-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:ENUMERATING-CHAIN%20CLASS"></a>

- \[class\] **enumerating-chain** *[mc-chain][ee90]*

    A simple abstract chain subclass that explicitly
    enumerates the probabilities of the distribution.

<a id="x-28MICMAC-2EMETROPOLIS-HASTINGS-3ATRACING-CHAIN-20CLASS-29"></a>
<a id="MICMAC.METROPOLIS-HASTINGS:TRACING-CHAIN%20CLASS"></a>

- \[class\] **tracing-chain**

    Mix this in with your chain to have it print trace
    of acceptances/rejections.

<a id="x-28MICMAC-2EGAME-THEORY-3A-40MICMAC-GAME-THEORY-20MGL-PAX-3ASECTION-29"></a>
<a id="MICMAC.GAME-THEORY:@MICMAC-GAME-THEORY%20MGL-PAX:SECTION"></a>

## 4 Game Theory

###### \[in package MICMAC.GAME-THEORY\]

<a id="x-28MICMAC-2EGAME-THEORY-3AFIND-NASH-EQUILIBRIUM-20FUNCTION-29"></a>
<a id="MICMAC.GAME-THEORY:FIND-NASH-EQUILIBRIUM%20FUNCTION"></a>

- \[function\] **find-nash-equilibrium** *payoff \&key (n-iterations 100)*

    Find a Nash equilibrium of a zero-sum game represented by `payoff`
    matrix (a 2d matrix or a nested list). `payoff` is from the point of
    view of the row player: the player who choses column wants to
    minimize, the row player wants to maximize. The first value returned
    is a vector of unnormalized probabilities assigned to each action of
    the row player, the second value is the same for the column player
    and the third is the expected payoff of the row player in the nash
    equilibrium represented by the oddment vectors.

[0676]: #MICMAC.METROPOLIS-HASTINGS:RANDOM-JUMP%20GENERIC-FUNCTION "MICMAC.METROPOLIS-HASTINGS:RANDOM-JUMP GENERIC-FUNCTION"

[28a5]: #MICMAC.METROPOLIS-HASTINGS:PREPARE-JUMP-DISTRIBUTION%20GENERIC-FUNCTION "MICMAC.METROPOLIS-HASTINGS:PREPARE-JUMP-DISTRIBUTION GENERIC-FUNCTION"

[2c8a]: #MICMAC:@MICMAC-GRAPH-SEARCH%20MGL-PAX:SECTION "Graph Search"

[3bf2]: #MICMAC:@MICMAC-LINKS%20MGL-PAX:SECTION "Links"

[496c]: #MICMAC.UCT:MAKE-UCT-NODE%20GENERIC-FUNCTION "MICMAC.UCT:MAKE-UCT-NODE GENERIC-FUNCTION"

[532e]: #MICMAC.UCT:OUTCOME-%3EREWARD%20GENERIC-FUNCTION "MICMAC.UCT:OUTCOME->REWARD GENERIC-FUNCTION"

[5825]: #MICMAC:@MICMAC-INTRODUCTION%20MGL-PAX:SECTION "Introduction"

[5b77]: #MICMAC.UCT:UCT-NODE%20CLASS "MICMAC.UCT:UCT-NODE CLASS"

[5e10]: #MICMAC.METROPOLIS-HASTINGS:LOG-PROBABILITY-RATIO%20GENERIC-FUNCTION "MICMAC.METROPOLIS-HASTINGS:LOG-PROBABILITY-RATIO GENERIC-FUNCTION"

[6fdb]: pax-manual.md#%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"

[7169]: #MICMAC.UCT:@MICMAC-UCT%20MGL-PAX:SECTION "UCT"

[7698]: #MICMAC.UCT:DEPTH%20%28MGL-PAX:READER%20MICMAC.UCT:UCT-NODE%29 "MICMAC.UCT:DEPTH (MGL-PAX:READER MICMAC.UCT:UCT-NODE)"

[76b0]: #MICMAC.UCT:UPDATE-UCT-STATISTICS%20GENERIC-FUNCTION "MICMAC.UCT:UPDATE-UCT-STATISTICS GENERIC-FUNCTION"

[7c34]: #MICMAC.UCT:AVERAGE-REWARD%20%28MGL-PAX:ACCESSOR%20MICMAC.UCT:UCT-NODE%29 "MICMAC.UCT:AVERAGE-REWARD (MGL-PAX:ACCESSOR MICMAC.UCT:UCT-NODE)"

[86c7]: #MICMAC.UCT:UCT-EDGE%20CLASS "MICMAC.UCT:UCT-EDGE CLASS"

[90e6]: #MICMAC.METROPOLIS-HASTINGS:JUMP-TO-SAMPLE%20FUNCTION "MICMAC.METROPOLIS-HASTINGS:JUMP-TO-SAMPLE FUNCTION"

[a04f]: #MICMAC.UCT:STATE%20GENERIC-FUNCTION "MICMAC.UCT:STATE GENERIC-FUNCTION"

[a80e]: #MICMAC.GAME-THEORY:@MICMAC-GAME-THEORY%20MGL-PAX:SECTION "Game Theory"

[aab6]: #MICMAC:BEAM-SEARCH%20FUNCTION "MICMAC:BEAM-SEARCH FUNCTION"

[ac40]: #MICMAC:@MICMAC-OVERVIEW%20MGL-PAX:SECTION "Overview"

[ba95]: #MICMAC.UCT:EDGE-SCORE%20GENERIC-FUNCTION "MICMAC.UCT:EDGE-SCORE GENERIC-FUNCTION"

[bd48]: #MICMAC.METROPOLIS-HASTINGS:STATE%20%28MGL-PAX:READER%20MICMAC.METROPOLIS-HASTINGS:MC-CHAIN%29 "MICMAC.METROPOLIS-HASTINGS:STATE (MGL-PAX:READER MICMAC.METROPOLIS-HASTINGS:MC-CHAIN)"

[d1c6]: #MICMAC.METROPOLIS-HASTINGS:@MICMAC-METROPOLIS-HASTINGS%20MGL-PAX:SECTION "Metropolis Hastings"

[e061]: #MICMAC.UCT:UCT%20FUNCTION "MICMAC.UCT:UCT FUNCTION"

[ee90]: #MICMAC.METROPOLIS-HASTINGS:MC-CHAIN%20CLASS "MICMAC.METROPOLIS-HASTINGS:MC-CHAIN CLASS"
