# Contributing

Contributions to `manynet`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `manynet` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Contents

Each entry says when to read the section.
Update this list whenever a heading below is added, renamed, or moved.

- [Identifying issues](#identifying-issues): before you report a bug or ask for a feature
- [Package architecture](#package-architecture)
  - [Project overview](#project-overview): to decide whether a function belongs here
    or in `{netrics}`, `{autograph}`, or `{migraph}`
- [Design principles](#design-principles)
  - [Declarative simplicity](#declarative-simplicity): before you name a function or an argument
  - [Pleasant predictability](#pleasant-predictability): before you choose a prefix,
    an argument order, or what a function returns
  - [Graceful inclusivity](#graceful-inclusivity): before you decide what a function accepts,
    and when it fails
- [Developing](#developing)
  - [Common commands](#common-commands): to load, document, test, or check the package
  - [File organization](#file-organization-file-naming--function-family): to find a function,
    or the file for a new one
  - [The `stocnet` object model](#the-stocnet-object-model): before you read or write the
    components of a `stocnet`, or its missing data
  - [Two-mode networks](#two-mode-networks): before you handle modes or their direction
  - [Console messaging](#console-messaging): before you write an error, a warning, or a message
  - [Tests](#tests): before you add a test, or when one is skipped with `AUDIT`
  - [Datasets](#datasets): before you add, convert, or document a dataset
  - [Deprecation](#deprecation): before you rename, move, or remove a function or an argument
- [Documenting](#documenting)
  - [README and Website](#readme-and-website): before you edit the README, a tutorial,
    or the website index, and whenever you export a function
  - [`NEWS.md` conventions](#newsmd-conventions): before you write a `NEWS.md` entry
    or a commit message
- [Releasing](#releasing)
  - [Branching and CI](#branching-and-ci): to choose a branch, or to read a failed check
  - [Checking downstream packages](#checking-downstream-packages): before you change shipped data,
    a class, or what a function returns
  - [Releasing to CRAN](#releasing-to-cran): before a submission to CRAN
- [Git for new contributors](#git-for-new-contributors): if git is new to you

## Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

## Package architecture

### Project overview

`manynet` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem)
providing the *data layer* for network analysis: tools to make, manipulate, and modify
many kinds of networks — one-mode/two-mode, directed/undirected, weighted, signed,
multiplex, multimodal, and longitudinal/dynamic.
Functions work across representations (matrices, edgelists, `{igraph}`, `{network}`,
`{tidygraph}`) via a lossless coercion layer,
so most functions call `as_igraph()`/`as_tidygraph()` etc. internally
rather than assuming a single class.
Division of labour to keep in mind when adding functions:

- `{manynet}` (this package): network classes/coercion (`as_*()`), making and reading
  networks, manipulating and modifying nodes, ties and attributes,
  and network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- `{netrics}`: everything analytic — marks, measures, memberships, motifs —
  at the node, tie, and network level.
- `{autograph}`: functions for drawing graphs and plotting network analytic or
  modelling results and diagnostics, along with deep (often institutional) theming.
  All plot methods should live there.
- `{migraph}`: functions for testing and modelling, e.g. QAP/MRQAP and diffusion models.

Where a function computes how alike two nodes are, the line between `{manynet}`
and `{netrics}` runs as follows.
Arithmetic over a network's own cells — comparing two nodes' profiles of ties or
affiliations, as `to_mode1()` and `to_proximity()` do — is a transformation of
the data, and belongs here as a `to_*()` function.
Algorithms that iterate or recurse to a fixed point — RoleSim, REGE, CONCOR —
are analytic, and belong in `{netrics}`,
which calls into this package's `to_*()` functions for the profile arithmetic
they need.

## Design principles

All `stocnet` packages aim for the same three things:
declarative simplicity, pleasant predictability, and graceful inclusivity.
The principles are shared across the packages;
the rules and examples below are those of `{manynet}`.
### Declarative simplicity

Functions should do one clearly named thing with few arguments.
This helps users orient themselves in the package and develop expectations
that are not tied to having to read deep into the documentation.

- **Function names describe actions.**
  A function is named after what it does or the mechanism it follows,
  not its progenitors, any abbreviations or model acronyms,
  and the originating authors are credited in its documentation.
  Example: `generate_smallworld()` and `generate_scalefree()`
  rather than names built on Watts and Strogatz or Barabási and Albert.
- **Features are detected, not declared.**
  A function finds out whether a network is directed, weighted, signed, or two-mode,
  and adjusts, so the user does not (need to) supply an argument for it.
  Example: `is_twomode()` and `is_directed()` are called inside most `to_*()` functions,
  and branch how the function handles the data.
  Functions use as much data as they are given, so if there is a weighted version,
  then this will be detected and used unless declared otherwise.
- **Few arguments per function.**
  Every argument is something the user must read about before they can trust the result.
  Arguments that state detected features are unnecessary, for example.
  Where a variant relies on network features (above),
  the result can be changed by explicitly changing the type of network it is:
  e.g. `to_undirected()`, `to_unweighted()`, or `to_unsigned()`.
  Where a variant only adjusts the same operation, keep it as an argument
  and reuse the name the package already has for that idea,
  e.g. `similarity` in `to_mode1()`, or `rule`, or `connectivity`.
- **One word means one thing.**
  The vocabulary is small on purpose, so some words are reserved.
  An argument that selects how a function decides something is `rule`, not `method`,
  which already means an S3 method and a statistical method.
  "mode" means the mode of a two-mode or multimodal network,
  so the most frequent value is `"modal"`,
  and the weak or strong components of a network are chosen with `connectivity`.
  Functions that assert or count connectedness default to `"strong"`
  (e.g. `is_connected()`), and functions that scope or split a network to `"weak"`
  (e.g. `to_giant()`, `to_components()`).
  Before adding an option, look for an existing signature that carries the same idea,
  and reuse its name and, where possible, its values.
- **Scope is signalled by prefix/suffix/infix and shared across all data classes.**
  The prefix says which level of the network a function describes or changes.
  Example: `mutate_nodes()` and `mutate_ties()` say which table they change.

### Pleasant predictability

Users should be able to guess a function's behaviour from its family.

- **Families share a verb prefix.**
  Each prefix implies a type of operation and a type of return value:
  `read_*()` and `write_*()` import and export, `collect_*()` gathers a network from a source,
  `as_*()` converts class, `to_*()` transforms within class, and `is_*()` returns a single logical.
  See [File organization](#file-organization-file-naming--function-family) for the full set.
- **Deterministic and stochastic generation are separated.**
  `create_*()` always yields the same network, 
  `generate_*()` draws a random one, and `play_*()` simulates a process on one.
- **Arguments follow a consistent order.**
  Where a function takes a network, it comes first, as `.data`.
  The making functions take `n` first, then `p`, `directed`, and `width`,
  wherever a function has them.
  Example: `create_ring(n, directed, width)` and `generate_random(n, p, directed)`.
  Document shared arguments once, as a `man-roxygen/param_*.R` template
  (e.g. `param_n`, `param_p`, `param_directed`, `param_width`, `param_groups`,
  `param_degree`, and `return_make` for what the making functions return),
  with the detail of a page given by `@templateVar`.
- **Outputs match the prefix and the input.**
  A `to_*()` function returns the class it was given,
  so that it can be dropped into an existing workflow.

### Graceful inclusivity

Functions should accept whatever network users bring and return something they can use.

- **Inputs span all common classes.**
  Functions accept matrices, edgelists, and `igraph`, `network`, `tidygraph`,
  `stocnet`, and `siena` objects, and each class has its `as_*()` function
  for when another package requires it.
  Conversion is lossless wherever the target class can hold the information.
- **Only truly unsupported cases fail.**
  A function with nothing to do returns its input rather than an error,
  which leaves users fewer failures to worry about and means they can use
  coercions as guarantees.
  Example: `to_undirected()` on an undirected network and `to_mode1()` on a one-mode
  network both return the network as it was.
  Where a function really cannot proceed, the error says why,
  and where possible suggests a conversion that would make it work
  (see [Console messaging](#console-messaging)).
- **Key steps are reported.**
  Functions say what they did, 
  such as a defaulted argument or an attribute tidied to `stocnet` conventions,
  at the verbosity the user has chosen.
- **Networks describe themselves.**
  A `stocnet` object records its metadata, including its name, source, DOI, date,
  and how it has been transformed since it was collected,
  and prints a description built from this rather than a dump of its contents.
  Example: `describe_transformations(to_undirected(ison_southern_women))`.
  The record of transformations follows section 4 of the GRAND guidelines.

## Developing

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Install dependencies / load for development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "coercion")` (matches `test-coercion.R`),
  or `testthat::test_file("tests/testthat/test-make_create.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Code coverage: `covr::package_coverage()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.

Run R in a UTF-8 locale, e.g. `LANG=en_US.UTF-8 Rscript -e "devtools::document()"`.
Under the C locale, `devtools::document()` writes byte escapes into `man/`
in place of quotation marks and accented letters, and tibbles print differently.

Some files are generated rather than edited directly — `README.md`, the tutorial
articles in `vignettes/articles/`, and the cheatsheet.
See [README and Website](#readme-and-website) below for which source each is built from.

### File organization (file naming = function family)

`R/` files are prefixed by the verb-family of the functions they contain,
not by data structure. When looking for a function, search by what it *does*:

| Prefix | Contains |
|---|---|
| `make_*.R` | creating/reading/generating networks: `create_*()` (deterministic structures), `generate_*()` (stochastic mechanisms), `read_*()`/`write_*()` (import/export), `collect_*()` (gathering a network from a source, `make_collect.R`), `play_*()` (diffusion/learning simulations) |
| `data_*.R` | documentation of the bundled datasets: `ison_*` classic/instructional, `fict_*` fictional, `irps_*` international-relations (see [Datasets](#datasets)) |
| `coerce_graph.R`, `coerce_list.R`, `coerce_matrix.R` | the `as_*()` translation layer between representations (`as_igraph()`, `as_tidygraph()`, `as_network()`, `as_matrix()`, `as_edgelist()`, `as_siena()`, `as_diffnet()`, …), implemented as S3 methods dispatching on input class |
| `class_*.R` | the network classes themselves (`class_stocnet.R`: `make_stocnet()`, `print.stocnet()`, the info/nodes/ties/changes/global data model; `class_networks.R`: the legacy `mnet` class, `print.mnet`, `$`/`$<-` accessors), related result classes (`class_measures.R`, `class_members.R`, `class_motifs.R`, `class_models.R`), the `snet_*()` CLI layer (`class_interface.R`), input validation (`class_validate.R`: `validate_stocnet()` and its component validators), and the `describe_*()` helpers behind the print methods (`class_describe.R`) |
| `manip_*.R` | dplyr-style verbs for manipulating nodes/ties/attributes (`manip_nodes.R`, `manip_ties.R`, `manip_globals.R`, `manip_info.R`, `manip_changes.R`) |
| `mark_*.R` | logical/predicate functions returning `TRUE`/`FALSE` or marks about a network, e.g. the `is_*()` family (`mark_classes.R`, `mark_features.R`, `mark_format.R`, `mark_changes.R`) |
| `modif_*.R` | structural transformations/reformatting: direction, weighting, projection, splitting (`to_*()`, `modif_split.R`) and joining (`from_*()`, `modif_from.R`), path/level operations, missing data (`modif_direction.R`, `modif_project.R`, `modif_miss.R`, …) |
| `measure_*.R` | descriptive/attribute measures (`measure_attributes.R`, `measure_properties.R`); heavier network-analytic measures live in `{netrics}` |
| `reexports_classes.R` | re-exports of classes/generics from `{igraph}`/`{tidygraph}`/`{network}` so users need not load those packages directly |
| `manynet-utils.R`, `manynet-glossary.R`, `manynet-defunct.R` | shared internal helpers, the `gloss()`/glossary system used in tutorials and docs, and defunct-function shims |

Shared roxygen documentation blocks live in `man-roxygen/` as `@template` fragments —
reuse these via `@template` tags instead of re-writing standard `@param`/`@returns` docs.

### The `stocnet` object model

`stocnet` is the package's native class for richer networks
(see [R/class_stocnet.R](../R/class_stocnet.R)),
constructed with `make_stocnet()` or coerced to with `as_stocnet()`.
It is **not** layered on top of `{igraph}`/`tbl_graph`;
it is a plain list of tibbles plus metadata, with six components —
`info`, `nodes`, `ties`, `changes`, `globals`, and `missings` — any of which may be
`NULL`. Every component but `info` is a table, which is what the plural name signifies.
This is what allows multimodal, multiplex, longitudinal/dynamic and modelling-oriented
networks (e.g. round-tripping `{RSiena}` `sienadata` objects) to live in one object.
Conventions to preserve when writing or editing functions:

- `info`: a list of network-level metadata. The reserved elements are listed in
  `.stocnet_info_fields` (see [R/manip_info.R](../R/manip_info.R)), and include
  `name`, `modes`, `layers`, `directed` (logical, optionally named per layer),
  `signed` (logical, optionally named per layer: the sign of a tie is the sign of its
  weight, so this says a network is signed where no tie is negative),
  `sender` and `receiver` (the modes a two-mode layer runs from and to),
  `observation` (`"cross-sectional"`, `"panel"`, `"event"`, `"egocentric"`, or
  `"cognitive"`, optionally named per layer), `update` (`"replace"` or `"increment"`:
  how a record of a tie relates to the one before it),
  `focal` (dependent variables), `centered`,
  the provenance fields `source`, `method`, `boundary`, `location`, `date`, and `doi`,
  and `transformations`, the record of what has been done to the network since it was
  collected. `siena` holds metadata specific to `{RSiena}`. The provenance fields and the transformations follow the GRAND
  project's FAIR-aligned metadata standards. See `?add_info` for the values each takes,
  and [Datasets](#datasets) for which to complete.
  A `time` column alone does not make a panel: `update` decides whether the moments
  of a network are the waves of a panel or a stream of events.
- `nodes`: one row per node. The label column is `label` (not `name`); reserved columns
  are `mode` (multimodal/two-mode), `active`/`present` (changing networks), and `na`.
- `ties`: one row per tie. `from` and `to` are required (even for undirected networks)
  and are stored as *integer indices* into `nodes`, not labels — `make_stocnet()`
  matches labels to indices on construction. Reserved columns are `layer` (multiplex),
  `weight` (negative weights mean a signed network, a missing weight a tie of unknown
  value), `length` (a distance, such as the length of a road; a larger length is a
  further tie, where a larger weight is a stronger one, so a tie can have both and
  neither stands in for the other: see `tie_lengths()`), `time` (longitudinal), `by` (the node that reported a tie, as in
  a cognitive social structure or an egocentric network), and `about` (the node a tie
  is about, as in gossip), and `fixed` (a logical marking a tie that is present by design and
  so not free to vary, which `as_siena()` writes as SIENA's structural one, `11`; a fixed tie
  is never missing or imputed away, and structural zeros are not held yet, see #195).
  `by` and `about` are integer indices into `nodes` too, and
  may hold `NA` for a layer that no node reported or that is about no node. Anything
  that renumbers or drops nodes must renumber them as well: use `.tie_node_cols` and
  `.remap_tie_nodes()` (see [R/class_stocnet.R](../R/class_stocnet.R)) rather than
  remapping `from` and `to` by hand. A broker ("tertius") is neither.
  A *hyperedge*, a tie among any number of nodes, is one row too: it lists its members
  in the list column `with`, also as indices into `nodes`, and holds `NA` in `from` and
  `to`, while a tie between two nodes holds `NULL` in `with`. A list column whose name
  begins `with_` holds a value for each member, in the same order (`with_weight`,
  `with_direction`). A layer holds dyads or hyperedges, never both, and `info$hyperedges`
  names what the hyperedges are. Code that reads `from` and `to` therefore keeps their
  type, but meets `NA`: use `.hyper_rows()` to tell the two kinds of row apart,
  `.hyper_to_twomode()` for the incidence form that other classes hold a hypergraph in,
  and `.needs_dyads()` where a function cannot take hyperedges.
  Every row is a tie: the ties a network records as *missing* are held elsewhere (below).
  `make_stocnet()` does accept an `na` column marking which rows are missing ties, since
  that is how the data often arrives, but it splits those rows out rather than storing
  them.
- `changes`: a nodal changelog, held as its own component rather than as extra rows in
  `nodes`/`ties`, with columns `time`, `node`, `var`, and `value`, plus an optional
  `layer` where a change applies to one layer and not others. `value` is stored as a
  list-column so changes of any class/length can be logged, but prints as a value plus a
  type label. A change states what the variable becomes *from that moment on*, so it is
  carried forward until another change says otherwise — a node that stops reporting and
  starts again therefore holds two changes.
- `globals`: network-level variables over time, with columns `var`, `value`, and
  optionally `time`.
- `missings`: the ties the network could have observed and did not, with the same
  columns as `ties` — usually `NULL`, see below.
- **Missingness** is recorded as the *nodes that did not report*, not as one record per
  tie, since that is nearly always what the data means: `nodes$na` where a node reports
  at no point, and a change of the `na` variable where it varies over time. Every tie
  such a node would report is then missing — those it sends where the layer is directed,
  both directions where it is undirected. The `missings` component holds only the
  residual, for ties no node's nonresponse implies.
  `as_missinglist()` derives the whole set, and everything else goes through it.
  See `?make_stocnet` for the four states a tie can be in, and keep them apart: a tie to
  an *inactive* node is not missing, and a tie of `NA` weight is present but unvalued.
  Missing ties are not ties, so nothing counts, returns, draws, or measures them unless
  it asks for them by name.
- Structure is enforced by `validate_stocnet()`
  (see [R/class_validate.R](../R/class_validate.R)),
  which `make_stocnet()` calls on construction.
  It checks required columns, coerces reserved columns to their expected classes, and
  renames common aliases (e.g. `name`/`id` → `label`, `sender`/`ego` → `from`) with an
  informative message. Extend the validators there rather than re-checking structure
  inside individual functions.
- Because `stocnet` is a list rather than an `igraph`, do not reach for `{igraph}`
  primitives on it. Either write an S3 method operating on the tibbles directly
  (`.data$nodes`, `.data$ties`, …), or coerce with `as_igraph()`/`as_tidygraph()` where
  the operation is genuinely graph-theoretic. Return a `stocnet` when you were given one:
  rebuild with `make_stocnet()` so validation and indexing re-run.

The older `mnet` class ([R/class_networks.R](../R/class_networks.R)) — a thin class over
`igraph`/`tbl_graph`, with `name`-first nodes, `type`/`wave`/`sign` tie attributes, and
changes stored as a graph attribute — is being progressively deprecated in favour of
`stocnet`. Keep its methods working, but write new functionality against `stocnet`.

### Two-mode networks

Many `create_*()`/`generate_*()` functions take an `n` argument that can be a single
integer (one-mode network) or a length-2 integer vector (two-mode network, the sizes of
each mode).
A `stocnet` holds the mode of each node in `nodes$mode` and names the modes in
`info$modes`; the logical `type` vertex attribute is how `{igraph}` and `{tidygraph}`
hold it, and the coercion functions translate between the two,
detecting two-mode structure or taking it from the `twomode` argument.
A two-mode network can be directed: `info$sender` and `info$receiver` say which mode
its ties run from and to, and `is_directed()` reports what the object holds.

### Console messaging

All user-facing messages go through the `snet_*()` wrappers in
[R/class_interface.R](../R/class_interface.R),
rather than base `message()`/`stop()`/`warning()` or `{cli}` calls directly:

| Wrapper | Use for |
|---|---|
| `snet_abort()` | errors: the function cannot proceed |
| `snet_warn()` | the function proceeds, but the user should not trust the result without knowing something; raises a real warning condition |
| `snet_info()` | notable information about what was done, e.g. a defaulted argument |
| `snet_minor_info()` | incidental detail, e.g. tidying node attributes to `stocnet` conventions |
| `snet_success()` | confirmation that a requested operation completed |
| `snet_prompt()` | interactive questions to the user |
| `snet_unavailable()` | not-yet-implemented features; aborts, as `snet_abort()` does |
| `snet_progress_step()`, `snet_progress_along()`, `snet_progress_seq()`, `snet_progress_nodes()` | progress reporting in longer-running loops |

One rule decides what the verbosity silences:
a call that changes control flow or tells the user not to trust a result always fires,
and a call that only reports is silenced.
So `snet_abort()`, `snet_unavailable()`, `snet_warn()`, and `snet_prompt()` fire
whatever the verbosity, and the rest depend on `options(snet_verbosity = )`,
which takes one of four levels:

| Level | Adds |
|---|---|
| `"quiet"` (the *default*) | nothing |
| `"normal"` | `snet_info()`, `snet_success()` |
| `"verbose"` | `snet_progress_*()` |
| `"debug"` | `snet_minor_info()` |

Users opt in with e.g. `options(snet_verbosity = "verbose")`.
Informational output must therefore never be load-bearing,
and errors and warnings must carry everything the user needs to act.
In tests, expect a `snet_warn()` with `expect_warning()`, not `expect_message()`.

These wrappers pass their input to `{cli}`, so:

- Braces interpolate, replacing `paste()`: `snet_abort("{.val {unknown}} could not be
  found on CRAN.")`.
- Use `{cli}` inline classes to mark up what you refer to — `{.fn}` for functions,
  `{.arg}`/`{.var}` for arguments and variables, `{.val}` for values,
  `{.url}` for links — so that styling stays consistent across the ecosystem.
- Use `{cli}`'s pluralisation rather than hand-written branches:
  `snet_abort("Node{?s} {.val {missing}} {?was/were} not found in the network.")`.
- Multiple strings can be passed as separate arguments for multiline messages.

Messages, warnings, and errors should be written in a way that is useful for new and advanced users alike.
This might include listing likely causes, mentioning objects or variables explicitly,
and indicating next actions clearly.
Prefer "`{.arg n}` must be a single integer or a length-2 vector for a two-mode network"
over "invalid input".

### Tests

This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

Tests in `tests/testthat/` mostly mirror the `R/` files (e.g. `test-make_create.R`,
`test-modif_project.R`), alongside `test-functional_*.R` harnesses that sweep whole
function families.
Some test files keep an older name than the file they test:
`test-manip_split.R` tests `R/modif_split.R`, `test-model_play.R` tests `R/make_play.R`,
and `test-coercion.R` tests all three `R/coerce_*.R` files.
`tests/testthat/helper-manynet.R` defines the shared fixtures and helpers used across
tests, and `tests/testthat/helper-functional.R` the family-sweeping machinery.
Test fixtures (Pajek, UCINET, GraphML, xlsx, etc.) live under `tests/testthat/sheets/`.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`);
`Config/testthat/start-first` prioritises `tutorials_manynet, mark_is`.

The harnesses find the exported functions of a family by their prefix
(`alive_functions()`, which leaves out defunct shims),
and run each across a grid of fixtures (`func_fixtures`: directed, two-mode, weighted,
signed, multiplex, longitudinal, …) and classes.
A new exported function is therefore tested without a new test:
that it takes `.data` first, has a default method, returns what its name implies,
and agrees across classes.
A combination that does not conform is skipped rather than failed,
with a message of the form `AUDIT [fn x fixture]: ...`,
so search the test output for `AUDIT` to find gaps in an implementation.
Where a function needs further arguments to run, add an entry to `to_argmakers` or
`meas_argmakers`; where a `to_*()` function implies a property of its result,
add one to `to_invariants`. Extend these maps rather than writing a one-off test.

### Datasets

The bundled datasets are built by scripts in `data-raw/`,
saved with `usethis::use_data(<name>, overwrite = TRUE, compress = "bzip2")`,
and documented in `R/data_ison.R`, `R/data_fict.R`, or `R/data_irps.R`.
[data-raw/stocnet_conversion.R](../data-raw/stocnet_conversion.R) is the worked example.

- **A new dataset is a `stocnet` object.**
  Some older datasets are still `mnet` objects, or lists of them,
  because a released downstream package relies on their class or attribute names;
  see issues #185 and #186 for which, and for what each waits on.
- **Record how the network was collected in `info`,** with `add_info()`:
  `name`, `modes`, `layers`, and `directed` (named per layer where the layers differ);
  `observation`, and `update` where the ties hold moments;
  `sender` and `receiver` for a two-mode layer;
  and the provenance fields `source`, `method`, `boundary`, `location`, `date`, and `doi`.
  `date` is when the data was collected or what it describes, not when it was published.
- **Take these details from the publication, not from memory,**
  and say in a comment in the script where each comes from.
  Where the source does not give a detail, leave the field out rather than guess.
- **Keep what the data holds.**
  A conversion must not change the number of nodes or ties,
  or how the network marks (`is_directed()`, `is_twomode()`, `is_signed()`, …).
- **Document what a user cannot read off the object:**
  who collected it, how, where and when, what a tie and its weight mean,
  and the references, with a `\doi{}` where there is one.
  The `@format` block prints the object itself, so it needs no description by hand.
- A change to the class or contents of a shipped dataset can break a downstream package,
  so check them first (see [Checking downstream packages](#checking-downstream-packages)).

### Deprecation

A function that is renamed, superseded, or moved keeps working under its old name
for a time, through a shim in [R/manynet-defunct.R](../R/manynet-defunct.R):

- Document the shim with `#' @describeIn defunct Deprecated on <date>.` and export it.
- For a rename, call `.Deprecated("<new>", package = "manynet", old = "<old>")`
  and then the new function, so that the old name still returns a result.
- For a function moved to another package, call `fn_moved()`,
  which tells the user where it went and how to install that package.
- A renamed *argument* keeps working under its former name through `...`
  and `.former_args()` (see [R/make_generate.R](../R/make_generate.R)).
- Record the change as `Renamed ... to ...` or `Moved ...` in `NEWS.md`.

Older shims are cleared at a minor release.
The functional test harnesses leave shims out, so they need no tests of their own
beyond one that the old name warns and still works.

## Documenting

### README and Website

The README offers a landing page for new users, both on the Github repository
as well as on the website.
As such, it should make a compelling case for the value added of the package,
and not drift out of date.
Note that `README.md` is generated from `README.Rmd` — edit `README.Rmd` and re-knit
(`devtools::build_readme()`), never edit `README.md` directly.

The website is created by pkgdown from [pkgdown/_pkgdown.yml](../pkgdown/_pkgdown.yml),
and is deployed automatically when changes reach `main`.
Please make sure that the pkgdown website will build correctly:
run `pkgdown::build_site()` locally before opening a PR.
The most common failure is a new exported function that is not picked up under the
function overview (the `reference:` section of `_pkgdown.yml`) —
pkgdown requires *every* exported topic to appear there exactly once, or it will not build.
Where possible, add functions to an existing subtitle's `starts_with()` pattern
(e.g. a new `to_*()` function needs no change), and only list the topic explicitly
where it does not fit a pattern.
These `reference:` titles are also the headings used in `NEWS.md` (see below),
so keep the two in step.

Two further sets of files are generated rather than edited directly:

- `vignettes/articles/*.Rmd`, the static pkgdown versions of the `{learnr}` tutorials,
  are built from `inst/tutorials/*/*.Rmd` by
  [data-raw/build_tutorial_articles.R](../data-raw/build_tutorial_articles.R).
  After editing a tutorial, re-run that script and commit the regenerated articles;
  CI checks that the two are in sync.
  New tutorials also need an entry under `articles:` and in the navbar's `tutorials`
  menu in `_pkgdown.yml`.
- The cheatsheet is built from [data-raw/cheatsheet/](../data-raw/cheatsheet)
  into `man/figures/cheatsheet.png` and `inst/figures/cheatsheet.pdf`.
  It might not cover all functionality as the package develops,
  but should at least not use deprecated or renamed function names.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then the function families in overview order:
`## Making`, `## Classes`, `## Coercion`, `## Manipulating`, `## Modifying`,
`## Describing`, `## Practicing`.
`## Classes` is not a website title, but collects changes to the stocnet object
itself (its components, `validate_*()`, and the description and print methods).
Where a release changes what downstream packages report,
`## Marking`, `## Measuring`, and `## Learning` follow the same pattern
after `## Modifying`.
Put `## Glossary` and `## Tutorials` near the end,
with `## Tutorials` immediately before any `## Data` section,
so that tutorials and data usually close the list.
Each heading appears at most once per version.
Spell the coercion heading `## Coercion`, not `## Coercing`.
`## Manipulating` covers the `manip_*` verbs (nodes/ties/attributes/changes/info/globals);
`## Modifying` covers the `modif_*` transformations (`to_*()`, projection, splits, etc.).
Keep the two distinct, as the website does.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closes #123)`.
  Note that GitHub closes the issue only where the same phrase is in a commit message
  or the pull request description
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Accelerated ...` — the same behaviour, made faster;
  give the speed-up in a sub-bullet (see below)
- `Updated ...` — documentation changes
- `Removed ...` / `Dropped ...` — functionality or dependencies taken out
- `Moved ...` / `Migrated ...` — functionality relocated to another package or file
- `Split ...` — one function or file divided into several

Any of these verbs can also lead a sub-bullet.

An `Accelerated ...` bullet says how the function was made faster,
and a sub-bullet says by how much and for what input, e.g.:

```
- Accelerated `to_correlation()` by correlating reciprocal and complex networks at once
  - ~40x faster for a directed network of 400 nodes
```

Time the old and the new version on the same input,
and give a rounded figure as `~40x`, a range as `~40-140x`,
or a lower bound as `>1000x`.
Name the input the figure was measured on,
since the speed-up usually grows with the size of the network.

Name a function by the generic, e.g. `net_modes()`, where the change reaches
every class it dispatches on.
Where it reaches only one method, spell that method out in full,
e.g. `net_modes.igraph()`,
so that a reader knows which classes the change applies to.

Keep every bullet to one line of fewer than 81 characters ideally (a few more or less is fine).
If a bullet wraps, it holds too much:
shorten it, or split it into a lead bullet and sub-bullets.
Each bullet stands on its own, and states what changed, not why or how unless there is space for context.
Explanation belongs in the function documentation or the vignettes.

Where several bullets describe parallel changes, reuse the sentence structure,
so that a reader sees the parallelism at a glance.
Use one word for one thing throughout a version's entries,
rather than varying the wording for effect.

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.
Cluster related changes (e.g. several fixes to the same function, or sub-points of one
feature) as indented sub-bullets under a lead bullet, to improve readability.
Where several changes concern one function, lead with an `Improved ...` bullet that
names the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
so the cluster groups by function rather than by change type.
Under an `Improved ...` lead bullet, do not name the function again in the
sub-bullets, since the lead bullet already carries it.
Sub-bullets indent by two spaces, and nest at most one level further (four spaces).
A sub-bullet does not need a verb: it can state the consequence, the previous
behaviour, or an example call.
The more entries a version holds, the more this structure matters,
so group first and only then write the bullets.

## Releasing

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- `breaking` runs alongside `develop` and holds the changes that must wait.
  Put a change on `breaking`, not `develop`, where it would break a *released*
  downstream package (`{netrics}`, `{autograph}`, `{migraph}`), e.g. a change
  to the class or contents of shipped data or to what a function returns.
  Record it under the heading for the version it waits for in `NEWS.md` on `breaking`,
  which is the first patch or minor release after the downstream packages can take it,
  and say there which release of which package it waits for.
  Bug fixes and other non-breaking changes still go on `develop`,
  which is merged into `breaking` regularly to keep the two in step.
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a reverse-dependency
  check, a check that the tutorial articles are in sync with the tutorials,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.

### Checking downstream packages

`{manynet}` is the data layer under `{netrics}`, `{autograph}`, and `{migraph}`,
so a change here can strand their users until their own next release.
Before committing a change to shipped data, to a class, or to what a function returns:

1. Build the package from a clean copy and install it into a separate library:
   `R CMD INSTALL --library=<lib> manynet_<version>.tar.gz`.
2. Run the test suite of each downstream package against that library, e.g.
   `R_LIBS=<lib> Rscript -e "devtools::test('<path to netrics>', stop_on_failure = FALSE)"`,
   and their examples and tutorials, since those are checked on CRAN too.
3. Compare against the same run on an unchanged `develop`,
   so that a failure that was already there is not taken for a new one.
4. Read the backtraces rather than the counts.
   A failure inside a downstream package's own `R/` code is a real break:
   the change waits on `breaking` until that package is fixed and released.
   A failure in its `tests/` is usually an expectation to update there.

The `Reverse dependencies` job in [prchecks.yml](workflows/prchecks.yml) only checks
that the CRAN releases still find every `{manynet}` object they use.
It does not run their tests or examples, so it does not replace this check.

### Releasing to CRAN

Once the pull request into `main` passes its checks:

1. Check that the `Version` in `DESCRIPTION` equals the newest heading in `NEWS.md`,
   and that every bullet under it describes something a user can see.
2. Build a tarball from a clean copy of the repository and run
   `R CMD check --as-cran --run-donttest` on it. Read every note.
3. Check the *CRAN releases* of all reverse dependencies against the new build:
   download the source tarballs of `{netrics}`, `{autograph}`, `{migraph}`, and
   `{goldfish}`, and run `R CMD check` on each with the new `{manynet}` first in the
   library path. CRAN runs the same check, and a failure there stops the submission.
4. Where a downstream package had to change to work with this release,
   its own release must be on CRAN first, and must require the `{manynet}` version
   it needs (`manynet (>= x.y.z)`), so that CRAN never pairs it with an older one.
5. Write `cran-comments.md` from the results of these checks, not from the last release:
   the environments that were really tested, the counts of errors, warnings, and notes,
   the full text of each note with a sentence on why it is acceptable,
   and the result for each reverse dependency.
6. CRAN notes a package with more than six releases in the past 180 days
   ("Number of updates in past 6 months"). Count the releases on CRAN and in its
   archive before submitting, and say why the release cannot wait if the note will occur.
7. Submit with `devtools::submit_cran()`, and tag the release only once CRAN has
   accepted it, since the version may have to move if CRAN asks for changes.

## Git for new contributors

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english),
and a more recent motivation [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
They explain the basics of git version control: committing and repos, pulling and pushing,
branching and merging.

Using git from the command line can be intimidating,
so I recommend [Fork](https://git-fork.com) software for Mac and Windows,
which allows mostly visual management of commits, diffs, and branches.

- **Clone** the remote repository to your computer, and work on the `develop` branch.
- **Pull** before you start working, so that you have the newest commits and files.
- **Commit** once you have saved a change: check the diff one last time,
  and write a commit message in the form of a `NEWS.md` bullet (see above),
  with the issue it relates to.
- **Push** the commit to the remote repository, making sure the branch is the one you worked on.
