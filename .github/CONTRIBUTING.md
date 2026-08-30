# Contributing

Contributions to `manynet`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `manynet` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

### Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

### Cloning

Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 

This command allows you to `pull` changes from the remote repository to your local repository in Fork.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose main or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 
Some documents might be stored using Large File Storage (LFS) to save space on the repository. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` it to the remote repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a reverse-dependency
  check, a check that the tutorial articles are in sync with the tutorials,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.

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

## Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`as_*()`, `is_*()`, `create_*()`, etc.) should share
argument order and naming, so that behaviour is guessable across the family.

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

Some files are generated rather than edited directly — `README.md`, the tutorial
articles in `vignettes/articles/`, and the cheatsheet.
See [README and Website](#readme-and-website) below for which source each is built from.

### File organization (file naming = function family)

`R/` files are prefixed by the verb-family of the functions they contain,
not by data structure. When looking for a function, search by what it *does*:

| Prefix | Contains |
|---|---|
| `make_*.R` | creating/reading/generating networks: `create_*()` (deterministic structures), `generate_*()` (stochastic mechanisms), `read_*()`/`write_*()` (import/export), `play_*()` (diffusion/learning simulations), `data_*`/`manynet-data.R` (bundled datasets: `ison_*` classic/instructional, `fict_*` fictional, `irps_*` international-relations) |
| `coerce_graph.R`, `coerce_list.R` | the `as_*()` translation layer between representations (`as_igraph()`, `as_tidygraph()`, `as_network()`, `as_matrix()`, `as_edgelist()`, `as_siena()`, `as_diffnet()`, …), implemented as S3 methods dispatching on input class |
| `class_*.R` | the network classes themselves (`class_stocnet.R`: `make_stocnet()`, `print.stocnet()`, the info/nodes/ties/changes/global data model; `class_networks.R`: the legacy `mnet` class, `print.mnet`, `$`/`$<-` accessors), related result classes (`class_measures.R`, `class_members.R`, `class_motifs.R`, `class_models.R`), the `snet_*()` CLI layer (`class_interface.R`), input validation (`class_validate.R`: `validate_stocnet()` and its component validators), and the `describe_*()` helpers behind the print methods (`class_describe.R`) |
| `manip_*.R` | dplyr-style verbs for manipulating nodes/ties/attributes (`manip_nodes.R`, `manip_ties.R`, `manip_globals.R`, `manip_info.R`, `manip_changes.R`) |
| `mark_*.R` | logical/predicate functions returning `TRUE`/`FALSE` or marks about a network, e.g. the `is_*()` family (`mark_classes.R`, `mark_features.R`, `mark_format.R`, `mark_changes.R`) |
| `modif_*.R` | structural transformations/reformatting: direction, weighting, projection, splitting/joining, path/level operations, missing data (`modif_direction.R`, `modif_project.R`, `modif_split.R`, `modif_miss.R`, …) |
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

- `info`: a list of network-level metadata. Reserved elements include `name`, `modes`,
  `layers`, `directed` (logical, optionally named per layer), `focal` (dependent
  variables), `centered`, `siena` (RSiena-specific metadata), plus provenance fields
  `doi`, `date`, `location`, and `source`. Per-layer entries are named after a layer and
  may carry `sender`, `recipient`, and `update`. Several of these follow the GRAND
  project's FAIR-aligned metadata standards.
- `nodes`: one row per node. The label column is `label` (not `name`); reserved columns
  are `mode` (multimodal/two-mode), `active`/`present` (changing networks), and `na`.
- `ties`: one row per tie. `from` and `to` are required (even for undirected networks)
  and are stored as *integer indices* into `nodes`, not labels — `make_stocnet()`
  matches labels to indices on construction. Reserved columns are `layer` (multiplex),
  `weight` (negative weights mean a signed network, a missing weight a tie of unknown
  value), `time` (longitudinal), and `by` (triadic/tertius ties).
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
each mode). Coercion functions detect and handle two-mode structure via the `twomode`
argument and the `type` vertex attribute.

### Console messaging

All user-facing messages go through the `snet_*()` wrappers in
[R/class_interface.R](../R/class_interface.R),
rather than base `message()`/`stop()`/`warning()` or `{cli}` calls directly:

| Wrapper | Use for |
|---|---|
| `snet_abort()` | errors: the function cannot proceed |
| `snet_warn()` | the function proceeds, but the user should know something |
| `snet_info()` | notable information about what was done, e.g. a defaulted argument |
| `snet_minor_info()` | incidental detail, e.g. tidying node attributes to `stocnet` conventions |
| `snet_success()` | confirmation that a requested operation completed |
| `snet_prompt()` | interactive questions to the user |
| `snet_unavailable()` | not-yet-implemented features |
| `snet_progress_step()`, `snet_progress_along()`, `snet_progress_seq()`, `snet_progress_nodes()` | progress reporting in longer-running loops |

Every wrapper except `snet_abort()` (and `snet_prompt()`) is silenced by
`options(snet_verbosity = "quiet")`, which is the *default* —
so informational output must never be load-bearing,
and errors must carry everything the user needs to act.
Users opt in with e.g. `options(snet_verbosity = "verbose")`.

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

Tests in `tests/testthat/` mirror the `R/` files (e.g. `test-make_create.R`,
`test-manip_split.R`), alongside `test-functional_*.R` harnesses that sweep whole
function families.
`tests/testthat/helper-manynet.R` defines the shared fixtures and helpers used across
tests, and `tests/testthat/helper-functional.R` the family-sweeping machinery.
Test fixtures (Pajek, UCINET, GraphML, xlsx, etc.) live under `tests/testthat/sheets/`.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`);
`Config/testthat/start-first` prioritises `tutorials_manynet, mark_is`.

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
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closed #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes
- `Removed ...` / `Dropped ...` — functionality or dependencies taken out
- `Moved ...` / `Migrated ...` — functionality relocated to another package or file
- `Split ...` — one function or file divided into several

Any of these verbs can also lead a sub-bullet.

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

