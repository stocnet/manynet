# Contributing

Contributions to `manynet`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Aims

Here is some things that Guy Kawasaki, Silicon Valley venture capitalist,
learned from Steve Jobs:

- "Experts" are clueless. Especially self-declared ones.
- Customers cannot tell you what they need. They can help with evolution, but not revolution.
- Biggest challenges beget the best work.
- Design counts. Users will see the skin/UI of your product, not the great algorithms.
- Big graphics, big fonts.
- Jump curves---do things 10 times better, not 10 percent.
- All that truly matters is whether something works or doesn't work. Open or close, iPhone or Android, car or train, doesn't matter---make
it work.
- "Value" is different from "price". There is a class of people who do care about value. Ease of use -> less support costs. You have to create a unique and valuable product as an engineer.
- Real CEOs can demo. If you can't demo your own product, then quit.
- Real entrepreneurs ship, not slip.
- Some things need to be believed to be seen.

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

## Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`as_*()`, `is_*()`, `create_*()`, etc.) should share
argument order and naming, so that behaviour is guessable across the family.

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

Note that `README.md` is generated from `README.Rmd` — edit `README.Rmd` and re-knit,
never edit `README.md` directly.
Similarly, `vignettes/articles/*.Rmd` (the static pkgdown versions of the `{learnr}`
tutorials) are generated from `inst/tutorials/*/*.Rmd` by
[data-raw/build_tutorial_articles.R](../data-raw/build_tutorial_articles.R) —
after editing a tutorial, re-run that script and commit the regenerated files
rather than editing them directly (CI checks that the two are in sync).

### File organization (file naming = function family)

`R/` files are prefixed by the verb-family of the functions they contain,
not by data structure. When looking for a function, search by what it *does*:

| Prefix | Contains |
|---|---|
| `make_*.R` | creating/reading/generating networks: `create_*()` (deterministic structures), `generate_*()` (stochastic mechanisms), `read_*()`/`write_*()` (import/export), `play_*()` (diffusion/learning simulations), `data_*`/`manynet-data.R` (bundled datasets: `ison_*` classic/instructional, `fict_*` fictional, `irps_*` international-relations) |
| `coerce_graph.R`, `coerce_list.R` | the `as_*()` translation layer between representations (`as_igraph()`, `as_tidygraph()`, `as_network()`, `as_matrix()`, `as_edgelist()`, `as_siena()`, `as_diffnet()`, …), implemented as S3 methods dispatching on input class |
| `class_*.R` | the `mnet` S3 class itself (`class_networks.R`: `print.mnet`, `$`/`$<-` accessors, node/tie/change data model), related result classes (`class_measures.R`, `class_members.R`, `class_motifs.R`, `class_models.R`), the `snet_*()` CLI layer (`class_interface.R`), input validation (`class_validate.R`), and the `describe_*()` helpers behind `print.mnet` (`class_describe.R`) |
| `manip_*.R` | dplyr-style verbs for manipulating nodes/ties/attributes (`manip_nodes.R`, `manip_ties.R`, `manip_global.R`, `manip_info.R`, `manip_changes.R`) |
| `mark_*.R` | logical/predicate functions returning `TRUE`/`FALSE` or marks about a network, e.g. the `is_*()` family (`mark_classes.R`, `mark_features.R`, `mark_format.R`, `mark_changes.R`) |
| `modif_*.R` | structural transformations/reformatting: direction, weighting, projection, splitting/joining, path/level operations, missing data (`modif_direction.R`, `modif_project.R`, `modif_split.R`, `modif_miss.R`, …) |
| `measure_*.R` | descriptive/attribute measures (`measure_attributes.R`, `measure_properties.R`); heavier network-analytic measures live in `{netrics}` |
| `reexports_classes.R` | re-exports of classes/generics from `{igraph}`/`{tidygraph}`/`{network}` so users need not load those packages directly |
| `manynet-utils.R`, `manynet-glossary.R`, `manynet-defunct.R` | shared internal helpers, the `gloss()`/glossary system used in tutorials and docs, and defunct-function shims |

Shared roxygen documentation blocks live in `man-roxygen/` as `@template` fragments —
reuse these via `@template` tags instead of re-writing standard `@param`/`@returns` docs.

### The `mnet` object model

`mnet` is layered on top of `{igraph}`/`tbl_graph` (see [R/class_networks.R](../R/class_networks.R)).
Conventions to preserve when writing or editing functions:

- Node table: first column is always `name`; reserved columns are `active`
  (changing networks) and `type` (multimodal/two-mode).
- Tie table: first two columns are always `from` and `to` (even for undirected
  networks); reserved columns are `weight`, `wave` (longitudinal), `type` (multiplex),
  and `sign` (signed).
- Changes (a longitudinal changelog) are stored as a graph attribute, not as extra
  rows in nodes/ties, with columns `wave`/`time`, `node`, `var`, `value`.
- Because an `mnet` object is simultaneously a valid `igraph` and `tbl_graph` object,
  prefer writing new functions against `{igraph}`/`{tidygraph}` primitives
  (via the `as_igraph()`/`as_tidygraph()` coercions)
  rather than hand-rolling data structure access.

### Two-mode networks

Many `create_*()`/`generate_*()` functions take an `n` argument that can be a single
integer (one-mode network) or a length-2 integer vector (two-mode network, the sizes of
each mode). Coercion functions detect and handle two-mode structure via the `twomode`
argument and the `type` vertex attribute.

### Console messaging

All user-facing messages go through the `snet_*()` wrappers in
[R/class_interface.R](../R/class_interface.R) — `snet_info()`, `snet_warn()`,
`snet_abort()`, `snet_success()`, `snet_prompt()`, `snet_unavailable()`,
plus `snet_progress_*()` — rather than base `message()`/`stop()`/`warning()`.
These respect `options(snet_verbosity = "quiet")` and give consistent `{cli}`-styled
output. Use `snet_unavailable()` for not-yet-implemented features.

### Tests

Tests in `tests/testthat/` mirror the `R/` files (e.g. `test-make_create.R`,
`test-manip_split.R`), alongside `test-functional_*.R` harnesses that sweep whole
function families.
`tests/testthat/helper-manynet.R` defines the shared fixtures and helpers used across
tests, and `tests/testthat/helper-functional.R` the family-sweeping machinery.
Test fixtures (Pajek, UCINET, GraphML, xlsx, etc.) live under `tests/testthat/sheets/`.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`);
`Config/testthat/start-first` prioritises `tutorials_manynet, mark_is`.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then the function families in overview order:
`## Making`, `## Coercion`, `## Manipulating`, `## Modifying`, `## Describing`,
`## Practicing`.
Put `## Glossary` and `## Tutorials` near the end,
with `## Tutorials` immediately before any `## Data` section,
so that tutorials and data usually close the list.
Each heading appears at most once per version.
`## Manipulating` covers the `manip_*` verbs (nodes/ties/attributes/changes/info/globals);
`## Modifying` covers the `modif_*` transformations (`to_*()`, projection, splits, etc.).
Keep the two distinct, as the website does.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.
Cluster related changes (e.g. several fixes to the same function, or sub-points of one
feature) as indented sub-bullets under a lead bullet, to improve readability.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a reverse-dependency
  check, a check that the tutorial articles are in sync with the tutorials,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- Commits should reference an existing GitHub issue number (`#123`), see below.

## Fork

### Cloning
Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 
This command allows you to `pull` changes from the remote repository to your local repository on Sourcetree.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose master or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 
Some documents might be stored using Large File Storage (LFS) to save space on the repository. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

## Issues and tests

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
Currently issue tracking is only open to those involved in the project.

The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

## Bug fixing or adding new code

Independent or assigned code contributions are most welcome.
When writing new code, please follow 
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/). 
It can help to use packages such as `lintr`, `goodpractice` and `formatR` 
to ensure these are followed.

Currently, commits can only be pushed to GitHub where they reference an existing issue.
If no issue exists for the code you have developed, please add an issue first before pushing.
Once the issue exists, you will need to mention the issue number (preceded by a hash symbol: #)
in the commit description:

``` Resolved #31 by adding a new function that does things, also updated documentation ```

Where the issue hash (i.e. #31) is preceded by
`resolve`, `resolves`, `resolved`, `close`, `closes`, `closed`, `fix`, `fixes`, or `fixed`
(capitalised or not),
Github will automatically updated the status of the issue(s) mentioned.

Our current syntactical standard is to mention the issue first and then 
provide a short description of what the committed changes do 
in relation to that issue.
Any ancillary changes can be mentioned after a comma.

## Documentation

A final way of contributing to the package is in developing the 
vignettes/articles that illustrate the value added in the package. 
Please contact me with any proposals here.

Please note that the `manynet` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

