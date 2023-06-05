
<!-- README.md is generated from README.Rmd. Please edit that file -->

# manynet

<img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
<!-- ![CRAN/METACRAN](https://img.shields.io/cran/v/manynet) -->
<!-- ![GitHub release (latest by date)](https://img.shields.io/github/v/release/snlab-ch/manynet) -->
![GitHub Release
Date](https://img.shields.io/github/release-date/snlab-ch/manynet)
<!-- [![Codecov test coverage](https://codecov.io/gh/snlab-ch/manynet/branch/main/graph/badge.svg)](https://app.codecov.io/gh/snlab-ch/manynet?branch=main) -->
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/snlab-ch/manynet/badge)](https://www.codefactor.io/repository/github/snlab-ch/manynet) -->
<!-- [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396) -->
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

`{manynet}` provides a common set of tools and a standard syntax for
analysing many different types of networks. It offers a broad range of
functions to make, manipulate, map, measure, and model:

- one-, two-, and sometimes three-mode networks
- undirected, directed, and sometimes complex networks
- unweighted, weighted, and sometimes signed networks

## How does manynet help?

`{manynet}` can help with many network analytic tasks, including Making
and Manipulating network data, Marking and Measuring nodes, ties, and
networks, calculating Motifs and identifying Memberships, as well as
Modelling and Mapping.

### Making network data

#### Importing network data

`{manynet}` offers a number of options for importing network data found
in other repositories. `{manynet}` can import and export to Excel
edgelists and nodelists,
[UCINET](http://www.analytictech.com/archive/ucinet.htm),
[Pajek](http://mrvar.fdv.uni-lj.si/pajek/), and
[DynetML](http://casos.cs.cmu.edu/projects/dynetml/) files, e.g.:

<img src="man/figures/README-import-graph-1.png" width="100%" />

- `read_dynetml()`, `read_edgelist()`, `read_matrix()`,
  `read_nodelist()`, `read_pajek()`, `read_ucinet()`
- `write_edgelist()`, `write_matrix()`, `write_nodelist()`,
  `write_pajek()`, `write_ucinet()`

#### Inventing network data

`{manynet}` includes algorithms for making networks with particular
properties. The `create_*` group of functions create networks with a
particular structure, e.g.:

- `create_components()`, `create_core()`, `create_empty()`,
  `create_filled()`, `create_lattice()`, `create_ring()`,
  `create_star()`, `create_tree()`

The `generate_*` group of functions generate networks from particular
generative mechanisms, e.g.:

- `generate_permutation()`, `generate_random()`, `generate_scalefree()`,
  `generate_smallworld()`, `generate_utilities()`

Note that all these functions work to create two-mode networks as well
as one-mode versions.

### Manipulating network data

In addition to functions that help add elements to or extract elements
from a network, `{manynet}` also includes functions for coercing and
changing network data.

#### Coercing network data

Once network data is in R, `{manynet}`’s `as_*()` functions can be used
to translate objects from one of the above classes into any other, and
include:

    #> Registered S3 method overwritten by 'migraph':
    #>   method        from   
    #>   mutate.igraph manynet

<img src="man/figures/README-coercion-graph-1.png" width="100%" />

These functions are designed to be as intuitive and lossless as
possible, outperforming many other class-coercion packages.

We use these functions internally in every `{manynet}` function to (1)
allow them to be run on any compatible network format and (2) use the
most efficient algorithm available. This makes `{manynet}` compatible
with your existing workflow, whether you use base R matrices or
edgelists as data frames, [`{igraph}`](https://igraph.org/r/),
[`{network}`](https://statnet.org), or
[`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html), and
extensible by developments in those other packages too.

#### Changing network data

`{manynet}`’s `to_*()` functions can be used on any class object to
reformat, transform, or split networks into networks with other
properties, e.g.:

- `to_acyclic()`, `to_anti()`, `to_blocks()`, `to_components()`,
  `to_directed()`, `to_egos()`, `to_giant()`, `to_matching()`,
  `to_mode1()`, `to_mode2()`, `to_multilevel()`, `to_named()`,
  `to_no_isolates()`, `to_onemode()`, `to_reciprocated()`,
  `to_redirected()`, `to_simplex()`, `to_slices()`, `to_subgraph()`,
  `to_subgraphs()`, `to_ties()`, `to_twomode()`, `to_undirected()`,
  `to_uniplex()`, `to_unnamed()`, `to_unsigned()`, `to_unweighted()`,
  `to_waves()`

Reformatting means changing the format of the network, e.g. from
directed to undirected via `to_undirected()`. Transforming means
changing the dimensions of the network, e.g. from a two-mode network to
a one-mode projection via `to_mode1()`. Splitting means separating a
network, e.g. from a whole network to the various ego networks via
`to_egos()`. Those functions that split a network into a list of
networks are distinguishable as those `to_*()` functions that are named
in the plural.

### Marks

`{manynet}` offers a range of measures and models with sensible
defaults. Many wrap existing functions in common packages for use with
one-mode networks, but extend these to treat and/or normalise for
two-mode (and sometimes three-mode) networks correctly. Functions are
given intuitive and succinct names that avoid conflicts with existing
function names wherever possible.

`{manynet}`’s `*is_*()` functions offer fast logical tests of various
properties. Whereas `is_*()` returns a single logical value for the
network, `node_is_*()` returns a logical vector the length of the number
of nodes in the network, and `tie_is_*()` returns a logical vector the
length of the number of ties in the network.

- `is_bipartite()`, `is_complex()`, `is_directed()`, `is_dynamic()`,
  `is_edgelist()`, `is_graph()`, `is_labelled()`, `is_longitudinal()`,
  `is_migraph()`, `is_multiplex()`, `is_signed()`, `is_twomode()`,
  `is_uniplex()`, `is_weighted()`
- `()`
- `()`

The `*is_max()` and `*is_min()` functions are used to identify the
maximum or minimum, respectively, node or tie according to some measure
(see below).

## Installation

### Stable

The easiest way to install the latest stable version of `{manynet}` is
via CRAN. Simply open the R console and enter:

`install.packages('manynet')`

You can then begin to use `{manynet}` by loading the package:

`library(manynet)`

This will load any required packages and make the data contained within
the package available. The version from CRAN also has all the vignettes
built and included. You can check them out with:

`vignettes(package = "manynet")`

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally.

The latest binary releases for all major OSes – Windows, Mac, and Linux
– can be found
[here](https://github.com/snlab-ch/manynet/releases/latest). Download
the appropriate binary for your operating system, and install using an
adapted version of the following commands:

- For Windows:
  `install.packages("~/Downloads/manynet_winOS.zip", repos = NULL)`
- For Mac:
  `install.packages("~/Downloads/manynet_macOS.tgz", repos = NULL)`
- For Unix:
  `install.packages("~/Downloads/manynet_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of `{manynet}` from
Github, please install the `{remotes}` or `{devtools}` package from CRAN
and then:

- For latest stable version:
  `remotes::install_github("snlab-ch/manynet", build_vignettes = TRUE)`
- For latest development version:
  `remotes::install_github("snlab-ch/manynet@develop", build_vignettes = TRUE)`

## Relationship to other packages

This package aims to provide an updated, and more comprehensive
replacement for `{intergraph}`. It also builds upon but makes some
different decisions to the excellent `{tidygraph}` package. It builds
upon `{igraph}` especially, as well as `{network}`, but works equally
well with basic edgelists and matrices.

`{manynet}` inherits core functionality from the `{migraph}` package.
For more analytic and modelling functions, please see `{migraph}`.

## Funding details

Subsequent work on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
