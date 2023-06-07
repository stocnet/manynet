
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

Learning and using network analysis tools in R can be challenging. There
are many great packages, all with their own offerings and advantages,
but also all with their own vocabulary, syntax, and expected formats for
data inputs and analytic outputs. Moreover, many of these packages only
work on *some* types of networks (usually one-mode, simple, directed or
undirected networks); if you have a different type of network, a
different package is needed. Lastly, even packages with excellent
graphical capabilities can make visualising networks slow and cumbersome
by using poor defaults and many, incomprehensible options to change
them. `{manynet}` aims to help researchers with Making, Molding, and
Mapping networks.

## Making

First, `{manynet}` provides a set of tools that can be used to make
networks in any number of common formats. Networks can come from many
sources, some can be imported from outside R, some can be found in this
or other packages and coerced into some common format, and many types of
networks can be created or generated using functions in this package.

#### Inventing network data

`{manynet}` includes algorithms for making networks with particular
properties. The `create_*` group of functions create networks with a
particular structure, and will always create the same format from the
same inputs, e.g.:

- `create_components()`, `create_core()`, `create_empty()`,
  `create_filled()`, `create_lattice()`, `create_ring()`,
  `create_star()`, `create_tree()`

The `generate_*` group of functions generate networks from generative
mechanisms that may include some random aspect, and so will return a
different output each time they are run, e.g.:

- `generate_permutation()`, `generate_random()`, `generate_scalefree()`,
  `generate_smallworld()`, `generate_utilities()`

Note that all these functions can create directed or undirected,
one-mode or two-mode networks. Creating two-mode networks is as easy as
passing the first argument (`n`) a vector of two integers instead of
one. For example, `n = 10` will create a one-mode network of 10 nodes,
whereas `n = c(10,5)` will create a two-mode network of 10 nodes in the
first mode, and 5 nodes in the second mode. Some of these functions wrap
existing algorithms in other packages, while others are unique offerings
or add additional formats, e.g. two-mode networks.

#### Importing network data

`{manynet}` offers a number of options for importing network data found
in other repositories. Besides importing and exporting to Excel
edgelists and nodelists, there are specific routines included for
[UCINET](http://www.analytictech.com/archive/ucinet.htm),
[Pajek](http://mrvar.fdv.uni-lj.si/pajek/), and
[DynetML](http://casos.cs.cmu.edu/projects/dynetml/) files, e.g.:

<img src="man/figures/README-import-graph-1.png" width="100%" />

- `read_dynetml()`, `read_edgelist()`, `read_matrix()`,
  `read_nodelist()`, `read_pajek()`, `read_ucinet()`
- `write_edgelist()`, `write_matrix()`, `write_nodelist()`,
  `write_pajek()`, `write_ucinet()`

#### Coercing network data

There is also a lot of network data in addition to analytic functions
that already reside in various packages in R. Once network data is in R,
`{manynet}`’s `as_*()` functions can be used to translate objects from
one of the above classes into any other, and include:

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

## Molding

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

### Reformatting

Reformatting means changing the format of the network, e.g. from
directed to undirected via `to_undirected()`.

### Transforming

Transforming means changing the dimensions of the network, e.g. from a
two-mode network to a one-mode projection via `to_mode1()`.

### Splitting and Joining

Splitting means separating a network, e.g. from a whole network to the
various ego networks via `to_egos()`.

#### Changing network data

Those functions that split a network into a list of networks are
distinguishable as those `to_*()` functions that are named in the
plural.

### Extracting

`{manynet}` offers a range of measures and models with sensible
defaults. Many wrap existing functions in common packages for use with
one-mode networks, but extend these to treat and/or normalise for
two-mode (and sometimes three-mode) networks correctly. Functions are
given intuitive and succinct names that avoid conflicts with existing
function names wherever possible.

`{manynet}`’s `*is_*()` functions offer fast logical tests of various
network properties. These can be used to create new

- `is_bipartite()`, `is_complex()`, `is_directed()`, `is_dynamic()`,
  `is_edgelist()`, `is_graph()`, `is_labelled()`, `is_longitudinal()`,
  `is_manynet()`, `is_multiplex()`, `is_signed()`, `is_twomode()`,
  `is_uniplex()`, `is_weighted()`

## Mapping

`{manynet}` includes three one-line graphing functions with sensible
defaults based on the network’s properties. `autographr()` is used to
graph networks in any of the `{manynet}` formats. It includes sensible
defaults so that researchers can view their network’s structure or
distribution with a minimum of fuss. `{manynet}` uses the excellent
`{ggraph}` package (and thus `{ggplot2}`) as a plotting engine. This
also makes extending and theming default output easy, and `{patchwork}`
is used to help arrange individual plots together.

There are two other graphing functions too. `autographs()` is used to
graph multiple networks together, which can be useful for ego networks
or network panels. `autographd()` is used to graph dynamic networks.

In addition, `{manynet}` offers some additional layout algorithms for
snapping layouts to a grid or visualising partitions horizontally,
vertically, or concentrically. The following figures illustrate the
difference in results over `{igraph}`:

<img src="man/figures/README-layout-comparison-1.png" width="100%" />

provide a common set of tools that can be used to import, export,
create, and manipulate network data in a wide variety of formats, and
obtain a good first visualisation quickly. This can be useful for
pedagogical purposes, initial description, or checking something part
way through the modelling process. Through the most comprehensive
network class-coercion available, users can access routines not
available in their chosen package or even in `{manynet}`.

`{manynet}` provides a common set of tools and a standard syntax for
analysing many different types of networks. It offers a broad range of
functions to make, manipulate, map, measure, and model:

- one-, two-, and sometimes three-mode networks
- undirected, directed, and sometimes complex networks
- unweighted, weighted, and sometimes signed networks

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

It works with and builds upon many popular network analytic packages,
such as `{igraph}` and `{network}`.

`{manynet}` inherits core functionality from the `{migraph}` package.
For more analytic and modelling functions, please see `{migraph}`.

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
