
<!-- README.md is generated from README.Rmd. Please edit that file -->

# manynet

<img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/manynet) ![GitHub release
(latest by
date)](https://img.shields.io/github/v/release/snlab-ch/manynet)
![GitHub Release
Date](https://img.shields.io/github/release-date/snlab-ch/manynet)
[![Codecov test
coverage](https://codecov.io/gh/snlab-ch/manynet/branch/main/graph/badge.svg)](https://app.codecov.io/gh/snlab-ch/manynet?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/snlab-ch/manynet/badge)](https://www.codefactor.io/repository/github/snlab-ch/manynet)
<!-- [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396) -->
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

While many awesome packages for network analysis exist for R, all with
their own offerings and advantages, they also all have their own
vocabulary, syntax, and expected formats for data inputs and analytic
outputs. Many of these packages only work on *some* types of networks
(usually one-mode, simple, directed or undirected networks) for *some*
types of analysis; if you want to analyse a different type of network or
try a different analysis, a different package is needed. And they can
rely on a very different visual language (and sometimes plotting
engine), which can mess up your pretty presentation or paper. This can
make learning and using network analysis tools in R challenging.

By contrast, we build packages that offer *many* analytic tools that
work on *many* (if not most) types of networks of all kinds. `{manynet}`
is the first package that helps researchers with Making, Modifying, and
Mapping networks. For Measures, Memberships, or Models, see
[`{migraph}`](https://snlab-ch.github.io/migraph/).

- [Making](#making)
  - [Importing network data](#importing-network-data)
  - [Identifying network data](#identifying-network-data)
  - [Inventing network data](#inventing-network-data)
  - [Translating network data](#translating-network-data)
- [Modifying](#modifying)
  - [Reformatting](#reformatting)
  - [Transforming](#transforming)
  - [Splitting and Joining](#splitting-and-joining)
  - [Extracting](#extracting)
- [Mapping](#mapping)
  - [autographr](#autographr)
  - [More options](#more-options)
  - [More layouts](#more-options)
  - [More themes and scales](#more-themes-and-scales)
  - [autographs](#autographs)
  - [autographd](#autographd)
- [Installation](#installation)
  - [Stable](#stable)
  - [Development](#development)
  - [Tutorials](#tutorials)
- [Relationship to other packages](#relationship-to-other-packages)
- [Funding details](#funding-details)

## Making

Networks can come from many sources and be found in many different
formats: some can be found in this or other packages, some can be
created or generated using functions in this package, and others can be
downloaded from the internet and imported from your file system.
`{manynet}` provides tools to make networks from all these sources in
any number of common formats.

#### Importing network data

`{manynet}` offers a number of options for importing network data found
in other repositories. Besides importing and exporting to Excel
edgelists, nodelists, and (bi)adjacency matrices, there are specific
routines included for
[UCINET](http://www.analytictech.com/archive/ucinet.htm),
[Pajek](http://mrvar.fdv.uni-lj.si/pajek/),
[GraphML](http://graphml.graphdrawing.org), and
[DynetML](http://casos.cs.cmu.edu/projects/dynetml/) files, e.g.:

![](https://www.jameshollway.com/post/manynet/README-import-graph-1.png)

If you cannot remember the file name/path, then just run `read_*()` with
the parentheses empty, and a file selection popup will open so that you
can browse through your file system to find the file. Usually both
`read_*()` and `write_*()` are offered to make sure that `{manynet}` is
compatible with your larger project and analytic workflow.

- `read_dynetml()`, `read_edgelist()`, `read_graphml()`,
  `read_matrix()`, `read_nodelist()`, `read_pajek()`, `read_ucinet()`
- `write_edgelist()`, `write_graphml()`, `write_matrix()`,
  `write_nodelist()`, `write_pajek()`, `write_ucinet()`

#### Identifying network data

There may be no need to import network data though, if that network data
already exists in a package in R. To facilitate testing and to
contribute to an ecosystem of easily accessible network data,
particularly for pedagogical purposes, we include a number of classical
and instructional network datasets, all thoroughly documented and ready
for analysis. Here are just a few examples, all available in
`{manynet}`:

![](https://www.jameshollway.com/post/manynet/README-ison_egs-1.png)

Here are some others: `ison_adolescents`, `ison_algebra`,
`ison_brandes`, `ison_friends`, `ison_karateka`, `ison_konigsberg`,
`ison_laterals`, `ison_lawfirm`, `ison_lotr`,
`ison_marvel_relationships`, `ison_marvel_teams`, `ison_networkers`,
`ison_physicians`, `ison_southern_women`, `ison_starwars`

#### Inventing network data

`{manynet}` includes functions for making networks algorithmically. The
`create_*` group of functions create networks with a particular
structure, and will always create the same format from the same inputs,
e.g.:

![](https://www.jameshollway.com/post/manynet/README-create_egs-1.png)

See also `create_components()`, `create_core()`, `create_empty()`,
`create_explicit()`, `create_filled()`, `create_lattice()`,
`create_ring()`, `create_star()`, `create_tree()`.

The `generate_*` group of functions generate networks from generative
mechanisms that may include some random aspect, and so will return a
different output each time they are run, e.g.:

![](https://www.jameshollway.com/post/manynet/README-generate_egs-1.png)

See also `generate_permutation()`, `generate_random()`,
`generate_scalefree()`, `generate_smallworld()`, `generate_utilities()`.

Note that all these functions can create directed or undirected,
one-mode or two-mode networks. Creating two-mode networks is as easy as
passing the first argument (`n`) a vector of two integers instead of
one. For example, while `n = 15` will create a one-mode network of 10
nodes, whereas `n = c(10,5)` will create a two-mode network of 10 nodes
in the first mode, and 5 nodes in the second mode. Some of these
functions wrap existing algorithms in other packages, while others are
unique offerings or add additional formats, e.g. two-mode networks.

![](https://www.jameshollway.com/post/manynet/README-generate_tm-1.png)

#### Translating network data

Once you have imported network data, identified network data in this or
other packages in R, or invented your own, you may need to translate
this data into another class for analysis. `{manynet}`’s `as_*()`
functions can be used to coerce objects from one of many common classes
into any other. Below is a directed graph showing the currently
available options:

![](https://www.jameshollway.com/post/manynet/README-coercion-graph-1.png)

These functions are designed to be as intuitive and lossless as
possible, outperforming many other class-coercion packages.

We use these functions internally in every `{manynet}` and `{migraph}`
function to (1) allow them to be run on any compatible network format
and (2) use the most efficient algorithm available. This makes
`{manynet}` and `{migraph}` compatible with your existing workflow,
whether you use base R matrices or edgelists as data frames,
[`{igraph}`](https://igraph.org/r/), [`{network}`](https://statnet.org),
or [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html), and
extensible by developments in those other packages too.

## Modifying

Before or during analysis, you may need to modify the network you are
analysing in various ways. Different packages have different syntaxes
and vocabulary for such actions; `{manynet}`’s `to_*()` functions can be
used on any class object to reformat, transform, or split networks into
networks with other properties.

### Reformatting

Reformatting means changing the format of the network, e.g. from
directed to undirected via `to_undirected()`.

![](https://www.jameshollway.com/post/manynet/README-directed_egs-1.png)

### Transforming

Transforming means changing the dimensions of the network, e.g. from a
two-mode network to a one-mode projection via `to_mode1()`.

![](https://www.jameshollway.com/post/manynet/README-projection_egs-1.png)

### Splitting and Joining

Splitting means separating a network, e.g. from a whole network to the
various ego networks via `to_egos()`.

![](https://www.jameshollway.com/post/manynet/README-splitting_egs-1.png)

Those functions that split a network into a list of networks are
distinguishable as those `to_*()` functions that are named in the
plural. Split data can be rejoined using the `from_*()` family of
functions.

See also `to_acyclic()`, `to_anti()`, `to_blocks()`, `to_components()`,
`to_directed()`, `to_egos()`, `to_eulerian()`, `to_galois()`,
`to_giant()`, `to_matching()`, `to_mentoring()`, `to_mode1()`,
`to_mode2()`, `to_multilevel()`, `to_named()`, `to_no_isolates()`,
`to_onemode()`, `to_reciprocated()`, `to_redirected()`, `to_simplex()`,
`to_slices()`, `to_subgraph()`, `to_subgraphs()`, `to_ties()`,
`to_twomode()`, `to_undirected()`, `to_uniplex()`, `to_unnamed()`,
`to_unsigned()`, `to_unweighted()`, `to_waves()` and `from_egos()`,
`from_slices()`, `from_subgraphs()`, `from_waves()`.

### Extracting

`{manynet}`’s `*is_*()` functions offer fast logical tests of various
network properties.

- `is_acyclic()`, `is_aperiodic()`, `is_complex()`, `is_connected()`,
  `is_directed()`, `is_dynamic()`, `is_edgelist()`, `is_eulerian()`,
  `is_graph()`, `is_labelled()`, `is_longitudinal()`, `is_manynet()`,
  `is_multiplex()`, `is_perfect_matching()`, `is_signed()`,
  `is_twomode()`, `is_uniplex()`, `is_weighted()`

## Mapping

`{manynet}` includes three one-line graphing functions with sensible
defaults based on the network’s properties.

### autographr

First, `autographr()` is used to graph networks in any of the
`{manynet}` formats. It includes sensible defaults so that researchers
can view their network’s structure or distribution quickly with a
minimum of fuss. Compare the output from `{manynet}` with a similar
default from `{igraph}`:

![](https://www.jameshollway.com/post/manynet/README-layout-comparison-1.png)

Here the `{manynet}` function recognises that the network is a two-mode
network and uses a bipartite layout by default, and recognises that the
network contains names for the nodes and prints them vertically so that
they are legible in this layout. Other ‘clever’ features include
automatic node sizing and more. By contrast, `{igraph}` requires the
bipartite layout to be specified, has cumbersome node size defaults for
all but the smallest graphs, and labels also very often need resizing
and adjustment to avoid overlap. All of `{manynet}`’s adjustments can be
overridden, however…

#### More options

Changing the size and colors of nodes and ties is as easy as specifying
the function’s relevant argument with a replacement, or indicating from
which attribute it should inherit this information.

![](https://www.jameshollway.com/post/manynet/README-more-options-1.png)

#### More layouts

`{manynet}` can use all the layout algorithms offered by packages such
as `{igraph}`, `{ggraph}`, and `{graphlayouts}`, and offers some
additional layout algorithms for snapping layouts to a grid, visualising
partitions horizontally, vertically, or concentrically, or conforming to
configurational coordinates.

![](https://www.jameshollway.com/post/manynet/README-more-layouts-1.png)

#### More themes and scales

Lastly, `autographr()` is highly extensible in terms of the overall look
of your plots. `{manynet}` uses the excellent `{ggraph}` package (and
thus `{ggplot2}`) as a plotting engine. This enables alterations such as
the application of themes to be applied upon the defaults. If you want
to quickly make sure your plots conform to your institution or taste,
then it is easy to do with themes and scales that update the basic look
and color palette used in your plots.

![](https://www.jameshollway.com/post/manynet/README-more-themes-1.png)

More themes are on their way, and we’re happy to take suggestions.

### autographs

Second, `autographs()` is used to graph multiple networks together,
which can be useful for ego networks or network panels. `{patchwork}` is
used to help arrange individual plots together.

![](https://www.jameshollway.com/post/manynet/README-autographs-1.png)

### autographd

Third, `autographd()` is used to visualise dynamic networks. It uses
`{gganimate}` and `{gifski}` to create a gif that visualises network
changes over time. It really couldn’t be easier.

![](https://www.jameshollway.com/post/manynet/README-autographd-1.gif)

<!-- provide a common set of tools that can be used to import, export, create, and manipulate network data in a wide variety of formats, -->
<!-- and obtain a good first visualisation quickly. -->
<!-- This can be useful for pedagogical purposes, initial description, or checking something part way through the modelling process. -->
<!-- Through the most comprehensive network class-coercion available, -->
<!-- users can access routines not available in their chosen package or even in `{manynet}`. -->
<!-- `{manynet}` provides a common set of tools and a standard syntax for analysing many different types of networks. -->
<!-- It offers a broad range of functions to make, manipulate, map, measure, and model: -->
<!-- - one-, two-, and sometimes three-mode networks -->
<!-- - undirected, directed, and sometimes complex networks -->
<!-- - unweighted, weighted, and sometimes signed networks -->

## Installation

### Stable

The easiest way to install the latest stable version of `{manynet}` is
via CRAN. Simply open the R console and enter:[^1]

`install.packages('manynet')`

`library(manynet)` will then load the package and make the data and
tutorials (see below) contained within the package available.

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally. The latest binary
releases for all major OSes – Windows, Mac, and Linux – can be found
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
Github, please install the `{remotes}` package from CRAN and then:

- For latest stable version:
  `remotes::install_github("snlab-ch/manynet")`
- For latest development version:
  `remotes::install_github("snlab-ch/manynet@develop")`

### Tutorials

This package includes tutorials to help new and experienced users learn
how they can conduct social network analysis using the package. These
tutorials leverage the additional package `{learnr}` (see
[here](https://rstudio.github.io/learnr/)), but we have made it easy to
use `{manynet}` or `{migraph}` tutorials right out of the box:

``` r
run_tute()
#> # A tibble: 9 × 3
#>   package name      title        
#>   <chr>   <chr>     <chr>        
#> 1 manynet tutorial0 Intro to R   
#> 2 manynet tutorial1 Data         
#> 3 manynet tutorial2 Visualisation
#> 4 migraph tutorial3 Centrality   
#> 5 migraph tutorial4 Community    
#> 6 migraph tutorial5 Position     
#> 7 migraph tutorial6 Topology     
#> 8 migraph tutorial7 Diffusion    
#> 9 migraph tutorial8 Regression
# run_tute("tutorial1")
```

## Relationship to other packages

This package stands on the shoulders of several incredible packages.

In terms of the objects it works with, this package aims to provide an
updated, more comprehensive replacement for `{intergraph}`. As such it
works with objects in `{igraph}` and `{network}` formats, but also
equally well with base matrices and edgelists (data frames), and formats
from several other packages.

The user interface is inspired in some ways by Thomas Lin Pedersen’s
excellent `{tidygraph}` package, though makes some different decisions,
and uses the quickest `{igraph}` or `{network}` routines where
available.

`{manynet}` has inherited most of its core functionality from its
maternal package, `{migraph}`. `{migraph}` continues to offer more
analytic and modelling functions that builds upon the architecture
provided by `{manynet}`. For more, please check out `{migraph}`
directly.

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).

[^1]: Macs with Macports installed may also install from the command
    line [using Macports](https://ports.macports.org/port/R-manynet/).
