# manynet 0.4.2

2024-03-11

## Making

- Closed #57 by updating `play_diffusions()` to revert future plan on exit

## Mapping

* Closed #6 by updating "lineage" layout.
* Closed #39 by making `autographr()` more flexible and efficient in setting variables to aesthetics.

# manynet 0.4.1

2023-12-24

## Package

- Fixed minor documentation bug in `run_tute()`
- pkgdown urls now point to "stocnet/manynet"
- Closes #34 by pushing changes to CRAN

# manynet 0.4.0

2023-12-24

## Package

- Major overhaul of documentation
  - Moved function description up so that it appears in tooltips
  - Split documentation of many functions to provide more space for explanation
  - Moved some functions around for more thematic documentation
- Added `pkg_data()` to report an overview of data contained within the package(s)
- Updated README and DESCRIPTION with new organisational affiliation: stocnet/manynet
- Updated tests to be consistent with future `{ggplot2}` release

## Making

- Migrated `play_diffusion()` and `play_diffusions()` from `{migraph}`
  - Including class creation, and `print()`, `summary()`, and `plot()` methods
- Migrated `play_learning()` and `play_segregation()` from `{migraph}`
  - Including class creation, and `print()`, `summary()`, and `plot()` methods
- Fixed bug in `create_tree()` where it was not returning a two-mode network correctly

## Modifying

- Added `as_diffusion()` to coerce a table of diffusion events into diff_model class
  - `as_*()` functions are now considered modifications
- Completed a full range of tidy verbs for nodes
  - Added `mutate_nodes()`
  - Added `filter_nodes()`
  - Added `rename_nodes()`
- Completed a full range of tidy and igraph verbs for ties
  - Added `bind_ties()`
  - Added `delete_ties()`
- Added `to_tree()` to find one or more spanning trees amongst a network's ties
- Added `from_ties()` to collect multiple networks into a multiplex network

## Marking

- Renamed `is.igraph()` to `is_igraph()` for igraph v2.0.0
- Added `is_list()` for identifying a list of networks
- Migrated `node_is_core()`, `node_is_cutpoint()`,  `node_is_fold()`, `node_is_isolate()`,  `node_is_mentor()` from `{migraph}`
- Migrated `node_is_exposed()`, `node_is_infected()`, `node_is_latent()`, `node_is_recovered()` from `{migraph}`
- Migrated `node_is_max()`, `node_is_min()`, `node_is_random()` from `{migraph}`
- Migrated `tie_is_bridge()`, `tie_is_loop()`,  `tie_is_multiple()`, `tie_is_reciprocated()` from `{migraph}`
- Added `tie_is_feedback()`
- Migrated `tie_is_max()`, `tie_is_min()` from `{migraph}`
- Added `tie_is_random()`

## Mapping

- Added `scale_edge_color_centres()`,  `scale_edge_color_ethz()`,  `scale_edge_color_iheid()`,  `scale_edge_color_rug()`,  `scale_edge_color_sdgs()`
  - Fixed bug that meant edge scales were unavailable (closes #43)
- `autographr()` now provides legends by default where multiple colours are used (closes #52)
- `autographs()` now labels legends correctly for binary variables (closes #38)
- `autographs()` now graphs just the first and last networks in a list (closes #45)
- `autographs()` now includes an option whether the layout should be based on the first, last, or both of two networks (closes #48)

## Data

- Renamed `ison_konigsberg` to `ison_koenigsberg` and named the bridges
- `ison_algebra` now in long multiplex format
- `ison_karateka` now weighted, anonymous members are named by number, and "obc" variable renamed "allegiance"
- `ison_lawfirm` enlarged from 36 to 71 nodes and now consists of three multiplex, directed networks
- `ison_southern_women` names are now title case
- Added `ison_hightech`, a multiplex, directed network from Krackhardt 1987
- Added four `ison_monastery` datasets, three of which are signed and weighted, and the other is longitudinal, from Sampson 1969 (closes #49)
- Added six `ison_potter` datasets in a list of networks, from Bossaert and Meidert 2013 (closes #47)
- Added `ison_usstates` data on the contiguity of US states, from Meghanathan 2017

# manynet 0.3.1

2023-12-17

## Making

* `as_tidygraph.diff_model()` no longer creates names for unlabelled networks

## Modifying

* `to_waves.diff_model()` now adds three logical vectors as variables, "Infected", "Exposed", and "Recovered"
  - Relies on parallels to migraph's `node_is_latent()`, `node_is_infected()`, and `node_is_recovered()`
  - Fixed bug in wave naming

## Mapping

* `autographr()` now shapes seed, adopter, and non-adopter nodes using a parallel to migraph's `node_adoption_time()` for 
  - Improved guide/legend labelling and positioning
* `autographs()` now colors susceptible, exposed, infected, and recovered nodes correctly
* `autographd()` now colors susceptible, exposed, infected, and recovered nodes correctly

# manynet 0.3.0

2023-12-15

## Package

* Overhaul of the README to summarise many of the unique aspects of the package (closed #36)

## Making

* Added `as_tidygraph()` method for diff_model objects
* Added `as_siena()` method for tidygraph objects

## Modifying

* Manipulating functions now called Modifying
* `to_waves()` now works on diff_model objects, add attributes and names
* `is_multiplex()` now recognises a tie/edge 'type' attribute as evidence of multiplexity
* Deleted unnecessary reexports from other packages
  - `igraph::is_bipartite()` is superseded by `is_twomode()`
  - `tidygraph::activate()` is superseded by `mutate_ties()` and similar functions
* Fixed bug by replacing older `igraph::as_incidence_matrix()` and `igraph::graph_from_incidence_matrix()`
with `igraph::as_biadjacency_matrix()` and `igraph::graph_from_biadjacency_matrix()`

## Mapping

* `autographr()` now plots diff_model objects, showing the diffusion as a heatmap on the vertices
* `autographs()` and `autographd()` now utilise network information in diff_model objects to provide better layouts (closed #17)
* Fixed bug with specifying `node_size` in `autographd()`
* `many_palettes` replaces `iheid_palette`
* Added new palettes, themes and scales for graphs (closed #9)
  - `theme_ethz()`, `scale_color_ethz()`/`scale_colour_ethz()`, and `scale_fill_ethz()` for ETH Zürich
  - `theme_uzh()`, `scale_color_uzh()`/`scale_colour_uzh()`, and `scale_fill_uzh()` for Uni Zürich
  - `theme_rug()`, `scale_color_rug()`/`scale_colour_rug()`, and `scale_fill_rug()` for Uni Gröningen

## Data

* Added `ison_physicians` data that includes four, multiplex networks with adoption data

# manynet 0.2.10

2023-12-06

## Package

* Fixed documentation issues with `run_tute()`

## Making

* Added `read_graphml()` and `write_graphml()` for importing and exporting graphml objects, mostly wrappers for igraph functions.

## Mapping

* `autographd()` and `autographs()` can now be used for plotting diffusion models.
  * Updates to `to_waves()` and `autographd()` to account for 'exposed' nodes in diffusion models.
* Updates to layouts
  * Updates to `hierarchical` layout so that node name can be specified for centering the layout
  * Updated `theme_heid()` layout

## Data

* Added faction attribute to `ison_starwars` data, thanks to coding by Yichen Shen and Tiphaine Aeby

# manynet 0.2.9

2023-11-15

## Package 

* Updated `run_tute()`function to "fuzzy" match tutorial names

## Mapping

* Added `+.ggplot()` method for visualising multiple plots in the same pane
* Added custom theme `theme_iheid` for plots
* Added custom `scale_` family of functions for changing colour scales in plots 
* Updated `autographr()`:
  * Added "center" option for hierarchy layout that places events or actors in the middle alike "bip" and "bip3" layouts
  * Added "lineage" layout that fixes node positions according to values in y axis
* Fixed bugs with `autographd()` function

# manynet 0.2.8

2023-11-02

## Package

- Fixed tutorials documentation issue for CRAN

# manynet 0.2.7

2023-11-01

## Making

- Updated treatment of adjacency matrices in `as_igraph()` in accordance with upcoming updates to `{igraph}` package (closing #27)

## Manipulating

- Added `to_mentoring` function

## Mapping

- Fixed bugs in `autographr()` related to `edge_size` and `edge_color`
- Fixed bugs and updated label placements for "circular" and "concentric" layouts for `autographr()`
- Fixed issues with self-ties in `autographr()`
- Updated tests for `autographr()`

## Data

- Added `ison_friends`, a one-mode network on character connections of a popular TV series

# manynet 0.2.6

2023-10-25

## Package

- Added documentation for tutorial helper functions

## Making

- `generate` examples leverage `autographr()` again

## Manipulating

- Fixed bug in `to_redirected.tbl_graph()`
- `print.tbl_graph()` no longer mentions the object class

## Mapping

- `layout_tbl_graph_concentric()` now works with two-mode networks, multiple levels for one-mode networks, and accepts new vectors
- Added `layout_tbl_graph_multilevel()` for laying out multilevel networks
- Added `layout_tbl_graph_triad()` and `layout_tbl_graph_quad()` configurational layouts

## Data

- Added `ison_starwars`, a collection of seven weighted interaction networks on a popular film franchise
- `ison_networkers` names are now in title case, not all caps

# manynet 0.2.5

2023-10-19

## Package

- Added 'tutorial0' for those less comfortable with R
- Added `run_tute()` helper for quicker access to `{manynet}` and `{migraph}` tutorials
- Added `extract_tute()` for extracting the main code examples from `{manynet}` and `{migraph}` tutorials
- Added `purl = FALSE` to tutorial chunks that are not needed for extraction (thanks @JaelTan)
- Updated website with new functions `run_tute()` and `extract_tute()`

## Mapping

- Fixed remaining issues with `node_group` and updated documentation

# manynet 0.2.4

2023-10-11

## Package

* README now includes Macports installation instructions
* Added `{graphlayouts}`, `{ggforce}`, and `{multiplex}` to Suggested

## Manipulating

* Added `to_galois()` for transforming networks into partially ordered Galois lattices

## Mapping

* Fixed bug in node_shape argument in `autographr()`
* Fixed bug in node_group argument in `autographr()`, closes #11
* Fixed bug in inconsistent default colour scheme for node_color and node_group in `autographr()`
* `autographr()` now automatically bends arcs for reciprocated ties when directed network is not too large/dense
* `autographr()` now accepts unquoted variables as arguments
* `autographr()` now uses `graphlayouts::layout_igraph_multilevel` where appropriate
* Default node labelling now larger and further from nodes in circular layout

## Data

- `ison_algebra` now an anonymised network (again)

# manynet 0.2.3

2023-09-17

## Package

* Fixed URL in read_

# manynet 0.2.2

2023-09-17

## Package

* README now points to `{migraph}` earlier

## Tutorials

* Fixed various bugs in first (data) tutorial

## Map

* Fixed explicit node_shape bug

## Data

* Added `ison_lawfirm` data from Lazega, see documentation for more details
* Upgraded ison_marvel data to latest igraph specification

# manynet 0.2.1

2023-08-11

## Map

* Fixed documentation issues with map_palettes

# manynet 0.2.0

2023-08-11

## Package 

* Closed #4 by adding `thisRequiresBio()` helper function to download Bioconductor packages
* Upgraded ison data to latest igraph specification
  * Added `ison_konigsberg` for illustrating Seven Bridges of Konigsberg
  * Removed `ison_brandes2` and added potential modal type as extra variable to `ison_brandes`
  * Consolidated `ison_bb`, `ison_bm`, `ison_mb`, and `ison_mm` into a list of networks called `ison_laterals`

## Make

* Added `create_explicit()` for creating networks based on explicit nodes and ties

## Manip

* Added `delete_nodes()` for deleting specific nodes
* Added `to_eulerian()` function that returns a Eulerian path network, if available, from a given network

## Map

* Moved additional `is_` functions from `{migraph}`
  * Added `is_connected()` to test if network is strongly connected
  * Added `is_perfect_matching()` to test if there is a matching for every node in the network
  * Added `is_eulerian()` to test whether there is a Eulerian path for a network
  * Added `is_acyclic` to test whether network is a directed acyclic graph
  * Added `is_aperiodic` to test whether network is aperiodic
* Added partition layouts
  * Added `layout_tbl_graph_alluvial()` that places successive layers horizontally
  * Added `layout_tbl_graph_concentric()`that places a "hierarchy" layout around a circle
  * Added `layout_tbl_graph_hierarchy()` that layers the nodes along the top and bottom sequenced to minimise overlap
  * Added `layout_tbl_graph_ladder()`that aligns nodes across successive layers horizontally
  * Added `layout_tbl_graph_railway`  that aligns nodes across successive layers vertically
* Added `theme_iheid()` function that themes graphs with colors based on the Geneva Graduate Institute

# manynet 0.1.2

2023-06-20

## Package

- Added tutorials for package
  - Moved and updated "data" tutorial from `{migraph}` 
  - Moved and updated "visualisations" tutorial from `{migraph}`

# manynet 0.1.1

2023-06-09

## Package

- Removed commented out examples
- Added more detail on what various functions return

## Manipulates

- Added `na_to_mean.data.frame()`

## Maps

- Added `network_dims.network()`

# manynet 0.1.0

2023-06-07

## Package

- Split up the `{migraph}` package, adding the Make, Manipulate, and Map functions to this package
- Added package documentation
  - Added .github files
  - Added README structured around the problems the package solves
- Improved consistency and readability
  - Functions that take data as their first argument has that first argument named `.data`
  - Added new `print.tbl_graph` method that offers easy to interpret information
  - Note that this method is exported but not currently documented
- Reduced package dependencies considerably
  - Many are now weak dependencies; a dialog is invoked when the calling function is used for the first time and the user is prompted to install the missing package
- Added extensive testing
- Added new 'manynet' logo

## Makes

- Added `read_*()` functions, e.g. `read_edgelist()`
- Added `write_*()` functions, e.g. `write_edgelist()`
  - Added `write_matrix()` for exporting to matrices
- Added `create_*()` functions, e.g. `create_lattice()`
  - All `create_*()` functions return `tbl_graph` class objects
- Added `generate_*()` functions, e.g. `generate_smallworld()`
- Added `ison_*` network data, e.g. `ison_southern_women`

## Manipulates

- Added `as_*()` functions, e.g. `as_igraph()`
  - Fixed `as_edgelist.network()`
  - Fixed `as_network.data.frame()`
  - Fixed `as_network.tbl_graph()`
- Added `join_*()` functions, e.g. `join_ties()`
- Added `add_*()` functions, e.g. `add_node_attribute()`
  - All `create_*()` functions return `tbl_graph` class objects
- Added `mutate_*()` functions, e.g. `mutate_ties()`
  - With `mutate_ties()`, it is no longer necessary to `activate(edges)`
- Added `rename_*()` functions, e.g. `rename_ties()`
- Added `is_*()` functions, e.g. `is_dynamic()`
  - Fixed `is_labelled()` to work correctly with multiple network formats
- Added `to_*()` functions, e.g. `to_mode1()`
  - This includes functions for reformatting, transforming, and splitting
  - This also includes functions for returning networks with
  missing data replaced by some imputed values
  - Fixed `to_giant.network()`
  - `to_directed()` now a method
  - `to_subgraphs()` now returns a list of `tbl_graph`s
  - `to_reciprocated()` now works on edgelists, matrices, tbl_graphs, and networks
  - `to_acylic()` now works on matrices, tbl_graphs, and networks
- Added `from_*()` functions, e.g. `from_egos()`
  - Fixed `from_subgraphs()`
- Added "grab" functions, e.g. `network_nodes()`
  - `network_dims()` is now a method

## Maps

- Added `autographr()`, `autographs()`, and `autographd()`
- Added layout functions
  - Fixed `layout_tbl_graph_concentric()`
