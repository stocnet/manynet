# manynet 1.6.5

## Package

- Removed extracted tute scripts from root folder

## Making

- Exported `describe_*()` functions for describing networks, nodes, ties, and changes

# manynet 1.6.4

## Making

- Added `read_gdf()` for importing GDF files
- Improved mnet object printing to publish the number of each type of tie in multiplex networks

## Modifying

- Fixed `join_nodes()` not recognising nodelists correctly

## Measuring

- Fixed #114 by updating net_heterophily documentation
- Added `node_randomwalk()` closeness centrality
- Added `node_subgraph()` centrality

## Tutorials

- Fixed bug where tutorials were not autoloading necessary packages
- Moved glossary to own script and added more tutorial 5-relevant entries
- Added more glossary keys to tutorial 5

## Data

- Improved `fict_actually` to be a multiplex network (thanks @korakotbua)

# manynet 1.6.3

## Tutorials

- Added more glossary items on centrality, modularity
- Excluded setup details when extracting a tutorial script
- Added French phrases to tutorials 3-6

# manynet 1.6.2

## Making

- Fixed #113 relating to igraph v2.2.0+ and explicit sample_bipartite functions
- Added `read_pkg()` for creating a network of the function interdependencies
within a package

## Modifying

- Added familiar $ operations for `mnet` objects
  - There is now dollar lookup functionality

## Measures

- Fixed bug in `node_coreness()` where data wasn't recognised within pipes

## Memberships

- Added `node_in_core()` for categorising nodes into 2+ core/periphery groups

## Motifs

- Added `net_by_hierarchy()` for characterising networks by their graph theoretic dimensions of hierarchy

## Data

- Added `ison_emotions`
- Renamed `irps_nuclear`
- Separated out data scripts into their different categories

## Tutorials

- Added `{autograph}` to setups for all manynet tutorials
- Improved data tutorial
  - Removed manynet two-mode question from data tutorial
  - Added more detail on examining data to data tutorial
- Improved topology tutorial
  - Using k-coreness in section on core-periphery
  - Added hierarchy multichoice
  - Demonstrated how to interpret hierarchy measure
  - Added more challenging comparison of hierarchy in two networks
  - Updated several glossary terms

# manynet 1.6.1

## Package

- Dropped mapping from function overview on website
- Dropped viz tutorial, moved to `{autograph}`
- Added CITATION
- Improved `run_tute()` and `extract_tute()` to search for `{autograph}`

## Printing

- Improved `describe_changes()` to work with time as well as begin/end tie attributes

## Making

- Added `create_windmill()`
- Added `create_wheel()`
- Added `create_cycle()`

## Modifying

- Fixed `to_uniplex()` test so that it doesn't rely on random sampling

## Marking

- Improved `is_multiplex()` to recognise more tie attributes

## Data

- Added `irps_nuclear_discourse` for goldfish and various testing of dynamic networks
- Added `ison_judo_moves` for judo move sequences

# manynet 1.6.0

## Package

- Dropped viz-related dependencies: `{ggplot2}`, `{ggraph}`, `{patchwork}`, 
`{ggdendro}`, `{concaveman}`, `{gifski}`, `{graphlayouts}`, `{ggforce}`,
and `{BiocManager}`

## Classes

- Added mdate pillar shaft for pretty printing of NAs in messydate variables

## Modifying

- Improved `to_no_missing()` to record missing removal
- Improved `to_no_isolates()` to record isolate removal
- Improved `to_giant()` to record giant component scoping
- Improved `to_matching()` to record matching
- Fixed `to_matching()` triggering warnings from handling NAs (thanks @schochastics for fixing #109)
- Improved `to_mentoring()` to record mentoring
- Improved `to_eulerian()` to record Eulerian pathing

## Measuring

- Improved `net_core()` with method options for calculating correlation, distance, ndiff, and diff

## Membership

- Improved `node_coreness()` to implement Borgatti and Everett's continuous coreness algorithm
- Moved previous functionality of `node_coreness()` to `node_kcoreness()`

## Mapping

- Moved `graphr()`, `graphs()`, and `grapht()` to `{autograph}`
- Moved palette functionality to `{autograph}`
- Moved remaining layouts to `{autograph}`
- Moved theme functionality to `{autograph}`
- Dropped remaining plot functions
- Dropped or avoided using `{autograph}`-related functions in examples to
make package lighter
- Please see `{autograph}` for continued development of these features

# manynet 1.5.1

## Package

- Removed failing URLs

## Modifying

- Improved `to_mode1()` and `to_mode2()` to record projection change

# manynet 1.5.0

## Package

- Updated pandoc setup in pushrelease workflow

## Making

- Dropped print and summary methods for diffs_model (moved to `{migraph}`)

## Modifying

- Added `to_no_missing()` for removing nodes with missing data
- Fixed `as_diffusion.mnet()` so that it includes "diff_model" class

## Measuring

- Dropped plotting methods (moved to `{autograph}`)
- Added Page 2010 citation to `node_richness()` documentation

## Memberships

- Fixed `node_in_leiden()` to use resolution parameter in call to igraph

## Data

- Improved `table_data()` so that it lists components, longitudinal, dynamic, 
and changing data
- Added more description for `fict_lotr`
- Added `irps_revere` data

# manynet 1.4.1

## Making

- Added Deffuant model to `play_learning()`
- Added fatality parameter to `play_diffusion()` for e.g. SID models
- Improvements to `create_ego()`
  - Now accepts an explicit ego in the call, e.g. create_ego("Andy")
  - Now uses pluralization to prompt alter?s
  - Can now create two-mode ego networks
  - Uses new `snet_prompt()` for easier to read consoles 
  - Uses new `snet_minor_info()` for reminding users to assign the function
  - Now names ego in the title of the resulting object
  - Fixed bug where ego duplicated if already in the roster
  
## Modifying

- Extended `to_matching()` to work on the more general class of stable matching
problems, including where nodes have different capacities

## Marking

- Added `node_is_universal()`

## Measuring

- Improved node measure printing with a spark bar

# manynet 1.4.0

## Making

- Updated documentation on "mnet" class objects
- Improved `print.mnet()` to print sections more prettily and 
accommodate changes when present
- Added `print_all()` for printing infinite rows
- Moved `play_diffusions()` to `{migraph}` (but not `play_diffusion()`)
- Improved `play_diffusion()` to return an 'mnet' class object with changes
instead of a 'diff_model' object with hidden network
  - Older behavior can still be obtained using `old_version = TRUE`
- Improved `write_*()` functions by printing picked pathnames to the console

## Modifying

- Added `as_changelist()` for extracting changelists from 'mnet' objects
- Added `as_diffusion.mnet()` for maintaining backward compatibility for
diffusion measures
- Added `as_tidygraph.networkDynamic()` and `as_igraph.networkDynamic()` for 
coercing 'networkDynamic' objects into 'mnet', 'tidygraph', and 'igraph' objects
- Added functions for adding and working with network changes, i.e. changes to network nodes
  - Added `add_changes()` for adding changes to a network,
  including checks and imputing activity and susceptibility
  - Added `mutate_changes()` to mutate changes
  - Added `select_changes()` to select change variables
  - Added `filter_changes()` to filter changes
  - Added `collect_changes()` to collect changes up to a time point
  - Added `apply_changes()` to apply collected changes to a time point
- Added `to_time()` for scoping a longitudinal network to a time point,
including nodal and/or tie changes 
- Added `select_nodes()` for selecting only some nodal variables
- Added `mutate_net()` for working with network information

## Marking

- Added `is_changing()` for identifying networks with a change component
- Added `node_is_mean()` for identifying typical nodes
- Updated `node_is_*()` diffusion marks to work with new .data output
- Internally added expect_mark test helper

## Measuring

- Added `net_strength()` for measuring the number of ties that would need to be
removed from a network to increase its number of components
- Added `net_toughness()` for measuring the number of nodes that would need to be
removed from a network to increase its number of components
- Moved `over_*()` to `{migraph}`
- Updated `node_*()` diffusion measures to work with new .data output
- Improved `net_modularity()` by informing users when a bipartition is used
- Added `net_waves()` for measuring the number of waves in a network

## Motifs

- Renamed `net_hazard()` to `net_by_hazard()`
- Updated `node_by_*()` diffusion motifs to work with new .data output

## Data

- Updated `fict_potter` with composition changes
- Renamed `ison_starwars` to `fict_starwars`, updating it with composition changes and additional coding
- Added `irps_911` network data on 9/11 hijackers and associates

## Learning

- Updated 'community' tutorial with guidance on `node_in_community()`
- Updated 'position' tutorial with guidance on `tie_is_bridge()` and `node_bridges()`
- Moved 'diffusion' tutorial to `{migraph}` to help with dependencies

# manynet 1.3.3

## Modifying

- Fixed bug in `to_waves()` where result was unordered

## Mapping

- `graphr()` now uses categorical palettes (effectively closing #60)

# manynet 1.3.2

## Measuring

- Added `net_correlation()` for calculating the product-moment correlation between networks
- Improved speed of make_network_measure by retrieving single call at parent level

# manynet 1.3.1

## Package

- Fixed miscellaneous documentation issues, re roxygen
- Separated out the data sections in pkgdown

## Modifying

- Added `to_simplex.data.frame()` and `to_simplex.network()`

## Marking

- Fixed bug in `is_aperiodic()` where it would not work in tutorial chunks

## Measuring

- Added `over_membership()` for obtaining summary statistics by a membership vector

## Practicing

- Fixed data pointers in several tutorials
- Added glossary items to diffusion tutorial

# manynet 1.3.0

## Package

- Improved user information in the console
  - manynet cli functions now inherit parent frame
  - Added manynet cli functions for success and progress along, seq, and nodes
- Improved testing architecture
  - Moving to nested testthats
  - Added custom testthat function for expecting values and ignoring dimension names

## Making

- Added some names to created networks (`create_ego()`, `create_empty()`, 
`create_filled()`, `create_ring()`, `create_star()`, `create_lattice()`)

## Modifying

- Fixed how `as_matrix()` handles signed networks
- Added `as_nodelist()` for extracting nodelists from networks into tibbles
- Added `to_cosine()`
- Dropped `to_galois()` until it can be refactored
- Split reformatting documentation into format, reformat, and deformat groups of functions
  - Added `to_signed()` for adding signs to networks
  - Added `to_weighted()` for adding weights to networks

## Mapping

- Fixed default color ordering so that red is the highlight
- Fixed bug in `graphr()` where line types were inferred incorrectly
- Improved `graphr()` so that layouts can now be `snap`ped to a grid,
mileage may vary
- Improved theme handling so that it is set globally (WIP)
  - Added highlight themes
  - Added background themes
  - Added categorical themes
- Improved configurational layouts
  - Added dyad, pentad, and hexad layouts to configurational layouts
  - Renamed quad layout to tetrad layout

## Marking

- Added `node_is_pendant()` for identifying pendant nodes
- Added `node_is_neighbor()` for identifying adjacent nodes
- Added `tie_is_imbalanced()` for identifying ties in imbalanced configurations

## Measuring

- Added `summary.network_measure()` to return z-scores and p-values for measures
- Added `node_vitality()` for measuring closeness vitality centrality
- Fixed #98 by dropping scale and normalization for `node_eigenvector()`

## Memberships

- Improved community detection options for new users
  - Community detection algorithms now reformat networks as necessary or suggest that it be used on only the giant component
  - Added `node_in_community()` which runs through most salient community detection algorithms to find and return the one with the highest modularity
  - Updated documentation on resolution parameters
- Improved `node_in_regular()` to inform user which census is being used
- Renamed `node_by_quad()` to `node_by_tetrad()` to be more consistent with Greek origins
  - Restored and updated documentation about the various configurations
- Added `summary.network_motif()` which returns the z-scores for the motif scores based on random or configurational networks, traces progress
- Fixed bug in `plot.network_motif()` where motif names were not identified correctly, internal make_network_motif now inherits call information

## Modelling

- Added `cluster_cosine()` for another equivalence option
- Added internal documentation for depth_first_recursive_search

## Practicing

- Improved how `run_tute()` fuzzy matched so that insertions are not as costly
- Improved tutorials with glossary architecture
  - Added `gloss()`, `clear_glossary()`, and `print_glossary()` for adding glossaries to tutorials
- Updated tutorials
  - Updated community, position, and topology tutorials with glossaries, free play sections, and gifs
  - Fixed miscellaneous issues in these tutorials
  - Added faction section to community tutorial

## Data

- Added `irps_wwi`, a dynamic, signed network
- Renamed `ison_blogs` to `irps_blogs`, added info
- Renamed `ison_books` to `irps_books`, added info
- Renamed `ison_usstates` to `irps_usgeo`, added info
- Renamed `ison_friends` to `fict_friends`, added info and fixed directed issue
- Renamed `ison_greys` to `fict_greys`, added info
- Renamed `ison_lotr` to `fict_lotr`, added info
- Renamed `ison_thrones` to `fict_thrones`, added info and some additional nodal attributes
- Renamed `ison_potter` to `fict_potter`, added info and combined waves into single object

# manynet 1.2.6

## Package

- Fixed errors when testing without `{concaveman}`
- Fixed errors when plotting some examples without `{ggdendro}`

# manynet 1.2.5

## Package

- Fixed errors when testing on extended features and Suggested packages not available

# manynet 1.2.4

## Package

- Fixed `thisRequires()` bug by testing for interactivity
- Dropped brokerage census examples

## Modifying

- Improved `to_ego()` and `to_egos()` to specify direction

# manynet 1.2.3

## Mapping

- Fixed bug in `graphr()` where user not informed about `{concaveman}` dependency
- Fixed `graphr()` examples

# manynet 1.2.2

## Package

- Updated all tutorials with different themes to make them more distinctive
- Updated centrality tutorial with gifs
- Updated visualisation tutorial with a few extras

## Modifying

- Added `to_dominating()` for extracting the dominating tree of a given network

## Mapping

- Reworked `graphr()` to make function more concise and consistent (thanks @henriquesposito)
  - This allows new functionality and improves debugging moving forward
  
## Measuring

- Updated closeness centrality documentation
- Improved `node_eccentricity()` to allow normalisation, appear in closeness documentation
- Added `node_stress()` as a new betweenness-like centrality measure
- Added `node_leverage()` as a new degree-like centrality measure

# manynet 1.2.1

## Making

- All `read_*()` now print the command used to the console if the (default) file.choose() is used
- Added `read_gml()`
- Updated references, structure, and DOIs to make and modify documentation

## Measures

- Updated references, structure, and DOIs to centrality, diffusion, and other measures, as well as to marks, motifs, memberships, and models
- Added more documentation on `node_power()`

## Data

- Added `ison_dolphins`
- Added `ison_books`
- Added `ison_blogs`

# manynet 1.2.0

## Package

- Added progress updates, information, and unavailability errors to several functions
  - These can be silenced by setting `options(manynet_verbosity ="quiet")`
  - Where a feature is unavailable, users are directed to the Github issues page
- Added console theme to color the startup and various warning or info messages

## Making

- Added `create_ego()` for collecting ego networks through interviews, including arguments for:
  - Indicating whether a roster should be used, otherwise follows a name generator approach
  - Indicating whether nodes should be interpreted, i.e. nodal attributes collected
  - Indicating whether ties between alters should be requested
- Added `create_motifs()` for creating networks that correspond to the isomorphic subgraphs of certain size and format

## Modifying

- Improved `print.mnet()`
  - Prints multiplex types if available
  - Prints both nodesets for two-mode networks
- Added `add_info()` for adding grand info to tidygraph objects
  - This includes the name of the network, node sets and ties, DOI, year and mode of collection
- Fixed `to_unweighted()` so that it passes through unweighted networks correctly

## Mapping

- Added `set_manynet_theme()` to set theme (re #60), but not yet fully implemented

## Marking

- Improved `is_multiplex()` to ignore "name" tie attributes

## Measuring

- Added `node_authority()` and `node_hub()` centrality measures
- Added `node_equivalency()` for calculating four-cycle closure by node
- Extended `net_equivalency()` to one-mode networks

## Members

- Fixed `node_in_equivalence()` to use census directly

## Motifs

- Added plot methods for network_motif and node_motif classes that use `create_motifs()`
- Added `node_by_dyad()` for node level dyad census
- Added `net_by_quad()` for network level quad census
- Fixed `node_by_quad()` to avoid `{oaqc}` dependency (#89), more flexible but slower
- Fixed `print.node_motif()` to convert to tibble and add modes and names where available only upon print
  - The underlying object is still a matrix, used for equivalence and blockmodelling

## Data

- Updated `ison_southern_women` with grand info
- Updated `ison_laterals` with reordered nodes

# manynet 1.1.0

## Package

- Added beautiful message on startup
  - Depends on `{cli}`
  - Reports version number
  - Offers different suggestions each time it is loaded
  - Can be silenced using `suppressPackageStartupMessages()`
- Improved error warnings, `stop()` replaced by `cli::cli_abort()`
- Improved website structure
  - Structured make_, manip_, map_, mark_, measure_, and model_ documentation
  - Improved function overview layout for measures
- Updated Github Actions scripts
  - Updated renaming and releasing binaries (thanks @auzaheta)
  - Using actions/download-artifact@v4 to address dependabot security warning
  - Using actions/upload-artifact@v4
- Removed `{minMSE}` dependency
- Moved `{roxygen2}` dependency to Config/Needs/build
- Dropped unnecessary `{grDevices}` and `{png}` dependencies
- Dropped unnecessary `{network}` reexports
- Using globalVariables
- Improved `run_tute()` and `extract_tute()` to look for installed packages and report progress

## Making

- Added `read_cran()` for creating networks of package dependencies on CRAN
  - Gathers dependency graph for whole CRAN by default, but can also trace successive outgoing dependencies from a single package (speed increase depends on the package, but is nearly 9x faster for `{manynet}`)
  - Also gathers nodal information about whether compilation is required
- Added `generate_man()` for generating dyad census conditional uniform graphs
- Fixed #86 by making sure igraph::sample_islands in `generate_islands()` only takes a single integer and not a vector

## Manipulating

- Improved `as_tidygraph()` to add an additional class 'mnet' that is used for prettier printing
  - Uses `make_mnet()` (internally) for future-proofing
  - `print.tbl_graph()` renamed to `print.mnet()`
  - `print.mnet()` uses 'grand' data if available
- Improved `bind_ties()` to be more flexible about the input it accepts, converting all input into the required edgelist
- Added `to_ego()` for obtaining a single neighbourhood
- Added tbl_graph and network methods for `delete_nodes()` and `delete_ties()`
- Added examples for `add_ties()` and `delete_ties()` in documentation
- Fixed bug in `to_unnamed.igraph()` when used with already unlabelled networks

## Marking

- tie_marks are now named vectors
  - Printing vectors no longer requires unique names
- Added `tie_is_path()` for tracing the ties on a particular path
- Added `tie_is_triplet()` for returning all the ties that are members of transitive triplets
- Added `tie_is_forbidden()` for identifying ties in forbidden triads
- Improved `tie_is_transitive()` efficiency, now only retrieves the edgelist once
- Improved `is_aperiodic()` to remove `{minMSE}` dependency and offer a progress bar if it takes longer than 2 seconds
- Fixed bug in `tie_is_triangular()` to do with altpath naming

## Measuring

- tie_measures are now named vectors
- Added `node_distance()` for measuring the distance from or to particular nodes
- Fixed #84 related to how `node_degree()` processed isolates in calculating strength in weighted networks

## Data

- Updated all `ison_` data with new `as_tidygraph()`
- Added 'grand' data to `ison_adolescents` as a test
- Added `ison_thrones` on kinship arcs between Game of Thrones characters, with 'grand' data
- Consolidated `ison_monastery_` data into `ison_monks`, a single multiplex, signed, weighted, longitudinal network

# manynet 1.0.5

## Making

- Added `create_degree()` for creating networks of a given degree sequence, including _k_-regular graphs
- Added `generate_citations()` for citation models
- Added `generate_fire()` for forest-fire models
- Added `generate_islands()` for island models
- `create_explicit()` now has its own documentation

## Marking

- Added `tie_is_triangular()` for identifying ties in triangles
- Added `tie_is_cyclical()` for identifying ties in cycles
- Added `tie_is_transitive()` for identifying ties involved in transitive closure
- Added `tie_is_simmelian()` for identifying Simmelian ties

## Manipulating

- `generate_permutation()` renamed to `to_permuted()`

## Mapping

- Updated how `graphr()` plots edges in directed networks
- Removed automatic legends for signed networks
- Fixed other legends issues

## Data

- `table_data()` can now report on data from multiple packages
  - `{manynet}` and `{migraph}` are included by default,
  and if any are not installed they are just ignored
- `table_data()` can now filter by any reported formats,
such as 'directed' or 'twomode'

## Website

- Added more structure to Modifying section
- Added more structure to Mapping section

# manynet 1.0.4

## Modifying

- `as_matrix.igraph()` now only draws from the "weight" attribute and not, e.g. "type"
- Fixed bug in `to_blocks()` related to categorical membership variables

## Marks

- Tie marks now infer networks when used within e.g. `mutate_ties()`

## Memberships

- `node_names()` now returns names of the form "N01" etc for unlabelled networks
- Fixed how `plot.matrix()` works for unlabelled networks
- Added more on density in community tutorial

## Mapping

- British spelling arguments now appear further back in e.g. `graphr()`
- Fixed how `graphs()` recognises ego networks so it is compatible with other splits

# manynet 1.0.3

## Mapping

- Fixed #73 "node_color" mapping issue with `graphr()`
- Fixed issues with variable name warnings in `graphr()`
- Fixed #76 numeric "node_size" issues with `graphr()` function
- Closed #66 so `graphs()` automatically uses "star" layout to plot ego networks
- Fixed #41 so arguments for `graphr()`, `graphs()`, and `grapht()` also accept British spellings

## Memberships

- Fixed bugs in hierarchical community detection algorithms for unconnected networks

# manynet 1.0.2

## Package

- Added alttext to images in the README, tutorials, and website
- Added CRAN link to homepage
- Updated favicons
- Added more structure to the function overview

## Making

- Updated intro tutorial with images, exercises, questions, and more explanation and structure
- Updated data tutorial with images, new function names, questions, and more explanation and structure
- Updated data tutorial with more details on adding and deleting nodes, ties, and attributes
- Updated topology tutorial with new function names and more structure

## Mapping

- Updated viz tutorial with examples of `graphs()` and `grapht()`
- Updated viz tutorial with more details and examples on colors and theming
- Updated viz tutorial with overview, examples, and details on layouts,
including force-directed, layered, circular, spectral, and grid layouts

## Measures

- Updated centrality tutorial with images, new function names, and more structure
- Updated centrality tutorial with more interpretation of centrality measures
- Updated position tutorial with new function names and more structure

## Memberships

- Updated community tutorial with new function names and more structure

# manynet 1.0.1

## Package

- Test of `to_subgraphs()` no longer sampled

# manynet 1.0.0

## Package

- Updated `{manynet}` logo with stocnet GitHub address and color blind safe colorway
- Fixed bug in diffusion tutorial because of undeclared `{minMse}` dependency
- Renamed all functions starting with the `network_*` prefix to `net_*` for conciseness
- Migrated network measures, membership, motifs, and models' functions from `{migraph}`
- Migrated community, position, topology, and diffusion tutorials from `{migraph}`
- Added descriptions to tutorials

## Making

- Fixed bug in `create_core()` where the membership inferred when passing an
existing network was incorrect
- Added `generate_configuration()` for generating configuration models
(including for two-mode networks)
- `play_diffusion()` now includes an explicit contact argument to control the
basis of exposure

## Marking

- `node_is_*()` functions now infer network data context
- Added `node_is_independent()` for identifying nodes among largest independent sets
- `is_multiplex()` now excludes reserved tie attribute names other than type,
such as "weight", "sign", or "wave"
- Added `is_attributed()` to check for non-name nodal attributes
- Fixed issues with ordering in `node_is_latent()`, `node_is_recovered()`,
and `node_is_infected()` (closes #71)
- Added list methods for `is_twomode()`, `is_labelled()`, and `is_complex()`

## Mapping

- Renamed functions to graph networks `graphr()`, `graphs()`, and `grapht()`
(`autographr()`, `autographs()`, and `autographd()` are now deprecated)
- node_size now an aesthetic, allowing `scale_size(range = c(...,...))` to be used
- Reexported `scale_size()` from `{ggplot2}`
- `graphr()` now rescales node size depending on network size (closes #51)
- Fixed issues with "hierarchy" layout for two mode network (closes #61)
- Updated "concentric" layout to accept unlabeled nodes (closes #68)

## Modifying

- `to_named()` now randomly generates and adds an alphabetic sequence of names,
  where previously this was just a random sample,
  which may assist pedagogical use
  - baby_names (internal) now includes a few extra "Q" and "U" names
- Added `to_correlation()` that implements pairwise correlation on network
- Added `arrange_ties()` for `{dplyr}`-like reordering of ties based on some attribute
- Added `to_correlation()` for calculating the Pearson correlation
  - This takes a method argument for "all", "diag", "recip", or "complex"
  - These are similar to functions implemented by Ron Breiger and shared by him in correspondence
- Fixed bug in `as_diff_model()` where events were out of order and named
  
## Marking

- `is_multiplex()` now recognises "date", "begin", and "end" as reserved

## Measuring

- Migrated measures from `{migraph}`
  - Centrality measures
    - Degree-related centrality measures include `node_degree()`, `node_deg()`, 
    `node_indegree()`, `node_outdegree()`, `node_multidegree()`, `node_posneg()`, 
    `tie_degree()`, `net_degree()`, `net_indegree()`, and `net_outdegree()`
    - Betweenness-related centrality measures include `node_betweenness()`,
    `node_induced()`, `node_flow()`, `tie_betweenness()`, and `net_betweenness()`
    - Closeness-related centrality measures include `node_closeness()`,
    `node_reach()`, `node_harmonic()`, `node_information()`, `tie_closeness()`,
    `net_closeness()`, `net_reach()`, and `net_harmonic()`
    - Eigenvector-related centrality measures include `node_eigenvector()`, 
    `node_power()`, `node_alpha()`, `node_pagerank()`, `tie_eigenvector()`, and
    `net_eigenvector()`
  - Closure measures include `net_reciprocity()`,
  `node_reciprocity()`, `net_transitivity()`, `node_transitivity()`,
  `net_equivalency()`, and `net_congruency()`
  - Cohesion measures include `net_density()`, `net_components()`, `net_cohesion()`,   
  `net_adhesion()`, `net_diameter()`, `net_length()`, and `net_independence()`
  - Diffusion measures include `net_transmissibility()`, `net_recovery()`, 
  `net_reproduction()`, `net_immunity()`, `net_hazard()`,
  `net_infection_complete()`, `net_infection_total()`, `net_infection_peak()`,
  `node_adoption_time()`, `node_thresholds()`, `node_recovery()`, and
  `node_exposure()`
  - Heterogeneity measures include `net_richness()`, `node_richness()`,
  `net_diversity()`, `node_diversity()`, `net_heterophily()`,
  `node_heterophily()`, `net_assortativity()`, and `net_spatial()`
  - Hierarchy measures include `net_reciprocity()`, `net_connectedness()`,
  `net_efficiency()`, and `net_upperbound()`
  - Holes' measures include `node_bridges()`, `node_redundancy()`, `node_effsize()`,   
  `node_efficiency()`, `node_constraint()`, `node_hierarchy()`, `node_eccentricity()`, 
  `node_neighbours_degree()`, and `tie_cohesion()`
  - Features' measures include `net_core()`, `net_richclub()`, `net_factions()`, 
  `node_partition()`, `net_modularity()`, `net_smallworld()`, `net_scalefree()`, 
  `net_balance()`, `net_change()`, and `net_stability()`
- Updated properties from mapping to measuring
- Updated attributes from mapping to measuring
    - Renamed `node_mode()` (deprecated) to `node_is_mode()` since it returns a
    logical vector
    - Updated `node_attribute()` and `tie_attribute()` to return measures
    when the output is numeric
- Updated `node_exposure()` to work with two-mode and signed networks
- Fixed `node_constraint()` to work with weighted two-mode networks, thanks to Toshitaka Izumi for spotting this
- All measure functions can now be used in e.g. `mutate()` without specifying `.data`
- Added `net_independence()` for calculating the number of nodes in the largest independent set
- `node_coreness()` now returns 'node_measure' output
- `node_exposure()` now sums tie weights where passed a weighted network

## Members

- Migrated members from `{migraph}`
  - Cliques memberships `node_in_roulette()` (previously `node_roulette()`)
  - Community and hierarchical memberships includes `node_in_optimal()`, 
  `node_in_partition()` (previously `node_kernaghinlin()`),
  `node_in_infomap()`, `node_in_spinglass()`, `node_in_fluid()`, 
  `node_in_louvain()`, `node_in_leiden()`, `node_in_betweenness()`,
  `node_in_greedy()`, `node_in_eigen()`, and `node_in_walktrap()`
  - Components' memberships include `node_in_component()`, `node_in_weak()`, and
  `node_in_strong()` (NB: `node_in_component()` is no longer phrased in the plural)
  - Core-periphery memberships include `node_is_core()` and `node_coreness()`
  - Diffusion memberships include `node_in_adopter()`
  - Equivalence memberships include `node_in_equivalence()`, `node_in_structural()`,
  `node_in_regular()`, and `node_in_automorphic()`
  - Note that these functions were previously named `node_*()`, but including the preposition `_in_` is more consistent.
- `node_member` class is now categorical
  - `make_node_member()` now converts numeric results to LETTER character results
  - `print.node_member()` now works with categorical membership vectors
  - `print.node_member()` now declares how many groups before reporting the vectors
- All membership functions can now be used in e.g. `mutate()` without specifying `.data`
- Hierarchical clustering algorithms now return dendrograms

## Motifs

- Migrated motifs to take census of network, node positions, and brokerage from `{migraph}`,
these include `node_by_tie()`, `node_by_triad()`, `node_by_quad()`, `node_by_path()`, 
`net_by_dyad()`, `net_by_triad()`, `net_by_mixed()`, `node_by_brokerage()`, `net_by_brokerage()`
  - Note that these functions were previously named `*_*_census()`, but the preposition `_by_` is more consistent.
- `node_tie_census()` now works on longitudinal network data
- Fixed bug where `print.node_motif()` wasn't printing the requested number of lines

## Methods

- Migrated methods from `{migraph}`
  - Methods for equivalence clustering include `cluster_hierarchical()` and `cluster_concor()`
  - Methods for selecting clusters include `k_strict()`, `k_elbow()`, and `k_silhouette()`
- Several improvements to `cluster_concor()`
  - `cluster_concor()` now uses `to_correlation()` for initial correlation
  - It still uses `stats::cor()` for subsequent iterations
  - Fixed how `cluster_concor()` handles unlabelled networks
  - Fixed how `cluster_concor()` handles two-mode networks
  - Fixed bug where `cluster_concor()` cutoff resulted in unsplit groups
- `cluster_hierarchical()` now also uses `to_correlation()`

## Data

- Added `ison_greys` dataset, including some corrections to that published in `{networkdata}`
- Updated `ison_friends` dataset to be explicitly longitudinal
- Updated `ison_usstates` dataset with population data (Alaska and Hawaii missing)
- Updated `ison_southern_women` dataset with surnames, titles, event dates, and corrected ties
- Updated data documentation with revised multiplex definition

# manynet 0.4.4

2024-03-15

## Modifying

- Fixed documentation issue with `to_scope()` for CRAN resubmission

# manynet 0.4.3

2024-03-13

## Mapping

- Fixed issues with `autographr()` examples that were taking too long to run
- Separated documentation for `autographr()`, `autographs()`, and `autographd()` functions
- Closed #64 by adding aliases for `autographr()`, `autographs()`, and `autographd()` functions
- Closed #65 by fixing bugs with how `node_is_infected()`, `node_is_recovery()`, `node_is_latent()` work for network lists

# manynet 0.4.2

2024-03-12

## Making

- Closed #57 by updating `play_diffusions()` to revert future plan on exit
- Fixed bug with how `generate_random()` works for two-mode networks with specified number of ties

## Mapping

- Closed #6 by updating how "lineage" layout works and places nodes on Y axis
- Closed #39 by making `autographr()` more flexible and efficient in setting variables to aesthetics
- Updated themes to be compatible with newer and older versions of `{ggplot2}`
- Added "configuration" layout for small triad/quad networks

## Modifying

- Updated `to_reciprocated.matrix()` to consistently work with matrices

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
