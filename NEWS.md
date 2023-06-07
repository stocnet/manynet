# manynet 1.0.0

2023-06-07

## Package

- Split up the `{migraph}` package, adding the Make, Mold, and Map functions to this package
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

## Molds

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
