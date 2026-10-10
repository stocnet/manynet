#' @return A `stocnet` object is returned,
#'   which can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, `as_igraph()`, or `as_network()`.
#'   `create_ring()`, `create_lattice()`, 
#'   `generate_random()`, `generate_smallworld()`, and
#'   `generate_scalefree()` return a `tbl_graph` or `igraph` object for now,
#'   since the released versions of the packages built on this one expect
#'   one from them.
#'   
#'   By default, most networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, the directed argument is ignored.
