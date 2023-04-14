#' Marking networks based on their properties
#'
#' These functions implement logical tests for various network
#' properties.
#' @param .data An object of a manynet-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marks
#' @name is
NULL

# Classes ####

#' @describeIn is Tests whether network is manynet-compatible
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_migraph(create_filled(2))
#' @export
is_migraph <- function(.data) {
  tidygraph::is.tbl_graph(.data) |
    network::is.network(.data) |
    igraph::is.igraph(.data) |
    (is.data.frame(.data) & 
       "from" %in% names(.data) & "to" %in% names(.data)) |
    (is.matrix(.data) & is.numeric(.data))
}

#' @describeIn is Tests whether network contains graph-level information
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_graph(create_star(2))
#' @export
is_graph <- function(.data) UseMethod("is_graph")

#' @export
is_graph.data.frame <- function(.data) FALSE

#' @export
is_graph.matrix <- function(.data) FALSE

#' @export
is_graph.tbl_graph <- function(.data) TRUE

#' @export
is_graph.igraph <- function(.data) TRUE

#' @export
is_graph.network <- function(.data) FALSE

#' @describeIn is Tests whether data frame is an edgelist
#' @examples
#' is_edgelist(matrix(c(2,2), 1, 2))
#' is_edgelist(as_edgelist(matrix(c(2,2), 1, 2)))
#' @export
is_edgelist <- function(.data) UseMethod("is_edgelist")
  
#' @export
is_edgelist.data.frame <- function(.data) {
  ncol(.data) >= 2 & "from" %in% names(.data) & "to" %in% names(.data)
}

#' @export
is_edgelist.matrix <- function(.data) FALSE

#' @export
is_edgelist.network <- function(.data) FALSE

#' @export
is_edgelist.igraph <- function(.data) FALSE

#' @export
is_edgelist.tbl_graph <- function(.data) FALSE

# Formats ####

#' @describeIn is Tests whether network is a two-mode network
#' @importFrom igraph is.bipartite
#' @examples
#' is_twomode(create_filled(c(2,2)))
#' @export
is_twomode <- function(.data) UseMethod("is_twomode")

#' @export
is_twomode.igraph <- function(.data) {
  igraph::is.bipartite(.data)
}

#' @export
is_twomode.tbl_graph <- function(.data) {
  igraph::is.bipartite(.data)
}

#' @export
is_twomode.matrix <- function(.data) {
  out <- dim(.data)[1] != dim(.data)[2]
  if(!out & is_labelled(.data))
    out <- !all(rownames(.data)==colnames(.data))
  out
}

#' @export
is_twomode.network <- function(.data) {
  .data <- as_matrix(.data)
  dim(.data)[1] != dim(.data)[2]
}

#' @export
is_twomode.data.frame <- function(.data) {
  is_edgelist(.data) && 
    length(intersect(.data[,1], .data[,2])) == 0
}

#' @describeIn is Tests whether network is weighted
#' @importFrom igraph is.weighted
#' @examples
#' is_weighted(create_tree(3))
#' @export
is_weighted <- function(.data) UseMethod("is_weighted")

#' @export
is_weighted.igraph <- function(.data) {
  igraph::is.weighted(.data)
}

#' @export
is_weighted.tbl_graph <- function(.data) {
  igraph::is.weighted(.data)
}

#' @export
is_weighted.matrix <- function(.data) {
  !all(.data == 0 | .data == 1)
}

#' @export
is_weighted.network <- function(.data) {
  "weight" %in% network::list.edge.attributes(.data)
}

#' @export
is_weighted.data.frame <- function(.data) {
  ncol(.data)>=3 && 
    ("weight" %in% names(.data) | is.numeric(.data[,3]))
}

#' @describeIn is Tests whether network is directed
#' @importFrom igraph is.directed
#' @importFrom migraph network_reciprocity
#' @examples
#' is_directed(create_tree(2))
#' is_directed(create_tree(2, directed = TRUE))
#' @export
is_directed <- function(.data) UseMethod("is_directed")

#' @export
is_directed.data.frame <- function(.data) {
  !(migraph::network_reciprocity(.data) == 0 |
      migraph::network_reciprocity(.data) == 1)
}

#' @export
is_directed.igraph <- function(.data) {
  if(is_twomode(.data)) FALSE else igraph::is.directed(.data)
}

#' @export
is_directed.tbl_graph <- function(.data) {
  if(is_twomode(.data)) FALSE else igraph::is.directed(.data)
}

#' @export
is_directed.network <- function(.data) {
  .data$gal$directed
}

#' @export
is_directed.matrix <- function(.data) {
  if(is_twomode(.data)) FALSE else !isSymmetric(.data)
}

#' @describeIn is Tests whether network includes names for the nodes
#' @importFrom igraph is.named
#' @examples
#' is_labelled(create_empty(3))
#' @export
is_labelled <- function(.data) UseMethod("is_labelled")

#' @export
is_labelled.igraph <- function(.data) {
  igraph::is.named(.data)
}

#' @export
is_labelled.tbl_graph <- function(.data) {
  igraph::is.named(.data)
}

#' @export
is_labelled.matrix <- function(.data) {
  !is.null(dimnames(.data))
}

#' @export
is_labelled.network <- function(.data) {
  !is.null(dimnames(as_matrix(.data)))
}

#' @export
is_labelled.data.frame <- function(.data) {
  is.character(.data[,1]) & is.character(.data[,2])
}

#' @describeIn is Tests whether network is signed positive/negative
#' @importFrom igraph edge_attr_names
#' @examples
#' is_signed(create_lattice(3))
#' @export
is_signed <- function(.data) UseMethod("is_signed")

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  
  abs(x - round(x)) < tol

#' @export
is_signed.data.frame <- function(.data) {
  all(is.wholenumber(.data[,3])) && any(.data[,3] < 0)
}

#' @export
is_signed.matrix <- function(.data) {
  all(is.wholenumber(c(.data))) && any(.data < 0)
}

#' @export
is_signed.igraph <- function(.data) {
  "sign" %in% igraph::edge_attr_names(.data)
}

#' @export
is_signed.tbl_graph <- function(.data) {
  "sign" %in% igraph::edge_attr_names(.data)
}

#' @export
is_signed.network <- function(.data) {
  "sign" %in% network::list.edge.attributes(.data)
}

#' @describeIn is Tests whether network contains any loops
#' @importFrom igraph is.loop
#' @examples
#' is_complex(create_lattice(4))
#' @export
is_complex <- function(.data) UseMethod("is_complex")

#' @export
is_complex.igraph <- function(.data) {
  any(igraph::which_loop(.data))
}

#' @export
is_complex.tbl_graph <- function(.data) {
  any(igraph::which_loop(.data))
}

#' @export
is_complex.matrix <- function(.data) {
  !(is_twomode(.data) || all(is.na(diag(.data))) || all(diag(.data) == 0))
}

#' @export
is_complex.data.frame <- function(.data) {
  any(.data[,1] == .data[,2])
}

#' @export
is_complex.network <- function(.data) {
  network::has.loops(.data)
}

#' @describeIn is Tests whether network is multiplex,
#'   either from multiple rows with the same sender and receiver,
#'   or multiple columns to the edgelist.
#' @importFrom igraph any_multiple
#' @importFrom migraph network_tie_attributes
#' @examples
#' is_multiplex(create_filled(c(3,3)))
#' @export
is_multiplex <- function(.data) UseMethod("is_multiplex")

#' @export
is_multiplex.matrix <- function(.data) {
  FALSE
}

#' @export
is_multiplex.tbl_graph <- function(.data) {
  igraph::any_multiple(.data) |
    length(migraph::network_tie_attributes(.data)) > 1
}

#' @export
is_multiplex.igraph <- function(.data) {
  igraph::any_multiple(.data) |
    length(migraph::network_tie_attributes(.data)) > 1
}

#' @export
is_multiplex.network <- function(.data) {
  network::is.multiplex(.data)
}

#' @export
is_multiplex.data.frame <- function(.data) {
  ncol(.data) > 3
}

#' @describeIn is Tests whether network is simple (both uniplex and simplex)
#' @importFrom igraph is.simple
#' @examples
#' is_uniplex(create_star(3))
#' @export
is_uniplex <- function(.data) {
  obj <- as_igraph(.data)
  igraph::is.simple(obj)
}

#' @describeIn is Tests whether network is longitudinal, panel data
#' @importFrom migraph network_tie_attributes
#' @examples
#' is_longitudinal(create_tree(5, 3))
#' @export
is_longitudinal <- function(.data) {
  atts <- migraph::network_tie_attributes(.data)
  "wave" %in% atts | "panel" %in% atts
}

#' @describeIn is Tests whether network is dynamic, time-stamped data
#' @importFrom migraph network_tie_attributes
#' @examples 
#' is_dynamic(create_tree(3))
#' @export
is_dynamic <- function(.data) {
  atts <- migraph::network_tie_attributes(.data)
  "time" %in% atts | "beg" %in% atts | "begin" %in% atts | "start" %in% atts
}
