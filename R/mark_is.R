# Classes ####

#' Marking networks classes
#'
#' @description
#'   These functions implement logical tests for networks' classes.
#'   
#'   - `is_manynet()` marks a network TRUE if it is compatible with `{manynet}` functions.
#'   - `is_edgelist()` marks a network TRUE if it is an edgelist.
#'   - `is_graph()` marks a network TRUE if it contains graph-level information.
#'   - `is_list()` marks a network TRUE if it is a (non-igraph) list of networks,
#'   for example a set of ego networks or a dynamic or longitudinal set of networks.
#'   - `is_longitudinal()` marks a network TRUE if it contains longitudinal, panel data.
#'   - `is_dynamic()` marks a network TRUE if it contains dynamic, time-stamped data
#'   
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#' @param .data An object of a manynet-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marking
#' @name is
NULL

#' @rdname is
#' @importFrom igraph is_igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_manynet(create_filled(2))
#' @export
is_manynet <- function(.data) {
  tidygraph::is.tbl_graph(.data) |
    network::is.network(.data) |
    igraph::is_igraph(.data) |
    (is.data.frame(.data) & 
       "from" %in% names(.data) & "to" %in% names(.data)) |
    (is.matrix(.data) & is.numeric(.data))
}

#' @rdname is
#' @importFrom igraph is_igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_graph(create_star(2))
#' @export
is_graph <- function(.data) UseMethod("is_graph")

#' @export
is_graph.data.frame <- function(.data){FALSE}

#' @export
is_graph.matrix <- function(.data){FALSE}

#' @export
is_graph.tbl_graph <- function(.data){TRUE}

#' @export
is_graph.igraph <- function(.data){TRUE}

#' @export
is_graph.network <- function(.data){TRUE}

#' @rdname is
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
is_edgelist.matrix <- function(.data){FALSE}

#' @export
is_edgelist.network <- function(.data){FALSE}

#' @export
is_edgelist.igraph <- function(.data){FALSE}

#' @export
is_edgelist.tbl_graph <- function(.data){FALSE}

#' @rdname is
#' @export
is_list <- function(.data) {
  inherits(.data, "list") && !is_manynet(.data)
}

#' @rdname is
#' @examples
#' is_longitudinal(create_tree(5, 3))
#' @export
is_longitudinal <- function(.data) {
  atts <- igraph::edge_attr_names(as_igraph(.data))
  "wave" %in% atts | "panel" %in% atts
}

#' @rdname is
#' @examples 
#' is_dynamic(create_tree(3))
#' @export
is_dynamic <- function(.data) {
  atts <- igraph::edge_attr_names(as_igraph(.data))
  "time" %in% atts | "beg" %in% atts | "begin" %in% atts | "start" %in% atts
}

# Formats ####

#' Marking networks formats
#'
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_twomode()` marks networks TRUE if they contain two sets of nodes.
#'   - `is_weighted()` marks networks TRUE if they contain tie weights.
#'   - `is_directed()` marks networks TRUE if the ties specify which node
#'   is the sender and which the receiver.
#'   - `is_labelled()` marks networks TRUE if there is a 'names' attribute
#'   for the nodes.
#'   - `is_signed()` marks networks TRUE if the ties can be either positive
#'   or negative.
#'   - `is_complex()` marks networks TRUE if any ties are loops,
#'   with the sender and receiver being the same node.
#'   - `is_multiplex()` marks networks TRUE if it contains multiple types 
#'   of ties, such that there can be multiple ties between the same
#'   sender and receiver.
#'   - `is_uniplex()` marks networks TRUE if it is neither complex nor multiplex.
#' @inheritParams is
#' @family marking
#' @name is_format
NULL

#' @rdname is_format
#' @importFrom igraph is_bipartite
#' @examples
#' is_twomode(create_filled(c(2,2)))
#' @export
is_twomode <- function(.data) UseMethod("is_twomode")

#' @export
is_twomode.igraph <- function(.data) {
  igraph::is_bipartite(.data)
}

#' @export
is_twomode.tbl_graph <- function(.data) {
  igraph::is_bipartite(.data)
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
  network::is.bipartite(.data)
  # .data <- as_matrix(.data)
  # dim(.data)[1] != dim(.data)[2]
}

#' @export
is_twomode.data.frame <- function(.data) {
  is_edgelist(.data) && 
    length(intersect(.data[,1], .data[,2])) == 0
}

#' @export
is_twomode.numeric <- function(.data) {
  return(FALSE)
}

#' @rdname is_format
#' @importFrom igraph is_weighted
#' @examples
#' is_weighted(create_tree(3))
#' @export
is_weighted <- function(.data) UseMethod("is_weighted")

#' @export
is_weighted.igraph <- function(.data) {
  igraph::is_weighted(.data)
}

#' @export
is_weighted.tbl_graph <- function(.data) {
  igraph::is_weighted(.data)
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

#' @rdname is_format
#' @importFrom igraph is_directed
#' @examples
#' is_directed(create_tree(2))
#' is_directed(create_tree(2, directed = TRUE))
#' @export
is_directed <- function(.data) UseMethod("is_directed")

#' @export
is_directed.data.frame <- function(.data) {
  !(infer_network_reciprocity(.data) == 0 |
      infer_network_reciprocity(.data) == 1)
}

#' @export
is_directed.igraph <- function(.data) {
  if(is_twomode(.data)) FALSE else igraph::is_directed(.data)
}

#' @export
is_directed.tbl_graph <- function(.data) {
  if(is_twomode(.data)) FALSE else igraph::is_directed(.data)
}

#' @export
is_directed.network <- function(.data) {
  .data$gal$directed
}

#' @export
is_directed.matrix <- function(.data) {
  if(is_twomode(.data)) FALSE else !isSymmetric(.data)
}

#' @rdname is_format
#' @importFrom igraph is_named
#' @examples
#' is_labelled(create_empty(3))
#' @export
is_labelled <- function(.data) UseMethod("is_labelled")

#' @export
is_labelled.igraph <- function(.data) {
  igraph::is_named(.data)
}

#' @export
is_labelled.tbl_graph <- function(.data) {
  igraph::is_named(.data)
}

#' @export
is_labelled.matrix <- function(.data) {
  any(c(!is.null(dimnames(.data)[[1]]), !is.null(dimnames(.data)[[2]])))
}

#' @export
is_labelled.network <- function(.data) {
  !all(is.na(network::get.vertex.attribute(.data, "vertex.names")))
}

#' @export
is_labelled.data.frame <- function(.data) {
  is.character(.data[,1]) & is.character(.data[,2])
}

#' @rdname is_format
#' @importFrom igraph edge_attr_names
#' @examples
#' is_signed(create_lattice(3))
#' @export
is_signed <- function(.data) UseMethod("is_signed")

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  
  abs(x - round(x)) < tol

#' @export
is_signed.data.frame <- function(.data) {
  if(ncol(.data) <= 2) FALSE else 
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

#' @rdname is_format
#' @importFrom igraph any_loop
#' @examples
#' is_complex(create_lattice(4))
#' @export
is_complex <- function(.data) UseMethod("is_complex")

#' @export
is_complex.igraph <- function(.data) {
  igraph::any_loop(.data)
}

#' @export
is_complex.tbl_graph <- function(.data) {
  igraph::any_loop(.data)
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

#' @rdname is_format 
#' @importFrom igraph any_multiple
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
    length(igraph::edge_attr_names(as_igraph(.data))) > 1 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.igraph <- function(.data) {
  igraph::any_multiple(.data) |
    length(igraph::edge_attr_names(.data)) > 1 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.network <- function(.data) {
  network::is.multiplex(.data)
}

#' @export
is_multiplex.data.frame <- function(.data) {
  ncol(.data) > 3
}

#' @rdname is_format
#' @importFrom igraph is_simple
#' @examples
#' is_uniplex(create_star(3))
#' @export
is_uniplex <- function(.data) {
  obj <- as_igraph(.data)
  igraph::is_simple(obj)
}

# Features ####

#' Marking networks features
#' 
#' @description
#'   These functions implement logical tests for various network
#'   features.
#'   
#'   - `is_connected()` tests whether network is strongly connected, 
#'   or weakly connected if undirected.
#'   - `is_perfect_matching()` tests whether there is a matching 
#'   for a network that covers every node in the network.
#'   - `is_eulerian()` tests whether there is a Eulerian path for a network
#'   where that path passes through every tie exactly once.
#'   - `is_acyclic()` tests whether network is a directed acyclic graph.
#'   - `is_aperiodic()` tests whether network is aperiodic.
#' @param .data An object of a `{manynet}`-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marking
#' @name features
NULL

#' @rdname features
#' @section is_connected: 
#'   To test weak connection on a directed network,
#'   please see `to_undirected()`.
#' @importFrom igraph is_connected
#' @examples
#' is_connected(ison_southern_women)
#' @export
is_connected <- function(.data) {
  igraph::is_connected(as_igraph(.data), 
                       mode = ifelse(is_directed(.data),
                                     "strong", "weak"))
}

#' @rdname features
#' @section is_perfect_matching: 
#'   For two-mode or bipartite networks, `to_matching()` is used
#'   to identify whether a perfect matching is possible.
#'   For one-mode networks, we use the Tutte theorem.
#'   Note that currently only subgraphs with cutpoints removed are tested,
#'   and not all possible subgraphs.
#'   This is to avoid computationally expensive combinatorial operations,
#'   but may come at the cost of some edge cases where a one-mode network
#'   cannot perfectly match as suggested.
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @references
#'   Tutte, W. T. (1950). 
#'   "The factorization of locally finite graphs". 
#'   _Canadian Journal of Mathematics_. 2: 44–49. 
#'   \doi{10.4153/cjm-1950-005-2}
#' @examples
#' is_perfect_matching(ison_southern_women)
#' @export
is_perfect_matching <- function(.data, mark = "type"){
  .data <- as_igraph(.data)
  if(mark %in% network_node_attributes(.data)){
    matches <- to_matching(.data, mark = mark)
    network_ties(matches)*2 == network_nodes(matches)
  } else {
    if (network_nodes(.data) %% 2 != 0) FALSE else # odd number of nodes cannot match perfectly
      if (!igraph::is_connected(.data) && # any odd components cannot match perfectly
          any(igraph::component_distribution(.data)[c(F,T)]!=0)) FALSE else { # note first index is 0...
            cutpoints <- igraph::articulation_points(.data)
            gminusu <- igraph::delete_vertices(.data, cutpoints)
            sum((igraph::component_distribution(gminusu) * igraph::count_components(gminusu))[c(F,T)]) <= length(cutpoints)
          }
  }
}

#' @rdname features
#' @importFrom igraph has_eulerian_path
#' @examples
#' is_eulerian(ison_brandes)
#' @export
is_eulerian <- function(.data){
  igraph::has_eulerian_path(as_igraph(.data))
}

#' @rdname features
#' @importFrom igraph is_dag
#' @examples 
#' is_acyclic(ison_algebra)
#' @export
is_acyclic <- function(.data){
  obj <- as_igraph(.data)
  igraph::is_dag(obj)
}

#' @rdname features
#' @param max_path_length Maximum path length considered.
#'   If negative, paths of all lengths are considered.
#'   By default 4, to avoid potentially very long computation times.
#' @source https://stackoverflow.com/questions/55091438/r-igraph-find-all-cycles
#' @examples 
#' is_aperiodic(ison_algebra)
#' @export
is_aperiodic <- function(.data, max_path_length = 4){
  g <- as_igraph(.data)
  out <- NULL
  for(v1 in igraph::V(g)) {
    if(igraph::degree(g, v1, mode="in") == 0) {next}
    goodNeighbors <- igraph::neighbors(g, v1, mode="out")
    goodNeighbors <- goodNeighbors[goodNeighbors > v1]
    out <- c(out, unlist(lapply(goodNeighbors, function(v2){
      vapply(igraph::all_simple_paths(g, v2, v1, mode="out", 
                                      cutoff = max_path_length), length, FUN.VALUE = numeric(1))
    })))
  }
  thisRequires("minMSE")
  minMSE::vector_gcd(out)==1
}

# Helper functions
infer_network_reciprocity <- function(.data, method = "default") {
  out <- igraph::reciprocity(as_igraph(.data), mode = method)
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- infer_dims(.data)
  out
}
