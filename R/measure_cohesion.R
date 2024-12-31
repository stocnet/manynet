#' Measures of network cohesion or connectedness
#' 
#' @description
#'   These functions return values or vectors relating to how connected a network is
#'   and the number of nodes or edges to remove that would increase fragmentation.
#'   
#'   - `net_density()` measures the ratio of ties to the number
#'   of possible ties.
#'   - `net_components()` measures the number of (strong) components 
#'   in the network.
#'   - `net_cohesion()` measures the minimum number of nodes to remove
#'   from the network needed to increase the number of components.
#'   - `net_adhesion()` measures the minimum number of ties to remove
#'   from the network needed to increase the number of components.
#'   - `net_diameter()` measures the maximum path length in the network.
#'   - `net_length()` measures the average path length in the network.
#'   - `net_independence()` measures the independence number, 
#'   or size of the largest independent set in the network.
#'   - `net_strength()` measures the number of ties that would need to be
#'   removed from a network to increase its number of components.
#'   - `net_toughness()` measures the number of nodes that would need to be
#'   removed from a network to increase its number of components.
#'   
#' @inheritParams mark_is
#' @name measure_cohesion
#' @family measures
NULL

#' @rdname measure_cohesion
#' @importFrom igraph edge_density
#' @examples 
#' net_density(ison_adolescents)
#' net_density(ison_southern_women)
#' @export
net_density <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    out <- sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    out <- igraph::edge_density(manynet::as_igraph(.data))
  }
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion
#' @section Cohesion: 
#'   To get the 'weak' components of a directed graph, 
#'   please use `manynet::to_undirected()` first.
#' @importFrom igraph components
#' @examples
#'   net_components(fict_thrones)
#'   net_components(to_undirected(fict_thrones))
#' @export
net_components <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::components(object, mode = "strong")$no,
                       object, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph cohesion
#' @references
#' ## On cohesion
#' White, Douglas R and Frank Harary. 2001. 
#' "The Cohesiveness of Blocks In Social Networks: Node Connectivity and Conditional Density." 
#' _Sociological Methodology_ 31(1): 305-59.
#' \doi{10.1111/0081-1750.00098}
#' @examples 
#' net_cohesion(ison_marvel_relationships)
#' net_cohesion(to_giant(ison_marvel_relationships))
#' @export
net_cohesion <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::cohesion(manynet::as_igraph(.data)), 
                       .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph adhesion
#' @examples 
#' net_adhesion(ison_marvel_relationships)
#' net_adhesion(to_giant(ison_marvel_relationships))
#' @export
net_adhesion <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::adhesion(manynet::as_igraph(.data)), 
                       .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph diameter
#' @examples 
#' net_diameter(ison_marvel_relationships)
#' net_diameter(to_giant(ison_marvel_relationships))
#' @export
net_diameter <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::diameter(object, 
                                        directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph mean_distance
#' @examples 
#' net_length(ison_marvel_relationships)
#' net_length(to_giant(ison_marvel_relationships))
#' @export
net_length <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::mean_distance(object,
                                             directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph ivs_size
#' @examples 
#' net_independence(ison_adolescents)
#' @export
net_independence <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(manynet::is_twomode(.data)){
    out <- igraph::ivs_size(manynet::to_mode1(manynet::as_igraph(.data)))
  } else {
    out <- igraph::ivs_size(manynet::to_undirected(manynet::as_igraph(.data)))
  }
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @examples 
#' net_strength(ison_adolescents)
#' @export
net_strength <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  n <- net_ties(.data)
  seties <- unlist(lapply(1:n, utils::combn, x = 1:n, simplify = FALSE), recursive = FALSE)
  out <- vapply(seties, function(x) length(x)/net_components(delete_ties(.data, x)), 
                FUN.VALUE = numeric(1))
  make_network_measure(min(out), .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @examples 
#' net_toughness(ison_adolescents)
#' @export
net_toughness <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  n <- net_nodes(.data)
  seties <- unlist(lapply(1:n, utils::combn, x = 1:n, simplify = FALSE), recursive = FALSE)
  out <- vapply(seties, function(x) length(x)/net_components(delete_nodes(.data, x)), 
                FUN.VALUE = numeric(1))
  make_network_measure(min(out), .data, call = deparse(sys.call()))
}

