#' Measures of network cohesion or connectedness
#' 
#' @description
#'   These functions return values or vectors relating to how connected a network is
#'   and the number of nodes or edges to remove that would increase fragmentation.
#'   
#'   - `network_density()` measures the ratio of ties to the number
#'   of possible ties.
#'   - `network_components()` measures the number of (strong) components 
#'   in the network.
#'   - `network_cohesion()` measures the minimum number of nodes to remove
#'   from the network needed to increase the number of components.
#'   - `network_adhesion()` measures the minimum number of ties to remove
#'   from the network needed to increase the number of components.
#'   - `network_diameter()` measures the maximum path length in the network.
#'   - `network_length()` measures the average path length in the network.
#'   - `network_independence()` measures the independence number, 
#'   or size of the largest independent set in the network.
#'   
#' @inheritParams is
#' @name measure_cohesion
#' @family measures
NULL

#' @rdname measure_cohesion
#' @importFrom igraph edge_density
#' @examples 
#' network_density(ison_adolescents)
#' network_density(ison_southern_women)
#' @export
network_density <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    out <- sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    out <- igraph::edge_density(manynet::as_igraph(.data))
  }
  make_network_measure(out, .data)
}

#' @rdname measure_cohesion
#' @section Cohesion: 
#'   To get the 'weak' components of a directed graph, 
#'   please use `manynet::to_undirected()` first.
#' @importFrom igraph components
#' @examples
#'   network_components(ison_friends)
#'   network_components(to_undirected(ison_friends))
#' @export
network_components <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::components(object, mode = "strong")$no,
                       object)
}

#' @rdname measure_cohesion 
#' @importFrom igraph cohesion
#' @references
#' White, Douglas R and Frank Harary. 2001. 
#' "The Cohesiveness of Blocks In Social Networks: Node Connectivity and Conditional Density." 
#' _Sociological Methodology_ 31(1): 305-59.
#' @examples 
#' network_cohesion(ison_marvel_relationships)
#' network_cohesion(to_giant(ison_marvel_relationships))
#' @export
network_cohesion <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::cohesion(manynet::as_igraph(.data)), .data)
}

#' @rdname measure_cohesion 
#' @importFrom igraph adhesion
#' @examples 
#' network_adhesion(ison_marvel_relationships)
#' network_adhesion(to_giant(ison_marvel_relationships))
#' @export
network_adhesion <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::adhesion(manynet::as_igraph(.data)), .data)
}

#' @rdname measure_cohesion 
#' @importFrom igraph diameter
#' @examples 
#' network_diameter(ison_marvel_relationships)
#' network_diameter(to_giant(ison_marvel_relationships))
#' @export
network_diameter <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::diameter(object, 
                                        directed = manynet::is_directed(object)),
                       object)
}

#' @rdname measure_cohesion 
#' @importFrom igraph mean_distance
#' @examples 
#' network_length(ison_marvel_relationships)
#' network_length(to_giant(ison_marvel_relationships))
#' @export
network_length <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::mean_distance(object,
                                             directed = manynet::is_directed(object)),
                       object)
}

#' @rdname measure_cohesion 
#' @importFrom igraph ivs_size
#' @examples 
#' network_independence(ison_adolescents)
#' @export
network_independence <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(manynet::is_twomode(.data)){
    out <- igraph::ivs_size(manynet::to_mode1(manynet::as_igraph(.data)))
  } else {
    out <- igraph::ivs_size(manynet::to_undirected(manynet::as_igraph(.data)))
  }
  make_network_measure(out, .data)
}
