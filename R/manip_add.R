#' Adding nodes and ties and their attributes
#' 
#' @description 
#'   These functions allow users to add nodes, ties, or attributes to the nodes or ties
#'   of a network.
#'   The `add_*()` functions operate similarly to in `{igraph}`.
#' @family manipulations
#' @inheritParams is
#' @param attribute A named list to be added as tie or node attributes.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @examples
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   add_nodes(other, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
#' @name add
NULL

#' @describeIn add Add additional ties to a network
#' @param nodes The number of nodes to be added.
#' @importFrom igraph add_edges
#' @export
add_nodes <- function(.data, nodes, attribute = NULL) UseMethod("add_nodes")

#' @export
add_nodes.igraph <- function(.data, nodes, attribute = NULL){
  igraph::add_vertices(.data, nv = nodes, attr = attribute)
}

#' @export
add_nodes.tbl_graph <- function(.data, nodes, attribute = NULL){
  as_tidygraph(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @export
add_nodes.network <- function(.data, nodes, attribute = NULL){
  as_network(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @describeIn add Add additional ties to a network
#' @param ties The number of ties to be added or an even list of ties.
#' @importFrom igraph add_edges
#' @export
add_ties <- function(.data, ties, attribute = NULL) UseMethod("add_ties")

#' @export
add_ties.igraph <- function(.data, ties, attribute = NULL){
  igraph::add_edges(.data, edges = ties, attr = attribute)
}

#' @export
add_ties.tbl_graph <- function(.data, ties, attribute = NULL){
  as_tidygraph(add_ties(as_igraph(.data), ties, attribute))
}

#' @export
add_ties.network <- function(.data, ties, attribute = NULL){
  as_network(add_ties(as_igraph(.data), ties, attribute))
}

#' @describeIn add Insert specified values from a vector into the graph 
#' as node attributes
#' @importFrom igraph vertex_attr
#' @export
add_node_attribute <- function(.data, attr_name, vector){
  if(length(vector)!=igraph::vcount(as_igraph(.data))){
    if(is_twomode(.data) && any(length(vector) == infer_dims(.data))){
      if(length(vector) == infer_dims(.data)[1]){
        vector <- c(vector, rep(NA, infer_dims(.data)[2]))
      } else if (length(vector) == infer_dims(.data)[2]){
        vector <- c(rep(NA, infer_dims(.data)[1]), vector)
      }
    } else 
      stop("Attribute vector must be same length as nodes in object.")
  }
  out <- as_igraph(.data)
  igraph::vertex_attr(out, name = attr_name) <- vector
  as_tidygraph(out)
}

#' @describeIn add Insert specified values from a vector into the network.
#'   as tie attributes
#' @importFrom igraph edge_attr
#' @export
add_tie_attribute <- function(.data, attr_name, vector){
  out <- as_igraph(.data)
  igraph::edge_attr(out, name = attr_name) <- vector
  as_tidygraph(out)
}
