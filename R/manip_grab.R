#' Grab various node or edge attributes from a network
#' 
#' @description These functions operate to help extract certain attributes
#'   from given network data.
#'   They are also useful as helpers within other functions.
#'   
#'   `network_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `node_*()` and `tie_*()` always return vectors the same length
#'   as the number of nodes or edges in the network, respectively.
#' @name grab
#' @family manipulations
#' @inheritParams is
#' @param attribute Character string naming an attribute in the object.
NULL

#' @describeIn grab Extracts the names of the nodes in a network.
#' @examples 
#' node_names(ison_southern_women)
#' @export
node_names <- function(.data){
  igraph::get.vertex.attribute(as_igraph(.data), "name")
}

#' @describeIn grab Extracts the mode of the nodes in a network.
#' @examples 
#' node_mode(ison_southern_women)
#' @export
node_mode <- function(.data){
  if(is_twomode(.data)){
    out <- igraph::get.vertex.attribute(as_igraph(.data), "type")
  } else{
    out <- rep(FALSE, network_nodes(.data))
  }
  # cannot use make_node_mark here because then eternal loop
  class(out) <- c("node_mark", class(out))
  if(is.null(names(out)) & is_labelled(.data))
    names(out) <- node_names(.data)
  attr(out, "mode") <- out
  out
}

#' @describeIn grab Extracts an attribute's values for the nodes in a network.
#' @examples
#' node_attribute(ison_lotr, "Race")
#' @export
node_attribute <- function(.data, attribute){
  igraph::get.vertex.attribute(as_igraph(.data), attribute)
}

#' @describeIn grab Extracts an attribute's values for the edges in a network.
#' @examples
#' tie_attribute(ison_algebra, "task_tie")
#' @export
tie_attribute <- function(.data, attribute){
  igraph::get.edge.attribute(as_igraph(.data), attribute)
}

#' @describeIn grab Extracts the weights of the edges in a network.
#' @examples
#' tie_weights(to_mode1(ison_southern_women))
#' @export
tie_weights <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::get.edge.attribute(.data, "weight")
  if(is.null(out)) out <- rep(1, network_ties(.data))
  out
}

#' @describeIn grab Extracts the signs of the edges in a network.
#' @examples 
#' tie_signs(ison_marvel_relationships)
#' @export
tie_signs <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::get.edge.attribute(.data, "sign")
  if(is.null(out)) out <- rep(1, network_ties(.data))
  out
}

#' @describeIn grab Returns the total number of nodes (of any mode) in a network.
#' @examples
#' network_nodes(ison_southern_women)
#' @export
network_nodes <- function(.data){
  igraph::vcount(as_igraph(.data))
}

#' @describeIn grab Returns the number of edges in a network.
#' @examples
#' network_ties(ison_southern_women)
#' @export
network_ties <- function(.data){
  igraph::ecount(as_igraph(.data))
}

#' @describeIn grab Returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#' @examples
#' network_dims(ison_southern_women)
#' network_dims(to_mode1(ison_southern_women))
#' @export
network_dims <- function(.data){
  if(is_twomode(.data)){
    c(sum(!igraph::V(as_igraph(.data))$type),
      sum(igraph::V(as_igraph(.data))$type))
  } else {
    igraph::vcount(as_igraph(.data))
  }
}

#' @describeIn grab Returns a vector of nodal attributes in a network
#' @importFrom igraph list.vertex.attributes
#' @examples
#' network_node_attributes(ison_lotr)
#' @export
network_node_attributes <- function(.data){
  igraph::list.vertex.attributes(as_igraph(.data))
}

#' @describeIn grab Returns a vector of edge attributes in a network
#' @importFrom igraph edge_attr_names
#' @examples
#' network_tie_attributes(ison_algebra)
#' @export
network_tie_attributes <- function(.data){
  igraph::edge_attr_names(as_igraph(.data))
}
