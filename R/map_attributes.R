#' Describing attributes of nodes or ties in a network
#' 
#' @description These functions extract certain attributes from given network data.
#'   They are also useful as helpers within other functions.
#' @return `node_*()` and `tie_*()` always return vectors the same length
#'   as the number of nodes or ties in the network, respectively.
#' @name attributes
#' @family mapping
#' @inheritParams is
#' @param attribute Character string naming an attribute in the object.
NULL

#' @describeIn attributes Extracts the names of the nodes in a network.
#' @examples 
#' node_names(ison_southern_women)
#' @export
node_names <- function(.data){
  igraph::get.vertex.attribute(as_igraph(.data), "name")
}

#' @describeIn attributes Extracts the mode of the nodes in a network.
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

#' @describeIn attributes Extracts an attribute's values for the nodes in a network.
#' @examples
#' node_attribute(ison_lotr, "Race")
#' @export
node_attribute <- function(.data, attribute){
  igraph::get.vertex.attribute(as_igraph(.data), attribute)
}

#' @describeIn attributes Extracts an attribute's values for the edges in a network.
#' @examples
#' tie_attribute(ison_algebra, "task_tie")
#' @export
tie_attribute <- function(.data, attribute){
  igraph::get.edge.attribute(as_igraph(.data), attribute)
}

#' @describeIn attributes Extracts the weights of the edges in a network.
#' @examples
#' tie_weights(to_mode1(ison_southern_women))
#' @export
tie_weights <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::get.edge.attribute(.data, "weight")
  if(is.null(out)) out <- rep(1, network_ties(.data))
  out
}

#' @describeIn attributes Extracts the signs of the edges in a network.
#' @examples 
#' tie_signs(ison_marvel_relationships)
#' @export
tie_signs <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::get.edge.attribute(.data, "sign")
  if(is.null(out)) out <- rep(1, network_ties(.data))
  out
}

