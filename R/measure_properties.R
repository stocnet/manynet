#' Describing network properties
#' 
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_nodes()` returns the total number of nodes (of any mode) in a network.
#'   - `net_ties()` returns the number of ties in a network.
#'   - `net_dims()` returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#'   - `net_node_attributes()` returns a vector of nodal attributes in a network.
#'   - `net_tie_attributes()` returns a vector of tie attributes in a network.
#'   
#'   These functions are also often used as helpers within other functions.
#' @return `net_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `net_dims()` returns an integer of the number of nodes in a one-mode network,
#'   or two integers representing the number of nodes in each nodeset 
#'   in the case of a two-mode network.
#'   `net_*_attributes()` returns a string vector with the names
#'   of all node or tie attributes in the network.
#' @name measure_properties
#' @family measures
#' @inheritParams mark_is
NULL

#' @rdname measure_properties
#' @examples
#' net_nodes(ison_southern_women)
#' @export
net_nodes <- function(.data){
  if(is_list(.data)){
    nodes <- vapply(.data, function(x) igraph::vcount(as_igraph(x)), 
           FUN.VALUE = numeric(1))
    make_network_measure(max(nodes), .data[[1]], call = deparse(sys.call()))
  } else make_network_measure(igraph::vcount(as_igraph(.data)), .data, 
                              call = deparse(sys.call()))
}

#' @rdname measure_properties
#' @examples
#' net_ties(ison_southern_women)
#' @export
net_ties <- function(.data){
  make_network_measure(igraph::ecount(as_igraph(.data)), .data,
                       call = deparse(sys.call()))
}

#' @rdname measure_properties
#' @examples
#' net_dims(ison_southern_women)
#' net_dims(to_mode1(ison_southern_women))
#' @export
net_dims <- function(.data) UseMethod("net_dims")

#' @export
net_dims.data.frame <- function(.data){
  if(is_twomode(.data)){
    c(length(unique(.data[,1])),
      length(unique(.data[,2])))
  } else {
    length(unique(c(.data[,1], .data[,2])))
  }
}

#' @export
net_dims.matrix <- function(.data){
  if(is_twomode(.data)){
    c(nrow(.data),
      ncol(.data))
  } else {
    nrow(.data)
  }
}

#' @export
net_dims.igraph <- function(.data){
  if(is_twomode(.data)){
    c(sum(!igraph::V(.data)$type),
      sum(igraph::V(.data)$type))
  } else {
    igraph::vcount(.data)
  }
}

#' @importFrom network network.size get.network.attribute
#' @export
net_dims.network <- function(.data){
  out <- network::network.size(.data)
  if(is_twomode(.data)){
    bip1 <- network::get.network.attribute(as_network(.data), 
                                         "bipartite")
    out <- c(bip1, out - bip1)
  }
  out
}

#' @rdname measure_properties
#' @importFrom igraph vertex_attr_names
#' @examples
#'   net_node_attributes(fict_lotr)
#' @export
net_node_attributes <- function(.data){
  igraph::vertex_attr_names(as_igraph(.data))
}

#' @rdname measure_properties
#' @importFrom igraph edge_attr_names
#' @examples
#'   net_tie_attributes(ison_algebra)
#' @export
net_tie_attributes <- function(.data){
  igraph::edge_attr_names(as_igraph(.data))
}
