# Dimensions ####

#' Describing network dimensions
#' @name measure_dims
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_nodes()` returns the total number of nodes (of any mode) in a network.
#'   - `net_ties()` returns the number of ties in a network.
#'   - `net_dims()` returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#'   
#'   These functions are also often used as helpers within other functions.
#' @return `net_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `net_dims()` returns an integer of the number of nodes in a one-mode network,
#'   or two integers representing the number of nodes in each nodeset 
#'   in the case of a two-mode network.
#' @family measures
#' @template param_data
NULL

#' @rdname measure_dims
#' @examples
#' net_nodes(ison_southern_women)
#' @export
net_nodes <- function(.data) UseMethod("net_nodes")

#' @export
net_nodes.snet <- function(.data){
  nrow(.data$nodes)
}

#' @export
net_nodes.igraph <- function(.data){
  if(is_list(.data)){
    nodes <- vapply(.data, function(x) igraph::vcount(as_igraph(x)), 
                    FUN.VALUE = numeric(1))
    make_network_measure(max(nodes), .data[[1]], call = deparse(sys.call()))
  } else make_network_measure(igraph::vcount(as_igraph(.data)), .data, 
                              call = deparse(sys.call()))
}

#' @export
net_nodes.tbl <- function(.data){
  if(is_list(.data)){
    nodes <- vapply(.data, function(x) igraph::vcount(as_igraph(x)), 
                    FUN.VALUE = numeric(1))
    make_network_measure(max(nodes), .data[[1]], call = deparse(sys.call()))
  } else make_network_measure(igraph::vcount(as_igraph(.data)), .data, 
                              call = deparse(sys.call()))
}

#' @rdname measure_dims
#' @examples
#' net_ties(ison_southern_women)
#' @export
net_ties <- function(.data) UseMethod("net_ties")

#' @export
net_ties.snet <- function(.data){
  nrow(.data$ties)
}

#' @export
net_ties.igraph <- function(.data){
  make_network_measure(igraph::ecount(as_igraph(.data)), .data,
                       call = deparse(sys.call()))
}

#' @rdname measure_dims
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

#' @export
net_dims.snet <- function(.data){
  if(is_twomode(.data)){
    out <- tabulate(match(.data$nodes$mode, unique(.data$nodes$mode)))
  } else nrow(.data$nodes)
}

# Names ####

#' Describing network names
#' @name member_names
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_name()` returns the name of the network, if it has one.
#'   - `net_node_names()` returns a vector of the names of the nodes in a network,
#'   if they have been defined.
#'   - `net_node_attributes()` returns a vector of nodal attributes in a network.
#'   - `net_tie_names()` returns a vector of the names of the ties in a network,
#'   if they have been defined.
#'   - `net_tie_attributes()` returns a vector of tie attributes in a network.
#'   
#'   These functions are also often used as helpers within other functions.
#' @return `net_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `net_*_attributes()` returns a string vector with the names
#'   of all node or tie attributes in the network.
#' @family attributes
#' @template param_data
NULL

#' @rdname member_names
#' @param prefix An optional string to be added before the name of the network.
#' @examples
#' net_name(ison_southern_women)
#' @export
net_name <- function(.data, prefix = NULL) UseMethod("net_name")

#' @export
net_name.snet <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(.data$info$name)) {
    existname <- .data$info$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname  
}

#' @export
net_name.igraph <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(igraph::graph_attr(.data, "name"))) {
    existname <- igraph::graph_attr(.data, 'name')
  } else if(is_grand(.data) && 
            !is.null(igraph::graph_attr(.data, "grand")$name)){
    existname <- igraph::graph_attr(.data, 'grand')$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname
}

#' @export
net_name.network <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(igraph::graph_attr(.data, "name"))) {
    existname <- igraph::graph_attr(.data, 'name')
  } else if(is_grand(.data) && 
            !is.null(igraph::graph_attr(.data, "grand")$name)){
    existname <- igraph::graph_attr(.data, 'grand')$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname
}


#' @rdname member_names
#' @importFrom igraph graph_attr
#' @examples
#'   net_node_names(ison_algebra)
#' @export
net_node_names <- function(.data) UseMethod("net_node_names")

#' @export
net_node_names.igraph <- function(.data){
  igraph::graph_attr(.data, "nodes") %||%
    c(igraph::graph_attr(.data, "grand")$vertex1,
      igraph::graph_attr(.data, "grand")$vertex2)
}

#' @export
net_node_names.snet <- function(.data){
  .data$info$nodes
}

#' @rdname member_names
#' @importFrom igraph vertex_attr_names
#' @examples
#'   net_node_attributes(fict_lotr)
#' @export
net_node_attributes <- function(.data) UseMethod("net_node_attributes")

#' @export
net_node_attributes.igraph <- function(.data){
  igraph::vertex_attr_names(.data)
}

#' @export
net_node_attributes.snet <- function(.data){
  names(.data$nodes)
}

#' @rdname member_names
#' @importFrom igraph graph_attr
#' @examples
#'   net_tie_names(ison_algebra)
#' @export
net_tie_names <- function(.data) UseMethod("net_tie_names")

#' @export
net_tie_names.igraph <- function(.data){
  igraph::graph_attr(.data, "ties") %||%
    c(igraph::graph_attr(.data, "grand")$edge.pos,
      igraph::graph_attr(.data, "grand")$edge.neg)
}

#' @export
net_tie_names.snet <- function(.data){
  .data$info$ties
}

#' @rdname member_names
#' @importFrom igraph edge_attr_names
#' @examples
#'   net_tie_attributes(ison_algebra)
#' @export
net_tie_attributes <- function(.data) UseMethod("net_tie_attributes")

#' @export
net_tie_attributes.igraph <- function(.data){
  igraph::edge_attr_names(.data)
}

#' @export
net_tie_attributes.snet <- function(.data){
  names(.data$ties)
}

