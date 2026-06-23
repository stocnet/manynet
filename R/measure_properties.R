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
net_nodes.default <- function(.data){
  net_nodes(as_igraph(.data))
}

#' @export
net_nodes.stocnet <- function(.data){
  dplyr::coalesce(nrow(.data$nodes),
                  length(unique(c(.data$ties$from, .data$ties$to))))
}

#' @export
net_nodes.matrix <- function(.data){
  if(is_twomode(.data)){
    sum(dim(.data))
  } else nrow(.data)
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
net_nodes.network <- function(.data){
  network::network.size(.data)
}

#' @export
net_nodes.data.frame <- function(.data){
  length(unique(c(.data$from, .data$to)))
}

#' @rdname measure_dims
#' @examples
#' net_modes(ison_southern_women)
#' @export
net_modes <- function(.data) UseMethod("net_modes")

#' @export
net_modes.stocnet <- function(.data){
  if("mode" %in% names(.data$nodes)){
    length(unique(.data$nodes$mode))
  } else 1L
}

#' @export
net_modes.igraph <- function(.data){
  if(is_twomode(.data)) 2L else 1L
}

#' @rdname measure_dims
#' @examples
#' net_ties(ison_southern_women)
#' @export
net_ties <- function(.data) UseMethod("net_ties")

#' @export
net_ties.default <- function(.data){
  net_ties(as_igraph(.data))
}

#' @export
net_ties.stocnet <- function(.data){
  nrow(.data$ties)
}

#' @export
net_ties.igraph <- function(.data){
  make_network_measure(igraph::ecount(.data), .data,
                       call = deparse(sys.call()))
}

#' @rdname measure_dims
#' @examples
#' net_layers(ison_southern_women)
#' @export
net_layers <- function(.data) UseMethod("net_layers")

#' @export
net_layers.default <- function(.data){
  net_layers(as_igraph(.data))
}

#' @export
net_layers.stocnet <- function(.data){
  if("layer" %in% names(.data$ties)){
    length(unique(.data$ties$layer))
  } else 1L
}

#' @export
net_layers.igraph <- function(.data){
  if("type" %in% net_tie_attributes(.data)){
    length(unique(tie_attribute(.data, "type")))
  } else 1L
}

#' @rdname measure_dims
#' @examples
#' net_dims(ison_southern_women)
#' net_dims(to_mode1(ison_southern_women))
#' @export
net_dims <- function(.data) UseMethod("net_dims")

#' @export
net_dims.default <- function(.data){
  net_dims(as_igraph(.data))
}

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
net_dims.stocnet <- function(.data){
  if(is_twomode(.data)){
    out <- tabulate(match(.data$nodes$mode, unique(.data$nodes$mode)))
  } else net_nodes(.data)
}

# Names ####

#' Describing network names
#' @name member_names
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_name()` returns the name of the network, if it has one.
#'   - `mode_names()` returns a vector of the names of the modes in a network,
#'   if they have been defined.
#'   - `net_node_attributes()` returns a vector of nodal attributes in a network.
#'   - `layer_names()` returns a vector of the names of the layers in a network,
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
net_name.default <- function(.data, prefix = NULL){
  net_name(as_igraph(.data), prefix = prefix)
}

#' @export
net_name.stocnet <- function(.data, prefix = NULL){
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
#'   mode_names(ison_algebra)
#' @export
mode_names <- function(.data) UseMethod("mode_names")

#' @export
mode_names.default <- function(.data){
  mode_names(as_igraph(.data))
}

#' @export
mode_names.igraph <- function(.data){
  igraph::graph_attr(.data, "nodes") %||%
    c(igraph::graph_attr(.data, "grand")$vertex1,
      igraph::graph_attr(.data, "grand")$vertex2)
}

#' @export
mode_names.stocnet <- function(.data){
  .data$info$modes
}

#' @rdname member_names
#' @importFrom igraph vertex_attr_names
#' @examples
#'   net_node_attributes(fict_lotr)
#' @export
net_node_attributes <- function(.data) UseMethod("net_node_attributes")

#' @export
net_node_attributes.default <- function(.data){
  net_node_attributes(as_igraph(.data))
}

#' @export
net_node_attributes.igraph <- function(.data){
  igraph::vertex_attr_names(.data)
}

#' @export
net_node_attributes.stocnet <- function(.data){
  names(.data$nodes)
}

#' @export
net_node_attributes.network <- function(.data){
  network::list.vertex.attributes(.data)
}

#' @rdname member_names
#' @importFrom igraph graph_attr
#' @examples
#'   layer_names(ison_algebra)
#' @export
layer_names <- function(.data) UseMethod("layer_names")

#' @export
layer_names.default <- function(.data){
  layer_names(as_igraph(.data))
}

#' @export
layer_names.igraph <- function(.data){
  igraph::graph_attr(.data, "ties") %||%
    c(igraph::graph_attr(.data, "grand")$edge.pos,
      igraph::graph_attr(.data, "grand")$edge.neg)
}

#' @export
layer_names.stocnet <- function(.data){
  .data$info$layers %||% unique(.data$ties$layer)
}

#' @rdname member_names
#' @importFrom igraph edge_attr_names
#' @examples
#'   net_tie_attributes(ison_algebra)
#' @export
net_tie_attributes <- function(.data) UseMethod("net_tie_attributes")

#' @export
net_tie_attributes.default <- function(.data){
  net_tie_attributes(as_igraph(.data))
}

#' @export
net_tie_attributes.igraph <- function(.data){
  igraph::edge_attr_names(.data)
}

#' @export
net_tie_attributes.stocnet <- function(.data){
  names(.data$ties)
}

#' @export
net_tie_attributes.network <- function(.data){
  network::list.edge.attributes(.data)
}

