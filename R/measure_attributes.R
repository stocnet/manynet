#' Describing attributes of nodes or ties in a network
#' 
#' @description 
#'   These functions extract certain attributes from network data:
#'   
#'   - `node_attribute()` returns an attribute's values for the nodes in a network.
#'   - `node_names()` returns the names of the nodes in a network.
#'   - `node_is_mode()` returns the mode of the nodes in a network.
#'   - `tie_attribute()` returns an attribute's values for the ties in a network.
#'   - `tie_weights()` returns the weights of the ties in a network.
#'   - `tie_signs()` returns the signs of the ties in a network.
#'   
#'   These functions are also often used as helpers within other functions.
#'   `node_*()` and `tie_*()` always return vectors the same length
#'   as the number of nodes or ties in the network, respectively.
#' @name measure_attributes
#' @family measures
#' @inheritParams mark_is
#' @param attribute Character string naming an attribute in the object.
NULL

#' @rdname measure_attributes
#' @examples
#' node_attribute(ison_lotr, "Race")
#' @export
node_attribute <- function(.data, attribute){
  out <- igraph::vertex_attr(as_igraph(.data), attribute)
  if(is.numeric(out)) make_node_measure(out, .data) else out
}

#' @rdname measure_attributes
#' @examples 
#' node_names(ison_southern_women)
#' @export
node_names <- function(.data){
  if(is_labelled(.data)){
    igraph::vertex_attr(as_igraph(.data), "name")  
  } else {
    indices <- seq.int(net_nodes(.data))
    paste0("N", gsub("\\s", "0", format(indices, width=max(nchar(indices)))))
  }
}

#' @rdname measure_attributes
#' @examples 
#' node_is_mode(ison_southern_women)
#' @export
node_is_mode <- function(.data){
  if(is_twomode(.data)){
    out <- igraph::vertex_attr(as_igraph(.data), "type")
  } else{
    out <- rep(FALSE, net_nodes(.data))
  }
  # cannot use make_node_mark here because then eternal loop
  class(out) <- c("node_mark", class(out))
  if(is.null(names(out)) & is_labelled(.data))
    names(out) <- node_names(.data)
  attr(out, "mode") <- out
  out
}

#' @rdname measure_attributes
#' @examples
#' tie_attribute(ison_algebra, "task_tie")
#' @export
tie_attribute <- function(.data, attribute){
  out <- igraph::edge_attr(as_igraph(.data), attribute)
  if(is.numeric(out)) make_tie_measure(out, .data) else out
}

#' @rdname measure_attributes
#' @examples
#' tie_weights(to_mode1(ison_southern_women))
#' @export
tie_weights <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::edge_attr(.data, "weight")
  if(is.null(out)) out <- rep(1, net_ties(.data))
  make_tie_measure(out, .data)
}

#' @rdname measure_attributes
#' @examples 
#' tie_signs(ison_marvel_relationships)
#' @export
tie_signs <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::edge_attr(.data, "sign")
  if(is.null(out)) out <- rep(1, net_ties(.data))
  make_tie_measure(out, .data)
}

