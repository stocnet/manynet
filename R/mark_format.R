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
#'   - `is_attributed()` marks networks TRUE if there are other nodal attributes
#'   than 'names' or 'type'.
#'   - `is_signed()` marks networks TRUE if the ties can be either positive
#'   or negative.
#'   - `is_complex()` marks networks TRUE if any ties are loops,
#'   with the sender and receiver being the same node.
#'   - `is_multiplex()` marks networks TRUE if it contains multiple types 
#'   of ties, such that there can be multiple ties between the same
#'   sender and receiver.
#'   - `is_uniplex()` marks networks TRUE if it is neither complex nor multiplex.
#' @inheritParams mark_is
#' @family marking
#' @name mark_format
NULL

#' @rdname mark_format
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
is_twomode.snet <- function(.data) {
  if(is.null(.data$nodes)) return(FALSE) else if (!"mode" %in% names(.data$nodes)) 
    return(FALSE) else length(unique(.data$nodes$mode)) == 2
}

#' @export
is_twomode.numeric <- function(.data) {
  return(FALSE)
}

#' @export
is_twomode.list <- function(.data) {
  if(is_list(.data)){
    is_twomode(.data[[1]])
  }
}

#' @rdname mark_format
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

#' @rdname mark_format
#' @importFrom igraph is_directed
#' @examples
#' is_directed(create_tree(2))
#' is_directed(create_tree(2, directed = TRUE))
#' @export
is_directed <- function(.data) UseMethod("is_directed")

#' @export
is_directed.data.frame <- function(.data) {
  !(.infer_net_reciprocity(.data) == 0 |
      .infer_net_reciprocity(.data) == 1)
}

#' @export
is_directed.igraph <- function(.data) {
  if(is_twomode(.data)) FALSE else igraph::is_directed(.data)
}

#' @export
is_directed.snet <- function(.data) {
  if(is_twomode(.data)) FALSE else any(.data$info$directed)
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

#' @rdname mark_format
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

#' @export
is_labelled.list <- function(.data) {
  if(is_list(.data)){
    is_labelled(.data[[1]])
  }
}

#' @rdname mark_format
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

#' @rdname mark_format
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

#' @export
is_complex.list <- function(.data) {
  if(is_list(.data)){
    is_complex(.data[[1]])
  }
}

#' @rdname mark_format 
#' @importFrom igraph any_multiple
#' @examples
#' is_multiplex(create_filled(c(3,3)))
#' @export
is_multiplex <- function(.data) UseMethod("is_multiplex")

#' @export
is_multiplex.matrix <- function(.data) {
  FALSE
}

reserved_tie_attr <- c("wave","panel","sign","weight","date","begin","end",
                       "name","default","increment","time")

#' @export
is_multiplex.tbl_graph <- function(.data) {
  igraph::any_multiple(.data) & length(setdiff(reserved_tie_attr, net_tie_attributes(.data)))==0 |
    length(setdiff(net_tie_attributes(.data), reserved_tie_attr)) > 0 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.igraph <- function(.data) {
  igraph::any_multiple(.data) & length(setdiff(reserved_tie_attr, net_tie_attributes(.data)))==0 |
    length(setdiff(net_tie_attributes(.data), reserved_tie_attr)) > 0 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.network <- function(.data) {
  network::is.multiplex(.data)
}

#' @export
is_multiplex.data.frame <- function(.data) {
  ncol(.data) >= 3 & "type" %in% setdiff(colnames(.data), reserved_tie_attr)
}

#' @rdname mark_format
#' @importFrom igraph is_simple
#' @examples
#' is_uniplex(create_star(3))
#' @export
is_uniplex <- function(.data) {
  obj <- as_igraph(.data)
  igraph::is_simple(obj)
}

#' @rdname mark_format
#' @examples
#' is_attributed(ison_algebra)
#' @export
is_attributed <- function(.data) {
  length(setdiff(net_node_attributes(.data), c("type","name")))!=0
}

#' @rdname mark_format
#' @examples
#' is_longitudinal(create_tree(5, 3))
#' @export
is_longitudinal <- function(.data) {
  if(is_manynet(.data)) {
    ig <- as_igraph(.data)
    # catts <- names(igraph::graph_attr(ig, "changes"))
    tatts <- igraph::edge_attr_names(ig)
    return(#"time" %in% catts | 
      "wave" %in% tatts | "panel" %in% tatts)
  } else if(is_list(.data)){
    all(lapply(.data, net_nodes)==net_nodes(.data[[1]]))
  } 
}

#' @rdname mark_format
#' @examples 
#' is_dynamic(create_tree(3))
#' @export
is_dynamic <- function(.data) {
  atts <- igraph::edge_attr_names(as_igraph(.data))
  "time" %in% atts | "beg" %in% atts | "begin" %in% atts | "start" %in% atts
}

#' @rdname mark_format
#' @examples 
#' is_changing(fict_starwars)
#' @export
is_changing <- function(.data) {
  "changes" %in% igraph::graph_attr_names(as_igraph(.data))
}

#' @rdname mark_format
#' @examples 
#' is_egonet(fict_starwars)
#' @export
is_egonet <- function(.data) {
  if(!is_list(.data)) return(FALSE) else if (all(unique(names(.data)) != "")) {
    length(names(.data)) == length(unique(unlist(unname(lapply(.data, 
                                                               manynet::node_names))))) &
      all(.order_alphabetically(names(.data)) ==
            .order_alphabetically(unique(unlist(unname(lapply(.data, 
                                                              manynet::node_names))))))
  } else FALSE
}

# Helper functions ----
.infer_net_reciprocity <- function(.data, method = "default") {
  out <- igraph::reciprocity(as_igraph(.data), mode = method)
  class(out) <- c("net_measure", class(out))
  attr(out, "mode") <- infer_dims(.data)
  out
}

.order_alphabetically <- function(v) {
  v[order(names(stats::setNames(v, v)))]
}
