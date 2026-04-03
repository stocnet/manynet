#' @importFrom igraph is_igraph
#' @export
igraph::is_igraph

#' @importFrom tidygraph with_graph
#' @export
tidygraph::with_graph

#' @importFrom tidygraph is.tbl_graph
#' @export
tidygraph::is.tbl_graph

#' @importFrom tidygraph .G
#' @export
tidygraph::.G

#' @importFrom tidygraph .N
#' @export
tidygraph::.N

#' @importFrom tidygraph .E
#' @export
tidygraph::.E

# nocov start

#' Expecting either nodes or ties to be active
#' @keywords internal
#' @name expect
NULL

#' @rdname expect
#' @export
expect_nodes <- function(.data) {
  if(missing(.data)){
    if (!tidygraph::.graph_context$free() && tidygraph::.graph_context$active() != "nodes") {
      snet_abort("This call requires nodes to be active", call. = FALSE)
    }
    tidygraph::.G()
  } else .data
}

#' @rdname expect
#' @export
expect_ties <- function(.data) {
  if(missing(.data)){
    if (!tidygraph::.graph_context$free() && tidygraph::.graph_context$active() != "edges") {
    snet_abort("This call requires ties to be active", call. = FALSE)
    }
    tidygraph::.G()
  } else .data
}

# nocov end