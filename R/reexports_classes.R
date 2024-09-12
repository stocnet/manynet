#' @importFrom igraph is_igraph
#' @export
igraph::is_igraph

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom tidygraph with_graph
#' @export
tidygraph::with_graph

#' @importFrom tidygraph is.tbl_graph
#' @export
is_tidygraph <- tidygraph::is.tbl_graph

#' @importFrom tidygraph .G
#' @export
tidygraph::.G

#' @importFrom tidygraph .N
#' @export
tidygraph::.N

#' @importFrom tidygraph .E
#' @export
tidygraph::.E

expect_nodes <- function() {
  if (!tidygraph::.graph_context$free() && tidygraph::.graph_context$active() != "nodes") {
    cli::cli_abort("This call requires nodes to be active", call. = FALSE)
  }
}

expect_edges <- function() {
  if (!tidygraph::.graph_context$free() && tidygraph::.graph_context$active() != "edges") {
    cli::cli_abort("This call requires ties to be active", call. = FALSE)
  }
}
