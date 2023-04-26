#' @importFrom igraph is.igraph
#' @export
igraph::is.igraph

#' @importFrom igraph is_bipartite
#' @export
igraph::is_bipartite

#' @importFrom network is.network
#' @export
network::is.network

#' @importFrom network as.network
#' @export
network::as.network

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

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

#' @importFrom tidygraph mutate
#' @export
tidygraph::mutate

#' @importFrom tidygraph rename
#' @export
tidygraph::rename

#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' @importFrom tidygraph bind_edges
#' @export
tidygraph::bind_edges

# expect_nodes <- function() {
#   if (!.graph_context$free() && .graph_context$active() != "nodes") {
#     stop("This call requires nodes to be active", call. = FALSE)
#   }
# }
# 
# expect_edges <- function() {
#   if (!.graph_context$free() && .graph_context$active() != "edges") {
#     stop("This call requires edges to be active", call. = FALSE)
#   }
# }

#' Custom print method for "tbl_graph" class
#'
#' @param x An object of class "tbl_graph".
#' @param ... Other arguments passed to or from other methods.
#' @export print.tbl_graph
#' @export
print.tbl_graph <- function(x, ...) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  graph_desc <- describe_graph(x)
  not_active <- if (tidygraph::active(x) == 'nodes') 'edges' else 'nodes'
  top <- suppressWarnings(do.call(tibble::trunc_mat,
                                  utils::modifyList(arg_list,
                                                    list(x = tidygraph::as_tibble(x),
                                                         n = 6))))
  top$summary[1] <- paste0(top$summary[1], ' (active)')
  names(top$summary)[1] <- tools::toTitleCase(paste0(substr(tidygraph::active(x),
                                                            1, 4), ' data'))
  bottom <- suppressWarnings(do.call(tibble::trunc_mat,
                                     utils::modifyList(arg_list,
                                                       list(x = tidygraph::as_tibble(
                                                         x, active = not_active),
                                                         n = 3))))
  names(bottom$summary)[1] <- tools::toTitleCase(paste0(substr(not_active, 1, 4),
                                                        ' data'))
  cat_subtle('# A tbl_graph: ', igraph::gorder(x), ' nodes and ',
             igraph::gsize(x), ' edges\n', sep = '')
  cat_subtle('#\n')
  cat_subtle('# ', graph_desc, '\n', sep = '')
  cat_subtle('#\n')
  print(top)
  cat_subtle('#\n')
  print(bottom)
  invisible(x)
}

describe_graph <- function(x) {
  if (igraph::gorder(x) == 0) {
    return('An empty graph')
  } else if (is_directed(x)) {
    if (is_twomode(x)) {
      return('A two mode directed graph') 
    } else {
      return('A directed graph')
    }
  } else {
    if (!is_directed(x) & is_twomode(x)) {
      return('A two mode undirected graph')
    } else {
      return('An undirected graph')
    }
  }
}

cat_subtle <- function(...) cat(pillar::style_subtle(paste0(...)))
