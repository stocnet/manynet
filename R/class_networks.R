# #' Custom print method for "tbl_graph" class
# #'
# #' @param x An object of class "tbl_graph".
# #' @param ... Other arguments passed to or from other methods.
#' @export
print.mnet <- function(x, ..., n = 6) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  graph_desc <- describe_graph(x)
  top <- dplyr::as_tibble(tidygraph::activate(x, "nodes"))
  bottom <- dplyr::as_tibble(tidygraph::activate(x, "edges"))
  if(is.null(igraph::graph_attr(x, "grand"))) node_name <- "nodes" else
    node_name <- igraph::graph_attr(x, "grand")$vertex1
  if(is.null(igraph::graph_attr(x, "grand"))) 
    tie_name <- ifelse(is_directed(x), "arcs", "ties") else
      tie_name <- igraph::graph_attr(x, "grand")$edge.pos
  cat('#', graph_desc, 'network of', igraph::gorder(x), node_name, 'and',
      igraph::gsize(x), tie_name, '\n', sep = ' ')  
  if (ncol(top)>0) print(top, n = n)
  if (ncol(bottom)>0) print(bottom, n = n, max_footer_lines = 1)
  invisible(x)
}

describe_graph <- function(x) {
  paste0("A ",
         ifelse(is_dynamic(x), "dynamic, ", ""),
         ifelse(is_longitudinal(x), "longitudinal, ", ""),
         ifelse(is_labelled(x), "labelled, ", ""),
         ifelse(is_complex(x), "complex, ", ""),
         ifelse(is_multiplex(x), "multiplex, ", ""),
         ifelse(is_signed(x), "signed, ", ""),
         ifelse(is_weighted(x), "weighted, ", ""),
         ifelse(is_twomode(x), "two-mode", 
                ifelse(is_directed(x), "directed", "undirected"))
  )
}
