# #' Custom print method for "tbl_graph" class
# #'
# #' @param x An object of class "tbl_graph".
# #' @param ... Other arguments passed to or from other methods.
#' @export
print.mnet <- function(x, ..., n = 6) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  if(is_grand(x) && !is.null(igraph::graph_attr(x, "grand")$name)) 
    cli::cli_text("# {igraph::graph_attr(x, 'grand')$name}")
  graph_desc <- describe_graph(x)
  tie_desc <- describe_ties(x)
  node_desc <- describe_nodes(x)
  cli::cli_text("# {graph_desc} network of {node_desc} and {tie_desc}")  
  top <- dplyr::as_tibble(tidygraph::activate(x, "nodes"))
  bottom <- dplyr::as_tibble(tidygraph::activate(x, "edges"))
  if (ncol(top)>0) print(top, n = n)
  if (ncol(bottom)>0) print(bottom, n = n, max_footer_lines = 1)
  invisible(x)
}

is_grand <- function(.data){
  !is.null(igraph::graph_attr(.data, "grand"))
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

describe_nodes <- function(x){
  nd <- net_dims(x)
  if(!is.null(igraph::graph_attr(x, "grand")$vertex1)){
    node_name <- paste(nd[1], igraph::graph_attr(x, "grand")$vertex1)
    if(length(nd)==2 && !is.null(igraph::graph_attr(x, "grand")$vertex2))
      node_name <- c(node_name, paste(nd[2], igraph::graph_attr(x, "grand")$vertex2))
  } else node_name <- paste(sum(nd), "nodes")
  node_name
}

describe_ties <- function(x){
  nt <- net_ties(x)
  tie_name <- ifelse(is_directed(x), "arcs", "ties") 
  if(!is.null(igraph::graph_attr(x, "grand")$edge.pos)){
    tie_name <- paste(igraph::graph_attr(x, "grand")$edge.pos,
                      tie_name)
  } else if(!is.null(tie_attribute(x, "type"))){
    tie_name <- paste(cli::ansi_collapse(unique(tie_attribute(x, "type"))), 
          tie_name)
  } 
  paste(nt, tie_name)
}
