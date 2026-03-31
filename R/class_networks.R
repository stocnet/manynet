#' Multilevel, multiplex, multimodal, signed, dynamic or longitudinal changing networks
#' @description
#'   The 'mnet' class of network object is an additional class layered on top of
#'   the 'igraph' and 'tbl_graph' classes.
#'   Under the hood it is an 'igraph' object, which enables all the igraph
#'   functions to operate.
#'   It is also a 'tbl_graph' object, which enables it to be used with `{ggraph}`.
#'   However, 'mnet' objects offer prettier printing and a consistent structure 
#'   that enables more complex forms of networks to be contained in a single object.
#' @section Nodes:
#'   Nodes are held as vertices and vertex attributes in the 'igraph' object,
#'   but printed as a nodelist.
#'   Here the convention is for the first column of the nodelist to be called
#'   'name' and records the labels of the nodes.
#'   Additional reserved columns include 'active' for changing networks and
#'   'type' for multimodal networks.
#' @section Changes: 
#'   Changes, that is a list of changes to the nodes in the network,
#'   are held internally as a graph attribute in the 'igraph' object,
#'   but printed as a changelist.
#'   Here the convention is for the 'wave' or 'time' column to appear first,
#'   followed by 'node' indicating to which node the change applies,
#'   'var' for the variable to which the change applies,
#'   and 'value' for the new value to be applied.
#' @section Ties:
#'   Ties are held as edges and edge attributes in the 'igraph' object,
#'   but printed as an edgelist.
#'   Here the convention is for the first column of the edgelist to be called
#'   'from' and the second column 'to', even if the network is not directed.
#'   Additional reserved columns include 'weight' for weighted networks,
#'   'wave' for longitudinal networks, 'type' for multiplex networks,
#'   and 'sign' for signed networks.
#' @section Printing: 
#'   When printed, 'mnet' objects will print to the console any information
#'   stored about the network's name, or its types of nodes or ties.
#'   It will also describe key features of the network,
#'   such as whether the network is multiplex, weighted, directed, etc.
#'   
#'   It will then print tibbles for the nodes, changes, and ties in the network,
#'   as appropriate.
#'   That is, if there is no nodal data 
#'   (e.g. it is an unlabelled network without any other nodal attributes),
#'   then this will be skipped.
#'   Similarly, if no nodal changes are logged, this information will be skipped
#'   too.
#' @name make_mnet
NULL

#' @rdname make_mnet
#' @param x An object of class "mnet" or "tbl_graph".
#' @param ... Other arguments passed to or from other methods.
#' @param n Number of observations to print across all network components,
#'   i.e. nodes, changes, and ties.
#'   By default 12.
#' @export
print.mnet <- function(x, ..., n = 12) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  if(!is.null(net_name(x)))
    cli::cli_h1("# {net_name(x)}")
  net_desc <- describe_network(x)
  tie_desc <- describe_ties(x)
  node_desc <- describe_nodes(x)
  change_desc <- describe_changes(x)
  cli::cli_par()
  cli_div(theme = list(.emph = list(color = "#4576B5")))
  cli::cli_text("{.emph # {net_desc} of {node_desc} and {tie_desc}{change_desc}}")
  cli::cli_end()
  top <- dplyr::as_tibble(tidygraph::activate(x, "nodes"))
  bottom <- dplyr::as_tibble(tidygraph::activate(x, "edges"))
  if(!is.null(igraph::graph_attr(x, "changes"))) n <- ceiling(n/3) else 
    n <- ceiling(n/2)
  if (ncol(top)>0){
    cli::cli_par()
    cli::cli_h3("Nodes")
    print(top, n = n)
    cli::cli_end()
  } 
  if(!is.null(igraph::graph_attr(x, "changes"))){
    cli::cli_par()
    cli::cli_h3("Changes")
    print(dplyr::as_tibble(igraph::graph_attr(x, "changes")),
          n = n)
    cli::cli_end()
  }
  if (ncol(bottom)>0){
    # cli::cli_par()
    cli::cli_h3("Ties")
    print(bottom, n = n, max_footer_lines = 1)
    # cli::cli_end()
  } 
  invisible(x)
}

#' @rdname make_mnet
#' @export
print.snet <- function(x, ..., n = 12) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  if(!is.null(net_name(x)))
    cli::cli_h1("# {net_name(x)}")
  net_desc <- describe_network(x)
  tie_desc <- describe_ties(x)
  node_desc <- describe_nodes(x)
  change_desc <- describe_changes(x)
  cli::cli_par()
  cli_div(theme = list(.emph = list(color = "#4576B5")))
  cli::cli_text("{.emph # {net_desc} of {node_desc} and {tie_desc}{change_desc}}")
  cli::cli_end()
  top <- x$nodes
  bottom <- x$ties
  if(!is.null(x$changes)) n <- ceiling(n/3) else 
    n <- ceiling(n/2)
  if (ncol(top)>0){
    cli::cli_par()
    cli::cli_h3("Nodes")
    print(top, n = n)
    cli::cli_end()
  } 
  if(!is.null(x$changes)){
    cli::cli_par()
    cli::cli_h3("Changes")
    print(dplyr::as_tibble(x$changes),
          n = n)
    cli::cli_end()
  }
  if (ncol(bottom)>0){
    # cli::cli_par()
    cli::cli_h3("Ties")
    print(bottom, n = n, max_footer_lines = 1)
    # cli::cli_end()
  } 
  invisible(x)
}

#' @rdname make_mnet
#' @export
print_all <- function(x, ...) {
  print(x, n = Inf)
}

is_grand <- function(.data){
  !is.null(igraph::graph_attr(.data, "grand"))
}

#' @export
`$.mnet` <- function(x, name) {
  if(grepl("\\$", name)){
    type <- sub("\\$.*", "", name)
    name <- sub(".*\\$", "", name)
    out <- switch(type,
                  node = node_attribute(x, name),
                  tie = tie_attribute(x, name),
                  net = igraph::graph_attr(x, name))
    if(is.null(out)) snet_abort("No {.var type} attribute {.var name} found in this object")
    return(out)
  } else {
    name <- sub(".*\\$", "", name)
    if (name %in% igraph::list.vertex.attributes(x)) {
      return(igraph::vertex_attr(x, name))
    } else if (name %in% igraph::list.edge.attributes(x)) {
      return(igraph::edge_attr(x, name))
    } else if (name %in% igraph::list.graph.attributes(x)) {
      return(igraph::graph_attr(x, name))
    } else {
      snet_abort("No attribute {.var name} found in this object")
    }
  }    
}

#' @export
`$<-.mnet` <- function(x, name, value) {
  if (igraph::vcount(x) == length(value)) {
    x <- igraph::set_vertex_attr(x, name, value = value)
  } else if (igraph::ecount(x) == length(value)) {
    x <- igraph::set_edge_attr(x, name, value = value)
  } else if (length(value) == 1) {
    x <- igraph::set_graph_attr(x, name, value = value)
  } else {
    snet_abort("Length of value does not match the length of the nodes, ties, or is not 1")
  }
  return(x)
}

# nocov start
#' @importFrom utils .DollarNames
#' @export
.DollarNames.mnet <- function(x, pattern = "") {
  # Collect possible names
  attrs <- c(
    paste0("node$", net_node_attributes(x)),
    paste0("tie$", net_tie_attributes(x)),
    paste0("net$", igraph::list.graph.attributes(x))
  )
  # Filter by the pattern (so typing g$co will suggest "color")
  grep(pattern, attrs, value = TRUE)
}
#nocov end
