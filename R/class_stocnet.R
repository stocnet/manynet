#' Multilevel, multiplex, multimodal, signed, dynamic or longitudinal changing networks
#' @name make_stocnet
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
NULL

#' @rdname make_stocnet
#' @export
make_stocnet <- function(info = NULL, nodes = NULL, ties = NULL, changes = NULL) {
  list(
    info = info,
    nodes = nodes,
    ties = ties,
    changes = changes
  ) %>% 
    structure(class = "stocnet")
}

res_cols <- function(.data, component, reserved_cols, class, length = NULL, aka = NULL) {
  if(reserved_cols %in% colnames(.data[[component]])){
    if(!is.null(length)){
      if(length(.data[[component]][[reserved_cols]]) != length) 
        snet_abort("'{reserved_cols}' must be of length {length}.")
    }
    if(!inherits(.data[[component]][[reserved_cols]], class)) 
      snet_abort("'{reserved_cols}' must be of class '{class}'.")
  } else if(!is.null(aka)){
    if(any(aka %in% colnames(.data[[component]]))){
      mislabelled <- colnames(.data[[component]])[colnames(.data[[component]]) %in% aka]
      snet_warn("Columns '{mislabelled}' might be better called {reserved_cols}.")
    }
  }
}

req_cols <- function(.data, component, required_cols) {
  if(!all(required_cols %in% colnames(.data[[component]]))) 
    snet_abort("The '{component}' component of a stocnet object must have the following columns: {to_phrase(required_cols)}.")
}

exp_class <- function(.data, component, expected_class) {
  if(!inherits(.data[[component]], expected_class)) 
    snet_abort("The '{component}' component of a stocnet object must be of class '{expected_class}'.")
}


#' @rdname make_stocnet
#' @export
print.stocnet <- function(x, ..., n = 12) {
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

