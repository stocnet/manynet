#' Multilevel, multiplex, multimodal, signed, dynamic or longitudinal changing networks
#' @name make_stocnet
#' @description
#'   The 'stocnet' class of network object is a list of four main elements:
#'   nodes, ties, (nodal) changes, and info metadata about the network as a whole.
#'   This offers a consistent and flexible structure that enables more complex 
#'   forms of networks to be contained in a single object.
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
#' @section Info:
#'  The info component of a stocnet object is a list of metadata about the network as a whole.
#'  This can include the name of the network, as well as the names of the types of nodes
#'  and ties in the network.
#'  For example, the info component could include a 'name' element with the name of the network,
#'  a 'nodes' element with a character vector of the names of the types of
#'  nodes in the network, and a 'ties' element with a character vector of the names of the types of
#'  ties in the network.
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
#' @param info A list of metadata about the network as a whole.
#'  This can include the name of the network, as well as the names of the
#'  types of nodes and ties in the network.
#'  For example, the info component could include a 'name' element with the name
#'  of the network, a 'nodes' element with a character vector of the names of the types of
#'  nodes in the network, and a 'ties' element with a character vector of
#'  the names of the types of ties in the network.
#'  By default NULL.
#' @param nodes A tibble of nodes in the network, with one row per node
#'   and one column for the node labels, which should be called 'name'.
#'   Additional columns can be included for node attributes, such as 'active' for changing networks
#'   and 'type' for multimodal networks.
#'   By default NULL.
#' @param ties A tibble of ties in the network, with one row per tie
#'   and at least two columns for the node labels of the tie endpoints, which should be
#'   called 'from' and 'to', even if the network is not directed.
#'   Additional columns can be included for tie attributes, such as 'weight' for weighted networks
#'   and 'type' for multiplex networks.
#'   By default NULL.
#' @param changes A tibble of nodal changes in the network, with one row
#'   per change and at least three columns for the node label of the change, which should be called 'node',
#'   the variable to which the change applies, which should be called 'var', and the
#'   new value to be applied, which should be called 'value'.
#'   Additional columns can be included for the time of the change, such as 'wave'
#'   or 'time'.
#'   By default NULL.
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

#' @rdname make_stocnet
#' @export
validate_stocnet <- function(.data) {
  if(!inherits(.data, "stocnet")) 
    snet_abort("This function only works for stocnet objects.")
  validate_info(.data)
  validate_nodes(.data)
  validate_ties(.data)
  validate_changes(.data)
  invisible(.data)
}

res_cols <- function(.data, component, reserved_cols, class, 
                     length = NULL, match = NULL, aka = NULL) {
  if(reserved_cols %in% names(.data[[component]])){
    if(!is.null(length)){
      if(length(.data[[component]][[reserved_cols]]) != length) 
        snet_abort("'{reserved_cols}' must be of length {length}.")
    }
    if(!inherits(.data[[component]][[reserved_cols]], class)) 
      snet_abort("'{reserved_cols}' must be of class '{class}'.")
    if(!is.null(match)){
      if(!all(.data[[component]][[reserved_cols]] %in% match)) 
        snet_abort("'{reserved_cols}' must be one of {to_phrase(match)}.")
    }
  } else if(!is.null(aka)){
    if(any(aka %in% names(.data[[component]]))){
      mislabelled <- names(.data[[component]])[names(.data[[component]]) %in% aka]
      snet_warn("Columns '{mislabelled}' might be better called {reserved_cols}.")
    }
  }
}

req_cols <- function(.data, component, required_cols) {
  if(!all(required_cols %in% names(.data[[component]]))) 
    snet_abort("The '{component}' component of a stocnet object must have the following columns: {to_phrase(required_cols)}.")
}

exp_class <- function(.data, component, expected_class) {
  if(!inherits(.data[[component]], expected_class)) 
    snet_abort("The '{component}' component of a stocnet object must be of class '{expected_class}'.")
}


validate_nodes <- function(.data){
  if(is.null(.data$nodes)) return(invisible(.data))
  exp_class(.data, "nodes", "tbl_df")
  res_cols(.data, "nodes", "label", "character", aka = c("name", "id"))
  res_cols(.data, "nodes", "mode", "character")
  res_cols(.data, "nodes", "active", "logical")
  invisible(.data)
}

validate_ties <- function(.data){
  if(is.null(.data$ties)) return(invisible(.data))
  exp_class(.data, "ties", "tbl_df")
  req_cols(.data, "ties", c("from", "to"))
  res_cols(.data, "ties", "from", "numeric", aka = c("source", "sender", "ego"))
  res_cols(.data, "ties", "to", "numeric", aka = c("receiver", "target", "alter"))
  res_cols(.data, "ties", "weight", "numeric", aka = c("value", "strength", "val", "sign"))
  res_cols(.data, "ties", "time", "character", aka = c("wave", "period", "date", "begin", "end"))
  res_cols(.data, "ties", "layer", "character", aka = c("type", "plex", "tie"))
  invisible(.data)
}

validate_changes <- function(.data){
  if(is.null(.data$changes)) return(invisible(.data))
  exp_class(.data, "changes", "tbl_df")
  req_cols(.data, "changes", c("node", "var", "value"))
  res_cols(.data, "changes", "node", "numeric", aka = c("id"))
  res_cols(.data, "changes", "var", "character")
  invisible(.data)
}

validate_info <- function(.data){
  if(is.null(.data$info)) return(invisible(.data))
  exp_class(.data, "info", "list")
  res_cols(.data, "info", "name", "character")
  res_cols(.data, "info", "nodes", "character", length = net_modes(.data))
  res_cols(.data, "info", "ties", "character", length = net_layers(.data))
  res_cols(.data, "info", "dependent", "character", length = 1, 
           match = net_tie_names(.data))
  invisible(.data)
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
  if (!is.null(top) && ncol(top)>0){
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
  if (!is.null(bottom) && ncol(bottom)>0){
    # cli::cli_par()
    cli::cli_h3("Ties")
    print(bottom, n = n, max_footer_lines = 1)
    # cli::cli_end()
  } 
  invisible(x)
}

as.logi <- function(x){
  class(x) <- c("logi", class(x))
  x
}

as.value <- function(x){
  class(x) <- c("value", class(x))
  x
}

#' @noRd
#' @importFrom pillar pillar_shaft type_sum
#' @export
pillar_shaft.logi <- function(x, ...) {
  pillar::new_pillar_shaft_simple(ifelse(x, pillar::style_bold(x),
                                         pillar::style_na(x)), align = "left")
}

#' @noRd
#' @export
pillar_shaft.mdate <- function(x, ...) {
  pillar::pillar_shaft(as.character(x), width = 11)
}

# Tell pillar this is a list-like column
#' @exportS3Method
type_sum.value <- function(x, ...) {
  "list"
}

#' @noRd
#' @export
pillar_shaft.value <- function(x, ...) {
  # Determine underlying class
  underlying_class <- sapply(x, function(y) class(y[[1]]))
  underlying_class <- dplyr::replace_values(underlying_class, 
                                            "logical" ~ "lgl",
                                            "character" ~ "chr",
                                            "numeric" ~ "dbl",
                                            "integer" ~ "int")
  
  lengths <- sapply(x, length)
  
  # Value to print (only first element)
  value_text <- lapply(x, function(y) if(length(y) == 1) y else "...")
  
  # Type label exactly like tibble uses
  type_label <- paste0("<", underlying_class, ">")
  
  # Pillar requires the type label to be a *named* vector
  type_vec <- c(type = pillar::style_subtle(type_label))
  
  pillar::new_pillar_shaft_simple(
    paste0(value_text, type_vec),
    align = "right"
  )
}

