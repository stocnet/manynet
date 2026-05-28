#' Multilevel, multiplex, multimodal, signed, dynamic or longitudinal changing networks
#' @name make_stocnet
#' @description
#'   The 'stocnet' class of network object is a list of four main elements:
#'   nodes, ties, (nodal) changes, and info metadata about the network as a whole.
#'   This offers a consistent and flexible structure that enables more complex 
#'   forms of networks to be contained in a single object.
#'   
#'   Unlike 'mnet' objects, 'stocnet' objects are not layered on top of 'igraph' or 'tbl_graph' objects,
#'   but instead are a list of tibbles and metadata.
#'   Unlike 'igraph' or 'tbl_graph' objects,
#'   'stocnet' objects typically include more complex multimodal, longitudinal, or dynamic networks.
#'   They also typically include more metadata about the network, 
#'   such as the names of the types of nodes and ties in the network.
#'   In other words, they are made not just for network analysis,
#'   but also network modelling.
#' @details
#'   The package includes a validation function for stocnet objects, `validate_stocnet()`. 
#'   This checks that the object has the correct structure and required components,
#'   and suggests improvements to the structure (e.g. correcting or adding reserved names) where possible.
#'   The required and reserved names for the components of a stocnet object are described below.
#' @section Info:
#'   There are several reserved names for the elements of the info component of a stocnet object.
#'   - 'name' should be a single character string with the name of the network.
#'   - 'modes' should be a character vector of the names of the modes of the nodes in a multimodal network.
#'   - 'layers' should be a character vector of the names of the layers of
#'   the ties in a multiplex or multilayer network.
#'   - 'directed' should be a logical indicating whether each layer is directed or undirected.
#'   If there are multiple layers, this can be a named logical vector with the directedness of each layer, 
#'   where the names correspond to the layer names.
#'   - 'dependent' should be a character string indicating which layer is the dependent layer in a multiplex network.
#'   - 'doi' can be a character string with the DOI of the network, if it is from a published source.
#'   - 'date' can be an integer of the year or the date the network represents.
#'   - 'location' can be a character string with the location of the network.
#'   - 'source' can be a character string indicating whether the network is observed or synthetic.
#'   If it is observed, the 'method' of data collection can be further specified.
#'   For example, the source could be 'observed', 'synthetic', 'survey', 'archival', 'digital trace', etc.
#'   If it is synthetic, then more details about how the network was generated can be included.
#'   
#'   Many of these elements are drawn from the GRAND project's metadata standards for networks, 
#'   which are designed to be consistent with the FAIR principles for data management.
#'   
#'   In addition to these reserved names, the info component can include metadata relating to each layer of the network, 
#'   such as the names of the types of nodes and ties in each layer, 
#'   as well as the names of the dependent and independent layers in a multiplex network.
#'   These must be named as one of the layer names.
#'   There are some reserved names for these elements too:
#'   - 'sender' should be a character string naming the type of node that sends ties in this layer.
#'   - 'recipient' should be a character string naming the type of node that receives ties in this layer.
#'   - 'update' should be a character string indicating whether layer changes are by "increment" or "replace".
#'   
#' @section Nodes:
#'   There are several reserved names for the columns of the nodes component of a stocnet object.
#'   - 'label' should be a character vector of the labels of the nodes in the network.
#'   - 'mode' should be a character vector of the modes of the nodes in a multimodal network.
#'   - 'present' should be a logical vector indicating the initial active status of the node in changing networks.
#'   It can also be used to indicate missing nodes in a network, 
#'   if the network is not changing but some nodes are missing,
#'   or the network is changing but some nodes never appear.
#' @section Changes: 
#'   There are several required names for the columns of the changes component of a stocnet object (if one is included).
#'   - 'time' can be an integer (e.g. for a wave) or date (e.g. POSIXct or mdate) vector of the times at which changes occur.
#'   - 'node' must be an index (or names) of the node to which the change applies.
#'   - 'var' must be a string vector naming the variable to which the change applies, such as 'active' for changing networks.
#'   - 'value' must be the new value that should be applied at that change (or incremented, as appropriate).
#'   Note that the value column can be of any class, such as logical for changes to active status, 
#'   or numeric for changes to a nodal attribute.
#'   These values are held internally as a list within the tibble, so that they can be of any class and length, 
#'   but printed as a tibble with a 'value' column that shows the first value and a type label for the class of the value.
#' @section Ties:
#'   There are several required names for the columns of the ties component of a stocnet object (if one is included).
#'   - 'from' must be an integer vector of the nodes sending each tie
#'   - 'to' must be an integer vector of the nodes receiving each tie
#'   
#'   There are also several reserved names for the columns of the ties component of a stocnet object.
#'   - 'layer' should be a character vector of the layer of each tie in a multiplex or multilayer network
#'   - 'weight' should be a numeric vector of the weights of the ties in a weighted network
#'   If the weight vector includes also negative values, then the network is a signed network, 
#'   and the sign of the tie can be determined from the weight.
#'   Missing weights can be used to indicate missing ties in a network.
#'   - 'time' should be a character or date vector of the time at which each tie is active in a longitudinal network.
#' @section Printing: 
#'   When printed, 'stocnet' objects will print to the console any information
#'   stored about the names of the network, its modes, or layers.
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
#'  of the network, 
#'  a 'modes' element with a character vector of the names of the types of
#'  nodes in the network, 
#'  and a 'layers' element with a character vector of
#'  the names of the types of ties in the network.
#'  By default NULL.
#' @param nodes A tibble of nodes in the network, with one row per node
#'   and one column for the node labels, which should be called 'label'.
#'   Additional columns can be included for node attributes, 
#'   such as 'active' for changing networks
#'   and 'mode' for multimodal networks.
#'   By default NULL.
#' @param ties A tibble of ties in the network, with one row per tie
#'   and at least two columns for the node labels of the tie endpoints, which should be
#'   called 'from' and 'to', even if the network is not directed.
#'   Additional columns can be included for tie attributes, 
#'   such as 'weight' for weighted networks
#'   and 'layer' for multiplex networks.
#'   By default NULL.
#' @param changes A tibble of nodal changes in the network, with one row
#'   per change and at least three columns for the node label of the change, 
#'   which should be called 'node',
#'   the variable to which the change applies, which should be called 'var', 
#'   and the new value to be applied, which should be called 'value'.
#'   Additional columns can be included for the time of the change, 
#'   such as 'wave' or 'time'.
#'   By default NULL.
#' @param global A tibble of global variables in the network, with one row
#'   per change and at least three columns for 
#'   the variable to which the change applies, which should be called 'var', 
#'   the time of the change, which should be called 'time',
#'   and the new value to be applied, which should be called 'value'.
#'   By default NULL.
#' @examples
#'   out <- make_stocnet(info = list(name = "Example Network", 
#'                            modes = c("Person", "Organization"), 
#'                            layers = c("Friendship", "Collaboration")),
#'     nodes = data.frame(label = c("A", "B", "C"), 
#'                        mode = c("Person", "Person", "Organization"),
#'                        active = c(TRUE, FALSE, TRUE)),
#'     ties = data.frame(from = c("A", "B"),
#'                       to = c("B", "C"),
#'                       weight = c(1, 2),
#'                       layer = c("Friendship", "Collaboration")),
#'     changes = data.frame(time = c(1, 2),
#'                          node = c("A", "B"),
#'                          var = c("active", "active"),
#'                          value = c(FALSE, TRUE)))
#' @export
make_stocnet <- function(info = NULL, nodes = NULL, ties = NULL, 
                         changes = NULL, global = NULL) {
  out <- list(
    info = info,
    nodes = `if`(is.null(nodes), NULL, dplyr::tibble(nodes)),
    ties = `if`(is.null(ties), NULL, dplyr::tibble(ties)),
    changes = `if`(is.null(changes), NULL, dplyr::tibble(changes)),
    global = `if`(is.null(global), NULL, dplyr::tibble(global))
  )
  # make sure from and to are numeric indices of the nodes, not labels
  out <- index_ties(out)
  out <- index_changes(out)
  out <- structure(out, class = c("stocnet", class(out)))
  validate_stocnet(out)
}

index_ties <- function(.data){
  out <- .data
  if(is.character(out$ties$from) && is.character(out$ties$to) && 
     is.character(out$nodes$label)){
    out$ties <- out$ties |>
      dplyr::mutate(from = match(from, out$nodes$label),
                    to = match(to, out$nodes$label))
  }
  if(is.numeric(out$ties$from) || is.numeric(out$ties$to)){
    out$ties <- out$ties |>
      dplyr::mutate(from = as.integer(from),
                    to = as.integer(to))
  }
  out
}

index_changes <- function(.data){
  out <- .data
  if(is.character(out$changes$node) && is.character(out$nodes$label)){
    out$changes <- out$changes |>
      dplyr::mutate(node = match(node, out$nodes$label))
  }
  out
}

#' @rdname make_stocnet
#' @inheritParams make_mnet
#' @export
print.stocnet <- function(x, ..., n = 12) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  if(!is.null(net_name(x)) && net_name(x) != "")
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
  if(!is.null(x$changes) && ncol(x$changes) >0) n <- ceiling(n/3) else 
    n <- ceiling(n/2)
  if (!is.null(top) && ncol(top)>0){
    cli::cli_par()
    cli::cli_h3("Nodes")
    print(top, n = n)
    cli::cli_end()
  } 
  if(!is.null(x$changes) && ncol(x$changes) >0){
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

