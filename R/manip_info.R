# Network information ####

#' Modifying network data
#' 
#' @description
#'   These functions allow users to add and edit information about the network
#'   itself.
#'   This includes the name, year, and mode of collection of the network,
#'   as well as definitions of the nodes and ties in the network.
#'   Where available, this information is printed for tidygraph-class objects,
#'   and can be used for printing a grand table in the `{grand}` package.
#' @name manip_info
#' @inheritParams mark_is
#' @param ... Named attributes. The following are currently recognised:
#'   "name", "year", and "doi" of the network,
#'   "collection" or "mode" of the network 
#'   ("survey", "interview","sensor","observation","archival", or "simulation"),
#'   "nodes" (a vector of the names of the nodes) or "vertex1"/"vertex2",
#'   "ties" or "edge.pos"/"edge.neg" for defining the ties.
#' @examples
#' add_info(ison_algebra, name = "Algebra")
#' @export
add_info <- function(.data, ...){
  
  if(!is.null(igraph::graph_attr(.data)$grand)){ # Updating
    snet_success("Deleting information from previous version(s).")
    igraph::graph_attr(.data)$grand <- NULL
  }
  
  info <- list(...)
  if(length(info)==0) return(.check_info(.data))
  
  unrecog <- setdiff(names(info), c("name", "nodes", "ties", "doi", 
                                    "source", "method", "location", "date", "system",
                                    "degree",
                                    "collection", "year", "mode", "vertex1", 
                                    "vertex1.total", "vertex2", 
                                    "vertex2.total", 
                                    "edge.pos", "edge.neg", "positive", "negative"))
  if(length(unrecog)>0) 
    snet_warn("{unrecog} are not recognised fields.")
  
  out <- .data
  if("name" %in% names(info)){
    igraph::graph_attr(out)$name <- info$name
  }
  if("nodes" %in% names(info)){
    if(is_twomode(.data) && length(info$nodes)!=2) 
      snet_abort("Please name both nodesets in a two-mode network.")
    igraph::graph_attr(out)$nodes <- info$nodes
  }
  if("ties" %in% names(info)){
    if(is_multiplex(.data) && length(info$ties) != length(unique(tie_attribute(.data, "type")))) 
      snet_abort("Please name all types of tie in a multiplex network.")
    igraph::graph_attr(out)$ties <- info$ties
  }
  if("collection" %in% names(info)){
    igraph::graph_attr(out)$collection <- info$collection
  }
  if("doi" %in% names(info)){
    igraph::graph_attr(out)$doi <- info$doi
  }
  if("year" %in% names(info)){
    igraph::graph_attr(out)$year <- info$year
  }
  # return(str(info)) # for debugging
  
  as_tidygraph(out)
}

#' @rdname manip_info
#' @export
mutate_net <- function(.data, ...){
  info <- list(...)
  out <- as_tidygraph(.data)
  for(item in names(info)){
    igraph::graph_attr(out, item) <- info[[item]]
  }
  out
}

#' @rdname manip_info
#' @export
net_info <- function(.data){
  igraph::graph_attr(as_igraph(.data))
}

#' @rdname manip_info
#' @export
net_attributes <- function(.data){
  names(igraph::graph_attr(as_igraph(.data)))
}


.check_info <- function(.data, optional = FALSE){
  
  out <- .data
  
  # Names
  if(is.null(net_name(.data)) || net_name(.data) == ""){
    snet_prompt("This network does not have a name. Please add one.")
    out <- add_info(out, name = readline(prompt = "Network name: "))
  } else snet_success("Network name: {net_name(out)}")
  
  # Nodes
  if(is_twomode(.data) && is.null(net_node_names(.data))){
    snet_prompt("This two-mode network does not have names for the nodesets. Please add one.")
    out$nodes <- c(readline(prompt = "Nodeset 1 name: "),
                   readline(prompt = "Nodeset 2 name: "))
  } else if(is_twomode(.data)){
    snet_success("Nodesets: {net_node_names(out)}")
  } else if(!is_twomode(.data) && is.null(net_node_names(.data))){
    snet_prompt("This network does not have a name for the nodes. Please add one.")
    out <- add_info(out, nodes = readline(prompt = "Nodeset name: "))
  } else snet_success("Nodeset: {net_node_names(out)}")
  
  # Source & method
  if(optional){
    if(!"source" %in% net_attributes(.data)){
      snet_prompt("This network does not have a source. You may add one.")
      source_options <- c("Observed", "Synthetic")
      source <- menu(choices = source_options, title = "Is this network observed or synthetic?")
      if(source == 1){
        method_options <- c("Survey", "Interview", "Sensor", "Archival", "Trace", "Ethnography")
        out <- add_info(out, source = source_options[source],
                        method = menu(choices = source_options, title = "Method: "))
      } else if(source == 2){
        out <- add_info(out, source = source_options[source],
                        method = readline(prompt = "Model: "))
      }
    } else snet_success("Source: {net_info(out)$source}")
  }
  
  # Ties
  if(is.null(net_tie_names(.data))){
    snet_prompt("This network does not have a name for the ties. Please add one.")
    out <- add_info(out, ties = readline(prompt = "Ties name: "))
  } else snet_success("Ties: {net_tie_names(out)}")

  out
}

