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
    mnet_success("Deleting information from previous version(s).")
    igraph::graph_attr(.data)$grand <- NULL
  }
  
  info <- list(...)
  unrecog <- setdiff(names(info), c("name", "nodes", "ties", "doi", 
                                    "collection", "year", "mode", "vertex1", 
                                    "vertex1.total", "vertex2", 
                                    "vertex2.total", 
                                    "edge.pos", "edge.neg", "positive", "negative"))
  if(length(unrecog)>0) 
    cli::cli_alert_warning("{unrecog} are not recognised fields.")
  
  out <- .data
  if("name" %in% names(info)){
    igraph::graph_attr(out)$name <- info$name
  }
  if("nodes" %in% names(info)){
    if(is_twomode(.data) && length(info$nodes)!=2) 
      cli::cli_abort("Please name both nodesets in a two-mode network.")
    igraph::graph_attr(out)$nodes <- info$nodes
  }
  if("ties" %in% names(info)){
    if(is_multiplex(.data) && length(info$ties) != length(unique(tie_attribute(.data, "type")))) 
      cli::cli_abort("Please name all types of tie in a multiplex network.")
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
  out <- .data
  for(item in names(info)){
    igraph::graph_attr(out, item) <- info[[item]]
  }
  out
}




