# Network information ####

#' Manipulating network information
#' @name manip_info
#' @description
#'   These functions allow users to add and edit information about the network
#'   itself.
#'   This includes the name, year, and mode of collection of the network,
#'   as well as definitions of the nodes and ties in the network.
#'   Where available, this information is printed for tidygraph-class objects,
#'   and can be used for printing a grand table in the `{grand}` package.
#'   
#'   - `add_info()` adds information attributes to the network.
#'   - `mutate_info()` updates information attributes of the network.
#'   - `select_info()` selects a subset of information attributes.
#'   - `filter_info()` filters information attributes by name.
#'   - `rename_info()` renames information attributes.
#' @template param_data
#' @family info
#' @template fam_manip
#' @param ... Named attributes. The following are currently recognised:
#'   "name", "year", and "doi" of the network,
#'   "collection" or "mode" of the network 
#'   ("survey", "interview","sensor","observation","archival", or "simulation"),
#'   "nodes" (a vector of the names of the nodes) or "vertex1"/"vertex2",
#'   "ties" or "edge.pos"/"edge.neg" for defining the ties.
#' @examples
#' add_info(ison_algebra, name = "Algebra")
#' @export
add_info <- function(.data, ...) UseMethod("add_info")

#' @export
add_info.default <- function(.data, ...){
  as_input(.data, add_info, ...)
}

#' @export
add_info.igraph <- function(.data, ...){
  
  if(!is.null(igraph::graph_attr(.data)$grand)){ # Updating
    snet_success("Deleting information from previous version(s).")
    igraph::graph_attr(.data)$grand <- NULL
  }
  
  info <- list(...)
  if(length(info)==0) return(.check_info(.data))
  
  unrecog <- setdiff(names(info), c("name", "nodes", "ties", "doi", 
                                    "source", "method", "location", "date", "system",
                                    "degree",
                                    "dependent",
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

#' @export
add_info.stocnet <- function(.data, ...){
  dots <- list(...)
  for(item in names(dots)){
    .data$info[[item]] <- dots[[item]]
  }
  .data
}

#' @rdname manip_info
#' @export
mutate_info <- function(.data, ...) UseMethod("mutate_info")

#' @export
mutate_info.default <- function(.data, ...){
  as_input(.data, mutate_info, ...)
}

#' @export
mutate_info.igraph <- function(.data, ...){
  info <- list(...)
  out <- as_tidygraph(.data)
  for(item in names(info)){
    igraph::graph_attr(out, item) <- info[[item]]
  }
  out
}

#' @export
mutate_info.stocnet <- function(.data, ...){
  dots <- list(...)
  out <- .data
  for(item in names(dots)){
    out$info[[item]] <- dots[[item]]
  }
  out
}

#' @rdname manip_info
#' @export
net_attributes <- function(.data){
  names(igraph::graph_attr(as_igraph(.data)))
}

#' @rdname manip_info
#' @export
filter_info <- function(.data, ...) UseMethod("filter_info")

#' @export
filter_info.default <- function(.data, ...){
  as_input(.data, filter_info, ...)
}

#' @export
filter_info.igraph <- function(.data, ...){
  keep <- c(...)
  out <- .data
  all_attrs <- names(igraph::graph_attr(out))
  to_remove <- setdiff(all_attrs, keep)
  for(attr in to_remove){
    out <- igraph::delete_graph_attr(out, attr)
  }
  out
}

#' @export
filter_info.stocnet <- function(.data, ...){
  keep <- c(...)
  out <- .data
  out$info <- out$info[intersect(names(out$info), keep)]
  out
}

#' @rdname manip_info
#' @export
select_info <- function(.data, ...) UseMethod("select_info")

#' @export
select_info.default <- function(.data, ...){
  as_input(.data, select_info, ...)
}

#' @export
select_info.igraph <- function(.data, ...){
  keep <- c(...)
  out <- .data
  all_attrs <- names(igraph::graph_attr(out))
  to_remove <- setdiff(all_attrs, keep)
  for(attr in to_remove){
    out <- igraph::delete_graph_attr(out, attr)
  }
  out
}

#' @export
select_info.stocnet <- function(.data, ...){
  keep <- c(...)
  out <- .data
  out$info <- out$info[intersect(names(out$info), keep)]
  out
}

#' @rdname manip_info
#' @export
rename_info <- function(.data, ...) UseMethod("rename_info")

#' @export
rename_info.default <- function(.data, ...){
  as_input(.data, rename_info, ...)
}

#' @export
rename_info.igraph <- function(.data, ...){
  dots <- c(...)
  out <- .data
  # dots is named: new_name = "old_name"
  for(i in seq_along(dots)){
    old_name <- dots[[i]]
    new_name <- names(dots)[i]
    val <- igraph::graph_attr(out, old_name)
    out <- igraph::delete_graph_attr(out, old_name)
    igraph::graph_attr(out, new_name) <- val
  }
  out
}

#' @export
rename_info.stocnet <- function(.data, ...){
  dots <- c(...)
  out <- .data
  for(i in seq_along(dots)){
    old_name <- dots[[i]]
    new_name <- names(dots)[i]
    out$info[[new_name]] <- out$info[[old_name]]
    out$info[[old_name]] <- NULL
  }
  out
}


.check_info <- function(.data, optional = FALSE){
  
  out <- .data
  
  # Names
  if(is.null(net_name(.data)) || net_name(.data) == ""){
    snet_prompt("This network does not have a name. Please add one.")
    out <- add_info(out, name = readline(prompt = "Network name: "))
  } else snet_success("Network name: {net_name(out)}")
  
  # Nodes
  if(is_twomode(.data) && is.null(mode_names(.data))){
    snet_prompt("This two-mode network does not have names for the nodesets. Please add one.")
    out$nodes <- c(readline(prompt = "Nodeset 1 name: "),
                   readline(prompt = "Nodeset 2 name: "))
  } else if(is_twomode(.data)){
    snet_success("Nodesets: {mode_names(out)}")
  } else if(!is_twomode(.data) && is.null(mode_names(.data))){
    snet_prompt("This network does not have a name for the nodes. Please add one.")
    out <- add_info(out, nodes = readline(prompt = "Nodeset name: "))
  } else snet_success("Nodeset: {mode_names(out)}")
  
  # Source & method
  if(optional){
    if(!"source" %in% net_attributes(.data)){
      snet_prompt("This network does not have a source. You may add one.")
      source_options <- c("Observed", "Synthetic")
      source <- utils::menu(choices = source_options, title = "Is this network observed or synthetic?")
      if(source == 1){
        method_options <- c("Survey", "Interview", "Sensor", "Archival", "Trace", "Ethnography")
        out <- add_info(out, source = source_options[source],
                        method = utils::menu(choices = source_options, title = "Method: "))
      } else if(source == 2){
        out <- add_info(out, source = source_options[source],
                        method = readline(prompt = "Model: "))
      }
    } else snet_success("Source: {net_info(out)$source}")
  }
  
  # Ties
  if(is.null(layer_names(.data))){
    snet_prompt("This network does not have a name for the ties. Please add one.")
    out <- add_info(out, ties = readline(prompt = "Ties name: "))
  } else snet_success("Ties: {layer_names(out)}")

  out
}

