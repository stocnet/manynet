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
#' @name manip_net
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

#' @rdname manip_net
#' @export
mutate_net <- function(.data, ...){
  info <- list(...)
  out <- .data
  for(item in names(info)){
    igraph::graph_attr(out, item) <- info[[item]]
  }
  out
}


# Network changes ####

#' Modifying nodal attributes over time
#' @description
#'   This function adds a table of changes to the nodes of a network.
#'   An example of when this might be useful is to track change in the
#'   composition of a network (when nodes are present or absent over time),
#'   though the function can flexibly accommodate changes in other
#'   nodal attributes.
#' @name manip_changes
#' @inheritParams mark_is
#' @param changes A data frame of changes.
#'   Ideally this will be in the form of "wave", "node", "var", and "value",
#'   but there are internal routines from some otherwise common formats.
#'   A data frame of composition change can be just two columns.
#' @examples
#' add_changes(ison_algebra, 
#'             data.frame(wave = 2, node = 1, var = "active", value = FALSE))
#' @export
add_changes <- function(.data, changes){
  out <- .data
  if(length(names(changes)) == 4){
    
    if("active" %in% changes[,3] && !("active" %in% net_node_attributes(.data))){
      out <- .infer_active(out, changes)
      # changes <- changes[changes[,1] != min(changes[,1]),]
    }
    .check_changevars(changes)
    .check_varexists(out, changes)
    
  } else {
    
    if("active" %in% net_node_attributes(.data))
      mnet_unavailable("There is already an `active` nodal attribute.")
    
    if(nrow(changes) == net_nodes(.data) && ncol(changes)==2){
      out <- out %>% mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
      changes <- data.frame(node = 1:nrow(changes), 
                            begin = changes[,1], end = changes[,2])
    } else if(all(names(changes) == c("node", "begin", "end"))){
      
      first <- changes[!duplicated(changes[,1]),]
      out <- out %>% mutate_nodes(active = first[,2] == min(first[,2]))
      
    } else mnet_unavailable()
    
    changes <- stats::reshape(changes,
                       varying = colnames(changes)[-1],
                       v.names = "wave",
                       times = colnames(changes)[-1],
                       timevar = "value", direction = "long")
    changes <- changes %>% dplyr::mutate(wave = wave, node = node, 
                                         var = "active", value = value=="begin") %>% 
      dplyr::select(wave, node, var, value) %>% dplyr::arrange(wave, node) %>% 
      dplyr::filter(wave != 1)
  }
  
  igraph::graph_attr(out)$changes <- changes
  as_tidygraph(out)
}

.check_changevars <- function(changes){
  if(!(all(names(changes) == c("wave", "node", "var", "value")) || 
       all(names(changes) == c("time", "node", "var", "value")))){
    notexist <- setdiff(names(changes), c("time", "wave", "node", "var", "value"))
    cli::cli_abort(paste("The following column names are in the changelist",
                         "but are not recognised:",
                         "{notexist}"))
  }
}

.check_varexists <- function(.data, changes){
  if(!all(unique(changes$var) %in% net_node_attributes(.data))){
    notexist <- unique(changes$var)[which(!unique(changes$var) %in% 
                                            net_node_attributes(.data))]
    cli::cli_abort(paste("The following variables are in the changelist",
                                "but not among the nodal attributes in the network:",
                                "{notexist}"))
  }
}

.infer_active <- function(.data, changes){
  
  if(length(unique(changes$node))==net_nodes(.data)){ # if table of when active
    out <- .data %>% mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))

  } else { # if some actives
    if(all(changes[changes[,3]=="active",4])){
      starts <- rep(FALSE, net_nodes(out))
    } else if(all(!changes[changes[,3]=="active",4])) {
      starts <- rep(TRUE, net_nodes(out))
    } else if(changes[order(changes[,1]),4][1]){
      starts <- rep(NA, net_nodes(out))
      first <- changes[changes[,1] == min(changes[,1]),]
      starts[first[,2]] <- first[,4]
      starts[is.na(starts) & changes[changes$var == "active" & changes$value == TRUE & changes$wave > min(changes$wave),2]] <- FALSE
    } else mnet_unavailable()
    out <- .data %>% mutate_nodes(active = starts)
  }
  out
}

