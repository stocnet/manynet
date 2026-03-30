# Manipulating changes ####

#' Modifying changes to nodes over time
#' @name manip_changes
#' @description
#'   These functions offer ways to modify data held about how nodes
#'   change over time. 
#'   They include:
#'   
#'   - `add_changes()` adds a table of changes to the nodes of a network.
#'   - `mutate_changes()` can be used to update network changes.
#'   - `filter_changes()` is used to subset network changes.
#'   - `collect_changes()` is similar to `filter_changes()`,
#'   but collects the cumulative changes up to a time point.
#'   - `apply_changes()` applies the changes collected up to a time point
#'   to a network, removing the changes.
#' 
#'   An example of when this might be useful is to track change in the
#'   composition of a network (when nodes are present or absent over time),
#'   though the function can flexibly accommodate changes in other
#'   nodal attributes.
#' @template param_data
#' @seealso [to_time()]
NULL

#' @rdname manip_changes
#' @param changes A data frame of changes.
#'   Ideally this will be in the form of "wave", "node", "var", and "value",
#'   but there are internal routines from some otherwise common formats.
#'   A data frame of composition change can be just two columns.
#' @examples
#' add_changes(ison_algebra, 
#'             data.frame(wave = 2, node = 1, var = "active", value = FALSE))
#' @export
add_changes <- function(.data, changes){
  out <- as_tidygraph(.data)
  if(length(names(changes)) == 4){
    
    if("active" %in% changes[,3] && !("active" %in% net_node_attributes(.data))){
      out <- .infer_active(out, changes)
      # changes <- changes[changes[,1] != min(changes[,1]),]
    }
    if("diffusion" %in% changes[,3] && !("diffusion" %in% net_node_attributes(.data))){
      out <- .infer_susceptible(out, changes)
    }
    .check_changevars(changes)
    .check_varexists(out, changes)
    
  } else {
    
    if("active" %in% net_node_attributes(out))
      snet_unavailable("There is already an `active` nodal attribute.")
    
    if(nrow(changes) == net_nodes(out) && ncol(changes)==2){
      # converting a begin-end composition table for all nodes
      out <- out %>% mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
      changes <- data.frame(node = 1:nrow(changes), 
                            begin = changes[,1], end = changes[,2])
    }
    if(all(names(changes) == c("node", "begin", "end"))){
      # inferring starting positions
      first <- changes[!duplicated(changes[,1]),]
      out <- out %>% mutate_nodes(active = interpolate(first[,2] == min(first[,2]),
                                                       first[,1],
                                                       net_nodes(out),
                                                       fill = TRUE))
      
    } else snet_unavailable()
    
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
    snet_abort(paste("The following column names are in the changelist",
                     "but are not recognised:",
                     "{notexist}"))
  }
}

.check_varexists <- function(.data, changes){
  if(!all(unique(changes$var) %in% net_node_attributes(.data))){
    notexist <- unique(changes$var)[which(!unique(changes$var) %in% 
                                            net_node_attributes(.data))]
    snet_abort(paste("The following variables are in the changelist",
                     "but not among the nodal attributes in the network:",
                     "{notexist}"))
  }
}

.infer_active <- function(.data, changes){
  
  if(length(unique(changes$node))==net_nodes(.data)){ # if table of when active
    out <- .data %>% mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
    
  } else { # if some actives
    if(all(changes[changes[,3]=="active",4])){
      starts <- rep(FALSE, net_nodes(.data))
    } else if(all(!changes[changes[,3]=="active",4])) {
      starts <- rep(TRUE, net_nodes(.data))
    } else if(changes[order(changes[,1]),4][1]){
      starts <- rep(NA, net_nodes(.data))
      first <- changes[changes[,1] == min(changes[,1]),]
      starts[first[,2]] <- first[,4]
      starts[is.na(starts) & changes[changes$var == "active" & changes$value == TRUE & changes$wave > min(changes$wave),2]] <- FALSE
    } else snet_unavailable()
    out <- .data %>% mutate_nodes(active = starts)
  }
  out
}

.infer_susceptible <- function(.data, changes){
  .data %>% mutate_nodes(diffusion = "S")
}

#' @rdname manip_changes
#' @export
delete_changes <- function(.data){
  igraph::delete_graph_attr(.data, "changes")
}

#' @rdname manip_changes
#' @export
mutate_changes <- function(.data, ...) UseMethod("mutate_changes")

#' @export
mutate_changes.tbl_graph <- function(.data, ...){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::mutate(changes, ...)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @rdname manip_changes
#' @examples
#' filter_changes(fict_starwars, node == "Anakin")
#' @export
filter_changes <- function(.data, ..., .by = NULL){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::filter(changes, ..., .by = .by)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @rdname manip_changes
#' @examples
#' select_changes(fict_starwars, node)
#' @export
select_changes <- function(.data, ..., .by = NULL){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::select(changes, ..., .by = .by)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @rdname manip_changes
#' @examples
#' collect_changes(fict_starwars, time = 3)
#' @export
collect_changes <- function(.data, time){
  t <- time
  changes <- igraph::graph_attr(.data, "changes")
  changes <- changes %>% 
    dplyr::filter(time <= t) %>% 
    dplyr::arrange(node, var, time) %>% 
    dplyr::group_by(node, var) %>% 
    dplyr::mutate(value = dplyr::last(value)) %>% 
    dplyr::distinct(node, var, value)
  changes
}

#' @rdname manip_changes
#' @examples
#' collect_changes(fict_starwars, time = 3)
#' @export
apply_changes <- function(.data, time){
  out <- as.data.frame(as_nodelist(.data))
  changes <- collect_changes(.data, time)
  if(is.character(changes$node)) 
    changes$node <- match(changes$node, node_names(.data))
  if(is.character(changes$var)) 
    changes$var <- match(changes$var, net_node_attributes(.data))
  for(i in cli::cli_progress_along(1:nrow(changes), "Applying changes")){
    if(is.numeric(out[,changes$var[i]])){
      out[changes$node[i], changes$var[i]] <- as.numeric(changes$value[i])
    } else if (is.logical(out[,changes$var[i]])){
      out[changes$node[i], changes$var[i]] <- as.logical(changes$value[i])
    } else out[changes$node[i], changes$var[i]] <- changes$value[i]
  }
  if(!is_labelled(.data)) out <- cbind(1:nrow(out), out)
  out <- as_tidygraph(igraph::graph_from_data_frame(as_edgelist(.data), 
                                                    vertices = out))
  if(!is_labelled(.data)) out <- to_unnamed(out)
  out
}
