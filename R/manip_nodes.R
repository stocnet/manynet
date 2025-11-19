# Manipulating nodes ####

#' Modifying node data
#' 
#' @description 
#'   These functions allow users to add and delete nodes and their attributes:
#'   
#'   - `add_nodes()` adds an additional number of nodes to network data.
#'   - `delete_nodes()` deletes nodes from network data.
#'   - `add_node_attribute()`, `mutate()`, or `mutate_nodes()` offer ways to add 
#'   a vector of values to a network as a nodal attribute.
#'   - `rename_nodes()` and `rename()` rename nodal attributes.
#'   - `bind_node_attributes()` appends all nodal attributes from one network to another,
#'   and `join_nodes()` merges all nodal attributes from one network to another.
#'   - `filter_nodes()` subsets nodes based on some nodal attribute-related logical statement.
#'   
#'   Note that while `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `mutate*()`, `bind*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |             | igraph| network| tbl_graph|
#'   |:------------|------:|-------:|---------:|
#'   |add_nodes    |      1|       1|         1|
#'   |delete_nodes |      1|       1|         1|
#' @family modifications
#' @inheritParams mark_is
#' @param attribute A named list to be added as tie or node attributes.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param object2 A second object to copy nodes or ties from.
#' @param vector A vector of values for the new attribute.
#' @param ... Additional arguments.
#' @return A data object of the same class as the function was given.
#' @examples
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   add_nodes(other, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
#' @name manip_nodes
NULL

#' @rdname manip_nodes
#' @param nodes The number of nodes to be added.
#' @importFrom igraph add_vertices
#' @export
add_nodes <- function(.data, nodes, attribute = NULL) UseMethod("add_nodes")

#' @export
add_nodes.igraph <- function(.data, nodes, attribute = NULL){
  igraph::add_vertices(.data, nv = nodes, attr = attribute)
}

#' @export
add_nodes.tbl_graph <- function(.data, nodes, attribute = NULL){
  as_tidygraph(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @export
add_nodes.network <- function(.data, nodes, attribute = NULL){
  as_network(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @rdname manip_nodes
#' @importFrom igraph delete_vertices
#' @export
delete_nodes <- function(.data, nodes) UseMethod("delete_nodes")

#' @export
delete_nodes.igraph <- function(.data, nodes){
  igraph::delete_vertices(.data, v = nodes)
}

#' @export
delete_nodes.tbl_graph <- function(.data, nodes){
  as_tidygraph(igraph::delete_vertices(.data, v = nodes))
}

#' @export
delete_nodes.network <- function(.data, nodes){
  as_network(igraph::delete_vertices(as_igraph(.data), v = nodes))
}

#' @rdname manip_nodes 
#' @importFrom igraph vertex_attr
#' @export
add_node_attribute <- function(.data, attr_name, vector){
  if(length(vector)!=igraph::vcount(as_igraph(.data))){
    if(is_twomode(.data) && any(length(vector) == infer_dims(.data))){
      if(length(vector) == infer_dims(.data)[1]){
        vector <- c(vector, rep(NA, infer_dims(.data)[2]))
      } else if (length(vector) == infer_dims(.data)[2]){
        vector <- c(rep(NA, infer_dims(.data)[1]), vector)
      }
    } else 
      snet_abort("Attribute vector must be same length as nodes in object.")
  }
  out <- as_igraph(.data)
  igraph::vertex_attr(out, name = attr_name) <- vector
  if(inherits(.data, "tbl_graph")) as_tidygraph(out) else
    if(inherits(.data, "igraph")) as_igraph(out) else
      if(inherits(.data, "igraph")) as_network(out) else
          message("This function only works for igraph, tidygraph, or network objects.")
}

#' @rdname manip_nodes
#' @importFrom tidygraph mutate
#' @export
mutate <- tidygraph::mutate

#' @rdname manip_nodes
#' @importFrom tidygraph mutate
#' @export
mutate_nodes <- function(.data, ...) UseMethod("mutate_nodes")

#' @export
mutate_nodes.tbl_graph <- function(.data, ...){
  .data %>% tidygraph::mutate(...)
}

#' @export
mutate_nodes.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    tidygraph::mutate(...) %>% as_igraph()
}

#' @rdname manip_nodes
#' @importFrom tidygraph select
#' @export
select <- tidygraph::select

#' @rdname manip_nodes
#' @importFrom tidygraph mutate
#' @export
select_nodes <- function(.data, ...) UseMethod("select_nodes")

#' @export
select_nodes.tbl_graph <- function(.data, ...){
  .data %>% tidygraph::select(...)
}

#' @export
select_nodes.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    tidygraph::select(...) %>% as_igraph()
}

#' @rdname manip_nodes
#' @export
bind_node_attributes <- function(.data, object2){
  out <- as_igraph(.data)
  object2 <- as_igraph(object2)
  if(igraph::vcount(as_igraph(.data)) != igraph::vcount(as_igraph(object2))){
    # warning("Not the same dimensions. Coercing to same.")
    out <- add_nodes(out, igraph::vcount(as_igraph(object2)) - igraph::vcount(as_igraph(out)))
  }
  for(a in igraph::vertex_attr_names(object2)){
    out <- igraph::set_vertex_attr(out, a, 
                                   value = igraph::vertex_attr(object2, a))
  }
  as_tidygraph(out)
}

#' @rdname manip_nodes 
#' @param join_type A type of join to be used.
#'   Options are "full","left", "right", "inner".
#' @param .by An attribute name to join objects by.
#'   By default, NULL.
#' @examples
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   another <- create_filled(3) %>% mutate(name = c("E", "F", "G"))
#'   join_nodes(another, other)
#' @export
join_nodes <- function(.data, object2, .by = NULL,
                       join_type = c("full","left", "right", "inner")){
  join_type <- match.arg(join_type)
  out <- as_tidygraph(.data)
  if(is_graph(object2)){
    object2 <- as_tidygraph(object2)
  }
  switch(join_type,
         "full" = dplyr::full_join(out, object2, by = .by, copy = TRUE),
         "left" = dplyr::left_join(out, object2, by = .by, copy = TRUE),
         "right" = dplyr::right_join(out, object2, by = .by, copy = TRUE),
         "inner" = dplyr::inner_join(out, object2, by = .by, copy = TRUE))
}

#' @rdname manip_nodes
#' @importFrom tidygraph rename
#' @export
rename_nodes <- tidygraph::rename

#' @rdname manip_nodes
#' @importFrom tidygraph rename
#' @export
rename <- tidygraph::rename

#' @rdname manip_nodes
#' @importFrom tidygraph filter
#' @export
filter_nodes <- function(.data, ..., .by = NULL){
  tidygraph::filter(.data, ..., .by = .by)
}

# Manipulating changes ####

#' Modifying changes to nodes over time
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
#' @name manip_changes
#' @inheritParams manip_nodes
#' @inheritParams manip_scope
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
  out <- .data
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
    
    if("active" %in% net_node_attributes(.data))
      snet_unavailable("There is already an `active` nodal attribute.")
    
    if(nrow(changes) == net_nodes(.data) && ncol(changes)==2){
      # converting a begin-end composition table for all nodes
      out <- out %>% mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
      changes <- data.frame(node = 1:nrow(changes), 
                            begin = changes[,1], end = changes[,2])
    }
    if(all(names(changes) == c("node", "begin", "end"))){
      # inferring starting positions
      first <- changes[!duplicated(changes[,1]),]
      out <- out %>% mutate_nodes(active = first[,2] == min(first[,2]))
      
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

