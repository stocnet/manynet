# Manipulating changes ####

#' Manipulating changes to nodes over time
#' @name manip_changes
#' @description
#'   These functions offer ways to modify data held about how nodes
#'   change over time. 
#'   They include:
#'   
#'   - `bind_changes()` adds a table of changes to the nodes of a network.
#'   - `mutate_changes()` can be used to update network changes.
#'   - `filter_changes()` is used to subset network changes.
#'   - `gather_changes()` is similar to `filter_changes()`,
#'   but collects the cumulative changes up to a time point.
#'   - `apply_changes()` applies the changes collected up to a time point
#'   to a network, removing the changes.
#' 
#'   An example of when this might be useful is to track change in the
#'   composition of a network (when nodes are present or absent over time),
#'   though the function can flexibly accommodate changes in other
#'   nodal attributes.
#' @template param_data
#' @template param_dots
#' @template param_by
#' @family changes
#' @eval detail_avail(".*_changes")
#' @template fam_manip
#' @seealso [to_time()]
NULL

#' @rdname manip_changes
#' @export
delete_changes <- function(.data) UseMethod("delete_changes")

#' @export
delete_changes.default <- function(.data){
  as_input(.data, delete_changes)
}

#' @export
delete_changes.igraph <- function(.data){
  igraph::delete_graph_attr(.data, "changes")
}

#' @export
delete_changes.stocnet <- function(.data){
  out <- .data
  out$changes <- NULL
  out
}

#' @rdname manip_changes
#' @export
mutate_changes <- function(.data, ...) UseMethod("mutate_changes")

#' @export
mutate_changes.default <- function(.data, ...){
  as_input(.data, mutate_changes, ...)
}

#' @export
mutate_changes.tbl_graph <- function(.data, ...){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::mutate(changes, ...)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @export
mutate_changes.stocnet <- function(.data, ...){
  out <- .data
  changes <- out$changes
  changes <- dplyr::mutate(changes, ...)
  out$changes <- changes
  out
}

#' @rdname manip_changes
#' @param changes A data frame containing the changes to be added. 
#'   This should have columns `time`, `node` (labelled), 
#'   `var` (referencing an existing nodal attribute), 
#'   and `value` (which can be of any class). 
#' @param var A character string specifying the nodal variable to which the changes apply,
#'  if not already specified in the `changes` data frame. This is only used
#'  when the `changes` data frame does not already have a `var` column, and is ignored otherwise.
#' @export
bind_changes <- function(.data, changes, var, ...) UseMethod("bind_changes")

#' @export
bind_changes.default <- function(.data, changes, var, ...){
  as_input(.data, bind_changes, changes, var, ...)
}

#' @export
bind_changes.tbl_graph <- function(.data, changes, var, ...){
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
    
    if("active" %in% net_node_attributes(out))
      snet_unavailable("There is already an `active` nodal attribute.")
    
    if(nrow(changes) == net_nodes(out) && ncol(changes)==2){
      # converting a begin-end composition table for all nodes
      out <- out |> mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
      changes <- data.frame(node = 1:nrow(changes), 
                            begin = changes[,1], end = changes[,2])
    }
    if(all(names(changes) == c("node", "begin", "end"))){
      # inferring starting positions
      first <- changes[!duplicated(changes[,1]),]
      out <- out |> mutate_nodes(active = interpolate(first[,2] == min(first[,2]),
                                                      first[,1],
                                                      net_nodes(out),
                                                      fill = TRUE))
      
    } else snet_unavailable()
    
    changes <- stats::reshape(changes,
                              varying = colnames(changes)[-1],
                              v.names = "wave",
                              times = colnames(changes)[-1],
                              timevar = "value", direction = "long")
    changes <- changes |> dplyr::mutate(wave = wave, node = node, 
                                        var = "active", value = value=="begin") |> 
      dplyr::select(wave, node, var, value) |> dplyr::arrange(wave, node) |> 
      dplyr::filter(wave != 1)
  }
  
  igraph::graph_attr(out)$changes <- changes
  as_tidygraph(out)
}

.check_changevars <- function(changes){
  required <- c("time", "node", "var", "value")
  if(!setequal(names(changes), required)){
    notexist <- setdiff(names(changes), required)
    snet_abort(paste("The following column names are in the changelist",
                     "but are not recognised:", phrase(notexist)))
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
    out <- .data |> mutate_nodes(active = as.logi(changes[,1] == min(changes[,1])))
    
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
    out <- .data |> mutate_nodes(active = starts)
  }
  out
}

.infer_susceptible <- function(.data, changes){
  .data |> mutate_nodes(diffusion = "S")
}

#' @export
bind_changes.stocnet <- function(.data, changes, var, ...){
  out <- .data
  changes <- dplyr::as_tibble(changes)
  
  ## 1. Rename columns to stocnet conventions --------------------------------
  rename_map <- c(wave = "time", period = "time", date = "time",
                  begin = "time", end = "time",
                  replace = "value", increment = "value", val = "value")
  update_type <- NULL
  for(old in names(rename_map)){
    if(old %in% names(changes)){
      if(old %in% c("replace", "increment")) update_type <- old
      new <- rename_map[[old]]
      if(!(new %in% names(changes)))
        names(changes)[names(changes) == old] <- new
    }
  }
  
  ## 2. Add 'var' if not already present --------------------------------------
  if(!"var" %in% names(changes)){
    if(missing(var))
      snet_abort(paste("Please specify which nodal variable these changes",
                       "apply to via 'var ='."))
    changes$var <- var
  }
  
  ## 3. Reindex node labels onto .data's existing node indexing ----------------
  changes <- .match_node_labels(out$nodes, changes, "node")
  
  ## 4. Reorder to (time, node, var, value) and validate column names ----------
  changes <- changes |> dplyr::select(time, node, var, value)
  .check_changevars(changes)
  
  ## 5. Special-case inference for 'active'/'diffusion' -------------------------
  if("active" %in% changes$var && !("active" %in% net_node_attributes(out)))
    out <- .infer_active(out, changes)
  if("diffusion" %in% changes$var && !("diffusion" %in% net_node_attributes(out)))
    out <- .infer_susceptible(out, changes)
  
  .check_varexists(out, changes)
  
  ## 6. Wrap 'value' as a list-column of class 'value' for storage -------------
  changes$value <- as.list(changes$value)
  
  ## 7. Bind onto any existing changes, and re-sort ------------------------------
  out$changes <- dplyr::bind_rows(out$changes, changes)
  out$changes$value <- out$changes$value
  out$changes <- out$changes |> dplyr::arrange(time, node)
  
  ## 8. Record update type (increment/replace) per variable ----------------------
  if(!is.null(update_type)){
    upd_value <- if(update_type == "increment") "increment" else "replace"
    for(v in unique(changes$var)){
      if(is.null(out$info[[v]])) out$info[[v]] <- list()
      out$info[[v]]$update <- upd_value
    }
  }
  
  validate_stocnet(out)
}

#' @rdname manip_changes
#' @examples
#' filter_changes(fict_starwars, node == "Anakin")
#' @export
filter_changes <- function(.data, ..., .by = NULL) UseMethod("filter_changes")

#' @export
filter_changes.default <- function(.data, ..., .by = NULL){
  as_input(.data, filter_changes, ..., .by = .by)
}

#' @export
filter_changes.igraph <- function(.data, ..., .by = NULL){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::filter(changes, ..., .by = .by)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @export
filter_changes.stocnet <- function(.data, ..., .by = NULL){
  out <- .data
  changes <- out$changes
  changes <- dplyr::filter(changes, ..., .by = .by)
  out$changes <- changes
  out
}

#' @rdname manip_changes
#' @examples
#' select_changes(fict_starwars, node)
#' @export
select_changes <- function(.data, ..., .by = NULL) UseMethod("select_changes")

#' @export
select_changes.default <- function(.data, ..., .by = NULL){
  as_input(.data, select_changes, ..., .by = .by)
}

#' @export
select_changes.igraph <- function(.data, ..., .by = NULL){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::select(changes, ..., .by = .by)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @export
select_changes.data.frame <- function(.data, ..., .by = NULL){
  out <- .data
  if(...length() == 0){
    out <- dplyr::select(out, dplyr::any_of(c("node", "time", "var", "value")), 
                         .by = .by)
  } else out <- dplyr::select(out, ..., .by = .by)
  out
}

#' @export
select_changes.stocnet <- function(.data, ..., .by = NULL){
  out <- .data
  out$changes <- select_changes(out$changes, ..., .by = .by)
  out
}

#' @rdname manip_changes
#' @export
arrange_changes <- function(.data, ...) UseMethod("arrange_changes")

#' @export
arrange_changes.default <- function(.data, ...){
  
  as_input(.data, arrange_changes, ...)
}

#' @export
arrange_changes.igraph <- function(.data, ...){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- dplyr::arrange(changes, ...)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @export
arrange_changes.stocnet <- function(.data, ...){
  out <- .data
  changes <- out$changes
  changes <- dplyr::arrange(changes, ...)
  out$changes <- changes
  out
}

#' @rdname manip_changes
#' @export
rename_changes <- function(.data, ...) UseMethod("rename_changes")

#' @export
rename_changes.default <- function(.data, ...){
  as_input(.data, rename_changes, ...)
}

#' @export
rename_changes.igraph <- function(.data, ...){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- dplyr::rename(changes, ...)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @export
rename_changes.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    aka <- list(
      node   = c("id", "actor", "vertex", "ego", "alter"),
      time   = c("wave", "period", "date", "begin"),
      var   = c("variable", "attribute"),
      value = c("weight", "strength", "increment", "replace", "sign")
    )
    
    current_names <- names(out)
    rename_map <- c()
    
    for(expected in names(aka)){
      if(!expected %in% current_names){
        match <- intersect(aka[[expected]], current_names)
        if(length(match) > 0){
          rename_map[expected] <- match[1]  # take the first match if multiple
        }
      }
    }
    
    if(length(rename_map) > 0){
      snet_minor_info("Renaming change columns to stocnet conventions:",
                      "{paste(rename_map, '->', names(rename_map), collapse = ', ')}")
      out <- out |> dplyr::rename(dplyr::any_of(rename_map))
    }
  } else out <- out |> dplyr::rename(...)
  out
}

#' @export
rename_changes.stocnet <- function(.data, ...){
  out <- .data
  out$changes <- rename_changes(out$changes, ...)
  out
}

#' @rdname manip_changes
#' @template param_time
#' @examples
#' gather_changes(fict_starwars, time = 3)
#' @export
gather_changes <- function(.data, time) UseMethod("gather_changes")

#' @export
gather_changes.default <- function(.data, time){
  gather_changes(as_igraph(.data), time = time)
}

#' @export
gather_changes.igraph <- function(.data, time){
  t <- time
  changes <- igraph::graph_attr(.data, "changes")
  changes <- changes |> 
    dplyr::filter(time <= t) |> 
    dplyr::arrange(node, var, time) |> 
    dplyr::group_by(node, var) |> 
    dplyr::mutate(value = dplyr::last(value)) |> 
    dplyr::distinct(node, var, value)
  changes
}

#' @rdname manip_changes
#' @examples
#' collect_changes(fict_starwars, time = 3)
#' @export
apply_changes <- function(.data, time) UseMethod("apply_changes")

#' @export
apply_changes.default <- function(.data, time){
  as_input(.data, apply_changes, time = time)
}

#' @export
apply_changes.tbl_graph <- function(.data, time){
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
