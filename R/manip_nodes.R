# Manipulating nodes number ####

#' Manipulating number of nodes
#' @name manip_nodes_num
#' @description 
#'   These functions allow users to add and delete nodes:
#'   
#'   - `add_nodes()` adds an additional number of nodes to network data.
#'   - `delete_nodes()` deletes nodes from network data.
#'   - `bind_nodes()` adds two nodesets together.
#'   - `filter_nodes()` subsets nodes based on some nodal attribute-related logical statement.
#'   - `arrange_nodes()` reorders nodes based on some nodal attribute.
#'   
#'   While `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `bind_*()` and `filter_*()` works like a `{tidyverse}` or `{dplyr}`-style function.
#' @eval detail_avail("(add|delete|bind|filter|arrange)_nodes")
#' @template param_data
#' @template param_dots
#' @template param_by
#' @template param_obj2
#' @family nodes
#' @template fam_manip
#' @param attribute A named list to be added as tie or node attributes.
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   add_nodes(other, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
NULL

#' @rdname manip_nodes_num
#' @param nodes The number of nodes to be added.
#' @importFrom igraph add_vertices
#' @export
add_nodes <- function(.data, nodes, attribute = NULL) UseMethod("add_nodes")

#' @export
add_nodes.default <- function(.data, nodes, attribute = NULL){
  as_input(.data, add_nodes, nodes = nodes, attribute = attribute)
}

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

#' @rdname manip_nodes_num
#' @importFrom igraph delete_vertices
#' @export
delete_nodes <- function(.data, nodes) UseMethod("delete_nodes")

#' @export
delete_nodes.default <- function(.data, nodes){
  as_input(.data, delete_nodes, nodes = nodes)
}

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

#' @rdname manip_nodes_num
#' @export
bind_nodes <- function(.data, object2) UseMethod("bind_nodes")

#' @export
bind_nodes.default <- function(.data, object2){
  as_input(.data, bind_nodes, object2 = object2)
}

#' @export
bind_nodes.stocnet <- function(.data, object2){
  out <- .data
  out$nodes <- dplyr::bind_rows(.data$nodes, object2$nodes)
  out
}

#' @rdname manip_nodes_num
#' @importFrom tidygraph filter
#' @export
filter_nodes <- function(.data, ..., .by = NULL) UseMethod("filter_nodes")

#' @export
filter_nodes.default <- function(.data, ..., .by = NULL){
  as_input(.data, filter_nodes, ..., .by = .by)
}

#' @export
filter_nodes.tbl_graph <- function(.data, ..., .by = NULL){
  tidygraph::filter(.data, ..., .by = dplyr::all_of(.by))
}

#' @export
filter_nodes.stocnet <- function(.data, ..., .by = NULL){
  with_active_context(.data, "nodes", {
    if(is.null(.data$nodes) || nrow(.data$nodes) == 0) return(.data)

  node_df <- dplyr::mutate(.data$nodes, .orig_id = dplyr::row_number())
  kept_nodes <- dplyr::filter(node_df, ..., .by = dplyr::all_of(.by))
  kept <- kept_nodes$.orig_id
  out_nodes <- dplyr::select(kept_nodes, -.orig_id)

  if(!is.null(.data$ties) && nrow(.data$ties) > 0){
    out_ties <- dplyr::filter(.data$ties, from %in% kept, to %in% kept) |>
      dplyr::mutate(from = match(from, kept),
                    to = match(to, kept))
  } else {
    out_ties <- .data$ties
  }

  if(!is.null(.data$changes) && nrow(.data$changes) > 0){
    out_changes <- dplyr::filter(.data$changes, node %in% kept) |>
      dplyr::mutate(node = match(node, kept))
  } else {
    out_changes <- .data$changes
  }

  make_stocnet(nodes = out_nodes, ties = out_ties, changes = out_changes,
              global = .data$global, info = .data$info)
  })
}

#' @rdname manip_nodes_num
#' @importFrom dplyr arrange
#' @export
arrange_nodes <- function(.data, ...) UseMethod("arrange_nodes")

#' @export
arrange_nodes.default <- function(.data, ...){
  as_input(.data, FUN = arrange_nodes, ...)
}

#' @export
arrange_nodes.tbl_graph <- function(.data, ...){
  .data |> tidygraph::activate(nodes) |> dplyr::arrange(...)
}

#' @export
arrange_nodes.stocnet <- function(.data, ...){
  nodes <- .data$nodes
  node_df <- dplyr::mutate(nodes, .orig_row = dplyr::row_number())
  arranged <- dplyr::arrange(node_df, ...)
  old_to_new <- integer(nrow(nodes))
  old_to_new[arranged$.orig_row] <- seq_len(nrow(nodes))
  
  out_nodes <- dplyr::select(arranged, -.orig_row)
  
  if(!is.null(.data$ties) && nrow(.data$ties) > 0){
    out_ties <- .data$ties |> 
      dplyr::mutate(from = old_to_new[from],
                    to = old_to_new[to])
  } else {
    out_ties <- .data$ties
  }
  
  if(!is.null(.data$changes) && nrow(.data$changes) > 0){
    out_changes <- .data$changes |> 
      dplyr::mutate(node = old_to_new[node])
  } else {
    out_changes <- .data$changes
  }
  
  make_stocnet(nodes = out_nodes, ties = out_ties, 
               changes = out_changes, info = .data$info)
}

# Manipulating nodes attributes ####

#' Manipulating node attributes
#' @name manip_nodes_attr
#' @description 
#'   These functions allow users to add nodes attributes:
#'   
#'   - `add_node_attribute()` offers an `{igraph}`-style way to add a vector of values to a network as a nodal attribute.
#'   - `mutate_nodes()` offers a `{tidyverse}`-style way to add one or more vectors of values to a network as nodal attributes.
#'   - `rename_nodes()` offers a `{tidyverse}`-style way to rename nodal attributes.
#'   - `select_nodes()` offers a `{tidyverse}`-style way to select a subset of nodal attributes.
#'   - `join_nodes()` merges all nodal attributes from one network to another.
#'   
#'   Note that while `add_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `mutate*()`, `bind*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @eval detail_avail("add_node_attribute|mutate_nodes|select_nodes|join_nodes")
#' @template param_data
#' @template param_dots
#' @template param_attr
#' @template param_vect
#' @template param_obj2
#' @family nodes
#' @template fam_manip
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   add_nodes(other, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
NULL

#' @rdname manip_nodes_attr 
#' @importFrom igraph V vertex_attr delete_vertex_attr 
#' @export
add_node_attribute <- function(.data, attr_name, vector) UseMethod("add_node_attribute")

#' @export
add_node_attribute.default <- function(.data, attr_name, vector){
  as_input(.data, add_node_attribute, attr_name = attr_name, vector = vector)
}

#' @export
add_node_attribute.igraph <- function(.data, attr_name, vector){
  if(length(vector)!=igraph::vcount(.data)){
    if(is_twomode(.data) && any(length(vector) == infer_dims(.data))){
      if(length(vector) == infer_dims(.data)[1]){
        vector <- c(vector, rep(NA, infer_dims(.data)[2]))
      } else if (length(vector) == infer_dims(.data)[2]){
        vector <- c(rep(NA, infer_dims(.data)[1]), vector)
      }
    } else 
      snet_abort("Attribute vector must be same length as nodes in object.")
  }
  out <- .data
  igraph::vertex_attr(out, name = attr_name) <- vector
  out
}

#' @export
add_node_attribute.network <- function(.data, attr_name, vector){
  network::set.vertex.attribute(x = .data, attrname = attr_name, value = vector)
}

#' @export
add_node_attribute.tbl_graph <- function(.data, attr_name, vector){
  as_igraph(.data) |> add_node_attribute(attr_name, vector) |> as_tidygraph()
}

#' @rdname manip_nodes_attr
#' @importFrom tidygraph mutate
#' @export
mutate <- tidygraph::mutate

#' @rdname manip_nodes_attr
#' @importFrom tidygraph mutate
#' @export
mutate_nodes <- function(.data, ...) UseMethod("mutate_nodes")

#' @export
mutate_nodes.default <- function(.data, ...){
  as_input(.data, FUN = mutate_nodes, ...)
}

#' @export
mutate_nodes.tbl_graph <- function(.data, ...){
  .data |> tidygraph::mutate(...)
}

#' @export
mutate_nodes.igraph <- function(.data, ...){
  .data |> as_tidygraph() |> 
    tidygraph::mutate(...) |> as_igraph()
}

#' @export
mutate_nodes.network <- function(.data, ...){
  .data |> as_tidygraph() |> 
    tidygraph::mutate(...) |> as_network()
}

#' @export
mutate_nodes.stocnet <- function(.data, ...){
  with_active_context(.data, "nodes", {
    if (is.null(.data$nodes) || nrow(.data$nodes) == 0) return(.data)
    out <- .data
    out$nodes <- out$nodes |> 
      dplyr::mutate(...)
    out
  })
}

#' @rdname manip_nodes_attr
#' @export
rename_nodes <- function(.data, ...) UseMethod("rename_nodes")

#' @export
rename_nodes.default <- function(.data, ...){
  as_input(.data, FUN = rename_nodes, ...)
}

#' @importFrom tidygraph rename
#' @export
rename_nodes.tbl_graph <- function(.data, ...){
  tidygraph::rename(.data, ...)
}

#' @export
rename_nodes.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    aka <- list(
      label   = c("name","id"),
      active  = c("present","presence"),
      mode    = c("type","class","category")
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
      snet_minor_info("Renaming node attributes to stocnet conventions:",
                      "{paste(rename_map, '->', names(rename_map), collapse = ', ')}")
      out <- out |> dplyr::rename(dplyr::any_of(rename_map))
    }
  } else out <- out |> dplyr::rename(...)
  out
}

#' @export
rename_nodes.stocnet <- function(.data, ...){
  out <- .data
  out$nodes <- rename_nodes.data.frame(out$nodes, ...)
  out
}

#' @rdname manip_nodes_attr
#' @importFrom tidygraph mutate
#' @export
select_nodes <- function(.data, ...) UseMethod("select_nodes")

#' @export
select_nodes.default <- function(.data, ...){
  as_input(.data, FUN = select_nodes, ...)
}

#' @export
select_nodes.tbl_graph <- function(.data, ...){
  .data |> tidygraph::select(...)
}

#' @export
select_nodes.igraph <- function(.data, ...){
  .data |> as_tidygraph() |> 
    tidygraph::select(...) |> as_igraph()
}

#' @export
select_nodes.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    out <- dplyr::select(out, dplyr::any_of(c("label","active")),
                         dplyr::everything())
  } else out <- dplyr::select(out, ...)
  out
}

#' @export
select_nodes.stocnet <- function(.data, ...){
  out <- .data
  out$nodes <- select_nodes.data.frame(out$nodes, ...)
  out
}

#' @rdname manip_nodes_attr 
#' @template param_join
#' @template param_by
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   another <- create_filled(3) |> mutate(name = c("E", "F", "G"))
#'   join_nodes(another, other)
#' @export
join_nodes <- function(.data, object2, .by = NULL,
                       join_type = c("full","left", "right", "inner")) UseMethod("join_nodes")

#' @export
join_nodes.default <- function(.data, object2, .by = NULL,
                       join_type = c("full","left", "right", "inner")){
  as_input(.data, join_nodes, object2 = object2, .by = .by, join_type = join_type)
}

#' @export
join_nodes.igraph <- function(.data, object2, .by = NULL,
                       join_type = c("full","left", "right", "inner")){
  join_type <- match.arg(join_type)
  out <- as_tidygraph(.data)
  if(is_graph(object2)){
    object2 <- as_tidygraph(object2)
  }
  if(is_labelled(.data) && is_labelled(object2)){
    if(is.null(.by)) .by <- "name"
  } else if(is_labelled(.data) && !is_labelled(object2)){
    snet_unavailable("Joining a labelled and an unlabelled object is not currently supported.")
  } else if(!is_labelled(.data) && is_labelled(object2)){
    snet_unavailable("Joining an unlabelled and a labelled object is not currently supported.")
  } else if(!is_labelled(.data) && !is_labelled(object2)){
    if(is.null(.by)) .by <- "id"
    out <- out |> mutate(id = 1:net_nodes(out))
    object2 <- object2 |> mutate(id = 1:net_nodes(object2))
  }
  out <- switch(join_type,
                "full" = dplyr::full_join(out, object2, by = .by, copy = TRUE),
                "left" = dplyr::left_join(out, object2, by = .by, copy = TRUE),
                "right" = dplyr::right_join(out, object2, by = .by, copy = TRUE),
                "inner" = dplyr::inner_join(out, object2, by = .by, copy = TRUE))
  if(!is_labelled(.data) && !is_labelled(object2)){
    out <- out |> select(-id)
  }
  out
}

#' @export
join_nodes.stocnet <- function(.data, object2, .by = NULL,
                                 join_type = c("full","left", "right", "inner")){
  join_type <- match.arg(join_type)
  out <- .data
  if(inherits(object2, "stocnet")) nodelist <- as_nodelist(object2) else
    nodelist <- object2
  if(is.null(.data$nodes) && net_nodes(.data) == nrow(nodelist)) 
    return(make_stocnet(info = .data$info, nodes = nodelist, ties = .data$ties, 
                        changes = .data$changes, global = .data$global))
  out$nodes <- switch(join_type,
                       "full" = dplyr::full_join(.data$nodes, nodelist, by = .by, copy = TRUE),
                       "left" = dplyr::left_join(.data$nodes, nodelist, by = .by, copy = TRUE),
                       "right" = dplyr::right_join(.data$nodes, nodelist, by = .by, copy = TRUE),
                       "inner" = dplyr::inner_join(.data$nodes, nodelist, by = .by, copy = TRUE))
  out
}

#' @rdname manip_nodes_attr
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

