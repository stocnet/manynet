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
      cli::cli_abort("Attribute vector must be same length as nodes in object.")
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
#' @export
mutate_changes <- function(.data, ...) UseMethod("mutate_changes")

#' @export
mutate_changes.tbl_graph <- function(.data, ...){
  changes <- igraph::graph_attr(.data, "changes")
  changes <- tidygraph::mutate(changes, ...)
  igraph::graph_attr(.data, "changes") <- changes
  .data
}

#' @rdname manip_nodes
#' @importFrom tidygraph mutate
#' @export
mutate <- tidygraph::mutate

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
  object2 <- as_tidygraph(object2)
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
filter_nodes <- function(.data, ..., .by){
  tidygraph::filter(.data, ..., .by = .by)
}

