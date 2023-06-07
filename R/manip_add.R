#' Adding and copying attributes from one network to another
#' 
#' @description 
#'   These functions allow users to add nodes, ties, or attributes to the nodes or ties
#'   of a network.
#'   The `join_*()`, `mutate_*()`, `select_*()`, `filter_*()`, `rename_*()`, and `summarise_*()`
#'   functions adapt the `{dplyr}`-type syntax to work with networks of any type.
#'   The `add_*()` functions operate similarly to in `{igraph}`.
#' @family manipulations
#' @inheritParams is
#' @param object2 A second object to copy nodes or edges from.
#' @param attribute A named list to be added as tie or node attributes.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @param ... Additional arguments.
#' @examples
#' \donttest{
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   another <- create_filled(3) %>% mutate(name = c("E", "F", "G"))
#'   other2 <- other %>% mutate_ties(type = c("a"))
#'   join_nodes(another, other)
#'   add_nodes(other, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
#'   add_ties(other, c(1,2), list(time = 2, increment = -1))
#'   add_node_attribute(other, "wealth", 1:4)
#'   bind_node_attributes(other, other2)
#'   summarise_ties(other2, type)
#'   rename_ties(other2, form = type)
#'   mutate_ties(other, form = 1:6) %>% filter_ties(form < 4)
#'   select_ties(other2, type)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
#' }
#' @name add
NULL

#' @describeIn add Copies node attributes from a given graph into specified graph
#' @param join_type A type of join to be used.
#' Options are "full","left", "right", "inner".
#' @param by An attribute name to join objects by.
#' By default, NULL.
#' @export
join_nodes <- function(.data, object2, by = NULL,
                       join_type = c("full","left", "right", "inner")){
  join_type <- match.arg(join_type)
  out <- as_tidygraph(.data)
  object2 <- as_tidygraph(object2)
  switch(join_type,
         "full" = dplyr::full_join(out, object2, by = by, copy = TRUE),
         "left" = dplyr::left_join(out, object2, by = by, copy = TRUE),
         "right" = dplyr::right_join(out, object2, by = by, copy = TRUE),
         "inner" = dplyr::inner_join(out, object2, by = by, copy = TRUE))
}

#' @describeIn add Copies ties from another graph to specified graph and 
#' adds a tie attribute identifying the ties that were newly added
#' @importFrom igraph add_edges set_edge_attr E
#' @importFrom dplyr mutate summarise across group_by everything ungroup %>%
#' @export
join_ties <- function(.data, object2, attr_name) {
  edges <- from <- to <- NULL
  el <- c(t(as.matrix(as_edgelist(object2))))
  obj <- as_tidygraph(.data) %>% 
    activate(edges)
  if(ncol(as.data.frame(obj)) < 3){
    obj <- obj %>% igraph::set_edge_attr("orig", value = 1)
  } 
  out <- igraph::add_edges(as_igraph(obj),
                           el, object2 = 1) %>% 
    as_tidygraph()
  if(!missing(attr_name)){
    out <- igraph::set_edge_attr(out, attr_name,
                                 value = igraph::E(out)$object2) %>%
      select_ties(-object2)
  }
  edges <- out %>%
    activate(edges) %>%
    as.data.frame() %>% 
    dplyr::group_by(from, to) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), 
                            function(x){
                              out <- suppressWarnings(max(x, na.rm = TRUE))
                              if(is.infinite(out)) out <- 0
                              out
                            }), 
                     .groups = "keep") %>% dplyr::ungroup()
  nodes <- out %>% activate(nodes) %>% as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(.data))
}

#' @describeIn add Add additional ties to a network
#' @param nodes The number of nodes to be added.
#' @importFrom igraph add_edges
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

#' @describeIn add Add additional ties to a network
#' @param ties The number of ties to be added or an even list of ties.
#' @importFrom igraph add_edges
#' @export
add_ties <- function(.data, ties, attribute = NULL) UseMethod("add_ties")

#' @export
add_ties.igraph <- function(.data, ties, attribute = NULL){
  igraph::add_edges(.data, edges = ties, attr = attribute)
}

#' @export
add_ties.tbl_graph <- function(.data, ties, attribute = NULL){
  as_tidygraph(add_ties(as_igraph(.data), ties, attribute))
}

#' @export
add_ties.network <- function(.data, ties, attribute = NULL){
  as_network(add_ties(as_igraph(.data), ties, attribute))
}

#' @describeIn add Insert specified values from a vector into the graph 
#' as node attributes
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
      stop("Attribute vector must be same length as nodes in object.")
  }
  out <- as_igraph(.data)
  igraph::vertex_attr(out, name = attr_name) <- vector
  as_tidygraph(out)
}

#' @describeIn add Insert specified values from a vector into the network.
#'   as tie attributes
#' @importFrom igraph edge_attr
#' @export
add_tie_attribute <- function(.data, attr_name, vector){
  out <- as_igraph(.data)
  igraph::edge_attr(out, name = attr_name) <- vector
  as_tidygraph(out)
}

#' @export
mutate.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    mutate(...) %>% as_igraph()
}

#' @describeIn add Tidy way to add vector as tie attributes.
#' @export
mutate_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% mutate(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to select tie attributes.
#' @importFrom dplyr select
#' @export
select_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::select(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to filter ties based on a logical statement with
#'   relation to some tie attribute.
#' @importFrom dplyr filter
#' @export
filter_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::filter(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to rename tie attributes.
#' @importFrom dplyr rename
#' @export
rename_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::rename(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to summarise tie attributes.
#' @importFrom dplyr summarise
#' @export
summarise_ties <- function(.data, ...){
  out <- as_edgelist(.data) %>% 
    dplyr::summarise(..., .by = c("from","to")) %>% 
    as_tidygraph(twomode = is_twomode(.data))
  out <- as_tidygraph(bind_node_attributes(out, .data))
  if(!is_directed(.data)) out <- to_undirected(out)
  out
}

#' @describeIn add Copying all nodal attributes from one network to another
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
                                        value = igraph::get.vertex.attribute(object2, a))
  }
  as_tidygraph(out)
}
