#' Tidy modifications of node or tie data
#' 
#' @description 
#'   These functions allow users to add nodes, ties, or attributes to the nodes or ties
#'   of a network.
#'   The `join_*()`, `mutate_*()`, `select_*()`, `filter_*()`, `rename_*()`, and `summarise_*()`
#'   functions adapt the `{dplyr}`-type syntax to work with networks of any type.
#' @family modifications
#' @inheritParams is
#' @param object2 A second object to copy nodes or edges from.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param ... Additional arguments.
#' @return A tidygraph (`tbl_graph`) data object.
#' @examples
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   another <- create_filled(3) %>% mutate(name = c("E", "F", "G"))
#'   join_nodes(another, other)
#'   mutate_ties(other, form = 1:6) %>% filter_ties(form < 4)
#' @name tidy
NULL

#' @describeIn tidy Copies node attributes from a given graph into specified graph
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

#' @describeIn tidy Copies ties from another graph to specified graph and 
#' adds a tie attribute identifying the ties that were newly added
#' @importFrom igraph add_edges set_edge_attr E
#' @importFrom dplyr mutate summarise across group_by everything ungroup %>%
#' @export
join_ties <- function(.data, object2, attr_name) {
  edges <- from <- to <- NULL
  el <- c(t(as.matrix(as_edgelist(object2))))
  obj <- as_tidygraph(.data) %>% 
    tidygraph::activate(edges)
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
    tidygraph::activate(edges) %>%
    as.data.frame() %>% 
    dplyr::group_by(from, to) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), 
                                   function(x){
                                     out <- suppressWarnings(max(x, na.rm = TRUE))
                                     if(is.infinite(out)) out <- 0
                                     out
                                   }), 
                     .groups = "keep") %>% dplyr::ungroup()
  nodes <- out %>% tidygraph::activate(nodes) %>% as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(.data))
}

#' @export
mutate.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    mutate(...) %>% as_igraph()
}

#' @describeIn tidy Tidy way to add vector as tie attributes.
#' @importFrom tidygraph activate
#' @export
mutate_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% mutate(...) %>% activate(nodes)
}

#' @describeIn tidy Tidy way to select tie attributes.
#' @importFrom dplyr select
#' @export
select_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::select(...) %>% activate(nodes)
}

#' @describeIn tidy Tidy way to filter ties based on a logical statement with
#'   relation to some tie attribute.
#' @importFrom dplyr filter
#' @export
filter_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::filter(...) %>% activate(nodes)
}

#' @describeIn tidy Tidy way to rename tie attributes.
#' @importFrom dplyr rename
#' @export
rename_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::rename(...) %>% activate(nodes)
}

#' @describeIn tidy Tidy way to summarise tie attributes.
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

#' @describeIn tidy Copying all nodal attributes from one network to another
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
