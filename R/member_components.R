#' Component partitioning algorithms
#' 
#' @description 
#'   These functions create a vector of nodes' memberships in components:
#'   
#'   - `node_in_component()` assigns nodes' component membership
#'   using edge direction where available.
#'   - `node_in_weak()` assigns nodes' component membership
#'   ignoring edge direction.
#'   - `node_in_strong()` assigns nodes' component membership
#'   based on edge direction.
#'   
#'   In graph theory, components, sometimes called connected components, 
#'   are induced subgraphs from partitioning the nodes into disjoint sets.
#'   All nodes that are members of the same partition as _i_ are reachable
#'   from _i_.
#'   
#'   For directed networks, 
#'   strongly connected components consist of subgraphs where there are paths
#'   in each direction between member nodes.
#'   Weakly connected components consist of subgraphs where there is a path
#'   in either direction between member nodes.
#'   
#' @inheritParams mark_is
#' @name member_components
#' @family memberships
NULL

#' @rdname member_components 
#' @importFrom igraph components
#' @examples
#' ison_monks %>% to_uniplex("esteem") %>%
#'   mutate_nodes(comp = node_in_component())
#' @export
node_in_component <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(!is_graph(.data)) .data <- as_igraph(.data) # nocov
  make_node_member(igraph::components(.data, mode = "strong")$membership,
              .data)
}

#' @rdname member_components 
#' @importFrom igraph components
#' @export
node_in_weak <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data) # nocov
  make_node_member(igraph::components(.data, mode = "weak")$membership,
                 .data)
}

#' @rdname member_components 
#' @importFrom igraph components
#' @export
node_in_strong <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data) # nocov
  make_node_member(igraph::components(.data, mode = "strong")$membership,
                 .data)
}



