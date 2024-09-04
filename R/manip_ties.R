#' Modifying tie data
#' 
#' @description 
#'   These functions allow users to add and delete ties and their attributes:
#'   
#'   - `add_ties()` adds additional ties to network data
#'   - `delete_ties()` deletes ties from network data
#'   - `add_tie_attribute()` and `mutate_ties()` offer ways to add 
#'   a vector of values to a network as a tie attribute.
#'   - `rename_ties()` renames tie attributes.
#'   - `bind_ties()` appends the tie data from two networks and 
#'   `join_ties()` merges ties from two networks,
#'   adding a tie attribute identifying the newly added ties.
#'   - `filter_ties()` subsets ties based on some tie attribute-related logical statement.
#'   
#'   Note that while `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `mutate*()`, `bind*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @family modifications
#' @inheritParams add_nodes
#' @param attr_name Name of the new attribute in the resulting object.
#' @return A tidygraph (`tbl_graph`) data object.
#' @examples
#'   other <- create_filled(4) %>% mutate(name = c("A", "B", "C", "D"))
#'   mutate_ties(other, form = 1:6) %>% filter_ties(form < 4)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
#' @name manip_ties
NULL

#' @rdname manip_ties
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

#' @rdname manip_ties
#' @importFrom igraph delete_edges
#' @examples
#' delete_ties(ison_adolescents, 3)
#' delete_ties(ison_adolescents, "Alice|Sue")
#' @export
delete_ties <- function(.data, ties) UseMethod("delete_ties")

#' @export
delete_ties.igraph <- function(.data, ties){
  igraph::delete_edges(.data, edges = ties)
}

#' @rdname manip_ties
#' @importFrom igraph edge_attr
#' @export
add_tie_attribute <- function(.data, attr_name, vector){
  out <- as_igraph(.data)
  igraph::edge_attr(out, name = attr_name) <- vector
  if(inherits(.data, "tbl_graph")) as_tidygraph(out) else
    if(inherits(.data, "igraph")) as_igraph(out) else
      if(inherits(.data, "igraph")) as_network(out) else
        if(inherits(.data, "data.frame")) as_edgelist(out) else
          message(paste("This function only works for",
                        "igraph, tidygraph, or network objects or data frame edgelists."))
}

#' @rdname manip_ties
#' @importFrom tidygraph activate
#' @export
mutate_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% mutate(...) %>% activate(nodes)
}

#' @rdname manip_ties
#' @importFrom dplyr rename
#' @export
rename_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::rename(...) %>% activate(nodes)
}

#' @rdname manip_ties
#' @importFrom dplyr arrange
#' @export
arrange_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::arrange(...) %>% activate(nodes)
}

#' @rdname manip_ties
#' @importFrom tidygraph bind_edges
#' @export
bind_ties <- function(.data, ...){
  tidygraph::bind_edges(.data, ...)
}

#' @rdname manip_ties 
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
                                     if(is.infinite(out)){
                                       if(is.numeric(out)) out <- 0 else 
                                         out <- NA
                                     }
                                     out
                                   }), 
                     .groups = "keep") %>% dplyr::ungroup()
  nodes <- out %>% tidygraph::activate(nodes) %>% as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(.data))
}

#' @rdname manip_ties 
#' @importFrom dplyr filter
#' @export
filter_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% 
    dplyr::filter(...) %>% 
    tidygraph::activate(nodes)
}

#' @rdname manip_ties
#' @importFrom dplyr select
#' @export
select_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% tidygraph::activate(edges) %>% dplyr::select(...) %>% activate(nodes)
}

#' @rdname manip_ties
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

