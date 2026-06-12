# Manipulating ties number ####

#' Manipulating number of ties
#' @name manip_ties_num
#' @description 
#'   These functions allow users to add and delete ties:
#'   
#'   - `add_ties()` adds additional ties to network data
#'   - `delete_ties()` deletes ties from network data
#'   - `bind_ties()` appends the tie data from two networks
#'   - `filter_ties()` subsets ties based on some tie attribute-related logical statement.
#'   
#'   While `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `filter*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @template param_data
#' @template param_dots
#' @family ties
#' @template fam_manip
#' @eval detail_avail("(add|del|bind|filter).*ties")
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   mutate_ties(other, form = 1:6) |> filter_ties(form < 4)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
NULL

#' @rdname manip_ties_num
#' @param ties The number of ties to be added or an even list of ties.
#' @param attr_list A list of attributes to be added to the new ties.
#' @importFrom igraph add_edges
#' @examples
#' ison_adolescents |> add_ties(c("Betty","Tina"))
#' @export
add_ties <- function(.data, ties, attr_list = NULL) UseMethod("add_ties")

#' @export
add_ties.default <- function(.data, ties, attr_list = NULL){
  as_input(.data, add_ties, ties = ties, attr_list = attr_list)
}

#' @export
add_ties.igraph <- function(.data, ties, attr_list = NULL){
  igraph::add_edges(.data, edges = ties, attr = attr_list)
}

#' @export
add_ties.tbl_graph <- function(.data, ties, attr_list = NULL){
  as_tidygraph(add_ties(as_igraph(.data), ties, attr_list))
}

#' @export
add_ties.network <- function(.data, ties, attr_list = NULL){
  as_network(add_ties(as_igraph(.data), ties, attr_list))
}

#' @rdname manip_ties_num
#' @importFrom igraph delete_edges
#' @examples
#' delete_ties(ison_adolescents, 3)
#' delete_ties(ison_adolescents, "Alice|Sue")
#' @export
delete_ties <- function(.data, ties) UseMethod("delete_ties")

#' @export
delete_ties.default <- function(.data, ties){
  as_input(.data, delete_ties, ties = ties)
}

#' @export
delete_ties.igraph <- function(.data, ties){
  igraph::delete_edges(.data, edges = ties)
}

#' @export
delete_ties.tbl_graph <- function(.data, ties){
  as_tidygraph(igraph::delete_edges(.data, edges = ties))
}

#' @export
delete_ties.network <- function(.data, ties){
  as_network(igraph::delete_edges(as_igraph(.data), edges = ties))
}

#' @rdname manip_ties_num
#' @importFrom tidygraph bind_edges
#' @export
bind_ties <- function(.data, ...) UseMethod("bind_ties")

#' @export
bind_ties.default <- function(.data, ...){
  as_input(.data, bind_ties, ...)
}

#' @export
bind_ties.tbl_graph <- function(.data, ...){
  toAdd <- as_edgelist(...)
  tidygraph::bind_edges(.data, toAdd) |> 
    arrange_ties(from, to)
}

#' @rdname manip_ties_num 
#' @importFrom dplyr filter
#' @export
filter_ties <- function(.data, ...) UseMethod("filter_ties")

#' @export
filter_ties.default <- function(.data, ...){
  as_input(.data, filter_ties, ...)
}

#' @export
filter_ties.tbl_graph <- function(.data, ...){
  .data |> tidygraph::activate(edges) |> 
    dplyr::filter(...) |> 
    tidygraph::activate(nodes)
}

# Manipulating ties attributes ####

#' Manipulating tie attributes
#' @name manip_ties_attr
#' @description 
#'   These functions allow users to add and delete tie attributes:
#'   
#'   - `add_tie_attribute()` and `mutate_ties()` offer ways to add 
#'   a vector of values to a network as a tie attribute.
#'   - `rename_ties()` renames tie attributes.
#'   - `join_ties()` merges ties from two networks,
#'   adding a tie attribute identifying the newly added ties.
#'   
#'   Note that while `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `mutate*()`, `bind*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @template param_data
#' @template param_dots
#' @template param_attr
#' @template param_vect
#' @template param_obj2
#' @family ties
#' @template fam_manip
#' @eval detail_avail("add_tie_attrib|(mutate|rename|arrange|join|select|summarise).*ties")
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   mutate_ties(other, form = 1:6) |> filter_ties(form < 4)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
NULL

#' @rdname manip_ties_attr
#' @importFrom igraph edge_attr delete_edge_attr set_edge_attr
#' @export
add_tie_attribute <- function(.data, attr_name, vector) UseMethod("add_tie_attribute")

#' @export
add_tie_attribute.default <- function(.data, attr_name, vector){
  as_input(.data, add_tie_attribute, attr_name = attr_name, vector = vector)
}

#' @export
add_tie_attribute.igraph <- function(.data, attr_name, vector){
  out <- .data
  igraph::edge_attr(out, name = attr_name) <- vector
  out
}

#' @export
add_tie_attribute.data.frame <- function(.data, attr_name, vector){
  is_edgelist(.data) || snet_abort("Not an edgelist")
  dplyr::mutate(.data, !!!stats::setNames(list(vector), attr_name))
}

#' @rdname manip_ties_attr
#' @importFrom tidygraph activate
#' @export
mutate_ties <- function(.data, ...) UseMethod("mutate_ties")

#' @export
mutate_ties.default <- function(.data, ...){
  as_input(.data, mutate_ties, ...)
}

#' @export
mutate_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> mutate(...) |> activate(nodes)
}

#' @export
mutate_ties.stocnet <- function(.data, ...){
  out <- .data
  out$ties <- out$ties |> 
    dplyr::mutate(...)
  out
}

#' @rdname manip_ties_attr
#' @importFrom dplyr rename
#' @export
rename_ties <- function(.data, ...) UseMethod("rename_ties")

#' @export
rename_ties.default <- function(.data, ...){
  as_input(.data, rename_ties, ...)
}

#' @export
rename_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::rename(...) |> activate(nodes)
}

#' @rdname manip_ties_attr
#' @importFrom dplyr arrange
#' @export
arrange_ties <- function(.data, ...) UseMethod("arrange_ties")

#' @export
arrange_ties.default <- function(.data, ...){
  as_input(.data, arrange_ties, ...)
}

#' @export
arrange_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::arrange(...) |> activate(nodes)
}

#' @export
arrange_ties.stocnet <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    default_cols <- intersect(c("from", "to", "by", "time"), names(out$ties))
    out$ties <- out$ties |> dplyr::arrange(dplyr::across(dplyr::all_of(default_cols)))
  } else {
    out$ties <- out$ties |> dplyr::arrange(...)
  }
  out
}

#' @rdname manip_ties_attr 
#' @importFrom igraph add_edges set_edge_attr E
#' @importFrom dplyr mutate summarise across group_by everything ungroup
#' @export
join_ties <- function(.data, object2, attr_name) {
  el <- c(t(as.matrix(as_edgelist(object2))))
  obj <- as_tidygraph(.data) |> 
    tidygraph::activate(edges)
  if(ncol(as.data.frame(obj)) < 3){
    obj <- obj |> igraph::set_edge_attr("orig", value = 1)
  } 
  out <- igraph::add_edges(as_igraph(obj),
                           el, object2 = 1) |> 
    as_tidygraph()
  if(!missing(attr_name)){
    out <- igraph::set_edge_attr(out, attr_name,
                                 value = igraph::E(out)$object2) |>
      select_ties(-object2)
  }
  edges <- out |>
    tidygraph::activate(edges) |>
    as.data.frame() |> 
    dplyr::group_by(from, to) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), 
                                   function(x){
                                     out <- suppressWarnings(max(x, na.rm = TRUE))
                                     if(is.infinite(out)){
                                       if(is.numeric(out)) out <- 0 else 
                                         out <- NA
                                     }
                                     out
                                   }), 
                     .groups = "keep") |> dplyr::ungroup()
  nodes <- out |> tidygraph::activate(nodes) |> as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(.data))
}

#' @rdname manip_ties_attr
#' @importFrom dplyr select
#' @export
select_ties <- function(.data, ...) UseMethod("select_ties")

#' @export
select_ties.default <- function(.data, ...){
  as_input(.data, select_ties, ...)
}

#' @export
select_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::select(...) |> activate(nodes)
}

#' @rdname manip_ties_attr
#' @importFrom dplyr summarise
#' @export
summarise_ties <- function(.data, ...) UseMethod("summarise_ties")

#' @export
summarise_ties.default <- function(.data, ...){
  as_input(.data, summarise_ties, ...)
}

#' @export
summarise_ties.tbl_graph <- function(.data, ...){
  out <- as_edgelist(.data) |> 
    dplyr::summarise(..., .by = c("from","to")) |> 
    as_tidygraph(twomode = is_twomode(.data))
  out <- as_tidygraph(bind_node_attributes(out, .data))
  if(!is_directed(.data)) out <- to_undirected(out)
  out
}

