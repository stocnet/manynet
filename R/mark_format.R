# Node Formats ####

#' Marking networks nodal formats
#' @name mark_format_node
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_twomode()` marks networks TRUE if they contain two sets of nodes.
#'   - `is_multilevel()` marks networks TRUE if they contain two or more levels
#'   of nodes that are tied both within and between levels.
#'   - `is_labelled()` marks networks TRUE if there is a 'names' attribute
#'   for the nodes.
#'   - `is_attributed()` marks networks TRUE if there are other nodal attributes
#'   than 'names' or 'type'.
#'   - `is_egonet()` marks networks TRUE if it is a list of networks where each
#'   network contains only one node and its ties.
#' @template param_data
#' @eval detail_avail("is_(twomode|multilevel|labelled|attributed|egonet)")
#' @family marks
NULL

#' @rdname mark_format_node
#' @importFrom igraph is_bipartite
#' @examples
#' is_twomode(create_filled(c(2,2)))
#' @export
is_twomode <- function(.data) UseMethod("is_twomode")

#' @export
is_twomode.default <- function(.data) {
  is_twomode(as_igraph(.data))
}

#' @export
is_twomode.igraph <- function(.data) {
  igraph::is_bipartite(.data)
}

#' @export
is_twomode.tbl_graph <- function(.data) {
  igraph::is_bipartite(.data)
}

#' @export
is_twomode.matrix <- function(.data) {
  out <- dim(.data)[1] != dim(.data)[2]
  # `isTRUE()` guards against partially labelled matrices,
  # where comparing NA names would otherwise return NA.
  if(!out & is_labelled(.data))
    out <- isTRUE(!all(rownames(.data)==colnames(.data)))
  out
}

#' @export
is_twomode.network <- function(.data) {
  network::is.bipartite(.data)
  # .data <- as_matrix(.data)
  # dim(.data)[1] != dim(.data)[2]
}

#' @export
is_twomode.data.frame <- function(.data) {
  # `[[` rather than `[` because `as_edgelist()` returns a tibble, for which
  # `.data[,1]` is a one-column tibble rather than the column vector.
  is_edgelist(.data) &&
    length(intersect(.data[[1]], .data[[2]])) == 0
}

#' @export
is_twomode.stocnet <- function(.data) {
  if(is.null(.data$nodes)) return(FALSE)
  if(!"mode" %in% names(.data$nodes)) return(FALSE)
  length(unique(.data$nodes$mode)) == 2
}

#' @export
is_twomode.numeric <- function(.data) {
  FALSE
}

#' @export
is_twomode.list <- function(.data) {
  if(is_list(.data)){
    is_twomode(.data[[1]])
  }
}

#' @rdname mark_format_node
#' @details
#'   A multilevel network is one in which the nodes belong to two or more
#'   levels, or nodesets, that are tied not only to each other but also among
#'   themselves. `fict_marvel`, for instance, interlocks a one-mode layer of
#'   ties among its characters with a two-mode layer of affiliations between
#'   those characters and their teams. Such networks are distinguished from
#'   plain two-mode networks, such as `ison_southern_women`, in which ties
#'   run only between the two nodesets and never within them.
#' @examples
#' is_multilevel(fict_marvel)
#' is_multilevel(ison_southern_women)
#' @export
is_multilevel <- function(.data) UseMethod("is_multilevel")

#' @export
is_multilevel.default <- function(.data) {
  is_multilevel(as_igraph(.data))
}

#' @export
is_multilevel.igraph <- function(.data) {
  # `to_multilevel()` records levels in a 'lvl' attribute and deletes 'type',
  # so a network that has already been converted is no longer two-mode and
  # has to be recognised by its levels instead.
  if ("lvl" %in% igraph::vertex_attr_names(.data))
    return(length(unique(igraph::vertex_attr(.data, "lvl"))) > 1)
  if (!is_twomode(.data)) return(FALSE)
  # Levels have to be tied both within and between to interlock: a two-mode
  # network whose ties all run between the modes, as `ison_southern_women`'s
  # do, is not multilevel, and neither is one whose ties all fall within them,
  # since then the modes are two networks rather than two levels of one. A
  # network without any ties is neither, and is returned early because
  # `tie_is_twomode()` cannot name an empty measure.
  if (net_ties(.data) == 0) return(FALSE)
  # `tie_is_twomode()` would name its result through `make_tie_mark()`, which
  # asks `is_directed()`, which asks this function: the same loop that
  # `node_is_mode()` notes. The modes of the two ends of each tie are read
  # directly instead, as `is_multilevel.stocnet()` reads them.
  modes <- igraph::vertex_attr(.data, "type")
  el <- igraph::as_edgelist(.data, names = FALSE)
  between <- modes[el[, 1]] != modes[el[, 2]]
  any(between) && any(!between)
}

#' @export
is_multilevel.tbl_graph <- function(.data) {
  is_multilevel(as_igraph(.data))
}

#' @export
is_multilevel.stocnet <- function(.data) {
  # A 'stocnet' records its levels in the 'mode' variable of its nodes table,
  # to which `as_stocnet()` maps an igraph 'type' or 'lvl' attribute. Unlike
  # either of those, this variable can name more than two levels, so this
  # method marks a three-level network TRUE too.
  if (is.null(.data$nodes) || !"mode" %in% names(.data$nodes)) return(FALSE)
  if (net_modes(.data) < 2) return(FALSE)
  if (is.null(.data$ties) || nrow(.data$ties) == 0) return(FALSE)
  # The ties table holds node indices, so the modes of the two ends of each
  # tie are the modes of the nodes at those rows. Levels have to be tied both
  # within and between to interlock.
  modes <- .data$nodes$mode
  between <- modes[.data$ties$from] != modes[.data$ties$to]
  any(between) && any(!between)
}

#' @export
is_multilevel.list <- function(.data) {
  # A `stocnet` is itself a list, and is dispatched by its own method above;
  # here a list is a list of networks, marked by its first.
  if(is_list(.data)) is_multilevel(.data[[1]]) else FALSE
}

#' @rdname mark_format_node
#' @examples
#' is_hypergraph(create_empty(3))
#' @export
is_hypergraph <- function(.data) UseMethod("is_hypergraph")

#' @export
is_hypergraph.default <- function(.data) {
  is_hypergraph(as_stocnet(.data))
}

#' @export
is_hypergraph.stocnet <- function(.data) {
  is.list(.data$ties$from)
}

#' @rdname mark_format_node
#' @importFrom igraph is_named
#' @examples
#' is_labelled(create_empty(3))
#' @export
is_labelled <- function(.data) UseMethod("is_labelled")

#' @export
is_labelled.default <- function(.data) {
  is_labelled(as_igraph(.data))
}

#' @export
is_labelled.igraph <- function(.data) {
  igraph::is_named(.data)
}

#' @export
is_labelled.tbl_graph <- function(.data) {
  igraph::is_named(.data)
}

#' @export
is_labelled.matrix <- function(.data) {
  any(c(!is.null(dimnames(.data)[[1]]), !is.null(dimnames(.data)[[2]])))
}

#' @export
is_labelled.network <- function(.data) {
  !all(is.na(network::get.vertex.attribute(.data, "vertex.names")))
}

#' @export
is_labelled.data.frame <- function(.data) {
  # `[[` rather than `[` because `as_edgelist()` returns a tibble, for which
  # `.data[,1]` is a one-column tibble rather than the column vector.
  is.character(.data[[1]]) & is.character(.data[[2]])
}

#' @export
is_labelled.stocnet <- function(.data) {
  "name" %in% net_node_attributes(.data) ||
    "label" %in% net_node_attributes(.data)
}

#' @export
is_labelled.list <- function(.data) {
  if(is_list(.data)){
    is_labelled(.data[[1]])
  }
}

#' @rdname mark_format_node
#' @examples
#' is_attributed(ison_algebra)
#' @export
is_attributed <- function(.data) UseMethod("is_attributed")

#' @export
is_attributed.default <- function(.data) {
  length(setdiff(net_node_attributes(.data),
                 manynet_reserved_node_attributes))!=0
}

#' @rdname mark_format_node
#' @examples 
#' is_egonet(fict_starwars)
#' @export
is_egonet <- function(.data) UseMethod("is_egonet")

#' @export
is_egonet.default <- function(.data) {
  if(!is_list(.data)) return(FALSE)
  if(all(unique(names(.data)) != "")) {
    length(names(.data)) == length(unique(unlist(unname(lapply(.data,
                                                               manynet::node_labels))))) &
      all(.order_alphabetically(names(.data)) ==
            .order_alphabetically(unique(unlist(unname(lapply(.data,
                                                              manynet::node_labels))))))
  } else FALSE
}

# Tie Formats ####

#' Marking networks tie formats
#' @name mark_format_tie
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_twomode()` marks networks TRUE if they contain two sets of nodes.
#'   - `is_weighted()` marks networks TRUE if they contain tie weights.
#'   Note that signed networks often hold their signs as weights of -1 and 1,
#'   so that no sign is lost when coercing between formats;
#'   since such a 'weight' records only the sign of each tie,
#'   these networks are marked FALSE unless the weights vary in magnitude.
#'   - `is_directed()` marks networks TRUE if the ties specify which node
#'   is the sender and which the receiver.
#'   - `is_labelled()` marks networks TRUE if there is a 'names' attribute
#'   for the nodes.
#'   - `is_attributed()` marks networks TRUE if there are other nodal attributes
#'   than 'names' or 'type'.
#'   - `is_signed()` marks networks TRUE if the ties can be either positive
#'   or negative.
#'   This is the case where the ties have a 'sign' attribute,
#'   and also where they are weighted and any of those weights are negative.
#'   - `is_complex()` marks networks TRUE if any ties are loops,
#'   with the sender and receiver being the same node.
#'   - `is_multiplex()` marks networks TRUE if it contains multiple types 
#'   of ties, such that there can be multiple ties between the same
#'   sender and receiver.
#'   - `is_uniplex()` marks networks TRUE if it is neither complex nor multiplex.
#' @template param_data
#' @eval detail_avail("is_(weighted|directed|signed|complex|multiplex|uniplex)")
#' @family marks
NULL

#' @rdname mark_format_tie
#' @importFrom igraph is_weighted
#' @examples
#' is_weighted(create_tree(3))
#' @export
is_weighted <- function(.data) UseMethod("is_weighted")

# A signed network's ties are often held compactly as weights of -1 and 1,
# so that coercion from one format to another does not lose the sign.
# Such a 'weight' column records only signs and not weights,
# unless there is a separate 'sign' attribute for it to complement.
.holds_only_signs <- function(wts, has_sign = FALSE){
  !has_sign && !is.null(wts) && length(wts) > 0 &&
    any(wts < 0, na.rm = TRUE) && all(abs(wts) == 1, na.rm = TRUE)
}

# A binary network's ties are also sometimes held as weights of 1,
# so that a tie recorded as missing can be held alongside them as a weight of NA.
# Such a 'weight' column records only which ties are present and not their values,
# so a network holding it is no more weighted than a matrix of zeros and ones is.
.holds_only_binary <- function(wts){
  !is.null(wts) && length(wts) > 0 && all(wts %in% c(0, 1) | is.na(wts))
}

#' @export
is_weighted.default <- function(.data) {
  as_input(.data, is_weighted)
}

#' @export
is_weighted.igraph <- function(.data) {
  igraph::is_weighted(.data) &&
    !.holds_only_binary(igraph::edge_attr(.data, "weight")) &&
    !.holds_only_signs(igraph::edge_attr(.data, "weight"),
                       "sign" %in% igraph::edge_attr_names(.data))
}

#' @export
is_weighted.tbl_graph <- function(.data) {
  is_weighted.igraph(.data)
}

#' @export
is_weighted.stocnet <- function(.data) {
  "weight" %in% names(.data$ties) &&
    !.holds_only_binary(.data$ties$weight) &&
    !.holds_only_signs(.data$ties$weight, "sign" %in% names(.data$ties))
}

#' @export
is_weighted.matrix <- function(.data) {
  !.holds_only_binary(c(.data)) &&
    !.holds_only_signs(c(.data)[which(c(.data) != 0)])
}

#' @export
is_weighted.network <- function(.data) {
  "weight" %in% network::list.edge.attributes(.data) &&
    !.holds_only_binary(unlist(network::get.edge.attribute(.data, "weight"))) &&
    !.holds_only_signs(unlist(network::get.edge.attribute(.data, "weight")),
                       "sign" %in% network::list.edge.attributes(.data))
}

#' @export
is_weighted.data.frame <- function(.data) {
  if(!(ncol(.data)>=3 &&
       ("weight" %in% names(.data) | is.numeric(.data[,3])))) return(FALSE)
  wts <- if("weight" %in% names(.data)) .data[["weight"]] else .data[[3]]
  !.holds_only_signs(wts, "sign" %in% names(.data))
}

#' @rdname mark_format_tie
#' @importFrom igraph is_directed
#' @examples
#' is_directed(create_tree(2))
#' is_directed(create_tree(2, directed = TRUE))
#' @export
is_directed <- function(.data) UseMethod("is_directed")

#' @export
is_directed.default <- function(.data) {
  as_input(.data, is_directed)
}

#' @export
is_directed.data.frame <- function(.data) {
  !(.infer_net_reciprocity(.data) == 0 |
      .infer_net_reciprocity(.data) == 1)
}

# A single bipartite relation runs between the modes and has no direction to
# report, but a multilevel network also ties within a level, and those ties can
# be directed. Such a network is therefore exempt from the two-mode rule, and
# is marked by whatever its underlying object or its info records.
.twomode_undirected <- function(.data) {
  is_twomode(.data) && !is_multilevel(.data)
}

#' @export
is_directed.igraph <- function(.data) {
  if(.twomode_undirected(.data)) FALSE else igraph::is_directed(.data)
}

#' @export
is_directed.stocnet <- function(.data) {
  if(.twomode_undirected(.data)) FALSE else any(.data$info$directed)
}

#' @export
is_directed.tbl_graph <- function(.data) {
  if(.twomode_undirected(.data)) FALSE else igraph::is_directed(.data)
}

#' @export
is_directed.network <- function(.data) {
  .data$gal$directed
}

#' @export
is_directed.matrix <- function(.data) {
  if(is_twomode(.data)) FALSE else !isSymmetric(.data)
}

#' @rdname mark_format_tie
#' @importFrom igraph edge_attr_names
#' @examples
#' is_signed(create_lattice(3))
#' @export
is_signed <- function(.data) UseMethod("is_signed")

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  
  abs(x - round(x)) < tol

#' @export
is_signed.default <- function(.data) {
  is_signed(as_igraph(.data))
}

#' @export
is_signed.data.frame <- function(.data) {
  if(ncol(.data) <= 2) FALSE else 
    any(.data[,3] < 0)
}

#' @export
is_signed.matrix <- function(.data) {
  all(is.wholenumber(c(.data))) && any(.data < 0)
}

#' @export
is_signed.igraph <- function(.data) {
  if("sign" %in% igraph::edge_attr_names(.data)) return(TRUE)
  # a signed network can also be held as negative weights, as it is in
  # 'stocnet' objects, so that coercion from one does not lose the sign
  "weight" %in% igraph::edge_attr_names(.data) &&
    any(igraph::edge_attr(.data, "weight") < 0, na.rm = TRUE)
}

#' @export
is_signed.stocnet <- function(.data) {
  if("sign" %in% net_tie_attributes(.data)) return(TRUE)
  "weight" %in% net_tie_attributes(.data) &&
    any(.data$ties$weight < 0, na.rm = TRUE)
}

#' @export
is_signed.tbl_graph <- function(.data) {
  is_signed.igraph(.data)
}

#' @export
is_signed.network <- function(.data) {
  if("sign" %in% network::list.edge.attributes(.data)) return(TRUE)
  "weight" %in% network::list.edge.attributes(.data) &&
    any(unlist(network::get.edge.attribute(.data, "weight")) < 0, na.rm = TRUE)
}

#' @rdname mark_format_tie
#' @importFrom igraph any_loop
#' @examples
#' is_complex(create_lattice(4))
#' @export
is_complex <- function(.data) UseMethod("is_complex")

#' @export
is_complex.default <- function(.data) {
  is_complex(as_igraph(.data))
}

#' @export
is_complex.igraph <- function(.data) {
  igraph::any_loop(.data)
}

#' @export
is_complex.tbl_graph <- function(.data) {
  igraph::any_loop(.data)
}

#' @export
is_complex.matrix <- function(.data) {
  !(is_twomode(.data) || all(is.na(diag(.data))) || all(diag(.data) == 0))
}

#' @export
is_complex.data.frame <- function(.data) {
  any(.data[[1]] == .data[[2]])
}

#' @export
is_complex.stocnet <- function(.data) {
  # a stocnet keeps its ties in `.data$ties`, not at the top level
  if(is.null(.data$ties)) FALSE else any(.data$ties$from == .data$ties$to)
}

#' @export
is_complex.network <- function(.data) {
  network::has.loops(.data)
}

#' @export
is_complex.list <- function(.data) {
  if(is_list(.data)){
    is_complex(.data[[1]])
  }
}

#' @rdname mark_format_tie 
#' @importFrom igraph any_multiple
#' @examples
#' is_multiplex(create_filled(c(3,3)))
#' @export
is_multiplex <- function(.data) UseMethod("is_multiplex")

#' @export
is_multiplex.default <- function(.data) {
  is_multiplex(as_igraph(.data))
}

#' @export
is_multiplex.matrix <- function(.data) {
  FALSE
}

reserved_tie_attr <- c("wave","panel","sign","weight","date","begin","end",
                       "name","default","increment","time")

#' @export
is_multiplex.tbl_graph <- function(.data) {
  igraph::any_multiple(.data) & length(setdiff(reserved_tie_attr, net_tie_attributes(.data)))==0 |
    length(setdiff(net_tie_attributes(.data), reserved_tie_attr)) > 0 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.igraph <- function(.data) {
  igraph::any_multiple(.data) & length(setdiff(reserved_tie_attr, net_tie_attributes(.data)))==0 |
    length(setdiff(net_tie_attributes(.data), reserved_tie_attr)) > 0 |
    "type" %in% igraph::edge_attr_names(.data)
}

#' @export
is_multiplex.network <- function(.data) {
  network::is.multiplex(.data)
}

#' @export
is_multiplex.stocnet <- function(.data) {
  "type" %in% net_tie_attributes(.data) ||
    "layer" %in% net_tie_attributes(.data)
}

#' @export
is_multiplex.data.frame <- function(.data) {
  ncol(.data) >= 3 & "type" %in% setdiff(colnames(.data), reserved_tie_attr)
}

#' @rdname mark_format_tie
#' @importFrom igraph is_simple
#' @examples
#' is_uniplex(create_star(3))
#' @export
is_uniplex <- function(.data) UseMethod("is_uniplex")

#' @export
is_uniplex.default <- function(.data) {
  is_uniplex(as_igraph(.data))
}

#' @export
is_uniplex.igraph <- function(.data) {
  igraph::is_simple(.data)
}

# Cognitive Formats ####

#' Marking networks cognitive formats
#' @name mark_format_cognitive
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_cognitive()` marks networks TRUE if they are cognitive social structures,
#'   i.e. where the edgelist contains a 'by' column indicating who reported/recorded
#'   each tie, in addition to the 'from' and 'to' columns.
#' @template param_data
#' @family marks
NULL

#' @rdname mark_format_cognitive
#' @examples
#' is_cognitive(create_filled(3))
#' @export
is_cognitive <- function(.data) UseMethod("is_cognitive")

#' @export
is_cognitive.default <- function(.data) {
  is_cognitive.igraph(as_igraph(.data))
}

#' @export
is_cognitive.data.frame <- function(.data) {
  all(c("from", "to", "by") %in% names(.data))
}

#' @export
is_cognitive.igraph <- function(.data) {
  "by" %in% igraph::edge_attr_names(.data)
}

#' @export
is_cognitive.network <- function(.data) {
  "by" %in% network::list.edge.attributes(.data)
}

#' @export
is_cognitive.matrix <- function(.data) {
  length(dim(.data)) == 3
}

#' @export
is_cognitive.stocnet <- function(.data) {
  "by" %in% names(.data$ties)
}

# Helper functions ----
.infer_net_reciprocity <- function(.data, method = "default") {
  out <- igraph::reciprocity(as_igraph(.data), mode = method)
  class(out) <- c("net_measure", class(out))
  attr(out, "mode") <- infer_dims(.data)
  out
}

.order_alphabetically <- function(v) {
  v[order(names(stats::setNames(v, v)))]
}
