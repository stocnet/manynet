# Reformatting ####

#' Tools for reformatting networks, graphs, and matrices
#' 
#' @description
#' These functions offer tools for reformatting migraph-consistent objects
#' (matrices, igraph, tidygraph, or network objects).
#' Unlike the `as_*()` group of functions,
#' these functions always return the same object type as they are given,
#' only transforming these objects' properties.
#' @details
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | unweighted  | X | X | X | X | X |
#' | undirected  |  | X | X | X | X |
#' | redirected  | X | X | X | X |  |
#' | unsigned  | X | X | X | X |   |
#' | uniplex  |  |   | X | X |   |
#' | unnamed  | X | X | X | X | X |
#' | named  | X | X | X | X | X |
#' | simplex  |  | X | X | X |   |
#' | onemode  |  |   | X | X |   |
#' | multilevel  |  | X | X | X |   |
#' @name reformat
#' @family manipulations
#' @inheritParams is
#' @param edge Character string naming an edge attribute to retain from a graph.
#' @param keep In the case of a signed network, whether to retain
#' the "positive" or "negative" ties.
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @param names Character vector of the node names. NULL by default.
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
NULL

#' @describeIn reformat Returns an object that includes only a single type of tie
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#' E edge_attr_names
#' @examples
#' a <- to_uniplex(ison_algebra, "friends")
#' to_giant(a)
#' to_undirected(a)
#' to_unweighted(a)
#' @export
to_uniplex <- function(object, edge) UseMethod("to_uniplex")

#' @export
to_uniplex.igraph <- function(object, edge){
  out <- igraph::delete_edges(object,
                              igraph::E(object)[igraph::edge_attr(object, edge) == 0])
  edge_names <- igraph::edge_attr_names(object)
  if (length(edge_names) > 1) {
    for (e in setdiff(edge_names, edge)) {
      out <- igraph::delete_edge_attr(out, e) 
    }
  }
  if (is.numeric(igraph::edge_attr(object, edge))) 
    names(igraph::edge_attr(out)) <- "weight"
  out
}

#' @export
to_uniplex.tbl_graph <- function(object, edge){
  as_tidygraph(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.network <- function(object, edge){
  as_network(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.data.frame <- function(object, edge){
  as_edgelist(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.matrix <- function(object, edge){
  as_matrix(to_uniplex(as_igraph(object), edge))
}

#' @describeIn reformat Returns an object that has any edge direction removed,
#'   so that any pair of nodes with at least one directed edge will be
#'   connected by an undirected edge in the new network.
#'   This is equivalent to the "collapse" mode in `{igraph}`.
#' @importFrom igraph as.undirected
#' @export
to_undirected <- function(object) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(object) {
  igraph::as.undirected(object, edge.attr.comb = "first")
}

#' @export
to_undirected.tbl_graph <- function(object) {
  as_tidygraph(igraph::as.undirected(object, edge.attr.comb = "first"))
}

#' @export
to_undirected.network <- function(object) {
  object$gal$directed <- FALSE
  object
}

#' @export
to_undirected.matrix <- function(object) {
  if (is_twomode(object)) {
    object
  } else ((object + t(object)) > 0) * 1
}

#' @export
to_undirected.data.frame <- function(object) {
  as_edgelist(to_undirected(as_igraph(object)))
}

#' @describeIn reformat Returns a directed object.
#'   Note that ties' direction will be randomly assigned.
#'   To flip the direction, use `to_redirected()`.
#'   To match the direction, use `to_reciprocated()`.
#' @importFrom igraph as.directed
#' @export
to_directed <- function(.data) UseMethod("to_directed")

#' @export
to_directed.igraph <- function(.data) {
  if(!is_directed.igraph(.data))
    igraph::as.directed(.data, mode = "random")
  else .data
}

#' @describeIn reformat Returns an object that has any edge direction transposed,
#'   or flipped, so that senders become receivers and receivers become senders.
#'   This essentially has no effect on undirected networks or reciprocated ties.
#' @importFrom igraph reverse_edges
#' @export
to_redirected <- function(.data) UseMethod("to_redirected")

#' @export
to_redirected.tbl_graph <- function(.data) {
  nodes <- NULL
  edges <- NULL
  out <- .data %>% activate(edges)
  out$from <- .data$to
  out$to <- .data$from
  out %>% activate(nodes)
}

#' @export
to_redirected.igraph <- function(.data) {
  igraph::reverse_edges(.data)
}

#' @export
to_redirected.data.frame <- function(.data) {
  out <- .data
  out$from <- .data$to
  out$to <- .data$from
  out
}

#' @export
to_redirected.matrix <- function(.data) {
  t(.data)
}

#' @export
to_redirected.network <- function(.data) {
  as_network(to_redirected(as_igraph(.data)))
}

#' @describeIn reformat Returns an object where all ties are reciprocated.
#' @importFrom igraph as.directed
#' @export
to_reciprocated <- function(.data) UseMethod("to_reciprocated")

#' @export
to_reciprocated.igraph <- function(.data) {
  igraph::as.directed(.data, mode = "mutual")
}

#' @describeIn reformat Returns an object where all ties are acyclic.
#' @importFrom igraph as.directed
#' @export
to_acyclic <- function(.data) UseMethod("to_acyclic")

#' @export
to_acyclic.igraph <- function(.data) {
  igraph::as.directed(.data, mode = "acyclic")
}

#' @describeIn reformat Returns an object that has all edge weights removed.
#' @importFrom dplyr filter select
#' @export
to_unweighted <- function(object, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(object, threshold = 1) {
  edges <- NULL
  weight <- NULL
  object %>% activate(edges) %>% 
    dplyr::filter(weight >= threshold) %>% 
    dplyr::select(-c(weight))
}

#' @export
to_unweighted.igraph <- function(object, threshold = 1) {
    as_igraph(to_unweighted(as_tidygraph(object), threshold))
}

#' @export
to_unweighted.network <- function(object, threshold = 1) {
  as_network(to_unweighted(as_tidygraph(object), threshold))
}

#' @export
to_unweighted.matrix <- function(object, threshold = 1) {
  (object >= threshold)*1
}

#' @export
to_unweighted.data.frame <- function(object, threshold = 1) {
  if(is_edgelist(object)) object[,1:2]
  else stop("Not an edgelist")
}

#' @describeIn reformat Returns a network with either just the "positive" ties
#'   or just the "negative" ties
#' @importFrom igraph delete_edges E delete_edge_attr
#' @export
to_unsigned <- function(object, 
                        keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.matrix <- function(object, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- object
  if(keep == "positive"){
    out[out < 0] <- 0
  } else if (keep == "negative"){
    out[out > 0] <- 0
    out <- abs(out)
  } else stop("Indicate whether 'positive' or 'negative' ties should be kept.")
  out
}

#' @export
to_unsigned.data.frame <- function(object, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- object
  if(is_signed(object)){
    if(keep == "positive"){
      out$sign[out$sign < 0] <- 0
    } else if (keep == "negative"){
      out$sign[out$sign > 0] <- 0
      out$sign <- out$sign(out)
    } else stop("Indicate whether 'positive' or 'negative' ties should be kept.")
  }
  out
}

#' @export
to_unsigned.tbl_graph <- function(object, 
                                  keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- to_unsigned(as_igraph(object), keep = keep)
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(object, 
                               keep = c("positive", "negative")){
  if (is_signed(object)) {
    keep <- match.arg(keep)
    if (keep == "positive") {
      out <- igraph::delete_edges(object, 
                                  which(igraph::E(object)$sign < 0))
    } else {
      out <- igraph::delete_edges(object, 
                                  which(igraph::E(object)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else object
}

#' @export
to_unsigned.network <- function(object, 
                               keep = c("positive", "negative")){
  as_network(to_unsigned(as_igraph(object)))
}

#' @describeIn reformat Returns an object with all vertex names removed
#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
#' @importFrom network delete.vertex.attribute
#' @importFrom dplyr as_tibble
#' @export
to_unnamed <- function(object) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(object) {
  if ("name" %in% igraph::vertex_attr_names(object)) {
    igraph::delete_vertex_attr(object, "name")
  } else object
}

#' @export
to_unnamed.tbl_graph <- function(object) {
  out <- igraph::delete_vertex_attr(object, "name")
  tidygraph::as_tbl_graph(out)
}

#' @export
to_unnamed.network <- function(object) {
  out <- network::delete.vertex.attribute(object, "vertex.names")
  out
}

#' @export
to_unnamed.matrix <- function(object) {
  out <- object
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @export
to_unnamed.data.frame <- function(object) {
  out <- object
  names <- unique(unlist(c(out[,1],out[,2])))
  out[,1] <- match(unlist(object[,1]), names)
  out[,2] <- match(unlist(object[,2]), names)
  dplyr::as_tibble(out)
}

#' @describeIn reformat Returns an object that has random vertex names added
#' @importFrom dplyr mutate
#' @importFrom igraph vcount V
#' @export
to_named <- function(object, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(object, names = NULL) {
  if (!is.null(names)) {
    object <- object %>% mutate(name = names)
  } else {
    object <- object %>%
      mutate(name = sample(baby_names, igraph::vcount(as_igraph(object))))
  }
  object
}

#' @export
to_named.igraph <- function(object, names = NULL) {
  if (!is.null(names)) {
    igraph::V(object)$name  <- names
  } else {
    igraph::V(object)$name  <- sample(baby_names,
                                      igraph::vcount(as_igraph(object)))
  }
  object
}

#' @export
to_named.data.frame <- function(object, names = NULL) {
  if (!is.null(names)) {
    object[,1]  <- names[as.numeric(object[,1])]
    object[,2]  <- names[as.numeric(object[,2])]
  } else {
    object[,1]  <- sample(baby_names, 
                          igraph::vcount(as_igraph(object)))[as.numeric(object[,1])]
    object[,2]  <- sample(baby_names, 
                          igraph::vcount(as_igraph(object)))[as.numeric(object[,2])]
  }
  object
}

#' @export
to_named.matrix <- function(object, names = NULL) {
  if(is.null(names)) names <- sample(baby_names,
                                     igraph::vcount(as_igraph(object)))
  if(is_twomode(object)){
    rownames(object)  <- names[seq_len(nrow(object))]
    colnames(object)  <- names[(nrow(object)+1):length(names)]
  } else {
    rownames(object)  <- names
    colnames(object)  <- names
  }
  object
}

#' @export
to_named.network <- function(object, names = NULL) {
  as_network(to_named(as_igraph(object), names))
}

#' @describeIn reformat Returns an object that has all loops or self-ties removed
#' @importFrom igraph simplify
#' @export
to_simplex <- function(object) UseMethod("to_simplex")

#' @export
to_simplex.tbl_graph <- function(object) {
  as_tidygraph(to_simplex(as_igraph(object)))
}

#' @export
to_simplex.igraph <- function(object) {
  igraph::simplify(object)
}

#' @export
to_simplex.matrix <- function(object) {
  out <- object
  diag(out) <- 0
  out
}

#' @describeIn reformat Returns an object that has any type/mode attributes removed,
#'   but otherwise includes all the same nodes and ties.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`,
#'   which return only some of the nodes and new ties established by coincidence.
#' @importFrom igraph delete_vertex_attr vertex_attr_names
#' @export
to_onemode <- function(object) UseMethod("to_onemode")

#' @export
to_onemode.matrix <- function(object) {
  if (is_twomode(object)){
    object <- rbind(cbind(matrix(0, nrow(object), nrow(object)), object),
                    cbind(t(object), matrix(0, ncol(object), ncol(object))))
    colnames(object) <- rownames(object)
  }
  object
}

#' @export
to_onemode.tbl_graph <- function(object) {
  as_tidygraph(to_onemode(as_igraph(object)))
}

#' @export
to_onemode.igraph <- function(object) {
  if ("type" %in% igraph::vertex_attr_names(object)) 
    object <- igraph::delete_vertex_attr(object, "type")
  object
}

#' @describeIn reformat Returns a network that is not divided into two mode types
#'   but embeds two or more modes into a multimodal network structure.
#' @importFrom igraph V delete_vertex_attr
#' @export
to_multilevel <- function(object) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(object) {
  as_tidygraph(to_multilevel(as_igraph(object)))
}

#' @export
to_multilevel.igraph <- function(object) {
  if(is_twomode(object)){
    igraph::V(object)$lvl <- ifelse(igraph::V(object)$type, 2, 1)
    object <- igraph::delete_vertex_attr(object, "type")
  }
  object
}

#' @export
to_multilevel.matrix <- function(object) {
  top <- cbind(matrix(0, nrow(object), nrow(object)), object)
  bottom <- cbind(t(object), matrix(0, ncol(object), ncol(object)))
  out <- rbind(top, bottom)
  colnames(out) <- rownames(out)
  out
}

#' @describeIn reformat Returns a network that divides the nodes into two mode types.
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph V
#' @export
to_twomode <- function(object, mark) UseMethod("to_twomode")

#' @export
to_twomode.igraph <- function(object, mark){
  igraph::V(object)$type <- mark
  to_undirected(object)
}

#' @export
to_twomode.tbl_graph <- function(object, mark){
  as_tidygraph(to_twomode.igraph(object, mark))
}

#' @export
to_twomode.network <- function(object, mark){
  as_network(to_twomode(as_igraph(object, mark)))
}
