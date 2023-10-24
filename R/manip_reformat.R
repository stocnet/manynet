# Reformatting ####

#' Tools for reformatting networks, graphs, and matrices
#' 
#' @description
#'   These functions offer tools for reformatting migraph-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same object type as they are given,
#'   only transforming these objects' properties.
#' 
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'    ```{r, echo = FALSE, cache = TRUE} 
#'  knitr::kable(available_methods(c("to_uniplex", "to_undirected", "to_directed", "to_redirected", 
#'  "to_reciprocated", "to_acyclic", "to_unweighted", "to_unsigned", "to_unnamed", "to_named", 
#'  "to_simplex", "to_onemode", "to_multilevel", "to_twomode")))
#'  ```
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
#'   E edge_attr_names
#' @examples
#' as_tidygraph(create_filled(5)) %>%
#'   mutate_ties(type = sample(1:2, 10, replace = TRUE)) %>%
#'   to_uniplex("type")
#' @export
to_uniplex <- function(.data, edge) UseMethod("to_uniplex")

#' @export
to_uniplex.igraph <- function(.data, edge){
  out <- .data
  out <- igraph::delete_edges(out,
                              igraph::E(out)[igraph::edge_attr(out, edge) == 0])
  edge_names <- igraph::edge_attr_names(out)
  if (length(edge_names) > 1) {
    for (e in setdiff(edge_names, edge)) {
      out <- igraph::delete_edge_attr(out, e) 
    }
  }
  if (is.numeric(igraph::edge_attr(.data, edge))) 
    names(igraph::edge_attr(out)) <- "weight"
  out
}

#' @export
to_uniplex.tbl_graph <- function(.data, edge){
  as_tidygraph(to_uniplex(as_igraph(.data), edge))
}

#' @export
to_uniplex.network <- function(.data, edge){
  as_network(to_uniplex(as_igraph(.data), edge))
}

#' @export
to_uniplex.data.frame <- function(.data, edge){
  as_edgelist(to_uniplex(as_igraph(.data), edge))
}

#' @export
to_uniplex.matrix <- function(.data, edge){
  as_matrix(to_uniplex(as_igraph(.data), edge))
}

#' @describeIn reformat Returns an object that has any edge direction removed,
#'   so that any pair of nodes with at least one directed edge will be
#'   connected by an undirected edge in the new network.
#'   This is equivalent to the "collapse" mode in `{igraph}`.
#' @importFrom igraph as.undirected
#' @export
to_undirected <- function(.data) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(.data) {
  igraph::as.undirected(.data, edge.attr.comb = "first")
}

#' @export
to_undirected.tbl_graph <- function(.data) {
  as_tidygraph(igraph::as.undirected(.data, edge.attr.comb = "first"))
}

#' @export
to_undirected.network <- function(.data) {
  .data$gal$directed <- FALSE
  .data
}

#' @export
to_undirected.matrix <- function(.data) {
  if (is_twomode(.data)) {
    .data
  } else ((.data + t(.data)) > 0) * 1
}

#' @export
to_undirected.data.frame <- function(.data) {
  as_edgelist(to_undirected(as_igraph(.data)))
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

#' @export
to_directed.tbl_graph <- function(.data) {
  as_tidygraph(to_directed(as_igraph(.data)))
}

#' @export
to_directed.matrix <- function(.data) {
  as_matrix(to_directed(as_igraph(.data)))
}

#' @export
to_directed.network <- function(.data) {
  as_network(to_directed(as_igraph(.data)))
}

#' @export
to_directed.data.frame <- function(.data) {
  as_edgelist(to_directed(as_igraph(.data)))
}

#' @describeIn reformat Returns an object that has any edge direction transposed,
#'   or flipped, so that senders become receivers and receivers become senders.
#'   This essentially has no effect on undirected networks or reciprocated ties.
#' @importFrom igraph reverse_edges
#' @importFrom tidygraph reroute
#' @export
to_redirected <- function(.data) UseMethod("to_redirected")

#' @export
to_redirected.tbl_graph <- function(.data) {
  as_tidygraph(to_redirected.igraph(.data))
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

#' @export
to_reciprocated.tbl_graph <- function(.data) {
  as_tidygraph(to_reciprocated(as_igraph(.data)))
}

#' @export
to_reciprocated.matrix <- function(.data) {
  as_matrix(to_reciprocated(as_igraph(.data)))
}

#' @export
to_reciprocated.network <- function(.data) {
  as_network(to_reciprocated(as_igraph(.data)))
}

#' @export
to_reciprocated.data.frame <- function(.data) {
  as_edgelist(to_reciprocated(as_igraph(.data)))
}

#' @describeIn reformat Returns an object where all ties are acyclic.
#' @importFrom igraph as.directed
#' @export
to_acyclic <- function(.data) UseMethod("to_acyclic")

#' @export
to_acyclic.igraph <- function(.data) {
  igraph::as.directed(.data, mode = "acyclic")
}

#' @export
to_acyclic.tbl_graph <- function(.data) {
  as_tidygraph(to_acyclic(as_igraph(.data)))
}

#' @export
to_acyclic.matrix <- function(.data) {
  as_matrix(to_acyclic(as_igraph(.data)))
}

#' @export
to_acyclic.network <- function(.data) {
  as_network(to_acyclic(as_igraph(.data)))
}

#' @describeIn reformat Returns an object that has all edge weights removed.
#' @importFrom dplyr filter select
#' @export
to_unweighted <- function(.data, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(.data, threshold = 1) {
  edges <- weight <- NULL
  .data %>% activate(edges) %>% 
    dplyr::filter(weight >= threshold) %>% 
    dplyr::select(-c(weight))
}

#' @export
to_unweighted.igraph <- function(.data, threshold = 1) {
    as_igraph(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.network <- function(.data, threshold = 1) {
  as_network(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.matrix <- function(.data, threshold = 1) {
  (.data >= threshold)*1
}

#' @export
to_unweighted.data.frame <- function(.data, threshold = 1) {
  if(is_edgelist(.data)) .data[,1:2]
  else stop("Not an edgelist")
}

#' @describeIn reformat Returns a network with either just the "positive" ties
#'   or just the "negative" ties
#' @importFrom igraph delete_edges E delete_edge_attr
#' @export
to_unsigned <- function(.data, 
                        keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.matrix <- function(.data, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- .data
  if(keep == "positive"){
    out[out < 0] <- 0
  } else if (keep == "negative"){
    out[out > 0] <- 0
    out <- abs(out)
  } else stop("Indicate whether 'positive' or 'negative' ties should be kept.")
  out
}

#' @export
to_unsigned.data.frame <- function(.data, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- .data
  if(is_signed(.data)){
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
to_unsigned.tbl_graph <- function(.data, 
                                  keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- to_unsigned(as_igraph(.data), keep = keep)
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(.data, 
                               keep = c("positive", "negative")){
  if (is_signed(.data)) {
    keep <- match.arg(keep)
    if (keep == "positive") {
      out <- igraph::delete_edges(.data, 
                                  which(igraph::E(.data)$sign < 0))
    } else {
      out <- igraph::delete_edges(.data, 
                                  which(igraph::E(.data)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else .data
}

#' @export
to_unsigned.network <- function(.data,
                                keep = c("positive", "negative")){
  as_network(to_unsigned(as_igraph(.data)))
}

#' @describeIn reformat Returns an object with all vertex names removed
#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
#' @importFrom network delete.vertex.attribute
#' @importFrom dplyr as_tibble
#' @export
to_unnamed <- function(.data) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(.data) {
  if ("name" %in% igraph::vertex_attr_names(.data)) {
    igraph::delete_vertex_attr(.data, "name")
  } else .data
}

#' @export
to_unnamed.tbl_graph <- function(.data) {
  out <- igraph::delete_vertex_attr(.data, "name")
  tidygraph::as_tbl_graph(out)
}

#' @export
to_unnamed.network <- function(.data) {
  out <- network::delete.vertex.attribute(.data, "vertex.names")
  out
}

#' @export
to_unnamed.matrix <- function(.data) {
  out <- .data
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @export
to_unnamed.data.frame <- function(.data) {
  out <- .data
  names <- unique(unlist(c(out[,1],out[,2])))
  out[,1] <- match(unlist(.data[,1]), names)
  out[,2] <- match(unlist(.data[,2]), names)
  dplyr::as_tibble(out)
}

#' @describeIn reformat Returns an object that has random vertex names added
#' @importFrom dplyr mutate
#' @importFrom igraph vcount V
#' @export
to_named <- function(.data, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    .data <- .data %>% mutate(name = names)
  } else {
    names = sample(baby_names, igraph::vcount(as_igraph(.data)))
    .data <- .data %>%
      mutate(name = names)
  }
  .data
}

#' @export
to_named.igraph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    igraph::V(.data)$name  <- names
  } else {
    igraph::V(.data)$name  <- sample(baby_names,
                                      igraph::vcount(as_igraph(.data)))
  }
  .data
}

#' @export
to_named.data.frame <- function(.data, names = NULL) {
  if (!is.null(names)) {
    .data[,1]  <- names[as.numeric(.data[,1])]
    .data[,2]  <- names[as.numeric(.data[,2])]
  } else {
    .data[,1]  <- sample(baby_names, 
                          igraph::vcount(as_igraph(.data)))[as.numeric(.data[,1])]
    .data[,2]  <- sample(baby_names, 
                          igraph::vcount(as_igraph(.data)))[as.numeric(.data[,2])]
  }
  .data
}

#' @export
to_named.matrix <- function(.data, names = NULL) {
  if(is.null(names)) names <- sample(baby_names,
                                     igraph::vcount(as_igraph(.data)))
  if(is_twomode(.data)){
    rownames(.data)  <- names[seq_len(nrow(.data))]
    colnames(.data)  <- names[(nrow(.data)+1):length(names)]
  } else {
    rownames(.data)  <- names
    colnames(.data)  <- names
  }
  .data
}

#' @export
to_named.network <- function(.data, names = NULL) {
  as_network(to_named(as_igraph(.data), names))
}

#' @describeIn reformat Returns an object that has all loops or self-ties removed
#' @importFrom igraph simplify
#' @export
to_simplex <- function(.data) UseMethod("to_simplex")

#' @export
to_simplex.tbl_graph <- function(.data) {
  as_tidygraph(to_simplex(as_igraph(.data)))
}

#' @export
to_simplex.igraph <- function(.data) {
  igraph::simplify(.data)
}

#' @export
to_simplex.matrix <- function(.data) {
  out <- .data
  diag(out) <- 0
  out
}

#' @describeIn reformat Returns an object that has any type/mode attributes removed,
#'   but otherwise includes all the same nodes and ties.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`,
#'   which return only some of the nodes and new ties established by coincidence.
#' @importFrom igraph delete_vertex_attr vertex_attr_names
#' @export
to_onemode <- function(.data) UseMethod("to_onemode")

#' @export
to_onemode.matrix <- function(.data) {
  if (is_twomode(.data)){
    .data <- rbind(cbind(matrix(0, nrow(.data), nrow(.data)), .data),
                    cbind(t(.data), matrix(0, ncol(.data), ncol(.data))))
    colnames(.data) <- rownames(.data)
  }
  .data
}

#' @export
to_onemode.tbl_graph <- function(.data) {
  as_tidygraph(to_onemode(as_igraph(.data)))
}

#' @export
to_onemode.igraph <- function(.data) {
  if ("type" %in% igraph::vertex_attr_names(.data)) 
    .data <- igraph::delete_vertex_attr(.data, "type")
  .data
}

#' @describeIn reformat Returns a network that is not divided into two mode types
#'   but embeds two or more modes into a multimodal network structure.
#' @importFrom igraph V delete_vertex_attr
#' @export
to_multilevel <- function(.data) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(.data) {
  as_tidygraph(to_multilevel(as_igraph(.data)))
}

#' @export
to_multilevel.igraph <- function(.data) {
  if(is_twomode(.data)){
    igraph::V(.data)$lvl <- ifelse(igraph::V(.data)$type, 2, 1)
    .data <- igraph::delete_vertex_attr(.data, "type")
  }
  .data
}

#' @export
to_multilevel.matrix <- function(.data) {
  top <- cbind(matrix(0, nrow(.data), nrow(.data)), .data)
  bottom <- cbind(t(.data), matrix(0, ncol(.data), ncol(.data)))
  out <- rbind(top, bottom)
  colnames(out) <- rownames(out)
  out
}

#' @describeIn reformat Returns a network that divides the nodes into two mode types.
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph V
#' @export
to_twomode <- function(.data, mark) UseMethod("to_twomode")

#' @export
to_twomode.igraph <- function(.data, mark){
  igraph::V(.data)$type <- mark
  to_undirected(.data)
}

#' @export
to_twomode.tbl_graph <- function(.data, mark){
  as_tidygraph(to_twomode.igraph(.data, mark))
}

#' @export
to_twomode.network <- function(.data, mark){
  as_network(to_twomode(as_igraph(.data), mark), twomode = TRUE)
}
