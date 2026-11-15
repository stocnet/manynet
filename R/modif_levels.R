# Levelling ####

#' Modifying network levels
#' @name modif_levels
#' @description
#'   These functions reformat the levels in manynet-consistent network data.
#' 
#'   - `to_onemode()` reformats two-mode network data into one-mode network data 
#'   by simply removing the nodeset 'type' information.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`;
#'   it does not project the two-mode network into one of its modes, 
#'   but rather simply removes the distinction between the two modes.
#'   - `to_twomode()` reformats one-mode network data into two-mode network data, 
#'   using a mark to distinguish the two sets of nodes.
#'   It also reformats a hypergraph into a two-mode network,
#'   in which each hyperedge is a node of the second mode,
#'   and for this needs no mark.
#'   With `through`, it projects a network of three or more modes
#'   onto the two modes that `through` does not name.
#'   Two nodes of these modes get a tie when they have ties
#'   to the same node of `through`.
#'   For example, in a network of authors, works, and keywords,
#'   `to_twomode(.data, through = "works")` ties each author
#'   to the keywords of their works.
#'   The weight of the tie is the number of these works.
#'   For valued ties, it adds the products of the two weights.
#'   Other tie attributes, such as layers, are lost.
#'   - `to_multilevel()` reformats two-mode network data into multimodal network data, 
#'   which allows for more levels and ties within modes.
#' 
#'   If the format condition is not met,
#'   for example `to_onemode()` is used on a network that is already one-mode,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'
#'   Note that a multilevel network is still two-mode, so `as_matrix()` returns
#'   its incidence matrix. For the square matrix of both modes,
#'   use `to_onemode()` or `as_matrix(.data, twomode = FALSE)`.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(onemode|twomode|level)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_levels
#' @importFrom igraph delete_vertex_attr vertex_attr_names
#' @export
to_onemode <- function(.data) UseMethod("to_onemode")

#' @export
to_onemode.default <- function(.data){
  as_input(.data, to_onemode)
}

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

#' @rdname modif_levels
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @param through For a network of three or more modes,
#'   the modes to project through, as indices or names, as for `to_mode()`.
#'   The result is a two-mode network of the two other modes.
#'   Leave `mark` out when you give `through`.
#' @importFrom igraph V
#' @export
to_twomode <- function(.data, mark, through = NULL){
  if(!is.null(through)) return(.twomode_through(.data, through))
  # a network that is two-mode already is returned as it is
  if(missing(mark) && is_twomode(.data) && !is_hypergraph(.data))
    return(.data)
  UseMethod("to_twomode")
}

#' @export
to_twomode.default <- function(.data, mark, through = NULL){
  as_input(.data, to_twomode, mark)
}

#' @export
to_twomode.stocnet <- function(.data, mark, through = NULL){
  # A hypergraph needs no mark: its nodes are one mode and its hyperedges
  # become the other.
  if(missing(mark) && is_hypergraph(.data)){
    out <- .hyper_to_twomode(.data)
    out$info$simplicial <- NULL
    return(validate_stocnet(out))
  }
  as_stocnet(to_twomode(as_igraph(.data), mark))
}

#' @export
to_twomode.igraph <- function(.data, mark, through = NULL){
  # An 'igraph' holds a hypergraph as a two-mode network already, so all that
  # tells the two apart is the mark that it is one.
  if(missing(mark) && is_hypergraph(.data))
    return(igraph::delete_graph_attr(.data, "hyper"))
  igraph::V(.data)$type <- mark
  to_undirected(.data)
}

#' @export
to_twomode.tbl_graph <- function(.data, mark, through = NULL){
  as_tidygraph(to_twomode.igraph(.data, mark))
}

#' @export
to_twomode.network <- function(.data, mark, through = NULL){
  as_network(to_twomode(as_igraph(.data), mark), twomode = TRUE)
}

#' @rdname modif_levels 
#' @importFrom igraph V delete_vertex_attr
#' @export
to_multilevel <- function(.data) UseMethod("to_multilevel")

#' @export
to_multilevel.default <- function(.data){
  as_input(.data, to_multilevel)
}

#' @export
to_multilevel.tbl_graph <- function(.data) {
  as_tidygraph(to_multilevel(as_igraph(.data)))
}

#' @export
to_multilevel.igraph <- function(.data) {
  if(is_twomode(.data)){
    # 'type' is kept alongside 'lvl'. A multilevel network is still two-mode:
    # it has two nodesets, and ties both within and between them, which is what
    # `is_multilevel()` reads and what `fict_marvel` records. Deleting 'type'
    # would unmark the modes, contradicting that mark, and would lose the mode
    # names on a round trip, since `as_stocnet()` reads them against 'type'.
    igraph::V(.data)$lvl <- ifelse(igraph::V(.data)$type, 2, 1)
  }
  .data
}

#' @export
to_multilevel.stocnet <- function(.data) {
  # A 'stocnet' records its levels in the 'mode' variable of its nodes table,
  # which already allows two or more levels and puts no bipartite constraint
  # on the ties table, so there is nothing here to reformat. The 'lvl' variable
  # that `to_multilevel.igraph()` writes is a `{graphlayouts}` convention that
  # igraph needs only because its 'type' attribute forbids ties within a mode.
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

# Projects a network of three or more modes through the modes of `through`,
# so that the two modes left are tied where a node of one has a tie to a node
# of `through` that has a tie to a node of the other. The weight of a tie
# adds, over these nodes of `through`, the product of the two ties' weights,
# which for binary ties is the number of such nodes.
# The network is made a stocnet for this, as only it holds more than two
# modes, and returned in the class it was given.
.twomode_through <- function(.data, through){
  if(!inherits(.data, "stocnet")){
    out <- .twomode_through(as_stocnet(.data), through)
    if(inherits(.data, "tbl_graph")) return(as_tidygraph(out))
    if(inherits(.data, "igraph")) return(as_igraph(out))
    if(inherits(.data, "network")) return(as_network(out, twomode = TRUE))
    return(out)
  }
  if(is_hypergraph(.data))
    snet_abort("{.arg through} works on ties between two nodes,",
               "and this network holds hyperedges.")
  nms <- mode_names(.data)
  joined <- unique(nms[vapply(through, function(m) .infer_mode(.data, m),
                              integer(1))])
  kept <- setdiff(nms, joined)
  if(length(kept) != 2)
    snet_abort("{.arg through} must leave two modes,",
               "but it leaves {length(kept)}: {phrase(kept)}.")
  modes <- .data$nodes$mode
  rows <- which(modes == kept[1])
  cols <- which(modes == kept[2])
  mids <- which(modes %in% joined)
  ties <- .data$ties
  value <- if("weight" %in% names(ties)) ties$weight else rep(1, nrow(ties))
  # the summed weights of the ties between the nodes `a` and the nodes `b`,
  # whichever way each tie is written
  tally <- function(a, b){
    out <- matrix(0, length(a), length(b))
    for(ends in list(c("from", "to"), c("to", "from"))){
      x <- ties[[ends[1]]]
      y <- ties[[ends[2]]]
      sel <- x %in% a & y %in% b
      if(!any(sel)) next
      cell <- (match(y[sel], b) - 1L) * length(a) + match(x[sel], a)
      sums <- rowsum(as.numeric(value[sel]), cell)
      at <- as.integer(rownames(sums))
      out[at] <- out[at] + sums[, 1]
    }
    out
  }
  joint <- tally(rows, mids) %*% tally(mids, cols)
  kept_nodes <- sort(c(rows, cols))
  .data$info$modes <- kept
  .data$info$sender <- .data$info$receiver <- NULL
  out <- keep_nodes(.data, kept_nodes)
  cells <- which(joint != 0, arr.ind = TRUE)
  cells <- cells[order(cells[, 1], cells[, 2]), , drop = FALSE]
  new_ties <- dplyr::tibble(from = match(rows[cells[, 1]], kept_nodes),
                         to = match(cols[cells[, 2]], kept_nodes))
  if(any(joint[cells] != 1)) new_ties$weight <- joint[cells]
  out$ties <- new_ties
  out$info <- .prune_layer_info(out$info, character(0))
  out$info$directed <- NULL
  validate_stocnet(out)
}
