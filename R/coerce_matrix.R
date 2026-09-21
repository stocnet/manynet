# Matrices ####

#' Coercing into matrices
#' @name coerce_matrix
#' @description
#'   `as_matrix()` coerces the object into an adjacency (one-mode/unipartite)
#'   or incidence (two-mode/bipartite) matrix.
#'
#'   If the network is a cognitive social structure or an egocentric network
#'   (i.e. the edgelist contains a 'by' column naming who reported each tie),
#'   or a gossip network (with an 'about' column naming whom each tie is about),
#'   `as_matrix()` returns a three-dimensional array instead,
#'   with dimensions for senders, receivers, and reporters (or targets).
#'   Every node takes a row, a column, and a slice, in the order of the nodes,
#'   and a tie recorded as missing holds `NA`.
#'   Where such a network has several layers, a list of one array for each
#'   layer is returned.
#'
#'   Where a network holds parallel ties, i.e. where `tie_is_parallel()` is TRUE
#'   for any tie, the cells of the matrix report how many ties join each pair
#'   of nodes, and so may be greater than one even where the network is
#'   neither weighted nor signed.
#'
#'   `as_matrix()` also turns a node measure or a node membership, as the
#'   `node_*()` functions and `node_attribute()` return them, into a matrix
#'   of the pairs of nodes, compared as `compare` says.
#'   This gives the dyadic covariates of an MRQAP model,
#'   such as those `migraph::net_regression()` builds.
#'
#'   This coercion is extractive in the sense that it loses any information
#'   that a matrix cannot hold, such as tie attributes other than the weight,
#'   or the direction of a two-mode network.
#' @details
#'   Matrices can be either adjacency (one-mode) or incidence (two-mode)
#'   matrices. Incidence matrices are typically inferred from unequal
#'   dimensions, but since in rare cases a matrix with equal dimensions may
#'   still be an incidence matrix, an additional argument `twomode` can be
#'   specified to override this heuristic.
#' @family coercions
#' @template param_data
#' @template param_two
#' @examples
#' test <- data.frame(from = c("A","B","B","C","C"), to = c("I","G","I","G","H"))
#' as_matrix(test)
#' @return
#' The currently implemented coercions or translations are:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("as_matrix"))
#'   ```
NULL

#' @rdname coerce_matrix
#' @importFrom dplyr arrange
#' @importFrom igraph edge_attr_names as_adjacency_matrix as_biadjacency_matrix
#' @importFrom network is.bipartite list.edge.attributes as.matrix.network
#' @export
as_matrix <- function(.data,
                      twomode = NULL, ...) UseMethod("as_matrix")


# Node vectors ####

#' @rdname coerce_matrix
#' @param compare How the values of two nodes are compared in each cell of
#'   the matrix returned for a node measure or a node membership.
#'   For a node measure `x`:
#'   - "absdiff" (the default) gives `abs(x[i] - x[j])`,
#'   - "diff" gives `x[i] - x[j]`, which `t()` turns into `x[j] - x[i]`,
#'   - "sender" gives `x[i]`, the value of the node in the row,
#'   - "receiver" gives `x[j]`, the value of the node in the column.
#'
#'   For a node membership, "same" gives 1 where the two nodes belong to the
#'   same group and 0 where they do not.
#'   The diagonal of a one-mode result is 0, and a missing value gives a
#'   missing cell.
#'   Where the vector comes from a two-mode network, the rows are the nodes of
#'   the first mode and the columns the nodes of the second.
#' @param ... Other arguments passed to or from other methods.
#' @examples
#' as_matrix(node_attribute(ison_lawfirm, "age"))[1:5, 1:5]
#' as_matrix(node_attribute(ison_lawfirm, "age"), compare = "diff")[1:5, 1:5]
#' @export
as_matrix.node_measure <- function(.data, twomode = NULL,
                                   compare = c("absdiff", "diff", "sender",
                                               "receiver"), ...) {
  if(identical(compare, "same"))
    snet_abort(paste("{.val same} compares the groups of a node membership.",
                     "A node measure is compared by {.val absdiff},",
                     "{.val diff}, {.val sender}, or {.val receiver}."))
  compare <- match.arg(compare)
  x <- as.numeric(unclass(.data))
  .compare_nodes(x, .data, switch(compare,
                                  absdiff = function(a, b) abs(a - b),
                                  diff = function(a, b) a - b,
                                  sender = function(a, b) a,
                                  receiver = function(a, b) b))
}

#' @rdname coerce_matrix
#' @export
as_matrix.node_member <- function(.data, twomode = NULL,
                                  compare = "same", ...) {
  if(!identical(compare, "same"))
    snet_abort(paste("A node membership names groups, which are the same or",
                     "not, so it is compared by {.val same}.",
                     "{.val {compare}} compares the values of a node measure."))
  x <- as.character(unclass(.data))
  if(isTRUE(twomode)) return(.membership_incidence(x, .data))
  .compare_nodes(x, .data, function(a, b) as.numeric(a == b))
}

# The matrix of every pair of nodes, each cell `fun()` of the two nodes'
# values. A vector from a two-mode network pairs the first mode with the
# second, since those are the dyads such a network can hold.
.compare_nodes <- function(x, .data, fun){
  labels <- names(.data)
  mode <- attr(.data, "mode")
  if(!is.null(mode) && any(mode) && !all(mode)){
    rows <- which(!mode)
    cols <- which(mode)
  } else rows <- cols <- seq_along(x)
  out <- outer(x[rows], x[cols], fun)
  if(identical(rows, cols)) diag(out) <- 0
  dimnames(out) <- if(is.null(labels)) NULL else
    list(labels[rows], labels[cols])
  out
}

# Which group each node belongs to, as a matrix of the nodes by the groups.
.membership_incidence <- function(x, .data){
  groups <- sort(unique(x[!is.na(x)]))
  out <- outer(x, groups, function(a, b) as.numeric(a == b))
  dimnames(out) <- list(names(.data), groups)
  out
}
