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

# Which tie column, if any, a network names a third node in: 'by' for the
# reporter of each tie, whether a reporter or an ego, and 'about' for its
# target. A matrix gains a third dimension for it.
.third_node_col <- function(.data){
  held <- c(by = is_cognitive(.data) || is_egocentric(.data),
            about = is_gossip(.data))
  if(all(held))
    snet_abort("This network names both the reporter and the target of its",
               "ties, which a three-dimensional array cannot hold together.",
               "Please drop one of the 'by' and 'about' columns first.")
  names(held)[held][1]
}

# A network whose ties name a third node, as a three-dimensional array of
# senders, receivers, and that node. Every node takes its row, column, and
# slice in the order of the nodelist, so that isolates, and reporters who
# reported nothing, keep theirs. A tie recorded as missing holds NA.
# Each layer is a separate array, since one array holds one relation, and a
# layer whose ties name no third node gives an ordinary matrix.
.third_node_array <- function(.data, col = "by"){
  net <- as_stocnet(.data)
  ties <- net$ties
  # A cognitive social structure in which no reporter named a tie holds the
  # column without any value, so the column is what is required.
  if(is.null(ties) || !col %in% names(ties))
    snet_abort("Expected a network naming a node in a '{col}' column of its ties.")
  layers <- if(!is.null(ties[["layer"]])) unique(as.character(ties$layer)) else NULL
  if(length(layers) > 1){
    out <- lapply(layers, function(layer){
      sub <- to_uniplex(net, layer)
      if(.holds_node(sub$ties[[col]])) .third_node_array(sub, col) else
        as_matrix(sub)
    })
    return(stats::setNames(out, layers))
  }
  n <- nrow(net$nodes) %||%
    max(c(0, ties$from, ties$to, ties[[col]]), na.rm = TRUE)
  labels <- net$nodes[["label"]]
  # A multilevel network ties nodes within a mode too, so it takes every node
  # on both of the first two dimensions, as a one-mode network does.
  bipartite <- is_twomode(net) && !is_multilevel(net)
  if(bipartite){
    modes <- as.character(net$nodes$mode)
    rows <- which(modes == modes[1])
    cols <- setdiff(seq_len(n), rows)
  } else rows <- cols <- seq_len(n)
  out <- array(0, dim = c(length(rows), length(cols), n),
               dimnames = if(is.null(labels)) NULL else
                 list(labels[rows], labels[cols], labels))
  undirected <- !is_directed(net) && !bipartite
  cells <- function(tab){
    idx <- cbind(match(tab$from, rows), match(tab$to, cols), tab[[col]])
    if(undirected) idx <- rbind(idx, idx[idx[, 1] != idx[, 2], c(2, 1, 3),
                                         drop = FALSE])
    idx[stats::complete.cases(idx), , drop = FALSE]
  }
  ties <- ties[!is.na(ties[[col]]), , drop = FALSE]
  if(nrow(ties)){
    ties$value <- if("weight" %in% names(ties)) ties$weight else 1
    if(undirected){
      rev <- ties[ties$from != ties$to, , drop = FALSE]
      rev[c("from", "to")] <- rev[c("to", "from")]
      ties <- rbind(ties, rev)
    }
    idx <- cbind(match(ties$from, rows), match(ties$to, cols), ties[[col]])
    keep <- stats::complete.cases(idx)
    idx <- idx[keep, , drop = FALSE]
    # Parallel records of one tie are counted, as `as_matrix()` counts them in
    # a matrix of two dimensions.
    key <- paste(idx[, 1], idx[, 2], idx[, 3])
    sums <- rowsum(ties$value[keep], key, reorder = FALSE)
    out[idx[!duplicated(key), , drop = FALSE]] <- sums[, 1]
  }
  missing <- as_missinglist(net)
  if(!is.null(missing) && !is.null(missing[[col]])){
    missing <- missing[!is.na(missing[[col]]), , drop = FALSE]
    if(nrow(missing)) out[cells(missing)] <- NA
  }
  out
}

#' @export
as_matrix.data.frame <- function(.data,
                                 twomode = NULL, ...) {
  # A reporter or target column that names nobody says nothing about the
  # ties, and must not be read below as their weights.
  for(col in intersect(c("by", "about"), names(.data)))
    if(!.holds_node(.data[[col]])) .data[[col]] <- NULL
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if ("tbl_df" %in% class(.data)) .data <- as.data.frame(.data)
  # A third column of nothing but ones and zeroes is not a weight, but where
  # any of its values are missing it still has to be read, since a tie recorded
  # as missing cannot be recovered from a count of the ties.
  valued <- ncol(.data) >= 3 &&
    (is_weighted(.data) | is_signed(.data) | anyNA(.data[, 3]))
  if (!valued) {
    .data <- data.frame(.data) # in case it's a tibble
    .data <- as.data.frame(table(c(.data[,1]), c(.data[,2])))
    names(.data) <- c("from","to","weight")
  }
  if (ncol(.data) == 3) {
    # Adds a third (weight) column to a two-column edgelist
    # .data <- .data[order(.data[,1], .data[,2]),]
    nodes1 <- as.character(unique(.data[,1]))
    nodes1 <- sort(nodes1)
    nodes2 <- as.character(unique(.data[,2]))
    nodes2 <- sort(nodes2)
    if(length(intersect(nodes1, nodes2)) > 0 &
       !setequal(nodes1, nodes2))
      nodes1 <- nodes2 <- sort(unique(c(nodes1,nodes2)))
    if (nrow(.data) != length(nodes1)*length(nodes2)) {
      allcombs <- expand.grid(nodes1, nodes2, stringsAsFactors = FALSE)
      allcombs <- subset(allcombs, !duplicated(allcombs))
      names(allcombs) <- c("from","to")
      .data <- merge(allcombs, .data, all.x = TRUE)
      .data <- .data[order(.data[,2], .data[,1]),]
      .data[is.na(.data)] <- 0
    }
    .data <- dplyr::arrange(.data,
                            as.character(.data$to),
                            as.character(.data$from))
    .data <- structure(as.numeric(.data[,3]),
                       dim = c(as.integer(length(nodes1)),
                               as.integer(length(nodes2))),
                       dimnames = list(nodes1, nodes2))
  }
  if(!is_twomode(.data) && all(rownames(.data) == as.character(seq_nodes(.data)))) attr(.data, "dimnames") <- NULL
  if(!is_twomode(.data) && sum(.data[lower.tri(.data)])==0) .data <- .data + t(.data)
  .data
}

#' @export
as_matrix.matrix <- function(.data,
                             twomode = NULL, ...) {
  .data
}

# A matrix can hold just one value per tie, so where a network is weighted or
# signed the cells take whichever of these attributes the network actually has,
# preferring the weights where it has both.
.tie_value_attribute <- function(.data){
  attrs <- igraph::edge_attr_names(.data)
  if("weight" %in% attrs) "weight" else
    if("sign" %in% attrs) "sign" else NULL
}

# A network holds a tie recorded as missing as a tie of missing value,
# so the cells of such ties must be filled from that attribute and not left
# to a count of the ties, which would report them as present.
.holds_missing_ties <- function(.data){
  val <- .tie_value_attribute(.data)
  !is.null(val) && anyNA(igraph::edge_attr(.data, val))
}

# In a multiplex network, a layer that records no values at all leaves its
# ties without one, which igraph reports as a missing value just as it does a
# tie recorded as missing. The two are told apart by their layer: a layer
# holding some values but not others has ties that are genuinely missing,
# while a layer holding none simply records which of its ties are present.
.unvalued_layer_ties <- function(.data){
  attrs <- igraph::edge_attr_names(.data)
  layer <- if("layer" %in% attrs) "layer" else if("type" %in% attrs) "type" else NULL
  val <- .tie_value_attribute(.data)
  # Without layers, or without values, no tie is left without a value it might
  # otherwise have held.
  if(is.null(layer) || is.null(val)) return(integer(0))
  vals <- igraph::edge_attr(.data, val)
  lyr <- as.character(igraph::edge_attr(.data, layer))
  valued <- vapply(split(!is.na(vals), lyr), any, logical(1))
  which(lyr %in% names(valued)[!valued])
}

# A matrix marks a missing tie by holding nothing in that cell, which is the
# only thing it can say and all that `net_tie_missing()` needs from it.
.blank_missing <- function(mat, .data){
  missing <- as_missinglist(.data)
  if(is.null(missing) || !nrow(missing)) return(mat)
  from <- missing$from; to <- missing$to
  if(!is.numeric(from)){
    from <- match(as.character(from), rownames(mat))
    to <- match(as.character(to), colnames(mat))
  } else if(!is.null(dim(mat)) && nrow(mat) != ncol(mat)) to <- to - nrow(mat)
  idx <- cbind(from, to)
  idx <- idx[!is.na(idx[, 1]) & !is.na(idx[, 2]) &
               idx[, 1] <= nrow(mat) & idx[, 2] <= ncol(mat), , drop = FALSE]
  if(nrow(idx)) mat[idx] <- NA
  if(!is_directed(.data) && nrow(idx) && nrow(mat) == ncol(mat))
    mat[idx[, c(2, 1), drop = FALSE]] <- NA
  mat
}

#' @export
as_matrix.igraph <- function(.data,
                             twomode = NULL, ...) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if ((!is.null(twomode) && twomode) |
      (is.null(twomode) & is_twomode(.data) & !is_multiplex(.data))) {
    if (is_weighted(.data) | is_signed(.data) | .holds_missing_ties(.data)) {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = .tie_value_attribute(.data))
    } else {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = NULL)
    }
  } else {
    if (is_weighted(.data) | is_signed(.data) | .holds_missing_ties(.data)) {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = .tie_value_attribute(.data))
      if(anyNA(mat) && is_multiplex(.data)){
        el <- igraph::as_edgelist(.data, names = FALSE)[.unvalued_layer_ties(.data), ,
                                                        drop = FALSE]
        if(nrow(el)){
          mat[el] <- 1
          if(!igraph::is_directed(.data)) mat[el[, c(2, 1), drop = FALSE]] <- 1
        }
      }
    } else {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = NULL)
    }
  }
  mat <- .blank_missing(mat, .data)
  if(!is_labelled(.data)) attr(mat, "dimnames") <- NULL
  mat
}

#' @export
as_matrix.tbl_graph <- function(.data,
                                twomode = NULL, ...) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  as_matrix(as_igraph(.data), twomode = twomode)
}

#' @export
as_matrix.network <- function(.data,
                              twomode = NULL, ...) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if (network::is.bipartite(.data)) {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      out <- network::as.matrix.network(.data,
                                        attrname = "weight",
                                        expand.bipartite = FALSE)
      # Note: if expand.bipartite is true it returns the adjacency matrix. If
      # false it returns the incidence matrix that we want. Use
      # to_multilevel(mat) on the resulting matrix to do the conversion if needed.
    } else {
      out <- network::as.matrix.network(.data,
                                        expand.bipartite = FALSE)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      out <- network::as.matrix.network(.data, attrname = "weight")
    } else {
      out <- network::as.matrix.network(.data)
    }
  }
  # because network can have vertex names that are integers (i.e. just node IDs), 
  # we remove them since they are really anonymous.
  if(is.integer(network::network.vertex.names(.data))){
    attr(out, "dimnames") <- NULL
  }
  out
}

#' @export
as_matrix.network.goldfish <- function(.data,
                                       twomode = FALSE, ...) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_matrix.siena <- function(.data,
                            twomode = NULL, ...) {
  # Get the dependent network(s) first
  # Identify all dyadic depvars
  dvs <- lapply(.data$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs))
  # Add in first wave of first DV network
  out <- .data$depvars[[ddvs[1]]][,,1]
  # Add remaining waves
  for(d in 2:dim(.data$depvars[[ddvs[1]]])[3]) {
    out <- .data$depvars[[ddvs[1]]][,,d] + out
  }
  # Add other dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      for (d in seq_len(dim(.data$depvars[[ddvs[l]]])[3])) {
        out <- .data$depvars[[ddvs[l]]][,,d] + out
      }
    }
  }
  # Add dycCovars
  for (k in seq_along(.data$dycCovars)) {
    out <- .data$dycCovars[[ddvs[k]]] + out
  }
  # Add dyvCovars
  for (k in seq_along(.data$dyvCovars)) {
    for (d in seq_len(dim(.data$dyvCovars[[k]])[3])) {
      out <- .data$dyvCovars[[k]][,,d] + out
    }
  }
  out
}

#' @export
as_matrix.diff_model <- function(.data,
                                 twomode = FALSE, ...) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_matrix.stocnet <- function(.data,
                              twomode = FALSE, ...) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  as_matrix(as_igraph(.data, twomode = twomode))
}


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
