# Permuting ####

#' Modifying networks by permutation
#' @name modif_permutation
#' @description 
#'   `to_permuted()` permutes the network using a Fisher-Yates shuffle 
#'   on both the rows and columns (for a one-mode network)
#'   or on each of the rows and columns (for a two-mode network).
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_permutation 
#' @param with_attr Logical whether any attributes of the object
#'   should be retained. 
#'   By default TRUE. 
#' @export
to_permuted <- function(.data, with_attr = TRUE) UseMethod("to_permuted")

#' @export
to_permuted.default <- function(.data, with_attr = TRUE){
  as_input(.data, to_permuted, with_attr = with_attr)
}

#' @export
to_permuted.matrix <- function(.data, with_attr = TRUE) {
  # Matrices are permuted directly, short-circuiting coercion to and from
  # a `tbl_graph`, which is orders of magnitude more expensive than the
  # permutation itself and matters in permutation loops.
  # `with_attr` is moot here, since a bare matrix holds no nodal attributes.
  storage.mode(.data) <- "double"
  if(is_twomode(.data)) .r2perm(.data) else .r1perm(.data)
}

#' @export
to_permuted.stocnet <- function(.data, with_attr = TRUE) {
  # A stocnet is permuted in place, by renumbering its ties, rather than routed
  # through a `tbl_graph` and back, since that round trip costs several times
  # the permutation and matters in permutation loops (#199).
  # The nodes keep their places and attributes, and the ties move among them,
  # just as the rows and columns of a matrix move under its dimnames.
  # A missing tie, a reporter, or a hyperedge also names nodes, and so takes
  # the route through a `tbl_graph`, which handles these as before.
  missing <- as_missinglist(.data)
  if(!isTRUE(with_attr) || is_hypergraph(.data) || .names_reporters(.data) ||
     (!is.null(missing) && nrow(missing)))
    return(as_stocnet(to_permuted(as_tidygraph(.data), with_attr = with_attr)))
  n <- as.numeric(net_nodes(.data))
  # The draws are made as `.r1perm()` and `.r2perm()` make them, so that a
  # seed gives the same permutation whatever the class.
  # A two-mode network is permuted within each mode, which its 'mode' column
  # names, in the order the modes first appear, as the rows come first.
  if(is_twomode(.data) && "mode" %in% names(.data$nodes)){
    modes <- as.character(.data$nodes$mode)
    perm <- integer(n)
    for(m in unique(modes)){
      these <- which(modes == m)
      perm[these] <- these[sample(seq_along(these))]
    }
  } else perm <- sample(seq_len(n))
  if(!is.null(.data$ties) && nrow(.data$ties)){
    # The cell (i, j) of the permuted network holds the tie (perm[i], perm[j]),
    # so a tie (a, b) moves to the places that a and b take in the permutation.
    pos <- match(seq_len(n), perm)
    .data$ties$from <- as.integer(pos[.data$ties$from])
    .data$ties$to <- as.integer(pos[.data$ties$to])
  }
  .data
}

#' @export
to_permuted.tbl_graph <- function(.data, with_attr = TRUE) {
  out <- as_matrix(.data)
  if(is_twomode(.data)){
    out <- .r2perm(out)
  } else {
    out <- .r1perm(out)
  }
  if(with_attr) out <- bind_node_attributes(out, .data)
  out
}

# Helper functions ------------------

.r1perm <- function(m) {
  n <- sample(seq_len(dim(m)[1]))
  if(is_labelled(m)){
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}

.r2perm <- function(m) {
  n <- sample(seq_len(dim(m)[1]))
  o <- sample(seq_len(dim(m)[2]))
  if(is_labelled(m)){
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}
