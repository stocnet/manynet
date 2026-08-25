# Permuting ####

#' Network permutation
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
