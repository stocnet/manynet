#' Node correlation
#' 
#' @description 
#'   This function performs a Pearson pairwise correlation on a given matrix or network data.
#'   It includes a switch: 
#'   whereas for a two-mode network it will perform a regular correlation,
#'   including all rows,
#'   for an undirected network it will perform a correlation on a matrix 
#'   with the diagonals removed,
#'   for a reciprocated network it will include the difference
#'   between reciprocated ties,
#'   and for complex networks it will include also the difference 
#'   between the self ties in each pairwise calculation.
#'   This function runs in \eqn{O(mn^2)} complexity.
#' @name manip_correlation
#' @inheritParams mark_is
#' @param method One of the following:
#'   "all" includes all information,
#'   "diag" excludes the diagonal (self-ties),
#'   "recip" excludes the diagonal but compares pairs' reciprocal ties,
#'   and "complex" compares pairs' reciprocal ties and their self ties.
#'   By default the appropriate method is chosen based on the network format.
#' @family modifications
NULL

#' @rdname manip_correlation
#' @export
to_correlation <- function(.data, method = NULL){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  mat <- as_matrix(.data)
  if(is.null(method)) method <- ifelse(is_twomode(.data),
                                       "all",
                                       ifelse(is_complex(.data),
                                              "complex",
                                              ifelse(is_directed(.data),
                                                     "recip", "diag")))
  out <- switch(method,
                all = .corTwomode(mat),
                complex = .corComplex(mat),
                recip = .corRecip(mat),
                diag = .corDiag(mat))
  out
}

#' @rdname manip_correlation
#' @export
to_cosine <- function(.data){
  x <- as_matrix(.data)
  co = array(0, c(ncol(x), ncol(x)))
  f = colnames(x)
  dimnames(co) = list(f, f)
  for (i in 2:ncol(x)) {
    for (j in 1:(i - 1)) {
      co[i, j] = crossprod(x[, i], x[, j])/sqrt(crossprod(x[, i]) * crossprod(x[, j]))
    }
  }
  co = co + t(co)
  diag(co) = 1
  as.matrix(co)
}
  
#' Network permutation
#' 
#' @description 
#'   `to_permuted()` permutes the network using a Fisher-Yates shuffle 
#'   on both the rows and columns (for a one-mode network)
#'   or on each of the rows and columns (for a two-mode network).
#' @name manip_permutation
#' @inheritParams mark_is
#' @family modifications
NULL

#' @rdname manip_permutation 
#' @param with_attr Logical whether any attributes of the object
#'   should be retained. 
#'   By default TRUE. 
#' @examples
#' graphr(ison_adolescents, node_size = 4)
#' graphr(to_permuted(ison_adolescents), node_size = 4)
#' @export
to_permuted <- function(.data, with_attr = TRUE) {
  out <- as_matrix(.data)
  if(is_twomode(.data)){
    out <- .r2perm(out)
  } else {
    out <- .r1perm(out)
  }
  if(with_attr) out <- bind_node_attributes(out, .data)
  out
}

#' @rdname make_random 
#' @export
generate_permutation <- to_permuted #to avoid migraph dependency issues

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

.corTwomode <- function(m0){
  stats::cor(m0)
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 17-18 times faster than corrColsExcludeDiag(),
# and uses 188 times less memory
.corDiag <- function(M){
  diag(M) <- NA
  out <- suppressWarnings(stats::cor(M, use = "pairwise.complete.obs"))
  out[is.na(out)] <- 0
  diag(out) <- 1
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2 times faster than corrColsRecipRLB()
.corRecip <- function(M){
  all.pairs <- utils::combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]])
    y <- c(M[-i,i[2]], M[i[2],i[1]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2.3 times faster than corrColsRecipUCI()
.corComplex <- function(M){
  all.pairs <- utils::combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]], M[i[1],i[1]])
    y <- c(M[-i,i[2]], M[i[2],i[1]], M[i[2],i[2]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

.makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}


